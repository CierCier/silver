use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::attributes::validate_global_attributes;
use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::module_artifact::{ModuleArtifact, ast_type_from_canonical_key};
use crate::parser::ast;
use crate::semantic::analyzer::Analyzer;
use crate::semantic::monomorph::MonomorphRequest;
use crate::semantic::send_check::{self, DefView};
use crate::symbol_table::{CompilerPhase, CompilerSymbolTable, SymbolId, SymbolKind};
use crate::traits::validate_traits_with_imports;
use crate::types::{
    StructAttrError, Type, TypeContext, TypeLayout, is_bool, is_integer, is_numeric, is_string,
    is_void, parse_struct_attributes, struct_layout,
};

mod lambda;

const MAX_TYPE_CHECK_DEPTH: usize = 32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

#[derive(Default)]
pub struct TypeChecker {
    lambdas: lambda::LambdaState,
    errors: Vec<TypeError>,
    expression_depth: usize,
    scopes: Vec<HashMap<String, (Type, bool)>>,
    known_type_ids: HashSet<SymbolId>,
    known_types: HashMap<String, SymbolId>,
    struct_defs: HashMap<String, StructDef>,
    enum_defs: HashMap<String, EnumDef>,
    moved_locals: Vec<HashSet<String>>,
    static_vars: Vec<HashSet<String>>,
    volatile_vars: Vec<HashSet<String>>,
    volatile_globals: HashSet<String>,
    current_return: Option<Type>,
    current_type_params: HashSet<String>,
    method_symbols: HashMap<SymbolId, MethodSig>,
    defer_depth: u32,
    methods: HashMap<(String, String), Vec<SymbolId>>,
    type_ctx: TypeContext,
    function_symbols: HashMap<SymbolId, FunctionSig>,
    functions: HashMap<String, Vec<SymbolId>>,
    /// Default type arguments for local aggregate definitions.
    generic_defaults: HashMap<String, Vec<Option<ast::Type>>>,
    source_trait_impls: HashMap<String, Vec<TraitImpl>>,
    /// Base type names that implement Drop (from `impl Drop<X>` — including
    /// generic impls like `impl<T> Drop<Vec<T>>`), used to require explicit
    /// `move` when an owned payload enters an enum.
    drop_owner_bases: HashSet<String>,
    monomorph_requests: Vec<MonomorphRequest>,
    imported_functions: HashMap<String, Vec<FunctionSig>>,
    extern_variables: HashMap<String, Type>,
    global_variables: HashMap<String, Type>,
    imported_types: HashSet<String>,
    imported_traits: HashSet<String>,
    imported_modules: Vec<ModuleArtifact>,
    imported_source_hashes: HashSet<u64>,
    casts: HashMap<(String, String), ()>,
    type_aliases: HashMap<String, ast::Type>,
    /// Expression type cache for LSP hover support.
    /// Maps (start_byte, end_byte) → type string.
    pub expr_types: HashMap<(usize, usize), String>,
    /// Resolved iterator types for ForIn expressions, populated during typeck.
    /// Maps (expr start, expr end) → AST type of the IntoIter associated type.
    /// Used to populate the ForIn AST node after typeck for codegen.
    pub resolved_iter_types: HashMap<(u32, usize, usize), Box<ast::Type>>,
    /// Bare enum constructors (`Some(x)`, `None`, `Ok(x)`, `Err(x)`) resolved
    /// via expected-type inference during typeck; applied to the AST after typeck.
    pub bare_constructors: HashMap<(u32, usize, usize), BareConstructorRewrite>,
    /// Implicit guards: binary operators on bare type params recorded while
    /// checking a generic function body, keyed by function/method signature.
    /// Checked at every concrete monomorphization request.
    current_implicit_reqs: Vec<ImplicitReq>,
    implicit_reqs: HashMap<String, Vec<ImplicitReq>>,
    /// Target triple of the compilation unit (empty = host default). Used for
    /// target-gated semantic checks, e.g. rejecting `launch` on wasm.
    pub target_triple: String,
    /// Trait-style method calls on bare type params, deferred until concrete
    /// monomorphization.
    current_implicit_method_reqs: Vec<ImplicitMethodReq>,
    implicit_method_reqs: HashMap<String, Vec<ImplicitMethodReq>>,
    /// Inferred `let x = expr;` bindings recorded during typeck:
    /// (stmt start, stmt end) → (inferred type, binding-name span).
    /// The driver materializes these as type annotations after typeck so
    /// downstream passes (move/borrow/escape checks, monomorphization,
    /// codegen) see ordinary annotated bindings.
    inferred_lets: HashMap<(usize, usize), (Type, Span)>,
}

/// A binary operation on a type parameter recorded during generic body
/// checking. The requirement is enforced at each concrete instantiation:
/// the substituted operand types must support the operator (builtin or
/// `__<op>` overload), or the call site errors.
#[derive(Debug, Clone)]
struct ImplicitReq {
    left: Type,
    right: Type,
    op: ast::BinaryOperator,
    origin_span: Span,
}

#[derive(Debug, Clone)]
struct ImplicitMethodReq {
    receiver: Type,
    name: String,
    args: Vec<Type>,
    style: MethodCallStyle,
    origin_span: Span,
}

/// A bare enum constructor (`Some(x)`, `None`, `Ok(x)`, `Err(x)`) resolved
/// via expected-type inference during typeck. The driver rewrites the AST node
/// into an `Enum.Variant(...)` construction after typeck, mirroring the
/// `populate_for_in_iterator_types` post-pass.
#[derive(Debug, Clone)]
pub struct BareConstructorRewrite {
    /// Enum type name ("Optional", "Result").
    pub enum_name: String,
    /// Variant name ("Some", "None", "Ok", "Err").
    pub variant: String,
    /// Concrete generic arguments (e.g. [i32] for `Optional<i32>`).
    pub generics: Vec<ast::Type>,
}

#[derive(Debug, Clone)]
struct MethodSig {
    params: Vec<Type>,
    return_type: Type,
    type_params: Vec<String>,
    owner: Type,
    bounds: Vec<TypeBoundPredicate>,
    // Shared handles: all methods of one impl block reference the same
    // Arc'd impl, so registering N methods clones the impl once instead of
    // N times (the O(N^2) AST blow-up that dominated typecheck RSS).
    source_impl: std::sync::Arc<ast::ImplItem>,
    source_method: std::sync::Arc<ast::ImplFunction>,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<Type>,
    return_type: Type,
    type_params: Vec<String>,
    bounds: Vec<TypeBoundPredicate>,
    source: std::sync::Arc<ast::FunctionItem>,
    is_variadic: bool,
    /// True when the signature came from an imported module artifact: the
    /// source is a signature-only placeholder with no body, and any
    /// monomorphized instance must be emitted as an external declaration.
    is_imported: bool,
}

#[derive(Debug, Clone)]
struct TypeBoundPredicate {
    bounded: Type,
    bounds: Vec<ast::TraitBound>,
}

#[derive(Debug, Clone)]
struct TraitImpl {
    owner: Type,
    params: Vec<String>,
    args: Vec<Type>,
    bounds: Vec<TypeBoundPredicate>,
}

#[derive(Debug, Clone)]
struct StructDef {
    type_params: Vec<String>,
    fields: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
struct VariantInfo {
    payload: Vec<ast::Type>,
}

#[derive(Debug, Clone)]
struct EnumDef {
    backing_type: ast::PrimitiveType,
    variants: HashMap<String, VariantInfo>,
    type_params: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MethodCallStyle {
    Instance,
    Static,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self::default()
    }
    fn type_from_ast(&mut self, source: &ast::Type) -> Type {
        let mut normalized = source.clone();
        let defaults = self
            .generic_defaults
            .iter()
            .map(|(name, params)| (name.clone(), params.clone()))
            .collect::<Vec<_>>();
        if let Err(message) = ast::apply_generic_defaults(&mut normalized, &defaults) {
            self.error(message, source.span);
        }
        Self::expand_imported_type_aliases(Type::from_ast(&normalized), &self.type_aliases)
    }

    pub fn with_imported_modules(mut self, modules: &[ModuleArtifact]) -> Self {
        self.imported_modules = modules.to_vec();
        for module in modules {
            self.ingest_module(module);
        }
        self
    }

    pub fn check_program(
        mut self,
        program: &ast::Program,
    ) -> (Vec<TypeError>, Vec<MonomorphRequest>) {
        let mut table = CompilerSymbolTable::new();
        self.check_program_with_table(program, &mut table)
    }

    pub fn check_program_with_table(
        &mut self,
        program: &ast::Program,
        table: &mut CompilerSymbolTable,
    ) -> (Vec<TypeError>, Vec<MonomorphRequest>) {
        table.touch_phase(CompilerPhase::TypeCheck, "type checking start");
        let mut analyzer = Analyzer::new();
        analyzer.inject_imported_modules(&self.imported_modules);
        for error in analyzer.analyze_program_with_table(program, table) {
            self.errors.push(TypeError {
                message: error.message,
                span: error.span,
            });
        }
        table.record_program_symbols(program, CompilerPhase::TypeCheck);
        for trait_error in validate_traits_with_imports(program, &self.imported_traits) {
            self.errors.push(TypeError {
                message: trait_error.message,
                span: trait_error.span,
            });
        }
        self.collect_known_types(program, table);
        self.register_imported_types();
        self.collect_trait_impls(program);
        self.collect_functions(program, table);
        self.collect_extern_variables(program, table);
        self.collect_global_variables(program, table);
        self.collect_imported_functions(table);
        self.collect_impl_methods(program, table);
        self.collect_struct_layouts(program);
        self.check_global_attributes(&program.attributes);
        self.lambdas.symbols = std::mem::take(table);
        for item in &program.items {
            self.check_global_attributes(&item.attributes);
            if let ast::ItemKind::Struct(_) = &item.kind {
                self.check_struct_attributes(&item.attributes);
            }
            if let ast::ItemKind::Function(func) = &item.kind {
                self.errors
                    .extend(Self::check_function_attributes(&item.attributes));
                if crate::attributes::is_test_function(&item.attributes) {
                    if !func.parameters.is_empty() {
                        self.errors.push(TypeError {
                            message: msg::test_fn_must_take_no_parameters(&func.name.name),
                            span: func.name.span,
                        });
                    }
                    if let Some(ref ret) = func.return_type {
                        let ret_ty = Type::from_ast(ret);
                        if !matches!(
                            ret_ty,
                            Type::Unit | Type::Primitive(ast::PrimitiveType::Void)
                        ) {
                            self.errors.push(TypeError {
                                message: msg::test_fn_must_return_void(&func.name.name),
                                span: ret.span,
                            });
                        }
                    }
                }
                self.check_function(func);
            }
            if let ast::ItemKind::ExternFunction(_) = &item.kind {
                self.errors
                    .extend(Self::check_function_attributes(&item.attributes));
            }
            if let ast::ItemKind::GlobalVariable(var) = &item.kind {
                self.check_global_variable(var);
            }
            if let ast::ItemKind::Impl(impl_item) = &item.kind {
                let self_ty = Type::from_ast(&impl_item.self_type);
                let mut type_params = Self::type_param_names(impl_item.generics.as_ref());
                type_params.extend(impl_item.implicit_type_params.iter().cloned());
                for impl_member in &impl_item.items {
                    match impl_member {
                        ast::ImplItemKind::Function(func) => {
                            self.errors
                                .extend(Self::check_function_attributes(&func.attributes));
                            self.check_impl_method(&self_ty, func, &type_params);
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            self.check_impl_cast(&self_ty, cast, &type_params);
                        }
                        _ => {}
                    }
                }
            }
        }
        *table = std::mem::take(&mut self.lambdas.symbols);
        table.touch_phase(
            CompilerPhase::TypeCheck,
            format!("type checking done (errors={})", self.errors.len()),
        );
        let requests = std::mem::take(&mut self.monomorph_requests);
        for request in &requests {
            match request {
                MonomorphRequest::Function {
                    source,
                    mapping,
                    call_span,
                    ..
                } => {
                    let key = Self::free_fn_key(source);
                    self.check_implicit_guards(&key, mapping, *call_span);
                    self.check_implicit_method_guards(&key, mapping, *call_span);
                }
                MonomorphRequest::ImplMethod {
                    impl_item,
                    method,
                    mapping,
                    call_span,
                    ..
                } => {
                    let self_ty = Type::from_ast(&impl_item.self_type);
                    let key = Self::impl_method_key(&self_ty, method);
                    self.check_implicit_guards(&key, mapping, *call_span);
                    self.check_implicit_method_guards(&key, mapping, *call_span);
                }
            }
        }
        (std::mem::take(&mut self.errors), requests)
    }

    /// Consume the resolved iterator types map for post-typeck AST population.
    pub fn take_resolved_iter_types(&mut self) -> HashMap<(u32, usize, usize), Box<ast::Type>> {
        std::mem::take(&mut self.resolved_iter_types)
    }

    /// Consume bare-constructor rewrite records for post-typeck AST rewriting.
    pub fn take_bare_constructors(
        &mut self,
    ) -> HashMap<(u32, usize, usize), BareConstructorRewrite> {
        std::mem::take(&mut self.bare_constructors)
    }

    /// Consume inferred `let` bindings for post-typeck AST materialization.
    pub fn take_inferred_lets(&mut self) -> HashMap<(usize, usize), (Type, Span)> {
        std::mem::take(&mut self.inferred_lets)
    }

    fn ingest_module(&mut self, module: &ModuleArtifact) {
        if !self
            .imported_source_hashes
            .insert(module.source_hash_fnv1a64)
        {
            return;
        }
        for export in &module.exports {
            match export.kind {
                crate::module_artifact::ExportKind::Function => {
                    if let Ok((params, return_type)) =
                        crate::types::parse_canonical_function_signature(&export.signature)
                    {
                        let sig = FunctionSig {
                            params: params.clone(),
                            return_type: return_type.clone(),
                            type_params: export.type_params.clone(),
                            bounds: Vec::new(),
                            // Signature-only placeholder: the params/return
                            // must be carried so monomorphizing the imported
                            // generic function yields a correctly-typed
                            // mangled instance (identity__i64). The body stays
                            // empty; codegen emits an external declaration.
                            source: std::sync::Arc::new(ast::FunctionItem {
                                name: ast::Identifier {
                                    name: export.name.clone(),
                                    span: Span::default(),
                                },
                                generics: Self::export_type_params_to_generics(&export.type_params),
                                is_variadic: export.is_variadic,
                                parameters: params
                                    .iter()
                                    .enumerate()
                                    .map(|(index, param_type)| ast::Parameter {
                                        name: ast::Identifier {
                                            name: format!("p{index}"),
                                            span: Span::default(),
                                        },
                                        param_type: param_type.to_ast(),
                                        is_mutable: false,
                                        is_variadic: false,
                                        span: Span::default(),
                                    })
                                    .collect(),
                                return_type: if matches!(return_type, Type::Unit) {
                                    None
                                } else {
                                    Some(return_type.to_ast())
                                },
                                body: ast::Block {
                                    statements: Vec::new(),
                                    span: Span::default(),
                                },
                            }),
                            is_variadic: export.is_variadic,
                            is_imported: true,
                        };
                        self.imported_functions
                            .entry(export.name.clone())
                            .or_default()
                            .push(sig);

                        if let Some((owner_name, method_name)) = export.name.split_once("::") {
                            let owner_ty =
                                Type::from_canonical_key(owner_name).unwrap_or_else(|_| {
                                    Type::Named {
                                        path: vec![owner_name.to_string()],
                                        generics: Vec::new(),
                                    }
                                });
                            let owner_key = self.method_key(&owner_ty);
                            let method_sig = MethodSig {
                                params: params.clone(),
                                return_type: return_type.clone(),
                                type_params: export.type_params.clone(),
                                owner: owner_ty.clone(),
                                bounds: Vec::new(),
                                source_impl: std::sync::Arc::new(ast::ImplItem {
                                    generics: None,
                                    trait_ref: None,
                                    self_type: owner_ty.to_ast(),
                                    items: Vec::new(),
                                    implicit_type_params: Vec::new(),
                                }),
                                source_method: std::sync::Arc::new(ast::ImplFunction {
                                    name: ast::Identifier {
                                        name: method_name.to_string(),
                                        span: Span::default(),
                                    },
                                    generics: None,
                                    is_variadic: export.is_variadic,
                                    parameters: params
                                        .iter()
                                        .enumerate()
                                        .map(|(i, p)| ast::Parameter {
                                            name: ast::Identifier {
                                                name: format!("p{i}"),
                                                span: Span::default(),
                                            },
                                            param_type: p.to_ast(),
                                            is_mutable: false,
                                            is_variadic: false,
                                            span: Span::default(),
                                        })
                                        .collect(),
                                    method_kind: if let Some(first) = params.first() {
                                        // A receiver is a first parameter naming the owner
                                        // itself (by value, pointer, or reference) — any
                                        // other first parameter makes this a static
                                        // method, even when it is a pointer (e.g.
                                        // `Terminal.new(BufWriter*, ...)`).
                                        let receiver_inner = match first {
                                            Type::Pointer { inner, .. }
                                            | Type::Reference { inner, .. } => Some(inner.as_ref()),
                                            _ => None,
                                        };
                                        if let Some(inner) = receiver_inner {
                                            if inner == &owner_ty {
                                                let is_mutable = match first {
                                                    Type::Pointer { is_mutable, .. }
                                                    | Type::Reference { is_mutable, .. } => {
                                                        *is_mutable
                                                    }
                                                    _ => false,
                                                };
                                                ast::MethodKind::InstancePointer { is_mutable }
                                            } else {
                                                ast::MethodKind::Static
                                            }
                                        } else if *first == owner_ty {
                                            ast::MethodKind::InstanceValue
                                        } else {
                                            ast::MethodKind::Static
                                        }
                                    } else {
                                        ast::MethodKind::Static
                                    },
                                    visibility: ast::Visibility::Public,
                                    return_type: if matches!(return_type, Type::Unit) {
                                        None
                                    } else {
                                        Some(return_type.to_ast())
                                    },
                                    body: ast::Block {
                                        statements: Vec::new(),
                                        span: Span::default(),
                                    },
                                    attributes: Vec::new(),
                                    span: Span::default(),
                                }),
                            };
                            let symbol_id =
                                (self.method_symbols.len() + 100_000 + self.methods.len()) as u64;
                            self.method_symbols.insert(symbol_id, method_sig);
                            self.methods
                                .entry((owner_key, method_name.to_string()))
                                .or_default()
                                .push(symbol_id);
                        }
                    }
                }
                crate::module_artifact::ExportKind::Struct => {
                    self.imported_types.insert(export.name.clone());
                    let mut fields = HashMap::default();
                    for field in &export.fields {
                        if let Ok(field_ty) =
                            crate::types::Type::from_canonical_key(&field.type_key)
                        {
                            fields.insert(field.name.clone(), field_ty);
                        }
                    }
                    if !fields.is_empty() {
                        self.struct_defs.insert(
                            export.name.clone(),
                            StructDef {
                                type_params: export.type_params.clone(),
                                fields,
                            },
                        );
                    }
                    if let Some(layout) = export.layout
                        && let (Some(size), Some(align)) = (layout.size, layout.align)
                    {
                        self.type_ctx.register_named(
                            std::slice::from_ref(&export.name),
                            TypeLayout::known(size as usize, align as usize),
                        );
                    }
                }
                crate::module_artifact::ExportKind::Enum => {
                    self.imported_types.insert(export.name.clone());
                    let backing_type = export
                        .enum_backing_type
                        .as_deref()
                        .and_then(|text| ast_type_from_canonical_key(text).ok())
                        .and_then(|ty| match *ty.kind {
                            ast::TypeKind::Primitive(ref primitive) => Some(primitive.clone()),
                            _ => None,
                        })
                        .unwrap_or(ast::PrimitiveType::I32);
                    let variants = export
                        .enum_variants
                        .iter()
                        .map(|variant| {
                            (
                                variant.name.clone(),
                                VariantInfo {
                                    payload: variant
                                        .payload_types
                                        .iter()
                                        .map(|key| ast_type_from_canonical_key(key))
                                        .collect::<Result<Vec<_>, _>>()
                                        .unwrap_or_else(|_| vec![]),
                                },
                            )
                        })
                        .collect::<HashMap<_, _>>();
                    self.enum_defs.insert(
                        export.name.clone(),
                        EnumDef {
                            backing_type: backing_type.clone(),
                            variants,
                            type_params: export.type_params.clone(),
                        },
                    );
                    self.register_enum_layout(&export.name, &backing_type);
                }
                crate::module_artifact::ExportKind::Trait => {
                    self.imported_traits.insert(export.name.clone());
                }
                crate::module_artifact::ExportKind::Constant
                | crate::module_artifact::ExportKind::Global => {
                    if let Some(type_key) = &export.type_key {
                        if let Ok(ty) = crate::types::Type::from_canonical_key(type_key) {
                            self.global_variables.insert(export.name.clone(), ty);
                        }
                    }
                }
                crate::module_artifact::ExportKind::TypeAlias => {
                    if let Some(type_key) = &export.type_key {
                        if let Ok(ast_ty) = ast_type_from_canonical_key(type_key) {
                            self.type_aliases.insert(export.name.clone(), ast_ty);
                        }
                    }
                }
            }
        }
    }

    fn export_type_params_to_generics(type_params: &[String]) -> Option<ast::Generics> {
        if type_params.is_empty() {
            return None;
        }
        let params = type_params
            .iter()
            .map(|name| {
                ast::GenericParam::Type(ast::TypeParam {
                    name: ast::Identifier {
                        name: name.clone(),
                        span: Span::default(),
                    },
                    bounds: Vec::new(),
                    default: None,
                    span: Span::default(),
                })
            })
            .collect::<Vec<_>>();
        Some(ast::Generics {
            params,
            where_clause: None,
            span: Span::default(),
        })
    }

    fn register_imported_types(&mut self) {
        for name in &self.imported_types {
            if self.known_types.contains_key(name) {
                continue;
            }
            let id = self.known_type_ids.len() as SymbolId + 10_000;
            self.known_type_ids.insert(id);
            self.known_types.insert(name.clone(), id);
        }
    }

    fn collect_imported_functions(&mut self, table: &mut CompilerSymbolTable) {
        for (name, sigs) in &self.imported_functions {
            for (index, sig) in sigs.iter().enumerate() {
                let symbol_key = format!("imported::{name}::{index}");
                let symbol_id = table.intern_symbol(
                    symbol_key.clone(),
                    SymbolKind::Function,
                    None,
                    CompilerPhase::TypeCheck,
                );
                self.function_symbols.insert(symbol_id, sig.clone());
                self.functions
                    .entry(name.clone())
                    .or_default()
                    .push(symbol_id);
            }
        }
    }

    fn collect_known_types(&mut self, program: &ast::Program, table: &mut CompilerSymbolTable) {
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Struct(struct_item) => {
                    let type_id = table.intern_symbol(
                        format!("type::{}", struct_item.name.name),
                        SymbolKind::Struct,
                        Some(struct_item.name.span),
                        CompilerPhase::TypeCheck,
                    );
                    self.known_type_ids.insert(type_id);
                    self.known_types
                        .insert(struct_item.name.name.clone(), type_id);
                    let type_params = struct_item
                        .generics
                        .as_ref()
                        .map(|generics| {
                            generics
                                .params
                                .iter()
                                .filter_map(|param| {
                                    if let ast::GenericParam::Type(type_param) = param {
                                        Some(type_param.name.name.clone())
                                    } else {
                                        None
                                    }
                                })
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default();
                    let defaults = struct_item
                        .generics
                        .as_ref()
                        .map(|generics| {
                            // Lifetime parameters occupy no type-argument
                            // slot (erased at use sites).
                            generics
                                .params
                                .iter()
                                .filter_map(|param| match param {
                                    ast::GenericParam::Type(param) => Some(param.default.clone()),
                                    ast::GenericParam::Lifetime(_) => None,
                                })
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default();
                    self.generic_defaults
                        .insert(struct_item.name.name.clone(), defaults);
                    let fields = struct_item
                        .fields
                        .iter()
                        .map(|field| {
                            let ty = Type::from_ast(&field.field_type);
                            self.reject_wasm_extended_float(&ty, field.field_type.span);
                            (field.name.name.clone(), ty)
                        })
                        .collect::<HashMap<_, _>>();
                    self.struct_defs.insert(
                        struct_item.name.name.clone(),
                        StructDef {
                            type_params,
                            fields,
                        },
                    );
                }
                ast::ItemKind::Enum(enum_item) => {
                    let defaults = enum_item
                        .generics
                        .as_ref()
                        .map(|generics| {
                            // Lifetime parameters occupy no type-argument
                            // slot (erased at use sites).
                            generics
                                .params
                                .iter()
                                .filter_map(|param| match param {
                                    ast::GenericParam::Type(param) => Some(param.default.clone()),
                                    ast::GenericParam::Lifetime(_) => None,
                                })
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default();
                    self.generic_defaults
                        .insert(enum_item.name.name.clone(), defaults);
                    let type_id = table.intern_symbol(
                        format!("type::{}", enum_item.name.name),
                        SymbolKind::Enum,
                        Some(enum_item.name.span),
                        CompilerPhase::TypeCheck,
                    );
                    self.known_type_ids.insert(type_id);
                    self.known_types
                        .insert(enum_item.name.name.clone(), type_id);
                    if let Some(enum_def) = self.build_enum_def(enum_item) {
                        self.register_enum_layout(&enum_item.name.name, &enum_def.backing_type);
                        self.enum_defs.insert(enum_item.name.name.clone(), enum_def);
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_trait_impls(&mut self, program: &ast::Program) {
        for item in &program.items {
            let ast::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };
            self.collect_trait_impl(impl_item);
        }
    }

    fn collect_trait_impl(&mut self, impl_item: &ast::ImplItem) {
        let Some(trait_ref) = &impl_item.trait_ref else {
            return;
        };
        let name = trait_ref
            .path
            .last()
            .map(|id| id.name.clone())
            .unwrap_or_default();
        if name.is_empty() {
            return;
        }
        let self_ty = Type::from_ast(&impl_item.self_type);
        let params = Self::impl_type_parameters(impl_item);
        let implementation = TraitImpl {
            owner: self_ty,
            params,
            args: trait_ref
                .generics
                .as_deref()
                .unwrap_or_default()
                .iter()
                .map(Type::from_ast)
                .collect(),
            bounds: self.collect_bounds(impl_item.generics.as_ref()),
        };
        self.source_trait_impls
            .entry(name.clone())
            .or_default()
            .push(implementation);
        // Record the Drop-owner base name for generic templates too.
        if name == "Drop"
            && let ast::TypeKind::Named(named) = impl_item.self_type.kind.as_ref()
        {
            self.drop_owner_bases.insert(named.path[0].name.clone());
        }
    }

    fn impl_type_parameters(impl_item: &ast::ImplItem) -> Vec<String> {
        let explicit = impl_item
            .generics
            .iter()
            .flat_map(|generics| &generics.params)
            .filter_map(|param| match param {
                ast::GenericParam::Type(param) => Some(param.name.name.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        if explicit.is_empty() {
            impl_item.implicit_type_params.clone()
        } else {
            explicit
        }
    }

    /// True if `ty` owns resources (base implements Drop). TODO(Phase 5): centralize via `type_properties::{is_copy,needs_drop}`.
    fn type_has_drop_impl(&self, ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                // Base-name check only: concrete payloads whose type (or a
                // type parameter resolving to a known owner) implements
                // Drop. Bare type parameters are deliberately NOT treated as
                // owned here — a concrete enum impl like `impl
                // Result<i64, Error>` still shows the generic `T` payload, so
                // flagging it would be a false positive; generic-template
                // constructions are enforced at their instantiation sites
                // (the std template bodies already use `move`).
                named.path.len() == 1 && self.drop_owner_bases.contains(&named.path[0].name)
            }
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_) => false,
            ast::TypeKind::Array(array) => self.type_has_drop_impl(&array.element_type),
            ast::TypeKind::Tuple(types) => types.iter().any(|t| self.type_has_drop_impl(t)),
            _ => false,
        }
    }

    fn check_function_attributes(attributes: &[ast::Attribute]) -> Vec<TypeError> {
        let mut errors = Vec::new();
        for attr in attributes {
            if attr.name.name == "link_name" {
                if attr.args.len() != 1 {
                    errors.push(TypeError {
                        message: msg::link_name_expects_one().to_string(),
                        span: attr.span,
                    });
                } else if let Some(arg) = attr.args.first() {
                    match arg {
                        ast::AttributeArg::Literal(ast::Literal::String(s)) => {
                            if s.is_empty() {
                                errors.push(TypeError {
                                    message: "#[link_name] requires a non-empty string".to_string(),
                                    span: attr.span,
                                });
                            }
                        }
                        _ => {
                            errors.push(TypeError {
                                message: "#[link_name] requires a string literal".to_string(),
                                span: attr.span,
                            });
                        }
                    }
                }
            } else if attr.name.name == "inline" {
                if attr.args.len() != 1 {
                    errors.push(TypeError {
                        message: msg::inline_expects_one().to_string(),
                        span: attr.span,
                    });
                } else if !matches!(
                    &attr.args[0],
                    ast::AttributeArg::Identifier(id) if id.name == "always"
                ) {
                    errors.push(TypeError {
                        message: msg::inline_only_always().to_string(),
                        span: attr.span,
                    });
                }
            } else if attr.name.name == "target_feature" {
                if attr.args.len() != 1 {
                    errors.push(TypeError {
                        message: msg::target_feature_exactly_one().to_string(),
                        span: attr.span,
                    });
                } else if let Some(arg) = attr.args.first() {
                    match arg {
                        ast::AttributeArg::Literal(ast::Literal::String(name)) => {
                            if crate::attributes::llvm_target_feature(name).is_none() {
                                errors.push(TypeError {
                                    message: msg::unknown_target_feature(name),
                                    span: attr.span,
                                });
                            }
                        }
                        _ => {
                            errors.push(TypeError {
                                message: msg::target_feature_string_literal().to_string(),
                                span: attr.span,
                            });
                        }
                    }
                }
            } else if attr.name.name == "test" {
                if !attr.args.is_empty() {
                    errors.push(TypeError {
                        message: msg::test_takes_no_arguments().to_string(),
                        span: attr.span,
                    });
                }
            }
        }
        errors
    }

    fn type_param_names(generics: Option<&ast::Generics>) -> HashSet<String> {
        generics.iter().flat_map(|g| &g.params)
            .filter_map(|p| match p {
                ast::GenericParam::Type(p) => Some(p.name.name.clone()),
                _ => None,
            }).collect()
    }

    fn check_function(&mut self, func: &ast::FunctionItem) {
        self.current_type_params = Self::type_param_names(func.generics.as_ref());
        self.current_implicit_reqs = Vec::new();
        self.current_implicit_method_reqs = Vec::new();
        let return_type = func
            .return_type
            .as_ref()
            .map(Type::from_ast)
            .unwrap_or(Type::Unit);
        if let Some(ast_ty) = func.return_type.as_ref() {
            self.reject_wasm_extended_float(&return_type, ast_ty.span);
        }
        self.current_return = Some(return_type);

        self.push_scope();
        for param in &func.parameters {
            let mut param_type = Type::from_ast(&param.param_type);
            if param.is_variadic {
                param_type = Type::Slice {
                    element: Box::new(param_type),
                };
            }
            self.reject_plain_void_value_type(&param_type, param.param_type.span);
            self.reject_wasm_extended_float(&param_type, param.param_type.span);
            self.bind(&param.name.name, param_type, param.is_mutable, param.span);
        }
        self.check_block(&func.body);
        self.pop_scope();
        self.current_return = None;
        let key = Self::free_fn_key(func);
        self.store_implicit_reqs(key.clone());
        self.store_implicit_method_reqs(key);
        self.current_type_params.clear();
    }
    fn check_impl_method(&mut self, self_ty: &Type, func: &ast::ImplFunction,
                         impl_type_params: &HashSet<String>) {
        self.current_type_params = impl_type_params.clone();
        self.current_type_params.extend(Self::type_param_names(func.generics.as_ref()));
        self.current_implicit_reqs = Vec::new();
        self.current_implicit_method_reqs = Vec::new();
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| self.substitute_self_type(&Type::from_ast(t), self_ty))
            .unwrap_or(Type::Unit);
        if let Some(ast_ty) = func.return_type.as_ref() {
            self.reject_wasm_extended_float(&return_type, ast_ty.span);
        }
        self.current_return = Some(return_type);

        self.push_scope();
        for param in &func.parameters {
            let mut param_type =
                self.substitute_self_type(&Type::from_ast(&param.param_type), self_ty);
            if param.is_variadic {
                param_type = Type::Slice {
                    element: Box::new(param_type),
                };
            }
            self.reject_plain_void_value_type(&param_type, param.param_type.span);
            self.reject_wasm_extended_float(&param_type, param.param_type.span);
            self.bind(&param.name.name, param_type, param.is_mutable, param.span);
        }
        self.check_block(&func.body);
        self.pop_scope();
        self.current_return = None;
        let key = Self::impl_method_key(self_ty, func);
        self.store_implicit_reqs(key.clone());
        self.store_implicit_method_reqs(key);
        self.current_type_params.clear();
    }

    fn check_impl_cast(&mut self, self_ty: &Type, cast: &ast::ImplCast,
                       impl_type_params: &HashSet<String>) {
        self.current_type_params = impl_type_params.clone();
        let return_type = self.substitute_self_type(&Type::from_ast(&cast.target_type), self_ty);
        self.reject_wasm_extended_float(&return_type, cast.target_type.span);
        self.current_return = Some(return_type);

        self.push_scope();
        for param in &cast.parameters {
            let param_type = self.substitute_self_type(&Type::from_ast(&param.param_type), self_ty);
            self.reject_plain_void_value_type(&param_type, param.param_type.span);
            self.reject_wasm_extended_float(&param_type, param.param_type.span);
            self.bind(&param.name.name, param_type, param.is_mutable, param.span);
        }
        self.check_block(&cast.body);
        self.pop_scope();
        self.current_return = None;
        self.current_type_params.clear();
    }

    fn check_block(&mut self, block: &ast::Block) {
        self.push_scope();
        for stmt in &block.statements {
            self.check_statement(stmt);
        }
        self.pop_scope();
    }

    fn check_block_value(&mut self, block: &ast::Block, expected: Option<&Type>) -> Type {
        if block.statements.is_empty() {
            return Type::Unit;
        }
        self.push_scope();
        for stmt in &block.statements[..block.statements.len() - 1] {
            self.check_statement(stmt);
        }
        let last = &block.statements[block.statements.len() - 1];
        let ty = match &last.kind {
            ast::StatementKind::Expression(expr) => self.check_expr(expr, expected),
            // Diverging statements — control flow never reaches the merge point.
            ast::StatementKind::Return(_)
            | ast::StatementKind::Break(_)
            | ast::StatementKind::Continue => {
                self.check_statement(last);
                Type::Never
            }
            _ => {
                self.check_statement(last);
                Type::Unit
            }
        };
        self.pop_scope();
        ty
    }

    fn check_statement(&mut self, stmt: &ast::Statement) {
        match &stmt.kind {
            ast::StatementKind::Block(block) => self.check_block(block),
            ast::StatementKind::Let(let_stmt) => {
                // Two binding forms:
                // - `T x = expr;`  — declared type comes from the annotation.
                // - `let x = expr;` — inferred binding: the declared type is
                //   taken from the initializer and materialized into the AST
                //   after type checking (see populate_inferred_let_types).
                if let Some(annotation) = &let_stmt.type_annotation {
                    let declared = self.type_from_ast(annotation);
                    self.reject_plain_void_value_type(&declared, annotation.span);
                    self.reject_wasm_extended_float(&declared, annotation.span);

                    if let Some(init) = &let_stmt.initializer {
                        let init_type = self.check_expr(init, Some(&declared));
                        if !self.lambdas.frames.is_empty() && !self.lambda_copy_type(&init_type) {
                            self.consume_capture(init);
                        }
                        if !self.is_assignable(&declared, &init_type)
                            && !self.is_implicitly_castable(&init_type, &declared)
                        {
                            self.error(msg::type_mismatch(&declared, &init_type), init.span);
                        }
                    }

                    Self::bind_let_pattern(self, let_stmt, declared);
                } else {
                    let Some(init) = &let_stmt.initializer else {
                        self.error(msg::inferred_let_needs_initializer(), stmt.span);
                        return;
                    };
                    let inferred = self.check_expr(init, None);
                    if !self.lambdas.frames.is_empty() && !self.lambda_copy_type(&inferred) {
                        self.consume_capture(init);
                    }
                    if Self::is_void_like(&inferred) {
                        self.error(msg::void_func_cannot_return_value(), init.span);
                        return;
                    }
                    self.inferred_lets.insert(
                        (stmt.span.start, stmt.span.end),
                        (inferred.clone(), let_stmt.pattern.span),
                    );
                    Self::bind_let_pattern(self, let_stmt, inferred);
                }
            }
            ast::StatementKind::Expression(expr) => {
                self.check_expr(expr, None);
            }
            ast::StatementKind::Return(value) => {
                if self.defer_depth > 0 {
                    self.error(msg::return_not_allowed_in_defer(), stmt.span);
                }
                if !self.lambdas.frames.is_empty() {
                    let expected = self.current_return.clone();
                    let found = value.as_ref().map(|value| {
                        let found = self.check_expr(value, expected.as_ref());
                        if !self.lambda_copy_type(&found) {
                            self.consume_capture(value);
                        }
                        found
                    }).unwrap_or(Type::Unit);
                    if let Some(frame) = self.lambdas.frames.last_mut() {
                        frame.returns.push(found);
                    }
                    return;
                }
                let expected = self.current_return.clone().unwrap_or(Type::Unit);
                match value {
                    Some(expr) => {
                        let found = self.check_expr(expr, Some(&expected));
                        if Self::is_void_like(&expected) {
                            self.error(msg::void_func_cannot_return_value(), expr.span);
                        } else if !self.is_assignable(&expected, &found)
                            && !self.is_implicitly_castable(&found, &expected)
                        {
                            self.error(msg::return_type_mismatch(&expected, &found), expr.span);
                        }
                    }
                    None => {
                        if !Self::is_void_like(&expected) {
                            self.error(msg::return_type_mismatch_unit(&expected), stmt.span);
                        }
                    }
                }
            }
            ast::StatementKind::Break(value) => {
                if self.defer_depth > 0 {
                    self.error(msg::break_not_allowed_in_defer(), stmt.span);
                }
                if let Some(expr) = value {
                    self.check_expr(expr, None);
                }
            }
            ast::StatementKind::Continue => {
                if self.defer_depth > 0 {
                    self.error(msg::continue_not_allowed_in_defer(), stmt.span);
                }
            }
            ast::StatementKind::Defer(inner) => {
                self.defer_depth += 1;
                self.check_statement(inner);
                self.defer_depth -= 1;
            }
        }
    }

    /// Bind an identifier or tuple let pattern to its declared (or inferred) type:
    /// registers the local(s), static/volatile qualifiers, and the hover type.
    fn bind_let_pattern(&mut self, let_stmt: &ast::LetStatement, declared: Type) {
        self.bind_pattern(
            &let_stmt.pattern,
            &declared,
            let_stmt.is_mutable,
            let_stmt.is_static,
            let_stmt.is_volatile,
        );
    }

    fn bind_pattern(
        &mut self,
        pattern: &ast::Pattern,
        declared: &Type,
        is_mutable: bool,
        is_static: bool,
        is_volatile: bool,
    ) {
        match &pattern.kind {
            ast::PatternKind::Identifier(ident) => {
                self.bind(&ident.name, declared.clone(), is_mutable, pattern.span);
                if let Some(scope) = self.static_vars.last_mut()
                    && is_static
                {
                    scope.insert(ident.name.clone());
                }
                if let Some(scope) = self.volatile_vars.last_mut()
                    && is_volatile
                {
                    scope.insert(ident.name.clone());
                }
                // Record for hover: variable name gets its declared type
                self.expr_types
                    .insert((pattern.span.start, pattern.span.end), declared.to_string());
            }
            ast::PatternKind::Wildcard => {}
            ast::PatternKind::Tuple(sub_patterns) => match declared {
                Type::Tuple(elem_types) => {
                    if sub_patterns.len() != elem_types.len() {
                        self.error(
                            format!(
                                "cannot unpack tuple of {} elements into {} variables",
                                elem_types.len(),
                                sub_patterns.len()
                            ),
                            pattern.span,
                        );
                        return;
                    }
                    for (sub_pat, elem_ty) in sub_patterns.iter().zip(elem_types.iter()) {
                        self.bind_pattern(sub_pat, elem_ty, is_mutable, is_static, is_volatile);
                    }
                    self.expr_types
                        .insert((pattern.span.start, pattern.span.end), declared.to_string());
                }
                _ => {
                    self.error(
                        format!(
                            "cannot destructure non-tuple type `{}` with tuple pattern",
                            declared
                        ),
                        pattern.span,
                    );
                }
            },
            _ => {
                self.error(
                    "destructuring pattern is not supported in let statement".to_string(),
                    pattern.span,
                );
            }
        }
    }

    /// If `block` is a lone `if` expression statement (an `else if` arm),
    /// return its parts so the checker can walk the chain iteratively
    /// instead of nesting one frame per arm.
    fn as_else_if_arm(
        else_branch: &Option<ast::Block>,
    ) -> Option<(&Box<ast::Expression>, &ast::Block, &Option<ast::Block>)> {
        let block = else_branch.as_ref()?;
        if block.statements.len() != 1 {
            return None;
        }
        if let ast::StatementKind::Expression(next) = &block.statements[0].kind {
            if let ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } = next.kind.as_ref()
            {
                return Some((condition, then_branch, else_branch));
            }
        }
        None
    }

    pub(crate) fn check_expr(&mut self, expr: &ast::Expression, expected: Option<&Type>) -> Type {
        if self.expression_depth >= MAX_TYPE_CHECK_DEPTH {
            self.error(
                format!(
                    "type-check expression nesting exceeds maximum depth ({MAX_TYPE_CHECK_DEPTH})"
                ),
                expr.span,
            );
            return Type::Unknown;
        }
        self.expression_depth += 1;
        let ty = self.check_expr_inner(expr, expected);
        self.expression_depth -= 1;
        ty
    }

    fn check_expr_inner(&mut self, expr: &ast::Expression, expected: Option<&Type>) -> Type {
        self.observe_lambda_access(expr);
        #[expect(
            clippy::match_like_matches_macro,
            reason = "two-column (primitive, literal) table reads better as a match than matches!"
        )]
        let ty = match expr.kind.as_ref() {
            ast::ExpressionKind::Lambda { parameters, body } => {
                self.check_lambda(parameters, body, expected, expr.span)
            }
            ast::ExpressionKind::Literal(literal) => {
                self.literal_type(literal, expected, &expr.span)
            }
            ast::ExpressionKind::Identifier(ident) => match self.lookup_type(&ident.name) {
                Some(ty) => ty,
                None => {
                    // Bare payload-less enum constructor with expected-type
                    // inference: `Optional<i32> x = None;` / `Result<i32,str> r = Err("x")`.
                    if let Some(bare) = self.check_bare_enum_constructor(
                        &ident.name,
                        &[] as &[&ast::Expression],
                        expected,
                        &expr.span,
                    ) {
                        return bare;
                    }
                    if let Some(sigs) = self.functions.get(&ident.name).and_then(|syms| {
                        syms.first().and_then(|sym| self.function_symbols.get(sym))
                    }) {
                        return Type::Function {
                            params: sigs.params.clone(),
                            return_type: Box::new(sigs.return_type.clone()),
                        };
                    }
                    if let Some(sigs) = self.imported_functions.get(&ident.name)
                        && let Some(sig) = sigs.first()
                    {
                        return Type::Function {
                            params: sig.params.clone(),
                            return_type: Box::new(sig.return_type.clone()),
                        };
                    }
                    if self.known_types.contains_key(&ident.name) {
                        return Type::Named {
                            path: vec![ident.name.clone()],
                            generics: Vec::new(),
                        };
                    }
                    let suggestion = self.identifier_suggestion(&ident.name);
                    self.error(
                        msg::unknown_identifier(&ident.name, &suggestion),
                        ident.span,
                    );
                    Type::Unknown
                }
            },
            ast::ExpressionKind::TypeName(ty) => Type::from_ast(ty),
            ast::ExpressionKind::Unary { operator, operand } => {
                // Signed literals: `-n` / `+n` are range-checked as a single
                // value (so `i8 x = -128` is legal while `u8 x = -5` errors)
                // instead of checking the positive magnitude in isolation.
                if matches!(
                    operator,
                    ast::UnaryOperator::Minus | ast::UnaryOperator::Plus
                ) && let ast::ExpressionKind::Literal(ast::Literal::Integer(n)) =
                    operand.kind.as_ref()
                {
                    let magnitude = *n as u128;
                    let negated = *operator == ast::UnaryOperator::Minus;
                    self.type_integer_literal_value(magnitude, negated, expected, &expr.span)
                } else {
                    let operand_expected =
                        expected.filter(|ty| self.is_numeric_type(ty) || is_bool(ty));
                    let operand_ty = self.check_expr(operand, operand_expected);
                    match operator {
                        ast::UnaryOperator::Plus | ast::UnaryOperator::Minus => {
                            if !self.is_numeric_type(&operand_ty) {
                                // For Minus on non-primitive types, try __neg overload
                                if *operator == ast::UnaryOperator::Minus
                                    && !self.is_primitive_type(&operand_ty)
                                {
                                    if let Some(result_ty) = self.resolve_method_overload_types(
                                        &operand_ty,
                                        "__neg",
                                        &[],
                                        None,
                                        MethodCallStyle::Instance,
                                        None,
                                        expr.span,
                                    ) {
                                        result_ty
                                    } else {
                                        self.error(
                                            format!(
                                                "unary +/- requires numeric operand, found {}",
                                                operand_ty
                                            ),
                                            expr.span,
                                        );
                                        operand_ty
                                    }
                                } else {
                                    self.error(
                                        format!(
                                            "unary +/- requires numeric operand, found {}",
                                            operand_ty
                                        ),
                                        expr.span,
                                    );
                                    operand_ty
                                }
                            } else {
                                operand_ty
                            }
                        }
                        ast::UnaryOperator::Dereference => {
                            let operand_ty_clone = operand_ty.clone();
                            match operand_ty {
                                Type::Pointer { inner, .. } | Type::Reference { inner, .. } => {
                                    *inner
                                }
                                _ => {
                                    self.error(
                                        format!(
                                            "dereference requires pointer or reference operand, found {}",
                                            operand_ty_clone
                                        ),
                                        expr.span,
                                    );
                                    operand_ty_clone
                                }
                            }
                        }
                        ast::UnaryOperator::Not => {
                            if is_bool(&operand_ty) {
                                Type::Primitive(ast::PrimitiveType::Bool)
                            } else if !self.is_primitive_type(&operand_ty) {
                                if let Some(result_ty) = self.resolve_method_overload_types(
                                    &operand_ty,
                                    "__not",
                                    &[],
                                    None,
                                    MethodCallStyle::Instance,
                                    None,
                                    expr.span,
                                ) {
                                    result_ty
                                } else {
                                    self.error(
                                        format!("logical not requires bool, found {}", operand_ty),
                                        expr.span,
                                    );
                                    Type::Primitive(ast::PrimitiveType::Bool)
                                }
                            } else {
                                self.error(
                                    format!("logical not requires bool, found {}", operand_ty),
                                    expr.span,
                                );
                                Type::Primitive(ast::PrimitiveType::Bool)
                            }
                        }
                        ast::UnaryOperator::BitwiseNot => {
                            if self.is_integer_type(&operand_ty) {
                                operand_ty
                            } else if !self.is_primitive_type(&operand_ty) {
                                if let Some(result_ty) = self.resolve_method_overload_types(
                                    &operand_ty,
                                    "__bitnot",
                                    &[],
                                    None,
                                    MethodCallStyle::Instance,
                                    None,
                                    expr.span,
                                ) {
                                    result_ty
                                } else {
                                    self.error(
                                        format!(
                                            "bitwise not requires integer, found {}",
                                            operand_ty
                                        ),
                                        expr.span,
                                    );
                                    operand_ty
                                }
                            } else {
                                self.error(
                                    format!("bitwise not requires integer, found {}", operand_ty),
                                    expr.span,
                                );
                                operand_ty
                            }
                        }
                        ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement => {
                            if !self.is_incdec_type(&operand_ty) {
                                self.error(
                                    "increment/decrement requires numeric or pointer operand",
                                    expr.span,
                                );
                            }
                            operand_ty
                        }
                    }
                }
            }
            ast::ExpressionKind::Postfix { operator, operand } => {
                let operand_ty = self.check_expr(operand, None);
                match operator {
                    ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement => {
                        if !self.is_incdec_type(&operand_ty) {
                            self.error(
                                format!("increment/decrement requires numeric or pointer operand, found {}", operand_ty),
                                expr.span,
                            );
                        }
                        operand_ty
                    }
                    _ => {
                        self.error(msg::invalid_postfix_operator(), expr.span);
                        Type::Unknown
                    }
                }
            }
            ast::ExpressionKind::Binary {
                left,
                operator,
                right,
            } => match operator {
                ast::BinaryOperator::Add
                | ast::BinaryOperator::Subtract
                | ast::BinaryOperator::Multiply
                | ast::BinaryOperator::Divide
                | ast::BinaryOperator::Modulo => {
                    let numeric_expected = expected.filter(|ty| self.is_numeric_type(ty));
                    let left_ty = self.check_expr(left, numeric_expected);
                    let right_ty = self.check_expr(right, numeric_expected);
                    // Pointer arithmetic: p + i, i + p, p - i (str is a byte
                    // pointer, so s + n / s - n work too).
                    if let Some(ptr_ty) = self.pointer_arith_result(&left_ty, &right_ty, operator) {
                        return ptr_ty;
                    }
                    if self.is_numeric_type(&left_ty)
                        && self.is_numeric_type(&right_ty)
                        && let Some(common) = self.common_numeric_type(&left_ty, &right_ty)
                    {
                        return common;
                    }

                    if self.defer_operator_if_generic(&left_ty, &right_ty, operator, expr.span) {
                        return left_ty;
                    }

                    if let Some(ty) =
                        self.resolve_operator_overload(&left_ty, &right_ty, operator, expr)
                    {
                        return ty;
                    }

                    self.error(
                        format!(
                            "binary operator requires numeric operands, got {} and {}",
                            left_ty, right_ty
                        ),
                        expr.span,
                    );
                    Type::Unknown
                }
                ast::BinaryOperator::Equal
                | ast::BinaryOperator::NotEqual
                | ast::BinaryOperator::Less
                | ast::BinaryOperator::Greater
                | ast::BinaryOperator::LessEqual
                | ast::BinaryOperator::GreaterEqual => {
                    let left_ty = self.check_expr(left, None);
                    let right_ty = self.check_expr(right, None);
                    // char vs byte (u8/u16): both widen losslessly, so
                    // `buf[i] == 'x'` needs no cast.
                    let char_ty = Type::Primitive(ast::PrimitiveType::Char);
                    let is_byte = |ty: &Type| {
                        matches!(
                            ty,
                            Type::Primitive(ast::PrimitiveType::U8 | ast::PrimitiveType::U16)
                        )
                    };
                    if (left_ty == char_ty && (right_ty == char_ty || is_byte(&right_ty)))
                        || (right_ty == char_ty && is_byte(&left_ty))
                    {
                        return Type::Primitive(ast::PrimitiveType::Bool);
                    }
                    if self.is_numeric_type(&left_ty) && self.is_numeric_type(&right_ty) {
                        if self.common_numeric_type(&left_ty, &right_ty).is_none() {
                            self.error(
                                format!(
                                    "comparison operands must be compatible, got {} and {}",
                                    left_ty, right_ty
                                ),
                                expr.span,
                            );
                        }
                        return Type::Primitive(ast::PrimitiveType::Bool);
                    }

                    if left_ty == right_ty
                        && matches!(left_ty, Type::Pointer { .. } | Type::Reference { .. })
                    {
                        return Type::Primitive(ast::PrimitiveType::Bool);
                    }

                    // Matching generic type params: the comparison is checked
                    // at monomorphization time via the recorded implicit guard.
                    if left_ty == right_ty
                        && let Type::Named { path, generics } = &left_ty
                        && path.len() == 1
                        && generics.is_empty()
                        && !self.known_types.contains_key(&path[0])
                    {
                        self.defer_operator_if_generic(&left_ty, &right_ty, operator, expr.span);
                        return Type::Primitive(ast::PrimitiveType::Bool);
                    }

                    if let Some(ty) =
                        self.resolve_operator_overload(&left_ty, &right_ty, operator, expr)
                    {
                        let bool_ty = Type::Primitive(ast::PrimitiveType::Bool);
                        if !self.is_implicitly_castable(&ty, &bool_ty) {
                            self.error(
                                format!("comparison operator must return bool, found {}", ty),
                                expr.span,
                            );
                        }
                        return bool_ty;
                    }

                    if left_ty != right_ty {
                        if self.defer_operator_if_generic(&left_ty, &right_ty, operator, expr.span)
                        {
                            return Type::Primitive(ast::PrimitiveType::Bool);
                        }
                        self.error(
                            format!(
                                "comparison operands must match, got {} and {}",
                                left_ty, right_ty
                            ),
                            expr.span,
                        );
                    }
                    Type::Primitive(ast::PrimitiveType::Bool)
                }
                ast::BinaryOperator::LogicalAnd | ast::BinaryOperator::LogicalOr => {
                    let left_ty = self.check_expr(left, None);
                    let right_ty = self.check_expr(right, None);
                    let bool_ty = Type::Primitive(ast::PrimitiveType::Bool);
                    if !is_bool(&left_ty) && !self.is_implicitly_castable(&left_ty, &bool_ty) {
                        self.error(
                            format!("logical operator requires bool operands, found {}", left_ty),
                            expr.span,
                        );
                    }
                    if !is_bool(&right_ty) && !self.is_implicitly_castable(&right_ty, &bool_ty) {
                        self.error(
                            format!(
                                "logical operator requires bool operands, found {}",
                                right_ty
                            ),
                            expr.span,
                        );
                    }
                    Type::Primitive(ast::PrimitiveType::Bool)
                }
                ast::BinaryOperator::BitwiseAnd
                | ast::BinaryOperator::BitwiseOr
                | ast::BinaryOperator::BitwiseXor
                | ast::BinaryOperator::LeftShift
                | ast::BinaryOperator::RightShift => {
                    let left_ty = self.check_expr(left, None);
                    let right_ty = self.check_expr(right, None);
                    if self.is_integer_type(&left_ty) && self.is_integer_type(&right_ty) {
                        if matches!(
                            operator,
                            ast::BinaryOperator::LeftShift | ast::BinaryOperator::RightShift
                        ) {
                            return left_ty;
                        }
                        return self
                            .common_numeric_type(&left_ty, &right_ty)
                            .unwrap_or(left_ty);
                    }

                    if self.defer_operator_if_generic(&left_ty, &right_ty, operator, expr.span) {
                        return left_ty;
                    }

                    // Try operator overload for non-primitive types
                    if !self.is_primitive_type(&left_ty)
                        && let Some(result_ty) =
                            self.resolve_operator_overload(&left_ty, &right_ty, operator, expr)
                    {
                        return result_ty;
                    }

                    if self.is_primitive_type(&left_ty) || self.is_primitive_type(&right_ty) {
                        self.error(
                            msg::bitwise_operator_requires_integers(&left_ty, &right_ty),
                            expr.span,
                        );
                    }

                    Type::Unknown
                }
                ast::BinaryOperator::Assign => {
                    let left_ty = self.check_expr(left, None);
                    let right_ty = self.check_expr(right, None);
                    if !self.lambdas.frames.is_empty() && !self.lambda_copy_type(&right_ty) {
                        self.consume_capture(right);
                    }
                    if left_ty != right_ty && !self.is_implicitly_castable(&right_ty, &left_ty) {
                        self.error(
                            msg::assignment_type_mismatch(&left_ty, &right_ty),
                            expr.span,
                        );
                    }
                    // Check mutability of assignment target
                    if let ast::ExpressionKind::Identifier(ident) = left.kind.as_ref()
                        && let Some((_, is_mut)) = self.lookup(&ident.name)
                        && !is_mut
                    {
                        self.error(msg::cannot_assign_const(&ident.name), ident.span);
                    }
                    if let ast::ExpressionKind::FieldAccess { object, .. } = left.kind.as_ref()
                        && let ast::ExpressionKind::Identifier(ident) = object.kind.as_ref()
                        && let Some((_, is_mut)) = self.lookup(&ident.name)
                        && !is_mut
                    {
                        self.error(msg::cannot_assign_const_field(&ident.name), ident.span);
                    }
                    left_ty
                }
                ast::BinaryOperator::AddAssign
                | ast::BinaryOperator::SubtractAssign
                | ast::BinaryOperator::MultiplyAssign
                | ast::BinaryOperator::DivideAssign
                | ast::BinaryOperator::ModuloAssign
                | ast::BinaryOperator::BitwiseAndAssign
                | ast::BinaryOperator::BitwiseOrAssign
                | ast::BinaryOperator::BitwiseXorAssign
                | ast::BinaryOperator::LeftShiftAssign
                | ast::BinaryOperator::RightShiftAssign => {
                    let left_ty = self.check_expr(left, None);
                    let right_ty = self.check_expr(right, None);
                    // Check mutability of assignment target
                    if let ast::ExpressionKind::Identifier(ident) = left.kind.as_ref()
                        && let Some((_, is_mut)) = self.lookup(&ident.name)
                        && !is_mut
                    {
                        self.error(msg::cannot_assign_const(&ident.name), ident.span);
                    }
                    if let ast::ExpressionKind::FieldAccess { object, .. } = left.kind.as_ref()
                        && let ast::ExpressionKind::Identifier(ident) = object.kind.as_ref()
                        && let Some((_, is_mut)) = self.lookup(&ident.name)
                        && !is_mut
                    {
                        self.error(msg::cannot_assign_const_field(&ident.name), ident.span);
                    }

                    if self.is_primitive_type(&left_ty) {
                        if left_ty != right_ty && !self.is_implicitly_castable(&right_ty, &left_ty)
                        {
                            self.error(
                                msg::assignment_type_mismatch(&left_ty, &right_ty),
                                expr.span,
                            );
                        }
                        // For bitwise compound assignments on primitive types, enforce integer operands
                        if matches!(
                            operator,
                            ast::BinaryOperator::BitwiseAndAssign
                                | ast::BinaryOperator::BitwiseOrAssign
                                | ast::BinaryOperator::BitwiseXorAssign
                                | ast::BinaryOperator::LeftShiftAssign
                                | ast::BinaryOperator::RightShiftAssign
                        ) && (!self.is_integer_type(&left_ty)
                            || !self.is_integer_type(&right_ty))
                        {
                            self.error(
                                msg::bitwise_assignment_requires_integers(&left_ty, &right_ty),
                                expr.span,
                            );
                        }
                    } else {
                        let bin_op = match operator {
                            ast::BinaryOperator::AddAssign => ast::BinaryOperator::Add,
                            ast::BinaryOperator::SubtractAssign => ast::BinaryOperator::Subtract,
                            ast::BinaryOperator::MultiplyAssign => ast::BinaryOperator::Multiply,
                            ast::BinaryOperator::DivideAssign => ast::BinaryOperator::Divide,
                            ast::BinaryOperator::ModuloAssign => ast::BinaryOperator::Modulo,
                            ast::BinaryOperator::BitwiseAndAssign => {
                                ast::BinaryOperator::BitwiseAnd
                            }
                            ast::BinaryOperator::BitwiseOrAssign => ast::BinaryOperator::BitwiseOr,
                            ast::BinaryOperator::BitwiseXorAssign => {
                                ast::BinaryOperator::BitwiseXor
                            }
                            ast::BinaryOperator::LeftShiftAssign => ast::BinaryOperator::LeftShift,
                            ast::BinaryOperator::RightShiftAssign => {
                                ast::BinaryOperator::RightShift
                            }
                            _ => unreachable!(),
                        };
                        if let Some(res_ty) =
                            self.resolve_operator_overload(&left_ty, &right_ty, &bin_op, expr)
                        {
                            if res_ty != left_ty && !self.is_implicitly_castable(&res_ty, &left_ty)
                            {
                                self.error(
                                    msg::assignment_type_mismatch(&left_ty, &res_ty),
                                    expr.span,
                                );
                            }
                        }
                    }

                    left_ty
                }
                ast::BinaryOperator::Range => {
                    let left_ty = self.check_expr(left, None);
                    let right_ty = self.check_expr(right, None);
                    if !self.is_integer_type(&left_ty) || !self.is_integer_type(&right_ty) {
                        self.error(
                            format!(
                                "range bounds must be integers, got {} and {}",
                                left_ty, right_ty
                            ),
                            expr.span,
                        );
                    } else if left_ty != right_ty {
                        self.error(
                            format!(
                                "range bounds must be the same type, got {} and {}",
                                left_ty, right_ty
                            ),
                            expr.span,
                        );
                    }
                    left_ty
                }
            },
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_ty = self.check_expr(condition, None);
                if !is_bool(&cond_ty) {
                    self.error(
                        format!("ternary condition must be bool, found {}", cond_ty),
                        condition.span,
                    );
                }
                let then_ty = self.check_expr(then_expr, expected);
                let else_ty = self.check_expr(else_expr, expected);
                if then_ty == else_ty {
                    then_ty
                } else if let Some(exp) = expected {
                    if self.is_assignable(exp, &then_ty) && self.is_assignable(exp, &else_ty) {
                        exp.clone()
                    } else {
                        self.error(
                            format!(
                                "ternary branches must have the same type, found {} and {}",
                                then_ty, else_ty
                            ),
                            expr.span,
                        );
                        then_ty
                    }
                } else {
                    self.error(
                        format!(
                            "ternary branches must have the same type, found {} and {}",
                            then_ty, else_ty
                        ),
                        expr.span,
                    );
                    then_ty
                }
            }
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                let val_ty = self.check_expr(value, None);
                let target_ty = match &val_ty {
                    Type::Optional { inner } => Some((**inner).clone()),
                    Type::Named { path, generics } => {
                        let name = path.last().map(|s| s.as_str());
                        if name == Some("Optional")
                            || name == Some("Result")
                            || name == Some("SysResult")
                        {
                            generics.first().cloned()
                        } else {
                            None
                        }
                    }
                    Type::Pointer {
                        is_mutable,
                        is_volatile,
                        inner,
                    } => Some(Type::Pointer {
                        is_mutable: *is_mutable,
                        is_volatile: *is_volatile,
                        inner: inner.clone(),
                    }),
                    _ => None,
                };

                if let Some(target) = target_ty {
                    let fallback_ty = self.check_expr(fallback, Some(&target));
                    if self.is_assignable(&target, &fallback_ty) {
                        target
                    } else {
                        self.error(
                            format!(
                                "unwrap-or fallback type mismatch: expected {}, found {}",
                                target, fallback_ty
                            ),
                            fallback.span,
                        );
                        target
                    }
                } else if val_ty != Type::Unknown {
                    self.error(
                        format!(
                            "unwrap-or operator '?' requires Optional, Result, or pointer on left-hand side, found {}",
                            val_ty
                        ),
                        value.span,
                    );
                    Type::Unknown
                } else {
                    Type::Unknown
                }
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Check else-if chains iteratively. Statement-level chains
                // nest If expressions (Silver has no If statement), and
                // holding one check_expr frame per arm exhausts the 32-deep
                // budget on legitimate long dispatch chains. Conditions and
                // bodies are still checked at the usual depth; only the
                // chaining itself no longer nests.
                let mut cond = condition;
                let mut then_b = then_branch;
                let mut else_b = else_branch;
                loop {
                    let cond_ty = self.check_expr(cond, None);
                    if !is_bool(&cond_ty) {
                        self.error(
                            format!("if condition must be bool, found {}", cond_ty),
                            cond.span,
                        );
                    }
                    self.check_block(then_b);
                    match Self::as_else_if_arm(else_b) {
                        Some((next_cond, next_then, next_else)) => {
                            cond = next_cond;
                            then_b = next_then;
                            else_b = next_else;
                        }
                        None => {
                            if let Some(block) = else_b {
                                self.check_block(block);
                            }
                            break;
                        }
                    }
                }
                Type::Unit
            }
            ast::ExpressionKind::While { condition, body } => {
                let cond_ty = self.check_expr(condition, None);
                if !is_bool(&cond_ty) {
                    self.error(
                        format!("while condition must be bool, found {}", cond_ty),
                        condition.span,
                    );
                }
                self.check_block(body);
                Type::Unit
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.push_scope();
                let init_stmt = ast::Statement {
                    kind: ast::StatementKind::Let(init.clone()),
                    span: init.pattern.span,
                };
                self.check_statement(&init_stmt);
                let cond_ty = self.check_expr(condition, None);
                if !is_bool(&cond_ty) {
                    self.error(
                        format!("for condition must be bool, found {}", cond_ty),
                        condition.span,
                    );
                }
                self.check_expr(increment, None);
                self.check_block(body);
                self.pop_scope();
                Type::Unit
            }
            ast::ExpressionKind::Block(block) => self.check_block_value(block, expected),
            ast::ExpressionKind::Array(elements) => {
                if let Some(Type::Slice { element }) = expected {
                    for element_expr in elements {
                        let item_ty = self.check_expr(element_expr, Some(element));
                        if !self.is_assignable(element, &item_ty) {
                            self.error(
                                format!(
                                    "array element type mismatch: expected {}, found {}",
                                    element, item_ty
                                ),
                                element_expr.span,
                            );
                        }
                    }
                    Type::Slice {
                        element: element.clone(),
                    }
                } else {
                    for element_expr in elements {
                        self.check_expr(element_expr, None);
                    }
                    Type::Unknown
                }
            }
            ast::ExpressionKind::Initializer { items } => {
                for (position, item) in items.iter().enumerate() {
                    let (value, target) = match item {
                        ast::InitializerItem::Field { name, value } => (
                            value,
                            expected.and_then(|ty| self.resolve_field_access_type(ty, &name.name)),
                        ),
                        ast::InitializerItem::Positional(value)
                        | ast::InitializerItem::Index { value, .. } => {
                            if let ast::InitializerItem::Index { index, .. } = item {
                                self.check_expr(index, None);
                            }
                            let target = match expected {
                                Some(Type::Tuple(types)) => types.get(position).cloned(),
                                Some(Type::Array { element, .. } | Type::Slice { element }) => {
                                    Some((**element).clone())
                                }
                                _ => None,
                            };
                            (value, target)
                        }
                    };
                    let found = self.check_expr(value, target.as_ref());
                    if let Some(target) = target.as_ref()
                        && !self.is_assignable(target, &found)
                        && !self.is_implicitly_castable(&found, target)
                    {
                        self.error(msg::type_mismatch(target, &found), value.span);
                    }
                }
                expected.cloned().unwrap_or(Type::Unknown)
            }
            ast::ExpressionKind::Tuple(items) => {
                let elem_expected = match expected {
                    Some(Type::Tuple(elem_tys)) => elem_tys.as_slice(),
                    _ => &[],
                };
                let types = items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| self.check_expr(item, elem_expected.get(i)))
                    .collect();
                Type::Tuple(types)
            }
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                let from = self.check_expr(expression, None);
                let to = Type::from_ast(target_type);
                self.reject_wasm_extended_float(&to, target_type.span);
                if !self.is_castable(&from, &to) {
                    self.error(msg::invalid_cast(&from, &to), expr.span);
                }
                to
            }
            ast::ExpressionKind::Reference {
                expression,
                is_mutable: _is_mutable,
            } => {
                let inner = self.check_expr(expression, None);
                // Reject address-of on volatile variables (local or global):
                // the pointee could be observed changing underneath, so a plain
                // pointer view would bypass the volatile access guarantees.
                // Field chains and array element accesses are walked to the
                // root, so `&volatile_arr[0]` is rejected too.
                let mut root = expression;
                while let ast::ExpressionKind::FieldAccess { object, .. } = root.kind.as_ref() {
                    root = object;
                }
                while let ast::ExpressionKind::Index { object, .. } = root.kind.as_ref() {
                    root = object;
                }
                if let ast::ExpressionKind::Identifier(ident) = root.kind.as_ref() {
                    let is_local = self
                        .scopes
                        .iter()
                        .rev()
                        .any(|scope| scope.contains_key(ident.name.as_str()));
                    let volatile = if is_local {
                        self.is_volatile_local(&ident.name)
                    } else {
                        self.volatile_globals.contains(&ident.name)
                    };
                    if volatile {
                        self.error(
                            format!(
                                "cannot take the address of volatile variable '{}'",
                                ident.name
                            ),
                            expr.span,
                        );
                    }
                }
                // Constness of &expr depends on the source variable:
                // if the source is const, the pointer is immutable regardless of syntax.
                let source_is_mutable =
                    if let ast::ExpressionKind::Identifier(ident) = expression.kind.as_ref() {
                        self.lookup(&ident.name)
                            .map(|(_, mutability)| mutability)
                            .unwrap_or(true) // unknown/global → assume mutable
                    } else {
                        true // non-identifier expression → assume mutable
                    };
                Type::Pointer {
                    is_mutable: source_is_mutable,
                    is_volatile: false,
                    inner: Box::new(inner),
                }
            }
            ast::ExpressionKind::Move(inner) => {
                let ty = self.check_expr(inner, None);
                if let ast::ExpressionKind::Identifier(ident) = inner.kind.as_ref() {
                    self.mark_moved(&ident.name);
                    if self.is_static_var(&ident.name) {
                        self.error(
                            format!("cannot move out of static variable '{}'", ident.name),
                            expr.span,
                        );
                    }
                } else if matches!(
                    inner.kind.as_ref(),
                    ast::ExpressionKind::FieldAccess { .. }
                        | ast::ExpressionKind::Index { .. }
                        | ast::ExpressionKind::Unary {
                            operator: ast::UnaryOperator::Dereference,
                            ..
                        }
                ) {
                    // Any addressable place may transfer ownership.
                } else {
                    self.error(msg::move_operand_place(), inner.span);
                }
                ty
            }
            ast::ExpressionKind::Comptime(inner) => self.check_expr(inner, None),
            ast::ExpressionKind::Launch(inner) => {
                // WebAssembly (WASI preview1) is single-threaded: reject
                // `launch` at typecheck time so `agc check` catches it and
                // users get a target-specific message before codegen.
                if crate::codegen::abi::target_is_wasm(Some(&self.target_triple.clone())) {
                    self.error(
                        "`launch` is not supported on WebAssembly targets: WASI preview1 is single-threaded".to_string(),
                        inner.span,
                    );
                }
                // `launch f(args...)`: the callee must be a directly-named
                // function (no indirect/fn-pointer launch in v1). The wrapped
                // call is validated through the normal overload path, which
                // resolves overloads, checks argument types, and records
                // monomorph requests. The result is a Task<ret>.
                //
                // The structural Send gate runs on every argument: launch
                // MOVES its args into the child thread, so shared-ownership
                // (Rc), GC-handle, raw-pointer, and reference types are
                // rejected here.
                let ast::ExpressionKind::Call {
                    function,
                    arguments,
                } = inner.kind.as_ref()
                else {
                    self.error(
                        "launch operand must be a call to a named function ".to_string()
                            + "(function pointers are not supported in v1)",
                        inner.span,
                    );
                    return Type::Unknown;
                };
                let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref() else {
                    self.error(
                        "launch operand must be a call to a named function ".to_string()
                            + "(function pointers are not supported in v1)",
                        inner.span,
                    );
                    return Type::Unknown;
                };
                let (call_ty, arg_types) =
                    self.resolve_overload_with_explicit(ident, &[], arguments, inner.span);
                for (arg, arg_ty) in arguments.iter().zip(arg_types.iter()) {
                    if let Err(reason) = self.check_send(arg_ty) {
                        self.error(
                            format!("launch argument of type {arg_ty} is not Send: {reason}"),
                            arg.span,
                        );
                    }
                }
                Type::Task(Box::new(call_ty))
            }
            ast::ExpressionKind::Wait(inner) => {
                // `wait t` joins the Task t (consuming it) and yields its
                // result type. Non-Task operands are rejected.
                match self.check_expr(inner, None) {
                    Type::Task(inner_ty) => *inner_ty,
                    other => {
                        self.error(msg::wait_requires_task(&other), expr.span);
                        Type::Unknown
                    }
                }
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                let checked_callee = if matches!(
                    function.kind.as_ref(),
                    ast::ExpressionKind::Identifier(_)
                ) {
                    None
                } else {
                    Some(self.check_expr(function, None))
                };
                if let Some(result) = self.check_lambda_call(
                    function, arguments, expr.span, checked_callee.as_ref(),
                ) {
                    return result;
                }
                if let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref() {
                    // Bare enum constructor with expected-type inference:
                    // `Optional<i32> x = Some(42);` / `Result<i32,str> r = Ok(7);`.
                    if !self.functions.contains_key(&ident.name) {
                        let arg_refs: Vec<&ast::Expression> = arguments.iter().collect();
                        if let Some(bare) = self.check_bare_enum_constructor(
                            &ident.name,
                            &arg_refs,
                            expected,
                            &expr.span,
                        ) {
                            return bare;
                        }
                    }
                    // Atomic intrinsics (`__atomic_*`) are recognized by name and
                    // type-checked by a dedicated hook; they never live in the
                    // symbol table.
                    if let Some(result) =
                        self.try_resolve_atomic_intrinsic(&ident.name, arguments, expr.span)
                    {
                        return result;
                    }
                    // Check if this is a known function name first
                    if self.functions.contains_key(&ident.name) {
                        self.resolve_overload(ident, arguments, expr.span)
                    } else {
                        // Type-check the function expression — may be a fn pointer variable
                        let fn_ty = self.check_expr(function, None);
                        if let Type::Function {
                            params,
                            return_type,
                        } = &fn_ty
                        {
                            let arg_types: Vec<Type> = arguments
                                .iter()
                                .enumerate()
                                .map(|(index, arg)| {
                                    if matches!(arg.kind.as_ref(), ast::ExpressionKind::Lambda { .. }) {
                                        self.check_expr(arg, params.get(index))
                                    } else {
                                        self.check_expr_with_literal_naturals(arg)
                                    }
                                })
                                .collect();
                            if arguments.len() != params.len() {
                                self.error(
                                    format!(
                                        "function '{}' expected {} arguments, got {}",
                                        ident.name,
                                        params.len(),
                                        arguments.len()
                                    ),
                                    expr.span,
                                );
                            }
                            for (i, (param_ty, arg_ty)) in
                                params.iter().zip(arg_types.iter()).enumerate()
                            {
                                self.capture_argument(&arguments[i], param_ty);
                                // Integer literals narrow to the parameter type
                                // when the value fits (overflow otherwise).
                                let literal_ok = Self::literal_integer_value(&arguments[i])
                                    .is_some_and(|value| {
                                        matches!(param_ty, Type::Primitive(prim)
                                            if Self::integer_value_fits(value, prim))
                                    });
                                if !literal_ok
                                    && !self.is_assignable(param_ty, arg_ty)
                                    && !self.is_implicitly_castable(arg_ty, param_ty)
                                {
                                    self.error(
                                        format!(
                                            "mismatched argument type for parameter {}: expected {}, got {}",
                                            i, param_ty, arg_ty
                                        ),
                                        arguments[i].span,
                                    );
                                }
                            }
                            *return_type.clone()
                        } else {
                            self.error(msg::not_callable(&ident.name), expr.span);
                            Type::Unknown
                        }
                    }
                } else if let ast::ExpressionKind::TypeName(ty) = function.kind.as_ref() {
                    if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                        if named.path.len() == 1 {
                            let ident = &named.path[0];
                            if self.functions.contains_key(&ident.name) {
                                let explicit_types: Vec<Type> = named
                                    .generics
                                    .as_ref()
                                    .map(|gs| gs.iter().map(Type::from_ast).collect())
                                    .unwrap_or_default();
                                self.resolve_overload_with_explicit(
                                    ident,
                                    &explicit_types,
                                    arguments,
                                    expr.span,
                                )
                                .0
                            } else {
                                for arg in arguments {
                                    self.check_expr(arg, None);
                                }
                                self.error(msg::type_not_callable(&named.path[0].name), expr.span);
                                Type::Unknown
                            }
                        } else {
                            for arg in arguments {
                                self.check_expr(arg, None);
                            }
                            self.error(
                                "cannot call a namespaced type path; calls must use a single function name".to_string(),
                                expr.span,
                            );
                            Type::Unknown
                        }
                    } else {
                        for arg in arguments {
                            self.check_expr(arg, None);
                        }
                        self.error(msg::type_expression_not_callable(), expr.span);
                        Type::Unknown
                    }
                } else {
                    // Type-check the function expression and try indirect call
                    let fn_ty = checked_callee.unwrap_or_else(|| self.check_expr(function, None));
                    if let Type::Function {
                        params,
                        return_type,
                    } = &fn_ty
                    {
                        let arg_types: Vec<Type> = arguments
                            .iter()
                            .enumerate()
                            .map(|(index, arg)| {
                                if matches!(arg.kind.as_ref(), ast::ExpressionKind::Lambda { .. }) {
                                    self.check_expr(arg, params.get(index))
                                } else {
                                    self.check_expr_with_literal_naturals(arg)
                                }
                            })
                            .collect();
                        for (i, (param_ty, arg_ty)) in params.iter().zip(&arg_types).enumerate() {
                            self.capture_argument(&arguments[i], param_ty);
                            let literal_ok = Self::literal_integer_value(&arguments[i])
                                .is_some_and(|value| {
                                    matches!(param_ty, Type::Primitive(prim)
                                        if Self::integer_value_fits(value, prim))
                                });
                            if !literal_ok
                                && !self.is_assignable(param_ty, arg_ty)
                                && !self.is_implicitly_castable(arg_ty, param_ty)
                            {
                                self.error(
                                    format!(
                                        "mismatched argument type for parameter {}: expected {}, got {}",
                                        i, param_ty, arg_ty
                                    ),
                                    arguments[i].span,
                                );
                            }
                        }
                        if arguments.len() != params.len() {
                            self.error(
                                format!(
                                    "expected {} arguments, got {}",
                                    params.len(),
                                    arguments.len()
                                ),
                                expr.span,
                            );
                        }
                        *return_type.clone()
                    } else {
                        for arg in arguments {
                            self.check_expr(arg, None);
                        }
                        self.error(msg::expr_not_callable(&fn_ty), expr.span);
                        Type::Unknown
                    }
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                let style = self.method_call_style(receiver);
                let receiver_ty = if let ast::ExpressionKind::Identifier(ident) = receiver.kind.as_ref()
                    && style == MethodCallStyle::Static
                    && self.current_type_params.contains(&ident.name)
                {
                    Type::Named { path: vec![ident.name.clone()], generics: Vec::new() }
                } else {
                    self.check_expr(receiver, None)
                };
                // Check if this is an enum variant construction first
                if let Type::Named { path: ty_path, .. } = &receiver_ty
                    && ty_path.len() == 1
                {
                    let enum_name = &ty_path[0];
                    if let Some(enum_def) = self.enum_defs.get(enum_name)
                        && let Some(variant_info) = enum_def.variants.get(&method.name)
                    {
                        let expected_count = variant_info.payload.len();
                        let enum_type_params = enum_def.type_params.clone();
                        let mut receiver_mapping = HashMap::default();
                        if let Type::Named { generics, .. } = &receiver_ty {
                            for (param, concrete) in enum_type_params.iter().zip(generics.iter()) {
                                receiver_mapping.insert(param.clone(), concrete.clone());
                            }
                        }
                        let payload_types: Vec<ast::Type> = variant_info.payload.clone();
                        let expected_types: Vec<Type> = payload_types
                            .iter()
                            .map(|t| Type::from_ast(t).substitute(&receiver_mapping))
                            .collect();
                        if arguments.len() != expected_count {
                            self.error(
                                msg::variant_arg_count_mismatch(
                                    &method.name,
                                    expected_count,
                                    arguments.len(),
                                ),
                                expr.span,
                            );
                        } else {
                            // Collect argument types upfront (releasing the
                            // immutable borrow of enum_defs).
                            let mut arg_types: Vec<Type> = Vec::with_capacity(arguments.len());
                            for (i, arg) in arguments.iter().enumerate() {
                                arg_types.push(self.check_expr(arg, expected_types.get(i)));
                                // Move-in enforcement: an owned payload (Drop
                                // type, or a type param that may instantiate
                                // to one) passed as a named lvalue must be
                                // `move`d — otherwise the source keeps its
                                // drop flag and the enum's copy shares the
                                // same buffers (silent double owner).
                                let is_named_lvalue = matches!(
                                    arg.kind.as_ref(),
                                    ast::ExpressionKind::Identifier(_)
                                        | ast::ExpressionKind::FieldAccess { .. }
                                );
                                // Check the CONCRETE argument type (resolved
                                // through the expected payload type): in a
                                // concrete enum impl the arg is i64 even
                                // though the enum def still shows T.
                                if is_named_lvalue
                                    && !matches!(arg.kind.as_ref(), ast::ExpressionKind::Move(_))
                                    && self.type_has_drop_impl(&arg_types[i].to_ast())
                                {
                                    self.error(msg::payload_must_be_moved(&arg_types[i]), arg.span);
                                }
                            }
                            if !receiver_mapping.is_empty() {
                                return receiver_ty.clone();
                            }
                            // For a generic enum (`Optional.Some(x)`), infer the
                            // type args from the argument types so the result is
                            // `Optional<i32>` rather than bare `Optional`.
                            if !enum_type_params.is_empty() {
                                let mut mapping = HashMap::default();
                                let mut inferred = true;
                                for (i, arg_ty) in arg_types.iter().enumerate() {
                                    if let Some(pt) = expected_types.get(i)
                                        && !self.infer_type_params(
                                            pt,
                                            arg_ty,
                                            &enum_type_params,
                                            &mut mapping,
                                        )
                                    {
                                        inferred = false;
                                        break;
                                    }
                                }
                                // For unmapped or partially mapped type parameters
                                // (e.g. `Optional.None` or `Result.Ok(x)` missing E),
                                // take the generic args from the expected type when it is
                                // the same enum with concrete args.
                                if let Some(Type::Named {
                                    path: exp_path,
                                    generics: exp_generics,
                                }) = expected
                                    && exp_path == ty_path
                                    && exp_generics.len() == enum_type_params.len()
                                {
                                    for (param, exp_arg) in
                                        enum_type_params.iter().zip(exp_generics.iter())
                                    {
                                        mapping
                                            .entry(param.clone())
                                            .or_insert_with(|| exp_arg.clone());
                                    }
                                }
                                if inferred {
                                    let generics = enum_type_params
                                        .iter()
                                        .map(|param| {
                                            mapping.get(param).cloned().unwrap_or_else(|| {
                                                Type::Named {
                                                    path: vec![param.clone()],
                                                    generics: Vec::new(),
                                                }
                                            })
                                        })
                                        .collect::<Vec<_>>();
                                    return Type::Named {
                                        path: ty_path.clone(),
                                        generics,
                                    };
                                }
                            }
                        }
                        return receiver_ty.clone();
                    }
                }
                // Otherwise, resolve as normal method call
                self.resolve_method_overload(
                    &receiver_ty,
                    method,
                    arguments,
                    style,
                    expected,
                    expr.span,
                )
            }
            ast::ExpressionKind::Index { object, index } => {
                let object_ty = self.check_expr(object, None);
                let index_ty = self.check_expr(index, None);
                let object_ty_display = object_ty.to_string();
                match &object_ty {
                    Type::Slice { element } => {
                        if !is_integer(&index_ty) {
                            self.error(
                                format!("slice index must be integer, found {}", index_ty),
                                index.span,
                            );
                        }
                        (**element).clone()
                    }
                    Type::Pointer { inner, .. } => match &**inner {
                        Type::Slice { element } => {
                            if !is_integer(&index_ty) {
                                self.error(
                                    format!("slice index must be integer, found {}", index_ty),
                                    index.span,
                                );
                            }
                            (**element).clone()
                        }
                        _ => {
                            if !is_integer(&index_ty) {
                                self.error(
                                    format!("pointer index must be integer, found {}", index_ty),
                                    index.span,
                                );
                            }
                            inner.as_ref().clone()
                        }
                    },
                    Type::Reference { inner, .. } => match &**inner {
                        Type::Slice { element } => {
                            if !is_integer(&index_ty) {
                                self.error(
                                    format!("slice index must be integer, found {}", index_ty),
                                    index.span,
                                );
                            }
                            (**element).clone()
                        }
                        Type::Pointer { inner, .. } => match &**inner {
                            Type::Slice { element } => {
                                if !is_integer(&index_ty) {
                                    self.error(
                                        format!("slice index must be integer, found {}", index_ty),
                                        index.span,
                                    );
                                }
                                (**element).clone()
                            }
                            _ => {
                                if !is_integer(&index_ty) {
                                    self.error(
                                        format!(
                                            "pointer index must be integer, found {}",
                                            index_ty
                                        ),
                                        index.span,
                                    );
                                }
                                inner.as_ref().clone()
                            }
                        },
                        Type::Tuple(elem_types) => {
                            let Some(idx) = Self::literal_integer_value(index) else {
                                self.error(
                                    "tuple index must be a constant integer literal".to_string(),
                                    index.span,
                                );
                                return Type::Unknown;
                            };
                            if idx < 0 || idx as usize >= elem_types.len() {
                                self.error(
                                    format!(
                                        "tuple index out of bounds: index is {}, but tuple has {} elements",
                                        idx,
                                        elem_types.len()
                                    ),
                                    index.span,
                                );
                                return Type::Unknown;
                            }
                            elem_types[idx as usize].clone()
                        }
                        _ => {
                            if let Some(result_ty) = self.resolve_method_overload_types(
                                &object_ty,
                                "__index_get",
                                &[index_ty],
                                None,
                                MethodCallStyle::Instance,
                                None,
                                object.span,
                            ) {
                                result_ty
                            } else {
                                self.error(
                                    format!(
                                        "indexing requires array or pointer type with integer index, found {}",
                                        object_ty_display
                                    ),
                                    object.span,
                                );
                                Type::Unknown
                            }
                        }
                    },
                    Type::Array { element, size } => {
                        if *size == 0 {
                            self.error(
                                "cannot index into zero-size array".to_string(),
                                object.span,
                            );
                            return Type::Unknown;
                        }
                        if !is_integer(&index_ty) {
                            self.error(
                                format!("array index must be integer, found {}", index_ty),
                                index.span,
                            );
                            return Type::Unknown;
                        }
                        (**element).clone()
                    }
                    Type::Tuple(elem_types) => {
                        let Some(idx) = Self::literal_integer_value(index) else {
                            self.error(
                                "tuple index must be a constant integer literal".to_string(),
                                index.span,
                            );
                            return Type::Unknown;
                        };
                        if idx < 0 || idx as usize >= elem_types.len() {
                            self.error(
                                format!(
                                    "tuple index out of bounds: index is {}, but tuple has {} elements",
                                    idx,
                                    elem_types.len()
                                ),
                                index.span,
                            );
                            return Type::Unknown;
                        }
                        elem_types[idx as usize].clone()
                    }
                    Type::Primitive(ast::PrimitiveType::Str) => {
                        // str is a byte pointer: s[i] reads the i-th byte (u8).
                        if !is_integer(&index_ty) {
                            self.error(
                                format!("string index must be integer, found {}", index_ty),
                                index.span,
                            );
                        }
                        Type::Primitive(ast::PrimitiveType::U8)
                    }
                    _ => {
                        if let Some(result_ty) = self.resolve_method_overload_types(
                            &object_ty,
                            "__index_get",
                            &[index_ty],
                            None,
                            MethodCallStyle::Instance,
                            None,
                            object.span,
                        ) {
                            result_ty
                        } else {
                            self.error(
                                format!(
                                    "indexing requires array or pointer type with integer index, found {}",
                                    object_ty_display
                                ),
                                object.span,
                            );
                            Type::Unknown
                        }
                    }
                }
            }
            ast::ExpressionKind::Slice {
                object,
                start,
                end,
                step,
            } => {
                let object_ty = self.check_expr(object, None);
                let object_ty_display = object_ty.to_string();

                let mut arg_types = Vec::new();
                let i64_expected = Type::Primitive(ast::PrimitiveType::I64);

                if let Some(s) = start {
                    let s_ty = self.check_expr(s, Some(&i64_expected));
                    if !is_integer(&s_ty) {
                        self.error(
                            format!("slice start index must be integer, found {}", s_ty),
                            s.span,
                        );
                    }
                    arg_types.push(s_ty);
                } else {
                    arg_types.push(Type::Primitive(ast::PrimitiveType::I64));
                }

                if let Some(e) = end {
                    let e_ty = self.check_expr(e, Some(&i64_expected));
                    if !is_integer(&e_ty) {
                        self.error(
                            format!("slice end index must be integer, found {}", e_ty),
                            e.span,
                        );
                    }
                    arg_types.push(e_ty);
                } else {
                    arg_types.push(Type::Primitive(ast::PrimitiveType::I64));
                }

                if let Some(st) = step {
                    let st_ty = self.check_expr(st, Some(&i64_expected));
                    if !is_integer(&st_ty) {
                        self.error(
                            format!("slice step must be integer, found {}", st_ty),
                            st.span,
                        );
                    }
                    arg_types.push(st_ty);
                }

                if let Some(declared_ty) = match object.kind.as_ref() {
                    ast::ExpressionKind::Identifier(ident) => self.lookup_type(&ident.name),
                    _ => None,
                } {
                    if let Type::Array { element, .. } = declared_ty {
                        return Type::Slice { element };
                    }
                }

                match &object_ty {
                    Type::Primitive(ast::PrimitiveType::Str) => {
                        if matches!(
                            expected,
                            Some(
                                Type::Primitive(ast::PrimitiveType::Str)
                                    | Type::Pointer { .. }
                                    | Type::Reference { .. }
                            )
                        ) {
                            Type::Primitive(ast::PrimitiveType::Str)
                        } else {
                            Type::Slice {
                                element: Box::new(Type::Primitive(ast::PrimitiveType::U8)),
                            }
                        }
                    }
                    Type::Array { element, .. } => Type::Slice {
                        element: element.clone(),
                    },
                    Type::Slice { element } => Type::Slice {
                        element: element.clone(),
                    },
                    Type::Pointer { inner, .. } => match &**inner {
                        Type::Primitive(ast::PrimitiveType::Str) => {
                            if matches!(
                                expected,
                                Some(
                                    Type::Primitive(ast::PrimitiveType::Str)
                                        | Type::Pointer { .. }
                                        | Type::Reference { .. }
                                )
                            ) {
                                Type::Primitive(ast::PrimitiveType::Str)
                            } else {
                                Type::Slice {
                                    element: Box::new(Type::Primitive(ast::PrimitiveType::U8)),
                                }
                            }
                        }
                        Type::Array { element, .. } => Type::Slice {
                            element: element.clone(),
                        },
                        Type::Slice { element } => Type::Slice {
                            element: element.clone(),
                        },
                        _ => {
                            if let Some(result_ty) = self.resolve_method_overload_types(
                                &object_ty,
                                "__slice_get",
                                &arg_types,
                                None,
                                MethodCallStyle::Instance,
                                None,
                                object.span,
                            ) {
                                result_ty
                            } else {
                                self.error(
                                    format!("cannot slice type {}", object_ty_display),
                                    object.span,
                                );
                                Type::Unknown
                            }
                        }
                    },
                    Type::Reference { inner, .. } => match &**inner {
                        Type::Primitive(ast::PrimitiveType::Str) => {
                            if matches!(
                                expected,
                                Some(
                                    Type::Primitive(ast::PrimitiveType::Str)
                                        | Type::Pointer { .. }
                                        | Type::Reference { .. }
                                )
                            ) {
                                Type::Primitive(ast::PrimitiveType::Str)
                            } else {
                                Type::Slice {
                                    element: Box::new(Type::Primitive(ast::PrimitiveType::U8)),
                                }
                            }
                        }
                        Type::Array { element, .. } => Type::Slice {
                            element: element.clone(),
                        },
                        Type::Slice { element } => Type::Slice {
                            element: element.clone(),
                        },
                        _ => {
                            if let Some(result_ty) = self.resolve_method_overload_types(
                                &object_ty,
                                "__slice_get",
                                &arg_types,
                                None,
                                MethodCallStyle::Instance,
                                None,
                                object.span,
                            ) {
                                result_ty
                            } else {
                                self.error(
                                    format!("cannot slice type {}", object_ty_display),
                                    object.span,
                                );
                                Type::Unknown
                            }
                        }
                    },
                    _ => {
                        if let Some(result_ty) = self.resolve_method_overload_types(
                            &object_ty,
                            "__slice_get",
                            &arg_types,
                            None,
                            MethodCallStyle::Instance,
                            None,
                            object.span,
                        ) {
                            result_ty
                        } else {
                            self.error(
                                format!("cannot slice type {}", object_ty_display),
                                object.span,
                            );
                            Type::Unknown
                        }
                    }
                }
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                let object_ty = self.check_expr(object, None);
                if let Type::Named { path, .. } = &object_ty
                    && path.len() == 1
                    && self.enum_defs.contains_key(&path[0])
                {
                    let is_type_scoped_access = matches!(
                        object.kind.as_ref(),
                        ast::ExpressionKind::Identifier(ident) if ident.name == path[0]
                    ) || matches!(
                        object.kind.as_ref(),
                        ast::ExpressionKind::TypeName(ty) if matches!(
                            ty.kind.as_ref(),
                            ast::TypeKind::Named(named) if named.path.len() == 1 && named.path[0].name == path[0]
                        )
                    );
                    if !is_type_scoped_access {
                        self.error(msg::enum_members_type_scoped(), object.span);
                        return Type::Unknown;
                    }
                }
                if let Some(mut field_ty) = self.resolve_field_access_type(&object_ty, &field.name)
                {
                    if let Type::Named { path, generics } = &mut field_ty
                        && generics.is_empty()
                        && let Some(Type::Named {
                            path: exp_path,
                            generics: exp_generics,
                        }) = expected
                        && path == exp_path
                    {
                        *generics = exp_generics.clone();
                    }
                    field_ty
                } else {
                    let suggestion = self.field_suggestion(&object_ty, &field.name);
                    self.error(
                        msg::unknown_field(&field.name, &object_ty, &suggestion),
                        field.span,
                    );
                    Type::Unknown
                }
            }
            ast::ExpressionKind::Match { expression, arms } => {
                let scrutinee_ty = self.check_expr(expression, None);

                // Enum dispatch: scrutinee is a named enum type.
                if let Type::Named { path, .. } = &scrutinee_ty {
                    if path.len() == 1 && self.enum_defs.contains_key(&path[0]) {
                        let enum_name = path[0].clone();
                        // For a generic enum like `Box2<i32>`, map the enum's
                        // type params (`T`) to the scrutinee's concrete generic
                        // args (`i32`) so payload bindings get the concrete type.
                        let type_param_map = self
                            .enum_defs
                            .get(&enum_name)
                            .map(|def| def.type_params.clone())
                            .unwrap_or_default();
                        let concrete_args: Vec<Type> = match &scrutinee_ty {
                            Type::Named { generics, .. } => generics.clone(),
                            _ => Vec::new(),
                        };
                        let mut enum_type_map = HashMap::default();
                        for (param, concrete) in type_param_map.iter().zip(concrete_args.iter()) {
                            enum_type_map.insert(param.clone(), concrete.clone());
                        }
                        let mut arm_types: Vec<Type> = Vec::new();
                        for (arm_index, arm) in arms.iter().enumerate() {
                            self.push_scope();
                            match &arm.pattern.kind {
                                ast::PatternKind::Wildcard
                                | ast::PatternKind::Identifier(_)
                                | ast::PatternKind::Move(_) => {
                                    // A top-level `move v` pattern is only
                                    // meaningful on enum payloads; reject it here.
                                    if matches!(arm.pattern.kind, ast::PatternKind::Move(_)) {
                                        self.error(
                                            "move patterns are only supported on enum payload bindings (Event(move v))"
                                                .to_string(),
                                            arm.pattern.span,
                                        );
                                    }
                                }
                                ast::PatternKind::Enum { variant, data, .. } => {
                                    if let Some(data_pattern) = data {
                                        let payload_types = self
                                            .enum_defs
                                            .get(&enum_name)
                                            .and_then(|def| def.variants.get(&variant.name))
                                            .map(|info| info.payload.clone())
                                            .unwrap_or_default();
                                        match &data_pattern.kind {
                                            ast::PatternKind::Identifier(binding)
                                            | ast::PatternKind::Move(binding) => {
                                                if let Some(pt) = payload_types.first() {
                                                    let bound = self.substitute_type_params(
                                                        &Type::from_ast(pt),
                                                        &enum_type_map,
                                                    );
                                                    let is_mut = matches!(
                                                        data_pattern.kind,
                                                        ast::PatternKind::Move(_)
                                                    );
                                                    self.bind(
                                                        &binding.name,
                                                        bound,
                                                        is_mut,
                                                        binding.span,
                                                    );
                                                }
                                            }
                                            ast::PatternKind::Tuple(bindings) => {
                                                for (i, sub) in bindings.iter().enumerate() {
                                                    if let ast::PatternKind::Identifier(b) =
                                                        &sub.kind
                                                        && let Some(pt) = payload_types.get(i)
                                                    {
                                                        let bound = self.substitute_type_params(
                                                            &Type::from_ast(pt),
                                                            &enum_type_map,
                                                        );
                                                        self.bind(&b.name, bound, false, b.span);
                                                    }
                                                }
                                            }
                                            _ => {
                                                self.error(
                                                    "data pattern in match must bind identifiers"
                                                        .to_string(),
                                                    data_pattern.span,
                                                );
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    self.error(
                                        format!(
                                            "unsupported pattern in enum match arm; expected a variant pattern like '{}.Variant(...)', a binding, or '_'",
                                            enum_name
                                        ),
                                        arm.pattern.span,
                                    );
                                }
                            }
                            if let Some(guard) = &arm.guard {
                                let guard_ty = self.check_expr(
                                    guard,
                                    Some(&Type::Primitive(ast::PrimitiveType::Bool)),
                                );
                                if guard_ty != Type::Primitive(ast::PrimitiveType::Bool)
                                    && guard_ty != Type::Unknown
                                {
                                    self.error(
                                        format!(
                                            "match guard expression must have type 'bool', found '{}'",
                                            guard_ty
                                        ),
                                        guard.span,
                                    );
                                }
                            }
                            // Later arms infer their type from the first arm so
                            // integer literals like `Rect2.E : 0` widen to i64
                            // instead of defaulting to i32.
                            let arm_expected = if arm_index == 0 {
                                expected
                            } else {
                                arm_types.first()
                            };
                            arm_types.push(self.check_expr(&arm.body, arm_expected));
                            self.pop_scope();
                        }
                        let non_never: Vec<&Type> =
                            arm_types.iter().filter(|t| **t != Type::Never).collect();
                        if let Some(first) = non_never.first() {
                            let unified = (*first).clone();
                            for (i, other) in arm_types.iter().enumerate().skip(1) {
                                if other == &Type::Never {
                                    continue;
                                }
                                if &unified != other && !Self::void_compatible(&unified, other) {
                                    self.error(
                                        format!(
                                            "match arm {} has type {}, expected {}",
                                            i + 1,
                                            other,
                                            unified
                                        ),
                                        arms[i].span,
                                    );
                                }
                            }
                            unified
                        } else {
                            Type::Never
                        }
                    } else {
                        let suggestion = self.type_suggestion(&path[0]);
                        self.error(msg::unknown_enum_type(&path[0], &suggestion), expr.span);
                        for arm in arms {
                            self.check_expr(&arm.body, None);
                        }
                        Type::Unknown
                    }
                } else if let Type::Primitive(prim) = &scrutinee_ty
                    && matches!(
                        prim,
                        ast::PrimitiveType::I8
                            | ast::PrimitiveType::I16
                            | ast::PrimitiveType::I32
                            | ast::PrimitiveType::I64
                            | ast::PrimitiveType::I128
                            | ast::PrimitiveType::U8
                            | ast::PrimitiveType::U16
                            | ast::PrimitiveType::U32
                            | ast::PrimitiveType::U64
                            | ast::PrimitiveType::U128
                            | ast::PrimitiveType::F32
                            | ast::PrimitiveType::F64
                            | ast::PrimitiveType::Bool
                            | ast::PrimitiveType::Char
                            | ast::PrimitiveType::Str
                    )
                {
                    // Value dispatch: literal patterns compared against a
                    // primitive int / float / bool / char / str scrutinee.
                    let mut arm_types: Vec<Type> = Vec::new();
                    for (arm_index, arm) in arms.iter().enumerate() {
                        self.push_scope();
                        match &arm.pattern.kind {
                            ast::PatternKind::Wildcard => {}
                            ast::PatternKind::Identifier(ident) => {
                                self.bind(&ident.name, scrutinee_ty.clone(), false, ident.span);
                            }
                            ast::PatternKind::Literal(lit) => {
                                let ok = match (prim, lit) {
                                    (
                                        ast::PrimitiveType::F32 | ast::PrimitiveType::F64,
                                        ast::Literal::Float(_),
                                    )
                                    | (
                                        ast::PrimitiveType::I8
                                        | ast::PrimitiveType::I16
                                        | ast::PrimitiveType::I32
                                        | ast::PrimitiveType::I64
                                        | ast::PrimitiveType::I128
                                        | ast::PrimitiveType::U8
                                        | ast::PrimitiveType::U16
                                        | ast::PrimitiveType::U32
                                        | ast::PrimitiveType::U64
                                        | ast::PrimitiveType::U128,
                                        ast::Literal::Integer(_),
                                    )
                                    | (ast::PrimitiveType::Bool, ast::Literal::Bool(_))
                                    | (ast::PrimitiveType::Char, ast::Literal::Char(_))
                                    | (ast::PrimitiveType::Str, ast::Literal::String(_)) => true,
                                    _ => false,
                                };
                                if !ok {
                                    self.error(
                                        format!(
                                            "match pattern type does not match scrutinee type {}",
                                            scrutinee_ty
                                        ),
                                        arm.pattern.span,
                                    );
                                }
                            }
                            _ => {
                                self.error(
                                    format!(
                                        "unsupported pattern in match on a {} value; expected a literal pattern, a binding, or '_'",
                                        scrutinee_ty
                                    ),
                                    arm.pattern.span,
                                );
                            }
                        }
                        if let Some(guard) = &arm.guard {
                            let guard_ty = self.check_expr(
                                guard,
                                Some(&Type::Primitive(ast::PrimitiveType::Bool)),
                            );
                            if guard_ty != Type::Primitive(ast::PrimitiveType::Bool)
                                && guard_ty != Type::Unknown
                            {
                                self.error(
                                    format!(
                                        "match guard expression must have type 'bool', found '{}'",
                                        guard_ty
                                    ),
                                    guard.span,
                                );
                            }
                        }
                        // Later arms infer their type from the first arm so
                        // integer literals widen to the arm type instead of
                        // defaulting to i32.
                        let arm_expected = if arm_index == 0 {
                            expected
                        } else {
                            arm_types.first()
                        };
                        arm_types.push(self.check_expr(&arm.body, arm_expected));
                        self.pop_scope();
                    }
                    let non_never: Vec<&Type> =
                        arm_types.iter().filter(|t| **t != Type::Never).collect();
                    if let Some(first) = non_never.first() {
                        let unified = (*first).clone();
                        for (i, other) in arm_types.iter().enumerate().skip(1) {
                            if other == &Type::Never {
                                continue;
                            }
                            if &unified != other && !Self::void_compatible(&unified, other) {
                                self.error(
                                    format!(
                                        "match arm {} has type {}, expected {}",
                                        i + 1,
                                        other,
                                        unified
                                    ),
                                    arms[i].span,
                                );
                            }
                        }
                        unified
                    } else {
                        Type::Never
                    }
                } else {
                    self.error(
                        "match expression requires an enum type or a primitive int/float/bool/char/str value".to_string(),
                        expr.span,
                    );
                    for arm in arms {
                        self.check_expr(&arm.body, None);
                    }
                    Type::Unknown
                }
            }
            ast::ExpressionKind::StructLiteral { .. } => {
                self.error(
                    "struct literal expressions are not supported here; use a variable \
                    initializer ('Type id = { .field = value }') instead"
                        .to_string(),
                    expr.span,
                );
                Type::Unknown
            }
            ast::ExpressionKind::MacroCall { name, args } => {
                match crate::builtin_macros::handle_typeck(&name.name, self, expr, args) {
                    Some(ty) => ty,
                    None => {
                        let known_macros = [
                            "print",
                            "println",
                            "eprint",
                            "eprintln",
                            "fprint",
                            "sprint",
                            "assert",
                            "align",
                            "size",
                            "json",
                            "from_json",
                            "hash",
                            "memcpy",
                            "memset",
                            "memmove",
                            "file",
                            "line",
                            "column",
                            "dbg",
                        ];
                        let suggestion =
                            crate::diagnostics::suggestion_suffix(&name.name, known_macros);
                        self.error(
                            msg::unknown_builtin_macro(&name.name, &suggestion),
                            expr.span,
                        );
                        Type::Unknown
                    }
                }
            }
            ast::ExpressionKind::ForIn {
                binding,
                is_mutable,
                iterable,
                body,
                mode,
                ..
            } => {
                self.push_scope();
                let item_ty = match iterable.kind.as_ref() {
                    ast::ExpressionKind::Binary {
                        left,
                        operator: ast::BinaryOperator::Range,
                        right,
                    } => {
                        let left_ty = self.check_expr(left, None);
                        let right_ty = self.check_expr(right, None);
                        if !self.is_integer_type(&left_ty) || !self.is_integer_type(&right_ty) {
                            self.error(
                                format!(
                                    "range bounds must be integers, got {} and {}",
                                    left_ty, right_ty
                                ),
                                expr.span,
                            );
                            Type::Unknown
                        } else if left_ty != right_ty {
                            self.error(
                                format!(
                                    "range bounds must be the same type, got {} and {}",
                                    left_ty, right_ty
                                ),
                                expr.span,
                            );
                            Type::Unknown
                        } else {
                            left_ty
                        }
                    }
                    _ => {
                        let method_name = match mode {
                            ast::IterAccessMode::ByValue => "into_iter",
                            ast::IterAccessMode::ByPtr => "into_iter_ptr",
                            ast::IterAccessMode::ByConstPtr => "into_iter_const_ptr",
                        };
                        let iterable_ty = self.check_expr(iterable, None);

                        let method_ident = ast::Identifier {
                            name: method_name.to_string(),
                            span: iterable.span,
                        };
                        let iterator_ty = self.resolve_method_overload(
                            &iterable_ty,
                            &method_ident,
                            &[],
                            MethodCallStyle::Instance,
                            None,
                            iterable.span,
                        );

                        // Store resolved iterator type for codegen
                        self.resolved_iter_types.insert(
                            (expr.span.file, expr.span.start, expr.span.end),
                            Box::new(iterator_ty.to_ast()),
                        );

                        if iterator_ty == Type::Unknown {
                            self.error(
                                format!(
                                    "type {} cannot be iterated over: missing `impl IntoIterator for {}`",
                                    iterable_ty, iterable_ty
                                ),
                                iterable.span,
                            );
                            Type::Unknown
                        } else {
                            let next_ret = self.resolve_method_overload_types(
                                &iterator_ty,
                                "next",
                                &[],
                                None,
                                MethodCallStyle::Instance,
                                None,
                                expr.span,
                            );
                            match next_ret {
                                Some(Type::Named { path, generics })
                                    if path.last().map(|s| s.as_str()) == Some("Optional") =>
                                {
                                    generics.first().cloned().unwrap_or(Type::Unknown)
                                }
                                Some(Type::Optional { inner }) => *inner,
                                Some(other) => {
                                    self.error(
                                        format!(
                                            "'next' on iterator must return Optional<T>, found {}",
                                            other
                                        ),
                                        expr.span,
                                    );
                                    Type::Unknown
                                }
                                None => {
                                    self.error(
                                        format!(
                                            "type {} has no 'next' method returning Optional<T>",
                                            iterator_ty
                                        ),
                                        expr.span,
                                    );
                                    Type::Unknown
                                }
                            }
                        }
                    }
                };
                self.bind(&binding.name, item_ty, *is_mutable, binding.span);
                self.check_block(body);
                self.pop_scope();
                Type::Unit
            }
            ast::ExpressionKind::Asm { inputs, .. } => {
                for input in inputs {
                    self.check_expr(input, None);
                }
                Type::Primitive(ast::PrimitiveType::I64)
            }
            ast::ExpressionKind::EnumVariant {
                path,
                variant,
                fields,
            } => {
                for field in fields {
                    self.check_expr(field, None);
                }
                // Resolve to the enum type
                let enum_name = if path.len() == 1 { &path[0].name } else { "" };
                if let Some(enum_def) = self.enum_defs.get(enum_name) {
                    let variant_payload: Vec<ast::Type> = enum_def
                        .variants
                        .get(&variant.name)
                        .map(|info| info.payload.clone())
                        .unwrap_or_default();
                    if variant_payload.len() != fields.len() {
                        self.error(
                            msg::enum_variant_field_count_mismatch(
                                &variant.name,
                                enum_name,
                                variant_payload.len(),
                                fields.len(),
                            ),
                            expr.span,
                        );
                    }
                    // Move-in enforcement: an owned payload (Drop type, or a
                    // type parameter that may instantiate to one) passed as a
                    // named lvalue must be `move`d — otherwise the source
                    // keeps its drop flag and the enum's copy shares the same
                    // buffers (silent double owner / dangling payload).
                    Type::Named {
                        path: path.iter().map(|p| p.name.clone()).collect(),
                        generics: Vec::new(),
                    }
                } else {
                    let suggestion = self.type_suggestion(enum_name);
                    self.error(msg::unknown_enum(enum_name, &suggestion), expr.span);
                    Type::Unknown
                }
            }
        };
        self.expr_types
            .insert((expr.span.start, expr.span.end), ty.to_string());
        self.record_lambda_type(expr, &ty);
        ty
    }

    fn resolve_overload(
        &mut self,
        ident: &ast::Identifier,
        arguments: &[ast::Expression],
        span: Span,
    ) -> Type {
        self.resolve_overload_with_explicit(ident, &[], arguments, span)
            .0
    }

    /// Structural Send gate: returns `Err(reason)` if `ty` cannot be MOVED
    /// across a thread boundary (used for `launch` arguments).  See
    /// `send_check` for the walk's rules.
    fn check_send(&self, ty: &Type) -> Result<(), String> {
        send_check::structural_send(ty, &|name: &str| {
            if let Some(def) = self.struct_defs.get(name) {
                return Some(DefView::Struct {
                    type_params: def.type_params.clone(),
                    fields: def
                        .fields
                        .iter()
                        .map(|(field, ty)| (field.clone(), ty.clone()))
                        .collect(),
                });
            }
            if let Some(def) = self.enum_defs.get(name) {
                return Some(DefView::Enum {
                    type_params: def.type_params.clone(),
                    variants: def
                        .variants
                        .values()
                        .map(|variant| {
                            variant
                                .payload
                                .iter()
                                .map(Type::from_ast)
                                .collect::<Vec<_>>()
                        })
                        .collect(),
                });
            }
            None
        })
    }

    /// Recognizes compiler intrinsics of the form `__atomic_<op>_<width>` and
    /// returns the type of the call site, or `None` if `name` is not an atomic
    /// intrinsic. Atomic intrinsics are always bare identifiers (never generic),
    /// so only the plain-call path consults this hook.
    ///
    /// Widths: i8/i32/i64 for load/store/exchange (byte flags are i8), i32/i64
    /// for fetch_add/fetch_sub/cmpxchg, plus the width-less `__atomic_fence`.
    fn try_resolve_atomic_intrinsic(
        &mut self,
        name: &str,
        arguments: &[ast::Expression],
        span: Span,
    ) -> Option<Type> {
        let body = name.strip_prefix("__atomic_")?;

        // (op, optional element width). Unknown suffix -> None (not an intrinsic).
        let (op, width): (&str, Option<ast::PrimitiveType>) = match body {
            "load_i8" => ("load", Some(ast::PrimitiveType::I8)),
            "load_i32" => ("load", Some(ast::PrimitiveType::I32)),
            "load_i64" => ("load", Some(ast::PrimitiveType::I64)),
            "store_i8" => ("store", Some(ast::PrimitiveType::I8)),
            "store_i32" => ("store", Some(ast::PrimitiveType::I32)),
            "store_i64" => ("store", Some(ast::PrimitiveType::I64)),
            "exchange_i8" => ("exchange", Some(ast::PrimitiveType::I8)),
            "exchange_i32" => ("exchange", Some(ast::PrimitiveType::I32)),
            "exchange_i64" => ("exchange", Some(ast::PrimitiveType::I64)),
            "fetch_add_i8" => ("fetch_add", Some(ast::PrimitiveType::I8)),
            "fetch_add_i32" => ("fetch_add", Some(ast::PrimitiveType::I32)),
            "fetch_add_i64" => ("fetch_add", Some(ast::PrimitiveType::I64)),
            "fetch_sub_i8" => ("fetch_sub", Some(ast::PrimitiveType::I8)),
            "fetch_sub_i32" => ("fetch_sub", Some(ast::PrimitiveType::I32)),
            "fetch_sub_i64" => ("fetch_sub", Some(ast::PrimitiveType::I64)),
            "cmpxchg_i8" => ("cmpxchg", Some(ast::PrimitiveType::I8)),
            "cmpxchg_i32" => ("cmpxchg", Some(ast::PrimitiveType::I32)),
            "cmpxchg_i64" => ("cmpxchg", Some(ast::PrimitiveType::I64)),
            "fence" => ("fence", None),
            _ => return None,
        };

        let elem = width.map(Type::Primitive);
        let order = Type::Primitive(ast::PrimitiveType::I32);
        let addr = |inner: &Type| Type::Pointer {
            is_mutable: true,
            is_volatile: false,
            inner: Box::new(inner.clone()),
        };
        // Only the elemental ops carry an element width; `fence` is standalone.
        let elem_ty = || elem.as_ref().expect("atomic intrinsic without width");

        let (params, result): (Vec<Type>, Type) = match op {
            "load" => (vec![addr(elem_ty()), order.clone()], elem_ty().clone()),
            "store" => (
                vec![addr(elem_ty()), elem_ty().clone(), order.clone()],
                Type::Unit,
            ),
            "exchange" => (
                vec![addr(elem_ty()), elem_ty().clone(), order.clone()],
                elem_ty().clone(),
            ),
            "fetch_add" | "fetch_sub" => (
                vec![addr(elem_ty()), elem_ty().clone(), order.clone()],
                elem_ty().clone(),
            ),
            "cmpxchg" => (
                vec![
                    addr(elem_ty()),
                    elem_ty().clone(),
                    elem_ty().clone(),
                    order.clone(),
                    order.clone(),
                ],
                Type::Primitive(ast::PrimitiveType::Bool),
            ),
            "fence" => (vec![order.clone()], Type::Unit),
            _ => return None,
        };

        // Type-check each argument. Integer literals (e.g. the ordering
        // constant) are accepted when they fit the expected width.
        let arg_types = arguments
            .iter()
            .map(|arg| self.check_expr_with_literal_naturals(arg))
            .collect::<Vec<_>>();
        if arguments.len() != params.len() {
            self.error(
                format!(
                    "atomic intrinsic '{}' expected {} arguments, got {}",
                    name,
                    params.len(),
                    arguments.len()
                ),
                span,
            );
        }
        for (i, (param_ty, arg_ty)) in params.iter().zip(arg_types.iter()).enumerate() {
            let literal_ok = Self::literal_integer_value(&arguments[i]).is_some_and(|value| {
                matches!(param_ty, Type::Primitive(prim)
                    if Self::integer_value_fits(value, prim))
            });
            // The address parameter accepts a pointer of any mutability: LLVM
            // atomic operations do not distinguish constness at runtime, and
            // wrappers hand over `&self.value` through a const `&Self` receiver.
            let addr_ok = i == 0
                && matches!(
                    (param_ty, arg_ty),
                    (Type::Pointer { inner: pe, .. }, Type::Pointer { inner: ae, .. })
                        if self.is_assignable(pe, ae)
                );
            if !literal_ok
                && !addr_ok
                && !self.is_assignable(param_ty, arg_ty)
                && !self.is_implicitly_castable(arg_ty, param_ty)
            {
                self.error(
                    format!(
                        "atomic intrinsic '{}' parameter {}: expected {}, got {}",
                        name, i, param_ty, arg_ty
                    ),
                    arguments[i].span,
                );
            }
        }

        Some(result)
    }

    /// Resolves `ident` to the best matching overload and returns the call's
    /// return type together with the (already type-checked) argument types,
    /// so callers can run the Send gate on `launch` arguments.
    fn resolve_overload_with_explicit(
        &mut self,
        ident: &ast::Identifier,
        explicit_types: &[Type],
        arguments: &[ast::Expression],
        span: Span,
    ) -> (Type, Vec<Type>) {
        let contextual_parameters: Vec<_> = if arguments.iter().any(|argument| {
            matches!(argument.kind.as_ref(), ast::ExpressionKind::Lambda { .. })
        }) {
            self.functions.get(&ident.name).into_iter().flatten()
                .filter_map(|symbol| self.function_symbols.get(symbol))
                .filter(|signature| signature.type_params.is_empty() && signature.params.len() == arguments.len())
                .map(|signature| signature.params.clone()).collect()
        } else {
            Vec::new()
        };
        let arg_types = arguments
            .iter()
            .enumerate()
            .map(|(index, arg)| {
                if matches!(arg.kind.as_ref(), ast::ExpressionKind::Lambda { .. })
                    && let Some(first) = contextual_parameters.first().and_then(|params| params.get(index))
                    && contextual_parameters.iter().all(|params| params.get(index) == Some(first))
                {
                    self.check_expr(arg, Some(first))
                } else {
                    self.check_expr_with_literal_naturals(arg)
                }
            })
            .collect::<Vec<_>>();

        let Some(candidate_ids) = self.functions.get(&ident.name).cloned() else {
            let suggestion = self.function_suggestion(&ident.name);
            self.error(msg::unknown_function(&ident.name, &suggestion), span);
            return (Type::Unknown, arg_types);
        };

        let mut matches: Vec<(usize, Type, HashMap<String, Type>, FunctionSig)> = Vec::new();

        for candidate_id in &candidate_ids {
            let Some(candidate) = self.function_symbols.get(candidate_id).cloned() else {
                continue;
            };
            let mut ok = true;
            let mut score = 0usize;
            let mut mapping = HashMap::default();
            if !explicit_types.is_empty() {
                if explicit_types.len() != candidate.type_params.len() {
                    continue;
                }
                for (i, tp) in candidate.type_params.iter().enumerate() {
                    mapping.insert(tp.clone(), explicit_types[i].clone());
                }
            }

            let has_var_param = candidate.source.parameters.iter().any(|p| p.is_variadic);
            if has_var_param {
                let fixed_params_count = candidate.params.len().saturating_sub(1);
                if arguments.len() < fixed_params_count {
                    continue;
                }
                let var_elem_ty = &candidate.params[fixed_params_count];

                // Check fixed arguments:
                for (i, param_ty) in candidate.params[..fixed_params_count].iter().enumerate() {
                    let arg_ty = &arg_types[i];
                    let mut matched = false;
                    if let Some(lit_value) = Self::literal_integer_value(&arguments[i])
                        && let Type::Primitive(prim) = &self.substitute_type(param_ty, &mapping)
                        && Self::integer_prim_range(prim).is_some()
                        && !Self::integer_value_fits(lit_value, prim)
                    {
                        ok = false;
                        break;
                    }
                    let mut inferred_mapping = mapping.clone();
                    if self.infer_type_params(
                        param_ty,
                        arg_ty,
                        &candidate.type_params,
                        &mut inferred_mapping,
                    ) {
                        let substituted = self.substitute_type(param_ty, &inferred_mapping);
                        if self.is_assignable(&substituted, arg_ty) {
                            if substituted != *arg_ty {
                                score += 1;
                            }
                            mapping = inferred_mapping;
                            matched = true;
                        } else if self.is_implicitly_castable(arg_ty, &substituted) {
                            score += 1;
                            mapping = inferred_mapping;
                            matched = true;
                        }
                    }
                    if !matched {
                        let substituted = self.substitute_type(param_ty, &mapping);
                        if self.is_assignable(&substituted, arg_ty) {
                            if substituted != *arg_ty {
                                score += 1;
                            }
                            matched = true;
                        } else if self.is_implicitly_castable(arg_ty, &substituted) {
                            score += 1;
                            matched = true;
                        }
                    }
                    if !matched {
                        ok = false;
                        break;
                    }
                }

                // Check trailing variadic arguments against var_elem_ty:
                if ok {
                    for i in fixed_params_count..arguments.len() {
                        let arg_ty = &arg_types[i];
                        let mut matched = false;
                        if let Some(lit_value) = Self::literal_integer_value(&arguments[i])
                            && let Type::Primitive(prim) =
                                &self.substitute_type(var_elem_ty, &mapping)
                            && Self::integer_prim_range(prim).is_some()
                            && !Self::integer_value_fits(lit_value, prim)
                        {
                            ok = false;
                            break;
                        }
                        let mut inferred_mapping = mapping.clone();
                        if self.infer_type_params(
                            var_elem_ty,
                            arg_ty,
                            &candidate.type_params,
                            &mut inferred_mapping,
                        ) {
                            let substituted = self.substitute_type(var_elem_ty, &inferred_mapping);
                            if self.is_assignable(&substituted, arg_ty) {
                                if substituted != *arg_ty {
                                    score += 1;
                                }
                                mapping = inferred_mapping;
                                matched = true;
                            } else if self.is_implicitly_castable(arg_ty, &substituted) {
                                score += 1;
                                mapping = inferred_mapping;
                                matched = true;
                            }
                        }
                        if !matched {
                            let substituted = self.substitute_type(var_elem_ty, &mapping);
                            if self.is_assignable(&substituted, arg_ty) {
                                if substituted != *arg_ty {
                                    score += 1;
                                }
                                matched = true;
                            } else if self.is_implicitly_castable(arg_ty, &substituted) {
                                score += 1;
                                matched = true;
                            }
                        }
                        if !matched {
                            ok = false;
                            break;
                        }
                    }
                }
            } else {
                if candidate.is_variadic {
                    if candidate.params.len() > arguments.len() {
                        continue;
                    }
                } else if candidate.params.len() != arguments.len() {
                    continue;
                }

                for (i, (param_ty, arg_ty)) in
                    candidate.params.iter().zip(arg_types.iter()).enumerate()
                {
                    let mut matched = false;

                    // Phase 0: integer literals cannot narrow into a parameter
                    // type they overflow (e.g. `foo(300)` cannot call `foo(u8)`);
                    // otherwise the natural-typed arg flows through the normal
                    // assignable/cast matching below.
                    if let Some(lit_value) = Self::literal_integer_value(&arguments[i])
                        && let Type::Primitive(prim) = &self.substitute_type(param_ty, &mapping)
                        && Self::integer_prim_range(prim).is_some()
                        && !Self::integer_value_fits(lit_value, prim)
                    {
                        ok = false;
                        break;
                    }

                    // Phase 1: try with inferred type-parameter mapping
                    let mut inferred_mapping = mapping.clone();
                    if !matched
                        && self.infer_type_params(
                            param_ty,
                            arg_ty,
                            &candidate.type_params,
                            &mut inferred_mapping,
                        )
                    {
                        let substituted = self.substitute_type(param_ty, &inferred_mapping);
                        if self.is_assignable(&substituted, arg_ty) {
                            if substituted != *arg_ty {
                                score += 1;
                            }
                            mapping = inferred_mapping;
                            matched = true;
                        } else if self.is_implicitly_castable(arg_ty, &substituted) {
                            score += 1;
                            mapping = inferred_mapping;
                            matched = true;
                        }
                    }

                    // Phase 2: fallback for concrete types (e.g. f32 vs f64)
                    if !matched {
                        let substituted = self.substitute_type(param_ty, &mapping);
                        if self.is_assignable(&substituted, arg_ty) {
                            if substituted != *arg_ty {
                                score += 1;
                            }
                            matched = true;
                        } else if self.is_implicitly_castable(arg_ty, &substituted) {
                            score += 1;
                            matched = true;
                        }
                    }

                    if !matched {
                        ok = false;
                        break;
                    }
                }
            }

            if ok {
                if !self.bounds_satisfied(&candidate.bounds, &mapping, span) {
                    continue;
                }
                let return_type = self.substitute_type(&candidate.return_type, &mapping);
                matches.push((score, return_type, mapping, candidate));
            }
        }

        if matches.is_empty() {
            let arg_desc: String = if arg_types.len() == 1 {
                format!("{}", arg_types[0])
            } else {
                let args: Vec<String> = arg_types.iter().map(|t| t.to_string()).collect();
                format!("({})", args.join(", "))
            };
            let candidates: Vec<String> = candidate_ids
                .iter()
                .filter_map(|id| {
                    self.function_symbols.get(id).map(|c| {
                        let params: Vec<String> = c.params.iter().map(|p| p.to_string()).collect();
                        format!("{}({})", ident.name, params.join(", "))
                    })
                })
                .collect();
            self.error(
                msg::no_matching_overload(&ident.name, &arg_desc, &candidates, ""),
                span,
            );
            return (Type::Unknown, arg_types);
        }
        matches.sort_by_key(|(score, _, _, _)| *score);
        let best_score = matches[0].0;
        let best_matches: Vec<_> = matches
            .into_iter()
            .filter(|(score, _, _, _)| *score == best_score)
            .collect();

        if best_matches.len() > 1 {
            let candidates: Vec<String> = best_matches
                .iter()
                .map(|(_, _, _, c)| {
                    let params: Vec<String> = c.params.iter().map(|p| p.to_string()).collect();
                    format!("{}({})", ident.name, params.join(", "))
                })
                .collect();
            self.error(msg::ambiguous_overload(&ident.name, &candidates), span);
            return (Type::Unknown, arg_types);
        }

        let (_, return_type, mapping, candidate) = &best_matches[0];
        self.record_function_monomorph(candidate, mapping, span);
        if !self.lambdas.frames.is_empty() {
            // Trailing variadic arguments all take the variadic element type.
            let variadic = candidate
                .source
                .parameters
                .iter()
                .any(|p| p.is_variadic)
                .then(|| candidate.params.last())
                .flatten();
            for (index, argument) in arguments.iter().enumerate() {
                let Some(parameter) = candidate.params.get(index).or(variadic) else {
                    break;
                };
                let parameter = self.substitute_type(parameter, mapping);
                self.capture_argument(argument, &parameter);
            }
        }
        (return_type.clone(), arg_types)
    }

    /// Collect type-parameter-like names from an owner type — the set of bare
    /// single-segment `Named` types that appear as generic arguments.
    fn owner_type_param_names(&self, ty: &Type) -> Vec<String> {
        match ty {
            Type::Named { generics, .. } => {
                let mut out = Vec::new();
                for g in generics {
                    if let Type::Named {
                        path,
                        generics: inner_gs,
                    } = g
                        && path.len() == 1
                        && inner_gs.is_empty()
                    {
                        out.push(path[0].clone());
                    }
                }
                out
            }
            _ => Vec::new(),
        }
    }

    fn resolve_method_overload(
        &mut self,
        receiver_ty: &Type,
        method: &ast::Identifier,
        arguments: &[ast::Expression],
        style: MethodCallStyle,
        expected: Option<&Type>,
        span: Span,
    ) -> Type {
        let arg_types = arguments
            .iter()
            .map(|arg| self.check_expr_with_literal_naturals(arg))
            .collect::<Vec<_>>();

        match self.resolve_method_overload_types(
            receiver_ty,
            &method.name,
            &arg_types,
            Some(arguments),
            style,
            expected,
            span,
        ) {
            Some(ty) => ty,
            None => {
                if self.is_bare_type_param(receiver_ty) {
                    self.current_implicit_method_reqs.push(ImplicitMethodReq {
                        receiver: receiver_ty.clone(),
                        name: method.name.clone(),
                        args: arg_types,
                        style,
                        origin_span: span,
                    });
                    return expected.cloned().unwrap_or(Type::Unknown);
                }
                let arg_desc: String = if arg_types.len() == 1 {
                    format!("{}", arg_types[0])
                } else {
                    let args: Vec<String> = arg_types.iter().map(|t| t.to_string()).collect();
                    format!("({})", args.join(", "))
                };
                let key = (self.method_key(receiver_ty), method.name.clone());
                let suggestion = {
                    let target_key = self.method_key(receiver_ty);
                    let method_names: Vec<&str> = self
                        .methods
                        .keys()
                        .filter(|(ty, _)| ty == &target_key)
                        .map(|(_, m)| m.as_str())
                        .collect();
                    crate::diagnostics::suggestion_suffix(&method.name, method_names)
                };
                let candidates: Vec<String> = self
                    .methods
                    .get(&key)
                    .map(|candidate_ids| {
                        candidate_ids
                            .iter()
                            .filter_map(|id| {
                                self.method_symbols.get(id).map(|c| {
                                    let params: Vec<String> =
                                        c.params.iter().map(|p| p.to_string()).collect();
                                    format!("{}({})", method.name, params.join(", "))
                                })
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                self.error(
                    msg::no_matching_overload(&method.name, &arg_desc, &candidates, &suggestion),
                    span,
                );
                Type::Unknown
            }
        }
    }

    fn resolve_method_overload_types(
        &mut self,
        receiver_ty: &Type,
        name: &str,
        arg_types: &[Type],
        arg_exprs: Option<&[ast::Expression]>,
        style: MethodCallStyle,
        expected: Option<&Type>,
        span: Span,
    ) -> Option<Type> {
        let key = (self.method_key(receiver_ty), name.to_string());
        let candidate_ids = self.methods.get(&key).cloned()?;

        let mut matches: Vec<(usize, Type, HashMap<String, Type>, MethodSig)> = Vec::new();
        let owner_ty = Self::method_owner_type(receiver_ty);

        for candidate_id in candidate_ids {
            let Some(candidate) = self.method_symbols.get(&candidate_id).cloned() else {
                continue;
            };
            let is_instance =
                !matches!(candidate.source_method.method_kind, ast::MethodKind::Static);
            if style == MethodCallStyle::Static && is_instance {
                continue;
            }
            if style == MethodCallStyle::Instance && !is_instance {
                continue;
            }

            let mut ok = true;
            let mut score = 0usize;
            let mut mapping = HashMap::default();

            // Try owner type inference — but if the receiver type lacks generic args
            // that correspond to method type params, don't fail: those can be
            // inferred from argument types further below.
            let target_owner = match &candidate.owner {
                Type::Reference { .. } | Type::Pointer { .. } => receiver_ty,
                _ => owner_ty,
            };
            let owner_inferred = self.infer_type_params(
                &candidate.owner,
                target_owner,
                &candidate.type_params,
                &mut mapping,
            );
            if !owner_inferred {
                // If all type params are generic placeholders that could not be matched
                // because the receiver type has fewer generics than the owner type,
                // allow them to be inferred from arguments instead.
                let all_type_params = self.owner_type_param_names(&candidate.owner);
                if all_type_params.is_empty()
                    || !self.infer_type_params(
                        &candidate.owner,
                        target_owner,
                        &all_type_params,
                        &mut mapping,
                    )
                {
                    continue;
                }
            }

            // A bare generic receiver (`Result.ok(x)` with no type args) leaves
            // the owner's type params unmapped. Fill them from the expected
            // type when it names the same generic with concrete args, so
            // `Result<i32, Error> r = Result.ok(x);` infers E from the LHS.
            if mapping.len() < candidate.type_params.len()
                && let Some(Type::Named {
                    path: exp_path,
                    generics: exp_generics,
                }) = expected
                && let Type::Named {
                    path: owner_path,
                    generics: owner_generics,
                } = &candidate.owner
                && owner_path == exp_path
                && owner_generics.len() == exp_generics.len()
            {
                for (owner_param, exp_arg) in owner_generics.iter().zip(exp_generics.iter()) {
                    if let Type::Named {
                        path: param_path, ..
                    } = owner_param
                        && param_path.len() == 1
                        && candidate.type_params.contains(&param_path[0])
                    {
                        mapping.entry(param_path[0].clone()).or_insert_with(|| exp_arg.clone());
                    }
                }
            }

            let has_var_param = candidate
                .source_method
                .parameters
                .iter()
                .any(|p| p.is_variadic);
            if has_var_param {
                let fixed_arg_count = if style == MethodCallStyle::Instance {
                    candidate.params.len().saturating_sub(2)
                } else {
                    candidate.params.len().saturating_sub(1)
                };
                if arg_types.len() < fixed_arg_count {
                    continue;
                }
                if style == MethodCallStyle::Instance {
                    let receiver_param =
                        self.substitute_self_type(&candidate.params[0], receiver_ty);
                    let infer_expected = match &receiver_param {
                        Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
                            inner.as_ref()
                        }
                        _ => &receiver_param,
                    };
                    let infer_found = match &receiver_param {
                        Type::Reference { .. } | Type::Pointer { .. } => owner_ty,
                        _ => receiver_ty,
                    };
                    if !self.infer_type_params(
                        infer_expected,
                        infer_found,
                        &candidate.type_params,
                        &mut mapping,
                    ) {
                        ok = false;
                    } else {
                        let substituted = self.substitute_type(&receiver_param, &mapping);
                        if !self.receiver_compatible(&substituted, receiver_ty, &mut score) {
                            ok = false;
                        }
                    }
                }
                if !ok {
                    continue;
                }
                let param_offset = if style == MethodCallStyle::Instance {
                    1
                } else {
                    0
                };
                // Check fixed arguments:
                for (i, param_ty) in candidate.params[param_offset..param_offset + fixed_arg_count]
                    .iter()
                    .enumerate()
                {
                    let param_ty = self.substitute_self_type(param_ty, receiver_ty);
                    let arg_ty = &arg_types[i];
                    let mut matched = false;
                    if let Some(exprs) = arg_exprs
                        && let Some(lit_value) = exprs.get(i).and_then(Self::literal_integer_value)
                        && let Type::Primitive(prim) = &self.substitute_type(&param_ty, &mapping)
                        && Self::integer_prim_range(prim).is_some()
                        && !Self::integer_value_fits(lit_value, prim)
                    {
                        ok = false;
                        break;
                    }
                    let mut inferred_mapping = mapping.clone();
                    if self.infer_type_params(
                        &param_ty,
                        arg_ty,
                        &candidate.type_params,
                        &mut inferred_mapping,
                    ) {
                        let substituted = self.substitute_type(&param_ty, &inferred_mapping);
                        if self.is_assignable(&substituted, arg_ty) {
                            if substituted != *arg_ty {
                                score += 1;
                            }
                            mapping = inferred_mapping;
                            matched = true;
                        } else if self.is_implicitly_castable(arg_ty, &substituted) {
                            score += 1;
                            mapping = inferred_mapping;
                            matched = true;
                        }
                    }
                    if !matched {
                        let substituted = self.substitute_type(&param_ty, &mapping);
                        if self.is_assignable(&substituted, arg_ty) {
                            if substituted != *arg_ty {
                                score += 1;
                            }
                            matched = true;
                        } else if self.is_implicitly_castable(arg_ty, &substituted) {
                            score += 1;
                            matched = true;
                        }
                    }
                    if !matched {
                        ok = false;
                        break;
                    }
                }
                // Check trailing variadic arguments:
                if ok {
                    let var_elem_ty = candidate.params.last().unwrap();
                    let var_elem_ty = self.substitute_self_type(var_elem_ty, receiver_ty);
                    for i in fixed_arg_count..arg_types.len() {
                        let arg_ty = &arg_types[i];
                        let mut matched = false;
                        if let Some(exprs) = arg_exprs
                            && let Some(lit_value) =
                                exprs.get(i).and_then(Self::literal_integer_value)
                            && let Type::Primitive(prim) =
                                &self.substitute_type(&var_elem_ty, &mapping)
                            && Self::integer_prim_range(prim).is_some()
                            && !Self::integer_value_fits(lit_value, prim)
                        {
                            ok = false;
                            break;
                        }
                        let mut inferred_mapping = mapping.clone();
                        if self.infer_type_params(
                            &var_elem_ty,
                            arg_ty,
                            &candidate.type_params,
                            &mut inferred_mapping,
                        ) {
                            let substituted = self.substitute_type(&var_elem_ty, &inferred_mapping);
                            if self.is_assignable(&substituted, arg_ty) {
                                if substituted != *arg_ty {
                                    score += 1;
                                }
                                mapping = inferred_mapping;
                                matched = true;
                            } else if self.is_implicitly_castable(arg_ty, &substituted) {
                                score += 1;
                                mapping = inferred_mapping;
                                matched = true;
                            }
                        }
                        if !matched {
                            let substituted = self.substitute_type(&var_elem_ty, &mapping);
                            if self.is_assignable(&substituted, arg_ty) {
                                if substituted != *arg_ty {
                                    score += 1;
                                }
                                matched = true;
                            } else if self.is_implicitly_castable(arg_ty, &substituted) {
                                score += 1;
                                matched = true;
                            }
                        }
                        if !matched {
                            ok = false;
                            break;
                        }
                    }
                }
            } else if style == MethodCallStyle::Instance
                && candidate.params.len() == arg_types.len() + 1
            {
                let receiver_param = self.substitute_self_type(&candidate.params[0], receiver_ty);
                let infer_expected = match &receiver_param {
                    Type::Reference { inner, .. } | Type::Pointer { inner, .. } => inner.as_ref(),
                    _ => &receiver_param,
                };
                let infer_found = match &receiver_param {
                    Type::Reference { .. } | Type::Pointer { .. } => owner_ty,
                    _ => receiver_ty,
                };
                if !self.infer_type_params(
                    infer_expected,
                    infer_found,
                    &candidate.type_params,
                    &mut mapping,
                ) {
                    ok = false;
                } else {
                    let substituted = self.substitute_type(&receiver_param, &mapping);
                    if self.receiver_compatible(&substituted, receiver_ty, &mut score) {
                        // ok
                    } else {
                        ok = false;
                    }
                }
            } else if candidate.params.len() != arg_types.len() {
                ok = false;
            }

            if ok && !has_var_param {
                let mut iter = candidate.params.iter();
                if style == MethodCallStyle::Instance
                    && candidate.params.len() == arg_types.len() + 1
                {
                    iter.next();
                }
                for (param_offset, (param_ty, arg_ty)) in iter.zip(arg_types.iter()).enumerate() {
                    let param_ty = self.substitute_self_type(param_ty, receiver_ty);
                    let mut matched = false;

                    // Phase 0: integer literals cannot narrow into a parameter
                    // type they overflow; otherwise the natural-typed arg flows
                    // through the normal assignable/cast matching below.
                    if let Some(exprs) = arg_exprs
                        && let Some(lit_value) = exprs
                            .get(param_offset)
                            .and_then(Self::literal_integer_value)
                        && let Type::Primitive(prim) = &self.substitute_type(&param_ty, &mapping)
                        && Self::integer_prim_range(prim).is_some()
                        && !Self::integer_value_fits(lit_value, prim)
                    {
                        ok = false;
                        break;
                    }

                    // First try with inferred type-parameter mapping.
                    let mut inferred_mapping = mapping.clone();
                    if !matched
                        && self.infer_type_params(
                            &param_ty,
                            arg_ty,
                            &candidate.type_params,
                            &mut inferred_mapping,
                        )
                    {
                        let substituted = self.substitute_type(&param_ty, &inferred_mapping);
                        if self.is_assignable(&substituted, arg_ty) {
                            if substituted != *arg_ty
                                && !matches!(&substituted, Type::Reference { inner, .. } if inner.as_ref() == arg_ty)
                            {
                                score += 1;
                            }
                            mapping = inferred_mapping;
                            matched = true;
                        } else if let Type::Reference {
                            is_mutable: false,
                            inner,
                        } = &substituted
                            && (inner.as_ref() == arg_ty || self.is_assignable(inner, arg_ty))
                        {
                            if inner.as_ref() != arg_ty {
                                score += 1;
                            }
                            mapping = inferred_mapping;
                            matched = true;
                        } else if self.is_implicitly_castable(arg_ty, &substituted) {
                            score += 1;
                            mapping = inferred_mapping;
                            matched = true;
                        }
                    }

                    // If inference did not match (e.g., concrete mapped param + numeric literal),
                    // fall back to current mapping and allow implicit cast.
                    if !matched {
                        let substituted = self.substitute_type(&param_ty, &mapping);
                        if self.is_assignable(&substituted, arg_ty) {
                            if substituted != *arg_ty
                                && !matches!(&substituted, Type::Reference { inner, .. } if inner.as_ref() == arg_ty)
                            {
                                score += 1;
                            }
                            matched = true;
                        } else if let Type::Reference {
                            is_mutable: false,
                            inner,
                        } = &substituted
                            && (inner.as_ref() == arg_ty || self.is_assignable(inner, arg_ty))
                        {
                            if inner.as_ref() != arg_ty {
                                score += 1;
                            }
                            matched = true;
                        } else if self.is_implicitly_castable(arg_ty, &substituted) {
                            score += 1;
                            matched = true;
                        }
                    }

                    if !matched {
                        ok = false;
                        break;
                    }
                }
            }

            if ok {
                let sat = self.bounds_satisfied(&candidate.bounds, &mapping, span);
                if !sat {
                    continue;
                }
                let return_type = self.substitute_type(&candidate.return_type, &mapping);
                matches.push((score, return_type, mapping, candidate));
            }
        }

        if matches.is_empty() {
            return None;
        }

        matches.sort_by_key(|(score, _, _, _)| *score);
        let best_score = matches[0].0;
        let best_matches: Vec<_> = matches
            .into_iter()
            .filter(|(score, _, _, _)| *score == best_score)
            .collect();
        if best_matches.len() > 1 {
            let candidates: Vec<String> = best_matches
                .iter()
                .map(|(_, _, _, c)| {
                    let params: Vec<String> = c.params.iter().map(|p| p.to_string()).collect();
                    format!("{}({})", name, params.join(", "))
                })
                .collect();
            self.error(msg::ambiguous_overload(name, &candidates), span);
            return None;
        }

        let (_, return_type, mapping, candidate) = &best_matches[0];
        self.record_method_monomorph(candidate, mapping, span);
        if !self.lambdas.frames.is_empty() {
            let offset = usize::from(style == MethodCallStyle::Instance);
            if offset != 0 && let Some(parameter) = candidate.params.first() {
                let parameter = self.substitute_type(&self.substitute_self_type(parameter, receiver_ty), mapping);
                self.capture_method_receiver(span, &parameter);
            }
            if let Some(arguments) = arg_exprs {
                for (argument, parameter) in arguments.iter().zip(candidate.params.iter().skip(offset)) {
                    let parameter = self.substitute_type(&self.substitute_self_type(parameter, receiver_ty), mapping);
                    self.capture_argument(argument, &parameter);
                }
            }
        }
        Some(return_type.clone())
    }

    fn method_owner_type(ty: &Type) -> &Type {
        match ty {
            Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
                Self::method_owner_type(inner.as_ref())
            }
            _ => ty,
        }
    }

    fn collect_functions(&mut self, program: &ast::Program, table: &mut CompilerSymbolTable) {
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Function(func) => {
                    let is_var = func.is_variadic || func.parameters.iter().any(|p| p.is_variadic);
                    self.collect_function_item(func, is_var, table);
                }
                ast::ItemKind::ExternFunction(func) => {
                    let stub = ast::FunctionItem {
                        name: func.name.clone(),
                        generics: None,
                        is_variadic: func.signature.is_variadic,
                        parameters: func.signature.parameters.clone(),
                        return_type: func.signature.return_type.clone(),
                        body: ast::Block {
                            statements: Vec::new(),
                            span: func.name.span,
                        },
                    };
                    self.collect_function_item(&stub, func.signature.is_variadic, table);
                }
                ast::ItemKind::ExternBlock(block) => {
                    for func in &block.functions {
                        let stub = ast::FunctionItem {
                            name: func.name.clone(),
                            generics: None,
                            is_variadic: func.signature.is_variadic,
                            parameters: func.signature.parameters.clone(),
                            return_type: func.signature.return_type.clone(),
                            body: ast::Block {
                                statements: Vec::new(),
                                span: func.name.span,
                            },
                        };
                        self.collect_function_item(&stub, func.signature.is_variadic, table);
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_extern_variables(
        &mut self,
        program: &ast::Program,
        table: &mut CompilerSymbolTable,
    ) {
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::ExternVariable(var) => {
                    let symbol_key = format!("extern_var::{}", var.name.name);
                    table.intern_symbol(
                        symbol_key,
                        SymbolKind::ExternVariable,
                        Some(var.name.span),
                        CompilerPhase::TypeCheck,
                    );
                    self.extern_variables
                        .insert(var.name.name.clone(), Type::from_ast(&var.var_type));
                }
                ast::ItemKind::ExternBlock(block) => {
                    for var in &block.variables {
                        let symbol_key = format!("extern_var::{}", var.name.name);
                        table.intern_symbol(
                            symbol_key,
                            SymbolKind::ExternVariable,
                            Some(var.name.span),
                            CompilerPhase::TypeCheck,
                        );
                        self.extern_variables
                            .insert(var.name.name.clone(), Type::from_ast(&var.var_type));
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_global_variables(
        &mut self,
        program: &ast::Program,
        table: &mut CompilerSymbolTable,
    ) {
        for item in &program.items {
            let ast::ItemKind::GlobalVariable(var) = &item.kind else {
                continue;
            };
            let symbol_key = format!("global_var::{}", var.name.name);
            table.intern_symbol(
                symbol_key,
                SymbolKind::GlobalVariable,
                Some(var.name.span),
                CompilerPhase::TypeCheck,
            );
            self.global_variables
                .insert(var.name.name.clone(), Type::from_ast(&var.var_type));
            if var.is_volatile {
                self.volatile_globals.insert(var.name.name.clone());
            }
        }
    }

    fn check_global_variable(&mut self, var: &ast::GlobalVariableItem) {
        let declared = Type::from_ast(&var.var_type);
        self.reject_plain_void_value_type(&declared, var.var_type.span);
        self.reject_wasm_extended_float(&declared, var.var_type.span);
        if let Some(init) = &var.initializer {
            let init_type = self.check_expr(init, Some(&declared));
            if !self.is_assignable(&declared, &init_type)
                && !self.is_implicitly_castable(&init_type, &declared)
            {
                self.error(msg::type_mismatch(&declared, &init_type), init.span);
            }
        }
    }

    fn collect_function_item(
        &mut self,
        func: &ast::FunctionItem,
        is_variadic: bool,
        table: &mut CompilerSymbolTable,
    ) {
        let mut type_params = Vec::new();
        if let Some(generics) = &func.generics {
            for param in &generics.params {
                if let ast::GenericParam::Type(type_param) = param {
                    type_params.push(type_param.name.name.clone());
                }
            }
        }
        let bounds = self.collect_bounds(func.generics.as_ref());
        let params = func
            .parameters
            .iter()
            .map(|param| Type::from_ast(&param.param_type))
            .collect::<Vec<_>>();
        let return_type = func
            .return_type
            .as_ref()
            .map(Type::from_ast)
            .unwrap_or(Type::Unit);
        let symbol_key = self.function_symbol_key(func, is_variadic);
        let symbol_id = table.intern_symbol(
            symbol_key.clone(),
            SymbolKind::Function,
            Some(func.name.span),
            CompilerPhase::TypeCheck,
        );
        debug_assert_eq!(table.symbol_id(&symbol_key), Some(symbol_id));
        debug_assert_eq!(table.symbol_key(symbol_id), Some(symbol_key.as_str()));
        self.function_symbols
            .entry(symbol_id)
            .or_insert_with(|| FunctionSig {
                params,
                return_type,
                type_params,
                bounds,
                source: std::sync::Arc::new(func.clone()),
                is_variadic,
                is_imported: false,
            });
        let overloads = self.functions.entry(func.name.name.clone()).or_default();
        // Identical redeclarations (e.g. the same `extern "C"` prototype
        // inlined from multiple imported files) intern to the same symbol id;
        // registering the id twice would make overload resolution report a
        // phantom ambiguity between a candidate and itself.
        if !overloads.contains(&symbol_id) {
            overloads.push(symbol_id);
        }
    }

    fn literal_type(
        &mut self,
        literal: &ast::Literal,
        expected: Option<&Type>,
        span: &Span,
    ) -> Type {
        // Integer literals default to the biggest integer type (i128) and
        // decompose down to the expected type with an overflow check when a
        // narrower integer type is expected (e.g. `u8 x = 300` errors).
        if let ast::Literal::Integer(value) = literal {
            // Source integer literals are always non-negative magnitudes in the
            // range 0..=u128::MAX (stored as wrapped two's-complement i128 bits).
            // Unary negation (`-n`) is represented as ast::UnaryOperator::Minus
            // and handled by check_expr, so bare integer literals have negated = false.
            let magnitude = *value as u128;
            return self.type_integer_literal_value(magnitude, false, expected, span);
        }
        if let Some(expected_ty) = expected
            && self.literal_matches_expected(literal, expected_ty)
        {
            return expected_ty.clone();
        }

        match literal {
            ast::Literal::Integer(_) => unreachable!("handled above"),
            ast::Literal::Float(_) => Type::Primitive(ast::PrimitiveType::F64),
            ast::Literal::Complex(_, _) => Type::Primitive(ast::PrimitiveType::C64),
            ast::Literal::String(_) => Type::Primitive(ast::PrimitiveType::Str),
            ast::Literal::Char(_) => Type::Primitive(ast::PrimitiveType::Char),
            ast::Literal::Bool(_) => Type::Primitive(ast::PrimitiveType::Bool),
        }
    }

    /// Type an integer literal value against an optional expected type: the
    /// biggest integer type (i128) by default, narrowed to the expected
    /// integer type with an overflow error when it does not fit.
    ///
    /// `magnitude` is the SOURCE magnitude (the lexer stores full-u128
    /// magnitudes as their two's-complement bit pattern; `as u128` recovers
    /// them exactly), and `negated` records a unary minus. Keeping them
    /// separate is what distinguishes `u128 x = u128::MAX` (wrapped, legal)
    /// from `u128 x = -1` (negated, illegal) — the combined i128 value alone
    /// cannot.
    fn type_integer_literal_value(
        &mut self,
        magnitude: u128,
        negated: bool,
        expected: Option<&Type>,
        span: &Span,
    ) -> Type {
        if let Some(expected_ty) = expected
            && let Type::Primitive(prim) = expected_ty
        {
            if let Some(max) = Self::unsigned_prim_max(prim) {
                // A negated literal never fits an unsigned type
                // (`u8 x = -5`, `u128 x = -1` both error).
                if negated || magnitude > max {
                    let shown = if negated {
                        format!("-{magnitude}")
                    } else {
                        format!("{magnitude}")
                    };
                    self.error(
                        format!("integer literal {shown} does not fit in type {:?}", prim),
                        *span,
                    );
                }
                return expected_ty.clone();
            }
            if let Some((min, max)) = Self::signed_prim_range(prim) {
                let fits = if negated {
                    match magnitude {
                        m if m <= i128::MAX as u128 => {
                            let v = -(m as i128);
                            v >= min && v <= max
                        }
                        // -(2^127) is exactly i128::MIN.
                        m if m == (i128::MAX as u128) + 1 => min == i128::MIN,
                        _ => false,
                    }
                } else {
                    magnitude <= i128::MAX as u128 && {
                        let v = magnitude as i128;
                        v >= min && v <= max
                    }
                };
                if !fits {
                    let shown = if negated {
                        format!("-{magnitude}")
                    } else {
                        format!("{magnitude}")
                    };
                    self.error(
                        format!("integer literal {shown} does not fit in type {:?}", prim),
                        *span,
                    );
                }
                return expected_ty.clone();
            }
        }
        Type::Primitive(ast::PrimitiveType::I128)
    }

    /// Inclusive maximum for an unsigned integer primitive, or None.
    fn unsigned_prim_max(prim: &ast::PrimitiveType) -> Option<u128> {
        Some(match prim {
            ast::PrimitiveType::U8 => u8::MAX as u128,
            ast::PrimitiveType::U16 => u16::MAX as u128,
            ast::PrimitiveType::U32 => u32::MAX as u128,
            ast::PrimitiveType::U64 => u64::MAX as u128,
            ast::PrimitiveType::U128 => u128::MAX,
            _ => return None,
        })
    }

    /// Inclusive value range for an integer primitive, or None for non-ints.
    /// Unsigned prims report the signed-representable subset; checks that
    /// need the full source magnitude use `unsigned_prim_max` instead.
    fn integer_prim_range(prim: &ast::PrimitiveType) -> Option<(i128, i128)> {
        if let Some((min, max)) = Self::signed_prim_range(prim) {
            return Some((min, max));
        }
        Self::unsigned_prim_max(prim).map(|max| (0, (max as i128).min(i128::MAX)))
    }

    /// Inclusive value range for a signed integer primitive, or None.
    fn signed_prim_range(prim: &ast::PrimitiveType) -> Option<(i128, i128)> {
        Some(match prim {
            ast::PrimitiveType::I8 => (i8::MIN as i128, i8::MAX as i128),
            ast::PrimitiveType::I16 => (i16::MIN as i128, i16::MAX as i128),
            ast::PrimitiveType::I32 => (i32::MIN as i128, i32::MAX as i128),
            ast::PrimitiveType::I64 => (i64::MIN as i128, i64::MAX as i128),
            ast::PrimitiveType::I128 => (i128::MIN, i128::MAX),
            _ => return None,
        })
    }

    /// True when an integer literal value fits the integer primitive.
    ///
    /// `value` is the combined signed value (unary minus already applied), so
    /// a negative value never fits an unsigned primitive. Note this cannot
    /// see a wrapped positive magnitude (full-u128 literals): the
    /// expected-type path routes those through `type_integer_literal_value`
    /// with magnitude/negated kept separate.
    fn integer_value_fits(value: i128, prim: &ast::PrimitiveType) -> bool {
        if let Some(max) = Self::unsigned_prim_max(prim) {
            return value >= 0 && (value as u128) <= max;
        }
        Self::signed_prim_range(prim).is_some_and(|(min, max)| value >= min && value <= max)
    }

    /// Extract the effective integer value of a literal expression, honoring
    /// unary +/- (so `-128` yields -128, not 128).
    fn literal_integer_value(expr: &ast::Expression) -> Option<i128> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Literal(ast::Literal::Integer(n)) => Some(*n),
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Minus,
                operand,
            } => match operand.kind.as_ref() {
                ast::ExpressionKind::Literal(ast::Literal::Integer(n)) => n.checked_neg(),
                _ => None,
            },
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Plus,
                operand,
            } => match operand.kind.as_ref() {
                ast::ExpressionKind::Literal(ast::Literal::Integer(n)) => Some(*n),
                _ => None,
            },
            _ => None,
        }
    }

    /// Natural integer type for a literal value: the smallest standard width
    /// that holds it (i32, then i64, then i128). Used when resolution must
    /// pick between candidates (overloads, generic inference) so that
    /// `foo(1)` infers T = i32 rather than the i128 default.
    fn literal_natural_type(value: i128) -> ast::PrimitiveType {
        if value >= i32::MIN as i128 && value <= i32::MAX as i128 {
            ast::PrimitiveType::I32
        } else if value >= i64::MIN as i128 && value <= i64::MAX as i128 {
            ast::PrimitiveType::I64
        } else {
            ast::PrimitiveType::I128
        }
    }

    /// Type-check an expression, but report integer literals at their natural
    /// width (i32/i64/i128 by value) instead of the i128 default, so overload
    /// and generic-parameter resolution sees the literal's intrinsic type.
    fn check_expr_with_literal_naturals(&mut self, expr: &ast::Expression) -> Type {
        if let Some(value) = Self::literal_integer_value(expr) {
            return Type::Primitive(Self::literal_natural_type(value));
        }
        self.check_expr(expr, None)
    }

    fn literal_matches_expected(&self, literal: &ast::Literal, expected: &Type) -> bool {
        if let Some(backing) = self.enum_backing_type(expected)
            && !matches!(expected, Type::Primitive(_))
        {
            return self.literal_matches_expected(literal, &Type::Primitive(backing));
        }

        match literal {
            ast::Literal::Integer(_) => is_integer(expected),
            ast::Literal::Float(_) => matches!(
                expected,
                Type::Primitive(
                    ast::PrimitiveType::F32 | ast::PrimitiveType::F64 | ast::PrimitiveType::F80
                )
            ),
            ast::Literal::Complex(_, _) => matches!(
                expected,
                Type::Primitive(
                    ast::PrimitiveType::C32 | ast::PrimitiveType::C64 | ast::PrimitiveType::C80
                )
            ),
            ast::Literal::String(_) => {
                is_string(expected)
                    || matches!(expected, Type::Slice { element } if Self::is_byte_element(element))
                    || matches!(expected, Type::Reference { inner, .. } | Type::Pointer { inner, .. } if matches!(inner.as_ref(), Type::Slice { element } if Self::is_byte_element(element)))
            }
            ast::Literal::Char(value) => {
                if matches!(expected, Type::Primitive(ast::PrimitiveType::Char)) {
                    return true;
                }
                // A char literal (a code point, 0..=0x10FFFF) also fits any
                // integer type whose range covers it: `u8 b = 'a';` needs no
                // cast.
                if let Type::Primitive(prim) = expected
                    && let Some((min, max)) = Self::integer_prim_range(prim)
                    && i128::from(u32::from(*value)) >= min
                    && i128::from(u32::from(*value)) <= max
                {
                    return true;
                }
                false
            }
            ast::Literal::Bool(_) => is_bool(expected),
        }
    }

    fn collect_bounds(&self, generics: Option<&ast::Generics>) -> Vec<TypeBoundPredicate> {
        let Some(generics) = generics else {
            return Vec::new();
        };
        let mut bounds = Vec::new();
        for param in &generics.params {
            if let ast::GenericParam::Type(type_param) = param
                && !type_param.bounds.is_empty()
            {
                bounds.push(TypeBoundPredicate {
                    bounded: Type::Named {
                        path: vec![type_param.name.name.clone()],
                        generics: Vec::new(),
                    },
                    bounds: type_param.bounds.clone(),
                });
            }
        }
        if let Some(where_clause) = &generics.where_clause {
            for predicate in &where_clause.predicates {
                if let ast::WherePredicate::Type {
                    bounded_type,
                    bounds: preds,
                } = predicate
                {
                    bounds.push(TypeBoundPredicate {
                        bounded: Type::from_ast(bounded_type),
                        bounds: preds.clone(),
                    });
                }
            }
        }
        bounds
    }

    fn bounds_satisfied(
        &mut self,
        bounds: &[TypeBoundPredicate],
        mapping: &HashMap<String, Type>,
        span: Span,
    ) -> bool {
        let mut ok = true;
        for predicate in bounds {
            let bounded = predicate.bounded.substitute(mapping);
            if !self.is_concrete_type(&bounded) {
                continue;
            }
            for bound in &predicate.bounds {
                let trait_name = bound
                    .trait_ref
                    .path
                    .last()
                    .map(|id| id.name.clone())
                    .unwrap_or_default();
                if trait_name.is_empty() {
                    continue;
                }
                let args = bound
                    .trait_ref
                    .generics
                    .as_deref()
                    .unwrap_or_default()
                    .iter()
                    .map(|arg| Type::from_ast(arg).substitute(mapping))
                    .collect::<Vec<_>>();
                let has_impl = self.has_trait_impl(&trait_name, &bounded, &args, 0);
                if !has_impl {
                    self.error(
                        format!("missing impl for bound '{}' on {:?}", trait_name, bounded),
                        span,
                    );
                    ok = false;
                }
            }
        }
        ok
    }

    fn record_function_monomorph(
        &mut self,
        candidate: &FunctionSig,
        mapping: &HashMap<String, Type>,
        span: Span,
    ) {
        if candidate.type_params.is_empty() {
            return;
        }
        if !self.mapping_is_concrete(mapping, &candidate.type_params) {
            return;
        }
        self.monomorph_requests.push(MonomorphRequest::Function {
            source: Box::new((*candidate.source).clone()),
            type_params: candidate.type_params.clone(),
            mapping: mapping.clone(),
            call_span: span,
            is_imported: candidate.is_imported,
        });
    }

    fn record_method_monomorph(
        &mut self,
        candidate: &MethodSig,
        mapping: &HashMap<String, Type>,
        span: Span,
    ) {
        if candidate.type_params.is_empty() && candidate.source_impl.generics.is_none() {
            return;
        }
        if !self.mapping_is_concrete(mapping, &candidate.type_params) {
            return;
        }
        self.monomorph_requests.push(MonomorphRequest::ImplMethod {
            impl_item: Box::new((*candidate.source_impl).clone()),
            method: Box::new((*candidate.source_method).clone()),
            type_params: candidate.type_params.clone(),
            mapping: mapping.clone(),
            call_span: span,
        });
    }

    fn mapping_is_concrete(&self, mapping: &HashMap<String, Type>, type_params: &[String]) -> bool {
        type_params.iter().all(|param| {
            mapping
                .get(param)
                .is_some_and(|ty| self.is_concrete_type(ty))
        })
    }

    fn dedup_type_params(&self, params: Vec<String>) -> Vec<String> {
        let mut seen = HashSet::default();
        let mut out = Vec::new();
        for param in params {
            if seen.insert(param.clone()) {
                out.push(param);
            }
        }
        out
    }

    fn is_concrete_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Named { path, generics } => {
                if path.len() == 1 && !self.known_types.contains_key(&path[0]) {
                    return false;
                }
                generics.iter().all(|inner| self.is_concrete_type(inner))
            }
            Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
                self.is_concrete_type(inner)
            }
            Type::Slice { element } => self.is_concrete_type(element),
            Type::Task(inner) => self.is_concrete_type(inner),
            Type::Array { element, .. } => self.is_concrete_type(element),
            Type::Optional { inner } => self.is_concrete_type(inner),
            Type::Tuple(items) => items.iter().all(|inner| self.is_concrete_type(inner)),
            Type::Function {
                params,
                return_type,
            } => {
                params.iter().all(|inner| self.is_concrete_type(inner))
                    && self.is_concrete_type(return_type)
            }
            Type::Primitive(_) | Type::Unit | Type::Never => true,
            Type::Unknown => false,
        }
    }

    /// Byte-sized element types. `char` is deliberately absent: it is a
    /// 32-bit Unicode scalar, so treating it as byte-like would let a `u8*`
    /// pass as a `char*` and a char store would write four bytes into a
    /// one-byte buffer.
    pub(crate) fn is_byte_element(ty: &Type) -> bool {
        matches!(
            ty,
            Type::Primitive(ast::PrimitiveType::U8 | ast::PrimitiveType::I8)
        )
    }


    pub(crate) fn can_coerce_to_slice(&self, found: &Type, expected_elem: &Type) -> bool {
        match found {
            Type::Slice { element } => {
                self.is_assignable(expected_elem, element)
                    || (Self::is_byte_element(expected_elem) && Self::is_byte_element(element))
            }
            Type::Primitive(ast::PrimitiveType::Str) => Self::is_byte_element(expected_elem),
            Type::Named { path, generics } => {
                if path.last().map(|s| s.as_str()) == Some("String") && generics.is_empty() {
                    Self::is_byte_element(expected_elem)
                } else if path.last().map(|s| s.as_str()) == Some("Vec") && generics.len() == 1 {
                    self.is_assignable(expected_elem, &generics[0])
                        || (Self::is_byte_element(expected_elem) && Self::is_byte_element(&generics[0]))
                } else {
                    false
                }
            }
            Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
                if Self::is_byte_element(inner) && Self::is_byte_element(expected_elem) {
                    true
                } else {
                    self.can_coerce_to_slice(inner, expected_elem)
                }
            }
            Type::Array { element, .. } => {
                self.is_assignable(expected_elem, element)
                    || (Self::is_byte_element(expected_elem) && Self::is_byte_element(element))
            }
            _ => false,
        }
    }

    fn is_assignable(&self, expected: &Type, found: &Type) -> bool {
        if expected == found || Self::void_compatible(expected, found) {
            return true;
        }

        if let Type::Slice { element } = expected {
            if self.can_coerce_to_slice(found, element) {
                return true;
            }
        }
        if let Type::Reference {
            is_mutable: false,
            inner,
        }
        | Type::Pointer {
            is_mutable: false,
            inner,
            ..
        } = expected
        {
            if let Type::Slice { element } = inner.as_ref() {
                if self.can_coerce_to_slice(found, element) {
                    return true;
                }
            }
        }
        if let Type::Pointer { inner: e_inner, .. } | Type::Reference { inner: e_inner, .. } =
            expected
        {
            if let Type::Array {
                element: f_elem, ..
            } = found
            {
                if e_inner == f_elem || self.is_assignable(e_inner, f_elem) {
                    return true;
                }
            }
        }

        // References and pointers share the same representation: a reference
        // is accepted where a pointer is expected and vice versa (legacy `&x`
        // expressions type as Pointer while `&T` params are Reference).
        // Non-volatile pointers only (a volatile pointee must not silently
        // become a plain reference).
        let view_compatible = match (expected, found) {
            (
                Type::Reference { inner: e_inner, .. },
                Type::Pointer {
                    inner: f_inner,
                    is_volatile,
                    ..
                },
            ) => {
                // Transparent reborrow (mirrors inference above): a pointer
                // to a reference used as a reference dereferences through.
                !is_volatile
                    && match f_inner.as_ref() {
                        Type::Reference { inner: deref, .. } => e_inner.as_ref() == deref.as_ref(),
                        _ => e_inner == f_inner,
                    }
            }
            (
                Type::Pointer {
                    inner: e_inner,
                    is_volatile,
                    ..
                },
                Type::Reference { inner: f_inner, .. },
            ) => !is_volatile && e_inner == f_inner,
            // Mutable references coerce to immutable ones.
            (
                Type::Reference {
                    is_mutable,
                    inner: e_inner,
                },
                Type::Reference {
                    is_mutable: f_mut,
                    inner: f_inner,
                },
            ) => (!*is_mutable || *f_mut) && e_inner == f_inner,
            // Byte pointers (u8*/i8*) convert implicitly; a mutable source
            // may become a const target, never the reverse. Volatile pointees
            // must not silently become plain pointers. `char*` is excluded:
            // a char is four bytes wide, so the pointers do not alias.
            (
                Type::Pointer {
                    is_mutable: to_mut,
                    is_volatile: to_vol,
                    inner: to_inner,
                },
                Type::Pointer {
                    is_mutable: from_mut,
                    is_volatile: from_vol,
                    inner: from_inner,
                },
            ) if Self::is_byte_element(to_inner.as_ref())
                && Self::is_byte_element(from_inner.as_ref()) =>
            {
                !*to_vol && !*from_vol && (!*to_mut || *from_mut)
            }
            // str and u8* are the same byte pointer: convert implicitly in
            // both directions (str is a non-owning view of a NUL-terminated
            // byte buffer, so mutable u8* is accepted as well).
            (Type::Primitive(ast::PrimitiveType::Str), Type::Pointer { inner, .. }) => {
                Self::is_byte_element(inner.as_ref())
            }
            (Type::Pointer { inner, .. }, Type::Primitive(ast::PrimitiveType::Str)) => {
                Self::is_byte_element(inner.as_ref())
            }
            (Type::Primitive(ast::PrimitiveType::Str), Type::Slice { element }) => {
                Self::is_byte_element(element.as_ref())
            }
            (Type::Slice { element }, Type::Primitive(ast::PrimitiveType::Str)) => {
                Self::is_byte_element(element.as_ref())
            }
            (Type::Pointer { inner, .. }, Type::Slice { element }) => {
                Self::is_byte_element(inner.as_ref())
                    && Self::is_byte_element(element.as_ref())
            }
            _ => false,
        };
        if view_compatible {
            return true;
        }
        if let (Type::Primitive(expected_p), Type::Primitive(found_p)) = (expected, found) {
            // 8-bit byte and char types are mutually assignable:
            // char <-> u8, char <-> i8, u8 <-> i8
            if matches!(
                (expected_p, found_p),
                (
                    ast::PrimitiveType::Char | ast::PrimitiveType::U8 | ast::PrimitiveType::I8,
                    ast::PrimitiveType::Char | ast::PrimitiveType::U8 | ast::PrimitiveType::I8
                )
            ) {
                return true;
            }
        }
        if let (Type::Tuple(expected_items), Type::Tuple(found_items)) = (expected, found) {
            if expected_items.len() == found_items.len()
                && expected_items
                    .iter()
                    .zip(found_items.iter())
                    .all(|(e, f)| self.is_assignable(e, f) || self.is_implicitly_castable(f, e))
            {
                return true;
            }
        }
        match (
            self.enum_backing_type(expected),
            self.enum_backing_type(found),
        ) {
            (Some(expected_backing), Some(found_backing)) => expected_backing == found_backing,
            _ => false,
        }
    }

    fn infer_type_params(
        &self,
        expected: &Type,
        found: &Type,
        type_params: &[String],
        mapping: &mut HashMap<String, Type>,
    ) -> bool {
        if let Type::Named { path, generics } = expected
            && path.len() == 1
            && type_params.contains(&path[0])
            && generics.is_empty()
        {
            if let Some(existing) = mapping.get(&path[0]) {
                return existing == found;
            }
            mapping.insert(path[0].clone(), found.clone());
            return true;
        }

        match (expected, found) {
            (Type::Primitive(_), _) | (Type::Unit, _) => {
                expected == found || Self::void_compatible(expected, found)
            }
            (
                Type::Pointer {
                    is_mutable,
                    is_volatile: _,
                    inner,
                },
                Type::Primitive(ast::PrimitiveType::Str),
            ) => {
                if *is_mutable {
                    return false;
                }
                Self::is_byte_element(inner.as_ref())
            }
            (
                Type::Named { path, generics },
                Type::Named {
                    path: found_path,
                    generics: found_generics,
                },
            ) => {
                if path != found_path {
                    return false;
                }
                // If the expected type has generic arguments but the found type
                // does not, allow the mismatch when the missing generics are
                // all type-parameter references (they'll be inferred from
                // argument types instead).
                if generics.len() != found_generics.len() {
                    if found_generics.is_empty() && generics.iter().all(|g| {
                        matches!(g, Type::Named { path, generics: gs } if path.len() == 1 && gs.is_empty() && type_params.contains(&path[0]))
                    }) {
                        return true;
                    }
                    return false;
                }
                for (exp, got) in generics.iter().zip(found_generics.iter()) {
                    if !self.infer_type_params(exp, got, type_params, mapping) {
                        return false;
                    }
                }
                true
            }
            (
                Type::Reference { is_mutable, inner },
                Type::Reference {
                    is_mutable: found_mut,
                    inner: found_inner,
                },
            ) => {
                // Mutable references coerce to immutable ones.
                (!*is_mutable || *found_mut)
                    && self.infer_type_params(inner, found_inner, type_params, mapping)
            }
            (
                Type::Reference { inner, .. },
                Type::Pointer {
                    inner: found_inner, ..
                },
            ) => {
                // Address-of always adds a Pointer layer, so a Pointer-typed
                // argument whose pointee is itself a reference (e.g. `&mut r`
                // where r: &mut T) must dereference through before binding:
                // the variable denotes the referent T, not the pointer slot.
                match found_inner.as_ref() {
                    Type::Reference { inner: deref, .. } => {
                        self.infer_type_params(inner, deref, type_params, mapping)
                    }
                    _ => self.infer_type_params(inner, found_inner, type_params, mapping),
                }
            }
            (
                Type::Pointer { inner, .. },
                Type::Reference {
                    inner: found_inner, ..
                },
            ) => self.infer_type_params(inner, found_inner, type_params, mapping),
            (
                Type::Pointer {
                    is_mutable,
                    is_volatile,
                    inner,
                },
                Type::Pointer {
                    is_mutable: found_mut,
                    is_volatile: found_volatile,
                    inner: found_inner,
                },
            ) => {
                is_mutable == found_mut
                    && is_volatile == found_volatile
                    && self.infer_type_params(inner, found_inner, type_params, mapping)
            }
            (
                Type::Slice { element },
                Type::Slice {
                    element: found_elem,
                },
            ) => self.infer_type_params(element, found_elem, type_params, mapping),
            (Type::Slice { element }, Type::Named { path, generics })
                if path.last().map(|s| s.as_str()) == Some("Vec") && generics.len() == 1 =>
            {
                self.infer_type_params(element, &generics[0], type_params, mapping)
            }
            (
                Type::Slice { element },
                Type::Array {
                    element: found_elem,
                    ..
                },
            ) => self.infer_type_params(element, found_elem, type_params, mapping),
            (Type::Slice { .. }, Type::Reference { inner, .. } | Type::Pointer { inner, .. }) => {
                self.infer_type_params(expected, inner, type_params, mapping)
            }
            (Type::Reference { inner, .. }, found)
                if matches!(inner.as_ref(), Type::Slice { .. }) =>
            {
                self.infer_type_params(inner, found, type_params, mapping)
            }
            (
                Type::Pointer { inner, .. } | Type::Reference { inner, .. },
                Type::Array {
                    element: f_elem, ..
                },
            ) => self.infer_type_params(inner, f_elem, type_params, mapping),
            (Type::Optional { inner }, Type::Optional { inner: found_inner }) => {
                self.infer_type_params(inner, found_inner, type_params, mapping)
            }
            (Type::Tuple(items), Type::Tuple(found_items)) => {
                if items.len() != found_items.len() {
                    return false;
                }
                for (exp, got) in items.iter().zip(found_items.iter()) {
                    if !self.infer_type_params(exp, got, type_params, mapping) {
                        return false;
                    }
                }
                true
            }
            (
                Type::Function {
                    params,
                    return_type,
                },
                Type::Function {
                    params: found_params,
                    return_type: found_return,
                },
            ) => {
                if params.len() != found_params.len() {
                    return false;
                }
                for (exp, got) in params.iter().zip(found_params.iter()) {
                    if !self.infer_type_params(exp, got, type_params, mapping) {
                        return false;
                    }
                }
                self.infer_type_params(return_type, found_return, type_params, mapping)
            }
            (Type::Unknown, _) => true,
            _ => false,
        }
    }

    fn substitute_type(&self, ty: &Type, mapping: &HashMap<String, Type>) -> Type {
        ty.substitute(mapping)
    }

    fn is_castable(&self, from: &Type, to: &Type) -> bool {
        if from == to {
            return true;
        }
        if matches!(from, Type::Unknown) || matches!(to, Type::Unknown) {
            return true;
        }
        if Self::is_void_like(from) || Self::is_void_like(to) {
            return Self::void_compatible(from, to);
        }
        let from_ok = self.is_primitive_type(from)
            || matches!(
                from,
                Type::Pointer { .. }
                    | Type::Reference { .. }
                    | Type::Function { .. }
                    | Type::Array { .. }
            );
        let to_ok = self.is_primitive_type(to)
            || matches!(
                to,
                Type::Pointer { .. } | Type::Reference { .. } | Type::Function { .. }
            );
        if from_ok && to_ok {
            return true;
        }
        if let Type::Slice { element } = to {
            if self.can_coerce_to_slice(from, element) {
                return true;
            }
        }
        if let (Some(from_backing), Some(to_backing)) =
            (self.enum_backing_type(from), self.enum_backing_type(to))
            && (!matches!(from, Type::Primitive(_)) || !matches!(to, Type::Primitive(_)))
        {
            return self.is_castable(&Type::Primitive(from_backing), &Type::Primitive(to_backing));
        }
        if self.is_primitive_type(from) && self.is_primitive_type(to) {
            return true;
        }
        self.casts
            .contains_key(&(self.method_key(from), self.method_key(to)))
    }

    pub(crate) fn size_typeck(&mut self, expr: &ast::Expression, args: &[ast::MacroArg]) -> Type {
        if args.len() != 1 {
            self.error(msg::size_exactly_one(), expr.span);
            return Type::Unknown;
        }
        let inner_expr = match args.first() {
            Some(ast::MacroArg::Expression(e)) => e,
            _ => {
                self.error(msg::size_expression(), expr.span);
                return Type::Unknown;
            }
        };
        let sized_ty = self.resolve_type_name(inner_expr);
        let sized_ty = match sized_ty {
            Some(ty) => ty,
            None => {
                // Could be a generic type parameter — defer to codegen
                if matches!(inner_expr.kind.as_ref(), ast::ExpressionKind::Identifier(_)) {
                    return Type::Primitive(ast::PrimitiveType::U64);
                }
                self.error(
                    "cannot determine size: argument is not a known type name or variable"
                        .to_string(),
                    expr.span,
                );
                return Type::Unknown;
            }
        };
        let layout = self.type_ctx.layout_of(&sized_ty);
        if layout.size.is_some() {
            Type::Primitive(ast::PrimitiveType::U64)
        } else if matches!(&sized_ty, Type::Named { .. }) {
            // Generic type param like T — will be resolved during monomorphization
            Type::Primitive(ast::PrimitiveType::U64)
        } else {
            self.error(
                format!("cannot determine size of type {}", sized_ty),
                expr.span,
            );
            Type::Unknown
        }
    }

    pub(crate) fn align_typeck(&mut self, expr: &ast::Expression, args: &[ast::MacroArg]) -> Type {
        if args.len() != 1 {
            self.error(msg::align_exactly_one(), expr.span);
            return Type::Unknown;
        }
        let inner_expr = match args.first() {
            Some(ast::MacroArg::Expression(e)) => e,
            _ => {
                self.error(msg::align_expression(), expr.span);
                return Type::Unknown;
            }
        };
        let aligned_ty = self.resolve_type_name(inner_expr);
        let aligned_ty = match aligned_ty {
            Some(ty) => ty,
            None => {
                // Could be a generic type parameter — defer to codegen
                if matches!(inner_expr.kind.as_ref(), ast::ExpressionKind::Identifier(_)) {
                    return Type::Primitive(ast::PrimitiveType::U64);
                }
                self.error(
                    "cannot determine alignment: argument is not a known type name or variable"
                        .to_string(),
                    expr.span,
                );
                return Type::Unknown;
            }
        };
        let layout = self.type_ctx.layout_of(&aligned_ty);
        if layout.align.is_some() {
            Type::Primitive(ast::PrimitiveType::U64)
        } else if matches!(&aligned_ty, Type::Named { .. }) {
            // Generic type param like T — will be resolved during monomorphization
            Type::Primitive(ast::PrimitiveType::U64)
        } else {
            self.error(
                format!("cannot determine alignment of type {}", aligned_ty),
                expr.span,
            );
            Type::Unknown
        }
    }

    pub(crate) fn hash_typeck(&mut self, expr: &ast::Expression, args: &[ast::MacroArg]) -> Type {
        if args.len() != 1 {
            self.error(msg::hash_exactly_one(), expr.span);
            return Type::Unknown;
        }
        let Some(ast::MacroArg::Expression(inner_expr)) = args.first() else {
            self.error(msg::hash_expression(), expr.span);
            return Type::Unknown;
        };
        // Type-check the inner expression (emits errors if invalid, returns type)
        let _ = self.check_expr(inner_expr, None);
        // @hash always returns i64
        Type::Primitive(ast::PrimitiveType::I64)
    }

    pub(crate) fn memcpy_typeck(&mut self, expr: &ast::Expression, args: &[ast::MacroArg]) -> Type {
        if args.len() != 3 {
            self.error(msg::memcpy_expects_three(), expr.span);
            return Type::Unknown;
        }
        // dst
        if let ast::MacroArg::Expression(e) = &args[0] {
            self.check_expr(e, None);
        }
        // src
        if let ast::MacroArg::Expression(e) = &args[1] {
            self.check_expr(e, None);
        }
        // len
        if let ast::MacroArg::Expression(e) = &args[2] {
            self.check_expr(e, None);
        }
        // Returns the destination pointer (u8*)
        Type::Pointer {
            is_mutable: true,
            is_volatile: false,
            inner: Box::new(Type::Primitive(ast::PrimitiveType::U8)),
        }
    }

    pub(crate) fn memset_typeck(&mut self, expr: &ast::Expression, args: &[ast::MacroArg]) -> Type {
        if args.len() != 3 {
            self.error(msg::memset_expects_three(), expr.span);
            return Type::Unknown;
        }
        // dst
        if let ast::MacroArg::Expression(e) = &args[0] {
            self.check_expr(e, None);
        }
        // value
        if let ast::MacroArg::Expression(e) = &args[1] {
            self.check_expr(e, None);
        }
        // len
        if let ast::MacroArg::Expression(e) = &args[2] {
            self.check_expr(e, None);
        }
        Type::Pointer {
            is_mutable: true,
            is_volatile: false,
            inner: Box::new(Type::Primitive(ast::PrimitiveType::U8)),
        }
    }

    pub(crate) fn memmove_typeck(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        if args.len() != 3 {
            self.error(msg::memmove_expects_three(), expr.span);
            return Type::Unknown;
        }
        // dst
        if let ast::MacroArg::Expression(e) = &args[0] {
            self.check_expr(e, None);
        }
        // src
        if let ast::MacroArg::Expression(e) = &args[1] {
            self.check_expr(e, None);
        }
        // len
        if let ast::MacroArg::Expression(e) = &args[2] {
            self.check_expr(e, None);
        }
        Type::Pointer {
            is_mutable: true,
            is_volatile: false,
            inner: Box::new(Type::Primitive(ast::PrimitiveType::U8)),
        }
    }

    pub(crate) fn drop_in_place_typeck(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        if args.len() != 2 && args.len() != 3 {
            self.error(
                format!(
                    "@drop_in_place expects 2 or 3 arguments, got {}",
                    args.len()
                ),
                expr.span,
            );
            return Type::Unknown;
        }
        let (ptr_arg, count_arg) = if args.len() == 3 {
            (&args[1], &args[2])
        } else {
            (&args[0], &args[1])
        };
        if let ast::MacroArg::Expression(e) = ptr_arg {
            self.check_expr(e, None);
        }
        if let ast::MacroArg::Expression(e) = count_arg {
            self.check_expr(e, None);
        }
        Type::Primitive(ast::PrimitiveType::Void)
    }

    pub(crate) fn json_typeck(&mut self, expr: &ast::Expression, args: &[ast::MacroArg]) -> Type {
        if !(1..=2).contains(&args.len()) {
            self.error(msg::json_arg_count(), expr.span);
            return Type::Unknown;
        }

        let value_expr = match &args[0] {
            ast::MacroArg::Expression(value) => value,
            _ => {
                self.error(msg::json_expression(), expr.span);
                return Type::Unknown;
            }
        };
        let value_ty = self.check_expr(value_expr, None);
        let owner_ty = Self::json_owner_type(&value_ty);
        if !self.has_trait_impl("ToJson", &owner_ty, &[], 0) {
            self.error(
                format!("type {} does not implement ToJson", owner_ty),
                value_expr.span,
            );
        }

        if args.len() == 2 {
            match &args[1] {
                ast::MacroArg::Expression(prefix) => {
                    let prefix_ty = self.check_expr(prefix, None);
                    if prefix_ty != Type::Primitive(ast::PrimitiveType::Str) {
                        self.error(msg::json_prefix_type(), prefix.span);
                    }
                }
                _ => self.error(msg::json_prefix_string(), expr.span),
            }
        }

        Type::Primitive(ast::PrimitiveType::Str)
    }

    pub(crate) fn json_from_typeck(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        if args.len() != 2 {
            self.error(msg::from_json_arg_count(), expr.span);
            return Type::Unknown;
        }
        let target = match &args[0] {
            ast::MacroArg::Expression(target) => target,
            _ => {
                self.error(msg::from_json_target(), expr.span);
                return Type::Unknown;
            }
        };
        let target_ty = match target.kind.as_ref() {
            ast::ExpressionKind::TypeName(ty) => Type::from_ast(ty),
            ast::ExpressionKind::Identifier(identifier) => {
                if let Some(primitive) = crate::types::parse_primitive_name(&identifier.name) {
                    Type::Primitive(primitive)
                } else if self.known_types.contains_key(&identifier.name) {
                    Type::Named {
                        path: vec![identifier.name.clone()],
                        generics: Vec::new(),
                    }
                } else {
                    Type::Unknown
                }
            }
            _ => Type::Unknown,
        };
        if target_ty == Type::Unknown {
            self.error(msg::from_json_named_target(), target.span);
            return Type::Unknown;
        }
        let input = match &args[1] {
            ast::MacroArg::Expression(input) => input,
            _ => {
                self.error(msg::from_json_input(), expr.span);
                return Type::Unknown;
            }
        };
        if self.check_expr(input, None) != Type::Primitive(ast::PrimitiveType::Str) {
            self.error(msg::from_json_input_type(), input.span);
        }
        if !self.has_trait_impl("FromJson", &target_ty, &[], 0) {
            self.error(
                format!("type {} does not implement FromJson", target_ty),
                target.span,
            );
        }
        Type::Named {
            path: vec!["Result".to_string()],
            generics: vec![
                target_ty,
                Type::Named {
                    path: vec!["JsonError".to_string()],
                    generics: Vec::new(),
                },
            ],
        }
    }

    fn json_owner_type(ty: &Type) -> Type {
        match ty {
            Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
                Self::json_owner_type(inner)
            }
            _ => ty.clone(),
        }
    }

    fn has_trait_impl(&self, trait_name: &str, ty: &Type, args: &[Type], depth: usize) -> bool {
        if depth >= MAX_TYPE_CHECK_DEPTH {
            return false;
        }
        self.source_trait_impls
            .get(trait_name)
            .is_some_and(|implementations| {
                implementations.iter().any(|implementation| {
                    if implementation.args.len() != args.len() {
                        return false;
                    }
                    let mut mapping = HashMap::default();
                    if !self.infer_type_params(
                        &implementation.owner,
                        ty,
                        &implementation.params,
                        &mut mapping,
                    ) || implementation.owner.substitute(&mapping) != *ty
                    {
                        return false;
                    }
                    for (param, arg) in implementation.args.iter().zip(args) {
                        if !self.infer_type_params(param, arg, &implementation.params, &mut mapping)
                            || param.substitute(&mapping) != *arg
                        {
                            return false;
                        }
                    }
                    implementation.bounds.iter().all(|predicate| {
                        let bounded = predicate.bounded.substitute(&mapping);
                        predicate.bounds.iter().all(|bound| {
                            let name = bound
                                .trait_ref
                                .path
                                .last()
                                .map(|id| id.name.as_str())
                                .unwrap_or_default();
                            let args = bound
                                .trait_ref
                                .generics
                                .as_deref()
                                .unwrap_or_default()
                                .iter()
                                .map(|arg| Type::from_ast(arg).substitute(&mapping))
                                .collect::<Vec<_>>();
                            self.has_trait_impl(name, &bounded, &args, depth + 1)
                        })
                    })
                })
            })
    }

    pub(crate) fn print_typeck(
        &mut self,
        name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        let fmt_arg_idx = if name == "fprint" { 1 } else { 0 };

        // Minimum args: format string (+ BufWriter for @fprint)
        let min_args = fmt_arg_idx + 1;
        if args.len() < min_args {
            self.error(msg::macro_requires_min_args(name, min_args), expr.span);
            return Type::Unknown;
        }

        // Extract format string
        let placeholder_count = match &args[fmt_arg_idx] {
            ast::MacroArg::Expression(e) => match e.kind.as_ref() {
                ast::ExpressionKind::Literal(ast::Literal::String(s)) => {
                    let segments = crate::builtin_macros::parse_format(s);
                    segments
                        .iter()
                        .filter(|seg| {
                            matches!(seg, crate::builtin_macros::FormatSegment::Placeholder)
                        })
                        .count()
                }
                _ => {
                    self.error(msg::format_string_must_be_literal(), e.span);
                    return Type::Unknown;
                }
            },
            _ => {
                self.error(msg::format_string_must_be_literal(), expr.span);
                return Type::Unknown;
            }
        };

        // Check value arg count
        let value_args_start = fmt_arg_idx + 1;
        let value_args_count = args.len().saturating_sub(value_args_start);

        if placeholder_count != value_args_count {
            self.error(
                msg::macro_expected_format_args(name, placeholder_count, value_args_count),
                expr.span,
            );
            return Type::Unknown;
        }

        // Type-check each value argument
        for arg in &args[value_args_start..] {
            if let ast::MacroArg::Expression(e) = arg {
                self.check_expr(e, None);
            }
        }

        // For @fprint, also type-check the BufWriter argument
        if name == "fprint"
            && let ast::MacroArg::Expression(e) = &args[0]
        {
            self.check_expr(e, None);
        }

        match name {
            "format" => Type::Named {
                path: vec!["String".to_string()],
                generics: Vec::new(),
            },
            "sprint" => Type::Primitive(ast::PrimitiveType::Str),
            _ => Type::Unit,
        }
    }

    fn resolve_type_name(&mut self, expr: &ast::Expression) -> Option<Type> {
        match &expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                let builtin = match ident.name.as_str() {
                    "i8" => Some(Type::Primitive(ast::PrimitiveType::I8)),
                    "i16" => Some(Type::Primitive(ast::PrimitiveType::I16)),
                    "i32" => Some(Type::Primitive(ast::PrimitiveType::I32)),
                    "i64" => Some(Type::Primitive(ast::PrimitiveType::I64)),
                    "i128" => Some(Type::Primitive(ast::PrimitiveType::I128)),
                    "u8" => Some(Type::Primitive(ast::PrimitiveType::U8)),
                    "u16" => Some(Type::Primitive(ast::PrimitiveType::U16)),
                    "u32" => Some(Type::Primitive(ast::PrimitiveType::U32)),
                    "u64" => Some(Type::Primitive(ast::PrimitiveType::U64)),
                    "u128" => Some(Type::Primitive(ast::PrimitiveType::U128)),
                    "f32" => Some(Type::Primitive(ast::PrimitiveType::F32)),
                    "f64" => Some(Type::Primitive(ast::PrimitiveType::F64)),
                    "f80" => Some(Type::Primitive(ast::PrimitiveType::F80)),
                    "c32" => Some(Type::Primitive(ast::PrimitiveType::C32)),
                    "c64" => Some(Type::Primitive(ast::PrimitiveType::C64)),
                    "c80" => Some(Type::Primitive(ast::PrimitiveType::C80)),
                    "bool" => Some(Type::Primitive(ast::PrimitiveType::Bool)),
                    "char" => Some(Type::Primitive(ast::PrimitiveType::Char)),
                    "str" => Some(Type::Primitive(ast::PrimitiveType::Str)),
                    "void" => Some(Type::Unit),
                    _ => None,
                };
                if let Some(ty) = builtin {
                    return Some(ty);
                }
                if let Some(ty) = self.lookup_type(&ident.name) {
                    return Some(ty);
                }
                if self.known_types.contains_key(&ident.name) {
                    return Some(Type::Named {
                        path: vec![ident.name.clone()],
                        generics: Vec::new(),
                    });
                }
                None
            }
            ast::ExpressionKind::TypeName(ty) => Some(Type::from_ast(ty)),
            _ => {
                let inner_ty = self.check_expr(expr, None);
                if inner_ty != Type::Unknown {
                    Some(inner_ty)
                } else {
                    None
                }
            }
        }
    }

    fn is_implicitly_castable(&self, from: &Type, to: &Type) -> bool {
        if Self::void_compatible(from, to) {
            return true;
        }
        if let (Some(from_backing), Some(to_backing)) =
            (self.enum_backing_type(from), self.enum_backing_type(to))
            && (!matches!(from, Type::Primitive(_)) || !matches!(to, Type::Primitive(_)))
        {
            return self.is_implicitly_castable(
                &Type::Primitive(from_backing),
                &Type::Primitive(to_backing),
            );
        }
        if from == to {
            return true;
        }
        if matches!(from, Type::Unknown) || matches!(to, Type::Unknown) {
            return true;
        }
        // Numeric and bool primitives are implicitly castable to each other
        if let (Type::Primitive(from_p), Type::Primitive(to_p)) = (from, to)
            && Self::is_numeric_or_bool_primitive(from_p)
            && Self::is_numeric_or_bool_primitive(to_p)
        {
            return true;
        }
        // User-defined casts
        if self
            .casts
            .contains_key(&(self.method_key(from), self.method_key(to)))
        {
            return true;
        }

        if let Type::Slice { element } = to {
            if self.can_coerce_to_slice(from, element) {
                return true;
            }
        }
        if let Type::Reference {
            is_mutable: false,
            inner,
        }
        | Type::Pointer {
            is_mutable: false,
            inner,
            ..
        } = to
        {
            if let Type::Slice { element } = inner.as_ref() {
                if self.can_coerce_to_slice(from, element) {
                    return true;
                }
            }
        }
        if let Type::Slice { element } = from {
            if matches!(to, Type::Primitive(ast::PrimitiveType::Str)) && Self::is_byte_element(element)
            {
                return true;
            }
            if let Type::Pointer {
                is_mutable: false,
                inner,
                ..
            } = to
            {
                if self.is_assignable(inner, element)
                    || (Self::is_byte_element(inner) && Self::is_byte_element(element))
                {
                    return true;
                }
            }
        }
        if let Type::Pointer {
            inner: to_inner, ..
        }
        | Type::Reference {
            inner: to_inner, ..
        } = to
        {
            if let Type::Array {
                element: from_elem, ..
            } = from
            {
                if to_inner == from_elem || self.is_implicitly_castable(from_elem, to_inner) {
                    return true;
                }
            }
        }

        match (from, to) {
            (
                Type::Primitive(ast::PrimitiveType::Str),
                Type::Pointer {
                    is_mutable,
                    is_volatile: _,
                    inner,
                },
            ) => {
                // str is a byte pointer; implicit conversion to const u8* or
                // const i8* (mutable targets require an explicit cast).
                // `char*` is excluded: a char is four bytes wide.
                if *is_mutable {
                    return false;
                }
                Self::is_byte_element(inner.as_ref())
            }
            (Type::Pointer { inner, .. }, Type::Primitive(ast::PrimitiveType::Str)) => {
                Self::is_byte_element(inner.as_ref())
            }
            // Byte pointers (u8*/i8*) convert implicitly; a mutable source
            // may become a const target, never the reverse. `char*` is
            // excluded: a char is four bytes wide, so the pointers do not
            // alias.
            (
                Type::Pointer {
                    is_mutable: to_mut,
                    is_volatile: to_vol,
                    inner: to_inner,
                },
                Type::Pointer {
                    is_mutable: from_mut,
                    is_volatile: from_vol,
                    inner: from_inner,
                },
            ) if Self::is_byte_element(to_inner.as_ref())
                && Self::is_byte_element(from_inner.as_ref()) =>
            {
                !*to_vol && !*from_vol && (!*to_mut || *from_mut)
            }
            (
                Type::Pointer {
                    is_mutable: from_mut,
                    ..
                },
                Type::Pointer {
                    is_mutable: to_mut,
                    is_volatile: _,
                    inner,
                },
            ) if is_void(inner.as_ref()) => !*to_mut || *from_mut,
            (
                Type::Pointer {
                    is_mutable: from_mut,
                    inner: from_inner,
                    ..
                },
                Type::Reference {
                    is_mutable: to_mut,
                    inner: to_inner,
                },
            )
            | (
                Type::Reference {
                    is_mutable: from_mut,
                    inner: from_inner,
                },
                Type::Pointer {
                    is_mutable: to_mut,
                    inner: to_inner,
                    ..
                },
            )
            | (
                Type::Reference {
                    is_mutable: from_mut,
                    inner: from_inner,
                },
                Type::Reference {
                    is_mutable: to_mut,
                    inner: to_inner,
                },
            ) => {
                (!*to_mut || *from_mut)
                    && (from_inner == to_inner || self.is_implicitly_castable(from_inner, to_inner))
            }
            (
                from_val,
                Type::Reference {
                    is_mutable: false,
                    inner: to_inner,
                },
            ) if self.is_concrete_type(from_val)
                && (from_val == to_inner.as_ref()
                    || self.is_implicitly_castable(from_val, to_inner)) =>
            {
                true
            }
            (
                Type::Pointer { inner, .. },
                Type::Function {
                    params,
                    return_type,
                },
            ) => {
                if let Type::Function {
                    params: from_params,
                    return_type: from_ret,
                } = inner.as_ref()
                {
                    from_params == params && from_ret == return_type
                } else {
                    false
                }
            }
            (
                Type::Function {
                    params,
                    return_type,
                },
                Type::Pointer { inner, .. },
            ) => {
                if let Type::Function {
                    params: to_params,
                    return_type: to_ret,
                } = inner.as_ref()
                {
                    params == to_params && return_type == to_ret
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn is_numeric_or_bool_primitive(p: &ast::PrimitiveType) -> bool {
        matches!(
            p,
            ast::PrimitiveType::I8
                | ast::PrimitiveType::I16
                | ast::PrimitiveType::I32
                | ast::PrimitiveType::I64
                | ast::PrimitiveType::I128
                | ast::PrimitiveType::U8
                | ast::PrimitiveType::U16
                | ast::PrimitiveType::U32
                | ast::PrimitiveType::U64
                | ast::PrimitiveType::U128
                | ast::PrimitiveType::F32
                | ast::PrimitiveType::F64
                | ast::PrimitiveType::F80
                | ast::PrimitiveType::C32
                | ast::PrimitiveType::C64
                | ast::PrimitiveType::C80
                | ast::PrimitiveType::Char
                | ast::PrimitiveType::Bool
        )
    }

    fn is_numeric_type(&self, ty: &Type) -> bool {
        self.numeric_type(ty).is_some()
    }

    fn is_integer_type(&self, ty: &Type) -> bool {
        self.numeric_type(ty)
            .map(|primitive| is_integer(&Type::Primitive(primitive)))
            .unwrap_or(false)
    }

    /// Result type of pointer arithmetic, or None when the operand shapes do
    /// not form a pointer + integer pair. `str` counts as a byte pointer, so
    /// `s + n`, `n + s`, and `s - n` are valid on strings.
    fn pointer_arith_result(
        &self,
        left: &Type,
        right: &Type,
        operator: &ast::BinaryOperator,
    ) -> Option<Type> {
        let is_ptr = |ty: &Type| {
            matches!(
                ty,
                Type::Pointer { .. }
                    | Type::Reference { .. }
                    | Type::Primitive(ast::PrimitiveType::Str)
            )
        };
        match operator {
            ast::BinaryOperator::Add => {
                if is_ptr(left) && self.is_integer_type(right) {
                    Some(left.clone())
                } else if is_ptr(right) && self.is_integer_type(left) {
                    Some(right.clone())
                } else {
                    None
                }
            }
            ast::BinaryOperator::Subtract => {
                if is_ptr(left) && self.is_integer_type(right) {
                    Some(left.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn receiver_compatible(&self, param: &Type, receiver: &Type, score: &mut usize) -> bool {
        match param {
            Type::Reference { inner, .. } => {
                if self.is_assignable(param, receiver) || self.is_assignable(inner, receiver) {
                    true
                } else if self.is_implicitly_castable(receiver, inner) {
                    *score += 1;
                    true
                } else {
                    false
                }
            }
            Type::Pointer { inner, .. } => {
                if self.is_assignable(param, receiver) {
                    true
                } else if self.is_assignable(inner, receiver) {
                    // Allow instance call on value receiver for pointer-self methods:
                    // `T x; x.m()` where method expects `T* self`.
                    true
                } else if self.is_implicitly_castable(receiver, inner) {
                    *score += 1;
                    true
                } else {
                    false
                }
            }
            _ => {
                if self.is_assignable(param, receiver) {
                    true
                } else if self.is_implicitly_castable(receiver, param) {
                    *score += 1;
                    true
                } else {
                    false
                }
            }
        }
    }

    /// Resolve a bare enum constructor (`Some(x)`, `None`, `Ok(x)`, `Err(x)`)
    /// against the expected type. Returns the constructed enum type and records
    /// a rewrite for the post-typeck AST pass; returns None when the name is
    /// not a recognized constructor or the expected type is not a matching enum.
    fn check_bare_enum_constructor(
        &mut self,
        name: &str,
        arguments: &[&ast::Expression],
        expected: Option<&Type>,
        span: &Span,
    ) -> Option<Type> {
        let (enum_name, variant) = match name {
            "Some" => ("Optional", "Some"),
            "None" => ("Optional", "None"),
            "Ok" => ("Result", "Ok"),
            "Err" => ("Result", "Err"),
            _ => return None,
        };
        // The expected type must be the matching enum with concrete args.
        let (exp_enum_name, exp_generics) = match expected? {
            Type::Named { path, generics } => (path.last()?.as_str(), generics.clone()),
            Type::Optional { inner } => ("Optional", vec![*inner.clone()]),
            _ => return None,
        };
        if exp_enum_name != enum_name {
            return None;
        }
        // Special case: Err(code, message) constructing Result<T, Error> directly.
        if name == "Err" && arguments.len() == 2 {
            let arg0_ty = self.check_expr_with_literal_naturals(arguments[0]);
            let arg1_ty = self.check_expr(
                arguments[1],
                Some(&Type::Primitive(ast::PrimitiveType::Str)),
            );
            if !crate::types::is_integer(&arg0_ty) {
                self.error(
                    format!("Err(code, message) expected integer error code, found {arg0_ty}"),
                    arguments[0].span,
                );
                return None;
            }
            if !crate::types::is_string(&arg1_ty) {
                self.error(
                    format!("Err(code, message) expected str message, found {arg1_ty}"),
                    arguments[1].span,
                );
                return None;
            }
            if let Some(e_ty) = exp_generics.get(1) {
                if let Type::Named { path, .. } = e_ty {
                    if path.last().map(|s| s.as_str()) != Some("Error") {
                        self.error(
                            format!(
                                "Err(code, message) produces Error, but expected error type is {e_ty}"
                            ),
                            *span,
                        );
                        return None;
                    }
                }
            }
            let err_ty = Type::Named {
                path: vec!["Error".to_string()],
                generics: Vec::new(),
            };
            let generics = if !exp_generics.is_empty() {
                vec![exp_generics[0].clone(), err_ty]
            } else {
                vec![Type::Unit, err_ty]
            };
            self.bare_constructors.insert(
                (span.file, span.start, span.end),
                BareConstructorRewrite {
                    enum_name: enum_name.to_string(),
                    variant: variant.to_string(),
                    generics: generics.iter().map(|g| g.to_ast()).collect(),
                },
            );
            return Some(Type::Named {
                path: vec![enum_name.to_string()],
                generics,
            });
        }
        let enum_def = self.enum_defs.get(enum_name)?;
        let variant_info = enum_def.variants.get(variant)?;
        let enum_type_params = enum_def.type_params.clone();
        let payload_types: Vec<Type> = variant_info.payload.iter().map(Type::from_ast).collect();
        if arguments.len() != payload_types.len() {
            self.error(
                msg::variant_arg_count_mismatch(variant, payload_types.len(), arguments.len()),
                *span,
            );
            return None;
        }
        for (i, arg) in arguments.iter().enumerate() {
            self.check_expr(arg, Some(&payload_types[i]));
        }
        // Infer generic args: prefer the expected type's concrete args;
        // fall back to inferring from the argument against the payload type.
        let generics: Vec<Type> =
            if exp_generics.len() == enum_type_params.len() && !exp_generics.is_empty() {
                exp_generics.clone()
            } else {
                let mut mapping = HashMap::default();
                for (i, arg) in arguments.iter().enumerate() {
                    if let Some(pt) = payload_types.get(i) {
                        let arg_ty = self.check_expr_with_literal_naturals(arg);
                        if !self.infer_type_params(pt, &arg_ty, &enum_type_params, &mut mapping) {
                            return None;
                        }
                    }
                }
                enum_type_params
                    .iter()
                    .map(|param| {
                        mapping.get(param).cloned().unwrap_or_else(|| Type::Named {
                            path: vec![param.clone()],
                            generics: Vec::new(),
                        })
                    })
                    .collect()
            };
        self.bare_constructors.insert(
            (span.file, span.start, span.end),
            BareConstructorRewrite {
                enum_name: enum_name.to_string(),
                variant: variant.to_string(),
                generics: generics.iter().map(|g| g.to_ast()).collect(),
            },
        );
        Some(Type::Named {
            path: vec![enum_name.to_string()],
            generics,
        })
    }

    fn substitute_self_type(&self, ty: &Type, receiver: &Type) -> Type {
        match ty {
            Type::Named { path, generics } => {
                if path.len() == 1 && path[0] == "Self" {
                    return receiver.clone();
                }
                Type::Named {
                    path: path.clone(),
                    generics: generics
                        .iter()
                        .map(|inner| self.substitute_self_type(inner, receiver))
                        .collect(),
                }
            }
            Type::Reference { is_mutable, inner } => Type::Reference {
                is_mutable: *is_mutable,
                inner: Box::new(self.substitute_self_type(inner, receiver)),
            },
            Type::Pointer {
                is_mutable,
                is_volatile,
                inner,
            } => Type::Pointer {
                is_mutable: *is_mutable,
                is_volatile: *is_volatile,
                inner: Box::new(self.substitute_self_type(inner, receiver)),
            },
            Type::Slice { element } => Type::Slice {
                element: Box::new(self.substitute_self_type(element, receiver)),
            },
            Type::Optional { inner } => Type::Optional {
                inner: Box::new(self.substitute_self_type(inner, receiver)),
            },
            Type::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .map(|ty| self.substitute_self_type(ty, receiver))
                    .collect(),
            ),
            Type::Function {
                params,
                return_type,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|ty| self.substitute_self_type(ty, receiver))
                    .collect(),
                return_type: Box::new(self.substitute_self_type(return_type, receiver)),
            },
            _ => ty.clone(),
        }
    }

    /// Replace generic type parameters (e.g. `T` in `enum Box2<T>`) with
    /// their concrete arguments from a type-param → type mapping. Used to bind
    /// payload patterns with the scrutinee's concrete types (`Box2<i32>` binds
    /// `val : i32`, not `val : T`).
    fn substitute_type_params(&self, ty: &Type, mapping: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Named { path, generics } => {
                // A bare single-segment name that is a known type param (e.g.
                // `T`) resolves to its concrete argument.
                if path.len() == 1
                    && generics.is_empty()
                    && let Some(replacement) = mapping.get(&path[0])
                {
                    return replacement.clone();
                }
                Type::Named {
                    path: path.clone(),
                    generics: generics
                        .iter()
                        .map(|inner| self.substitute_type_params(inner, mapping))
                        .collect(),
                }
            }
            Type::Reference { is_mutable, inner } => Type::Reference {
                is_mutable: *is_mutable,
                inner: Box::new(self.substitute_type_params(inner, mapping)),
            },
            Type::Pointer {
                is_mutable,
                is_volatile,
                inner,
            } => Type::Pointer {
                is_mutable: *is_mutable,
                is_volatile: *is_volatile,
                inner: Box::new(self.substitute_type_params(inner, mapping)),
            },
            Type::Slice { element } => Type::Slice {
                element: Box::new(self.substitute_type_params(element, mapping)),
            },
            Type::Optional { inner } => Type::Optional {
                inner: Box::new(self.substitute_type_params(inner, mapping)),
            },
            Type::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .map(|ty| self.substitute_type_params(ty, mapping))
                    .collect(),
            ),
            Type::Function {
                params,
                return_type,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|ty| self.substitute_type_params(ty, mapping))
                    .collect(),
                return_type: Box::new(self.substitute_type_params(return_type, mapping)),
            },
            _ => ty.clone(),
        }
    }

    fn is_incdec_type(&self, ty: &Type) -> bool {
        is_numeric(ty) || matches!(ty, Type::Pointer { .. })
    }

    fn is_primitive_type(&self, ty: &Type) -> bool {
        self.enum_backing_type(ty).is_some()
    }

    fn is_void_like(ty: &Type) -> bool {
        matches!(ty, Type::Unit) || is_void(ty)
    }

    fn void_compatible(expected: &Type, found: &Type) -> bool {
        Self::is_void_like(expected) && Self::is_void_like(found)
    }

    fn reject_plain_void_value_type(&mut self, ty: &Type, span: Span) {
        if is_void(ty) {
            self.error(
                "plain `void` is only valid as a function return type; use `void*` for opaque data",
                span,
            );
        }
    }

    /// WebAssembly has no 80-bit float type: reject `f80`/`c80` anywhere they
    /// enter through source-level types so `agc check` reports a
    /// target-specific message instead of an LLVM lowering failure.
    fn reject_wasm_extended_float(&mut self, ty: &Type, span: Span) {
        if crate::codegen::abi::target_is_wasm(Some(&self.target_triple))
            && ty.contains_extended_float()
        {
            self.error(
                "`f80`/`c80` are not supported on WebAssembly targets: wasm has no 80-bit float type (use `f64`/`c64`)",
                span,
            );
        }
    }

    fn numeric_rank(p: &ast::PrimitiveType) -> u32 {
        match p {
            ast::PrimitiveType::Bool => 1,
            ast::PrimitiveType::I8 => 10,
            ast::PrimitiveType::U8 => 11,
            ast::PrimitiveType::I16 => 20,
            ast::PrimitiveType::U16 => 21,
            ast::PrimitiveType::Char => 29,
            ast::PrimitiveType::I32 => 30,
            ast::PrimitiveType::U32 => 31,
            ast::PrimitiveType::I64 => 40,
            ast::PrimitiveType::U64 => 41,
            ast::PrimitiveType::I128 => 50,
            ast::PrimitiveType::U128 => 51,
            ast::PrimitiveType::F32 => 60,
            ast::PrimitiveType::F64 => 70,
            ast::PrimitiveType::F80 => 80,
            ast::PrimitiveType::C32 => 90,
            ast::PrimitiveType::C64 => 100,
            ast::PrimitiveType::C80 => 110,
            _ => 0,
        }
    }

    fn common_numeric_type(&self, left: &Type, right: &Type) -> Option<Type> {
        let left_prim = self.numeric_type(left)?;
        let right_prim = self.numeric_type(right)?;
        if left_prim == right_prim {
            return Some(Type::Primitive(left_prim));
        }
        if Self::numeric_rank(&left_prim) >= Self::numeric_rank(&right_prim) {
            Some(Type::Primitive(left_prim))
        } else {
            Some(Type::Primitive(right_prim))
        }
    }

    fn resolve_operator_overload(
        &mut self,
        left: &Type,
        right: &Type,
        operator: &ast::BinaryOperator,
        expr: &ast::Expression,
    ) -> Option<Type> {
        if self.is_primitive_type(left) {
            return None;
        }

        let name = operator_method_name(operator)?;

        let result = self.resolve_method_overload_types(
            left,
            name,
            std::slice::from_ref(right),
            None,
            MethodCallStyle::Instance,
            None,
            expr.span,
        );
        if result.is_none() {
            if self.defer_operator_if_generic(left, right, operator, expr.span) {
                // Recorded as an implicit guard; enforced at instantiation.
                return None;
            }
            self.error(
                format!("missing operator overload '{}' for {:?}", name, left),
                expr.span,
            );
        }
        result
    }

    /// True for a bare type-parameter reference (`T`, not `Vec<T>` and not a
    /// known concrete type) — the operands whose operators become implicit
    /// guards on generic functions.
    fn is_bare_type_param(&self, ty: &Type) -> bool {
        match ty {
            Type::Named { path, generics } => {
                path.len() == 1 && generics.is_empty() && !self.known_types.contains_key(&path[0])
            }
            Type::Pointer { inner, .. } | Type::Reference { inner, .. } => {
                self.is_bare_type_param(inner)
            }
            _ => false,
        }
    }

    fn is_generic_operand(&self, left: &Type, right: &Type) -> bool {
        self.is_bare_type_param(left) || self.is_bare_type_param(right)
    }

    /// Record an operator-on-type-param requirement during generic body
    /// checking. Returns true when the operands are bare type params (the
    /// check is deferred to instantiation); false for concrete operands.
    fn defer_operator_if_generic(
        &mut self,
        left: &Type,
        right: &Type,
        op: &ast::BinaryOperator,
        origin_span: Span,
    ) -> bool {
        if !self.is_generic_operand(left, right) {
            return false;
        }
        self.current_implicit_reqs.push(ImplicitReq {
            left: left.clone(),
            right: right.clone(),
            op: op.clone(),
            origin_span,
        });
        true
    }
    fn store_implicit_method_reqs(&mut self, key: String) {
        let reqs = std::mem::take(&mut self.current_implicit_method_reqs);
        if !reqs.is_empty() {
            self.implicit_method_reqs.insert(key, reqs);
        }
    }

    fn store_implicit_reqs(&mut self, key: String) {
        let reqs = std::mem::take(&mut self.current_implicit_reqs);
        if !reqs.is_empty() {
            self.implicit_reqs.insert(key, reqs);
        }
    }

    fn params_key(params: &[ast::Parameter]) -> String {
        params
            .iter()
            .map(|p| Type::from_ast(&p.param_type).canonical_key())
            .collect::<Vec<_>>()
            .join(",")
    }

    fn free_fn_key(func: &ast::FunctionItem) -> String {
        format!(
            "fn:{}:{}",
            func.name.name,
            Self::params_key(&func.parameters)
        )
    }

    fn impl_method_key(self_ty: &Type, func: &ast::ImplFunction) -> String {
        format!(
            "m:{}:{}:{}",
            self_ty.canonical_key(),
            func.name.name,
            Self::params_key(&func.parameters)
        )
    }

    fn binary_operator_symbol(op: &ast::BinaryOperator) -> &'static str {
        use ast::BinaryOperator::*;
        match op {
            Add | AddAssign => "+",
            Subtract | SubtractAssign => "-",
            Multiply | MultiplyAssign => "*",
            Divide | DivideAssign => "/",
            Modulo | ModuloAssign => "%",
            Equal => "==",
            NotEqual => "!=",
            Less => "<",
            Greater => ">",
            LessEqual => "<=",
            GreaterEqual => ">=",
            BitwiseAnd | BitwiseAndAssign => "&",
            BitwiseOr | BitwiseOrAssign => "|",
            BitwiseXor | BitwiseXorAssign => "^",
            LeftShift | LeftShiftAssign => "<<",
            RightShift | RightShiftAssign => ">>",
            LogicalAnd => "&&",
            LogicalOr => "||",
            Assign => "=",
            Range => "..",
        }
    }

    /// Enforce recorded implicit guards for one concrete instantiation. The
    /// substituted operand types must support the operator (builtin or an
    /// `__<op>` overload), otherwise the call site errors.
    fn check_implicit_guards(&mut self, key: &str, mapping: &HashMap<String, Type>, span: Span) {
        let Some(reqs) = self.implicit_reqs.get(key).cloned() else {
            return;
        };
        for req in reqs {
            let left = req.left.substitute(mapping);
            let right = req.right.substitute(mapping);
            if !self.is_concrete_type(&left) || !self.is_concrete_type(&right) {
                continue;
            }
            if self
                .operator_is_supported(&left, &right, &req.op, span)
                .is_none()
            {
                self.error(
                    msg::implicit_guard_missing(
                        Self::binary_operator_symbol(&req.op),
                        &left,
                        &right,
                    ),
                    span,
                );
                self.error(
                    msg::implicit_guard_origin(Self::binary_operator_symbol(&req.op)),
                    req.origin_span,
                );
            }
        }
    }
    fn check_implicit_method_guards(
        &mut self,
        key: &str,
        mapping: &HashMap<String, Type>,
        span: Span,
    ) {
        let Some(reqs) = self.implicit_method_reqs.get(key).cloned() else {
            return;
        };
        for req in reqs {
            let receiver = req.receiver.substitute(mapping);
            let args = req
                .args
                .iter()
                .map(|arg| arg.substitute(mapping))
                .collect::<Vec<_>>();
            if !self.is_concrete_type(&receiver)
                || args.iter().any(|arg| !self.is_concrete_type(arg))
            {
                continue;
            }
            if self
                .resolve_method_overload_types(
                    &receiver,
                    &req.name,
                    &args,
                    None,
                    req.style,
                    None,
                    span,
                )
                .is_none()
            {
                let arg_strings = args.iter().map(ToString::to_string).collect::<Vec<_>>();
                self.error(
                    msg::implicit_method_guard_missing(&req.name, &receiver, &arg_strings),
                    span,
                );
                self.error(
                    msg::implicit_method_guard_origin(&req.name),
                    req.origin_span,
                );
            }
        }
    }

    /// Whether a binary operator is supported on these (concrete) operand
    /// types: a builtin numeric/pointer/string path or a `__<op>` overload.
    /// Mirrors the body-check logic in the binary-expression arms.
    fn operator_is_supported(
        &mut self,
        left: &Type,
        right: &Type,
        op: &ast::BinaryOperator,
        span: Span,
    ) -> Option<Type> {
        use ast::BinaryOperator::*;
        let bool_ty = Type::Primitive(ast::PrimitiveType::Bool);
        match op {
            Add | Subtract | Multiply | Divide | Modulo => {
                if let Some(ptr_ty) = self.pointer_arith_result(left, right, op) {
                    return Some(ptr_ty);
                }
                if self.is_numeric_type(left) && self.is_numeric_type(right) {
                    return self.common_numeric_type(left, right);
                }
                self.operator_overload_probe(left, right, op, span)
            }
            Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual => {
                let char_ty = Type::Primitive(ast::PrimitiveType::Char);
                let is_byte = |ty: &Type| {
                    matches!(
                        ty,
                        Type::Primitive(ast::PrimitiveType::U8 | ast::PrimitiveType::U16)
                    )
                };
                if (left == &char_ty && (right == &char_ty || is_byte(right)))
                    || (right == &char_ty && is_byte(left))
                {
                    return Some(bool_ty);
                }
                if self.is_numeric_type(left) && self.is_numeric_type(right) {
                    return self.common_numeric_type(left, right).map(|_| bool_ty);
                }
                if left == right && matches!(left, Type::Pointer { .. } | Type::Reference { .. }) {
                    return Some(bool_ty);
                }
                // str equality/inequality lowers to strcmp.
                if matches!(op, Equal | NotEqual) && is_string(left) && is_string(right) {
                    return Some(bool_ty);
                }
                self.operator_overload_probe(left, right, op, span)
            }
            BitwiseAnd | BitwiseOr | BitwiseXor | LeftShift | RightShift => {
                if self.is_integer_type(left) && self.is_integer_type(right) {
                    return Some(left.clone());
                }
                self.operator_overload_probe(left, right, op, span)
            }
            _ => None,
        }
    }

    /// Probe for a `__<op>` overload without emitting errors.
    fn operator_overload_probe(
        &mut self,
        left: &Type,
        right: &Type,
        op: &ast::BinaryOperator,
        span: Span,
    ) -> Option<Type> {
        if self.is_primitive_type(left) {
            return None;
        }
        let name = operator_method_name(op)?;
        self.resolve_method_overload_types(
            left,
            name,
            std::slice::from_ref(right),
            None,
            MethodCallStyle::Instance,
            None,
            span,
        )
    }

    fn method_call_style(&self, receiver: &ast::Expression) -> MethodCallStyle {
        match receiver.kind.as_ref() {
            ast::ExpressionKind::TypeName(_) => MethodCallStyle::Static,
            ast::ExpressionKind::Identifier(ident) => {
                if self.lookup_type(&ident.name).is_some() {
                    MethodCallStyle::Instance
                } else if self.known_types.contains_key(&ident.name)
                    || self.current_type_params.contains(&ident.name)
                {
                    MethodCallStyle::Static
                } else {
                    MethodCallStyle::Instance
                }
            }
            _ => MethodCallStyle::Instance,
        }
    }

    fn collect_impl_methods(&mut self, program: &ast::Program, table: &mut CompilerSymbolTable) {
        let mut all_impls: Vec<ast::ImplItem> = Vec::new();
        for item in &program.items {
            if let ast::ItemKind::Impl(impl_item) = &item.kind {
                all_impls.push(impl_item.clone());
            }
        }
        let local_impl_count = all_impls.len();
        for module in &self.imported_modules {
            for template_src in &module.generic_templates {
                let file_id = crate::lexer::register_source(&module.source_path, template_src);
                if let Ok(tokens) = crate::lexer::lex_with_source(template_src, file_id) {
                    let mut parser =
                        crate::parser::Parser::new_with_source(tokens, module.source_path.clone());
                    let (prog, _) = parser.parse_program();
                    for item in prog.items {
                        if let ast::ItemKind::Impl(mut impl_item) = item.kind {
                            // Templates are parsed without the module's concrete declarations.
                            impl_item.implicit_type_params.retain(|name| {
                                !module.exports.iter().any(|export| {
                                    matches!(
                                        export.kind,
                                        crate::module_artifact::ExportKind::Struct
                                            | crate::module_artifact::ExportKind::Enum
                                            | crate::module_artifact::ExportKind::TypeAlias
                                    )
                                        && export.name == *name
                                })
                            });
                            all_impls.push(impl_item);
                        }
                    }
                }
            }
        }

        // Imported templates carry complete trait arguments and conditional bounds.
        for impl_item in all_impls.iter().skip(local_impl_count) {
            self.collect_trait_impl(impl_item);
        }

        for (index, impl_item) in all_impls.iter().enumerate() {
            let self_ty = Type::from_ast(&impl_item.self_type);
            let self_key = self.method_key(&self_ty);

            // One Arc per impl block, shared by every method's MethodSig:
            // registering N methods clones the impl body once, not N times.
            let shared_impl = std::sync::Arc::new(impl_item.clone());

            let mut impl_type_params = Self::impl_type_parameters(impl_item);
            if index < local_impl_count
                && impl_type_params.is_empty()
                && impl_item.generics.is_none()
            {
                // Preserve parameter order from the AST (HashSet loses order,
                // causing non-deterministic generic argument swapping).
                let mut implicit = Vec::new();
                self.collect_implicit_type_params_ordered(&impl_item.self_type, &mut implicit);
                impl_type_params.extend(implicit);
            }

            let bounds = self.collect_bounds(impl_item.generics.as_ref());

            for impl_member in &impl_item.items {
                match impl_member {
                    ast::ImplItemKind::Function(func) => {
                        if index >= local_impl_count
                            && matches!(func.visibility, ast::Visibility::Private)
                        {
                            continue;
                        }
                        let mut type_params = impl_type_params.clone();
                        if let Some(generics) = &func.generics {
                            for param in &generics.params {
                                if let ast::GenericParam::Type(type_param) = param {
                                    type_params.push(type_param.name.name.clone());
                                }
                            }
                        }
                        type_params = self.dedup_type_params(type_params);

                        let mut func_bounds = bounds.clone();
                        func_bounds.extend(self.collect_bounds(func.generics.as_ref()));

                        let params = func
                            .parameters
                            .iter()
                            .map(|param| Type::from_ast(&param.param_type))
                            .collect::<Vec<_>>();
                        let return_type = func
                            .return_type
                            .as_ref()
                            .map(Type::from_ast)
                            .unwrap_or(Type::Unit);
                        if index >= local_impl_count
                            && type_params.is_empty()
                            && self
                                .imported_functions
                                .get(&format!("{}::{}", self_ty.canonical_key(), func.name.name))
                                .is_some_and(|signatures| {
                                    signatures.iter().any(|signature| {
                                        signature.type_params.is_empty()
                                            && signature.params == params
                                            && signature.return_type == return_type
                                            && signature.is_variadic == func.is_variadic
                                    })
                                })
                        {
                            // Use a linked signature only when this method was actually exported.
                            continue;
                        }
                        let symbol_key = self.method_symbol_key(
                            &self_ty,
                            &func.name.name,
                            &params,
                            &return_type,
                        );
                        let symbol_id = table.intern_symbol(
                            symbol_key.clone(),
                            SymbolKind::ImplMethod,
                            Some(func.name.span),
                            CompilerPhase::TypeCheck,
                        );
                        debug_assert_eq!(table.symbol_id(&symbol_key), Some(symbol_id));
                        debug_assert_eq!(table.symbol_key(symbol_id), Some(symbol_key.as_str()));
                        self.method_symbols.insert(
                            symbol_id,
                            MethodSig {
                                params,
                                return_type,
                                type_params,
                                owner: self_ty.clone(),
                                bounds: func_bounds,
                                source_impl: shared_impl.clone(),
                                source_method: std::sync::Arc::new((**func).clone()),
                            },
                        );
                        self.methods
                            .entry((self_key.clone(), func.name.name.clone()))
                            .or_default()
                            .push(symbol_id);
                    }
                    ast::ImplItemKind::Cast(cast) => {
                        let from_ty = Type::from_ast(&impl_item.self_type);
                        let to_ty = Type::from_ast(&cast.target_type);
                        let from_key = self.method_key(&from_ty);
                        let to_key = self.method_key(&to_ty);
                        self.casts.insert((from_key, to_key), ());
                    }
                    _ => {}
                }
            }
        }
    }

    /// Collects implicit type parameters from a type AST, preserving insertion order.
    /// Uses Vec + contains() for dedup instead of HashSet, so that parameter order
    /// matches the order they appear in the source.
    fn collect_implicit_type_params_ordered(&self, ty: &ast::Type, params: &mut Vec<String>) {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1 {
                    let name = &named.path[0].name;
                    if !self.known_types.contains_key(name) && !params.contains(name) {
                        params.push(name.clone());
                    }
                }
                if let Some(generics) = &named.generics {
                    for arg in generics {
                        self.collect_implicit_type_params_ordered(arg, params);
                    }
                }
            }
            ast::TypeKind::Generic(generic) => {
                if !self.known_types.contains_key(&generic.name.name)
                    && !params.contains(&generic.name.name)
                {
                    params.push(generic.name.name.clone());
                }
                for arg in &generic.args {
                    self.collect_implicit_type_params_ordered(arg, params);
                }
            }
            ast::TypeKind::Reference(reference) => {
                self.collect_implicit_type_params_ordered(&reference.inner, params)
            }
            ast::TypeKind::Pointer(pointer) => {
                self.collect_implicit_type_params_ordered(&pointer.inner, params)
            }
            ast::TypeKind::Slice(slice) => {
                self.collect_implicit_type_params_ordered(&slice.element_type, params)
            }
            ast::TypeKind::Array(array) => {
                self.collect_implicit_type_params_ordered(&array.element_type, params)
            }
            ast::TypeKind::Optional(inner) => {
                self.collect_implicit_type_params_ordered(inner, params)
            }
            ast::TypeKind::Tuple(items) => {
                for item in items {
                    self.collect_implicit_type_params_ordered(item, params);
                }
            }
            ast::TypeKind::Function(func) => {
                for param in &func.parameters {
                    self.collect_implicit_type_params_ordered(param, params);
                }
                self.collect_implicit_type_params_ordered(&func.return_type, params)
            }
            ast::TypeKind::Primitive(_) => {}
        }
    }

    fn function_symbol_key(&self, func: &ast::FunctionItem, is_variadic: bool) -> String {
        let params = func
            .parameters
            .iter()
            .map(|param| Type::from_ast(&param.param_type).canonical_key())
            .collect::<Vec<_>>()
            .join(",");
        let return_type = func
            .return_type
            .as_ref()
            .map(Type::from_ast)
            .unwrap_or(Type::Unit)
            .canonical_key();
        format!(
            "fn::{}({params})->{return_type}{}",
            func.name.name,
            if is_variadic { "::variadic" } else { "" }
        )
    }

    fn method_symbol_key(
        &self,
        owner: &Type,
        method: &str,
        params: &[Type],
        return_type: &Type,
    ) -> String {
        let params = params
            .iter()
            .map(Type::canonical_key)
            .collect::<Vec<_>>()
            .join(",");
        format!(
            "method::{}::{method}({params})->{}",
            owner.canonical_key(),
            return_type.canonical_key()
        )
    }

    fn method_key(&self, ty: &Type) -> String {
        match ty {
            Type::Reference { inner, .. } | Type::Pointer { inner, .. } => self.method_key(inner),
            Type::Named { path, .. } => path.join("::"),
            Type::Slice { .. } => "Slice".to_string(),
            _ => ty.canonical_key(),
        }
    }

    fn resolve_field_access_type(&self, object_ty: &Type, field_name: &str) -> Option<Type> {
        let mut current = object_ty;
        while let Type::Reference { inner, .. } | Type::Pointer { inner, .. } = current {
            current = inner.as_ref();
        }

        // Builtin Slice<T> has pseudo-fields `data: T*` and `len: i64`.
        if let Type::Slice { element } = current {
            match field_name {
                "data" => {
                    return Some(Type::Pointer {
                        is_mutable: true,
                        is_volatile: false,
                        inner: Box::new((**element).clone()),
                    });
                }
                "len" => {
                    return Some(Type::Primitive(crate::parser::ast::PrimitiveType::I64));
                }
                _ => return None,
            }
        }

        // Builtin str has pseudo-fields `data: u8*` and `len: i64`.
        if let Type::Primitive(crate::parser::ast::PrimitiveType::Str) = current {
            match field_name {
                "data" => {
                    return Some(Type::Pointer {
                        is_mutable: false,
                        is_volatile: false,
                        inner: Box::new(Type::Primitive(crate::parser::ast::PrimitiveType::U8)),
                    });
                }
                "len" => {
                    return Some(Type::Primitive(crate::parser::ast::PrimitiveType::I64));
                }
                _ => return None,
            }
        }

        let Type::Named { path, generics } = current else {
            return None;
        };

        let struct_name = path.last()?;
        if let Some(enum_def) = self.enum_defs.get(struct_name) {
            if enum_def.variants.contains_key(field_name) {
                return Some(current.clone());
            }
            return None;
        }
        let struct_def = self.struct_defs.get(struct_name)?;
        let field_ty = struct_def.fields.get(field_name)?;

        if struct_def.type_params.is_empty() || generics.is_empty() {
            return Some(field_ty.clone());
        }

        let mut mapping = HashMap::default();
        for (param, arg) in struct_def.type_params.iter().zip(generics.iter()) {
            mapping.insert(param.clone(), arg.clone());
        }
        Some(field_ty.substitute(&mapping))
    }

    fn identifier_suggestion(&self, name: &str) -> String {
        let mut candidates: Vec<&str> = Vec::new();
        for scope in self.scopes.iter().rev() {
            candidates.extend(scope.keys().map(|s| s.as_str()));
        }
        candidates.extend(self.global_variables.keys().map(|s| s.as_str()));
        candidates.extend(self.extern_variables.keys().map(|s| s.as_str()));
        candidates.extend(self.functions.keys().map(|s| s.as_str()));
        candidates.extend(self.imported_functions.keys().map(|s| s.as_str()));
        candidates.extend(self.known_types.keys().map(|s| s.as_str()));
        crate::diagnostics::suggestion_suffix(name, candidates)
    }

    fn function_suggestion(&self, name: &str) -> String {
        let mut candidates: Vec<&str> = Vec::new();
        candidates.extend(self.functions.keys().map(|s| s.as_str()));
        candidates.extend(self.imported_functions.keys().map(|s| s.as_str()));
        for scope in self.scopes.iter().rev() {
            candidates.extend(scope.keys().map(|s| s.as_str()));
        }
        crate::diagnostics::suggestion_suffix(name, candidates)
    }

    fn field_suggestion(&self, object_ty: &Type, field_name: &str) -> String {
        let mut current = object_ty;
        while let Type::Reference { inner, .. } | Type::Pointer { inner, .. } = current {
            current = inner.as_ref();
        }
        let Type::Named { path, .. } = current else {
            return String::new();
        };
        let Some(type_name) = path.last() else {
            return String::new();
        };
        if let Some(enum_def) = self.enum_defs.get(type_name) {
            return crate::diagnostics::suggestion_suffix(field_name, enum_def.variants.keys());
        }
        if let Some(struct_def) = self.struct_defs.get(type_name) {
            return crate::diagnostics::suggestion_suffix(field_name, struct_def.fields.keys());
        }
        String::new()
    }

    fn type_suggestion(&self, name: &str) -> String {
        let mut candidates: Vec<&str> = Vec::new();
        candidates.extend(self.known_types.keys().map(|s| s.as_str()));
        candidates.extend(self.struct_defs.keys().map(|s| s.as_str()));
        candidates.extend(self.enum_defs.keys().map(|s| s.as_str()));
        candidates.extend(self.type_aliases.keys().map(|s| s.as_str()));
        candidates.extend([
            "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64",
            "bool", "str", "char", "void",
        ]);
        crate::diagnostics::suggestion_suffix(name, candidates)
    }

    fn collect_struct_layouts(&mut self, program: &ast::Program) {
        for item in &program.items {
            let ast::ItemKind::Struct(struct_item) = &item.kind else {
                continue;
            };

            let attrs = match parse_struct_attributes(&item.attributes) {
                Ok(attrs) => attrs,
                Err(err) => {
                    self.error(err.message, err.span);
                    continue;
                }
            };

            let field_types = struct_item
                .fields
                .iter()
                .map(|field| Type::from_ast(&field.field_type))
                .collect::<Vec<_>>();
            let layout = struct_layout(&self.type_ctx, &field_types, &attrs);
            let path = vec![struct_item.name.name.clone()];
            self.type_ctx.register_named(&path, layout);
        }
    }

    pub fn resolve_type_aliases_in_program(program: &mut ast::Program) {
        let aliases = Self::local_type_aliases(program);
        Self::resolve_type_aliases_in_program_with_aliases(program, &aliases);
    }

    pub fn resolve_type_aliases_in_program_with_imports(
        program: &mut ast::Program,
        modules: &[ModuleArtifact],
    ) {
        let mut aliases = Self::local_type_aliases(program);
        for module in modules {
            for export in &module.exports {
                if export.kind != crate::module_artifact::ExportKind::TypeAlias {
                    continue;
                }
                let Some(type_key) = &export.type_key else {
                    continue;
                };
                let Ok(type_def) = ast_type_from_canonical_key(type_key) else {
                    continue;
                };
                aliases.entry(export.name.clone()).or_insert(type_def);
            }
        }
        Self::resolve_type_aliases_in_program_with_aliases(program, &aliases);
    }

    fn local_type_aliases(program: &ast::Program) -> HashMap<String, ast::Type> {
        program
            .items
            .iter()
            .filter_map(|item| {
                if let ast::ItemKind::TypeAlias(alias) = &item.kind {
                    Some((alias.name.name.clone(), alias.type_def.clone()))
                } else {
                    None
                }
            })
            .collect()
    }

    fn resolve_type_aliases_in_program_with_aliases(
        program: &mut ast::Program,
        aliases: &HashMap<String, ast::Type>,
    ) {
        if aliases.is_empty() {
            return;
        }

        for item in &mut program.items {
            Self::resolve_type_aliases_in_item(item, &aliases);
        }
    }

    fn expand_imported_type_aliases(ty: Type, aliases: &HashMap<String, ast::Type>) -> Type {
        fn expand(
            ty: Type,
            aliases: &HashMap<String, ast::Type>,
            visiting: &mut HashSet<String>,
        ) -> Type {
            match ty {
                Type::Named { path, generics } => {
                    let generics = generics
                        .into_iter()
                        .map(|generic| expand(generic, aliases, visiting))
                        .collect::<Vec<_>>();
                    if path.len() == 1
                        && generics.is_empty()
                        && let Some(alias) = aliases.get(&path[0])
                        && visiting.insert(path[0].clone())
                    {
                        let expanded = expand(Type::from_ast(alias), aliases, visiting);
                        visiting.remove(&path[0]);
                        return expanded;
                    }
                    Type::Named { path, generics }
                }
                Type::Reference { is_mutable, inner } => Type::Reference {
                    is_mutable,
                    inner: Box::new(expand(*inner, aliases, visiting)),
                },
                Type::Pointer {
                    is_mutable,
                    is_volatile,
                    inner,
                } => Type::Pointer {
                    is_mutable,
                    is_volatile,
                    inner: Box::new(expand(*inner, aliases, visiting)),
                },
                Type::Slice { element } => Type::Slice {
                    element: Box::new(expand(*element, aliases, visiting)),
                },
                Type::Task(inner) => Type::Task(Box::new(expand(*inner, aliases, visiting))),
                Type::Array { element, size } => Type::Array {
                    element: Box::new(expand(*element, aliases, visiting)),
                    size,
                },
                Type::Optional { inner } => Type::Optional {
                    inner: Box::new(expand(*inner, aliases, visiting)),
                },
                Type::Tuple(items) => Type::Tuple(
                    items
                        .into_iter()
                        .map(|item| expand(item, aliases, visiting))
                        .collect(),
                ),
                Type::Function {
                    params,
                    return_type,
                } => Type::Function {
                    params: params
                        .into_iter()
                        .map(|param| expand(param, aliases, visiting))
                        .collect(),
                    return_type: Box::new(expand(*return_type, aliases, visiting)),
                },
                other => other,
            }
        }

        expand(ty, aliases, &mut HashSet::default())
    }

    fn resolve_type_aliases_in_item(item: &mut ast::Item, aliases: &HashMap<String, ast::Type>) {
        match &mut item.kind {
            ast::ItemKind::Struct(s) => {
                for field in &mut s.fields {
                    Self::resolve_type_aliases_in_type(&mut field.field_type, aliases);
                }
            }
            ast::ItemKind::Enum(e) => {
                for variant in &mut e.variants {
                    match &mut variant.data {
                        ast::EnumVariantData::Tuple(types) => {
                            for ty in types {
                                Self::resolve_type_aliases_in_type(ty, aliases);
                            }
                        }
                        ast::EnumVariantData::Struct(fields) => {
                            for field in fields {
                                Self::resolve_type_aliases_in_type(&mut field.field_type, aliases);
                            }
                        }
                        ast::EnumVariantData::Unit => {}
                    }
                }
            }
            ast::ItemKind::Function(f) => {
                for param in &mut f.parameters {
                    Self::resolve_type_aliases_in_type(&mut param.param_type, aliases);
                }
                if let Some(return_type) = &mut f.return_type {
                    Self::resolve_type_aliases_in_type(return_type, aliases);
                }
                Self::resolve_type_aliases_in_block(&mut f.body, aliases);
            }
            ast::ItemKind::Impl(impl_item) => {
                Self::resolve_type_aliases_in_type(&mut impl_item.self_type, aliases);
                for member in &mut impl_item.items {
                    match member {
                        ast::ImplItemKind::Function(func) => {
                            for param in &mut func.parameters {
                                Self::resolve_type_aliases_in_type(&mut param.param_type, aliases);
                            }
                            if let Some(return_type) = &mut func.return_type {
                                Self::resolve_type_aliases_in_type(return_type, aliases);
                            }
                        }
                        ast::ImplItemKind::AssociatedType(assoc) => {
                            Self::resolve_type_aliases_in_type(&mut assoc.type_def, aliases);
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            Self::resolve_type_aliases_in_type(&mut cast.target_type, aliases);
                            for param in &mut cast.parameters {
                                Self::resolve_type_aliases_in_type(&mut param.param_type, aliases);
                            }
                        }
                    }
                }
            }
            ast::ItemKind::Trait(trait_item) => {
                for member in &mut trait_item.items {
                    match member {
                        ast::TraitItemKind::Function(func) => {
                            for param in &mut func.parameters {
                                Self::resolve_type_aliases_in_type(&mut param.param_type, aliases);
                            }
                            if let Some(return_type) = &mut func.return_type {
                                Self::resolve_type_aliases_in_type(return_type, aliases);
                            }
                        }
                        ast::TraitItemKind::AssociatedType(assoc) => {
                            if let Some(default) = &mut assoc.default {
                                Self::resolve_type_aliases_in_type(default, aliases);
                            }
                        }
                        ast::TraitItemKind::AssociatedFunctionValue(fv) => {
                            for param in &mut fv.fn_type.parameters {
                                Self::resolve_type_aliases_in_type(param, aliases);
                            }
                            Self::resolve_type_aliases_in_type(
                                &mut fv.fn_type.return_type,
                                aliases,
                            );
                        }
                    }
                }
            }
            ast::ItemKind::ExternFunction(f) => {
                for param in &mut f.signature.parameters {
                    Self::resolve_type_aliases_in_type(&mut param.param_type, aliases);
                }
                if let Some(return_type) = &mut f.signature.return_type {
                    Self::resolve_type_aliases_in_type(return_type, aliases);
                }
            }
            ast::ItemKind::ExternVariable(v) => {
                Self::resolve_type_aliases_in_type(&mut v.var_type, aliases);
            }
            ast::ItemKind::GlobalVariable(v) => {
                Self::resolve_type_aliases_in_type(&mut v.var_type, aliases);
            }
            ast::ItemKind::TypeAlias(_) => {}
            _ => {}
        }
    }

    fn resolve_type_aliases_in_type(ty: &mut ast::Type, aliases: &HashMap<String, ast::Type>) {
        Self::resolve_type_aliases_in_type_with_visiting(ty, aliases, &mut HashSet::default());
    }

    fn resolve_type_aliases_in_type_with_visiting(
        ty: &mut ast::Type,
        aliases: &HashMap<String, ast::Type>,
        visiting: &mut HashSet<String>,
    ) {
        if let ast::TypeKind::Named(named) = ty.kind.as_ref()
            && named.path.len() == 1
            && named.generics.is_none()
            && let Some(alias_name) = named.path.first().map(|name| name.name.clone())
            && let Some(aliased) = aliases.get(&alias_name)
        {
            if !visiting.insert(alias_name.clone()) {
                return;
            }
            let mut expanded = aliased.clone();
            Self::resolve_type_aliases_in_type_with_visiting(&mut expanded, aliases, visiting);
            visiting.remove(&alias_name);
            *ty = expanded;
            return;
        }

        match ty.kind.as_mut() {
            ast::TypeKind::Named(named) => {
                if let Some(generics) = &mut named.generics {
                    for g in generics.iter_mut() {
                        Self::resolve_type_aliases_in_type_with_visiting(g, aliases, visiting);
                    }
                }
            }
            ast::TypeKind::Reference(r) => {
                Self::resolve_type_aliases_in_type_with_visiting(&mut r.inner, aliases, visiting)
            }
            ast::TypeKind::Pointer(p) => {
                Self::resolve_type_aliases_in_type_with_visiting(&mut p.inner, aliases, visiting)
            }
            ast::TypeKind::Slice(s) => Self::resolve_type_aliases_in_type_with_visiting(
                &mut s.element_type,
                aliases,
                visiting,
            ),
            ast::TypeKind::Array(array) => Self::resolve_type_aliases_in_type_with_visiting(
                &mut array.element_type,
                aliases,
                visiting,
            ),
            ast::TypeKind::Optional(inner) => {
                Self::resolve_type_aliases_in_type_with_visiting(inner, aliases, visiting)
            }
            ast::TypeKind::Function(f) => {
                for p in &mut f.parameters {
                    Self::resolve_type_aliases_in_type_with_visiting(p, aliases, visiting);
                }
                Self::resolve_type_aliases_in_type_with_visiting(
                    &mut f.return_type,
                    aliases,
                    visiting,
                );
            }
            ast::TypeKind::Tuple(items) => {
                for item in items.iter_mut() {
                    Self::resolve_type_aliases_in_type_with_visiting(item, aliases, visiting);
                }
            }
            _ => {}
        }
    }

    fn resolve_type_aliases_in_block(block: &mut ast::Block, aliases: &HashMap<String, ast::Type>) {
        for stmt in &mut block.statements {
            Self::resolve_type_aliases_in_statement(stmt, aliases);
        }
    }

    fn resolve_type_aliases_in_statement(
        stmt: &mut ast::Statement,
        aliases: &HashMap<String, ast::Type>,
    ) {
        match &mut stmt.kind {
            ast::StatementKind::Let(var) => {
                if let Some(type_ann) = &mut var.type_annotation {
                    Self::resolve_type_aliases_in_type(type_ann, aliases);
                }
                if let Some(initializer) = &mut var.initializer {
                    Self::resolve_type_aliases_in_expression(initializer, aliases);
                }
            }
            ast::StatementKind::Expression(expr) => {
                Self::resolve_type_aliases_in_expression(expr, aliases);
            }
            ast::StatementKind::Return(expr) => {
                if let Some(expr) = expr {
                    Self::resolve_type_aliases_in_expression(expr, aliases);
                }
            }
            ast::StatementKind::Block(block) => {
                Self::resolve_type_aliases_in_block(block, aliases);
            }
            ast::StatementKind::Break(_) | ast::StatementKind::Continue => {}
            ast::StatementKind::Defer(inner) => {
                Self::resolve_type_aliases_in_statement(inner, aliases);
            }
        }
    }

    fn resolve_type_aliases_in_expression(
        expr: &mut ast::Expression,
        aliases: &HashMap<String, ast::Type>,
    ) {
        use ast::ExpressionKind;
        match expr.kind.as_mut() {
            ExpressionKind::Lambda { parameters, body } => {
                for parameter in parameters {
                    if let Some(ty) = &mut parameter.type_annotation { Self::resolve_type_aliases_in_type(ty, aliases); }
                }
                Self::resolve_type_aliases_in_block(body, aliases);
            }
            ExpressionKind::Binary { left, right, .. } => {
                Self::resolve_type_aliases_in_expression(left, aliases);
                Self::resolve_type_aliases_in_expression(right, aliases);
            }
            ExpressionKind::Unary { operand, .. } | ExpressionKind::Postfix { operand, .. } => {
                Self::resolve_type_aliases_in_expression(operand, aliases);
            }
            ExpressionKind::Call {
                function,
                arguments,
            } => {
                Self::resolve_type_aliases_in_expression(function, aliases);
                for arg in arguments.iter_mut() {
                    Self::resolve_type_aliases_in_expression(arg, aliases);
                }
            }
            ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                Self::resolve_type_aliases_in_expression(receiver, aliases);
                for arg in arguments.iter_mut() {
                    Self::resolve_type_aliases_in_expression(arg, aliases);
                }
            }
            ExpressionKind::FieldAccess { object, .. } => {
                Self::resolve_type_aliases_in_expression(object, aliases);
            }
            ExpressionKind::Index { object, index } => {
                Self::resolve_type_aliases_in_expression(object, aliases);
                Self::resolve_type_aliases_in_expression(index, aliases);
            }
            ExpressionKind::Block(block) => {
                Self::resolve_type_aliases_in_block(block, aliases);
            }
            ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                Self::resolve_type_aliases_in_expression(condition, aliases);
                Self::resolve_type_aliases_in_block(then_branch, aliases);
                if let Some(else_branch) = else_branch {
                    Self::resolve_type_aliases_in_block(else_branch, aliases);
                }
            }
            ExpressionKind::While { condition, body } => {
                Self::resolve_type_aliases_in_expression(condition, aliases);
                Self::resolve_type_aliases_in_block(body, aliases);
            }
            ExpressionKind::ForIn {
                iterable,
                body,
                item_type,
                ..
            } => {
                Self::resolve_type_aliases_in_expression(iterable, aliases);
                if let Some(item_type) = item_type {
                    Self::resolve_type_aliases_in_type(item_type, aliases);
                }
                Self::resolve_type_aliases_in_block(body, aliases);
            }
            ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(type_ann) = &mut init.type_annotation {
                    Self::resolve_type_aliases_in_type(type_ann, aliases);
                }
                if let Some(init_expr) = &mut init.initializer {
                    Self::resolve_type_aliases_in_expression(init_expr, aliases);
                }
                Self::resolve_type_aliases_in_expression(condition, aliases);
                Self::resolve_type_aliases_in_expression(increment, aliases);
                Self::resolve_type_aliases_in_block(body, aliases);
            }
            ExpressionKind::Match { expression, arms } => {
                Self::resolve_type_aliases_in_expression(expression, aliases);
                for arm in arms.iter_mut() {
                    if let Some(guard) = &mut arm.guard {
                        Self::resolve_type_aliases_in_expression(guard, aliases);
                    }
                    Self::resolve_type_aliases_in_expression(&mut arm.body, aliases);
                }
            }
            ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                Self::resolve_type_aliases_in_type(target_type, aliases);
                Self::resolve_type_aliases_in_expression(expression, aliases);
            }
            ExpressionKind::Move(inner) => {
                Self::resolve_type_aliases_in_expression(inner, aliases);
            }
            ExpressionKind::Reference { expression, .. } => {
                Self::resolve_type_aliases_in_expression(expression, aliases);
            }
            ExpressionKind::Comptime(inner) => {
                Self::resolve_type_aliases_in_expression(inner, aliases);
            }
            ExpressionKind::TypeName(ty) => {
                Self::resolve_type_aliases_in_type(ty, aliases);
            }
            ExpressionKind::Array(items) | ExpressionKind::Tuple(items) => {
                for item in items.iter_mut() {
                    Self::resolve_type_aliases_in_expression(item, aliases);
                }
            }
            ExpressionKind::MacroCall { args, .. } => {
                for arg in args.iter_mut() {
                    if let ast::MacroArg::Type(ty) = arg {
                        Self::resolve_type_aliases_in_type(ty, aliases);
                    }
                }
            }
            _ => {}
        }
    }

    fn variant_payload(variant_data: &ast::EnumVariantData) -> Vec<ast::Type> {
        match variant_data {
            ast::EnumVariantData::Unit => vec![],
            ast::EnumVariantData::Tuple(types) => types.clone(),
            ast::EnumVariantData::Struct(fields) => {
                fields.iter().map(|f| f.field_type.clone()).collect()
            }
        }
    }

    fn build_enum_def(&mut self, enum_item: &ast::EnumItem) -> Option<EnumDef> {
        let mut variants = HashMap::default();
        let mut next_value = 0i128;
        let mut min_value = 0i128;
        let mut max_value = 0i128;
        let mut saw_any = false;

        for variant in &enum_item.variants {
            let payload = Self::variant_payload(&variant.data);

            let value = variant.discriminant.unwrap_or(next_value);
            if variants
                .insert(variant.name.name.clone(), VariantInfo { payload })
                .is_some()
            {
                self.error(
                    format!(
                        "duplicate enum variant '{}' in '{}'",
                        variant.name.name, enum_item.name.name
                    ),
                    variant.name.span,
                );
                continue;
            }

            next_value = match value.checked_add(1) {
                Some(next) => next,
                None => {
                    self.error(
                        msg::enum_discriminant_overflow(&variant.name.name),
                        variant.span,
                    );
                    value
                }
            };

            if !saw_any {
                min_value = value;
                max_value = value;
                saw_any = true;
            } else {
                min_value = min_value.min(value);
                max_value = max_value.max(value);
            }
        }

        Some(EnumDef {
            backing_type: choose_enum_backing_type(min_value, max_value),
            variants,
            type_params: enum_item
                .generics
                .as_ref()
                .map(|generics| {
                    generics
                        .params
                        .iter()
                        .filter_map(|param| {
                            if let ast::GenericParam::Type(type_param) = param {
                                Some(type_param.name.name.clone())
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default(),
        })
    }

    fn register_enum_layout(&mut self, name: &str, backing_type: &ast::PrimitiveType) {
        let bytes = match backing_type {
            ast::PrimitiveType::I8 | ast::PrimitiveType::U8 | ast::PrimitiveType::Bool => 1,
            ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => 2,
            ast::PrimitiveType::I32 | ast::PrimitiveType::U32 | ast::PrimitiveType::Char => 4,
            ast::PrimitiveType::I64 | ast::PrimitiveType::U64 => 8,
            ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => 16,
            _ => return,
        };
        self.type_ctx
            .register_named(&[name.to_string()], TypeLayout::known(bytes, bytes));
    }

    fn enum_backing_type(&self, ty: &Type) -> Option<ast::PrimitiveType> {
        match ty {
            Type::Primitive(primitive) => Some(primitive.clone()),
            Type::Named { path, .. } if path.len() == 1 => self
                .enum_defs
                .get(&path[0])
                .filter(|enum_def| enum_def.variants.values().all(|v| v.payload.is_empty()))
                .map(|enum_def| enum_def.backing_type.clone()),
            _ => None,
        }
    }

    fn numeric_type(&self, ty: &Type) -> Option<ast::PrimitiveType> {
        let primitive = self.enum_backing_type(ty)?;
        if is_numeric(&Type::Primitive(primitive.clone())) {
            Some(primitive)
        } else {
            None
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::default());
        self.moved_locals.push(HashSet::default());
        self.static_vars.push(HashSet::default());
        self.volatile_vars.push(HashSet::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.moved_locals.pop();
        self.static_vars.pop();
        self.volatile_vars.pop();
    }

    fn bind(&mut self, name: &str, ty: Type, is_mutable: bool, span: Span) {
        let mut duplicate = false;
        if let Some(scope) = self.scopes.last_mut() {
            duplicate = scope.contains_key(name);
            scope.insert(name.to_string(), (ty, is_mutable));
        }
        if duplicate {
            self.error(msg::duplicate_binding(name), span);
        }
    }

    fn lookup(&self, name: &str) -> Option<(Type, bool)> {
        for scope in self.scopes.iter().rev() {
            if let Some((ty, is_mut)) = scope.get(name) {
                return Some((ty.clone(), *is_mut));
            }
        }
        if let Some(ty) = self.extern_variables.get(name) {
            return Some((ty.clone(), true));
        }
        if let Some(ty) = self.global_variables.get(name) {
            return Some((ty.clone(), true));
        }
        None
    }
    fn lookup_type(&self, name: &str) -> Option<Type> {
        self.lookup(name).map(|(ty, _)| ty)
    }

    /// True if `name` is declared `static` in any enclosing scope.
    fn is_static_var(&self, name: &str) -> bool {
        self.static_vars
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    /// True if `name` is declared `volatile` in the innermost scope that binds it.
    fn is_volatile_local(&self, name: &str) -> bool {
        self.volatile_vars
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    fn mark_moved(&mut self, name: &str) {
        if let Some(scope) = self.moved_locals.last_mut() {
            scope.insert(name.to_string());
        }
    }

    pub(crate) fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(TypeError {
            message: message.into(),
            span,
        });
    }
    fn check_struct_attributes(&mut self, attributes: &[ast::Attribute]) {
        if attributes.is_empty() {
            return;
        }

        if let Err(StructAttrError { message, span }) = parse_struct_attributes(attributes) {
            self.error(message, span);
        }
    }

    fn check_global_attributes(&mut self, attributes: &[ast::Attribute]) {
        for error in validate_global_attributes(attributes) {
            self.error(error.message, error.span);
        }
    }
}

fn choose_enum_backing_type(min_value: i128, max_value: i128) -> ast::PrimitiveType {
    if min_value < 0 {
        if min_value >= i8::MIN as i128 && max_value <= i8::MAX as i128 {
            ast::PrimitiveType::I8
        } else if min_value >= i16::MIN as i128 && max_value <= i16::MAX as i128 {
            ast::PrimitiveType::I16
        } else if min_value >= i32::MIN as i128 && max_value <= i32::MAX as i128 {
            ast::PrimitiveType::I32
        } else if min_value >= i64::MIN as i128 && max_value <= i64::MAX as i128 {
            ast::PrimitiveType::I64
        } else {
            ast::PrimitiveType::I128
        }
    } else if max_value <= u8::MAX as i128 {
        ast::PrimitiveType::U8
    } else if max_value <= u16::MAX as i128 {
        ast::PrimitiveType::U16
    } else if max_value <= u32::MAX as i128 {
        ast::PrimitiveType::U32
    } else if max_value <= u64::MAX as i128 {
        ast::PrimitiveType::U64
    } else {
        ast::PrimitiveType::U128
    }
}

pub(crate) fn operator_method_name(operator: &ast::BinaryOperator) -> Option<&'static str> {
    match operator {
        ast::BinaryOperator::Add => Some("__add"),
        ast::BinaryOperator::Subtract => Some("__sub"),
        ast::BinaryOperator::Multiply => Some("__mul"),
        ast::BinaryOperator::Divide => Some("__div"),
        ast::BinaryOperator::Modulo => Some("__mod"),
        ast::BinaryOperator::Equal => Some("__eq"),
        ast::BinaryOperator::NotEqual => Some("__ne"),
        ast::BinaryOperator::Less => Some("__lt"),
        ast::BinaryOperator::Greater => Some("__gt"),
        ast::BinaryOperator::LessEqual => Some("__le"),
        ast::BinaryOperator::GreaterEqual => Some("__ge"),
        ast::BinaryOperator::BitwiseAnd => Some("__bitand"),
        ast::BinaryOperator::BitwiseOr => Some("__bitor"),
        ast::BinaryOperator::BitwiseXor => Some("__bitxor"),
        ast::BinaryOperator::LeftShift => Some("__shl"),
        ast::BinaryOperator::RightShift => Some("__shr"),
        _ => None,
    }
}

pub(crate) fn unary_operator_method_name(operator: &ast::UnaryOperator) -> Option<&'static str> {
    match operator {
        ast::UnaryOperator::Minus => Some("__neg"),
        ast::UnaryOperator::Not => Some("__not"),
        ast::UnaryOperator::BitwiseNot => Some("__bitnot"),
        _ => None,
    }
}

/// After type checking, fills in `iterator_type` on all ForIn AST nodes
/// using the resolved iterator types collected during type checking.
pub fn populate_for_in_iterator_types(
    program: &mut ast::Program,
    resolved_iter_types: &HashMap<(u32, usize, usize), Box<ast::Type>>,
) {
    for item in &mut program.items {
        populate_item_for_in_types(item, resolved_iter_types);
    }
}

/// After type checking, rewrites bare enum constructors (`Some(x)`, `None`,
/// `Ok(x)`, `Err(x)`) into typed `Enum<V>.Variant(...)` constructions using the
/// expected-type inference recorded during typeck.
pub fn rewrite_bare_constructors(
    program: &mut ast::Program,
    rewrites: &HashMap<(u32, usize, usize), BareConstructorRewrite>,
) {
    for item in &mut program.items {
        rewrite_item_bare_constructors(item, rewrites);
    }
}

/// Materialize inferred `let x = expr;` bindings as annotated lets.
/// Runs after type checking so every downstream consumer (escape/move/borrow
/// checks, monomorphization substitution, codegen) sees a plain annotated
/// binding instead of having to handle inference itself.
pub fn populate_inferred_let_types(
    program: &mut ast::Program,
    inferred: &HashMap<(usize, usize), (Type, Span)>,
) {
    for item in &mut program.items {
        match &mut item.kind {
            ast::ItemKind::Function(func) => {
                populate_block_inferred_let_types(&mut func.body, inferred)
            }
            ast::ItemKind::Impl(impl_item) => {
                for member in &mut impl_item.items {
                    if let ast::ImplItemKind::Function(func) = member {
                        populate_block_inferred_let_types(&mut func.body, inferred);
                    }
                }
            }
            ast::ItemKind::Macro(def) => populate_block_inferred_let_types(&mut def.body, inferred),
            _ => {}
        }
    }
}

fn populate_block_inferred_let_types(
    block: &mut ast::Block,
    inferred: &HashMap<(usize, usize), (Type, Span)>,
) {
    for stmt in &mut block.statements {
        populate_statement_inferred_let_types(stmt, inferred);
    }
}

fn populate_statement_inferred_let_types(
    stmt: &mut ast::Statement,
    inferred: &HashMap<(usize, usize), (Type, Span)>,
) {
    if let ast::StatementKind::Let(let_stmt) = &mut stmt.kind
        && let_stmt.type_annotation.is_none()
        && let Some((ty, _name_span)) = inferred.get(&(stmt.span.start, stmt.span.end))
    {
        let_stmt.type_annotation = Some(ty.to_ast());
    }
    // Recurse into nested statement positions regardless of whether this
    // statement itself was an inferred binding.
    match &mut stmt.kind {
        ast::StatementKind::Block(block) => populate_block_inferred_let_types(block, inferred),
        ast::StatementKind::Defer(inner) => {
            populate_statement_inferred_let_types(inner, inferred);
        }
        ast::StatementKind::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.initializer {
                populate_expr_blocks_inferred_let_types(init, inferred);
            }
        }
        ast::StatementKind::Expression(expr)
        | ast::StatementKind::Return(Some(expr))
        | ast::StatementKind::Break(Some(expr)) => {
            populate_expr_blocks_inferred_let_types(expr, inferred);
        }
        _ => {}
    }
}

/// Inferred lets nested inside block-valued expressions (if/while/match/for
/// bodies and blocks).
fn populate_expr_blocks_inferred_let_types(
    expr: &mut ast::Expression,
    inferred: &HashMap<(usize, usize), (Type, Span)>,
) {
    match expr.kind.as_mut() {
        ast::ExpressionKind::Block(block) | ast::ExpressionKind::Lambda { body: block, .. } => {
            populate_block_inferred_let_types(block, inferred);
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            populate_expr_blocks_inferred_let_types(condition, inferred);
            populate_block_inferred_let_types(then_branch, inferred);
            if let Some(else_block) = else_branch {
                populate_block_inferred_let_types(else_block, inferred);
            }
        }
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            populate_expr_blocks_inferred_let_types(condition, inferred);
            populate_expr_blocks_inferred_let_types(then_expr, inferred);
            populate_expr_blocks_inferred_let_types(else_expr, inferred);
        }
        ast::ExpressionKind::While { condition, body } => {
            populate_expr_blocks_inferred_let_types(condition, inferred);
            populate_block_inferred_let_types(body, inferred);
        }
        ast::ExpressionKind::For {
            condition, body, ..
        } => {
            populate_expr_blocks_inferred_let_types(condition, inferred);
            populate_block_inferred_let_types(body, inferred);
        }
        ast::ExpressionKind::Match { expression, arms } => {
            populate_expr_blocks_inferred_let_types(expression, inferred);
            for arm in arms {
                // Arm bodies are block-valued: recurse through the block.
                populate_expr_blocks_inferred_let_types(&mut arm.body, inferred);
            }
        }
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            populate_expr_blocks_inferred_let_types(iterable, inferred);
            populate_block_inferred_let_types(body, inferred);
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            populate_expr_blocks_inferred_let_types(left, inferred);
            populate_expr_blocks_inferred_let_types(right, inferred);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. }
        | ast::ExpressionKind::Move(operand)
        | ast::ExpressionKind::Comptime(operand)
        | ast::ExpressionKind::Reference {
            expression: operand,
            ..
        }
        | ast::ExpressionKind::Launch(operand)
        | ast::ExpressionKind::Wait(operand)
        | ast::ExpressionKind::Cast {
            expression: operand,
            ..
        } => {
            populate_expr_blocks_inferred_let_types(operand, inferred);
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. } => {
            populate_expr_blocks_inferred_let_types(object, inferred);
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            populate_expr_blocks_inferred_let_types(receiver, inferred);
            for arg in arguments {
                populate_expr_blocks_inferred_let_types(arg, inferred);
            }
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            populate_expr_blocks_inferred_let_types(function, inferred);
            for arg in arguments {
                populate_expr_blocks_inferred_let_types(arg, inferred);
            }
        }
        ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Asm { .. }
        | ast::ExpressionKind::EnumVariant { .. } => {}
        _ => {}
    }
}

fn rewrite_item_bare_constructors(
    item: &mut ast::Item,
    rewrites: &HashMap<(u32, usize, usize), BareConstructorRewrite>,
) {
    match &mut item.kind {
        ast::ItemKind::Function(func) => {
            rewrite_block_bare_constructors(&mut func.body, rewrites);
        }
        ast::ItemKind::Impl(impl_item) => {
            for member in &mut impl_item.items {
                if let ast::ImplItemKind::Function(func) = member {
                    rewrite_block_bare_constructors(&mut func.body, rewrites);
                }
            }
        }
        ast::ItemKind::Macro(def) => {
            rewrite_block_bare_constructors(&mut def.body, rewrites);
        }
        _ => {}
    }
}

fn rewrite_block_bare_constructors(
    block: &mut ast::Block,
    rewrites: &HashMap<(u32, usize, usize), BareConstructorRewrite>,
) {
    for stmt in &mut block.statements {
        rewrite_statement_bare_constructors(stmt, rewrites);
    }
}

fn rewrite_statement_bare_constructors(
    stmt: &mut ast::Statement,
    rewrites: &HashMap<(u32, usize, usize), BareConstructorRewrite>,
) {
    match &mut stmt.kind {
        ast::StatementKind::Expression(expr)
        | ast::StatementKind::Return(Some(expr))
        | ast::StatementKind::Break(Some(expr)) => {
            rewrite_expression_bare_constructors(expr, rewrites);
        }
        ast::StatementKind::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.initializer {
                rewrite_expression_bare_constructors(init, rewrites);
            }
        }
        ast::StatementKind::Block(block) => rewrite_block_bare_constructors(block, rewrites),
        ast::StatementKind::Defer(inner) => {
            rewrite_statement_bare_constructors(inner, rewrites);
        }
        ast::StatementKind::Return(None) | ast::StatementKind::Break(None) => {}
        ast::StatementKind::Continue => {}
    }
}

fn rewrite_expression_bare_constructors(
    expr: &mut ast::Expression,
    rewrites: &HashMap<(u32, usize, usize), BareConstructorRewrite>,
) {
    let key = (expr.span.file, expr.span.start, expr.span.end);
    if let Some(rewrite) = rewrites.get(&key).cloned() {
        let receiver_ty = ast::Type {
            kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                path: vec![ast::Identifier {
                    name: rewrite.enum_name.clone(),
                    span: expr.span,
                }],
                generics: if rewrite.generics.is_empty() {
                    None
                } else {
                    Some(rewrite.generics.clone())
                },
            })),
            span: expr.span,
        };
        let receiver = ast::Expression {
            kind: Box::new(ast::ExpressionKind::TypeName(receiver_ty)),
            span: expr.span,
        };
        let args: Vec<ast::Expression> = match expr.kind.as_ref() {
            ast::ExpressionKind::Call { arguments, .. } => arguments.clone(),
            ast::ExpressionKind::MethodCall { arguments, .. } => arguments.clone(),
            _ => Vec::new(),
        };
        let final_args = if rewrite.variant == "Err" && args.len() == 2 {
            let mut it = args.into_iter();
            let code_arg = it.next().unwrap();
            let msg_arg = it.next().unwrap();
            let cast_code = ast::Expression {
                kind: Box::new(ast::ExpressionKind::Cast {
                    expression: Box::new(code_arg),
                    target_type: Box::new(ast::Type {
                        kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::I64)),
                        span: expr.span,
                    }),
                }),
                span: expr.span,
            };
            let error_receiver = ast::Expression {
                kind: Box::new(ast::ExpressionKind::TypeName(ast::Type {
                    kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                        path: vec![ast::Identifier {
                            name: "Error".to_string(),
                            span: expr.span,
                        }],
                        generics: None,
                    })),
                    span: expr.span,
                })),
                span: expr.span,
            };
            let error_call = ast::Expression {
                kind: Box::new(ast::ExpressionKind::MethodCall {
                    receiver: Box::new(error_receiver),
                    method: ast::Identifier {
                        name: "new".to_string(),
                        span: expr.span,
                    },
                    arguments: vec![cast_code, msg_arg],
                }),
                span: expr.span,
            };
            vec![error_call]
        } else {
            args
        };
        if final_args.is_empty() && matches!(expr.kind.as_ref(), ast::ExpressionKind::Identifier(_))
        {
            *expr.kind = ast::ExpressionKind::FieldAccess {
                object: Box::new(receiver),
                field: ast::Identifier {
                    name: rewrite.variant.clone(),
                    span: expr.span,
                },
            };
        } else {
            *expr.kind = ast::ExpressionKind::MethodCall {
                receiver: Box::new(receiver),
                method: ast::Identifier {
                    name: rewrite.variant.clone(),
                    span: expr.span,
                },
                arguments: final_args,
            };
        }
        return;
    }
    match expr.kind.as_mut() {
        ast::ExpressionKind::Block(block) | ast::ExpressionKind::Lambda { body: block, .. } => {
            rewrite_block_bare_constructors(block, rewrites);
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rewrite_expression_bare_constructors(condition, rewrites);
            rewrite_block_bare_constructors(then_branch, rewrites);
            if let Some(else_block) = else_branch {
                rewrite_block_bare_constructors(else_block, rewrites);
            }
        }
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            rewrite_expression_bare_constructors(condition, rewrites);
            rewrite_expression_bare_constructors(then_expr, rewrites);
            rewrite_expression_bare_constructors(else_expr, rewrites);
        }
        ast::ExpressionKind::UnwrapOr { value, fallback } => {
            rewrite_expression_bare_constructors(value, rewrites);
            rewrite_expression_bare_constructors(fallback, rewrites);
        }
        ast::ExpressionKind::While { condition, body } => {
            rewrite_expression_bare_constructors(condition, rewrites);
            rewrite_block_bare_constructors(body, rewrites);
        }
        ast::ExpressionKind::For {
            condition, body, ..
        } => {
            rewrite_expression_bare_constructors(condition, rewrites);
            rewrite_block_bare_constructors(body, rewrites);
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            rewrite_expression_bare_constructors(left, rewrites);
            rewrite_expression_bare_constructors(right, rewrites);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. }
        | ast::ExpressionKind::Move(operand)
        | ast::ExpressionKind::Comptime(operand)
        | ast::ExpressionKind::Reference {
            expression: operand,
            ..
        }
        | ast::ExpressionKind::Launch(operand)
        | ast::ExpressionKind::Wait(operand) => {
            rewrite_expression_bare_constructors(operand, rewrites);
        }
        ast::ExpressionKind::Cast { expression, .. } => {
            rewrite_expression_bare_constructors(expression, rewrites);
        }
        ast::ExpressionKind::FieldAccess { object, .. } => {
            rewrite_expression_bare_constructors(object, rewrites);
        }
        ast::ExpressionKind::Index { object, index, .. } => {
            rewrite_expression_bare_constructors(object, rewrites);
            rewrite_expression_bare_constructors(index, rewrites);
        }
        ast::ExpressionKind::Slice {
            object,
            start,
            end,
            step,
            ..
        } => {
            rewrite_expression_bare_constructors(object, rewrites);
            if let Some(s) = start {
                rewrite_expression_bare_constructors(s, rewrites);
            }
            if let Some(e) = end {
                rewrite_expression_bare_constructors(e, rewrites);
            }
            if let Some(st) = step {
                rewrite_expression_bare_constructors(st, rewrites);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            rewrite_expression_bare_constructors(receiver, rewrites);
            for arg in arguments {
                rewrite_expression_bare_constructors(arg, rewrites);
            }
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            rewrite_expression_bare_constructors(function, rewrites);
            for arg in arguments {
                rewrite_expression_bare_constructors(arg, rewrites);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                rewrite_expression_bare_constructors(&mut field.value, rewrites);
            }
        }
        ast::ExpressionKind::Array(elements) | ast::ExpressionKind::Tuple(elements) => {
            for element in elements {
                rewrite_expression_bare_constructors(element, rewrites);
            }
        }
        ast::ExpressionKind::Initializer { items, .. } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr)
                    | ast::InitializerItem::Field { value: expr, .. } => {
                        rewrite_expression_bare_constructors(expr, rewrites);
                    }
                    ast::InitializerItem::Index { index, value, .. } => {
                        rewrite_expression_bare_constructors(index, rewrites);
                        rewrite_expression_bare_constructors(value, rewrites);
                    }
                }
            }
        }
        ast::ExpressionKind::Match { expression, arms } => {
            rewrite_expression_bare_constructors(expression, rewrites);
            for arm in arms {
                rewrite_expression_bare_constructors(&mut arm.body, rewrites);
            }
        }
        ast::ExpressionKind::MacroCall { args, .. } => {
            for arg in args {
                if let ast::MacroArg::Expression(expr) = arg {
                    rewrite_expression_bare_constructors(expr, rewrites);
                }
            }
        }
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            rewrite_expression_bare_constructors(iterable, rewrites);
            rewrite_block_bare_constructors(body, rewrites);
        }
        ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Asm { .. }
        | ast::ExpressionKind::EnumVariant { .. } => {}
    }
}

fn populate_item_for_in_types(
    item: &mut ast::Item,
    resolved_iter_types: &HashMap<(u32, usize, usize), Box<ast::Type>>,
) {
    match &mut item.kind {
        ast::ItemKind::Function(func) => {
            populate_block_for_in_types(&mut func.body, resolved_iter_types);
        }
        ast::ItemKind::Impl(impl_item) => {
            for member in &mut impl_item.items {
                if let ast::ImplItemKind::Function(func) = member {
                    populate_block_for_in_types(&mut func.body, resolved_iter_types);
                }
            }
        }
        ast::ItemKind::Macro(def) => {
            populate_block_for_in_types(&mut def.body, resolved_iter_types);
        }
        _ => {}
    }
}

fn populate_block_for_in_types(
    block: &mut ast::Block,
    resolved_iter_types: &HashMap<(u32, usize, usize), Box<ast::Type>>,
) {
    for stmt in &mut block.statements {
        populate_statement_for_in_types(stmt, resolved_iter_types);
    }
}

fn populate_statement_for_in_types(
    stmt: &mut ast::Statement,
    resolved_iter_types: &HashMap<(u32, usize, usize), Box<ast::Type>>,
) {
    match &mut stmt.kind {
        ast::StatementKind::Expression(expr) => {
            populate_expression_for_in_types(expr, resolved_iter_types);
        }
        ast::StatementKind::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.initializer {
                populate_expression_for_in_types(init, resolved_iter_types);
            }
        }
        ast::StatementKind::Block(block) => {
            populate_block_for_in_types(block, resolved_iter_types);
        }
        ast::StatementKind::Return(expr) => {
            if let Some(expr) = expr {
                populate_expression_for_in_types(expr, resolved_iter_types);
            }
        }
        ast::StatementKind::Defer(inner) => {
            populate_statement_for_in_types(inner, resolved_iter_types);
        }
        ast::StatementKind::Break(expr) => {
            if let Some(expr) = expr {
                populate_expression_for_in_types(expr, resolved_iter_types);
            }
        }
        ast::StatementKind::Continue => {}
    }
}

fn populate_expression_for_in_types(
    expr: &mut ast::Expression,
    resolved_iter_types: &HashMap<(u32, usize, usize), Box<ast::Type>>,
) {
    match expr.kind.as_mut() {
        ast::ExpressionKind::ForIn { iterator_type, .. } => {
            if let Some(iter_ty) =
                resolved_iter_types.get(&(expr.span.file, expr.span.start, expr.span.end))
            {
                *iterator_type = Some(iter_ty.clone());
            }
        }
        ast::ExpressionKind::Block(block) => {
            populate_block_for_in_types(block, resolved_iter_types);
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            populate_expression_for_in_types(condition, resolved_iter_types);
            populate_block_for_in_types(then_branch, resolved_iter_types);
            if let Some(else_block) = else_branch {
                populate_block_for_in_types(else_block, resolved_iter_types);
            }
        }
        ast::ExpressionKind::While {
            condition, body, ..
        } => {
            populate_expression_for_in_types(condition, resolved_iter_types);
            populate_block_for_in_types(body, resolved_iter_types);
        }
        ast::ExpressionKind::For {
            condition, body, ..
        } => {
            populate_expression_for_in_types(condition, resolved_iter_types);
            populate_block_for_in_types(body, resolved_iter_types);
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            populate_expression_for_in_types(left, resolved_iter_types);
            populate_expression_for_in_types(right, resolved_iter_types);
        }
        ast::ExpressionKind::Unary { operand, .. } => {
            populate_expression_for_in_types(operand, resolved_iter_types);
        }
        ast::ExpressionKind::Postfix { operand, .. } => {
            populate_expression_for_in_types(operand, resolved_iter_types);
        }
        ast::ExpressionKind::Cast { expression, .. } => {
            populate_expression_for_in_types(expression, resolved_iter_types);
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                populate_expression_for_in_types(&mut field.value, resolved_iter_types);
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. } => {
            populate_expression_for_in_types(object, resolved_iter_types);
        }
        ast::ExpressionKind::Index { object, index, .. } => {
            populate_expression_for_in_types(object, resolved_iter_types);
            populate_expression_for_in_types(index, resolved_iter_types);
        }
        ast::ExpressionKind::Slice {
            object,
            start,
            end,
            step,
            ..
        } => {
            populate_expression_for_in_types(object, resolved_iter_types);
            if let Some(s) = start {
                populate_expression_for_in_types(s, resolved_iter_types);
            }
            if let Some(e) = end {
                populate_expression_for_in_types(e, resolved_iter_types);
            }
            if let Some(st) = step {
                populate_expression_for_in_types(st, resolved_iter_types);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            populate_expression_for_in_types(receiver, resolved_iter_types);
            for arg in arguments {
                populate_expression_for_in_types(arg, resolved_iter_types);
            }
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
            ..
        } => {
            populate_expression_for_in_types(function, resolved_iter_types);
            for arg in arguments {
                populate_expression_for_in_types(arg, resolved_iter_types);
            }
        }
        ast::ExpressionKind::Array(elements) => {
            for element in elements {
                populate_expression_for_in_types(element, resolved_iter_types);
            }
        }
        ast::ExpressionKind::Tuple(elements) => {
            for element in elements {
                populate_expression_for_in_types(element, resolved_iter_types);
            }
        }
        ast::ExpressionKind::Initializer { items, .. } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr) => {
                        populate_expression_for_in_types(expr, resolved_iter_types);
                    }
                    ast::InitializerItem::Field { value, .. } => {
                        populate_expression_for_in_types(value, resolved_iter_types);
                    }
                    ast::InitializerItem::Index { index, value, .. } => {
                        populate_expression_for_in_types(index, resolved_iter_types);
                        populate_expression_for_in_types(value, resolved_iter_types);
                    }
                }
            }
        }
        ast::ExpressionKind::Match {
            expression, arms, ..
        } => {
            populate_expression_for_in_types(expression, resolved_iter_types);
            for arm in arms {
                populate_expression_for_in_types(&mut arm.body, resolved_iter_types);
            }
        }
        ast::ExpressionKind::MacroCall { args, .. } => {
            for arg in args {
                if let ast::MacroArg::Expression(expr) = arg {
                    populate_expression_for_in_types(expr, resolved_iter_types);
                }
            }
        }
        _ => {}
    }
}
