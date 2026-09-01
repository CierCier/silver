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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

#[derive(Default)]
pub struct TypeChecker {
    errors: Vec<TypeError>,
    scopes: Vec<HashMap<String, (Type, bool)>>,
    moved_locals: Vec<HashSet<String>>,
    static_vars: Vec<HashSet<String>>,
    volatile_vars: Vec<HashSet<String>>,
    volatile_globals: HashSet<String>,
    current_return: Option<Type>,
    method_symbols: HashMap<SymbolId, MethodSig>,
    defer_depth: u32,
    methods: HashMap<(String, String), Vec<SymbolId>>,
    type_ctx: TypeContext,
    function_symbols: HashMap<SymbolId, FunctionSig>,
    functions: HashMap<String, Vec<SymbolId>>,
    known_type_ids: HashSet<SymbolId>,
    known_types: HashMap<String, SymbolId>,
    struct_defs: HashMap<String, StructDef>,
    enum_defs: HashMap<String, EnumDef>,
    /// Default type arguments for local aggregate definitions.
    generic_defaults: HashMap<String, Vec<Option<ast::Type>>>,
    trait_impls: HashMap<String, HashSet<String>>,
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
    pub resolved_iter_types: HashMap<(usize, usize), Box<ast::Type>>,
    /// Bare enum constructors (`Some(x)`, `None`, `Ok(x)`, `Err(x)`) resolved
    /// via expected-type inference during typeck; applied to the AST after typeck.
    pub bare_constructors: HashMap<(usize, usize), BareConstructorRewrite>,
    /// Implicit guards: binary operators on bare type params recorded while
    /// checking a generic function body, keyed by function/method signature.
    /// Checked at every concrete monomorphization request.
    current_implicit_reqs: Vec<ImplicitReq>,
    implicit_reqs: HashMap<String, Vec<ImplicitReq>>,
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
    source_impl: ast::ImplItem,
    source_method: ast::ImplFunction,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<Type>,
    return_type: Type,
    type_params: Vec<String>,
    bounds: Vec<TypeBoundPredicate>,
    source: ast::FunctionItem,
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
        Type::from_ast(&normalized)
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
        for item in &program.items {
            self.check_global_attributes(&item.attributes);
            if let ast::ItemKind::Struct(_) = &item.kind {
                self.check_struct_attributes(&item.attributes);
            }
            if let ast::ItemKind::Function(func) = &item.kind {
                self.errors
                    .extend(Self::check_function_attributes(&item.attributes));
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
                for impl_member in &impl_item.items {
                    match impl_member {
                        ast::ImplItemKind::Function(func) => {
                            self.errors
                                .extend(Self::check_function_attributes(&func.attributes));
                            self.check_impl_method(&self_ty, func);
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            self.check_impl_cast(&self_ty, cast);
                        }
                        _ => {}
                    }
                }
            }
        }
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
    pub fn take_resolved_iter_types(&mut self) -> HashMap<(usize, usize), Box<ast::Type>> {
        std::mem::take(&mut self.resolved_iter_types)
    }

    /// Consume bare-constructor rewrite records for post-typeck AST rewriting.
    pub fn take_bare_constructors(&mut self) -> HashMap<(usize, usize), BareConstructorRewrite> {
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
                            source: ast::FunctionItem {
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
                            },
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
                                source_impl: ast::ImplItem {
                                    generics: None,
                                    trait_ref: None,
                                    self_type: owner_ty.to_ast(),
                                    items: Vec::new(),
                                    implicit_type_params: Vec::new(),
                                },
                                source_method: ast::ImplFunction {
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
                                            span: Span::default(),
                                        })
                                        .collect(),
                                    method_kind: if let Some(first) = params.first() {
                                        match first {
                                            Type::Pointer { .. } => {
                                                ast::MethodKind::InstancePointer {
                                                    is_mutable: true,
                                                }
                                            }
                                            Type::Reference { is_mutable, .. } => {
                                                ast::MethodKind::InstancePointer {
                                                    is_mutable: *is_mutable,
                                                }
                                            }
                                            _ if *first == owner_ty => {
                                                ast::MethodKind::InstanceValue
                                            }
                                            _ => ast::MethodKind::Static,
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
                                },
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
                        .map(|field| (field.name.name.clone(), Type::from_ast(&field.field_type)))
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
            let Some(trait_ref) = &impl_item.trait_ref else {
                continue;
            };
            let name = trait_ref
                .path
                .last()
                .map(|id| id.name.clone())
                .unwrap_or_default();
            if name.is_empty() {
                continue;
            }
            let self_ty = Type::from_ast(&impl_item.self_type);
            let key = self_ty.canonical_key();
            if self.is_concrete_type(&self_ty) {
                self.trait_impls
                    .entry(name.clone())
                    .or_default()
                    .insert(key);
            }
            // Record the Drop-owner base name for ANY Drop impl (generic
            // templates included): Vec<String> must count even though
            // `impl<T> Drop<Vec<T>>` has a non-concrete self type.
            if name == "Drop"
                && let ast::TypeKind::Named(named) = impl_item.self_type.kind.as_ref()
            {
                self.drop_owner_bases.insert(named.path[0].name.clone());
            }
            // Record the Drop-owner base name for ANY Drop impl (generic
            // templates included): Vec<String> must count even though
            // `impl<T> Drop<Vec<T>>` has a non-concrete self type.
            if name == "Drop"
                && let ast::TypeKind::Named(named) = impl_item.self_type.kind.as_ref()
            {
                self.drop_owner_bases.insert(named.path[0].name.clone());
            }
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
            }
        }
        errors
    }

    fn check_function(&mut self, func: &ast::FunctionItem) {
        self.current_implicit_reqs = Vec::new();
        self.current_implicit_method_reqs = Vec::new();
        let return_type = func
            .return_type
            .as_ref()
            .map(Type::from_ast)
            .unwrap_or(Type::Unit);
        self.current_return = Some(return_type);

        self.push_scope();
        for param in &func.parameters {
            let param_type = Type::from_ast(&param.param_type);
            self.reject_plain_void_value_type(&param_type, param.param_type.span);
            self.bind(&param.name.name, param_type, param.is_mutable, param.span);
        }
        self.check_block(&func.body);
        self.pop_scope();
        self.current_return = None;
        let key = Self::free_fn_key(func);
        self.store_implicit_reqs(key.clone());
        self.store_implicit_method_reqs(key);
    }
    fn check_impl_method(&mut self, self_ty: &Type, func: &ast::ImplFunction) {
        self.current_implicit_reqs = Vec::new();
        self.current_implicit_method_reqs = Vec::new();
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| self.substitute_self_type(&Type::from_ast(t), self_ty))
            .unwrap_or(Type::Unit);
        self.current_return = Some(return_type);

        self.push_scope();
        for param in &func.parameters {
            let param_type = self.substitute_self_type(&Type::from_ast(&param.param_type), self_ty);
            self.reject_plain_void_value_type(&param_type, param.param_type.span);
            self.bind(&param.name.name, param_type, param.is_mutable, param.span);
        }
        self.check_block(&func.body);
        self.pop_scope();
        self.current_return = None;
        let key = Self::impl_method_key(self_ty, func);
        self.store_implicit_reqs(key.clone());
        self.store_implicit_method_reqs(key);
    }

    fn check_impl_cast(&mut self, self_ty: &Type, cast: &ast::ImplCast) {
        let return_type = self.substitute_self_type(&Type::from_ast(&cast.target_type), self_ty);
        self.current_return = Some(return_type);

        self.push_scope();
        for param in &cast.parameters {
            let param_type = self.substitute_self_type(&Type::from_ast(&param.param_type), self_ty);
            self.reject_plain_void_value_type(&param_type, param.param_type.span);
            self.bind(&param.name.name, param_type, param.is_mutable, param.span);
        }
        self.check_block(&cast.body);
        self.pop_scope();
        self.current_return = None;
    }

    fn check_block(&mut self, block: &ast::Block) {
        self.push_scope();
        for stmt in &block.statements {
            self.check_statement(stmt);
        }
        self.pop_scope();
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

                    if let Some(init) = &let_stmt.initializer {
                        let init_type = self.check_expr(init, Some(&declared));
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

    /// Bind an identifier let pattern to its declared (or inferred) type:
    /// registers the local, static/volatile qualifiers, and the hover type.
    fn bind_let_pattern(&mut self, let_stmt: &ast::LetStatement, declared: Type) {
        let ast::PatternKind::Identifier(ident) = &let_stmt.pattern.kind else {
            self.error(
                "let declarations must bind a single identifier; destructuring \
                patterns are not supported"
                    .to_string(),
                let_stmt.pattern.span,
            );
            return;
        };
        self.bind(
            &ident.name,
            declared.clone(),
            let_stmt.is_mutable,
            let_stmt.pattern.span,
        );
        if let Some(scope) = self.static_vars.last_mut()
            && let_stmt.is_static
        {
            scope.insert(ident.name.clone());
        }
        if let Some(scope) = self.volatile_vars.last_mut()
            && let_stmt.is_volatile
        {
            scope.insert(ident.name.clone());
        }
        // Record for hover: variable name gets its declared type
        self.expr_types.insert(
            (let_stmt.pattern.span.start, let_stmt.pattern.span.end),
            declared.to_string(),
        );
    }

    fn check_expr(&mut self, expr: &ast::Expression, expected: Option<&Type>) -> Type {
        #[expect(
            clippy::match_like_matches_macro,
            reason = "two-column (primitive, literal) table reads better as a match than matches!"
        )]
        let ty = match expr.kind.as_ref() {
            ast::ExpressionKind::Literal(literal) => {
                self.literal_type(literal, expected, &expr.span)
            }
            ast::ExpressionKind::Identifier(ident) => match self.lookup_type(&ident.name) {
                Some(ty) => {
                    // Array-to-pointer decay: `i64 arr[9]` as an expression
                    // yields `i64*` pointing to the first element.
                    if let Type::Array { element, .. } = &ty {
                        return Type::Pointer {
                            inner: element.clone(),
                            is_mutable: true,
                            is_volatile: false,
                        };
                    }
                    ty
                }
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
                    let value = if *operator == ast::UnaryOperator::Minus {
                        n.checked_neg().unwrap_or(i128::MIN)
                    } else {
                        *n
                    };
                    self.type_integer_literal_value(value, expected, &expr.span)
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
                    if left_ty != right_ty {
                        // Integer literals default to i128, so a literal
                        // operand narrows to the other operand's type; mixed
                        // integer widths share a common type (codegen casts).
                        if !(self.is_integer_type(&left_ty)
                            && self.is_integer_type(&right_ty)
                            && self.common_numeric_type(&left_ty, &right_ty).is_some())
                            && !self
                                .defer_operator_if_generic(&left_ty, &right_ty, operator, expr.span)
                        {
                            self.error(
                                format!(
                                    "bitwise operands must match, got {} and {}",
                                    left_ty, right_ty
                                ),
                                expr.span,
                            );
                        }
                    }
                    if !self.is_integer_type(&left_ty) || !self.is_integer_type(&right_ty) {
                        if self.defer_operator_if_generic(&left_ty, &right_ty, operator, expr.span)
                        {
                            return left_ty;
                        }
                        self.error(
                            format!(
                                "bitwise operator requires integer operands, got {} and {}",
                                left_ty, right_ty
                            ),
                            expr.span,
                        );
                    }
                    // Try operator overload for non-primitive types
                    if !self.is_primitive_type(&left_ty)
                        && let Some(result_ty) =
                            self.resolve_operator_overload(&left_ty, &right_ty, operator, expr)
                    {
                        return result_ty;
                    }

                    self.common_numeric_type(&left_ty, &right_ty)
                        .unwrap_or(left_ty)
                }
                ast::BinaryOperator::Assign
                | ast::BinaryOperator::AddAssign
                | ast::BinaryOperator::SubtractAssign
                | ast::BinaryOperator::MultiplyAssign
                | ast::BinaryOperator::DivideAssign
                | ast::BinaryOperator::ModuloAssign => {
                    let left_ty = self.check_expr(left, None);
                    let right_ty = self.check_expr(right, None);
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
                    // Try operator overload for compound assignment
                    if !self.is_primitive_type(&left_ty)
                        && matches!(
                            operator,
                            ast::BinaryOperator::AddAssign
                                | ast::BinaryOperator::SubtractAssign
                                | ast::BinaryOperator::MultiplyAssign
                                | ast::BinaryOperator::DivideAssign
                                | ast::BinaryOperator::ModuloAssign
                        )
                    {
                        let bin_op = match operator {
                            ast::BinaryOperator::AddAssign => ast::BinaryOperator::Add,
                            ast::BinaryOperator::SubtractAssign => ast::BinaryOperator::Subtract,
                            ast::BinaryOperator::MultiplyAssign => ast::BinaryOperator::Multiply,
                            ast::BinaryOperator::DivideAssign => ast::BinaryOperator::Divide,
                            ast::BinaryOperator::ModuloAssign => ast::BinaryOperator::Modulo,
                            _ => unreachable!(),
                        };
                        self.resolve_operator_overload(&left_ty, &right_ty, &bin_op, expr);
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
                let cond_ty = self.check_expr(condition, None);
                if !is_bool(&cond_ty) {
                    self.error(
                        format!("if condition must be bool, found {}", cond_ty),
                        condition.span,
                    );
                }
                self.check_block(then_branch);
                if let Some(block) = else_branch {
                    self.check_block(block);
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
            ast::ExpressionKind::Block(block) => {
                self.check_block(block);
                Type::Unit
            }
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
            ast::ExpressionKind::Initializer { .. } => Type::Unknown,
            ast::ExpressionKind::Tuple(items) => {
                let types = items
                    .iter()
                    .map(|item| self.check_expr(item, None))
                    .collect();
                Type::Tuple(types)
            }
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                let from = self.check_expr(expression, None);
                let to = Type::from_ast(target_type);
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
                                .map(|arg| self.check_expr_with_literal_naturals(arg))
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
                    let fn_ty = self.check_expr(function, None);
                    if let Type::Function {
                        params,
                        return_type,
                    } = &fn_ty
                    {
                        let _arg_types: Vec<Type> = arguments
                            .iter()
                            .map(|arg| self.check_expr(arg, None))
                            .collect();
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
                let receiver_ty = self.check_expr(receiver, None);
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
                        if let Some(first) = arm_types.first() {
                            let unified = first.clone();
                            for (i, other) in arm_types.iter().enumerate().skip(1) {
                                if &unified != other {
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
                            Type::Unit
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
                    if let Some(first) = arm_types.first() {
                        let unified = first.clone();
                        for (i, other) in arm_types.iter().enumerate().skip(1) {
                            if &unified != other {
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
                        Type::Unit
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
                            (expr.span.start, expr.span.end),
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
        let arg_types = arguments
            .iter()
            .map(|arg| self.check_expr_with_literal_naturals(arg))
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
            if candidate.is_variadic {
                if candidate.params.len() > arguments.len() {
                    continue;
                }
            } else if candidate.params.len() != arguments.len() {
                continue;
            }
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

            for (i, (param_ty, arg_ty)) in candidate.params.iter().zip(arg_types.iter()).enumerate()
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
            let owner_inferred = self.infer_type_params(
                &candidate.owner,
                owner_ty,
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
                        owner_ty,
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
                        mapping.insert(param_path[0].clone(), exp_arg.clone());
                    }
                }
            }

            if style == MethodCallStyle::Instance && candidate.params.len() == arg_types.len() + 1 {
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

            if ok {
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
                            mapping = inferred_mapping;
                            matched = true;
                        } else if let Type::Reference {
                            is_mutable: false,
                            inner,
                        } = &substituted
                            && (inner.as_ref() == arg_ty || self.is_assignable(inner, arg_ty))
                        {
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
                            matched = true;
                        } else if let Type::Reference {
                            is_mutable: false,
                            inner,
                        } = &substituted
                            && (inner.as_ref() == arg_ty || self.is_assignable(inner, arg_ty))
                        {
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
                    self.collect_function_item(func, false, table);
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
                source: func.clone(),
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
            return self.type_integer_literal_value(*value, expected, span);
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
    fn type_integer_literal_value(
        &mut self,
        value: i128,
        expected: Option<&Type>,
        span: &Span,
    ) -> Type {
        if let Some(expected_ty) = expected
            && let Type::Primitive(prim) = expected_ty
            && let Some((min, max)) = Self::integer_prim_range(prim)
        {
            if value < min || value > max {
                self.error(
                    format!("integer literal {} does not fit in type {:?}", value, prim),
                    *span,
                );
            }
            return expected_ty.clone();
        }
        Type::Primitive(ast::PrimitiveType::I128)
    }

    /// Inclusive value range for an integer primitive, or None for non-ints.
    fn integer_prim_range(prim: &ast::PrimitiveType) -> Option<(i128, i128)> {
        Some(match prim {
            ast::PrimitiveType::I8 => (i8::MIN as i128, i8::MAX as i128),
            ast::PrimitiveType::I16 => (i16::MIN as i128, i16::MAX as i128),
            ast::PrimitiveType::I32 => (i32::MIN as i128, i32::MAX as i128),
            ast::PrimitiveType::I64 => (i64::MIN as i128, i64::MAX as i128),
            ast::PrimitiveType::I128 => (i128::MIN, i128::MAX),
            ast::PrimitiveType::U8 => (0, u8::MAX as i128),
            ast::PrimitiveType::U16 => (0, u16::MAX as i128),
            ast::PrimitiveType::U32 => (0, u32::MAX as i128),
            ast::PrimitiveType::U64 => (0, u64::MAX as i128),
            ast::PrimitiveType::U128 => (0, i128::MAX), // AST literal is i128
            _ => return None,
        })
    }

    /// True when an integer literal value fits the integer primitive.
    fn integer_value_fits(value: i128, prim: &ast::PrimitiveType) -> bool {
        Self::integer_prim_range(prim).is_some_and(|(min, max)| value >= min && value <= max)
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
            ast::Literal::String(_) => is_string(expected),
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
            let bounded_key = bounded.canonical_key();
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
                let has_impl = self
                    .trait_impls
                    .get(&trait_name)
                    .map(|set| set.contains(&bounded_key))
                    .unwrap_or(false);
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
            source: Box::new(candidate.source.clone()),
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
        if candidate.type_params.is_empty() {
            return;
        }
        if !self.mapping_is_concrete(mapping, &candidate.type_params) {
            return;
        }
        self.monomorph_requests.push(MonomorphRequest::ImplMethod {
            impl_item: Box::new(candidate.source_impl.clone()),
            method: Box::new(candidate.source_method.clone()),
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
            Type::Primitive(_) | Type::Unit => true,
            Type::Unknown => false,
        }
    }

    fn is_assignable(&self, expected: &Type, found: &Type) -> bool {
        if expected == found || Self::void_compatible(expected, found) {
            return true;
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
            ) => !is_volatile && e_inner == f_inner,
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
            // Byte pointers (u8*/char*) convert implicitly; a mutable source
            // may become a const target, never the reverse. Volatile pointees
            // must not silently become plain pointers.
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
            ) if matches!(
                to_inner.as_ref(),
                Type::Primitive(ast::PrimitiveType::U8 | ast::PrimitiveType::Char)
            ) && matches!(
                from_inner.as_ref(),
                Type::Primitive(ast::PrimitiveType::U8 | ast::PrimitiveType::Char)
            ) =>
            {
                !*to_vol && !*from_vol && (!*to_mut || *from_mut)
            }
            // str and u8* are the same byte pointer: convert implicitly in
            // both directions (str is a non-owning view of a NUL-terminated
            // byte buffer, so mutable u8* is accepted as well).
            (Type::Primitive(ast::PrimitiveType::Str), Type::Pointer { inner, .. }) => {
                matches!(
                    inner.as_ref(),
                    Type::Primitive(
                        ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                    )
                )
            }
            (Type::Pointer { inner, .. }, Type::Primitive(ast::PrimitiveType::Str)) => {
                matches!(
                    inner.as_ref(),
                    Type::Primitive(
                        ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                    )
                )
            }
            (Type::Primitive(ast::PrimitiveType::Str), Type::Slice { element }) => {
                matches!(
                    element.as_ref(),
                    Type::Primitive(
                        ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                    )
                )
            }
            (Type::Slice { element }, Type::Primitive(ast::PrimitiveType::Str)) => {
                matches!(
                    element.as_ref(),
                    Type::Primitive(
                        ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                    )
                )
            }
            (Type::Pointer { inner, .. }, Type::Slice { element }) => {
                matches!(
                    inner.as_ref(),
                    Type::Primitive(
                        ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                    )
                ) && matches!(
                    element.as_ref(),
                    Type::Primitive(
                        ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                    )
                )
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
                matches!(inner.as_ref(), Type::Primitive(ast::PrimitiveType::Char))
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
            ) => self.infer_type_params(inner, found_inner, type_params, mapping),
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
                Type::Pointer { .. } | Type::Reference { .. } | Type::Function { .. }
            );
        let to_ok = self.is_primitive_type(to)
            || matches!(
                to,
                Type::Pointer { .. } | Type::Reference { .. } | Type::Function { .. }
            );
        if from_ok && to_ok {
            return true;
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
        if !self.has_json_trait_impl("ToJson", &owner_ty) {
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
        if !self.has_json_trait_impl("FromJson", &target_ty) {
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

    fn has_json_trait_impl(&self, trait_name: &str, ty: &Type) -> bool {
        self.trait_impls
            .get(trait_name)
            .is_some_and(|impls| impls.contains(&ty.canonical_key()))
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

        match (from, to) {
            (
                Type::Primitive(ast::PrimitiveType::Str),
                Type::Pointer {
                    is_mutable,
                    is_volatile: _,
                    inner,
                },
            ) => {
                // str is a byte pointer; implicit conversion to const char*,
                // const u8*, or const i8* (mutable targets require an explicit cast).
                if *is_mutable {
                    return false;
                }
                matches!(
                    inner.as_ref(),
                    Type::Primitive(
                        ast::PrimitiveType::Char | ast::PrimitiveType::U8 | ast::PrimitiveType::I8
                    )
                )
            }
            (Type::Pointer { inner, .. }, Type::Primitive(ast::PrimitiveType::Str)) => {
                matches!(
                    inner.as_ref(),
                    Type::Primitive(
                        ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                    )
                )
            }
            // Byte pointers (u8*/i8*/char*) convert implicitly; a mutable source
            // may become a const target, never the reverse.
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
            ) if matches!(
                to_inner.as_ref(),
                Type::Primitive(
                    ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                )
            ) && matches!(
                from_inner.as_ref(),
                Type::Primitive(
                    ast::PrimitiveType::U8 | ast::PrimitiveType::I8 | ast::PrimitiveType::Char
                )
            ) =>
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
        let Type::Named {
            path: exp_path,
            generics: exp_generics,
        } = expected?
        else {
            return None;
        };
        if exp_path.len() != 1 || exp_path[0] != enum_name {
            return None;
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
            (span.start, span.end),
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
            BitwiseAnd => "&",
            BitwiseOr => "|",
            BitwiseXor => "^",
            LeftShift => "<<",
            RightShift => ">>",
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
                    MethodCallStyle::Instance,
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
                } else if self.known_types.contains_key(&ident.name) {
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
        for module in &self.imported_modules {
            for template_src in &module.generic_templates {
                let file_id = crate::lexer::register_source(&module.source_path, template_src);
                if let Ok(tokens) = crate::lexer::lex_with_source(template_src, file_id) {
                    let mut parser =
                        crate::parser::Parser::new_with_source(tokens, module.source_path.clone());
                    let (prog, _) = parser.parse_program();
                    for item in prog.items {
                        if let ast::ItemKind::Impl(impl_item) = item.kind {
                            all_impls.push(impl_item);
                        }
                    }
                }
            }
        }

        for impl_item in &all_impls {
            let self_ty = Type::from_ast(&impl_item.self_type);
            let self_key = self.method_key(&self_ty);

            let mut impl_type_params = Vec::new();
            if let Some(generics) = &impl_item.generics {
                for param in &generics.params {
                    if let ast::GenericParam::Type(type_param) = param {
                        impl_type_params.push(type_param.name.name.clone());
                    }
                }
            } else if !impl_item.implicit_type_params.is_empty() {
                // Parser-recorded file-local parameters: decided when the
                // impl's own file was parsed, so a global type registered
                // later (e.g. a user `struct T`) cannot steal the name —
                // generic parameters shadow global types in their context.
                impl_type_params.extend(impl_item.implicit_type_params.iter().cloned());
            } else {
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
                                source_impl: impl_item.clone(),
                                source_method: (**func).clone(),
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
        let aliases: HashMap<String, ast::Type> = program
            .items
            .iter()
            .filter_map(|item| {
                if let ast::ItemKind::TypeAlias(alias) = &item.kind {
                    Some((alias.name.name.clone(), alias.type_def.clone()))
                } else {
                    None
                }
            })
            .collect();

        if aliases.is_empty() {
            return;
        }

        for item in &mut program.items {
            Self::resolve_type_aliases_in_item(item, &aliases);
        }
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
        // Resolve top-level alias
        if let ast::TypeKind::Named(named) = ty.kind.as_ref()
            && named.path.len() == 1
            && named.generics.is_none()
            && let Some(aliased) = aliases.get(&named.path[0].name)
        {
            *ty = aliased.clone();
            return;
        }
        // Recurse into child types
        match ty.kind.as_mut() {
            ast::TypeKind::Named(named) => {
                if let Some(generics) = &mut named.generics {
                    for g in generics.iter_mut() {
                        Self::resolve_type_aliases_in_type(g, aliases);
                    }
                }
            }
            ast::TypeKind::Reference(r) => {
                Self::resolve_type_aliases_in_type(&mut r.inner, aliases)
            }
            ast::TypeKind::Pointer(p) => Self::resolve_type_aliases_in_type(&mut p.inner, aliases),
            ast::TypeKind::Slice(s) => {
                Self::resolve_type_aliases_in_type(&mut s.element_type, aliases)
            }
            ast::TypeKind::Optional(inner) => Self::resolve_type_aliases_in_type(inner, aliases),
            ast::TypeKind::Function(f) => {
                for p in &mut f.parameters {
                    Self::resolve_type_aliases_in_type(p, aliases);
                }
                Self::resolve_type_aliases_in_type(&mut f.return_type, aliases);
            }
            ast::TypeKind::Tuple(items) => {
                for item in items.iter_mut() {
                    Self::resolve_type_aliases_in_type(item, aliases);
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

    fn error(&mut self, message: impl Into<String>, span: Span) {
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
    resolved_iter_types: &HashMap<(usize, usize), Box<ast::Type>>,
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
    rewrites: &HashMap<(usize, usize), BareConstructorRewrite>,
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
        ast::ExpressionKind::Block(block) => {
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
    rewrites: &HashMap<(usize, usize), BareConstructorRewrite>,
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
    rewrites: &HashMap<(usize, usize), BareConstructorRewrite>,
) {
    for stmt in &mut block.statements {
        rewrite_statement_bare_constructors(stmt, rewrites);
    }
}

fn rewrite_statement_bare_constructors(
    stmt: &mut ast::Statement,
    rewrites: &HashMap<(usize, usize), BareConstructorRewrite>,
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
    rewrites: &HashMap<(usize, usize), BareConstructorRewrite>,
) {
    let key = (expr.span.start, expr.span.end);
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
            _ => Vec::new(),
        };
        *expr.kind = ast::ExpressionKind::MethodCall {
            receiver: Box::new(receiver),
            method: ast::Identifier {
                name: rewrite.variant.clone(),
                span: expr.span,
            },
            arguments: args,
        };
        return;
    }
    match expr.kind.as_mut() {
        ast::ExpressionKind::Block(block) => {
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
    resolved_iter_types: &HashMap<(usize, usize), Box<ast::Type>>,
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
    resolved_iter_types: &HashMap<(usize, usize), Box<ast::Type>>,
) {
    for stmt in &mut block.statements {
        populate_statement_for_in_types(stmt, resolved_iter_types);
    }
}

fn populate_statement_for_in_types(
    stmt: &mut ast::Statement,
    resolved_iter_types: &HashMap<(usize, usize), Box<ast::Type>>,
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
    resolved_iter_types: &HashMap<(usize, usize), Box<ast::Type>>,
) {
    match expr.kind.as_mut() {
        ast::ExpressionKind::ForIn { iterator_type, .. } => {
            if let Some(iter_ty) = resolved_iter_types.get(&(expr.span.start, expr.span.end)) {
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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::Parser;
    use quickcheck::{QuickCheck, TestResult};

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    #[test]
    fn stdlib_source_files_parse_successfully() {
        let std_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("std");
        let mut files: Vec<_> = std::fs::read_dir(&std_dir)
            .expect("std dir not found")
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().is_some_and(|ext| ext == "ag"))
            .map(|e| e.path())
            .collect();
        // Also add files from std/mem/
        let mem_dir = std_dir.join("mem");
        if mem_dir.exists()
            && let Ok(entries) = std::fs::read_dir(&mem_dir)
        {
            for entry in entries.flatten() {
                if entry.path().extension().is_some_and(|ext| ext == "ag") {
                    files.push(entry.path());
                }
            }
        }
        files.sort();
        for path in &files {
            let src = std::fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("failed to read {:?}: {e}", path));
            let tokens = lex(&src).unwrap_or_else(|e| panic!("lex failed for {:?}: {e:?}", path));
            let mut parser = Parser::new(tokens);
            let (_program, errors) = parser.parse_program();
            assert!(errors.is_empty(), "parse errors in {:?}: {errors:?}", path);
        }
    }

    #[test]
    fn type_checks_imported_generic_alloc_call() {
        let artifact = ModuleArtifact {
            module_name: "mem".to_string(),
            module_path: "std.mem".to_string(),
            source_path: "std/mem.ag".to_string(),
            source_hash_fnv1a64: 0,
            compiler_version: "test".to_string(),
            target_triple: "unknown".to_string(),
            code_artifacts: crate::module_artifact::ModuleCodeArtifacts {
                has_static_library: true,
                has_shared_library: false,
            },
            module_deps: Vec::new(),
            transitive_deps: Vec::new(),
            exports: vec![crate::module_artifact::ModuleExport {
                kind: crate::module_artifact::ExportKind::Function,
                name: "alloc".to_string(),
                signature: "fn() -> *mut T".to_string(),
                type_params: vec!["T".to_string()],
                link_name: Some("alloc".to_string()),
                abi: Some(crate::module_artifact::ModuleAbi::Silver),
                is_variadic: false,
                type_key: None,
                fields: Vec::new(),
                layout: None,
                enum_backing_type: None,
                enum_variants: Vec::new(),
                trait_items: Vec::new(),
                const_value: None,
                is_mutable: false,
            }],
            native_libs: Vec::new(),
            native_lib_paths: Vec::new(),
            generic_templates: Vec::new(),
            artifact_path: None,
        };

        let program = parse("i32 main() { alloc<i32>(); return 0; }");
        let (errors, _) = TypeChecker::new()
            .with_imported_modules(&[artifact])
            .check_program(&program);

        let has_alloc_error = errors.iter().any(|e| {
            e.message.contains("no matching overload")
                || e.message.contains("type count mismatch")
                || e.message.contains("unknown function")
        });
        assert!(!has_alloc_error, "unexpected error for alloc: {:?}", errors);
    }

    #[test]
    fn type_checks_local_assignment() {
        let program = parse("i32 main() { i32 x = 1; return x; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn type_checks_ternary_with_matching_branches() {
        let program =
            parse("i32 main() { i64 a = 5; i64 b = 3; i64 mx = a > b ? a : b; return (i32)mx; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn reports_ternary_branch_mismatch() {
        let program =
            parse("i32 main() { i64 a = 1; f64 b = 2.0; bool c = true; c ? a : b; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors.iter().any(|e| e
                .message
                .contains("ternary branches must have the same type")),
            "expected branch mismatch error, got: {errors:?}"
        );
    }

    #[test]
    fn reports_ternary_non_bool_condition() {
        let program = parse("i32 main() { i64 x = 5 ? 1 : 2; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("ternary condition must be bool")),
            "expected condition error, got: {errors:?}"
        );
    }

    #[test]
    fn enforces_trait_bounds_on_function_calls() {
        let program = parse(
            "trait Copy {} impl Copy for i32 {} T foo<T: Copy>(T x) { return x; } i32 main() { return foo(1); }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn reports_missing_trait_bound_impl() {
        let program =
            parse("trait Copy {} T foo<T: Copy>(T x) { return x; } i32 main() { return foo(1); }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected errors");
    }

    #[test]
    fn detects_assignment_mismatch() {
        let program = parse("struct Foo { i32 x; } i32 main() { Foo f; i32 x = f; return x; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
    }

    #[test]
    fn checks_return_type() {
        let program = parse("struct Foo { i32 x; } i32 main() { Foo f; return f; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
    }

    #[test]
    fn uses_context_for_integer_literals() {
        let program = parse("i64 main() { i64 x = 1; return 1; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn allows_implicit_numeric_cast_on_assignment() {
        let program = parse("i32 main() { i32 a = 1; i64 b = a; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn allows_implicit_numeric_cast_on_return() {
        let program = parse("i64 main() { i32 a = 1; return a; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn allows_implicit_bool_cast_to_int() {
        let program = parse("i32 main() { bool b = true; i32 x = b; return x; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn type_checks_enum_member_access_and_backing_casts() {
        let program = parse(
            "enum Color { Red; Blue = 255; } Color id() { return Color.Blue; } i32 main() { Color x = Color.Red; return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn type_checks_signed_enum_members() {
        let program = parse(
            "enum Status { Ok; Err = -1; } Status id() { return Status.Err; } i32 main() { Status x = Status.Ok; return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn rejects_instance_style_enum_member_access() {
        let program = parse(
            "enum Status { Ok; Err = -1; } i32 main() { Status x = Status.Ok; return x.Ok; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors.iter().any(|error| error
                .message
                .contains("enum members must be accessed through the enum type name")),
            "expected enum member access error, got {errors:?}"
        );
    }

    #[test]
    fn numeric_binary_promotes() {
        let program = parse("i32 main() { i32 a = 1; i64 b = 2; i64 c = a + b; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn address_of_produces_pointer_type_for_bindings() {
        let program = parse("i32 main() { i32 x = 1; i32* p = &x; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn address_of_pointer_passes_to_pointer_parameters() {
        let program = parse(
            "i32 read_ptr(i32* p) { return p[0]; } i32 main() { i32 x = 7; return read_ptr(&x); }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn address_of_error_reports_pointer_not_reference() {
        let program = parse("i32 main() { i32 x = 1; i32 y = &x; return y; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
        assert!(
            errors.iter().any(|error| error.message.contains("i32*")),
            "expected pointer type in diagnostics: {errors:?}"
        );
        assert!(
            !errors.iter().any(|error| error.message.contains("&")),
            "did not expect reference type in diagnostics: {errors:?}"
        );
    }

    #[test]
    fn allows_operator_overload_add() {
        let program = parse(
            "struct Vec2 { i32 x; } impl Vec2 { Vec2 __add(Vec2 self, Vec2 other) { return self; } } i32 main() { Vec2 a; Vec2 b; Vec2 c = a + b; return 0; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn resolves_function_overload() {
        let program = parse(
            "i32 add(i32 a, i32 b) { return a; } f64 add(f64 a, f64 b) { return a; } i32 main() { i32 x = add(1, 2); f64 y = add(1.0, 2.0); return 0; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn reports_ambiguous_overload() {
        let program = parse(
            "i32 add(i32 a, i32 b) { return a; } i64 add(i64 a, i64 b) { return a; } i32 main() { i32 x = add(1.0, 2.0); return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
    }

    #[test]
    fn resolves_generic_function() {
        let program =
            parse("T id<T>(T value) { return value; } i32 main() { i32 x = id(1); return x; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn resolves_method_overload() {
        let program = parse(
            "impl i32 { i32 add(i32 self, i32 other) { return self; } f64 add(f64 self, f64 other) { return self; } } i32 main() { i32 x = 1; i32 y = x.add(2); return y; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn resolves_pointer_receiver_method_call() {
        let program = parse(
            "struct Counter { i32 value; } impl Counter { i32 read(Counter* self) { return self.value; } } i32 main() { Counter c = { .value = 7 }; Counter* p = &c; i32 x = p.read(); return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn resolves_static_method_call_on_type_name() {
        let program = parse(
            "struct Counter { i32 value; } impl Counter { i32 one() { return 1; } } i32 main() { i32 x = Counter.one(); return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn resolves_static_method_call_on_generic_type_name() {
        let program = parse(
            "struct Box<T> { T value; } impl Box<T> { Box<T> none() { Box<T> result; return result; } } i32 main() { Box<i32> x = Box<i32>.none(); return 0; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn resolves_static_method_call_on_optional_keyword_type_name() {
        let program = parse(
            "struct Wrapper<T> { T value; } impl Wrapper<T> { Wrapper<T> none() { Wrapper<T> result; return result; } } i32 main() { Wrapper<i32> x = Wrapper<i32>.none(); return 0; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn resolves_struct_field_access() {
        let program = parse(
            "struct Point { i32 x; i32 y; } i32 main() { Point p = { .x = 1, .y = 2 }; i32 a = p.x; return a; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn resolves_generic_struct_field_access() {
        let program = parse(
            "struct Box<T> { T value; } i32 main() { Box<i32> b = { .value = 7 }; i32 x = b.value; return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn rejects_static_method_call_on_instance() {
        let program = parse(
            "struct Counter { i32 value; } impl Counter { i32 one() { return 1; } } i32 main() { Counter c; i32 x = c.one(); return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
    }

    #[test]
    fn rejects_instance_method_call_on_type_name() {
        let program = parse(
            "struct Counter { i32 value; } impl Counter { i32 get(Counter self) { return 1; } } i32 main() { i32 x = Counter.get(); return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
    }

    #[test]
    fn prop_overload_prefers_exact_match() {
        fn property(n: i32) -> TestResult {
            let program = parse(&format!(
                "i32 add(i32 a, i32 b) {{ return a; }} i64 add(i64 a, i64 b) {{ return a; }} i32 main() {{ i32 x = add({n}, {n}); return x; }}",
            ));
            let (errors, _) = TypeChecker::new().check_program(&program);
            if errors.is_empty() {
                TestResult::passed()
            } else {
                TestResult::failed()
            }
        }

        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(i32) -> TestResult);
    }

    #[test]
    fn prop_generic_overload_infers_type_param() {
        fn property(n: i32) -> TestResult {
            let program = parse(&format!(
                "T id<T>(T value) {{ return value; }} i32 main() {{ i32 x = id({n}); return x; }}",
            ));
            let (errors, _) = TypeChecker::new().check_program(&program);
            if errors.is_empty() {
                TestResult::passed()
            } else {
                TestResult::failed()
            }
        }

        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(i32) -> TestResult);
    }

    #[test]
    fn prop_method_overload_prefers_exact_match() {
        fn property(n: i32) -> TestResult {
            let program = parse(&format!(
                "impl i32 {{ i32 add(i32 self, i32 other) {{ return self; }} i64 add(i64 self, i64 other) {{ return self; }} }} i32 main() {{ i32 x = 1; i32 y = x.add({n}); return y; }}",
            ));
            let (errors, _) = TypeChecker::new().check_program(&program);
            if errors.is_empty() {
                TestResult::passed()
            } else {
                TestResult::failed()
            }
        }

        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(i32) -> TestResult);
    }

    #[test]
    fn reports_no_matching_overload() {
        let program = parse(
            "struct Foo {} i32 add(i32 a, i32 b) { return a; } i32 main() { Foo f; i32 x = add(1, f); return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors, got: {errors:?}");
        assert!(
            errors[0].message.contains("no matching overload"),
            "expected 'no matching overload' error, got: {}",
            errors[0].message
        );
    }

    #[test]
    fn matches_concrete_overload_with_implicit_cast() {
        let program = parse(
            "i32 add(i32 a, i32 b) { return a; } i32 main() { f64 x = 1.0; f64 y = 2.0; i32 z = add(x, y); return z; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn matches_concrete_f32_overload_with_f64_arg() {
        let program =
            parse("void take_f32(f32 x) {} i32 main() { f64 a = 3.0; take_f32(a); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn allows_explicit_void_return_type() {
        let program = parse("void sink(i32 x) { return; } i32 main() { sink(1); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn allows_void_pointer_types() {
        let program = parse(
            "void consume(void* p) { return; } i32 main() { i32 x = 1; void* p = &x; consume(p); return 0; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn rejects_plain_void_value_bindings() {
        let program = parse("i32 main() { void x; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("plain `void`")),
            "expected plain void diagnostic, got {errors:?}"
        );
    }

    #[test]
    fn rejects_returning_value_from_void_function() {
        let program = parse("void bad() { return 1; } i32 main() { bad(); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
        assert!(
            errors.iter().any(|error| error
                .message
                .contains("void function cannot return a value")),
            "expected void return diagnostic, got {errors:?}"
        );
    }

    #[test]
    fn rejects_packed_and_align_on_struct() {
        let program = parse("#[packed, align(4)] struct Bad { i32 x; } i32 main() { return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
    }

    #[test]
    fn rejects_unknown_struct_attribute() {
        let program = parse("#[mystery] struct Bad { i32 x; } i32 main() { return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
    }

    #[test]
    fn rejects_invalid_align_attribute() {
        let program = parse("#[align(3)] struct Bad { i32 x; } i32 main() { return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
    }

    #[test]
    fn accepts_link_attribute_in_global_scope() {
        let program = parse("#[link(m)] struct Good { i32 x; } i32 main() { return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "unexpected type errors: {errors:?}");
    }

    #[test]
    fn rejects_invalid_link_attribute() {
        let program = parse("#[link(1)] i32 main() { return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("link expects a library name")),
            "expected invalid link attribute error, got {errors:?}"
        );
    }

    #[test]
    fn expr_types_is_populated_for_literals() {
        let program = parse("i32 main() { return 1; }");
        let mut tc = TypeChecker::new();
        let mut table = CompilerSymbolTable::new();
        let (_errors, _) = tc.check_program_with_table(&program, &mut table);
        let types = std::mem::take(&mut tc.expr_types);
        assert!(
            !types.is_empty(),
            "expected at least one expression type, got none"
        );
        for ((start, end), ty) in &types {
            eprintln!("  span({start},{end}) → {ty}");
        }
    }

    #[test]
    fn rejects_move_of_static_local() {
        let program = parse("i32 main() { static i32 x = 1; i32 y = move x; return y; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("cannot move out of static variable")),
            "expected move-of-static error, got {errors:?}"
        );
    }

    #[test]
    fn rejects_address_of_volatile_local() {
        let program = parse("i32 main() { volatile i32 v = 1; i32* p = &v; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors.iter().any(|error| error
                .message
                .contains("cannot take the address of volatile")),
            "expected address-of-volatile error, got {errors:?}"
        );
    }

    #[test]
    fn rejects_address_of_volatile_global() {
        let program = parse("volatile i32 g = 0; i32 main() { i32* p = &g; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors.iter().any(|error| error
                .message
                .contains("cannot take the address of volatile")),
            "expected address-of-volatile error, got {errors:?}"
        );
    }

    #[test]
    fn shadowed_non_volatile_local_allows_address_of() {
        let program =
            parse("volatile i32 g = 0; i32 main() { i32 g = 5; i32* p = &g; return *p; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn accepts_volatile_array_local() {
        let program = parse("i32 main() { volatile i32 arr[10]; arr[0] = 1; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn accepts_volatile_array_global() {
        let program = parse("volatile i32 arr[10]; i32 main() { return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn rejects_address_of_volatile_array_element() {
        let program = parse("i32 main() { volatile i32 arr[10]; i32* p = &arr[0]; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors.iter().any(|error| error
                .message
                .contains("cannot take the address of volatile variable")),
            "expected address-of-volatile error, got {errors:?}"
        );
    }

    #[test]
    fn generic_method_infers_type_params_from_expected_type() {
        // A bare generic receiver with an LHS annotation must fill the type
        // params the arguments cannot provide (`E` in `Result.ok(x)` comes
        // only from the expected type).
        let program = parse(
            "enum Pair<T, E> { P(T, E); } impl Pair<T, E> { Pair<T, E> mk(T a, E b) { return Pair.P(move a, move b); } } \
             struct Wrapper<T> { T v; } impl Wrapper<T> { Wrapper<T> wrap(T v) { Wrapper<T> w; w.v = move v; return move w; } } \
             i32 main() { Pair<i32, str> p = Pair.mk(5, \"hi\"); Wrapper<i64> w = Wrapper.wrap((i64)3); return 0; }",
        );
        let mut checker = TypeChecker::new();
        let mut table = crate::symbol_table::CompilerSymbolTable::new();
        let (errors, _) = checker.check_program_with_table(&program, &mut table);
        assert!(
            errors.is_empty(),
            "expected-type generic inference should type-check: {errors:?}"
        );
    }

    #[test]
    fn generic_method_without_expected_type_still_errors() {
        // Without an expected type, a bare generic receiver whose type params
        // cannot be fully inferred from the arguments is an error, not a
        // silent partial instantiation (`E` here appears only in the return
        // type, never in `mkT`'s parameters).
        let program = parse(
            "enum Pair<T, E> { P(T, E); } impl Pair<T, E> { Pair<T, E> mkT(T a) { Pair<T, E> p; return Pair.P(move a, move a); } } \
             i32 main() { i32 x = Pair.mkT(5); return 0; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            !errors.is_empty(),
            "expected an error for uninferable generics, got {errors:?}"
        );
    }

    #[test]
    fn bare_enum_constructors_record_rewrites() {
        // Bare `Some`/`Ok` with expected-type inference must type-check and
        // record a rewrite so the post-typeck pass can lower them.
        let program = parse(
            "enum Optional<T> { None; Some(T); } enum Result<T, E> { Ok(T); Err(E); } \
             i32 main() { Optional<i32> a = Some(5); Optional<i32> b = None; \
             Result<i32, str> r = Ok(7); return 0; }",
        );
        let mut checker = TypeChecker::new();
        let mut table = crate::symbol_table::CompilerSymbolTable::new();
        let (errors, _) = checker.check_program_with_table(&program, &mut table);
        assert!(
            errors.is_empty(),
            "bare constructors should type-check: {errors:?}"
        );
        let rewrites = checker.take_bare_constructors();
        assert!(!rewrites.is_empty(), "expected bare-constructor rewrites");
        assert!(
            rewrites
                .values()
                .any(|r| r.enum_name == "Optional" && r.variant == "Some"),
            "expected an Optional::Some rewrite, got {rewrites:?}"
        );
        assert!(
            rewrites
                .values()
                .any(|r| r.enum_name == "Result" && r.variant == "Ok"),
            "expected a Result::Ok rewrite, got {rewrites:?}"
        );
    }

    #[test]
    fn typo_suggestion_for_unknown_identifier() {
        let program = parse("i32 main() { i32 my_counter = 10; return my_countr; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("did you mean 'my_counter'?")),
            "expected typo suggestion for identifier, got {errors:?}"
        );
    }

    #[test]
    fn typo_suggestion_for_unknown_field() {
        let program = parse("struct Point { i64 x; i64 y; } i32 main() { Point p; return p.xx; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("did you mean 'x'?")),
            "expected typo suggestion for field, got {errors:?}"
        );
    }

    #[test]
    fn typo_suggestion_for_unknown_method() {
        let program = parse(
            "struct Counter { i64 val; } \
             impl Counter { i64 get_count(Counter* self) { return self.val; } } \
             i32 main() { Counter c; return c.get_coun(); }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("did you mean 'get_count'?")),
            "expected typo suggestion for method, got {errors:?}"
        );
    }
    #[test]
    fn typo_suggestion_for_unknown_macro() {
        let program = parse("i32 main() { @printlln(\"hello\"); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("did you mean 'println'?")),
            "expected typo suggestion for macro, got {errors:?}"
        );
    }

    #[test]
    fn type_checks_unwrap_or_on_optional() {
        let program = parse(
            "enum Optional<T> { None; Some(T); } \
             i32 main() { Optional<i32> opt = Some(42); i32 val = opt ? 10; return val; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "expected no errors, got {errors:?}");
    }

    #[test]
    fn type_checks_unwrap_or_on_result() {
        let program = parse(
            "enum Result<T, E> { Ok(T); Err(E); } \
             i32 main() { Result<i32, str> res = Ok(42); i32 val = res ? 10; return val; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "expected no errors, got {errors:?}");
    }

    #[test]
    fn type_checks_unwrap_or_on_pointer() {
        let program = parse(
            "i32 main() { i32 x = 42; i32* p = &x; i32 y = 10; i32* r = p ? &y; return *r; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "expected no errors, got {errors:?}");
    }

    #[test]
    fn rejects_unwrap_or_on_invalid_lhs() {
        let program = parse("i32 main() { i32 x = 42; i32 val = x ? 10; return val; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors.iter().any(|e| e
                .message
                .contains("unwrap-or operator '?' requires Optional, Result, or pointer")),
            "expected unwrap-or invalid lhs error, got {errors:?}"
        );
    }

    #[test]
    fn rejects_unwrap_or_on_type_mismatch() {
        let program = parse(
            "enum Optional<T> { None; Some(T); } \
             i32 main() { Optional<i32> opt = Some(42); i32 val = opt ? \"fallback\"; return val; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("unwrap-or fallback type mismatch")),
            "expected fallback type mismatch error, got {errors:?}"
        );
    }

    #[test]
    fn bare_constructor_without_expected_type_errors() {
        // Without an expected Optional/Result type, bare `Some` is unknown.
        let program = parse("i32 main() { i32 x = Some(5); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("unknown identifier 'Some'")),
            "expected unknown-identifier error, got {errors:?}"
        );
    }

    #[test]
    fn generic_enum_construction_and_match_typecheck() {
        // Generic payload enums: construction via `Box<i32>.Full(...)` and
        // match arms binding the payload with the concrete type.
        let program = parse(
            "enum Box2<T> { Full(T); Empty; } i32 main() { \
             Box2<i32> x = Box2<i32>.Full(42); \
             i32 v = match x { Full(val) : val, Empty : 0 }; \
             return v - 42; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors.is_empty(),
            "generic enum should type-check: {errors:?}"
        );
    }

    #[test]
    fn auto_import_injection_adds_optional_and_result() {
        // A program using bare constructors with no explicit import should get
        // std.optional / std.result injected by the import hook (verified by
        // the import_hook tests + end-to-end driver runs).
        let source =
            "i32 main() { Optional<i32> a = Some(1); Result<i32, str> r = Ok(2); return 0; }";
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (mut program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap();
        let mut loader = crate::module_loader::ModuleLoader::new();
        loader.add_search_dir(repo_root.to_path_buf());
        let hook = crate::parser::import_hook::FileImportResolverHook::new(&loader);
        // Lowering must succeed: bare constructors are recognized and the
        // std modules are inlined (injected imports are consumed by lowering).
        let result = hook.lower_program_imports(&mut program, None, None);
        assert!(result.is_ok(), "import lowering failed: {result:?}");
    }

    #[test]
    fn operator_guard_origin_note_points_to_definition() {
        // Instantiating a generic that uses `+` on `T` with a type lacking an
        // overload must report the call-site error AND a definition-site note.
        let program = parse(
            "struct P { i32 x; } T add<T>(T a, T b) { return a + b; } \
             i32 main() { P p; P q; P r = add(p, q); return 0; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("no overload exists")),
            "expected missing-overload error, got: {errors:?}"
        );
        assert!(
            errors
                .iter()
                .any(|e| e.message == msg::implicit_guard_origin("+")),
            "expected operator-origin note, got: {errors:?}"
        );
    }

    #[test]
    fn method_guard_origin_note_points_to_definition() {
        let program = parse(
            "T poke<T>(T x) { x.poke(); return x; } \
             i32 main() { return poke(1); }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("method requirement for 'poke'")),
            "expected missing-method-guard error, got: {errors:?}"
        );
        assert!(
            errors
                .iter()
                .any(|e| e.message == msg::implicit_method_guard_origin("poke")),
            "expected method-origin note, got: {errors:?}"
        );
    }

    #[test]
    fn user_struct_named_t_does_not_break_generic_impls() {
        // Cross-file collision at the typeck level: when a generic impl's
        // parameters were recorded at parse time (file-local knowledge), a
        // globally registered `struct T` must not turn `T` concrete.
        // Simulate by parsing the impl from a file WITHOUT `struct T`, then
        // registering the struct — the merged-program shape after lowering.
        let impl_source = "enum Opt<T> { None; Some(T); } \
             impl Opt<T> { bool is_none(Opt<T>* self) { return false; } }";
        let tokens = lex(impl_source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (mut opt_program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");

        let collision_source = "struct T { i64 v; } i32 main() { T mine; mine.v = 1; return 0; }";
        let mut full = parse(collision_source);
        full.items.splice(0..0, opt_program.items.drain(..));

        let (errors, _) = TypeChecker::new().check_program(&full);
        assert!(
            errors.is_empty(),
            "global struct T must not break the generic impl: {errors:?}"
        );
    }

    #[test]
    fn impl_implicit_params_recorded_at_parse() {
        // The parser records implicit params per impl from FILE-LOCAL type
        // knowledge: with Result declared locally, T and E are params.
        let tokens = lex("enum Result<T, E> { Ok(T); Err(E); } \
             impl Result<T, E> { i32 f(T x) { return 0; } }")
        .expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        let ast::ItemKind::Impl(impl_item) = &program.items[1].kind else {
            panic!("expected impl item");
        };
        assert_eq!(impl_item.implicit_type_params, vec!["T", "E"]);
    }

    #[test]
    fn file_local_type_wins_over_param_in_same_file_impl() {
        // A type declared in the SAME file as the impl is concrete, not a
        // parameter: `impl Wrapper<MyStruct>` records no param.
        let tokens = lex("struct MyStruct { i64 v; } struct Wrapper<T> { T inner; } \
             impl Wrapper<MyStruct> { i32 f(Wrapper<MyStruct>* self) { return 0; } }")
        .expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        let ast::ItemKind::Impl(impl_item) = &program.items[2].kind else {
            panic!("expected impl item");
        };
        assert!(
            !impl_item
                .implicit_type_params
                .contains(&"MyStruct".to_string()),
            "file-local concrete type must not become a parameter"
        );
    }

    #[test]
    fn lifetime_only_generic_struct_needs_no_type_args() {
        // Regression (borrow_conflict_test): lifetime parameters occupy no
        // slot in the type-argument list, so a `struct SV<'a>` is used plain
        // — it must not trip "missing required type argument".
        let program = parse(
            "struct SV<'a> { &'a i64 data; i64 len; } \
             i32 main() { SV view; view.len = 1; return view.len; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("missing required type argument")),
            "lifetime params must not require type arguments: {errors:?}"
        );
    }

    #[test]
    fn satisfied_operator_guard_produces_no_errors() {
        // A concrete instantiation where the operator is builtin-supported
        // must not trip the guard or emit any diagnostics.
        let program = parse(
            "T add<T>(T a, T b) { return a + b; } \
             i32 main() { return add(1, 2); }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn infers_let_binding_from_initializer() {
        // `let y = identity(5);` binds without annotation; the generic call
        // instantiates T=i32 and the inferred type propagates.
        let program = parse(
            "T identity<T>(T x) { return x; } \
             i32 main() { let y = identity(5); return (i32)y; }",
        );
        let (errors, requests) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        assert!(
            !requests.is_empty(),
            "generic call should produce a monomorph request"
        );
    }

    #[test]
    fn inferred_let_materializes_annotation() {
        // After typeck, populate_inferred_let_types rewrites the binding to
        // carry its inferred annotation for downstream passes.
        let program = parse("i64 make() { return 1; } i32 main() { let n = make(); return 0; }");
        let mut checker = TypeChecker::new();
        let mut table = crate::symbol_table::CompilerSymbolTable::new();
        let (errors, _) = checker.check_program_with_table(&program, &mut table);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        let inferred = checker.take_inferred_lets();
        assert_eq!(inferred.len(), 1, "one inferred binding expected");
        let (_, (ty, _)) = inferred.iter().next().unwrap();
        assert_eq!(ty.to_string(), "i64", "inferred type from make()");
    }

    #[test]
    fn inferred_let_without_initializer_is_error() {
        let program = parse("i32 main() { let x; return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(
            errors
                .iter()
                .any(|e| e.message == msg::inferred_let_needs_initializer()),
            "expected missing-initializer error, got: {errors:?}"
        );
    }

    #[test]
    fn inferred_let_rejects_void_initializer() {
        let program = parse("void v() {} i32 main() { let z = v(); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected void-binding error");
    }

    #[test]
    fn parses_let_keyword_as_inferred_binding() {
        // The lexer maps `let` to Token::Let and the statement reduction
        // produces an un-annotated LetStatement.
        let tokens = lex("i32 main() { let value = 3; }").expect("lex failed");
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, crate::lexer::Token::Let)),
            "`let` should lex as a keyword token"
        );
    }
}
