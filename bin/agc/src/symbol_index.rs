//! Shared symbol/occurrence index for tooling (the language server).
//!
//! A single AST walk over an import-lowered program produces every
//! definition (with span, signature, doc comment and parameters) and every
//! scope-resolved identifier occurrence, so tools do not maintain their
//! own parallel walk of the AST.
//!
//! A single AST walk produces, from one parse + type-check pass:
//! - `Symbol`s: every definition (functions, methods, structs, enums,
//!   traits, fields, variants, globals, locals, parameters, ...) with its
//!   name span, formatted signature, doc comment and parameter list.
//! - `Occurrence`s: every identifier occurrence (definitions and uses)
//!   with scope-aware resolution to a symbol where possible. These drive
//!   semantic tokens, find-references and rename.
//! - Import paths for `import std.io;` style completion.

use crate::lexer::{LexToken, Span};
use crate::parser::ast::{self, ItemKind};
use rustc_hash::FxHashMap as HashMap;

use crate::format::*;

pub type SymbolId = usize;

/// Expression span (start, end) → formatted type string, from the type
/// checker.
pub type ExprTypeMap = rustc_hash::FxHashMap<(usize, usize), String>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Method,
    Struct,
    Enum,
    Trait,
    Global,
    Const,
    TypeAlias,
    Macro,
    Field,
    Variant,
    Parameter,
    Local,
    TypeParam,
    ExternFunction,
    ExternVariable,
}

#[derive(Debug, Clone)]
pub struct ParamInfo {
    pub name: String,
    pub type_str: String,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    /// Span of the identifier that names the symbol.
    pub span: Span,
    pub doc: Option<String>,
    /// Formatted signature/declaration for hover, completion and
    /// signature help.
    pub signature: String,
    /// Parameters, when this symbol is callable (signature help).
    pub parameters: Vec<ParamInfo>,
    /// `"StructName::"` / `"module::"` prefix for qualified completion
    /// labels (methods, fields, variants).
    pub qualifier: Option<String>,
    pub is_mutable: bool,
    pub is_static: bool,
    /// Inferred type for an unannotated local binding, when known.
    pub inferred_type: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OccurrenceKind {
    Function,
    Method,
    Struct,
    Enum,
    Trait,
    TypeAlias,
    Macro,
    Variable,
    Const,
    Property,
    EnumMember,
    TypeParam,
    Parameter,
    Type,
    Namespace,
}

#[derive(Debug, Clone)]
pub struct Occurrence {
    /// Resolved symbol, when the identifier could be tied to a definition.
    pub symbol: Option<SymbolId>,
    pub kind: OccurrenceKind,
    pub span: Span,
    pub is_definition: bool,
    pub readonly: bool,
    pub is_static: bool,
    pub documented: bool,
}

/// A function/method call site, used for parameter-name inlay hints.
#[derive(Debug, Clone)]
pub struct CallSite {
    /// Byte offset of the `(` that opens the argument list.
    pub open_paren: usize,
    /// (start, end) byte spans of each argument expression.
    pub args: Vec<(usize, usize)>,
    /// Resolved callee symbol (function or method), when known.
    pub callee: Option<SymbolId>,
}

#[derive(Debug, Clone)]
pub struct SymbolIndex {
    pub text: String,
    pub symbols: Vec<Symbol>,
    pub occurrences: Vec<Occurrence>,
    /// Call sites in the buffer (parameter-name inlay hints).
    pub call_sites: Vec<CallSite>,
    /// Expression span (start, end) → formatted type, from the type checker.
    pub expr_types: ExprTypeMap,
    /// Fully qualified module paths seen in `import` statements.
    pub import_paths: Vec<Vec<String>>,
    /// Span-annotated tokens of the current buffer (keywords, operators,
    /// comments, literals) for semantic token emission.
    pub tokens: Vec<LexToken>,
    /// Snapshot of `file id → (path, text)` for files whose definitions
    /// appear in `symbols` (inlined imports). The source registry is
    /// thread-local, so handlers must not query it; they use this map for
    /// cross-file locations.
    pub foreign_files: HashMap<u32, (String, String)>,
}

/// Run the AST walk over `program` (already import-lowered) and produce the
/// analysis. Occurrences whose spans come from other files (inlined imported
/// programs carry distinct `Span.file` ids) are dropped; their symbols remain
/// available for completion/hover.
pub fn analyze(
    program: &ast::Program,
    text: &str,
    tokens: &[LexToken],
    expr_types: ExprTypeMap,
    buffer_file: u32,
) -> SymbolIndex {
    // Precompute struct sizes for hover (recursive field sizes need the full
    // struct map first).
    let mut struct_map: HashMap<String, usize> = HashMap::default();
    for item in &program.items {
        if let ItemKind::Struct(s) = &item.kind {
            let packed = is_repr_packed(&item.attributes);
            if let Some(size) = compute_struct_size(s, &struct_map, packed) {
                struct_map.insert(s.name.name.clone(), size);
            }
        }
    }
    let mut walker = Walker {
        program,
        text_len: text.len(),
        expr_types: expr_types.clone(),
        buffer_file,
        symbols: Vec::new(),
        occurrences: Vec::new(),
        call_sites: Vec::new(),
        top_level: HashMap::default(),
        struct_children: HashMap::default(),
        locals: Vec::new(),
        import_paths: Vec::new(),
        struct_map,
    };
    for item in &program.items {
        walker.walk_item(item);
    }
    let current_tokens: Vec<LexToken> = tokens
        .iter()
        .filter(|t| walker.in_buffer(&t.span))
        .cloned()
        .collect();
    // Snapshot foreign file texts (imported definitions may be jumped to).
    let mut foreign_files: HashMap<u32, (String, String)> = HashMap::default();
    for symbol in &walker.symbols {
        let file = symbol.span.file;
        if file == 0 || file == buffer_file || foreign_files.contains_key(&file) {
            continue;
        }
        if let Some(source) = crate::lexer::source_file(file) {
            foreign_files.insert(file, (source.path, source.text));
        }
    }
    SymbolIndex {
        text: text.to_string(),
        symbols: walker.symbols,
        occurrences: walker.occurrences,
        call_sites: walker.call_sites,
        expr_types,
        import_paths: walker.import_paths,
        tokens: current_tokens,
        foreign_files,
    }
}

/// True for builtin primitive type names (i8..u128, f32..f80, bool, ...).
pub fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "i8" | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "f32"
            | "f64"
            | "f80"
            | "c32"
            | "c64"
            | "c80"
            | "bool"
            | "str"
            | "char"
            | "void"
    )
}

struct Walker<'a> {
    program: &'a ast::Program,
    text_len: usize,
    expr_types: ExprTypeMap,
    buffer_file: u32,
    symbols: Vec<Symbol>,
    occurrences: Vec<Occurrence>,
    call_sites: Vec<CallSite>,
    top_level: HashMap<String, SymbolId>,
    /// container name (struct/enum name) → field/method/variant symbols.
    struct_children: HashMap<String, Vec<SymbolId>>,
    /// Lexical scope stack of local name → symbol.
    locals: Vec<HashMap<String, SymbolId>>,
    import_paths: Vec<Vec<String>>,
    struct_map: HashMap<String, usize>,
}

impl Walker<'_> {
    /// True when the span belongs to the current buffer (exact file id, or a
    /// synthetic span within the buffer's byte range).
    fn in_buffer(&self, span: &Span) -> bool {
        if span.file == self.buffer_file {
            return true;
        }
        span.file == 0 && span.start < self.text_len && span.end <= self.text_len
    }
    #[expect(
        clippy::too_many_arguments,
        reason = "symbol registry context threading"
    )]
    fn add_symbol(
        &mut self,
        name: &str,
        kind: SymbolKind,
        span: Span,
        doc: Option<String>,
        signature: String,
        parameters: Vec<ParamInfo>,
        qualifier: Option<String>,
        is_mutable: bool,
        is_static: bool,
        container: Option<&str>,
        register_top_level: bool,
    ) -> SymbolId {
        let id = self.symbols.len();
        let documented = doc.is_some();
        self.symbols.push(Symbol {
            name: name.to_string(),
            kind,
            span,
            doc,
            signature,
            parameters,
            qualifier,
            is_mutable,
            is_static,
            inferred_type: None,
        });
        if let Some(container) = container {
            self.struct_children
                .entry(container.to_string())
                .or_default()
                .push(id);
        }
        if register_top_level {
            self.top_level.insert(name.to_string(), id);
        }
        self.emit(
            Some(id),
            occurrence_kind_for(kind),
            &span,
            true,
            !is_mutable && matches!(kind, SymbolKind::Const | SymbolKind::Global),
            is_static,
            documented,
        );
        id
    }

    /// Push an occurrence if its span lies within the current buffer.
    #[expect(
        clippy::too_many_arguments,
        reason = "occurrence emission context threading"
    )]
    fn emit(
        &mut self,
        symbol: Option<SymbolId>,
        kind: OccurrenceKind,
        span: &Span,
        is_definition: bool,
        readonly: bool,
        is_static: bool,
        documented: bool,
    ) {
        if !self.in_buffer(span) {
            return;
        }
        self.occurrences.push(Occurrence {
            symbol,
            kind,
            span: *span,
            is_definition,
            readonly,
            is_static,
            documented,
        });
    }

    fn declare_local(&mut self, name: &str, span: Span, kind: SymbolKind) -> SymbolId {
        let id = self.add_symbol(
            name,
            kind,
            span,
            None,
            name.to_string(),
            Vec::new(),
            None,
            false,
            false,
            None,
            false,
        );
        if let Some(scope) = self.locals.last_mut() {
            scope.insert(name.to_string(), id);
        }
        id
    }

    fn resolve(&self, name: &str) -> Option<SymbolId> {
        for scope in self.locals.iter().rev() {
            if let Some(id) = scope.get(name) {
                return Some(*id);
            }
        }
        self.top_level.get(name).copied()
    }

    /// The symbol resolved by the occurrence that starts at `byte`.
    fn occurrence_symbol_at(&self, byte: usize) -> Option<SymbolId> {
        self.occurrences
            .iter()
            .rev()
            .find(|o| o.span.start == byte)
            .and_then(|o| o.symbol)
    }

    /// Record a buffer-local call site for parameter-name inlay hints.
    fn record_call_site(
        &mut self,
        open_paren: usize,
        arguments: &[ast::Expression],
        callee: Option<SymbolId>,
    ) {
        if arguments.is_empty() {
            return;
        }
        // Skip call sites inside inlined std/imported code: hint positions
        // must land in the open buffer.
        if let Some(first) = arguments.first()
            && !self.in_buffer(&first.span)
        {
            return;
        }
        self.call_sites.push(CallSite {
            open_paren,
            args: arguments
                .iter()
                .map(|a| (a.span.start, a.span.end))
                .collect(),
            callee,
        });
    }

    // ----- items -----

    fn walk_item(&mut self, item: &ast::Item) {
        let doc = self.program.doc_comment_for(item);
        match &item.kind {
            ItemKind::Function(f) => {
                self.add_symbol(
                    &f.name.name,
                    SymbolKind::Function,
                    f.name.span,
                    doc,
                    format_function_sig(f),
                    params_of(&f.parameters),
                    None,
                    false,
                    false,
                    None,
                    true,
                );
                self.walk_function_body(&f.parameters, &f.body);
            }
            ItemKind::GlobalVariable(g) => {
                let kind = if g.is_mutable {
                    SymbolKind::Global
                } else {
                    SymbolKind::Const
                };
                self.add_symbol(
                    &g.name.name,
                    kind,
                    g.name.span,
                    doc,
                    format!(
                        "{} {}: {}",
                        if g.is_mutable { "let" } else { "const" },
                        g.name.name,
                        format_type(&g.var_type)
                    ),
                    Vec::new(),
                    None,
                    g.is_mutable,
                    g.is_static,
                    None,
                    true,
                );
                self.walk_type(&g.var_type);
                if let Some(init) = &g.initializer {
                    self.walk_expr(init);
                }
            }
            ItemKind::Struct(s) => {
                self.add_symbol(
                    &s.name.name,
                    SymbolKind::Struct,
                    s.name.span,
                    doc,
                    format_struct_hover(s, &item.attributes, &s.name.name, &self.struct_map),
                    Vec::new(),
                    None,
                    false,
                    false,
                    None,
                    true,
                );
                self.declare_generics(s.generics.as_ref());
                for field in &s.fields {
                    self.add_symbol(
                        &field.name.name,
                        SymbolKind::Field,
                        field.name.span,
                        None,
                        format!("{}: {}", field.name.name, format_type(&field.field_type)),
                        Vec::new(),
                        Some(format!("{}::", s.name.name)),
                        false,
                        false,
                        Some(&s.name.name),
                        false,
                    );
                    self.walk_type(&field.field_type);
                }
            }
            ItemKind::Enum(e) => {
                self.add_symbol(
                    &e.name.name,
                    SymbolKind::Enum,
                    e.name.span,
                    doc,
                    format!("enum {}", e.name.name),
                    Vec::new(),
                    None,
                    false,
                    false,
                    None,
                    true,
                );
                self.declare_generics(e.generics.as_ref());
                for variant in &e.variants {
                    let payload = match &variant.data {
                        ast::EnumVariantData::Unit => String::new(),
                        ast::EnumVariantData::Tuple(types) => {
                            let parts: Vec<String> = types.iter().map(format_type).collect();
                            format!("({})", parts.join(", "))
                        }
                        ast::EnumVariantData::Struct(fields) => {
                            let parts: Vec<String> = fields
                                .iter()
                                .map(|f| format!("{}: {}", f.name.name, format_type(&f.field_type)))
                                .collect();
                            format!(" {{ {} }}", parts.join(", "))
                        }
                    };
                    self.add_symbol(
                        &variant.name.name,
                        SymbolKind::Variant,
                        variant.name.span,
                        None,
                        format!("{}{}", variant.name.name, payload),
                        Vec::new(),
                        Some(format!("{}::", e.name.name)),
                        false,
                        false,
                        Some(&e.name.name),
                        false,
                    );
                    if let ast::EnumVariantData::Tuple(types) = &variant.data {
                        for t in types {
                            self.walk_type(t);
                        }
                    }
                    if let ast::EnumVariantData::Struct(fields) = &variant.data {
                        for f in fields {
                            self.walk_type(&f.field_type);
                        }
                    }
                }
            }
            ItemKind::Trait(t) => {
                self.add_symbol(
                    &t.name.name,
                    SymbolKind::Trait,
                    t.name.span,
                    doc,
                    format!("trait {}", t.name.name),
                    Vec::new(),
                    None,
                    false,
                    false,
                    None,
                    true,
                );
                self.declare_generics(t.generics.as_ref());
                for bound in &t.super_traits {
                    self.walk_trait_ref(&bound.trait_ref);
                }
                for item in &t.items {
                    match item {
                        ast::TraitItemKind::Function(f) => {
                            self.add_symbol(
                                &f.name.name,
                                SymbolKind::Method,
                                f.name.span,
                                None,
                                format_function_sig_from_parts(
                                    &f.name.name,
                                    f.generics.as_ref(),
                                    &f.parameters,
                                    f.return_type.as_ref(),
                                ),
                                params_of(&f.parameters),
                                Some(format!("{}::", t.name.name)),
                                false,
                                false,
                                Some(&t.name.name),
                                false,
                            );
                            self.walk_types_of_params(&f.parameters);
                            if let Some(ret) = &f.return_type {
                                self.walk_type(ret);
                            }
                            if let Some(body) = &f.default_body {
                                self.walk_block(body);
                            }
                        }
                        ast::TraitItemKind::AssociatedType(a) => {
                            self.add_symbol(
                                &a.name.name,
                                SymbolKind::TypeAlias,
                                a.name.span,
                                None,
                                format!("type {} = ...", a.name.name),
                                Vec::new(),
                                Some(format!("{}::", t.name.name)),
                                false,
                                false,
                                Some(&t.name.name),
                                false,
                            );
                        }
                        ast::TraitItemKind::AssociatedFunctionValue(v) => {
                            self.add_symbol(
                                &v.name.name,
                                SymbolKind::Function,
                                v.name.span,
                                None,
                                format!(
                                    "fn {} -> {}",
                                    v.name.name,
                                    format_type(&v.fn_type.return_type)
                                ),
                                v.fn_type
                                    .parameters
                                    .iter()
                                    .map(|t| ParamInfo {
                                        name: String::new(),
                                        type_str: format_type(t),
                                    })
                                    .collect(),
                                Some(format!("{}::", t.name.name)),
                                false,
                                false,
                                Some(&t.name.name),
                                false,
                            );
                        }
                    }
                }
            }
            ItemKind::Impl(imp) => {
                self.walk_type(&imp.self_type);
                if let Some(tr) = &imp.trait_ref {
                    self.walk_trait_ref(tr);
                }
                let container = type_root_name(&imp.self_type);
                for member in &imp.items {
                    match member {
                        ast::ImplItemKind::Function(f) => {
                            let is_static = matches!(f.method_kind, ast::MethodKind::Static);
                            self.add_symbol(
                                &f.name.name,
                                SymbolKind::Method,
                                f.name.span,
                                None,
                                format_impl_function_sig(f),
                                params_of(&f.parameters),
                                container.clone().map(|c| format!("{c}::")),
                                false,
                                is_static,
                                container.as_deref(),
                                false,
                            );
                            self.walk_method_body(&f.parameters, &f.body);
                        }
                        ast::ImplItemKind::AssociatedType(a) => {
                            self.add_symbol(
                                &a.name.name,
                                SymbolKind::TypeAlias,
                                a.name.span,
                                None,
                                format!("type {} = {}", a.name.name, format_type(&a.type_def)),
                                Vec::new(),
                                container.clone().map(|c| format!("{c}::")),
                                false,
                                false,
                                container.as_deref(),
                                false,
                            );
                            self.walk_type(&a.type_def);
                        }
                        ast::ImplItemKind::Cast(c) => {
                            self.walk_type(&c.target_type);
                            for (idx, p) in c.parameters.iter().enumerate() {
                                if idx == 0 {
                                    // `cast` receiver is the self value.
                                    self.walk_type(&p.param_type);
                                    continue;
                                }
                                self.walk_type(&p.param_type);
                            }
                            self.walk_block(&c.body);
                        }
                    }
                }
            }
            ItemKind::Import(import) => {
                let path: Vec<String> = import.path.iter().map(|s| s.name.clone()).collect();
                self.import_paths.push(path);
                for (idx, segment) in import.path.iter().enumerate() {
                    // First segment may be a module root; emit all as namespace.
                    let _ = idx;
                    self.emit(
                        None,
                        OccurrenceKind::Namespace,
                        &segment.span,
                        false,
                        false,
                        false,
                        false,
                    );
                }
            }
            ItemKind::ExternFunction(f) => {
                self.add_symbol(
                    &f.name.name,
                    SymbolKind::ExternFunction,
                    f.name.span,
                    doc,
                    format_extern_function_sig(f),
                    params_of(&f.signature.parameters),
                    None,
                    false,
                    false,
                    None,
                    true,
                );
            }
            ItemKind::ExternVariable(v) => {
                self.add_symbol(
                    &v.name.name,
                    SymbolKind::ExternVariable,
                    v.name.span,
                    doc,
                    format!("extern {}: {}", v.name.name, format_type(&v.var_type)),
                    Vec::new(),
                    None,
                    false,
                    false,
                    None,
                    true,
                );
            }
            ItemKind::ExternBlock(block) => {
                for f in &block.functions {
                    self.add_symbol(
                        &f.name.name,
                        SymbolKind::ExternFunction,
                        f.name.span,
                        None,
                        format_extern_function_sig(f),
                        params_of(&f.signature.parameters),
                        None,
                        false,
                        false,
                        None,
                        true,
                    );
                }
                for v in &block.variables {
                    self.add_symbol(
                        &v.name.name,
                        SymbolKind::ExternVariable,
                        v.name.span,
                        None,
                        format!("extern {}: {}", v.name.name, format_type(&v.var_type)),
                        Vec::new(),
                        None,
                        false,
                        false,
                        None,
                        true,
                    );
                }
            }
            ItemKind::Macro(m) => {
                self.add_symbol(
                    &m.name.name,
                    SymbolKind::Macro,
                    m.name.span,
                    doc,
                    format!("macro {}({})", m.name.name, format_params(&m.parameters)),
                    Vec::new(),
                    None,
                    false,
                    false,
                    None,
                    true,
                );
                self.walk_block(&m.body);
            }
            ItemKind::TypeAlias(a) => {
                self.add_symbol(
                    &a.name.name,
                    SymbolKind::TypeAlias,
                    a.name.span,
                    doc,
                    format!("type {} = {}", a.name.name, format_type(&a.type_def)),
                    Vec::new(),
                    None,
                    false,
                    false,
                    None,
                    true,
                );
                self.walk_type(&a.type_def);
            }
        }
    }

    fn walk_function_body(&mut self, params: &[ast::Parameter], body: &ast::Block) {
        self.locals.push(HashMap::default());
        for p in params {
            self.walk_type(&p.param_type);
            self.declare_local(&p.name.name, p.name.span, SymbolKind::Parameter);
        }
        self.walk_block(body);
        self.locals.pop();
    }

    fn walk_method_body(&mut self, params: &[ast::Parameter], body: &ast::Block) {
        self.walk_function_body(params, body);
    }

    fn declare_generics(&mut self, generics: Option<&ast::Generics>) {
        let Some(generics) = generics else { return };
        for param in &generics.params {
            if let ast::GenericParam::Type(tp) = param {
                let id = self.declare_local(&tp.name.name, tp.name.span, SymbolKind::TypeParam);
                let _ = id;
            }
        }
    }

    fn walk_types_of_params(&mut self, params: &[ast::Parameter]) {
        for p in params {
            self.walk_type(&p.param_type);
        }
    }

    // ----- types -----

    fn walk_type(&mut self, ty: &ast::Type) {
        match &*ty.kind {
            ast::TypeKind::Primitive(_) => {}
            ast::TypeKind::Named(n) => {
                for (idx, ident) in n.path.iter().enumerate() {
                    let kind = if idx == 0 {
                        OccurrenceKind::Type
                    } else {
                        OccurrenceKind::Namespace
                    };
                    let symbol = if idx == 0 {
                        self.resolve(&ident.name)
                    } else {
                        None
                    };
                    self.emit(symbol, kind, &ident.span, false, false, false, false);
                }
                if let Some(args) = &n.generics {
                    for arg in args {
                        self.walk_type(arg);
                    }
                }
            }
            ast::TypeKind::Generic(g) => {
                let symbol = self.resolve(&g.name.name);
                self.emit(
                    symbol,
                    OccurrenceKind::TypeParam,
                    &g.name.span,
                    false,
                    false,
                    false,
                    false,
                );
                for arg in &g.args {
                    self.walk_type(arg);
                }
            }
            ast::TypeKind::Reference(r) => self.walk_type(&r.inner),
            ast::TypeKind::Pointer(p) => self.walk_type(&p.inner),
            ast::TypeKind::Slice(s) => self.walk_type(&s.element_type),
            ast::TypeKind::Array(a) => self.walk_type(&a.element_type),
            ast::TypeKind::Optional(t) => self.walk_type(t),
            ast::TypeKind::Function(f) => {
                for t in &f.parameters {
                    self.walk_type(t);
                }
                self.walk_type(&f.return_type);
            }
            ast::TypeKind::Tuple(types) => {
                for t in types {
                    self.walk_type(t);
                }
            }
        }
    }

    fn walk_trait_ref(&mut self, tr: &ast::TraitRef) {
        for (idx, ident) in tr.path.iter().enumerate() {
            let kind = if idx == 0 {
                OccurrenceKind::Type
            } else {
                OccurrenceKind::Namespace
            };
            self.emit(
                self.resolve(&ident.name),
                kind,
                &ident.span,
                false,
                false,
                false,
                false,
            );
        }
        if let Some(args) = &tr.generics {
            for arg in args {
                self.walk_type(arg);
            }
        }
    }

    // ----- statements / blocks -----

    fn walk_block(&mut self, block: &ast::Block) {
        self.locals.push(HashMap::default());
        for stmt in &block.statements {
            self.walk_stmt(stmt);
        }
        self.locals.pop();
    }

    fn walk_stmt(&mut self, stmt: &ast::Statement) {
        match &stmt.kind {
            ast::StatementKind::Block(b) => self.walk_block(b),
            ast::StatementKind::Expression(e) => self.walk_expr(e),
            ast::StatementKind::Let(ls) => {
                let inferred = if ls.type_annotation.is_none() {
                    match (&ls.pattern.kind, ls.initializer.as_ref()) {
                        (
                            ast::PatternKind::Identifier(id) | ast::PatternKind::Move(id),
                            Some(init),
                        ) => self
                            .type_of(init)
                            .map(str::to_owned)
                            .map(|ty| (id.span, ty)),
                        _ => None,
                    }
                } else {
                    None
                };
                if let Some(ty) = &ls.type_annotation {
                    self.walk_type(ty);
                }
                if let Some(init) = &ls.initializer {
                    self.walk_expr(init);
                }
                self.walk_pattern(&ls.pattern);
                if let Some((span, ty)) = inferred
                    && self.in_buffer(&span)
                    && let Some(symbol) = self
                        .symbols
                        .iter_mut()
                        .rev()
                        .find(|symbol| symbol.kind == SymbolKind::Local && symbol.span == span)
                {
                    symbol.inferred_type = Some(ty);
                }
            }
            ast::StatementKind::Return(Some(e)) => self.walk_expr(e),
            ast::StatementKind::Return(None)
            | ast::StatementKind::Continue
            | ast::StatementKind::Break(None) => {}
            ast::StatementKind::Break(Some(e)) => self.walk_expr(e),
            ast::StatementKind::Defer(s) => self.walk_stmt(s),
        }
    }

    fn walk_pattern(&mut self, pattern: &ast::Pattern) {
        match &pattern.kind {
            ast::PatternKind::Identifier(id) | ast::PatternKind::Move(id) => {
                self.declare_local(&id.name, id.span, SymbolKind::Local);
            }
            ast::PatternKind::Tuple(patterns) => {
                for p in patterns {
                    self.walk_pattern(p);
                }
            }
            ast::PatternKind::Struct { path, fields } => {
                for (idx, ident) in path.iter().enumerate() {
                    self.emit(
                        if idx == 0 {
                            self.resolve(&ident.name)
                        } else {
                            None
                        },
                        if idx == 0 {
                            OccurrenceKind::Type
                        } else {
                            OccurrenceKind::Namespace
                        },
                        &ident.span,
                        false,
                        false,
                        false,
                        false,
                    );
                }
                for f in fields {
                    self.emit(
                        None,
                        OccurrenceKind::Property,
                        &f.name.span,
                        false,
                        false,
                        false,
                        false,
                    );
                    if let Some(inner) = &f.pattern {
                        self.walk_pattern(inner);
                    }
                }
            }
            ast::PatternKind::Enum {
                path,
                variant,
                data,
            } => {
                for ident in path {
                    self.emit(
                        self.resolve(&ident.name),
                        OccurrenceKind::Type,
                        &ident.span,
                        false,
                        false,
                        false,
                        false,
                    );
                }
                self.emit(
                    None,
                    OccurrenceKind::EnumMember,
                    &variant.span,
                    false,
                    false,
                    false,
                    false,
                );
                if let Some(inner) = data {
                    self.walk_pattern(inner);
                }
            }
            ast::PatternKind::Literal(_) | ast::PatternKind::Wildcard => {}
            ast::PatternKind::Range { start, end, .. } => {
                self.walk_expr(start);
                self.walk_expr(end);
            }
        }
    }

    // ----- expressions -----

    fn walk_expr(&mut self, expr: &ast::Expression) {
        match &*expr.kind {
            ast::ExpressionKind::Literal(_) => {}
            ast::ExpressionKind::Identifier(id) => self.emit_identifier(id),
            ast::ExpressionKind::TypeName(ty) => self.walk_type(ty),
            ast::ExpressionKind::Binary { left, right, .. } => {
                self.walk_expr(left);
                self.walk_expr(right);
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. } => self.walk_expr(operand),
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                self.walk_expr(function);
                let callee = self.occurrence_symbol_at(function.span.start);
                self.record_call_site(function.span.end, arguments, callee);
                for a in arguments {
                    self.walk_expr(a);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                let recv_type = self.type_of(receiver).map(str::to_string);
                self.walk_expr(receiver);
                self.emit_member_use(
                    method,
                    recv_type.as_deref(),
                    OccurrenceKind::Method,
                    SymbolKind::Method,
                );
                let callee = self.occurrence_symbol_at(method.span.start);
                self.record_call_site(method.span.end, arguments, callee);
                for a in arguments {
                    self.walk_expr(a);
                }
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                let recv_type = self.type_of(object).map(str::to_string);
                self.walk_expr(object);
                self.emit_member_use(
                    field,
                    recv_type.as_deref(),
                    OccurrenceKind::Property,
                    SymbolKind::Field,
                );
            }
            ast::ExpressionKind::Index { object, index } => {
                self.walk_expr(object);
                self.walk_expr(index);
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.walk_expr(condition);
                self.walk_expr(then_expr);
                self.walk_expr(else_expr);
            }
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                self.walk_expr(value);
                self.walk_expr(fallback);
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_expr(condition);
                self.walk_block(then_branch);
                if let Some(else_branch) = else_branch {
                    self.walk_block(else_branch);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                self.walk_expr(condition);
                self.walk_block(body);
            }
            ast::ExpressionKind::ForIn {
                binding,
                iterable,
                body,
                ..
            } => {
                self.walk_expr(iterable);
                self.locals.push(HashMap::default());
                self.declare_local(&binding.name, binding.span, SymbolKind::Local);
                self.walk_block(body);
                self.locals.pop();
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.locals.push(HashMap::default());
                if let Some(ty) = &init.type_annotation {
                    self.walk_type(ty);
                }
                if let Some(init_expr) = &init.initializer {
                    self.walk_expr(init_expr);
                }
                self.walk_pattern(&init.pattern);
                self.walk_expr(condition);
                self.walk_expr(increment);
                self.walk_block(body);
                self.locals.pop();
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.walk_expr(expression);
                for arm in arms {
                    self.locals.push(HashMap::default());
                    self.walk_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.walk_expr(guard);
                    }
                    self.walk_expr(&arm.body);
                    self.locals.pop();
                }
            }
            ast::ExpressionKind::Block(b) => self.walk_block(b),
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(e) => self.walk_expr(e),
                        ast::InitializerItem::Field { name, value } => {
                            self.emit(
                                None,
                                OccurrenceKind::Property,
                                &name.span,
                                false,
                                false,
                                false,
                                false,
                            );
                            self.walk_expr(value);
                        }
                        ast::InitializerItem::Index { index, value } => {
                            self.walk_expr(index);
                            self.walk_expr(value);
                        }
                    }
                }
            }
            ast::ExpressionKind::Asm { inputs, .. } => {
                for input in inputs {
                    self.walk_expr(input);
                }
            }
            ast::ExpressionKind::Array(exprs) | ast::ExpressionKind::Tuple(exprs) => {
                for e in exprs {
                    self.walk_expr(e);
                }
            }
            ast::ExpressionKind::StructLiteral { path, fields } => {
                for (idx, ident) in path.iter().enumerate() {
                    self.emit(
                        if idx == 0 {
                            self.resolve(&ident.name)
                        } else {
                            None
                        },
                        if idx == 0 {
                            OccurrenceKind::Type
                        } else {
                            OccurrenceKind::Namespace
                        },
                        &ident.span,
                        false,
                        false,
                        false,
                        false,
                    );
                }
                let struct_name = path.first().map(|i| i.name.as_str());
                for f in fields {
                    self.emit_member_by_container(
                        &f.name,
                        struct_name,
                        SymbolKind::Field,
                        OccurrenceKind::Property,
                    );
                    self.walk_expr(&f.value);
                }
            }
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                self.walk_expr(expression);
                self.walk_type(target_type);
            }
            ast::ExpressionKind::Move(e)
            | ast::ExpressionKind::Comptime(e)
            | ast::ExpressionKind::Launch(e)
            | ast::ExpressionKind::Wait(e) => self.walk_expr(e),
            ast::ExpressionKind::Reference { expression, .. } => self.walk_expr(expression),
            ast::ExpressionKind::EnumVariant {
                path,
                variant,
                fields,
            } => {
                for ident in path {
                    self.emit(
                        self.resolve(&ident.name),
                        OccurrenceKind::Type,
                        &ident.span,
                        false,
                        false,
                        false,
                        false,
                    );
                }
                let enum_name = path.first().map(|i| i.name.as_str());
                self.emit_member_by_container(
                    variant,
                    enum_name,
                    SymbolKind::Variant,
                    OccurrenceKind::EnumMember,
                );
                for f in fields {
                    self.walk_expr(f);
                }
            }
            ast::ExpressionKind::MacroCall { name, args } => {
                self.emit(
                    self.resolve(&name.name),
                    OccurrenceKind::Macro,
                    &name.span,
                    false,
                    false,
                    false,
                    false,
                );
                for arg in args {
                    match arg {
                        ast::MacroArg::Expression(e) => self.walk_expr(e),
                        ast::MacroArg::Type(t) => self.walk_type(t),
                        ast::MacroArg::Pattern(p) => self.walk_pattern(p),
                        ast::MacroArg::Statement(s) => self.walk_stmt(s),
                        ast::MacroArg::Item(i) => self.walk_item(i),
                        ast::MacroArg::Literal(_) => {}
                        ast::MacroArg::Identifier(id) => self.emit_identifier(id),
                    }
                }
            }
        }
    }

    /// Emit an identifier occurrence with scope-aware resolution.
    fn emit_identifier(&mut self, id: &ast::Identifier) {
        if let Some(symbol_id) = self.resolve(&id.name) {
            let symbol = &self.symbols[symbol_id];
            let kind = match symbol.kind {
                SymbolKind::Function | SymbolKind::ExternFunction => OccurrenceKind::Function,
                SymbolKind::Method => OccurrenceKind::Method,
                SymbolKind::Struct
                | SymbolKind::Enum
                | SymbolKind::Trait
                | SymbolKind::TypeAlias => OccurrenceKind::Type,
                SymbolKind::Global | SymbolKind::ExternVariable => OccurrenceKind::Variable,
                SymbolKind::Const => OccurrenceKind::Const,
                SymbolKind::Macro => OccurrenceKind::Macro,
                SymbolKind::Field => OccurrenceKind::Property,
                SymbolKind::Variant => OccurrenceKind::EnumMember,
                SymbolKind::Parameter => OccurrenceKind::Parameter,
                SymbolKind::Local => OccurrenceKind::Variable,
                SymbolKind::TypeParam => OccurrenceKind::TypeParam,
            };
            self.emit(
                Some(symbol_id),
                kind,
                &id.span,
                false,
                !symbol.is_mutable
                    && matches!(
                        symbol.kind,
                        SymbolKind::Const
                            | SymbolKind::Global
                            | SymbolKind::ExternVariable
                            | SymbolKind::Field
                    ),
                symbol.is_static,
                symbol.doc.is_some(),
            );
        } else if is_builtin_type(&id.name) {
            self.emit(
                None,
                OccurrenceKind::Type,
                &id.span,
                false,
                false,
                false,
                false,
            );
        } else {
            self.emit(
                None,
                OccurrenceKind::Variable,
                &id.span,
                false,
                false,
                false,
                false,
            );
        }
    }

    /// Resolve a member use (method/field) against the receiver's type when
    /// the type checker could name it; fall back to name matching among the
    /// struct's children if the receiver type names a known container.
    fn emit_member_use(
        &mut self,
        member: &ast::Identifier,
        receiver_type: Option<&str>,
        kind: OccurrenceKind,
        member_kind: SymbolKind,
    ) {
        let container = receiver_type.and_then(type_root_name_of_str);
        self.emit_member_by_container(member, container.as_deref(), member_kind, kind);
    }

    fn emit_member_by_container(
        &mut self,
        member: &ast::Identifier,
        container: Option<&str>,
        member_kind: SymbolKind,
        kind: OccurrenceKind,
    ) {
        let symbol = container.and_then(|c| {
            self.struct_children.get(c)?.iter().copied().find(|id| {
                let s = &self.symbols[*id];
                s.name == member.name && s.kind == member_kind
            })
        });
        let (symbol, readonly, is_static, documented) = match symbol {
            Some(id) => {
                let s = &self.symbols[id];
                (
                    Some(id),
                    !s.is_mutable && matches!(s.kind, SymbolKind::Field | SymbolKind::Const),
                    s.is_static,
                    s.doc.is_some(),
                )
            }
            None => (None, false, false, false),
        };
        self.emit(
            symbol,
            kind,
            &member.span,
            false,
            readonly,
            is_static,
            documented,
        );
    }

    /// Look up the formatted type of an expression's span from the type
    /// checker's map.
    fn type_of(&self, expr: &ast::Expression) -> Option<&str> {
        self.expr_types
            .get(&(expr.span.start, expr.span.end))
            .map(String::as_str)
    }
}

fn occurrence_kind_for(kind: SymbolKind) -> OccurrenceKind {
    match kind {
        SymbolKind::Function | SymbolKind::ExternFunction => OccurrenceKind::Function,
        SymbolKind::Method => OccurrenceKind::Method,
        SymbolKind::Struct => OccurrenceKind::Struct,
        SymbolKind::Enum => OccurrenceKind::Enum,
        SymbolKind::Trait => OccurrenceKind::Trait,
        SymbolKind::Global | SymbolKind::ExternVariable => OccurrenceKind::Variable,
        SymbolKind::Const => OccurrenceKind::Const,
        SymbolKind::TypeAlias => OccurrenceKind::TypeAlias,
        SymbolKind::Macro => OccurrenceKind::Macro,
        SymbolKind::Field => OccurrenceKind::Property,
        SymbolKind::Variant => OccurrenceKind::EnumMember,
        SymbolKind::Parameter => OccurrenceKind::Parameter,
        SymbolKind::Local => OccurrenceKind::Variable,
        SymbolKind::TypeParam => OccurrenceKind::TypeParam,
    }
}

fn params_of(params: &[ast::Parameter]) -> Vec<ParamInfo> {
    params
        .iter()
        .map(|p| ParamInfo {
            name: p.name.name.clone(),
            type_str: format_type(&p.param_type),
        })
        .collect()
}

/// The root type name of a type expression (e.g. `Point*` → `Point`,
/// `std.mem.Vec<i32>` → `Vec`).
fn type_root_name(ty: &ast::Type) -> Option<String> {
    match &*ty.kind {
        ast::TypeKind::Named(n) => n.path.last().map(|i| i.name.clone()),
        ast::TypeKind::Generic(g) => Some(g.name.name.clone()),
        ast::TypeKind::Primitive(p) => Some(format_primitive_type(p).to_string()),
        ast::TypeKind::Pointer(p) => type_root_name(&p.inner),
        ast::TypeKind::Reference(r) => type_root_name(&r.inner),
        ast::TypeKind::Optional(t) => type_root_name(t),
        _ => None,
    }
}

/// Strip pointer/reference/generic suffixes from a formatted type string
/// (`Point*`, `&mut Point`, `Vec<i32>` → `Point` / `Vec`).
pub fn type_root_name_of_str(ty: &str) -> Option<String> {
    let trimmed = ty.trim();
    let base = trimmed
        .trim_start_matches('&')
        .trim_start_matches("mut ")
        .trim_end_matches('*')
        .trim_end_matches('?')
        .trim();
    let name = base.split('<').next().unwrap_or(base).trim();
    if name.is_empty() {
        None
    } else {
        Some(name.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex_with_source;

    #[test]
    fn walk_produces_symbols_and_occurrences() {
        let text = "/// Adds two integers.\ni32 add(i32 a, i32 b) {\n    return a + b;\n}\n\nstruct Point {\n    i32 x;\n    i32 y;\n}\n\nimpl Point {\n    i32 sum(Point* self) {\n        return (*self).x + (*self).y;\n    }\n}\n\ni32 main() {\n    i32 result = add(1, 2);\n    Point p = { .x = 3, .y = 4 };\n    i32 total = p.sum();\n    return result + total;\n}\n";
        // Mirror the server path: register the source, lex with its file id.
        let file_id = crate::lexer::register_source("/tmp/unit_test.ag", text);
        let tokens = lex_with_source(text, file_id).unwrap();
        let mut parser = crate::parser::Parser::new(tokens.clone());
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");

        let analysis = analyze(&program, text, &tokens, ExprTypeMap::default(), file_id);
        // Top-level items, params, locals, fields, methods, variants.
        assert!(
            analysis
                .symbols
                .iter()
                .any(|s| s.name == "add" && s.kind == SymbolKind::Function),
            "function symbol missing: {:?}",
            analysis.symbols.iter().map(|s| &s.name).collect::<Vec<_>>()
        );
        assert!(
            analysis
                .symbols
                .iter()
                .any(|s| s.name == "Point" && s.kind == SymbolKind::Struct)
        );
        assert!(
            analysis
                .symbols
                .iter()
                .any(|s| s.name == "sum" && s.kind == SymbolKind::Method)
        );
        assert!(
            analysis
                .symbols
                .iter()
                .any(|s| s.name == "x" && s.kind == SymbolKind::Field)
        );
        assert!(
            analysis
                .symbols
                .iter()
                .any(|s| s.name == "result" && s.kind == SymbolKind::Local)
        );
        assert!(
            analysis
                .symbols
                .iter()
                .any(|s| s.name == "a" && s.kind == SymbolKind::Parameter)
        );

        // Doc comment attaches to the function symbol.
        let add = analysis.symbols.iter().find(|s| s.name == "add").unwrap();
        assert_eq!(add.doc.as_deref(), Some("Adds two integers."));

        // Occurrences: definitions plus uses (call sites, field accesses).
        assert!(analysis.occurrences.len() >= 20, "too few occurrences");
        let uses = analysis
            .occurrences
            .iter()
            .filter(|o| !o.is_definition)
            .count();
        assert!(uses >= 8, "expected identifier uses, got {uses}");
    }
}
