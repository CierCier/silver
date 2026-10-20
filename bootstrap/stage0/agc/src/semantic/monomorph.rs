use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::diagnostics::{self, Severity};
use crate::lexer::Span;
use crate::module_artifact::{ExportKind, ModuleArtifact, ast_type_from_canonical_key};
use crate::parser::ast;
use crate::types::Type;

/// Upper bound on fixpoint generations for both the request-driven and the
/// struct-instantiation loops. Each generation only instantiates previously
/// unseen keys, so a well-formed program converges in a few rounds; hitting
/// the cap indicates runaway recursive generic expansion (compiler bug or
/// pathological input) and is reported instead of hanging the compiler.
const MAX_FIXPOINT_GENERATIONS: usize = 256;

/// Synthetic attribute marking a generated monomorph instance (a concrete
/// function or impl-method produced from a generic template). Codegen gives
/// marked functions `linkonce_odr` linkage plus an `any` COMDAT so duplicate
/// instantiations across compilation units deduplicate at link time — on ELF
/// via weak symbols and on COFF via COMDAT selection (COFF has no dedup for
/// bare weak symbols, so the COMDAT is what makes Windows links work).
pub(crate) const MONOMORPH_INSTANCE_ATTR: &str = "silver_monomorph_instance";

pub(crate) fn monomorph_instance_attr() -> ast::Attribute {
    ast::Attribute {
        name: ast::Identifier {
            name: MONOMORPH_INSTANCE_ATTR.to_string(),
            span: Span::default(),
        },
        args: Vec::new(),
        span: Span::default(),
    }
}

/// Report a non-converging monomorphization fixpoint at `span` and stop
/// expanding. We deliberately do not abort the process: the missing
/// instantiations surface as ordinary downstream errors (and the LSP shares
/// this code path).
fn report_nonconvergence(what: &str, generation: usize, span: Span) {
    eprintln!(
        "{}",
        diagnostics::render(
            span,
            &format!(
                "monomorphization for {what} did not converge after {generation} \
                 generations; recursive generic expansion is too deep"
            ),
            Severity::Error,
        )
    );
}

#[derive(Debug, Clone)]
pub enum MonomorphRequest {
    Function {
        source: Box<ast::FunctionItem>,
        type_params: Vec<String>,
        mapping: HashMap<String, Type>,
        call_span: Span,
        /// True when the source came from an imported module (no body in
        /// this compilation unit): the generated instance is emitted as an
        /// external declaration only.
        is_imported: bool,
    },
    ImplMethod {
        impl_item: Box<ast::ImplItem>,
        method: Box<ast::ImplFunction>,
        type_params: Vec<String>,
        mapping: HashMap<String, Type>,
        call_span: Span,
    },
}

impl MonomorphRequest {
    /// Span of the call site that produced this request.
    pub fn call_span(&self) -> Span {
        match self {
            MonomorphRequest::Function { call_span, .. }
            | MonomorphRequest::ImplMethod { call_span, .. } => *call_span,
        }
    }
}

pub fn refresh_monomorph_bodies(monomorphs: &mut [MonomorphRequest], program: &ast::Program) {
    for request in monomorphs {
        match request {
            MonomorphRequest::Function { source, .. } => {
                for item in &program.items {
                    if let ast::ItemKind::Function(f) = &item.kind
                        && f.name.name == source.name.name
                        && f.generics == source.generics
                        && f.parameters == source.parameters
                        && f.return_type == source.return_type
                    {
                        source.body = f.body.clone();
                        break;
                    }
                }
            }
            MonomorphRequest::ImplMethod {
                impl_item, method, ..
            } => {
                for item in &program.items {
                    if let ast::ItemKind::Impl(impl_item_ast) = &item.kind
                        && impl_item_ast.self_type == impl_item.self_type
                    {
                        for member in &impl_item_ast.items {
                            if let ast::ImplItemKind::Function(func) = member
                                && func.name.name == method.name.name
                            {
                                for source_member in &mut impl_item.items {
                                    if let ast::ImplItemKind::Function(source_func) = source_member
                                        && source_func.name.name == method.name.name
                                    {
                                        source_func.body = func.body.clone();
                                        break;
                                    }
                                }
                                method.body = func.body.clone();
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn append_monomorphs(
    program: &mut ast::Program,
    requests: &[MonomorphRequest],
    imported_modules: &[ModuleArtifact],
) -> Vec<ast::Item> {
    let defaults = collect_local_generic_defaults(program);
    normalize_program_defaults(program, &defaults);
    let mut generic_types = collect_generic_types(program);
    // synthesize templates from the module export metadata (type params +
    // field type keys). Local templates win on name collisions.
    for (name, template) in collect_imported_generic_types(imported_modules) {
        generic_types.entry(name).or_insert(template);
    }

    let mut imported_generic_items = Vec::new();
    for module in imported_modules {
        for template_src in &module.generic_templates {
            let file_id = crate::lexer::register_source(&module.source_path, template_src);
            if let Ok(tokens) = crate::lexer::lex_with_source(template_src, file_id) {
                let mut parser =
                    crate::parser::Parser::new_with_source(tokens, module.source_path.clone());
                let (prog, _) = parser.parse_program();
                for item in prog.items {
                    match &item.kind {
                        ast::ItemKind::Struct(s) if s.generics.is_some() => {
                            generic_types
                                .entry(s.name.name.clone())
                                .or_insert(GenericTypeItem::Struct(s.clone()));
                        }
                        ast::ItemKind::Enum(e) if e.generics.is_some() => {
                            generic_types
                                .entry(e.name.name.clone())
                                .or_insert(GenericTypeItem::Enum(e.clone()));
                        }
                        _ => {}
                    }
                    imported_generic_items.push(item);
                }
            }
        }
    }

    let mut generic_impls = collect_generic_impls(program);
    for item in &imported_generic_items {
        if let ast::ItemKind::Impl(impl_item) = &item.kind {
            if impl_item.generics.is_some() || is_generic_self_type(&impl_item.self_type) {
                generic_impls.push(impl_item.clone());
            }
        }
    }

    let mut generic_fns = collect_generic_fns(program);
    // Imported generic function templates (parsed from artifact sources):
    // instantiating locally gives the instance a real body, so downstream
    // concrete types link without requiring the library to pre-instantiate
    // them (linkonce_odr deduplicates across units).
    let mut imported_fn_templates: HashMap<String, ast::FunctionItem> = HashMap::default();
    for item in &imported_generic_items {
        if let ast::ItemKind::Function(func) = &item.kind {
            if func.generics.is_some() {
                generic_fns.insert(func.name.name.clone());
                imported_fn_templates
                    .entry(func.name.name.clone())
                    .or_insert_with(|| func.clone());
            }
        }
    }

    let mut generated = HashSet::default();
    let mut all_new_items = Vec::new();

    let mut instantiations = HashMap::default();
    collect_struct_instantiations(program, &generic_types, &mut instantiations);

    for (key, inst) in instantiations.iter() {
        if generated.insert(key.clone()) {
            let item = inst.item.clone();
            all_new_items.push(item);
            program.items.push(inst.item.clone());
        }
    }
    let impl_items = instantiate_impls(&generic_impls, &instantiations, &mut generated);
    // Collect all newly generated items for remaining-call scanning
    all_new_items.reserve(impl_items.len());
    for item in &impl_items {
        all_new_items.push(item.clone());
        program.items.push(item.clone());
    }

    // Process function/method monomorphization requests to fixpoint.
    let mut current_requests: Vec<MonomorphRequest> = requests.to_vec();
    normalize_requests_defaults(&mut current_requests, &defaults);
    let mut generation = 0usize;
    while !current_requests.is_empty() {
        generation += 1;
        if generation > MAX_FIXPOINT_GENERATIONS {
            report_nonconvergence(
                "function instances",
                generation,
                current_requests[0].call_span(),
            );
            break;
        }
        let mut new_items = instantiate_requests(
            program,
            &current_requests,
            &mut generated,
            &imported_fn_templates,
        );
        // Bare-Identifier calls to generic functions carry no explicit type
        // arguments, so the span-based passes below cannot see them. Resolve
        // them against the substituted instance bodies here, rewriting each
        // matched site to its mangled instance inline.
        let mut bare_requests = discover_bare_generic_calls(
            &mut new_items,
            program,
            &imported_fn_templates,
            &generated,
        );
        for item in &new_items {
            all_new_items.push(item.clone());
            program.items.push(item.clone());
        }

        // Scan ALL new items (including newly generated impl items) for remaining
        // generic calls with concrete type args.
        current_requests = collect_remaining_function_requests(
            program,
            &imported_generic_items,
            &all_new_items,
            &generic_fns,
            &generated,
        );
        current_requests.append(&mut bare_requests);
    }
    all_new_items
}

fn collect_local_generic_defaults(program: &ast::Program) -> Vec<(String, Vec<Option<ast::Type>>)> {
    program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ast::ItemKind::Struct(item) => Some((&item.name.name, &item.generics)),
            ast::ItemKind::Enum(item) => Some((&item.name.name, &item.generics)),
            _ => None,
        })
        .filter_map(|(name, generics)| {
            generics.as_ref().map(|generics| {
                (
                    name.clone(),
                    // Lifetime parameters occupy no slot in the type-argument
                    // list: they are erased at use sites, so `StringView<'a>`
                    // is written plain `StringView` and must not trip
                    // default-filling arity checks.
                    generics
                        .params
                        .iter()
                        .filter_map(|param| match param {
                            ast::GenericParam::Type(param) => Some(param.default.clone()),
                            ast::GenericParam::Lifetime(_) => None,
                        })
                        .collect(),
                )
            })
        })
        .collect()
}

fn normalize_type(ty: &mut ast::Type, defaults: &[(String, Vec<Option<ast::Type>>)]) {
    let _ = ast::apply_generic_defaults(ty, defaults);
}

fn normalize_program_defaults(
    program: &mut ast::Program,
    defaults: &[(String, Vec<Option<ast::Type>>)],
) {
    for item in &mut program.items {
        match &mut item.kind {
            ast::ItemKind::Function(function) => {
                for parameter in &mut function.parameters {
                    normalize_type(&mut parameter.param_type, defaults);
                }
                if let Some(return_type) = &mut function.return_type {
                    normalize_type(return_type, defaults);
                }
                normalize_block(&mut function.body, defaults);
            }
            ast::ItemKind::GlobalVariable(variable) => {
                normalize_type(&mut variable.var_type, defaults);
                if let Some(initializer) = &mut variable.initializer {
                    normalize_expression(initializer, defaults);
                }
            }
            ast::ItemKind::Struct(item) => {
                for field in &mut item.fields {
                    normalize_type(&mut field.field_type, defaults);
                }
            }
            ast::ItemKind::Enum(item) => {
                for variant in &mut item.variants {
                    match &mut variant.data {
                        ast::EnumVariantData::Unit => {}
                        ast::EnumVariantData::Tuple(types) => {
                            for ty in types {
                                normalize_type(ty, defaults);
                            }
                        }
                        ast::EnumVariantData::Struct(fields) => {
                            for field in fields {
                                normalize_type(&mut field.field_type, defaults);
                            }
                        }
                    }
                }
            }
            ast::ItemKind::Impl(item) => {
                normalize_type(&mut item.self_type, defaults);
                for child in &mut item.items {
                    if let ast::ImplItemKind::Function(function) = child {
                        for parameter in &mut function.parameters {
                            normalize_type(&mut parameter.param_type, defaults);
                        }
                        if let Some(return_type) = &mut function.return_type {
                            normalize_type(return_type, defaults);
                        }
                        normalize_block(&mut function.body, defaults);
                    }
                }
            }
            _ => {}
        }
    }
}

fn normalize_requests_defaults(
    requests: &mut [MonomorphRequest],
    defaults: &[(String, Vec<Option<ast::Type>>)],
) {
    for request in requests {
        match request {
            MonomorphRequest::Function { source, .. } => {
                for parameter in &mut source.parameters {
                    normalize_type(&mut parameter.param_type, defaults);
                }
                if let Some(return_type) = &mut source.return_type {
                    normalize_type(return_type, defaults);
                }
                normalize_block(&mut source.body, defaults);
            }
            MonomorphRequest::ImplMethod {
                impl_item, method, ..
            } => {
                normalize_type(&mut impl_item.self_type, defaults);
                for parameter in &mut method.parameters {
                    normalize_type(&mut parameter.param_type, defaults);
                }
                if let Some(return_type) = &mut method.return_type {
                    normalize_type(return_type, defaults);
                }
                normalize_block(&mut method.body, defaults);
            }
        }
    }
}

fn normalize_block(block: &mut ast::Block, defaults: &[(String, Vec<Option<ast::Type>>)]) {
    for statement in &mut block.statements {
        match &mut statement.kind {
            ast::StatementKind::Block(block) => normalize_block(block, defaults),
            ast::StatementKind::Let(statement) => {
                if let Some(ty) = &mut statement.type_annotation {
                    normalize_type(ty, defaults);
                }
                if let Some(expression) = &mut statement.initializer {
                    normalize_expression(expression, defaults);
                }
            }
            ast::StatementKind::Expression(expression)
            | ast::StatementKind::Return(Some(expression))
            | ast::StatementKind::Break(Some(expression)) => {
                normalize_expression(expression, defaults)
            }
            ast::StatementKind::Defer(statement) => normalize_statement(statement, defaults),
            ast::StatementKind::Return(None)
            | ast::StatementKind::Break(None)
            | ast::StatementKind::Continue => {}
        }
    }
}

fn normalize_statement(
    statement: &mut ast::Statement,
    defaults: &[(String, Vec<Option<ast::Type>>)],
) {
    match &mut statement.kind {
        ast::StatementKind::Block(block) => normalize_block(block, defaults),
        ast::StatementKind::Let(statement) => {
            if let Some(ty) = &mut statement.type_annotation {
                normalize_type(ty, defaults);
            }
            if let Some(expression) = &mut statement.initializer {
                normalize_expression(expression, defaults);
            }
        }
        ast::StatementKind::Expression(expression)
        | ast::StatementKind::Return(Some(expression))
        | ast::StatementKind::Break(Some(expression)) => normalize_expression(expression, defaults),
        ast::StatementKind::Defer(statement) => normalize_statement(statement, defaults),
        ast::StatementKind::Return(None)
        | ast::StatementKind::Break(None)
        | ast::StatementKind::Continue => {}
    }
}

fn normalize_expression(
    expression: &mut ast::Expression,
    defaults: &[(String, Vec<Option<ast::Type>>)],
) {
    match expression.kind.as_mut() {
        ast::ExpressionKind::TypeName(ty) => normalize_type(ty, defaults),
        ast::ExpressionKind::Cast {
            expression,
            target_type,
        } => {
            normalize_expression(expression, defaults);
            normalize_type(target_type, defaults);
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            normalize_expression(function, defaults);
            for argument in arguments {
                normalize_expression(argument, defaults);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            normalize_expression(receiver, defaults);
            for argument in arguments {
                normalize_expression(argument, defaults);
            }
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            normalize_expression(left, defaults);
            normalize_expression(right, defaults);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. }
        | ast::ExpressionKind::Move(operand)
        | ast::ExpressionKind::Comptime(operand)
        | ast::ExpressionKind::Launch(operand)
        | ast::ExpressionKind::Wait(operand) => normalize_expression(operand, defaults),
        ast::ExpressionKind::Reference { expression, .. } => {
            normalize_expression(expression, defaults)
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. }
        | ast::ExpressionKind::Slice { object, .. } => normalize_expression(object, defaults),
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            normalize_expression(condition, defaults);
            normalize_block(then_branch, defaults);
            if let Some(else_branch) = else_branch {
                normalize_block(else_branch, defaults);
            }
        }
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            normalize_expression(condition, defaults);
            normalize_expression(then_expr, defaults);
            normalize_expression(else_expr, defaults);
        }
        ast::ExpressionKind::UnwrapOr { value, fallback } => {
            normalize_expression(value, defaults);
            normalize_expression(fallback, defaults);
        }
        ast::ExpressionKind::While { condition, body } => {
            normalize_expression(condition, defaults);
            normalize_block(body, defaults);
        }
        ast::ExpressionKind::ForIn {
            iterable,
            body,
            item_type,
            iterator_type,
            ..
        } => {
            normalize_expression(iterable, defaults);
            normalize_block(body, defaults);
            if let Some(item_type) = item_type {
                normalize_type(item_type, defaults);
            }
            if let Some(iterator_type) = iterator_type {
                normalize_type(iterator_type, defaults);
            }
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(ty) = &mut init.type_annotation {
                normalize_type(ty, defaults);
            }
            if let Some(expression) = &mut init.initializer {
                normalize_expression(expression, defaults);
            }
            normalize_expression(condition, defaults);
            normalize_expression(increment, defaults);
            normalize_block(body, defaults);
        }
        ast::ExpressionKind::Match { expression, arms } => {
            normalize_expression(expression, defaults);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    normalize_expression(guard, defaults);
                }
                normalize_expression(&mut arm.body, defaults);
            }
        }
        ast::ExpressionKind::Block(block) => normalize_block(block, defaults),
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(value)
                    | ast::InitializerItem::Field { value, .. } => {
                        normalize_expression(value, defaults)
                    }
                    ast::InitializerItem::Index { index, value } => {
                        normalize_expression(index, defaults);
                        normalize_expression(value, defaults);
                    }
                }
            }
        }
        ast::ExpressionKind::Asm { inputs, .. }
        | ast::ExpressionKind::Array(inputs)
        | ast::ExpressionKind::Tuple(inputs) => {
            for input in inputs {
                normalize_expression(input, defaults);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                normalize_expression(&mut field.value, defaults);
            }
        }
        ast::ExpressionKind::EnumVariant { fields, .. } => {
            for field in fields {
                normalize_expression(field, defaults);
            }
        }
        ast::ExpressionKind::MacroCall { args, .. } => {
            for arg in args {
                if let ast::MacroArg::Expression(expression) = arg {
                    normalize_expression(expression, defaults);
                }
            }
        }
        ast::ExpressionKind::Literal(_) | ast::ExpressionKind::Identifier(_) => {}
    }
}

#[derive(Clone)]
struct TypeInstance {
    base: String,
    mangled: String,
    mapping: HashMap<String, Type>,
    item: ast::Item,
}

enum GenericTypeItem {
    Struct(ast::StructItem),
    Enum(ast::EnumItem),
}

fn collect_generic_types(program: &ast::Program) -> HashMap<String, GenericTypeItem> {
    let mut types = HashMap::default();
    for item in &program.items {
        match &item.kind {
            ast::ItemKind::Struct(struct_item) => {
                if struct_item.generics.is_some() {
                    types.insert(
                        struct_item.name.name.clone(),
                        GenericTypeItem::Struct(struct_item.clone()),
                    );
                }
            }
            ast::ItemKind::Enum(enum_item) if enum_item.generics.is_some() => {
                types.insert(
                    enum_item.name.name.clone(),
                    GenericTypeItem::Enum(enum_item.clone()),
                );
            }
            _ => {}
        }
    }
    types
}

/// Rebuild generic struct templates for imported module exports that carry
/// type params, so the consumer can instantiate them (e.g. `Pair<i64>` from
/// an exported `Pair<T>`). The library's source is not available; the export
/// metadata (type param names + field type keys) is sufficient.
fn collect_imported_generic_types(modules: &[ModuleArtifact]) -> HashMap<String, GenericTypeItem> {
    let mut types = HashMap::default();
    for module in modules {
        for export in &module.exports {
            if (export.kind != ExportKind::Struct && export.kind != ExportKind::Enum)
                || export.type_params.is_empty()
            {
                continue;
            }
            let span = Span::default();
            let generics = Some(ast::Generics {
                params: export
                    .type_params
                    .iter()
                    .map(|name| {
                        ast::GenericParam::Type(ast::TypeParam {
                            name: ast::Identifier {
                                name: name.clone(),
                                span,
                            },
                            bounds: Vec::new(),
                            default: None,
                            span,
                        })
                    })
                    .collect(),
                where_clause: None,
                span,
            });
            match export.kind {
                ExportKind::Struct => {
                    let fields = export
                        .fields
                        .iter()
                        .filter_map(|field| {
                            let field_type = ast_type_from_canonical_key(&field.type_key).ok()?;
                            Some(ast::Field {
                                name: ast::Identifier {
                                    name: field.name.clone(),
                                    span,
                                },
                                field_type,
                                visibility: ast::Visibility::Public,
                                tags: HashMap::default(),
                                span,
                            })
                        })
                        .collect();
                    types.insert(
                        export.name.clone(),
                        GenericTypeItem::Struct(ast::StructItem {
                            name: ast::Identifier {
                                name: export.name.clone(),
                                span,
                            },
                            generics,
                            fields,
                        }),
                    );
                }
                ExportKind::Enum => {
                    let variants = export
                        .enum_variants
                        .iter()
                        .map(|variant| {
                            let data = if !variant.payload_fields.is_empty() {
                                ast::EnumVariantData::Struct(
                                    variant
                                        .payload_fields
                                        .iter()
                                        .filter_map(|field| {
                                            let field_type =
                                                ast_type_from_canonical_key(&field.type_key)
                                                    .ok()?;
                                            Some(ast::Field {
                                                name: ast::Identifier {
                                                    name: field.name.clone(),
                                                    span,
                                                },
                                                field_type,
                                                visibility: ast::Visibility::Public,
                                                tags: HashMap::default(),
                                                span,
                                            })
                                        })
                                        .collect(),
                                )
                            } else if !variant.payload_types.is_empty() {
                                ast::EnumVariantData::Tuple(
                                    variant
                                        .payload_types
                                        .iter()
                                        .filter_map(|key| ast_type_from_canonical_key(key).ok())
                                        .collect(),
                                )
                            } else {
                                ast::EnumVariantData::Unit
                            };
                            ast::EnumVariant {
                                name: ast::Identifier {
                                    name: variant.name.clone(),
                                    span,
                                },
                                data,
                                discriminant: Some(variant.value),
                                span,
                            }
                        })
                        .collect();
                    types.insert(
                        export.name.clone(),
                        GenericTypeItem::Enum(ast::EnumItem {
                            name: ast::Identifier {
                                name: export.name.clone(),
                                span,
                            },
                            generics,
                            variants,
                        }),
                    );
                }
                _ => {}
            }
        }
    }
    types
}

fn collect_generic_impls(program: &ast::Program) -> Vec<ast::ImplItem> {
    let mut impls = Vec::new();
    for item in &program.items {
        let ast::ItemKind::Impl(impl_item) = &item.kind else {
            continue;
        };
        if impl_item.generics.is_some() || is_generic_self_type(&impl_item.self_type) {
            impls.push(impl_item.clone());
        }
    }
    impls
}

fn collect_struct_instantiations(
    program: &ast::Program,
    generic_structs: &HashMap<String, GenericTypeItem>,
    instantiations: &mut HashMap<String, TypeInstance>,
) {
    let mut scopes = Vec::new();
    for item in &program.items {
        collect_item_instantiations(item, generic_structs, instantiations, &mut scopes);
    }

    // A concrete generic type can contain another generic type after substitution,
    // e.g. HashSet<i64> has a HashMap<i64, bool> field even though HashSet<K>'s
    // source field is HashMap<K, bool>. Walk generated instances to a fixpoint so
    // nested concrete types get their structs and impls monomorphized normally.
    let mut processed = HashSet::default();
    let mut generation = 0usize;
    loop {
        generation += 1;
        let pending = instantiations
            .iter()
            .filter(|(key, _)| !processed.contains(*key))
            .map(|(key, inst)| (key.clone(), inst.item.clone()))
            .collect::<Vec<_>>();
        if pending.is_empty() {
            break;
        }
        if generation > MAX_FIXPOINT_GENERATIONS {
            report_nonconvergence("nested struct instances", generation, Span::default());
            break;
        }
        for (key, item) in pending {
            processed.insert(key);
            let mut instance_scopes = Vec::new();
            collect_item_instantiations(
                &item,
                generic_structs,
                instantiations,
                &mut instance_scopes,
            );
        }
    }
}

fn collect_item_instantiations(
    item: &ast::Item,
    generic_structs: &HashMap<String, GenericTypeItem>,
    instantiations: &mut HashMap<String, TypeInstance>,
    scopes: &mut Vec<HashSet<String>>,
) {
    match &item.kind {
        ast::ItemKind::Struct(struct_item) => {
            push_type_params(scopes, struct_item.generics.as_ref());
            for field in &struct_item.fields {
                collect_type_instantiations(
                    &field.field_type,
                    generic_structs,
                    instantiations,
                    scopes,
                );
            }
            pop_type_params(scopes);
        }
        ast::ItemKind::Enum(enum_item) => {
            push_type_params(scopes, enum_item.generics.as_ref());
            for variant in &enum_item.variants {
                match &variant.data {
                    ast::EnumVariantData::Unit => {}
                    ast::EnumVariantData::Tuple(items) => {
                        for ty in items {
                            collect_type_instantiations(
                                ty,
                                generic_structs,
                                instantiations,
                                scopes,
                            );
                        }
                    }
                    ast::EnumVariantData::Struct(fields) => {
                        for field in fields {
                            collect_type_instantiations(
                                &field.field_type,
                                generic_structs,
                                instantiations,
                                scopes,
                            );
                        }
                    }
                }
            }
            pop_type_params(scopes);
        }
        ast::ItemKind::Trait(trait_item) => {
            push_type_params(scopes, trait_item.generics.as_ref());
            // `Self` is the implicit first type parameter of every trait.
            if let Some(scope) = scopes.last_mut() {
                scope.insert("Self".to_string());
            }
            // Register associated type names so they're treated as type params
            for item in &trait_item.items {
                if let ast::TraitItemKind::AssociatedType(assoc) = item
                    && let Some(scope) = scopes.last_mut()
                {
                    scope.insert(assoc.name.name.clone());
                }
            }
            for item in &trait_item.items {
                if let ast::TraitItemKind::Function(func) = item {
                    push_type_params(scopes, func.generics.as_ref());
                    for param in &func.parameters {
                        collect_type_instantiations(
                            &param.param_type,
                            generic_structs,
                            instantiations,
                            scopes,
                        );
                    }
                    if let Some(return_type) = &func.return_type {
                        collect_type_instantiations(
                            return_type,
                            generic_structs,
                            instantiations,
                            scopes,
                        );
                    }
                    pop_type_params(scopes);
                }
            }
            pop_type_params(scopes);
        }
        ast::ItemKind::Function(func) => {
            push_type_params(scopes, func.generics.as_ref());
            for param in &func.parameters {
                collect_type_instantiations(
                    &param.param_type,
                    generic_structs,
                    instantiations,
                    scopes,
                );
            }
            if let Some(return_type) = &func.return_type {
                collect_type_instantiations(return_type, generic_structs, instantiations, scopes);
            }
            collect_block_instantiations(&func.body, generic_structs, instantiations, scopes);
            pop_type_params(scopes);
        }
        ast::ItemKind::Impl(impl_item) => {
            push_type_params(scopes, impl_item.generics.as_ref());
            if impl_item.generics.is_none() {
                let mut implicit = HashSet::default();
                collect_implicit_type_params(&impl_item.self_type, &mut implicit);
                if let Some(scope) = scopes.last_mut() {
                    scope.extend(implicit);
                }
            }
            collect_type_instantiations(
                &impl_item.self_type,
                generic_structs,
                instantiations,
                scopes,
            );
            for impl_item in &impl_item.items {
                if let ast::ImplItemKind::Function(func) = impl_item {
                    push_type_params(scopes, func.generics.as_ref());
                    for param in &func.parameters {
                        collect_type_instantiations(
                            &param.param_type,
                            generic_structs,
                            instantiations,
                            scopes,
                        );
                    }
                    if let Some(return_type) = &func.return_type {
                        collect_type_instantiations(
                            return_type,
                            generic_structs,
                            instantiations,
                            scopes,
                        );
                    }
                    collect_block_instantiations(
                        &func.body,
                        generic_structs,
                        instantiations,
                        scopes,
                    );
                    pop_type_params(scopes);
                }
            }
            pop_type_params(scopes);
        }
        _ => {}
    }
}

fn collect_block_instantiations(
    block: &ast::Block,
    generic_structs: &HashMap<String, GenericTypeItem>,
    instantiations: &mut HashMap<String, TypeInstance>,
    scopes: &mut Vec<HashSet<String>>,
) {
    for stmt in &block.statements {
        collect_statement_instantiations(stmt, generic_structs, instantiations, scopes);
    }
}

fn collect_statement_instantiations(
    stmt: &ast::Statement,
    generic_structs: &HashMap<String, GenericTypeItem>,
    instantiations: &mut HashMap<String, TypeInstance>,
    scopes: &mut Vec<HashSet<String>>,
) {
    match &stmt.kind {
        ast::StatementKind::Block(block) => {
            collect_block_instantiations(block, generic_structs, instantiations, scopes)
        }
        ast::StatementKind::Let(let_stmt) => {
            if let Some(annotation) = &let_stmt.type_annotation {
                collect_type_instantiations(annotation, generic_structs, instantiations, scopes);
            }
            if let Some(init) = &let_stmt.initializer {
                collect_expression_instantiations(init, generic_structs, instantiations, scopes);
            }
        }
        ast::StatementKind::Expression(expr)
        | ast::StatementKind::Return(Some(expr))
        | ast::StatementKind::Break(Some(expr)) => {
            collect_expression_instantiations(expr, generic_structs, instantiations, scopes)
        }
        ast::StatementKind::Return(None) | ast::StatementKind::Break(None) => {}
        ast::StatementKind::Defer(inner) => {
            collect_statement_instantiations(inner, generic_structs, instantiations, scopes)
        }
        ast::StatementKind::Continue => {}
    }
}

fn collect_expression_instantiations(
    expr: &ast::Expression,
    generic_structs: &HashMap<String, GenericTypeItem>,
    instantiations: &mut HashMap<String, TypeInstance>,
    scopes: &mut Vec<HashSet<String>>,
) {
    match expr.kind.as_ref() {
        ast::ExpressionKind::TypeName(ty) => {
            collect_type_instantiations(ty, generic_structs, instantiations, scopes)
        }
        ast::ExpressionKind::Cast { target_type, .. } => {
            collect_type_instantiations(target_type, generic_structs, instantiations, scopes)
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            collect_expression_instantiations(function, generic_structs, instantiations, scopes);
            for arg in arguments {
                collect_expression_instantiations(arg, generic_structs, instantiations, scopes);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            collect_expression_instantiations(receiver, generic_structs, instantiations, scopes);
            for arg in arguments {
                collect_expression_instantiations(arg, generic_structs, instantiations, scopes);
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. }
        | ast::ExpressionKind::Slice { object, .. } => {
            collect_expression_instantiations(object, generic_structs, instantiations, scopes);
        }
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_expression_instantiations(condition, generic_structs, instantiations, scopes);
            collect_expression_instantiations(then_expr, generic_structs, instantiations, scopes);
            collect_expression_instantiations(else_expr, generic_structs, instantiations, scopes);
        }
        ast::ExpressionKind::UnwrapOr { value, fallback } => {
            collect_expression_instantiations(value, generic_structs, instantiations, scopes);
            collect_expression_instantiations(fallback, generic_structs, instantiations, scopes);
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            collect_expression_instantiations(left, generic_structs, instantiations, scopes);
            collect_expression_instantiations(right, generic_structs, instantiations, scopes);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. } => {
            collect_expression_instantiations(operand, generic_structs, instantiations, scopes);
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_expression_instantiations(condition, generic_structs, instantiations, scopes);
            collect_block_instantiations(then_branch, generic_structs, instantiations, scopes);
            if let Some(branch) = else_branch {
                collect_block_instantiations(branch, generic_structs, instantiations, scopes);
            }
        }
        ast::ExpressionKind::While { condition, body } => {
            collect_expression_instantiations(condition, generic_structs, instantiations, scopes);
            collect_block_instantiations(body, generic_structs, instantiations, scopes);
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(init_expr) = &init.initializer {
                collect_expression_instantiations(
                    init_expr,
                    generic_structs,
                    instantiations,
                    scopes,
                );
            }
            collect_expression_instantiations(condition, generic_structs, instantiations, scopes);
            collect_expression_instantiations(increment, generic_structs, instantiations, scopes);
            collect_block_instantiations(body, generic_structs, instantiations, scopes);
        }
        ast::ExpressionKind::Match { expression, arms } => {
            collect_expression_instantiations(expression, generic_structs, instantiations, scopes);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_expression_instantiations(
                        guard,
                        generic_structs,
                        instantiations,
                        scopes,
                    );
                }
                collect_expression_instantiations(
                    &arm.body,
                    generic_structs,
                    instantiations,
                    scopes,
                );
            }
        }
        ast::ExpressionKind::Block(block) => {
            collect_block_instantiations(block, generic_structs, instantiations, scopes)
        }
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr)
                    | ast::InitializerItem::Field { value: expr, .. } => {
                        collect_expression_instantiations(
                            expr,
                            generic_structs,
                            instantiations,
                            scopes,
                        )
                    }
                    ast::InitializerItem::Index { index, value } => {
                        collect_expression_instantiations(
                            index,
                            generic_structs,
                            instantiations,
                            scopes,
                        );
                        collect_expression_instantiations(
                            value,
                            generic_structs,
                            instantiations,
                            scopes,
                        );
                    }
                }
            }
        }
        ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
            for item in items {
                collect_expression_instantiations(item, generic_structs, instantiations, scopes);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                collect_expression_instantiations(
                    &field.value,
                    generic_structs,
                    instantiations,
                    scopes,
                );
            }
        }
        ast::ExpressionKind::Move(inner)
        | ast::ExpressionKind::Comptime(inner)
        | ast::ExpressionKind::Launch(inner)
        | ast::ExpressionKind::Wait(inner)
        | ast::ExpressionKind::Reference {
            expression: inner, ..
        } => collect_expression_instantiations(inner, generic_structs, instantiations, scopes),
        ast::ExpressionKind::MacroCall { args, .. } => {
            for arg in args {
                if let ast::MacroArg::Expression(expr) = arg {
                    collect_expression_instantiations(
                        expr,
                        generic_structs,
                        instantiations,
                        scopes,
                    );
                }
            }
        }
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            collect_expression_instantiations(iterable, generic_structs, instantiations, scopes);
            collect_block_instantiations(body, generic_structs, instantiations, scopes);
        }
        ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::EnumVariant { .. } => {}
        ast::ExpressionKind::Asm { inputs, .. } => {
            for input in inputs {
                collect_expression_instantiations(input, generic_structs, instantiations, scopes);
            }
        }
    }
}

fn collect_type_instantiations(
    ty: &ast::Type,
    generic_structs: &HashMap<String, GenericTypeItem>,
    instantiations: &mut HashMap<String, TypeInstance>,
    scopes: &Vec<HashSet<String>>,
) {
    let mut normalized = ty.clone();
    let defaults = generic_structs
        .iter()
        .map(|(name, item)| {
            let generics = match item {
                GenericTypeItem::Struct(item) => item.generics.as_ref(),
                GenericTypeItem::Enum(item) => item.generics.as_ref(),
            };
            (
                name.clone(),
                generics
                    .into_iter()
                    .flat_map(|generics| generics.params.iter())
                    .filter_map(|param| match param {
                        ast::GenericParam::Type(param) => Some(param.default.clone()),
                        ast::GenericParam::Lifetime(_) => None,
                    })
                    .collect(),
            )
        })
        .collect::<Vec<_>>();
    let _ = ast::apply_generic_defaults(&mut normalized, &defaults);
    if let Some((base, args)) = concrete_named_type(&normalized, scopes)
        && let Some(item) = generic_structs.get(&base)
    {
        let (generics, instance_item) = match item {
            GenericTypeItem::Struct(struct_item) => {
                let mapping = build_mapping_from_generics(struct_item.generics.as_ref(), &args);
                let mangled = mangle_name(&base, &args);
                let item = instantiate_struct(struct_item, &mangled, &mapping);
                (struct_item.generics.as_ref(), (mapping, mangled, item))
            }
            GenericTypeItem::Enum(enum_item) => {
                let mapping = build_mapping_from_generics(enum_item.generics.as_ref(), &args);
                let mangled = mangle_name(&base, &args);
                let item = instantiate_enum(enum_item, &mangled, &mapping);
                (enum_item.generics.as_ref(), (mapping, mangled, item))
            }
        };

        let (mapping, mangled, item) = instance_item;
        if generics.is_some() {
            let key = format!("type::{base}::{mangled}");
            instantiations.entry(key).or_insert_with(|| TypeInstance {
                base: base.clone(),
                mangled: mangled.clone(),
                mapping: mapping.clone(),
                item,
            });
        }
    }

    match normalized.kind.as_ref() {
        ast::TypeKind::Named(named) => {
            if let Some(generics) = &named.generics {
                for arg in generics {
                    collect_type_instantiations(arg, generic_structs, instantiations, scopes);
                }
            }
        }
        ast::TypeKind::Generic(generic) => {
            for arg in &generic.args {
                collect_type_instantiations(arg, generic_structs, instantiations, scopes);
            }
        }
        ast::TypeKind::Reference(reference) => {
            collect_type_instantiations(&reference.inner, generic_structs, instantiations, scopes)
        }
        ast::TypeKind::Pointer(pointer) => {
            collect_type_instantiations(&pointer.inner, generic_structs, instantiations, scopes)
        }
        ast::TypeKind::Slice(slice) => collect_type_instantiations(
            &slice.element_type,
            generic_structs,
            instantiations,
            scopes,
        ),
        ast::TypeKind::Array(array) => collect_type_instantiations(
            &array.element_type,
            generic_structs,
            instantiations,
            scopes,
        ),
        ast::TypeKind::Optional(inner) => {
            collect_type_instantiations(inner, generic_structs, instantiations, scopes)
        }
        ast::TypeKind::Tuple(items) => {
            for item in items {
                collect_type_instantiations(item, generic_structs, instantiations, scopes);
            }
        }
        ast::TypeKind::Function(func) => {
            for param in &func.parameters {
                collect_type_instantiations(param, generic_structs, instantiations, scopes);
            }
            collect_type_instantiations(&func.return_type, generic_structs, instantiations, scopes);
        }
        ast::TypeKind::Primitive(_) => {}
    }
}

fn instantiate_struct(
    struct_item: &ast::StructItem,
    mangled: &str,
    mapping: &HashMap<String, Type>,
) -> ast::Item {
    let fields = struct_item
        .fields
        .iter()
        .map(|field| ast::Field {
            name: field.name.clone(),
            field_type: substitute_ast_type(&field.field_type, mapping),
            visibility: field.visibility.clone(),
            tags: field.tags.clone(),
            span: field.span,
        })
        .collect::<Vec<_>>();

    ast::Item {
        kind: ast::ItemKind::Struct(ast::StructItem {
            name: ast::Identifier {
                name: mangled.to_string(),
                span: struct_item.name.span,
            },
            generics: None,
            fields,
        }),
        span: struct_item.name.span,
        visibility: ast::Visibility::Private,
        attributes: Vec::new(),
    }
}

fn instantiate_enum(
    enum_item: &ast::EnumItem,
    mangled: &str,
    mapping: &HashMap<String, Type>,
) -> ast::Item {
    let variants = enum_item
        .variants
        .iter()
        .map(|variant| ast::EnumVariant {
            name: variant.name.clone(),
            data: match &variant.data {
                ast::EnumVariantData::Unit => ast::EnumVariantData::Unit,
                ast::EnumVariantData::Tuple(items) => ast::EnumVariantData::Tuple(
                    items
                        .iter()
                        .map(|ty| substitute_ast_type(ty, mapping))
                        .collect(),
                ),
                ast::EnumVariantData::Struct(fields) => ast::EnumVariantData::Struct(
                    fields
                        .iter()
                        .map(|field| ast::Field {
                            name: field.name.clone(),
                            field_type: substitute_ast_type(&field.field_type, mapping),
                            visibility: field.visibility.clone(),
                            tags: field.tags.clone(),
                            span: field.span,
                        })
                        .collect(),
                ),
            },
            discriminant: variant.discriminant,
            span: variant.span,
        })
        .collect::<Vec<_>>();

    ast::Item {
        kind: ast::ItemKind::Enum(ast::EnumItem {
            name: ast::Identifier {
                name: mangled.to_string(),
                span: enum_item.name.span,
            },
            generics: None,
            variants,
        }),
        span: enum_item.name.span,
        visibility: ast::Visibility::Private,
        attributes: Vec::new(),
    }
}

fn instantiate_impls(
    generic_impls: &[ast::ImplItem],
    instantiations: &HashMap<String, TypeInstance>,
    generated: &mut HashSet<String>,
) -> Vec<ast::Item> {
    let mut items = Vec::new();
    for impl_item in generic_impls {
        let Some(base) = impl_self_base_name(&impl_item.self_type) else {
            continue;
        };
        for inst in instantiations.values().filter(|inst| inst.base == base) {
            let key = format!("impl::{base}::{}", inst.mangled);
            if !generated.insert(key.clone()) {
                continue;
            }

            let mapping = inst.mapping.clone();
            if !mapping_covers_impl(&mapping, impl_item.generics.as_ref()) {
                continue;
            }
            // Body-only mapping: when the impl's owner is a GENERIC ENUM, also
            // rewrite bare references to the enum's own name inside method
            // bodies (e.g. `Optional.Some(x)` in `impl Optional<T>`
            // instantiated as `Optional<i32>`) so enum variant construction
            // targets the concrete monomorphized enum. Structs are excluded:
            // rewriting `Vec<T> v;` to `Vec__String v;` would desync local
            // variables from the monomorphized function signature.
            let is_enum_impl = matches!(inst.item.kind, ast::ItemKind::Enum(_));
            let body_mapping = if is_enum_impl {
                let mut body_mapping = mapping.clone();
                body_mapping.insert(
                    base.clone(),
                    Type::Named {
                        path: vec![inst.mangled.clone()],
                        generics: Vec::new(),
                    },
                );
                body_mapping
            } else {
                mapping.clone()
            };
            let mut new_impl = impl_item.clone();
            new_impl.generics = None;
            new_impl.self_type = substitute_ast_type(&impl_item.self_type, &mapping);
            for item in &mut new_impl.items {
                if let ast::ImplItemKind::Function(func) = item {
                    let mut method_mapping = mapping.clone();
                    let mut method_body_mapping = body_mapping.clone();
                    if let Some(generics) = &func.generics {
                        for param in &generics.params {
                            if let ast::GenericParam::Type(param) = param {
                                method_mapping.remove(&param.name.name);
                                method_body_mapping.remove(&param.name.name);
                            }
                        }
                    }
                    func.attributes.push(monomorph_instance_attr());
                    for param in &mut func.parameters {
                        param.param_type = substitute_ast_type(&param.param_type, &method_mapping);
                    }
                    if let Some(return_type) = &mut func.return_type {
                        *return_type = substitute_ast_type(return_type, &method_mapping);
                    }
                    substitute_block_types(&mut func.body, &method_body_mapping);
                }
            }
            items.push(ast::Item {
                kind: ast::ItemKind::Impl(new_impl),
                span: impl_item.self_type.span,
                visibility: ast::Visibility::Private,
                attributes: Vec::new(),
            });
        }
    }

    items
}

fn monomorph_request_order_key(
    request: &MonomorphRequest,
) -> (u32, usize, usize, u32, u32, u32, u32, String) {
    let span = request.call_span();
    let symbol = match request {
        MonomorphRequest::Function {
            source,
            type_params,
            mapping,
            ..
        } => {
            let args = ordered_args(type_params, mapping);
            format!("fn::{}", mangle_function_instance(source, &args, mapping))
        }
        MonomorphRequest::ImplMethod {
            impl_item,
            method,
            type_params,
            mapping,
            ..
        } => {
            let args = ordered_args(type_params, mapping);
            let base = impl_self_base_name(&impl_item.self_type).unwrap_or_default();
            let args = args
                .iter()
                .map(Type::canonical_key)
                .collect::<Vec<_>>()
                .join(",");
            format!("impl::{base}::{}<{args}>", method.name.name)
        }
    };
    (
        span.file,
        span.start,
        span.end,
        span.start_line,
        span.start_col,
        span.end_line,
        span.end_col,
        symbol,
    )
}

fn instantiate_requests(
    program: &mut ast::Program,
    requests: &[MonomorphRequest],
    generated: &mut HashSet<String>,
    imported_fn_templates: &HashMap<String, ast::FunctionItem>,
) -> Vec<ast::Item> {
    let mut ordered_requests: Vec<_> = requests.iter().collect();
    ordered_requests.sort_by_key(|request| monomorph_request_order_key(request));
    let structs = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ast::ItemKind::Struct(definition) => {
                Some((definition.name.name.clone(), definition.clone()))
            }
            _ => None,
        })
        .collect::<HashMap<_, _>>();
    let mut items = Vec::new();
    for request in ordered_requests {
        match request {
            MonomorphRequest::Function {
                source,
                type_params,
                mapping,
                call_span,
                is_imported,
            } => {
                // Prefer a real template body over the import stub: the
                // defining unit cannot pre-instantiate downstream concrete
                // types, so the consuming unit emits the instance itself.
                let template;
                let (source, is_imported) = if *is_imported {
                    if let Some(tmpl) = imported_fn_templates.get(&source.name.name) {
                        template = tmpl.clone();
                        (&template, false)
                    } else {
                        (source.as_ref(), true)
                    }
                } else {
                    (source.as_ref(), false)
                };
                let source = source;
                let args = ordered_args(type_params, mapping);

                let mangled = mangle_function_instance(source, &args, mapping);
                let key = format!("fn::{mangled}");

                // Always rewrite call sites, even if the function was already
                // monomorphized from a different call site. The dedup below
                // prevents generating duplicate function definitions.
                rewrite_function_calls(
                    program,
                    &source.name.name,
                    &args,
                    &mangled,
                    call_span,
                    source.parameters.len(),
                );

                if !generated.insert(key) {
                    continue;
                }
                let mut func = source.clone();
                func.generics = None;
                func.name = ast::Identifier {
                    name: mangled,
                    span: func.name.span,
                };
                for param in &mut func.parameters {
                    param.param_type = substitute_ast_type(&param.param_type, mapping);
                }
                if let Some(return_type) = &mut func.return_type {
                    *return_type = substitute_ast_type(return_type, mapping);
                }
                substitute_block_types(&mut func.body, mapping);

                // Imported generic functions have no body in this unit: emit
                // an external declaration (marked with the synthetic
                // `agm_import` attribute; codegen skips the body) so the
                // mangled instance resolves against the library object.
                let attributes = if is_imported {
                    vec![ast::Attribute {
                        name: ast::Identifier {
                            name: "agm_import".to_string(),
                            span: Span::default(),
                        },
                        args: Vec::new(),
                        span: Span::default(),
                    }]
                } else {
                    vec![monomorph_instance_attr()]
                };

                items.push(ast::Item {
                    kind: ast::ItemKind::Function(func),
                    span: source.name.span,
                    visibility: if is_imported {
                        ast::Visibility::Public
                    } else {
                        ast::Visibility::Private
                    },
                    attributes,
                });
            }
            MonomorphRequest::ImplMethod {
                impl_item,
                method,
                type_params,
                mapping,
                call_span,
            } => {
                let impl_item = impl_item.as_ref();
                let method = method.as_ref();
                let has_method_generics = method.generics.is_some();
                let args = ordered_args(type_params, mapping);
                let base = impl_self_base_name(&impl_item.self_type).unwrap_or_default();
                let concrete_self_type = substitute_ast_type(&impl_item.self_type, mapping);
                let (concrete_owner, owner_args) = impl_self_owner(&concrete_self_type);
                let owner_params = impl_type_params(impl_item);
                let has_unresolved_owner_arg = owner_params.iter().any(|name| {
                    matches!(
                        mapping.get(name),
                        Some(Type::Named { path, generics })
                            if path.len() == 1 && path[0] == *name && generics.is_empty()
                    )
                });
                let is_unresolved_template_call = has_unresolved_owner_arg
                    && impl_item.items.iter().any(|item| {
                        let ast::ImplItemKind::Function(func) = item else {
                            return false;
                        };
                        func.span.file == call_span.file
                            && func.span.start <= call_span.start
                            && func.span.end >= call_span.end
                    });
                let method_args = args.iter().map(Type::canonical_key).collect::<Vec<_>>();
                let concrete_params = method
                    .parameters
                    .iter()
                    .map(|param| {
                        Type::from_ast(&param.param_type)
                            .substitute(mapping)
                            .canonical_key()
                    })
                    .collect::<Vec<_>>();
                let concrete_return = method
                    .return_type
                    .as_ref()
                    .map(|ty| Type::from_ast(ty).substitute(mapping).canonical_key());
                let concrete_method_name = if has_method_generics {
                    crate::mangling::generic_instance_symbol(
                        &method.name.name,
                        &method_args,
                        &concrete_params,
                        concrete_return.as_deref(),
                        method.is_variadic,
                    )
                } else {
                    method.name.name.clone()
                };
                let specialized_name = crate::mangling::method_symbol(
                    &concrete_owner,
                    &concrete_method_name,
                    &concrete_params,
                    concrete_return.as_deref(),
                    method.is_variadic,
                );
                let key = format!("impl::{specialized_name}");
                // Rewrite concrete call sites before deduplication; different
                // requests may target different method specializations.
                // Keep template-body calls on the generic method path until
                // their owner is instantiated; an identity mapping can look
                // concrete when a global type shares the parameter's name.
                if !is_unresolved_template_call {
                    rewrite_method_calls(
                        program,
                        &base,
                        &method.name.name,
                        &concrete_method_name,
                        &owner_args,
                        call_span,
                        &structs,
                    );
                }
                // A method's generic arguments do not belong to its owner type.
                // Emit only this method so another specialization on the same
                // owner cannot shadow it or erase its generic signature.
                if !generated.insert(key) {
                    continue;
                }
                let mut new_impl = impl_item.clone();
                new_impl.generics = None;
                new_impl.self_type = substitute_ast_type(&impl_item.self_type, mapping);
                let mut method = method.clone();
                method.generics = None;
                method.name.name = concrete_method_name;
                method.attributes.push(monomorph_instance_attr());
                for param in &mut method.parameters {
                    param.param_type = substitute_ast_type(&param.param_type, mapping);
                }
                if let Some(return_type) = &mut method.return_type {
                    *return_type = substitute_ast_type(return_type, mapping);
                }
                substitute_block_types(&mut method.body, mapping);
                new_impl.items = vec![ast::ImplItemKind::Function(Box::new(method))];

                items.push(ast::Item {
                    kind: ast::ItemKind::Impl(new_impl),
                    span: impl_item.self_type.span,
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                });
            }
        }
    }
    items
}

fn substitute_block_types(block: &mut ast::Block, mapping: &HashMap<String, Type>) {
    for stmt in &mut block.statements {
        substitute_statement_types(stmt, mapping);
    }
}

fn substitute_statement_types(stmt: &mut ast::Statement, mapping: &HashMap<String, Type>) {
    match &mut stmt.kind {
        ast::StatementKind::Block(block) => substitute_block_types(block, mapping),
        ast::StatementKind::Let(let_stmt) => {
            if let Some(annotation) = &mut let_stmt.type_annotation {
                *annotation = substitute_ast_type(annotation, mapping);
            }
            if let Some(init) = &mut let_stmt.initializer {
                substitute_expression_types(init, mapping);
            }
        }
        ast::StatementKind::Expression(expr)
        | ast::StatementKind::Return(Some(expr))
        | ast::StatementKind::Break(Some(expr)) => substitute_expression_types(expr, mapping),
        ast::StatementKind::Return(None) | ast::StatementKind::Break(None) => {}
        ast::StatementKind::Continue => {}
        ast::StatementKind::Defer(inner) => substitute_statement_types(inner, mapping),
    }
}

fn substitute_expression_types(expr: &mut ast::Expression, mapping: &HashMap<String, Type>) {
    match expr.kind.as_mut() {
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            substitute_expression_types(condition, mapping);
            substitute_expression_types(then_expr, mapping);
            substitute_expression_types(else_expr, mapping);
        }
        ast::ExpressionKind::UnwrapOr { value, fallback } => {
            substitute_expression_types(value, mapping);
            substitute_expression_types(fallback, mapping);
        }
        ast::ExpressionKind::TypeName(ty) => {
            *ty = substitute_ast_type(ty, mapping);
        }
        ast::ExpressionKind::Identifier(ident) => {
            if let Some(rewrite) = mapping.get(&ident.name) {
                let ty = type_to_ast(rewrite, ident.span);
                *expr.kind = ast::ExpressionKind::TypeName(ty);
            }
        }
        ast::ExpressionKind::StructLiteral { path, fields } => {
            if let Some(last) = path.last_mut()
                && let Some(rewrite) = mapping.get(&last.name)
                && let Type::Named { path: new_path, .. } = rewrite
                && let Some(new_name) = new_path.last()
            {
                last.name = new_name.clone();
            }
            for field in fields {
                substitute_expression_types(&mut field.value, mapping);
            }
        }
        ast::ExpressionKind::Cast {
            expression,
            target_type,
        } => {
            let replaced = substitute_ast_type(target_type.as_ref(), mapping);
            **target_type = replaced;
            substitute_expression_types(expression, mapping);
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            substitute_expression_types(function, mapping);
            for arg in arguments {
                substitute_expression_types(arg, mapping);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            substitute_expression_types(receiver, mapping);
            for arg in arguments {
                substitute_expression_types(arg, mapping);
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. }
        | ast::ExpressionKind::Slice { object, .. } => {
            substitute_expression_types(object, mapping);
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            substitute_expression_types(left, mapping);
            substitute_expression_types(right, mapping);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. } => {
            substitute_expression_types(operand, mapping);
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            substitute_expression_types(condition, mapping);
            substitute_block_types(then_branch, mapping);
            if let Some(branch) = else_branch {
                substitute_block_types(branch, mapping);
            }
        }
        ast::ExpressionKind::While { condition, body } => {
            substitute_expression_types(condition, mapping);
            substitute_block_types(body, mapping);
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(init_expr) = &mut init.initializer {
                substitute_expression_types(init_expr, mapping);
            }
            substitute_expression_types(condition, mapping);
            substitute_expression_types(increment, mapping);
            substitute_block_types(body, mapping);
        }
        ast::ExpressionKind::Match { expression, arms } => {
            substitute_expression_types(expression, mapping);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    substitute_expression_types(guard, mapping);
                }
                substitute_expression_types(&mut arm.body, mapping);
            }
        }
        ast::ExpressionKind::Block(block) => substitute_block_types(block, mapping),
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr)
                    | ast::InitializerItem::Field { value: expr, .. } => {
                        substitute_expression_types(expr, mapping)
                    }
                    ast::InitializerItem::Index { index, value } => {
                        substitute_expression_types(index, mapping);
                        substitute_expression_types(value, mapping);
                    }
                }
            }
        }
        ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
            for item in items {
                substitute_expression_types(item, mapping);
            }
        }
        ast::ExpressionKind::Move(inner)
        | ast::ExpressionKind::Comptime(inner)
        | ast::ExpressionKind::Launch(inner)
        | ast::ExpressionKind::Wait(inner)
        | ast::ExpressionKind::Reference {
            expression: inner, ..
        } => substitute_expression_types(inner, mapping),
        ast::ExpressionKind::MacroCall { args, .. } => {
            for arg in args {
                if let ast::MacroArg::Expression(expr) = arg {
                    substitute_expression_types(expr, mapping);
                }
            }
        }
        ast::ExpressionKind::Literal(_) => {}
        ast::ExpressionKind::Asm { inputs, .. } => {
            for input in inputs {
                substitute_expression_types(input, mapping);
            }
        }
        ast::ExpressionKind::ForIn {
            iterable,
            body,
            item_type,
            iterator_type,
            ..
        } => {
            substitute_expression_types(iterable, mapping);
            substitute_block_types(body, mapping);
            if let Some(item_type) = item_type {
                *item_type.as_mut() = substitute_ast_type(item_type, mapping);
            }
            if let Some(iterator_type) = iterator_type {
                *iterator_type.as_mut() = substitute_ast_type(iterator_type, mapping);
            }
        }
        ast::ExpressionKind::EnumVariant { fields, .. } => {
            for field in fields {
                substitute_expression_types(field, mapping);
            }
        }
    }
}

fn substitute_ast_type(ty: &ast::Type, mapping: &HashMap<String, Type>) -> ast::Type {
    let concrete = Type::from_ast(ty).substitute(mapping);
    type_to_ast(&concrete, ty.span)
}

fn rewrite_function_calls(
    program: &mut ast::Program,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
    param_count: usize,
) {
    for item in &mut program.items {
        rewrite_item_function_calls(item, name, args, mangled, span, param_count);
    }
}

fn rewrite_item_function_calls(
    item: &mut ast::Item,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
    param_count: usize,
) {
    match &mut item.kind {
        ast::ItemKind::Function(func) => {
            rewrite_block_function_calls(&mut func.body, name, args, mangled, span, param_count)
        }
        ast::ItemKind::Impl(impl_item) => {
            for impl_item in &mut impl_item.items {
                if let ast::ImplItemKind::Function(func) = impl_item {
                    rewrite_block_function_calls(
                        &mut func.body,
                        name,
                        args,
                        mangled,
                        span,
                        param_count,
                    )
                }
            }
        }
        _ => {}
    }
}

fn rewrite_block_function_calls(
    block: &mut ast::Block,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
    param_count: usize,
) {
    for stmt in &mut block.statements {
        rewrite_statement_function_calls(stmt, name, args, mangled, span, param_count);
    }
}

fn rewrite_statement_function_calls(
    stmt: &mut ast::Statement,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
    param_count: usize,
) {
    match &mut stmt.kind {
        ast::StatementKind::Block(block) => {
            rewrite_block_function_calls(block, name, args, mangled, span, param_count)
        }
        ast::StatementKind::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.initializer {
                rewrite_expression_function_calls(init, name, args, mangled, span, param_count);
            }
        }
        ast::StatementKind::Expression(expr)
        | ast::StatementKind::Return(Some(expr))
        | ast::StatementKind::Break(Some(expr)) => {
            rewrite_expression_function_calls(expr, name, args, mangled, span, param_count)
        }
        ast::StatementKind::Return(None) | ast::StatementKind::Break(None) => {}
        ast::StatementKind::Continue => {}
        ast::StatementKind::Defer(inner) => {
            rewrite_statement_function_calls(inner, name, args, mangled, span, param_count)
        }
    }
}

fn rewrite_expression_function_calls(
    expr: &mut ast::Expression,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
    param_count: usize,
) {
    match expr.kind.as_mut() {
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            rewrite_expression_function_calls(condition, name, args, mangled, span, param_count);
            rewrite_expression_function_calls(then_expr, name, args, mangled, span, param_count);
            rewrite_expression_function_calls(else_expr, name, args, mangled, span, param_count);
        }
        ast::ExpressionKind::UnwrapOr { value, fallback } => {
            rewrite_expression_function_calls(value, name, args, mangled, span, param_count);
            rewrite_expression_function_calls(fallback, name, args, mangled, span, param_count);
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            // For TypeName-style calls, match by function name, generic argument types,
            // AND value parameter count. This handles nested generic calls inside
            // monomorphized bodies where the span differs from the original request's
            // call_span, and prevents one overload from rewriting another.
            let should_rewrite = match function.kind.as_mut() {
                ast::ExpressionKind::Identifier(ident) => ident.name == name && expr.span == *span,
                ast::ExpressionKind::TypeName(ty) => {
                    if let ast::TypeKind::Named(named) = ty.kind.as_mut() {
                        let name_ok = named.path.len() == 1 && named.path[0].name == name;
                        let param_ok = arguments.len() == param_count;
                        let generics_ok = match &named.generics {
                            Some(generics) => {
                                let actual_args: Vec<Type> =
                                    generics.iter().map(Type::from_ast).collect();

                                actual_args == args
                            }
                            None => args.is_empty(),
                        };
                        name_ok && param_ok && generics_ok
                    } else {
                        false
                    }
                }
                _ => false,
            };
            if should_rewrite {
                **function = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: mangled.to_string(),
                        span: function.span,
                    })),
                    span: function.span,
                };
            }
            for arg in arguments {
                rewrite_expression_function_calls(arg, name, args, mangled, span, param_count);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            rewrite_expression_function_calls(receiver, name, args, mangled, span, param_count);
            for arg in arguments {
                rewrite_expression_function_calls(arg, name, args, mangled, span, param_count);
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. }
        | ast::ExpressionKind::Slice { object, .. } => {
            rewrite_expression_function_calls(object, name, args, mangled, span, param_count);
        }
        ast::ExpressionKind::Cast { expression, .. } => {
            rewrite_expression_function_calls(expression, name, args, mangled, span, param_count);
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            rewrite_expression_function_calls(left, name, args, mangled, span, param_count);
            rewrite_expression_function_calls(right, name, args, mangled, span, param_count);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. } => {
            rewrite_expression_function_calls(operand, name, args, mangled, span, param_count)
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rewrite_expression_function_calls(condition, name, args, mangled, span, param_count);
            rewrite_block_function_calls(then_branch, name, args, mangled, span, param_count);
            if let Some(branch) = else_branch {
                rewrite_block_function_calls(branch, name, args, mangled, span, param_count);
            }
        }
        ast::ExpressionKind::While { condition, body } => {
            rewrite_expression_function_calls(condition, name, args, mangled, span, param_count);
            rewrite_block_function_calls(body, name, args, mangled, span, param_count);
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(init_expr) = &mut init.initializer {
                rewrite_expression_function_calls(
                    init_expr,
                    name,
                    args,
                    mangled,
                    span,
                    param_count,
                );
            }
            rewrite_expression_function_calls(condition, name, args, mangled, span, param_count);
            rewrite_expression_function_calls(increment, name, args, mangled, span, param_count);
            rewrite_block_function_calls(body, name, args, mangled, span, param_count);
        }
        ast::ExpressionKind::Match { expression, arms } => {
            rewrite_expression_function_calls(expression, name, args, mangled, span, param_count);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    rewrite_expression_function_calls(
                        guard,
                        name,
                        args,
                        mangled,
                        span,
                        param_count,
                    );
                }
                rewrite_expression_function_calls(
                    &mut arm.body,
                    name,
                    args,
                    mangled,
                    span,
                    param_count,
                );
            }
        }
        ast::ExpressionKind::Block(block) => {
            rewrite_block_function_calls(block, name, args, mangled, span, param_count)
        }
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr)
                    | ast::InitializerItem::Field { value: expr, .. } => {
                        rewrite_expression_function_calls(
                            expr,
                            name,
                            args,
                            mangled,
                            span,
                            param_count,
                        )
                    }
                    ast::InitializerItem::Index { index, value } => {
                        rewrite_expression_function_calls(
                            index,
                            name,
                            args,
                            mangled,
                            span,
                            param_count,
                        );
                        rewrite_expression_function_calls(
                            value,
                            name,
                            args,
                            mangled,
                            span,
                            param_count,
                        );
                    }
                }
            }
        }
        ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
            for item in items {
                rewrite_expression_function_calls(item, name, args, mangled, span, param_count);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                rewrite_expression_function_calls(
                    &mut field.value,
                    name,
                    args,
                    mangled,
                    span,
                    param_count,
                );
            }
        }
        ast::ExpressionKind::Move(inner)
        | ast::ExpressionKind::Comptime(inner)
        | ast::ExpressionKind::Launch(inner)
        | ast::ExpressionKind::Wait(inner)
        | ast::ExpressionKind::Reference {
            expression: inner, ..
        } => rewrite_expression_function_calls(inner, name, args, mangled, span, param_count),
        ast::ExpressionKind::MacroCall {
            args: macro_args, ..
        } => {
            for arg in macro_args {
                if let ast::MacroArg::Expression(expr) = arg {
                    rewrite_expression_function_calls(expr, name, args, mangled, span, param_count);
                }
            }
        }
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            rewrite_expression_function_calls(iterable, name, args, mangled, span, param_count);
            rewrite_block_function_calls(body, name, args, mangled, span, param_count);
        }
        ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::EnumVariant { .. } => {}
        ast::ExpressionKind::Asm { inputs, .. } => {
            for input in inputs {
                rewrite_expression_function_calls(input, name, args, mangled, span, param_count);
            }
        }
    }
}

fn rewrite_method_calls(
    program: &mut ast::Program,
    base: &str,
    method: &str,
    specialized_name: &str,
    args: &[Type],
    span: &Span,
    structs: &HashMap<String, ast::StructItem>,
) {
    let contains_call =
        |body: Span| body.file == span.file && body.start <= span.start && body.end >= span.end;
    let matching_bodies: usize = program
        .items
        .iter()
        .map(|item| match &item.kind {
            ast::ItemKind::Function(function) => usize::from(contains_call(function.body.span)),
            ast::ItemKind::Impl(implementation) => implementation
                .items
                .iter()
                .filter(|member| {
                    matches!(member, ast::ImplItemKind::Function(function) if contains_call(function.span))
                })
                .count(),
            _ => 0,
        })
        .sum();
    let rewrite = MethodCallRewrite {
        base,
        method,
        specialized_name,
        args,
        span,
        structs,
        unique_call_site: matching_bodies == 1,
    };
    for item in &mut program.items {
        match &mut item.kind {
            ast::ItemKind::Function(func) if contains_call(func.body.span) => {
                let mut scope = rewrite.parameter_scope(&func.parameters, None);
                rewrite.block(&mut func.body, &mut scope);
            }
            ast::ItemKind::Impl(impl_item) => {
                let owner = Type::from_ast(&impl_item.self_type);
                for member in &mut impl_item.items {
                    if let ast::ImplItemKind::Function(func) = member
                        && contains_call(func.span)
                    {
                        let mut scope = rewrite.parameter_scope(&func.parameters, Some(&owner));
                        rewrite.block(&mut func.body, &mut scope);
                    }
                }
            }
            _ => {}
        }
    }
}

struct MethodCallRewrite<'a> {
    base: &'a str,
    method: &'a str,
    specialized_name: &'a str,
    args: &'a [Type],
    span: &'a Span,
    structs: &'a HashMap<String, ast::StructItem>,
    unique_call_site: bool,
}

impl MethodCallRewrite<'_> {
    fn parameter_scope(&self, params: &[ast::Parameter], owner: Option<&Type>) -> BareCallScope {
        let mapping = owner
            .map(|ty| HashMap::from_iter([("Self".to_string(), ty.clone())]))
            .unwrap_or_default();
        params
            .iter()
            .map(|param| {
                (
                    param.name.name.clone(),
                    Some(Type::from_ast(&param.param_type).substitute(&mapping)),
                )
            })
            .collect()
    }

    fn receiver_type(&self, receiver: &ast::Expression, scope: &BareCallScope) -> Option<Type> {
        match receiver.kind.as_ref() {
            ast::ExpressionKind::TypeName(ty) => Some(Type::from_ast(ty)),
            ast::ExpressionKind::Cast { target_type, .. } => Some(Type::from_ast(target_type)),
            ast::ExpressionKind::FieldAccess { object, field } => {
                let owner = self.receiver_type(object, scope)?;
                let mut owner = &owner;
                while let Type::Pointer { inner, .. } | Type::Reference { inner, .. } = owner {
                    owner = inner;
                }
                let Type::Named { path, generics } = owner else {
                    return None;
                };
                let definition = self.structs.get(path.last()?)?;
                let field = definition
                    .fields
                    .iter()
                    .find(|candidate| candidate.name.name == field.name)?;
                let mapping = build_mapping_from_generics(definition.generics.as_ref(), generics);
                Some(Type::from_ast(&field.field_type).substitute(&mapping))
            }
            ast::ExpressionKind::Move(inner) | ast::ExpressionKind::Comptime(inner) => {
                self.receiver_type(inner, scope)
            }
            ast::ExpressionKind::Reference {
                is_mutable,
                expression,
            } => self
                .receiver_type(expression, scope)
                .map(|inner| Type::Pointer {
                    is_mutable: *is_mutable,
                    is_volatile: false,
                    inner: Box::new(inner),
                }),
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => match self.receiver_type(operand, scope)? {
                Type::Pointer { inner, .. } | Type::Reference { inner, .. } => Some(*inner),
                _ => None,
            },
            _ => infer_bare_arg_type(receiver, scope),
        }
    }

    fn receiver_matches(&self, receiver: &ast::Expression, scope: &BareCallScope) -> bool {
        if matches!(receiver.kind.as_ref(), ast::ExpressionKind::Identifier(id)
            if id.name == self.base && !scope.contains_key(&id.name))
        {
            return true;
        }
        let ty = self.receiver_type(receiver, scope);
        let Some(mut ty) = ty.as_ref() else {
            // Semantic analysis identifies a unique call directly. Cloned
            // bodies share spans, so they also need a concrete owner match.
            return self.unique_call_site;
        };
        while let Type::Pointer { inner, .. } | Type::Reference { inner, .. } = ty {
            ty = inner;
        }
        match ty {
            Type::Primitive(_) => self.args.is_empty() && ty.canonical_key() == self.base,
            Type::Named { path, generics } => path.last().is_some_and(|name| {
                (name == self.base && generics == self.args)
                    || (generics.is_empty() && name == &mangle_name(self.base, self.args))
            }),
            _ => false,
        }
    }

    fn block(&self, block: &mut ast::Block, scope: &mut BareCallScope) {
        let saved = scope.clone();
        for statement in &mut block.statements {
            self.statement(statement, scope);
        }
        *scope = saved;
    }

    fn bind_let(&self, binding: &ast::LetStatement, scope: &mut BareCallScope) {
        let ty = binding
            .type_annotation
            .as_ref()
            .map(Type::from_ast)
            .or_else(|| {
                binding
                    .initializer
                    .as_ref()
                    .and_then(|init| infer_bare_arg_type(init, scope))
            });
        BareCallDiscoverer::bind_pattern(&binding.pattern, ty, scope);
    }

    fn statement(&self, statement: &mut ast::Statement, scope: &mut BareCallScope) {
        match &mut statement.kind {
            ast::StatementKind::Block(block) => self.block(block, scope),
            ast::StatementKind::Let(binding) => {
                if let Some(init) = &mut binding.initializer {
                    self.expression(init, scope);
                }
                self.bind_let(binding, scope);
            }
            ast::StatementKind::Expression(expr)
            | ast::StatementKind::Return(Some(expr))
            | ast::StatementKind::Break(Some(expr)) => self.expression(expr, scope),
            ast::StatementKind::Defer(inner) => self.statement(inner, scope),
            _ => {}
        }
    }

    fn expression(&self, expr: &mut ast::Expression, scope: &mut BareCallScope) {
        match expr.kind.as_mut() {
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                if expr.span == *self.span
                    && method.name == self.method
                    && self.receiver_matches(receiver, scope)
                {
                    method.name = self.specialized_name.to_string();
                    let static_receiver =
                        matches!(receiver.kind.as_ref(), ast::ExpressionKind::TypeName(_))
                            || matches!(receiver.kind.as_ref(), ast::ExpressionKind::Identifier(id)
                            if id.name == self.base && !scope.contains_key(&id.name));
                    if static_receiver
                        && !matches!(receiver.kind.as_ref(), ast::ExpressionKind::TypeName(ty)
                            if matches!(ty.kind.as_ref(), ast::TypeKind::Primitive(_)))
                    {
                        let named = ast::NamedType {
                            path: vec![ast::Identifier {
                                name: self.base.to_string(),
                                span: receiver.span,
                            }],
                            generics: Some(
                                self.args
                                    .iter()
                                    .map(|arg| type_to_ast(arg, receiver.span))
                                    .collect(),
                            ),
                        };
                        *receiver.kind = ast::ExpressionKind::TypeName(ast::Type {
                            kind: Box::new(ast::TypeKind::Named(named)),
                            span: receiver.span,
                        });
                    }
                }
                self.expression(receiver, scope);
                for argument in arguments {
                    self.expression(argument, scope);
                }
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                self.expression(function, scope);
                for argument in arguments {
                    self.expression(argument, scope);
                }
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.expression(condition, scope);
                self.expression(then_expr, scope);
                self.expression(else_expr, scope);
            }
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                self.expression(value, scope);
                self.expression(fallback, scope);
            }
            ast::ExpressionKind::FieldAccess { object, .. } => self.expression(object, scope),
            ast::ExpressionKind::Index { object, index } => {
                self.expression(object, scope);
                self.expression(index, scope);
            }
            ast::ExpressionKind::Slice { object, .. } => self.expression(object, scope),
            ast::ExpressionKind::Cast { expression, .. } => self.expression(expression, scope),
            ast::ExpressionKind::Binary { left, right, .. } => {
                self.expression(left, scope);
                self.expression(right, scope);
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. } => self.expression(operand, scope),
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.expression(condition, scope);
                self.block(then_branch, scope);
                if let Some(branch) = else_branch {
                    self.block(branch, scope);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                self.expression(condition, scope);
                self.block(body, scope);
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                let mut inner = scope.clone();
                if let Some(value) = &mut init.initializer {
                    self.expression(value, &mut inner);
                }
                self.bind_let(init, &mut inner);
                self.expression(condition, &mut inner);
                self.expression(increment, &mut inner);
                self.block(body, &mut inner);
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.expression(expression, scope);
                for arm in arms {
                    let mut inner = scope.clone();
                    BareCallDiscoverer::bind_pattern(&arm.pattern, None, &mut inner);
                    if let Some(guard) = &mut arm.guard {
                        self.expression(guard, &mut inner);
                    }
                    self.expression(&mut arm.body, &mut inner);
                }
            }
            ast::ExpressionKind::Block(block) => self.block(block, scope),
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(value)
                        | ast::InitializerItem::Field { value, .. } => {
                            self.expression(value, scope)
                        }
                        ast::InitializerItem::Index { index, value } => {
                            self.expression(index, scope);
                            self.expression(value, scope);
                        }
                    }
                }
            }
            ast::ExpressionKind::Array(items)
            | ast::ExpressionKind::Tuple(items)
            | ast::ExpressionKind::EnumVariant { fields: items, .. } => {
                for item in items {
                    self.expression(item, scope);
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.expression(&mut field.value, scope);
                }
            }
            ast::ExpressionKind::Move(inner)
            | ast::ExpressionKind::Comptime(inner)
            | ast::ExpressionKind::Launch(inner)
            | ast::ExpressionKind::Wait(inner)
            | ast::ExpressionKind::Reference {
                expression: inner, ..
            } => self.expression(inner, scope),
            ast::ExpressionKind::ForIn {
                iterable,
                body,
                binding,
                item_type,
                ..
            } => {
                self.expression(iterable, scope);
                let mut inner = scope.clone();
                inner.insert(
                    binding.name.clone(),
                    item_type.as_deref().map(Type::from_ast),
                );
                self.block(body, &mut inner);
            }
            ast::ExpressionKind::Asm { inputs, .. } => {
                for input in inputs {
                    self.expression(input, scope);
                }
            }
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args {
                    if let ast::MacroArg::Expression(value) = arg {
                        self.expression(value, scope);
                    }
                }
            }
            _ => {}
        }
    }
}

fn type_to_ast(ty: &Type, span: Span) -> ast::Type {
    let kind = match ty {
        Type::Unit | Type::Never => ast::TypeKind::Tuple(Vec::new()),
        Type::Primitive(p) => ast::TypeKind::Primitive(p.clone()),
        Type::Named { path, generics } => ast::TypeKind::Named(ast::NamedType {
            path: path
                .iter()
                .map(|name| ast::Identifier {
                    name: name.clone(),
                    span,
                })
                .collect(),
            generics: if generics.is_empty() {
                None
            } else {
                Some(
                    generics
                        .iter()
                        .map(|inner| type_to_ast(inner, span))
                        .collect(),
                )
            },
        }),
        Type::Reference { is_mutable, inner } => ast::TypeKind::Reference(ast::ReferenceType {
            is_mutable: *is_mutable,
            lifetime: None,
            inner: Box::new(type_to_ast(inner, span)),
        }),
        Type::Pointer {
            is_mutable,
            is_volatile,
            inner,
        } => ast::TypeKind::Pointer(ast::PointerType {
            is_mutable: *is_mutable,
            is_volatile: *is_volatile,
            inner: Box::new(type_to_ast(inner, span)),
        }),
        Type::Slice { element } => ast::TypeKind::Named(ast::NamedType {
            path: vec![ast::Identifier {
                name: "Slice".to_string(),
                span,
            }],
            generics: Some(vec![type_to_ast(element, span)]),
        }),
        Type::Task(inner) => ast::TypeKind::Named(ast::NamedType {
            path: vec![ast::Identifier {
                name: "Task".to_string(),
                span,
            }],
            generics: Some(vec![type_to_ast(inner, span)]),
        }),
        Type::Array { element, size } => ast::TypeKind::Array(Box::new(ast::ArrayType {
            element_type: Box::new(type_to_ast(element, span)),
            size: *size as i64,
            span,
        })),
        Type::Optional { inner } => ast::TypeKind::Optional(Box::new(type_to_ast(inner, span))),
        Type::Tuple(items) => {
            ast::TypeKind::Tuple(items.iter().map(|inner| type_to_ast(inner, span)).collect())
        }
        Type::Function {
            params,
            return_type,
        } => ast::TypeKind::Function(ast::FunctionType {
            parameters: params
                .iter()
                .map(|inner| type_to_ast(inner, span))
                .collect(),
            return_type: Box::new(type_to_ast(return_type, span)),
        }),
        Type::Unknown => ast::TypeKind::Named(ast::NamedType {
            path: vec![ast::Identifier {
                name: "_".to_string(),
                span,
            }],
            generics: None,
        }),
    };
    ast::Type {
        kind: Box::new(kind),
        span,
    }
}

fn push_type_params(scopes: &mut Vec<HashSet<String>>, generics: Option<&ast::Generics>) {
    let mut params = HashSet::default();
    if let Some(generics) = generics {
        for param in &generics.params {
            if let ast::GenericParam::Type(type_param) = param {
                params.insert(type_param.name.name.clone());
            }
        }
    }
    scopes.push(params);
}

fn pop_type_params(scopes: &mut Vec<HashSet<String>>) {
    scopes.pop();
}

fn collect_implicit_type_params(ty: &ast::Type, params: &mut HashSet<String>) {
    match ty.kind.as_ref() {
        ast::TypeKind::Named(named) => {
            if let Some(generics) = &named.generics {
                for arg in generics {
                    collect_implicit_type_params(arg, params);
                }
            } else if named.path.len() == 1 {
                params.insert(named.path[0].name.clone());
            }
        }
        ast::TypeKind::Generic(generic) => {
            params.insert(generic.name.name.clone());
            for arg in &generic.args {
                collect_implicit_type_params(arg, params);
            }
        }
        ast::TypeKind::Reference(reference) => {
            collect_implicit_type_params(&reference.inner, params)
        }
        ast::TypeKind::Pointer(pointer) => collect_implicit_type_params(&pointer.inner, params),
        ast::TypeKind::Slice(slice) => collect_implicit_type_params(&slice.element_type, params),
        ast::TypeKind::Array(array) => collect_implicit_type_params(&array.element_type, params),
        ast::TypeKind::Optional(inner) => collect_implicit_type_params(inner, params),
        ast::TypeKind::Tuple(items) => {
            for item in items {
                collect_implicit_type_params(item, params);
            }
        }
        ast::TypeKind::Function(func) => {
            for param in &func.parameters {
                collect_implicit_type_params(param, params);
            }
            collect_implicit_type_params(&func.return_type, params)
        }
        ast::TypeKind::Primitive(_) => {}
    }
}

fn concrete_named_type(
    ty: &ast::Type,
    scopes: &Vec<HashSet<String>>,
) -> Option<(String, Vec<Type>)> {
    let ast::TypeKind::Named(named) = ty.kind.as_ref() else {
        return None;
    };
    let base = named.path.last()?.name.clone();
    let generics = named.generics.as_ref()?;
    let args = generics.iter().map(Type::from_ast).collect::<Vec<_>>();
    if args.iter().all(|arg| is_concrete_type(arg, scopes)) {
        Some((base, args))
    } else {
        None
    }
}

fn is_concrete_type(ty: &Type, scopes: &Vec<HashSet<String>>) -> bool {
    match ty {
        Type::Named { path, generics } => {
            if path.len() == 1 {
                let name = &path[0];
                for scope in scopes.iter().rev() {
                    if scope.contains(name) {
                        return false;
                    }
                }
            }
            generics.iter().all(|inner| is_concrete_type(inner, scopes))
        }
        Type::Array { element, .. } => is_concrete_type(element, scopes),
        Type::Slice { element } => is_concrete_type(element, scopes),
        Type::Task(inner) => is_concrete_type(inner, scopes),
        Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
            is_concrete_type(inner, scopes)
        }
        Type::Optional { inner } => is_concrete_type(inner, scopes),
        Type::Tuple(items) => items.iter().all(|inner| is_concrete_type(inner, scopes)),
        Type::Function {
            params,
            return_type,
        } => {
            params.iter().all(|inner| is_concrete_type(inner, scopes))
                && is_concrete_type(return_type, scopes)
        }
        Type::Primitive(_) | Type::Unit | Type::Never => true,
        Type::Unknown => false,
    }
}

fn build_mapping_from_generics(
    generics: Option<&ast::Generics>,
    args: &[Type],
) -> HashMap<String, Type> {
    let mut mapping = HashMap::default();
    if let Some(generics) = generics {
        for (param, arg) in generics.params.iter().zip(args.iter()) {
            if let ast::GenericParam::Type(type_param) = param {
                mapping.insert(type_param.name.name.clone(), arg.clone());
            }
        }
    }
    mapping
}

fn mapping_covers_impl(mapping: &HashMap<String, Type>, generics: Option<&ast::Generics>) -> bool {
    let Some(generics) = generics else {
        return true;
    };
    for param in &generics.params {
        if let ast::GenericParam::Type(type_param) = param
            && !mapping.contains_key(&type_param.name.name)
        {
            return false;
        }
    }
    true
}

fn impl_type_params(impl_item: &ast::ImplItem) -> Vec<String> {
    let mut params = impl_item.implicit_type_params.clone();
    if let Some(generics) = &impl_item.generics {
        for param in &generics.params {
            if let ast::GenericParam::Type(type_param) = param
                && !params.contains(&type_param.name.name)
            {
                params.push(type_param.name.name.clone());
            }
        }
    }
    params
}

fn is_generic_self_type(ty: &ast::Type) -> bool {
    matches!(ty.kind.as_ref(), ast::TypeKind::Named(named) if named.generics.is_some())
}

fn impl_self_base_name(ty: &ast::Type) -> Option<String> {
    match ty.kind.as_ref() {
        ast::TypeKind::Named(named) => named.path.last().map(|id| id.name.clone()),
        ast::TypeKind::Primitive(_) => Some(Type::from_ast(ty).canonical_key()),
        _ => None,
    }
}

fn impl_self_owner(ty: &ast::Type) -> (String, Vec<Type>) {
    if matches!(ty.kind.as_ref(), ast::TypeKind::Primitive(_)) {
        return (Type::from_ast(ty).canonical_key(), Vec::new());
    }
    let ast::TypeKind::Named(named) = ty.kind.as_ref() else {
        return (String::new(), Vec::new());
    };
    let base = named
        .path
        .last()
        .map(|id| id.name.clone())
        .unwrap_or_default();
    let args = named
        .generics
        .as_ref()
        .into_iter()
        .flatten()
        .map(Type::from_ast)
        .collect::<Vec<_>>();
    (mangle_name(&base, &args), args)
}

pub fn mangle_name(base: &str, args: &[Type]) -> String {
    let mut parts = Vec::new();
    for arg in args {
        parts.push(crate::mangling::sanitize_type_key(&arg.canonical_key()));
    }
    if parts.is_empty() {
        base.to_string()
    } else {
        format!("{}__{}", base, parts.join("_"))
    }
}

fn ordered_args(type_params: &[String], mapping: &HashMap<String, Type>) -> Vec<Type> {
    type_params
        .iter()
        .filter_map(|name| mapping.get(name).cloned())
        .collect()
}

/// Compute the full mangled function name including parameter type signature,
/// enabling disambiguation of overloaded generic functions with different
/// value parameter counts (e.g. alloc<i32>() vs alloc<i32>(i64)).
/// Format: `{name}__{K}_{args}__{P}_{params}__{hash}` — explicit arity
/// counts plus an FNV-1a-64 hash of the complete canonical signature make
/// distinct instances always distinct (see crate::mangling).
pub(crate) fn mangle_function_instance(
    source: &ast::FunctionItem,
    args: &[Type],
    mapping: &HashMap<String, Type>,
) -> String {
    let arg_keys = args
        .iter()
        .map(|arg| arg.canonical_key())
        .collect::<Vec<_>>();
    let param_keys = source
        .parameters
        .iter()
        .map(|param| {
            let concrete = Type::from_ast(&param.param_type).substitute(mapping);
            concrete.canonical_key()
        })
        .collect::<Vec<_>>();
    let ret_key = source
        .return_type
        .as_ref()
        .map(|ret| Type::from_ast(ret).substitute(mapping).canonical_key());
    crate::mangling::generic_instance_symbol(
        &source.name.name,
        &arg_keys,
        &param_keys,
        ret_key.as_deref(),
        source.is_variadic,
    )
}

/// Collect names of all generic functions (functions with generic type parameters).
/// Compute the full mangled function name including parameter type signature,
/// enabling disambiguation of overloaded generic functions with different
/// value parameter counts (e.g. alloc<i32>() vs alloc<i32>(i64)).
fn collect_generic_fns(program: &ast::Program) -> HashSet<String> {
    let mut fns = HashSet::default();
    for item in &program.items {
        if let ast::ItemKind::Function(func) = &item.kind
            && func.generics.is_some()
        {
            fns.insert(func.name.name.clone());
        }
    }
    fns
}

/// Find a generic function by name and parameter count.
fn find_generic_fn<'a>(
    program: &'a ast::Program,
    imported_items: &'a [ast::Item],
    name: &str,
    param_count: usize,
) -> Option<&'a ast::FunctionItem> {
    for item in program.items.iter().chain(imported_items.iter()) {
        if let ast::ItemKind::Function(func) = &item.kind
            && func.name.name == name
            && func.generics.is_some()
            && func.parameters.len() == param_count
        {
            return Some(func);
        }
    }
    None
}

/// Scan an expression tree for TypeName-style calls to generic functions with concrete
/// type arguments, and return the list as MonomorphRequest values so they can be processed
/// through the existing monomorphization pipeline.
fn collect_expression_remaining_calls(
    expr: &ast::Expression,
    generic_fns: &HashSet<String>,
) -> Vec<(String, Vec<Type>, Span, usize)> {
    let mut results = Vec::new();
    match expr.kind.as_ref() {
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            results.extend(collect_expression_remaining_calls(condition, generic_fns));
            results.extend(collect_expression_remaining_calls(then_expr, generic_fns));
            results.extend(collect_expression_remaining_calls(else_expr, generic_fns));
        }
        ast::ExpressionKind::UnwrapOr { value, fallback } => {
            results.extend(collect_expression_remaining_calls(value, generic_fns));
            results.extend(collect_expression_remaining_calls(fallback, generic_fns));
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            if let ast::ExpressionKind::TypeName(ty) = function.kind.as_ref()
                && let ast::TypeKind::Named(named) = ty.kind.as_ref()
                && named.path.len() == 1
            {
                let fn_name = &named.path[0].name;
                if generic_fns.contains(fn_name)
                    && let Some(generics) = &named.generics
                {
                    let concrete_args: Vec<Type> = generics.iter().map(Type::from_ast).collect();
                    // Only process if all type args are concrete
                    if concrete_args.iter().all(is_concrete) {
                        results.push((fn_name.clone(), concrete_args, expr.span, arguments.len()));
                    }
                }
            }
            // Recurse into arguments
            for arg in arguments {
                results.extend(collect_expression_remaining_calls(arg, generic_fns));
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            results.extend(collect_expression_remaining_calls(receiver, generic_fns));

            for arg in arguments {
                results.extend(collect_expression_remaining_calls(arg, generic_fns));
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. }
        | ast::ExpressionKind::Slice { object, .. } => {
            results.extend(collect_expression_remaining_calls(object, generic_fns));
        }
        ast::ExpressionKind::Cast { expression, .. } => {
            results.extend(collect_expression_remaining_calls(expression, generic_fns));
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            results.extend(collect_expression_remaining_calls(left, generic_fns));
            results.extend(collect_expression_remaining_calls(right, generic_fns));
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. } => {
            results.extend(collect_expression_remaining_calls(operand, generic_fns));
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            results.extend(collect_expression_remaining_calls(condition, generic_fns));
            results.extend(collect_block_remaining_calls(then_branch, generic_fns));
            if let Some(branch) = else_branch {
                results.extend(collect_block_remaining_calls(branch, generic_fns));
            }
        }
        ast::ExpressionKind::While { condition, body } => {
            results.extend(collect_expression_remaining_calls(condition, generic_fns));
            results.extend(collect_block_remaining_calls(body, generic_fns));
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(init_expr) = &init.initializer {
                results.extend(collect_expression_remaining_calls(init_expr, generic_fns));
            }
            results.extend(collect_expression_remaining_calls(condition, generic_fns));
            results.extend(collect_expression_remaining_calls(increment, generic_fns));
            results.extend(collect_block_remaining_calls(body, generic_fns));
        }
        ast::ExpressionKind::Match { expression, arms } => {
            results.extend(collect_expression_remaining_calls(expression, generic_fns));
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    results.extend(collect_expression_remaining_calls(guard, generic_fns));
                }
                results.extend(collect_expression_remaining_calls(&arm.body, generic_fns));
            }
        }
        ast::ExpressionKind::Block(block) => {
            results.extend(collect_block_remaining_calls(block, generic_fns));
        }
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr)
                    | ast::InitializerItem::Field { value: expr, .. } => {
                        results.extend(collect_expression_remaining_calls(expr, generic_fns));
                    }
                    ast::InitializerItem::Index { index, value } => {
                        results.extend(collect_expression_remaining_calls(index, generic_fns));
                        results.extend(collect_expression_remaining_calls(value, generic_fns));
                    }
                }
            }
        }
        ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
            for item in items {
                results.extend(collect_expression_remaining_calls(item, generic_fns));
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                results.extend(collect_expression_remaining_calls(
                    &field.value,
                    generic_fns,
                ));
            }
        }
        ast::ExpressionKind::Move(inner)
        | ast::ExpressionKind::Comptime(inner)
        | ast::ExpressionKind::Launch(inner)
        | ast::ExpressionKind::Wait(inner)
        | ast::ExpressionKind::Reference {
            expression: inner, ..
        } => {
            results.extend(collect_expression_remaining_calls(inner, generic_fns));
        }
        ast::ExpressionKind::MacroCall { args, .. } => {
            for arg in args {
                if let ast::MacroArg::Expression(expr) = arg {
                    results.extend(collect_expression_remaining_calls(expr, generic_fns));
                }
            }
        }
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            results.extend(collect_expression_remaining_calls(iterable, generic_fns));
            results.extend(collect_block_remaining_calls(body, generic_fns));
        }
        ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::EnumVariant { .. } => {}
        ast::ExpressionKind::Asm { inputs, .. } => {
            for input in inputs {
                results.extend(collect_expression_remaining_calls(input, generic_fns));
            }
        }
    }
    results
}

fn collect_block_remaining_calls(
    block: &ast::Block,
    generic_fns: &HashSet<String>,
) -> Vec<(String, Vec<Type>, Span, usize)> {
    let mut results = Vec::new();
    for stmt in &block.statements {
        results.extend(collect_statement_remaining_calls(stmt, generic_fns));
    }
    results
}

fn collect_statement_remaining_calls(
    stmt: &ast::Statement,
    generic_fns: &HashSet<String>,
) -> Vec<(String, Vec<Type>, Span, usize)> {
    let mut results = Vec::new();
    match &stmt.kind {
        ast::StatementKind::Block(block) => {
            results.extend(collect_block_remaining_calls(block, generic_fns));
        }
        ast::StatementKind::Let(let_stmt) => {
            if let Some(init) = &let_stmt.initializer {
                results.extend(collect_expression_remaining_calls(init, generic_fns));
            }
        }
        ast::StatementKind::Expression(expr)
        | ast::StatementKind::Return(Some(expr))
        | ast::StatementKind::Break(Some(expr)) => {
            results.extend(collect_expression_remaining_calls(expr, generic_fns));
        }
        ast::StatementKind::Return(None) | ast::StatementKind::Break(None) => {}
        ast::StatementKind::Continue => {}
        ast::StatementKind::Defer(inner) => {
            results.extend(collect_statement_remaining_calls(inner, generic_fns));
        }
    }
    results
}

/// Check if a type is fully concrete (no type parameter references).
fn is_concrete(ty: &Type) -> bool {
    match ty {
        Type::Named { generics, .. } => {
            // In a monomorphized body, all type params are substituted.
            // A Named type is concrete if all its generic args are concrete.
            generics.iter().all(is_concrete)
        }
        Type::Primitive(_) | Type::Unit | Type::Never => true,
        Type::Pointer { inner, .. } | Type::Reference { inner, .. } => is_concrete(inner),
        Type::Slice { element } => is_concrete(element),
        Type::Task(inner) => is_concrete(inner),
        Type::Optional { inner } => is_concrete(inner),
        Type::Tuple(items) => items.iter().all(is_concrete),
        Type::Function {
            params,
            return_type,
        } => params.iter().all(is_concrete) && is_concrete(return_type),
        Type::Array { element, .. } => is_concrete(element),
        Type::Unknown => false,
    }
}

/// Collect remaining concrete generic function calls that need monomorphization.
/// Returns MonomorphRequest values to be processed in a subsequent round.
fn collect_remaining_function_requests(
    program: &ast::Program,
    imported_items: &[ast::Item],
    items: &[ast::Item],
    generic_fns: &HashSet<String>,
    generated: &HashSet<String>,
) -> Vec<MonomorphRequest> {
    let mut requests = Vec::new();
    // Collect calls first (to avoid borrow issues with program.items)
    let mut found_calls: Vec<(String, Vec<Type>, Span, usize)> = Vec::new();
    for item in items {
        match &item.kind {
            ast::ItemKind::Function(func) => {
                found_calls.extend(collect_block_remaining_calls(&func.body, generic_fns));
            }
            ast::ItemKind::Impl(impl_item) => {
                for member in &impl_item.items {
                    if let ast::ImplItemKind::Function(func) = member {
                        found_calls.extend(collect_block_remaining_calls(&func.body, generic_fns));
                    }
                }
            }
            _ => {}
        }
    }
    for (fn_name, concrete_args, call_span, param_count) in found_calls {
        if let Some(source) = find_generic_fn(program, imported_items, &fn_name, param_count) {
            let mapping = build_mapping_from_generics(source.generics.as_ref(), &concrete_args);
            let full_mangled = mangle_function_instance(source, &concrete_args, &mapping);
            let key = format!("fn::{full_mangled}");
            if generated.contains(&key) {
                continue;
            }
            // Use source's type param order for deterministic mangling
            let type_params: Vec<String> = source
                .generics
                .as_ref()
                .map(|g| {
                    g.params
                        .iter()
                        .filter_map(|p| {
                            if let ast::GenericParam::Type(tp) = p {
                                mapping
                                    .contains_key(&tp.name.name)
                                    .then(|| tp.name.name.clone())
                            } else {
                                None
                            }
                        })
                        .collect()
                })
                .unwrap_or_default();
            // Only add if all type params are covered and at least one exists
            if !type_params.is_empty()
                && type_params.len()
                    == source
                        .generics
                        .as_ref()
                        .map(|g| g.params.len())
                        .unwrap_or(0)
            {
                requests.push(MonomorphRequest::Function {
                    source: Box::new(source.clone()),
                    type_params,
                    mapping,
                    call_span,
                    is_imported: false,
                });
            }
        }
    }
    requests
}

/// Scope of known local bindings for bare-call inference. `None` means bound
// but type-unknown (e.g. destructured): calls using it are skipped, never
// guessed, so shadowing a generic function name can only suppress a request.
type BareCallScope = HashMap<String, Option<Type>>;

/// Best-effort type of an argument expression inside a substituted instance
// body. Only shapes with locally knowable types are covered; anything else
// yields None and the enclosing call is left for existing handling.
fn infer_bare_arg_type(expr: &ast::Expression, scope: &BareCallScope) -> Option<Type> {
    match expr.kind.as_ref() {
        ast::ExpressionKind::Identifier(ident) => scope.get(&ident.name).cloned().flatten(),
        ast::ExpressionKind::Reference {
            is_mutable,
            expression,
        } => {
            // Address-of always yields a Pointer (mirroring typeck); any
            // dereference-through happens in unify_call_arg, never here.
            infer_bare_arg_type(expression, scope).map(|inner| Type::Pointer {
                is_mutable: *is_mutable,
                is_volatile: false,
                inner: Box::new(inner),
            })
        }
        ast::ExpressionKind::Move(inner) | ast::ExpressionKind::Comptime(inner) => {
            infer_bare_arg_type(inner, scope)
        }
        _ => None,
    }
}

/// Whether a callee parameter type mentions any of the callee's own type
// parameters. Pairs without variables need no inference: typeck already
// proved them in the generic body and substitution preserves typing.
fn type_mentions_vars(ty: &Type, vars: &HashSet<String>) -> bool {
    match ty {
        Type::Named { path, generics } => {
            (path.len() == 1 && vars.contains(&path[0]))
                || generics.iter().any(|g| type_mentions_vars(g, vars))
        }
        Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
            type_mentions_vars(inner, vars)
        }
        Type::Slice { element } | Type::Optional { inner: element } | Type::Task(element) => {
            type_mentions_vars(element, vars)
        }
        Type::Array { element, .. } => type_mentions_vars(element, vars),
        Type::Tuple(items) => items.iter().any(|t| type_mentions_vars(t, vars)),
        Type::Function {
            params,
            return_type,
        } => {
            params.iter().any(|t| type_mentions_vars(t, vars))
                || type_mentions_vars(return_type, vars)
        }
        Type::Primitive(_) | Type::Unit | Type::Never | Type::Unknown => false,
    }
}

/// First-order unification of one callee parameter type against an argument
// type. Names in `vars` (the callee's own type params) bind; everything else
// must match structurally. Returns false on any doubt.
fn unify_call_arg(
    pattern: &Type,
    concrete: &Type,
    vars: &HashSet<String>,
    mapping: &mut HashMap<String, Type>,
) -> bool {
    if let Type::Named { path, generics } = pattern
        && path.len() == 1
        && generics.is_empty()
        && vars.contains(&path[0])
    {
        if let Some(bound) = mapping.get(&path[0]) {
            return bound == concrete;
        }
        mapping.insert(path[0].clone(), concrete.clone());
        return true;
    }
    match (pattern, concrete) {
        (
            Type::Reference {
                is_mutable: pm,
                inner: pi,
            },
            Type::Reference {
                is_mutable: cm,
                inner: ci,
            },
        ) => (!*pm || *cm) && unify_call_arg(pi, ci, vars, mapping),
        (
            Type::Reference { inner, .. },
            Type::Pointer {
                inner: found_inner, ..
            },
        ) => {
            // Mirrors typeck inference: a pointer to a reference
            // dereferences through before binding.
            match found_inner.as_ref() {
                Type::Reference { inner: deref, .. } => unify_call_arg(inner, deref, vars, mapping),
                _ => unify_call_arg(inner, found_inner, vars, mapping),
            }
        }
        (
            Type::Pointer { inner, .. },
            Type::Reference {
                inner: found_inner, ..
            },
        ) => unify_call_arg(inner, found_inner, vars, mapping),
        (
            Type::Pointer {
                is_mutable: pm,
                is_volatile: pv,
                inner: pi,
            },
            Type::Pointer {
                is_mutable: cm,
                is_volatile: cv,
                inner: ci,
            },
        ) => pm == cm && pv == cv && unify_call_arg(pi, ci, vars, mapping),
        (
            Type::Named {
                path: pp,
                generics: pg,
            },
            Type::Named {
                path: cp,
                generics: cg,
            },
        ) => {
            pp == cp
                && pg.len() == cg.len()
                && pg
                    .iter()
                    .zip(cg.iter())
                    .all(|(a, b)| unify_call_arg(a, b, vars, mapping))
        }
        (Type::Primitive(a), Type::Primitive(b)) => a == b,
        (Type::Unit, Type::Unit) => true,
        (Type::Never, _) | (_, Type::Never) => true,
        (Type::Slice { element: a }, Type::Slice { element: b })
        | (Type::Optional { inner: a }, Type::Optional { inner: b })
        | (Type::Task(a), Type::Task(b)) => unify_call_arg(a, b, vars, mapping),
        (
            Type::Array {
                element: a,
                size: sa,
            },
            Type::Array {
                element: b,
                size: sb,
            },
        ) => sa == sb && unify_call_arg(a, b, vars, mapping),
        (Type::Tuple(a), Type::Tuple(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(x, y)| unify_call_arg(x, y, vars, mapping))
        }
        (
            Type::Function {
                params: ap,
                return_type: ar,
            },
            Type::Function {
                params: bp,
                return_type: br,
            },
        ) => {
            ap.len() == bp.len()
                && ap
                    .iter()
                    .zip(bp.iter())
                    .all(|(x, y)| unify_call_arg(x, y, vars, mapping))
                && unify_call_arg(ar, br, vars, mapping)
        }
        _ => false,
    }
}

/// Source-ordered type parameter names of a generic function template.
fn template_type_params(source: &ast::FunctionItem) -> Vec<String> {
    source
        .generics
        .as_ref()
        .map(|g| {
            g.params
                .iter()
                .filter_map(|p| {
                    if let ast::GenericParam::Type(tp) = p {
                        Some(tp.name.name.clone())
                    } else {
                        None
                    }
                })
                .collect()
        })
        .unwrap_or_default()
}

/// Walker discovering bare-Identifier calls to generic functions inside
// freshly substituted instance bodies, where no explicit type arguments
// exist to collect. Inferred mappings unify the callee template's parameters
// against locally knowable argument types; anything ambiguous is skipped.
struct BareCallDiscoverer<'a> {
    program: &'a ast::Program,
    imported: &'a HashMap<String, ast::FunctionItem>,
    generated: &'a HashSet<String>,
    seen: HashSet<String>,
    requests: Vec<MonomorphRequest>,
}

impl<'a> BareCallDiscoverer<'a> {
    fn template(&self, name: &str, arity: usize) -> Option<(&ast::FunctionItem, bool)> {
        for item in &self.program.items {
            if let ast::ItemKind::Function(func) = &item.kind
                && func.name.name == name
                && func.generics.is_some()
                && func.parameters.len() == arity
            {
                return Some((func, false));
            }
        }
        if let Some(func) = self.imported.get(name)
            && func.generics.is_some()
            && func.parameters.len() == arity
        {
            return Some((func, true));
        }
        None
    }

    fn bind_pattern(pattern: &ast::Pattern, ty: Option<Type>, scope: &mut BareCallScope) {
        match &pattern.kind {
            ast::PatternKind::Identifier(id) | ast::PatternKind::Move(id) => {
                scope.insert(id.name.clone(), ty);
            }
            ast::PatternKind::Tuple(items) => {
                for item in items {
                    Self::bind_pattern(item, None, scope);
                }
            }
            ast::PatternKind::Struct { fields, .. } => {
                for field in fields {
                    if let Some(pattern) = &field.pattern {
                        Self::bind_pattern(pattern, None, scope);
                    } else {
                        scope.insert(field.name.name.clone(), None);
                    }
                }
            }
            ast::PatternKind::Enum { data, .. } => {
                if let Some(pattern) = data {
                    Self::bind_pattern(pattern, None, scope);
                }
            }
            _ => {}
        }
    }

    fn bind_let(&self, stmt: &ast::LetStatement, scope: &mut BareCallScope) {
        let mut ty = stmt.type_annotation.as_ref().map(Type::from_ast);
        if ty.is_none()
            && let Some(init) = stmt.initializer.as_ref()
        {
            ty = infer_bare_arg_type(init, scope);
        }
        Self::bind_pattern(&stmt.pattern, ty, scope);
    }

    fn walk_block(&mut self, block: &mut ast::Block, scope: &mut BareCallScope) {
        let saved = scope.clone();
        for i in 0..block.statements.len() {
            self.walk_stmt(&mut block.statements[i], scope);
        }
        *scope = saved;
    }

    fn walk_stmt(&mut self, stmt: &mut ast::Statement, scope: &mut BareCallScope) {
        match &mut stmt.kind {
            ast::StatementKind::Block(block) => self.walk_block(block, scope),
            ast::StatementKind::Let(let_stmt) => self.bind_let(&*let_stmt, scope),
            ast::StatementKind::Expression(expr)
            | ast::StatementKind::Return(Some(expr))
            | ast::StatementKind::Break(Some(expr)) => self.walk_expr(expr, scope),
            ast::StatementKind::Return(None) | ast::StatementKind::Break(None) => {}
            ast::StatementKind::Continue => {}
            ast::StatementKind::Defer(inner) => self.walk_stmt(inner, scope),
        }
    }

    fn walk_expr(&mut self, expr: &mut ast::Expression, scope: &mut BareCallScope) {
        match expr.kind.as_mut() {
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                let span = expr.span;
                if let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref() {
                    let name = ident.name.clone();
                    self.visit_bare_call(function, &name, arguments, span, scope);
                } else {
                    self.walk_expr(function, scope);
                }
                for arg in arguments.iter_mut() {
                    self.walk_expr(arg, scope);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                self.walk_expr(receiver, scope);
                for arg in arguments.iter_mut() {
                    self.walk_expr(arg, scope);
                }
            }
            ast::ExpressionKind::FieldAccess { object, .. }
            | ast::ExpressionKind::Index { object, .. }
            | ast::ExpressionKind::Slice { object, .. } => self.walk_expr(object, scope),
            ast::ExpressionKind::Cast { expression, .. } => self.walk_expr(expression, scope),
            ast::ExpressionKind::Binary { left, right, .. } => {
                self.walk_expr(left, scope);
                self.walk_expr(right, scope);
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. } => self.walk_expr(operand, scope),
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_expr(condition, scope);
                let mut inner = scope.clone();
                self.walk_block(then_branch, &mut inner);
                if let Some(branch) = else_branch {
                    let mut inner = scope.clone();
                    self.walk_block(branch, &mut inner);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                self.walk_expr(condition, scope);
                let mut inner = scope.clone();
                self.walk_block(body, &mut inner);
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                let mut inner = scope.clone();
                self.bind_let(init, &mut inner);
                if let Some(init_expr) = &mut init.initializer {
                    self.walk_expr(init_expr, &mut inner);
                }
                self.walk_expr(condition, &mut inner);
                self.walk_expr(increment, &mut inner);
                self.walk_block(body, &mut inner);
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.walk_expr(expression, scope);
                for arm in arms.iter_mut() {
                    if let Some(guard) = &mut arm.guard {
                        self.walk_expr(guard, scope);
                    }
                    self.walk_expr(&mut arm.body, scope);
                }
            }
            ast::ExpressionKind::Block(block) => {
                let mut inner = scope.clone();
                self.walk_block(block, &mut inner);
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items.iter_mut() {
                    match item {
                        ast::InitializerItem::Positional(expr)
                        | ast::InitializerItem::Field { value: expr, .. } => {
                            self.walk_expr(expr, scope)
                        }
                        ast::InitializerItem::Index { index, value } => {
                            self.walk_expr(index, scope);
                            self.walk_expr(value, scope);
                        }
                    }
                }
            }
            ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
                for item in items.iter_mut() {
                    self.walk_expr(item, scope);
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields.iter_mut() {
                    self.walk_expr(&mut field.value, scope);
                }
            }
            ast::ExpressionKind::Move(inner)
            | ast::ExpressionKind::Comptime(inner)
            | ast::ExpressionKind::Launch(inner)
            | ast::ExpressionKind::Wait(inner)
            | ast::ExpressionKind::Reference {
                expression: inner, ..
            } => self.walk_expr(inner, scope),
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args.iter_mut() {
                    if let ast::MacroArg::Expression(expr) = arg {
                        self.walk_expr(expr, scope);
                    }
                }
            }
            ast::ExpressionKind::ForIn { iterable, body, .. } => {
                self.walk_expr(iterable, scope);
                let mut inner = scope.clone();
                self.walk_block(body, &mut inner);
            }
            ast::ExpressionKind::Asm { inputs, .. } => {
                for input in inputs.iter_mut() {
                    self.walk_expr(input, scope);
                }
            }
            _ => {}
        }
    }

    fn visit_bare_call(
        &mut self,
        function_expr: &mut ast::Expression,
        name: &str,
        arguments: &mut [ast::Expression],
        span: Span,
        scope: &mut BareCallScope,
    ) {
        // A shadowing local binding means typeck did not resolve this to the
        // global function; infer nothing.
        if scope.contains_key(name) {
            return;
        }
        // Copy template data out first: the rewrite below needs &mut self.
        let resolved: Option<(
            ast::FunctionItem,
            bool,
            Vec<Type>,
            HashSet<String>,
            Vec<String>,
        )> = (|| {
            let (template, is_imported) = self.template(name, arguments.len())?;
            if template.is_variadic {
                return None;
            }
            let params = template
                .parameters
                .iter()
                .map(|p| Type::from_ast(&p.param_type))
                .collect();
            let type_params = template_type_params(template);
            let vars: HashSet<String> = type_params.iter().cloned().collect();
            Some((template.clone(), is_imported, params, vars, type_params))
        })();
        let Some((source, is_imported, param_types, vars, type_params)) = resolved else {
            return;
        };
        let mut mapping = HashMap::default();
        for (param, arg_expr) in param_types.iter().zip(arguments.iter()) {
            // Concrete parameters were already proved by typeck; only
            // variable-carrying ones need inference.
            if !type_mentions_vars(param, &vars) {
                continue;
            }
            let Some(arg) = infer_bare_arg_type(arg_expr, scope) else {
                return;
            };
            if !unify_call_arg(param, &arg, &vars, &mut mapping) {
                return;
            }
        }
        if type_params.is_empty()
            || type_params.iter().any(|p| !mapping.contains_key(p))
            || mapping.values().any(|t| !is_concrete(t))
        {
            return;
        }
        let args = ordered_args(&type_params, &mapping);
        let mangled = mangle_function_instance(&source, &args, &mapping);
        // Rewrite this call site now: the instance body is already
        // substituted, so no later span-based pass can match it.
        *function_expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                name: mangled.clone(),
                span: function_expr.span,
            })),
            span: function_expr.span,
        };
        let key = format!("fn::{mangled}");
        if self.generated.contains(&key) || !self.seen.insert(key) {
            return;
        }
        self.requests.push(MonomorphRequest::Function {
            source: Box::new(source),
            type_params,
            mapping,
            call_span: span,
            is_imported,
        });
    }
}

/// Discover bare-Identifier calls to generic functions inside freshly
// substituted instance bodies and rewrite them to their mangled instances,
// returning requests for instances not yet generated. Only unambiguous
// local inference is attempted; everything else keeps existing behavior.
fn discover_bare_generic_calls(
    new_items: &mut [ast::Item],
    program: &ast::Program,
    imported_fn_templates: &HashMap<String, ast::FunctionItem>,
    generated: &HashSet<String>,
) -> Vec<MonomorphRequest> {
    let mut discoverer = BareCallDiscoverer {
        program,
        imported: imported_fn_templates,
        generated,
        seen: HashSet::default(),
        requests: Vec::new(),
    };
    for item in new_items.iter_mut() {
        match &mut item.kind {
            ast::ItemKind::Function(func) => {
                let mut scope = BareCallScope::default();
                for param in &func.parameters {
                    scope.insert(
                        param.name.name.clone(),
                        Some(Type::from_ast(&param.param_type)),
                    );
                }
                discoverer.walk_block(&mut func.body, &mut scope);
            }
            ast::ItemKind::Impl(impl_item) => {
                for member in impl_item.items.iter_mut() {
                    if let ast::ImplItemKind::Function(func) = member {
                        let mut scope = BareCallScope::default();
                        for param in &func.parameters {
                            scope.insert(
                                param.name.name.clone(),
                                Some(Type::from_ast(&param.param_type)),
                            );
                        }
                        discoverer.walk_block(&mut func.body, &mut scope);
                    }
                }
            }
            _ => {}
        }
    }
    discoverer.requests
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Span, lex};
    use crate::parser::Parser;

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    fn find_function(program: &ast::Program, name: &str) -> ast::FunctionItem {
        program
            .items
            .iter()
            .find_map(|item| {
                if let ast::ItemKind::Function(f) = &item.kind
                    && f.name.name == name
                {
                    return Some(f.clone());
                }
                None
            })
            .expect("function not found")
    }

    #[test]
    fn normalizes_local_generic_defaults_before_monomorphization() {
        let mut program = parse(
            "struct Box<T = i32> { T value; } \
             i64 main() { Box b; b.value = 7; return b.value; }",
        );
        let items = append_monomorphs(&mut program, &[], &[]);
        let main = find_function(&program, "main");
        let ast::StatementKind::Let(statement) = &main.body.statements[0].kind else {
            panic!("expected local declaration");
        };
        let Some(annotation) = &statement.type_annotation else {
            panic!("expected local type annotation");
        };
        let ast::TypeKind::Named(named) = annotation.kind.as_ref() else {
            panic!("expected named local type");
        };
        assert_eq!(named.generics.as_ref().map(Vec::len), Some(1));
        assert!(items.iter().any(|item| matches!(
            &item.kind,
            ast::ItemKind::Struct(item) if item.name.name == "Box__i32"
        )));
    }

    #[test]
    fn monomorphizes_generic_structs() {
        let mut program = parse("struct Box<T> { T value; } i32 main() { Box<i32> b; return 0; }");
        let items = append_monomorphs(&mut program, &[], &[]);
        let has_struct = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Struct(struct_item) => struct_item.name.name.starts_with("Box__"),
            _ => false,
        });
        assert!(has_struct, "expected monomorphized struct");
    }

    #[test]
    fn monomorphizes_generic_function() {
        let mut program = parse("T foo<T>(T x) { return x; } i32 main() { return 0; }");
        let source = find_function(&program, "foo");
        let type_params = vec!["T".to_string()];
        let mapping =
            HashMap::from_iter([("T".to_string(), Type::Primitive(ast::PrimitiveType::I32))]);

        let requests = vec![MonomorphRequest::Function {
            source: Box::new(source),
            type_params,
            mapping,
            call_span: Span::default(),
            is_imported: false,
        }];

        let items = append_monomorphs(&mut program, &requests, &[]);
        let has_fn = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Function(f) => {
                f.name.name.starts_with("foo__1_i32__1_i32__") && f.name.name.len() == 19 + 16
            }
            _ => false,
        });
        assert!(has_fn, "expected monomorphized function foo__i32");
    }

    #[test]
    fn monomorphizes_generic_function_with_multiple_type_params() {
        let mut program = parse("T bar<T, U>(T x, U y) { return x; } i32 main() { return 0; }");
        let source = find_function(&program, "bar");
        let type_params = vec!["T".to_string(), "U".to_string()];
        let mapping = HashMap::from_iter([
            ("T".to_string(), Type::Primitive(ast::PrimitiveType::I32)),
            ("U".to_string(), Type::Primitive(ast::PrimitiveType::F64)),
        ]);

        let requests = vec![MonomorphRequest::Function {
            source: Box::new(source),
            type_params,
            mapping,
            call_span: Span::default(),
            is_imported: false,
        }];

        let items = append_monomorphs(&mut program, &requests, &[]);
        let has_fn = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Function(f) => {
                f.name.name.starts_with("bar__2_i32_f64__2_i32_f64__")
                    && f.name.name.len() == 27 + 16
            }
            _ => false,
        });
        assert!(has_fn, "expected monomorphized function bar__i32_f64");
    }

    #[test]
    fn monomorphizes_duplicate_request_only_once() {
        let mut program = parse("T foo<T>(T x) { return x; } i32 main() { return 0; }");
        let source = find_function(&program, "foo");
        let type_params = vec!["T".to_string()];
        let mapping =
            HashMap::from_iter([("T".to_string(), Type::Primitive(ast::PrimitiveType::I32))]);

        let request = MonomorphRequest::Function {
            source: Box::new(source),
            type_params,
            mapping,
            call_span: Span::default(),
            is_imported: false,
        };

        let items = append_monomorphs(&mut program, &[request.clone(), request], &[]);
        let count = items
            .iter()
            .filter(|item| {
                matches!(&item.kind, ast::ItemKind::Function(f)
                if f.name.name.starts_with("foo__1_i32__1_i32__"))
            })
            .count();
        assert_eq!(
            count, 1,
            "duplicate request should only produce one monomorph"
        );
    }

    #[test]
    fn monomorph_instance_order_is_independent_of_request_discovery() {
        let source = parse(
            "T zed<T>(T x) { return x; } \
             T alpha<T>(T x) { return x; } \
             i32 main() { return 0; }",
        );
        let request_for = |name: &str, primitive, start| {
            let mut call_span = Span::default();
            call_span.file = 1;
            call_span.start = start;
            call_span.end = start + 1;
            MonomorphRequest::Function {
                source: Box::new(find_function(&source, name)),
                type_params: vec!["T".to_string()],
                mapping: HashMap::from_iter([("T".to_string(), Type::Primitive(primitive))]),
                call_span,
                is_imported: false,
            }
        };
        let zed = request_for("zed", ast::PrimitiveType::I32, 10);
        let alpha = request_for("alpha", ast::PrimitiveType::F64, 10);

        let instance_names = |requests: &[MonomorphRequest]| {
            let mut program = source.clone();
            append_monomorphs(&mut program, requests, &[])
                .iter()
                .filter_map(|item| match &item.kind {
                    ast::ItemKind::Function(function) if function.name.name.contains("__") => {
                        Some(function.name.name.clone())
                    }
                    _ => None,
                })
                .collect::<Vec<_>>()
        };

        let ordered = instance_names(&[zed.clone(), alpha.clone()]);
        assert_eq!(
            ordered,
            instance_names(&[alpha, zed]),
            "emitted instance order must not depend on request discovery"
        );
        assert_eq!(ordered.len(), 2, "both unique requests must be emitted");
        assert!(ordered[0].starts_with("alpha__"));
        assert!(ordered[1].starts_with("zed__"));
    }

    #[test]
    fn monomorphizes_struct_and_function_together() {
        let mut program = parse(
            "struct Pair<T, U> { T first; U second; } T id<T>(T x) { return x; } i32 main() { return 0; }",
        );
        let source = find_function(&program, "id");
        let type_params = vec!["T".to_string()];
        let mapping =
            HashMap::from_iter([("T".to_string(), Type::Primitive(ast::PrimitiveType::I32))]);

        let requests = vec![MonomorphRequest::Function {
            source: Box::new(source),
            type_params,
            mapping,
            call_span: Span::default(),
            is_imported: false,
        }];

        let items = append_monomorphs(&mut program, &requests, &[]);
        let has_fn = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Function(f) => {
                f.name.name.starts_with("id__1_i32__1_i32__") && f.name.name.len() == 18 + 16
            }
            _ => false,
        });
        assert!(has_fn, "expected monomorphized function id__i32");
    }

    #[test]
    fn mangle_name_empty_args() {
        assert_eq!(mangle_name("foo", &[]), "foo");
    }

    #[test]
    fn mangle_name_single_arg() {
        assert_eq!(
            mangle_name("foo", &[Type::Primitive(ast::PrimitiveType::I32)]),
            "foo__i32"
        );
    }

    #[test]
    fn mangle_name_multiple_args() {
        assert_eq!(
            mangle_name(
                "convert",
                &[
                    Type::Primitive(ast::PrimitiveType::I32),
                    Type::Primitive(ast::PrimitiveType::F64)
                ]
            ),
            "convert__i32_f64"
        );
    }

    #[test]
    fn mangle_name_pointer_type() {
        let mangled = mangle_name(
            "deref",
            &[Type::Pointer {
                is_mutable: false,
                is_volatile: false,
                inner: Box::new(Type::Primitive(ast::PrimitiveType::I32)),
            }],
        );
        assert_eq!(mangled, "deref__ptr_i32");
    }

    #[test]
    fn mangle_name_named_type() {
        assert_eq!(
            mangle_name(
                "alloc",
                &[Type::Named {
                    path: vec!["Point".to_string()],
                    generics: vec![]
                }]
            ),
            "alloc__Point"
        );
    }

    #[test]
    fn monomorphizes_nested_generic_struct() {
        let mut program = parse(
            "struct Wrapper<T> { T inner; } struct Pair<T, U> { T first; U second; } i32 main() { Pair<Wrapper<i32>, i32> p; return 0; }",
        );
        let items = append_monomorphs(&mut program, &[], &[]);
        let has_wrapper = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Struct(s) => s.name.name.starts_with("Wrapper__"),
            _ => false,
        });
        let pair_names: Vec<&str> = items
            .iter()
            .filter_map(|item| match &item.kind {
                ast::ItemKind::Struct(s) => Some(s.name.name.as_str()),
                _ => None,
            })
            .collect();
        let has_pair = pair_names.iter().any(|n| n.starts_with("Pair__"));
        assert!(has_wrapper, "expected Wrapper<i32> monomorph");
        assert!(
            has_pair,
            "expected Pair<Wrapper<i32>, i32> monomorph, got: {:?}",
            pair_names
        );
    }

    #[test]
    fn monomorphizes_generic_enum() {
        let mut program =
            parse("enum Option<T> { Some(T); None; } i32 main() { Option<i32> x; return 0; }");
        let items = append_monomorphs(&mut program, &[], &[]);
        let has_enum = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Enum(e) => e.name.name.starts_with("Option__"),
            _ => false,
        });
        assert!(has_enum, "expected Option<i32> monomorph");
    }

    #[test]
    fn specializes_generic_methods_independently_for_each_method_type() {
        let mut program = parse(
            "struct MethodProbe {} \
             impl MethodProbe { void accept<T>(MethodProbe* self, T value) {} } \
             i32 main() { MethodProbe probe; probe.accept((i64)1); \
                 probe.accept((u64)2); return 0; }",
        );
        let impl_item = program
            .items
            .iter()
            .find_map(|item| match &item.kind {
                ast::ItemKind::Impl(impl_item) => Some(impl_item.clone()),
                _ => None,
            })
            .expect("expected MethodProbe impl");
        let method = impl_item
            .items
            .iter()
            .find_map(|item| match item {
                ast::ImplItemKind::Function(method) => Some((**method).clone()),
                _ => None,
            })
            .expect("expected generic accept method");
        let call_spans = program
            .items
            .iter()
            .find_map(|item| match &item.kind {
                ast::ItemKind::Function(function) if function.name.name == "main" => Some(
                    function
                        .body
                        .statements
                        .iter()
                        .filter_map(|statement| match &statement.kind {
                            ast::StatementKind::Expression(expression)
                                if matches!(
                                    expression.kind.as_ref(),
                                    ast::ExpressionKind::MethodCall { method, .. }
                                        if method.name == "accept"
                                ) =>
                            {
                                Some(expression.span)
                            }
                            _ => None,
                        })
                        .collect::<Vec<_>>(),
                ),
                _ => None,
            })
            .expect("expected main function");
        assert_eq!(call_spans.len(), 2, "expected two generic method calls");
        let request = |ty, call_span| MonomorphRequest::ImplMethod {
            impl_item: Box::new(impl_item.clone()),
            method: Box::new(method.clone()),
            type_params: vec!["T".to_string()],
            mapping: HashMap::from_iter([("T".to_string(), ty)]),
            call_span,
        };
        let items = append_monomorphs(
            &mut program,
            &[
                request(Type::Primitive(ast::PrimitiveType::I64), call_spans[0]),
                request(Type::Primitive(ast::PrimitiveType::U64), call_spans[1]),
            ],
            &[],
        );
        let parameter_types = items
            .iter()
            .filter_map(|item| match &item.kind {
                ast::ItemKind::Impl(impl_item) => impl_item.items.iter().find_map(|item| {
                    let ast::ImplItemKind::Function(method) = item else {
                        return None;
                    };
                    method
                        .name
                        .name
                        .starts_with("accept__")
                        .then(|| Type::from_ast(&method.parameters[1].param_type))
                }),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            parameter_types,
            vec![
                Type::Primitive(ast::PrimitiveType::I64),
                Type::Primitive(ast::PrimitiveType::U64),
            ],
            "each concrete method type needs its own specialized method"
        );
        let call_names = program
            .items
            .iter()
            .find_map(|item| match &item.kind {
                ast::ItemKind::Function(function) if function.name.name == "main" => Some(
                    function
                        .body
                        .statements
                        .iter()
                        .filter_map(|statement| match &statement.kind {
                            ast::StatementKind::Expression(expression) => {
                                let ast::ExpressionKind::MethodCall { method, .. } =
                                    expression.kind.as_ref()
                                else {
                                    return None;
                                };
                                Some(method.name.clone())
                            }
                            _ => None,
                        })
                        .collect::<Vec<_>>(),
                ),
                _ => None,
            })
            .expect("expected main function");
        assert_eq!(call_names.len(), 2);
        assert_ne!(call_names[0], call_names[1]);
        assert!(call_names.iter().all(|name| name.starts_with("accept__")));
    }

    #[test]
    fn distinguishes_method_arguments_absent_from_the_signature() {
        let mut program =
            parse("struct Probe {} impl Probe { i64 width<T>(Probe* self) { return @size(T); } }");
        let ast::ItemKind::Impl(template) = &program.items[1].kind else {
            panic!("expected impl")
        };
        let ast::ImplItemKind::Function(method) = &template.items[0] else {
            panic!("expected method")
        };
        let requests = [
            ast::PrimitiveType::I32,
            ast::PrimitiveType::I64,
            ast::PrimitiveType::I32,
        ]
        .into_iter()
        .map(|ty| MonomorphRequest::ImplMethod {
            impl_item: Box::new(template.clone()),
            method: method.clone(),
            type_params: vec!["T".into()],
            mapping: HashMap::from_iter([("T".into(), Type::Primitive(ty))]),
            call_span: Span::default(),
        })
        .collect::<Vec<_>>();
        let mut generated = HashSet::default();
        let items =
            instantiate_requests(&mut program, &requests, &mut generated, &HashMap::default());
        let names = items
            .iter()
            .map(|item| {
                let ast::ItemKind::Impl(item) = &item.kind else {
                    panic!("expected impl")
                };
                let ast::ImplItemKind::Function(method) = &item.items[0] else {
                    panic!("expected method")
                };
                method.name.name.clone()
            })
            .collect::<Vec<_>>();
        assert_eq!(names.len(), 2, "identical requests should deduplicate");
        assert_ne!(
            names[0], names[1],
            "phantom method arguments affect the body and symbol"
        );
    }

    #[test]
    fn rewrites_only_the_requested_owner_in_cloned_bodies() {
        let source = parse(
            "struct Wrapper<T> { Wrapper<T>* child; } impl<T> Wrapper<T> { \
            void run(Wrapper<T>* self) { self.child.accept(); } }",
        );
        let ast::ItemKind::Struct(definition) = &source.items[0].kind else {
            panic!("expected struct")
        };
        let structs = HashMap::from_iter([("Wrapper".into(), definition.clone())]);
        let mut program = source.clone();
        program.items.clear();
        for ty in [ast::PrimitiveType::I32, ast::PrimitiveType::I64] {
            let mut item = source.items[1].clone();
            let ast::ItemKind::Impl(implementation) = &mut item.kind else {
                panic!("expected impl")
            };
            let mapping = HashMap::from_iter([("T".into(), Type::Primitive(ty))]);
            implementation.self_type = substitute_ast_type(&implementation.self_type, &mapping);
            let ast::ImplItemKind::Function(method) = &mut implementation.items[0] else {
                panic!("expected method")
            };
            method.parameters[0].param_type =
                substitute_ast_type(&method.parameters[0].param_type, &mapping);
            substitute_block_types(&mut method.body, &mapping);
            program.items.push(item);
        }
        let call = |item: &ast::Item| {
            let ast::ItemKind::Impl(item) = &item.kind else {
                panic!("expected impl")
            };
            let ast::ImplItemKind::Function(method) = &item.items[0] else {
                panic!("expected method")
            };
            let ast::StatementKind::Expression(expr) = &method.body.statements[0].kind else {
                panic!("expected call")
            };
            let ast::ExpressionKind::MethodCall { method, .. } = expr.kind.as_ref() else {
                panic!("expected method call")
            };
            (expr.span, method.name.clone())
        };
        let span = call(&program.items[0]).0;
        assert_eq!(span, call(&program.items[1]).0);
        rewrite_method_calls(
            &mut program,
            "Wrapper",
            "accept",
            "accept_i32",
            &[Type::Primitive(ast::PrimitiveType::I32)],
            &span,
            &structs,
        );
        assert_eq!(call(&program.items[0]).1, "accept_i32");
        assert_eq!(call(&program.items[1]).1, "accept");
        rewrite_method_calls(
            &mut program,
            "Wrapper",
            "accept",
            "accept_i64",
            &[Type::Primitive(ast::PrimitiveType::I64)],
            &span,
            &structs,
        );
        assert_eq!(call(&program.items[0]).1, "accept_i32");
        assert_eq!(call(&program.items[1]).1, "accept_i64");
    }

    #[test]
    fn preserves_primitive_static_method_receivers() {
        let mut program = parse(
            "impl i64 { i64 width<T>(T value) { return @size(T); } } \
             i64 measure() { return i64.width((i32)3); }",
        );
        let ast::ItemKind::Function(function) = &mut program.items[1].kind else {
            panic!("expected measure function")
        };
        let ast::StatementKind::Return(Some(expression)) = &mut function.body.statements[0].kind
        else {
            panic!("expected return expression")
        };
        let ast::ExpressionKind::MethodCall { receiver, .. } = expression.kind.as_mut() else {
            panic!("expected static method call")
        };
        *receiver.kind = ast::ExpressionKind::TypeName(type_to_ast(
            &Type::Primitive(ast::PrimitiveType::I64),
            receiver.span,
        ));
        let call_span = expression.span;
        rewrite_method_calls(
            &mut program,
            "i64",
            "width",
            "width_i32",
            &[],
            &call_span,
            &HashMap::default(),
        );
        let ast::ItemKind::Function(function) = &program.items[1].kind else {
            panic!("expected measure function")
        };
        let ast::StatementKind::Return(Some(expression)) = &function.body.statements[0].kind else {
            panic!("expected return expression")
        };
        let ast::ExpressionKind::MethodCall {
            receiver, method, ..
        } = expression.kind.as_ref()
        else {
            panic!("expected static method call")
        };
        assert_eq!(method.name, "width_i32");
        assert!(
            matches!(receiver.kind.as_ref(), ast::ExpressionKind::TypeName(ty)
            if matches!(ty.kind.as_ref(), ast::TypeKind::Primitive(ast::PrimitiveType::I64)))
        );
    }

    #[test]
    fn rewrites_generic_method_calls_in_enum_payloads() {
        let mut program = parse(
            "struct Probe {} \
             enum Width { Some(i64); } \
             impl Probe { i64 width<T>(&self, T value) { return @size(T); } } \
             Width measure(Probe probe) { return Width.Some(probe.width((i32)3)); }",
        );
        let call_span = {
            let ast::ItemKind::Function(function) = &mut program.items[3].kind else {
                panic!("expected measure function")
            };
            let ast::StatementKind::Return(Some(expression)) =
                &mut function.body.statements[0].kind
            else {
                panic!("expected return expression")
            };
            let ast::ExpressionKind::MethodCall { arguments, .. } = expression.kind.as_ref() else {
                panic!("expected enum constructor")
            };
            let call_span = arguments[0].span;
            *expression.kind = ast::ExpressionKind::EnumVariant {
                path: vec![ast::Identifier {
                    name: "Width".into(),
                    span: expression.span,
                }],
                variant: ast::Identifier {
                    name: "Some".into(),
                    span: expression.span,
                },
                fields: arguments.clone(),
            };
            call_span
        };
        rewrite_method_calls(
            &mut program,
            "Probe",
            "width",
            "width_i32",
            &[],
            &call_span,
            &HashMap::default(),
        );
        let ast::ItemKind::Function(function) = &program.items[3].kind else {
            panic!("expected measure function")
        };
        let ast::StatementKind::Return(Some(expression)) = &function.body.statements[0].kind else {
            panic!("expected return expression")
        };
        let ast::ExpressionKind::EnumVariant { fields, .. } = expression.kind.as_ref() else {
            panic!("expected enum variant")
        };
        let ast::ExpressionKind::MethodCall { method, .. } = fields[0].kind.as_ref() else {
            panic!("expected payload method call")
        };
        assert_eq!(method.name, "width_i32");
    }

    #[test]
    fn monomorphizes_nested_generic_function_call_in_impl_body() {
        let mut program = parse(
            "T* alloc<T>(i32 size) { return 0; } \
             struct Holder<T> { T* ptr; i64 size; } \
             impl<T> Holder<T> { \
                 void grow(Holder<T>* self, i64 extra) { \
                     T* next = alloc<T>(4); \
                     self.ptr = next; \
                     self.size = self.size + extra; \
                 } \
             } \
             i32 main() { Holder<i32> h; return 0; }",
        );

        // Find the generic impl and method to create an initial ImplMethod request
        let impl_item = program
            .items
            .iter()
            .find_map(|item| {
                if let ast::ItemKind::Impl(impl_item) = &item.kind
                    && impl_item.generics.is_some()
                {
                    let has_grow = impl_item.items.iter().any(
                        |m| matches!(m, ast::ImplItemKind::Function(f) if f.name.name == "grow"),
                    );
                    if has_grow {
                        return Some(impl_item.clone());
                    }
                }
                None
            })
            .expect("expected generic impl Holder<T> with grow");

        let grow_method = impl_item
            .items
            .iter()
            .find_map(|m| {
                if let ast::ImplItemKind::Function(f) = m
                    && f.name.name == "grow"
                {
                    return Some(Box::new((**f).clone()));
                }
                None
            })
            .expect("expected grow method");

        let type_params = vec!["T".to_string()];
        let mapping =
            HashMap::from_iter([("T".to_string(), Type::Primitive(ast::PrimitiveType::I32))]);

        let request = MonomorphRequest::ImplMethod {
            impl_item: Box::new(impl_item),
            method: grow_method,
            type_params,
            mapping,
            call_span: Span::default(),
        };

        let items = append_monomorphs(&mut program, &[request], &[]);

        // Verify Holder<i32> impl was created
        let has_holder = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Impl(impl_item) => {
                let is_holder =
                    if let ast::TypeKind::Named(named) = &impl_item.self_type.kind.as_ref() {
                        named.path.last().is_some_and(|id| id.name == "Holder__i32")
                    } else {
                        false
                    };
                is_holder
                    || impl_item.items.iter().any(|m| {
                        if let ast::ImplItemKind::Function(f) = m {
                            f.name.name == "grow"
                        } else {
                            false
                        }
                    })
            }
            _ => false,
        });
        assert!(has_holder, "expected monomorphized Holder__i32 impl");

        // Verify that alloc<i32>(i32) was also monomorphized (nested call within Holder<i32>.grow)
        let has_alloc = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Function(f) => {
                f.name.name.starts_with("alloc__1_i32__1_i32__") && f.name.name.len() == 21 + 16
            }
            _ => false,
        });
        assert!(has_alloc, "expected nested alloc<i32> to be monomorphized");
    }

    /// Bare-Identifier calls to generic functions inside generic bodies carry
    // no explicit type arguments, so span-based discovery cannot see them.
    // The substituted instance body must resolve, instantiate, and rewrite
    // them (the run_app -> run_app_view shape).
    #[test]
    fn monomorphizes_bare_nested_generic_function_call() {
        let mut program = parse(
            "void inner<A>(&mut A a) { } \
             void outer<A>(&mut A app) { inner(&mut app); } \
             struct Box { i64 x; } \
             i32 main() { Box b; b.x = 1; outer(&mut b); return 0; }",
        );

        let source = find_function(&program, "outer");
        let type_params = vec!["A".to_string()];
        let mapping = HashMap::from_iter([(
            "A".to_string(),
            Type::Named {
                path: vec!["Box".to_string()],
                generics: vec![],
            },
        )]);
        let request = MonomorphRequest::Function {
            source: Box::new(source),
            type_params,
            mapping,
            call_span: Span::default(),
            is_imported: false,
        };

        let items = append_monomorphs(&mut program, &[request], &[]);

        // The nested inner<Box> instance must exist.
        let has_inner = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Function(f) => f.name.name.starts_with("inner__1_"),
            _ => false,
        });
        assert!(has_inner, "expected nested inner<Box> to be monomorphized");

        // The outer<Box> instance must call the mangled instance, not the
        // bare template name.
        let outer_instance = items
            .iter()
            .find_map(|item| match &item.kind {
                ast::ItemKind::Function(f) if f.name.name.starts_with("outer__1_") => Some(f),
                _ => None,
            })
            .expect("expected monomorphized outer<Box> instance");
        let mut called: Vec<String> = Vec::new();
        for stmt in &outer_instance.body.statements {
            let exprs: Vec<&ast::Expression> = match &stmt.kind {
                ast::StatementKind::Expression(e)
                | ast::StatementKind::Return(Some(e))
                | ast::StatementKind::Break(Some(e)) => vec![e],
                _ => vec![],
            };
            for expr in exprs {
                if let ast::ExpressionKind::Call { function, .. } = expr.kind.as_ref()
                    && let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref()
                {
                    called.push(ident.name.clone());
                }
            }
        }
        assert!(
            !called.iter().any(|name| name == "inner"),
            "bare template call must be rewritten, found: {called:?}"
        );
        assert!(
            called.iter().any(|name| name.starts_with("inner__")),
            "outer<Box> must call the mangled inner instance, found: {called:?}"
        );
    }

    #[test]
    fn monomorphizes_imported_generic_template_struct_and_methods() {
        let mut program = parse("struct Box<T> {} i32 main() { Box<i32> b; return 0; }");
        program
            .items
            .retain(|item| !matches!(&item.kind, ast::ItemKind::Struct(s) if s.name.name == "Box"));
        let artifact = ModuleArtifact {
            module_name: "box".to_string(),
            module_path: "std.box".to_string(),
            source_path: "std/box.ag".to_string(),
            source_hash_fnv1a64: 0,
            compiler_version: "test".to_string(),
            target_triple: "unknown".to_string(),
            code_artifacts: crate::module_artifact::ModuleCodeArtifacts {
                has_static_library: true,
                has_shared_library: false,
            },
            module_deps: Vec::new(),
            transitive_deps: Vec::new(),
            exports: Vec::new(),
            native_libs: Vec::new(),
            native_lib_paths: Vec::new(),
            generic_templates: vec![
                "struct Box<T> { T* ptr; }".to_string(),
                "impl Box<T> { T* get(Box<T>* self) { return self.ptr; } }".to_string(),
            ],
            artifact_path: None,
        };

        let items = append_monomorphs(&mut program, &[], &[artifact]);
        let has_box_struct = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Struct(s) => s.name.name == "Box__i32",
            _ => false,
        });
        assert!(has_box_struct, "expected monomorphized Box__i32 struct");

        let has_box_impl = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Impl(i) => i.items.iter().any(|m| match m {
                ast::ImplItemKind::Function(f) => f.name.name == "get",
                _ => false,
            }),
            _ => false,
        });
        assert!(
            has_box_impl,
            "expected monomorphized Box impl with get method"
        );
    }
}
