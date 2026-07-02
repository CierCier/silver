use std::collections::{HashMap, HashSet};

use crate::lexer::Span;
use crate::parser::ast;
use crate::types::Type;

#[derive(Debug, Clone)]
pub enum MonomorphRequest {
    Function {
        source: ast::FunctionItem,
        type_params: Vec<String>,
        mapping: HashMap<String, Type>,
        call_span: Span,
    },
    ImplMethod {
        impl_item: ast::ImplItem,
        method: ast::ImplFunction,
        type_params: Vec<String>,
        mapping: HashMap<String, Type>,
        call_span: Span,
    },
}

pub fn append_monomorphs(
    program: &mut ast::Program,
    requests: &[MonomorphRequest],
) -> Vec<ast::Item> {
    let generic_types = collect_generic_types(program);
    let generic_impls = collect_generic_impls(program);
    let generic_fns = collect_generic_fns(program);
    let mut generated = HashSet::new();
    let mut all_new_items = Vec::new();

    let mut instantiations = HashMap::new();
    collect_struct_instantiations(program, &generic_types, &mut instantiations);

    for (key, inst) in instantiations.iter() {
        if generated.insert(key.clone()) {
            let item = inst.item.clone();
            all_new_items.push(item);
            program.items.push(inst.item.clone());
        }
    }

    let impl_items = instantiate_impls(&generic_impls, &instantiations, &mut generated);
    for item in &impl_items {
        all_new_items.push(item.clone());
        program.items.push(item.clone());
    }

    // Process function/method monomorphization requests to fixpoint.
    // This handles nested generic calls: e.g., alloc<T>() inside Rc.new<T> is deferred
    // during type checking (non-concrete mapping), so we must detect it here and
    // create the monomorphized callee in a second round.
    let mut current_requests: Vec<MonomorphRequest> = requests.to_vec();
    while !current_requests.is_empty() {
        let new_items = instantiate_requests(program, &current_requests, &mut generated);
        for item in &new_items {
            all_new_items.push(item.clone());
            program.items.push(item.clone());
        }

        // Scan for remaining generic calls with concrete type args in the updated program
        current_requests = collect_remaining_function_requests(program, &new_items, &generic_fns, &generated);
    }
    all_new_items
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
    let mut types = HashMap::new();
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
            ast::ItemKind::Enum(enum_item) => {
                if enum_item.generics.is_some() {
                    types.insert(
                        enum_item.name.name.clone(),
                        GenericTypeItem::Enum(enum_item.clone()),
                    );
                }
            }
            _ => {}
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
        if impl_item.generics.is_some() {
            impls.push(impl_item.clone());
        } else if is_generic_self_type(&impl_item.self_type) {
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
            // Register associated type names so they're treated as type params
            for item in &trait_item.items {
                if let ast::TraitItemKind::AssociatedType(assoc) = item {
                    if let Some(scope) = scopes.last_mut() {
                        scope.insert(assoc.name.name.clone());
                    }
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
                let mut implicit = HashSet::new();
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
        | ast::ExpressionKind::Index { object, .. } => {
            collect_expression_instantiations(object, generic_structs, instantiations, scopes);
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
        | ast::ExpressionKind::Asm(_) => {}
    }
}

fn collect_type_instantiations(
    ty: &ast::Type,
    generic_structs: &HashMap<String, GenericTypeItem>,
    instantiations: &mut HashMap<String, TypeInstance>,
    scopes: &Vec<HashSet<String>>,
) {
    if let Some((base, args)) = concrete_named_type(ty, scopes) {
        if let Some(item) = generic_structs.get(&base) {
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
    }

    match ty.kind.as_ref() {
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
            span: field.span.clone(),
        })
        .collect::<Vec<_>>();

    let item = ast::Item {
        kind: ast::ItemKind::Struct(ast::StructItem {
            name: ast::Identifier {
                name: mangled.to_string(),
                span: struct_item.name.span.clone(),
            },
            generics: None,
            fields,
        }),
        span: struct_item.name.span.clone(),
        visibility: ast::Visibility::Private,
        attributes: Vec::new(),
    };
    item
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
                            span: field.span.clone(),
                        })
                        .collect(),
                ),
            },
            discriminant: variant.discriminant,
            span: variant.span.clone(),
        })
        .collect::<Vec<_>>();

    ast::Item {
        kind: ast::ItemKind::Enum(ast::EnumItem {
            name: ast::Identifier {
                name: mangled.to_string(),
                span: enum_item.name.span.clone(),
            },
            generics: None,
            variants,
        }),
        span: enum_item.name.span.clone(),
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
            let mut new_impl = impl_item.clone();
            new_impl.generics = None;
            new_impl.self_type = substitute_ast_type(&impl_item.self_type, &mapping);
            for item in &mut new_impl.items {
                if let ast::ImplItemKind::Function(func) = item {
                    func.generics = None;
                    for param in &mut func.parameters {
                        param.param_type = substitute_ast_type(&param.param_type, &mapping);
                    }
                    if let Some(return_type) = &mut func.return_type {
                        *return_type = substitute_ast_type(return_type, &mapping);
                    }
                    substitute_block_types(&mut func.body, &mapping);
                }
            }

            items.push(ast::Item {
                kind: ast::ItemKind::Impl(new_impl),
                span: impl_item.self_type.span.clone(),
                visibility: ast::Visibility::Private,
                attributes: Vec::new(),
            });
        }
    }

    items
}

fn instantiate_requests(
    program: &mut ast::Program,
    requests: &[MonomorphRequest],
    generated: &mut HashSet<String>,
) -> Vec<ast::Item> {
    let mut items = Vec::new();
    for request in requests {
        match request {
            MonomorphRequest::Function {
                source,
                type_params,
                mapping,
                call_span,
            } => {
                let args = ordered_args(type_params, mapping);

                // Compute parameter type signature to disambiguate overloaded generic functions
                // (e.g. alloc<i32>() vs alloc<i32>(i64))
                let param_sig: String = source.parameters.iter()
                    .map(|p| {
                        let concrete = Type::from_ast(&p.param_type).substitute(mapping);
                        sanitize(&concrete.canonical_key())
                    })
                    .collect::<Vec<_>>()
                    .join("_");

                let mangled_base = mangle_name(&source.name.name, &args);
                let mangled = if param_sig.is_empty() {
                    format!("{}__v", mangled_base)
                } else {
                    format!("{}_{}", mangled_base, param_sig)
                };
                let key = format!("fn::{mangled}");

                // Always rewrite call sites, even if the function was already
                // monomorphized from a different call site. The dedup below
                // prevents generating duplicate function definitions.
                rewrite_function_calls(program, &source.name.name, &args, &mangled, call_span);

                if !generated.insert(key) {
                    continue;
                }
                let mut func = source.clone();
                func.generics = None;
                func.name = ast::Identifier {
                    name: mangled,
                    span: func.name.span.clone(),
                };
                for param in &mut func.parameters {
                    param.param_type = substitute_ast_type(&param.param_type, mapping);
                }
                if let Some(return_type) = &mut func.return_type {
                    *return_type = substitute_ast_type(return_type, mapping);
                }
                substitute_block_types(&mut func.body, mapping);

                items.push(ast::Item {
                    kind: ast::ItemKind::Function(func),
                    span: source.name.span.clone(),
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                });
            }
            MonomorphRequest::ImplMethod {
                impl_item,
                method,
                type_params,
                mapping,
                call_span,
            } => {
                let args = ordered_args(type_params, mapping);
                let base = impl_self_base_name(&impl_item.self_type).unwrap_or_default();
                let mangled = mangle_name(&base, &args);
                let key = format!("impl::{mangled}");
                // Always rewrite method calls — this must happen for EVERY request
                // (each method in the same impl needs its method calls rewritten).
                rewrite_method_calls(program, &base, &method.name.name, &args, call_span);
                // Only generate the monomorphized impl once per unique key.
                if !generated.insert(key) {
                    continue;
                }
                let mut new_impl = impl_item.clone();
                new_impl.generics = None;
                new_impl.self_type = substitute_ast_type(&impl_item.self_type, mapping);
                new_impl.items = new_impl
                    .items
                    .into_iter()
                    .map(|mut item| {
                        if let ast::ImplItemKind::Function(func) = &mut item {
                            func.generics = None;
                            for param in &mut func.parameters {
                                param.param_type = substitute_ast_type(&param.param_type, mapping);
                            }
                            if let Some(return_type) = &mut func.return_type {
                                *return_type = substitute_ast_type(return_type, mapping);
                            }
                            substitute_block_types(&mut func.body, mapping);
                        }
                        item
                    })
                    .collect();

                items.push(ast::Item {
                    kind: ast::ItemKind::Impl(new_impl),
                    span: impl_item.self_type.span.clone(),
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
        ast::ExpressionKind::TypeName(ty) => {
            *ty = substitute_ast_type(ty, mapping);
        }
        ast::ExpressionKind::Identifier(ident) => {
            if let Some(rewrite) = mapping.get(&ident.name) {
                let ty = type_to_ast(rewrite, ident.span.clone());
                expr.kind = Box::new(ast::ExpressionKind::TypeName(ty));
            }
        }
        ast::ExpressionKind::StructLiteral { path, fields } => {
            if let Some(last) = path.last_mut() {
                if let Some(rewrite) = mapping.get(&last.name) {
                    if let Type::Named { path: new_path, .. } = rewrite {
                        if let Some(new_name) = new_path.last() {
                            last.name = new_name.clone();
                        }
                    }
                }
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
            *target_type = Box::new(replaced);
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
        | ast::ExpressionKind::Index { object, .. } => {
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
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            substitute_expression_types(iterable, mapping);
            substitute_block_types(body, mapping);
        }
        ast::ExpressionKind::Literal(_) | ast::ExpressionKind::Asm(_) => {}
    }
}

fn substitute_ast_type(ty: &ast::Type, mapping: &HashMap<String, Type>) -> ast::Type {
    let concrete = Type::from_ast(ty).substitute(mapping);
    type_to_ast(&concrete, ty.span.clone())
}

fn rewrite_function_calls(
    program: &mut ast::Program,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
) {
    for item in &mut program.items {
        rewrite_item_function_calls(item, name, args, mangled, span);
    }
}

fn rewrite_item_function_calls(
    item: &mut ast::Item,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
) {
    match &mut item.kind {
        ast::ItemKind::Function(func) => {
            rewrite_block_function_calls(&mut func.body, name, args, mangled, span)
        }
        ast::ItemKind::Impl(impl_item) => {
            for impl_item in &mut impl_item.items {
                if let ast::ImplItemKind::Function(func) = impl_item {
                    rewrite_block_function_calls(&mut func.body, name, args, mangled, span)
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
) {
    for stmt in &mut block.statements {
        rewrite_statement_function_calls(stmt, name, args, mangled, span);
    }
}

fn rewrite_statement_function_calls(
    stmt: &mut ast::Statement,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
) {
    match &mut stmt.kind {
        ast::StatementKind::Block(block) => {
            rewrite_block_function_calls(block, name, args, mangled, span)
        }
        ast::StatementKind::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.initializer {
                rewrite_expression_function_calls(init, name, args, mangled, span);
            }
        }
        ast::StatementKind::Expression(expr)
        | ast::StatementKind::Return(Some(expr))
        | ast::StatementKind::Break(Some(expr)) => {
            rewrite_expression_function_calls(expr, name, args, mangled, span)
        }
        ast::StatementKind::Return(None) | ast::StatementKind::Break(None) => {}
        ast::StatementKind::Continue => {}
        ast::StatementKind::Defer(inner) => {
            rewrite_statement_function_calls(inner, name, args, mangled, span)
        }
    }
}

fn rewrite_expression_function_calls(
    expr: &mut ast::Expression,
    name: &str,
    args: &[Type],
    mangled: &str,
    span: &Span,
) {
    match expr.kind.as_mut() {
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
                    // For TypeName-style calls, match by function name AND generic argument types
                    // (not just span). This handles nested generic calls inside monomorphized
                    // bodies where the span differs from the original request's call_span.
                    let should_rewrite = match function.kind.as_mut() {
                        ast::ExpressionKind::Identifier(ident) => {
                            ident.name == name && expr.span == *span
                        }
                        ast::ExpressionKind::TypeName(ty) => {
                            if let ast::TypeKind::Named(named) = ty.kind.as_mut() {
                                if named.path.len() == 1 && named.path[0].name == name {
                                    if let Some(generics) = &named.generics {
                                        let actual_args: Vec<Type> =
                                            generics.iter().map(|g| Type::from_ast(g)).collect();
                                        actual_args == args
                                    } else {
                                        args.is_empty()
                                    }
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        }
                        _ => false,
                    };
                    if should_rewrite {
                        *function = Box::new(ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                                name: mangled.to_string(),
                                span: function.span.clone(),
                            })),
                            span: function.span.clone(),
                        });
                    }
            for arg in arguments {
                rewrite_expression_function_calls(arg, name, args, mangled, span);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            rewrite_expression_function_calls(receiver, name, args, mangled, span);
            for arg in arguments {
                rewrite_expression_function_calls(arg, name, args, mangled, span);
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. } => {
            rewrite_expression_function_calls(object, name, args, mangled, span);
        }
        ast::ExpressionKind::Cast {
            expression,
            ..
        } => {
            rewrite_expression_function_calls(expression, name, args, mangled, span);
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            rewrite_expression_function_calls(left, name, args, mangled, span);
            rewrite_expression_function_calls(right, name, args, mangled, span);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. } => {
            rewrite_expression_function_calls(operand, name, args, mangled, span)
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rewrite_expression_function_calls(condition, name, args, mangled, span);
            rewrite_block_function_calls(then_branch, name, args, mangled, span);
            if let Some(branch) = else_branch {
                rewrite_block_function_calls(branch, name, args, mangled, span);
            }
        }
        ast::ExpressionKind::While { condition, body } => {
            rewrite_expression_function_calls(condition, name, args, mangled, span);
            rewrite_block_function_calls(body, name, args, mangled, span);
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(init_expr) = &mut init.initializer {
                rewrite_expression_function_calls(init_expr, name, args, mangled, span);
            }
            rewrite_expression_function_calls(condition, name, args, mangled, span);
            rewrite_expression_function_calls(increment, name, args, mangled, span);
            rewrite_block_function_calls(body, name, args, mangled, span);
        }
        ast::ExpressionKind::Match { expression, arms } => {
            rewrite_expression_function_calls(expression, name, args, mangled, span);
            for arm in arms {
                rewrite_expression_function_calls(&mut arm.body, name, args, mangled, span);
            }
        }
        ast::ExpressionKind::Block(block) => {
            rewrite_block_function_calls(block, name, args, mangled, span)
        }
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr)
                    | ast::InitializerItem::Field { value: expr, .. } => {
                        rewrite_expression_function_calls(expr, name, args, mangled, span)
                    }
                    ast::InitializerItem::Index { index, value } => {
                        rewrite_expression_function_calls(index, name, args, mangled, span);
                        rewrite_expression_function_calls(value, name, args, mangled, span);
                    }
                }
            }
        }
        ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
            for item in items {
                rewrite_expression_function_calls(item, name, args, mangled, span);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                rewrite_expression_function_calls(&mut field.value, name, args, mangled, span);
            }
        }
        ast::ExpressionKind::Move(inner)
        | ast::ExpressionKind::Comptime(inner)
        | ast::ExpressionKind::Reference {
            expression: inner, ..
        } => rewrite_expression_function_calls(inner, name, args, mangled, span),
        ast::ExpressionKind::MacroCall { args: macro_args, .. } => {
            for arg in macro_args {
                if let ast::MacroArg::Expression(expr) = arg {
                    rewrite_expression_function_calls(expr, name, args, mangled, span);
                }
            }
        }
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            rewrite_expression_function_calls(iterable, name, args, mangled, span);
            rewrite_block_function_calls(body, name, args, mangled, span);
        }
        ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::Asm(_) => {}
    }
}

fn rewrite_method_calls(
    program: &mut ast::Program,
    base: &str,
    method: &str,
    args: &[Type],
    span: &Span,
) {
    for item in &mut program.items {
        rewrite_item_method_calls(item, base, method, args, span);
    }
}

fn rewrite_item_method_calls(
    item: &mut ast::Item,
    base: &str,
    method: &str,
    args: &[Type],
    span: &Span,
) {
    match &mut item.kind {
        ast::ItemKind::Function(func) => {
            rewrite_block_method_calls(&mut func.body, base, method, args, span)
        }
        ast::ItemKind::Impl(impl_item) => {
            for impl_item in &mut impl_item.items {
                if let ast::ImplItemKind::Function(func) = impl_item {
                    rewrite_block_method_calls(&mut func.body, base, method, args, span)
                }
            }
        }
        _ => {}
    }
}

fn rewrite_block_method_calls(
    block: &mut ast::Block,
    base: &str,
    method: &str,
    args: &[Type],
    span: &Span,
) {
    for stmt in &mut block.statements {
        rewrite_statement_method_calls(stmt, base, method, args, span);
    }
}

fn rewrite_statement_method_calls(
    stmt: &mut ast::Statement,
    base: &str,
    method: &str,
    args: &[Type],
    span: &Span,
) {
    match &mut stmt.kind {
        ast::StatementKind::Block(block) => {
            rewrite_block_method_calls(block, base, method, args, span)
        }
        ast::StatementKind::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.initializer {
                rewrite_expression_method_calls(init, base, method, args, span);
            }
        }
        ast::StatementKind::Expression(expr)
        | ast::StatementKind::Return(Some(expr))
        | ast::StatementKind::Break(Some(expr)) => {
            rewrite_expression_method_calls(expr, base, method, args, span)
        }
        ast::StatementKind::Return(None) | ast::StatementKind::Break(None) => {}
        ast::StatementKind::Defer(inner) => {
            rewrite_statement_method_calls(inner, base, method, args, span)
        }
        ast::StatementKind::Continue => {}
    }
}

fn rewrite_expression_method_calls(
    expr: &mut ast::Expression,
    base: &str,
    method: &str,
    args: &[Type],
    span: &Span,
) {
    match expr.kind.as_mut() {
        ast::ExpressionKind::MethodCall {
            receiver,
            method: call_method,
            arguments,
        } => {
            if expr.span == *span && call_method.name == method {
                // Determine the base type name — may be Identifier("Rc") for static
                // calls like `Rc.new(...)` or TypeName(Named("Rc")) for explicit
                // type expressions like `Rc<i32>.new(...)`.
                let mut base_name: Option<String> = None;
                let mut current_generics: Option<&Vec<ast::Type>> = None;
                match receiver.kind.as_ref() {
                    ast::ExpressionKind::TypeName(ty) => {
                        if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                            if let Some(last) = named.path.last() {
                                if last.name == base {
                                    base_name = Some(last.name.clone());
                                    current_generics = named.generics.as_ref();
                                }
                            }
                        }
                    }
                    ast::ExpressionKind::Identifier(ident) => {
                        if ident.name == base {
                            base_name = Some(ident.name.clone());
                        }
                    }
                    _ => {}
                }
                if let Some(_) = base_name {
                    // For Identifier receivers (Rc.new(...)), current_generics is None and
                    // we always need to rewrite. For TypeName receivers (Rc<i32>.new(...)),
                    // skip if the generic count already matches.
                    let should_rewrite = current_generics.is_none()
                        || args.len() == current_generics.unwrap().len();
                    if should_rewrite {
                        let new_args = args
                            .iter()
                            .map(|arg| type_to_ast(arg, span.clone()))
                            .collect::<Vec<_>>();
                        let new_named = ast::NamedType {
                            path: vec![ast::Identifier {
                                name: base.to_string(),
                                span: span.clone(),
                            }],
                            generics: Some(new_args),
                        };
                        *receiver = Box::new(ast::Expression {
                            kind: Box::new(ast::ExpressionKind::TypeName(ast::Type {
                                kind: Box::new(ast::TypeKind::Named(new_named)),
                                span: receiver.span.clone(),
                            })),
                            span: receiver.span.clone(),
                        });
                    }
                }
            }
            rewrite_expression_method_calls(receiver, base, method, args, span);
            for arg in arguments {
                rewrite_expression_method_calls(arg, base, method, args, span);
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. } => {
            rewrite_expression_method_calls(object, base, method, args, span);
        }
        ast::ExpressionKind::Cast {
            expression,
            target_type: _,
        } => {
            rewrite_expression_method_calls(expression, base, method, args, span);
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            rewrite_expression_method_calls(function, base, method, args, span);
            for arg in arguments {
                rewrite_expression_method_calls(arg, base, method, args, span);
            }
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            rewrite_expression_method_calls(left, base, method, args, span);
            rewrite_expression_method_calls(right, base, method, args, span);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. } => {
            rewrite_expression_method_calls(operand, base, method, args, span)
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rewrite_expression_method_calls(condition, base, method, args, span);
            rewrite_block_method_calls(then_branch, base, method, args, span);
            if let Some(branch) = else_branch {
                rewrite_block_method_calls(branch, base, method, args, span);
            }
        }
        ast::ExpressionKind::While { condition, body } => {
            rewrite_expression_method_calls(condition, base, method, args, span);
            rewrite_block_method_calls(body, base, method, args, span);
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(init_expr) = &mut init.initializer {
                rewrite_expression_method_calls(init_expr, base, method, args, span);
            }
            rewrite_expression_method_calls(condition, base, method, args, span);
            rewrite_expression_method_calls(increment, base, method, args, span);
            rewrite_block_method_calls(body, base, method, args, span);
        }
        ast::ExpressionKind::Match { expression, arms } => {
            rewrite_expression_method_calls(expression, base, method, args, span);
            for arm in arms {
                rewrite_expression_method_calls(&mut arm.body, base, method, args, span);
            }
        }
        ast::ExpressionKind::Block(block) => {
            rewrite_block_method_calls(block, base, method, args, span)
        }
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr)
                    | ast::InitializerItem::Field { value: expr, .. } => {
                        rewrite_expression_method_calls(expr, base, method, args, span)
                    }
                    ast::InitializerItem::Index { index, value } => {
                        rewrite_expression_method_calls(index, base, method, args, span);
                        rewrite_expression_method_calls(value, base, method, args, span);
                    }
                }
            }
        }
        ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
            for item in items {
                rewrite_expression_method_calls(item, base, method, args, span);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                rewrite_expression_method_calls(&mut field.value, base, method, args, span);
            }
        }
        ast::ExpressionKind::Move(inner)
        | ast::ExpressionKind::Comptime(inner)
        | ast::ExpressionKind::Reference {
            expression: inner, ..
        } => rewrite_expression_method_calls(inner, base, method, args, span),
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            rewrite_expression_method_calls(iterable, base, method, args, span);
            rewrite_block_method_calls(body, base, method, args, span);
        }
        ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::Asm(_)
        | ast::ExpressionKind::MacroCall { .. } => {}
    }
}

fn type_to_ast(ty: &Type, span: Span) -> ast::Type {
    let kind = match ty {
        Type::Unit => ast::TypeKind::Tuple(Vec::new()),
        Type::Primitive(p) => ast::TypeKind::Primitive(p.clone()),
        Type::Named { path, generics } => ast::TypeKind::Named(ast::NamedType {
            path: path
                .iter()
                .map(|name| ast::Identifier {
                    name: name.clone(),
                    span: span.clone(),
                })
                .collect(),
            generics: if generics.is_empty() {
                None
            } else {
                Some(
                    generics
                        .iter()
                        .map(|inner| type_to_ast(inner, span.clone()))
                        .collect(),
                )
            },
        }),
        Type::Reference { is_mutable, inner } => ast::TypeKind::Reference(ast::ReferenceType {
            is_mutable: *is_mutable,
            lifetime: None,
            inner: Box::new(type_to_ast(inner, span.clone())),
        }),
        Type::Pointer { is_mutable, inner } => ast::TypeKind::Pointer(ast::PointerType {
            is_mutable: *is_mutable,
            inner: Box::new(type_to_ast(inner, span.clone())),
        }),
        Type::Slice { element } => ast::TypeKind::Named(ast::NamedType {
            path: vec![ast::Identifier {
                name: "Slice".to_string(),
                span: span.clone(),
            }],
            generics: Some(vec![type_to_ast(element, span.clone())]),
        }),
        Type::Array { element, size } => ast::TypeKind::Array(Box::new(ast::ArrayType {
            element_type: Box::new(type_to_ast(element, span.clone())),
            size: *size as i64,
            span: span.clone(),
        })),
        Type::Optional { inner } => {
            ast::TypeKind::Optional(Box::new(type_to_ast(inner, span.clone())))
        }
        Type::Tuple(items) => ast::TypeKind::Tuple(
            items
                .iter()
                .map(|inner| type_to_ast(inner, span.clone()))
                .collect(),
        ),
        Type::Function {
            params,
            return_type,
        } => ast::TypeKind::Function(ast::FunctionType {
            parameters: params
                .iter()
                .map(|inner| type_to_ast(inner, span.clone()))
                .collect(),
            return_type: Box::new(type_to_ast(return_type, span.clone())),
        }),
        Type::Unknown => ast::TypeKind::Named(ast::NamedType {
            path: vec![ast::Identifier {
                name: "_".to_string(),
                span: span.clone(),
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
    let mut params = HashSet::new();
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
        ast::TypeKind::Array(array) => {
            collect_implicit_type_params(&array.element_type, params)
        }
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
        Type::Primitive(_) | Type::Unit => true,
        Type::Unknown => false,
    }
}

fn build_mapping_from_generics(
    generics: Option<&ast::Generics>,
    args: &[Type],
) -> HashMap<String, Type> {
    let mut mapping = HashMap::new();
    if let Some(generics) = generics {
        for (param, arg) in generics.params.iter().zip(args.iter()) {
            if let ast::GenericParam::Type(type_param) = param {
                mapping.insert(type_param.name.name.clone(), arg.clone());
            }
        }
    }
    mapping
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{lex, Span};
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
                if let ast::ItemKind::Function(f) = &item.kind {
                    if f.name.name == name {
                        return Some(f.clone());
                    }
                }
                None
            })
            .expect("function not found")
    }

    #[test]
    fn monomorphizes_generic_structs() {
        let mut program = parse("struct Box<T> { T value; } i32 main() { Box<i32> b; return 0; }");
        let items = append_monomorphs(&mut program, &[]);
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
        let mapping = HashMap::from_iter([("T".to_string(), Type::Primitive(ast::PrimitiveType::I32))]);

        let requests = vec![MonomorphRequest::Function {
            source,
            type_params,
            mapping,
            call_span: Span { start: 0, end: 0 },
        }];

        let items = append_monomorphs(&mut program, &requests);
        let has_fn = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Function(f) => f.name.name == "foo__i32_i32",
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
            source,
            type_params,
            mapping,
            call_span: Span { start: 0, end: 0 },
        }];

        let items = append_monomorphs(&mut program, &requests);
        let has_fn = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Function(f) => f.name.name == "bar__i32_f64_i32_f64",
            _ => false,
        });
        assert!(has_fn, "expected monomorphized function bar__i32_f64");
    }

    #[test]
    fn monomorphizes_duplicate_request_only_once() {
        let mut program = parse("T foo<T>(T x) { return x; } i32 main() { return 0; }");
        let source = find_function(&program, "foo");
        let type_params = vec!["T".to_string()];
        let mapping = HashMap::from_iter([("T".to_string(), Type::Primitive(ast::PrimitiveType::I32))]);

        let request = MonomorphRequest::Function {
            source,
            type_params,
            mapping,
            call_span: Span { start: 0, end: 0 },
        };

        let items = append_monomorphs(&mut program, &[request.clone(), request]);
        let count = items
            .iter()
            .filter(|item| matches!(&item.kind, ast::ItemKind::Function(f) if f.name.name == "foo__i32_i32"))
            .count();
        assert_eq!(count, 1, "duplicate request should only produce one monomorph");
    }

    #[test]
    fn monomorphizes_struct_and_function_together() {
        let mut program =
            parse("struct Pair<T, U> { T first; U second; } T id<T>(T x) { return x; } i32 main() { return 0; }");
        let source = find_function(&program, "id");
        let type_params = vec!["T".to_string()];
        let mapping = HashMap::from_iter([("T".to_string(), Type::Primitive(ast::PrimitiveType::I32))]);

        let requests = vec![MonomorphRequest::Function {
            source,
            type_params,
            mapping,
            call_span: Span { start: 0, end: 0 },
        }];

        let items = append_monomorphs(&mut program, &requests);
        let has_fn = items.iter().any(|item| match &item.kind {
            ast::ItemKind::Function(f) => f.name.name == "id__i32_i32",
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
}

fn mapping_covers_impl(mapping: &HashMap<String, Type>, generics: Option<&ast::Generics>) -> bool {
    let Some(generics) = generics else {
        return true;
    };
    for param in &generics.params {
        if let ast::GenericParam::Type(type_param) = param {
            if !mapping.contains_key(&type_param.name.name) {
                return false;
            }
        }
    }
    true
}

fn is_generic_self_type(ty: &ast::Type) -> bool {
    matches!(ty.kind.as_ref(), ast::TypeKind::Named(named) if named.generics.is_some())
}

fn impl_self_base_name(ty: &ast::Type) -> Option<String> {
    let ast::TypeKind::Named(named) = ty.kind.as_ref() else {
        return None;
    };
    named.path.last().map(|id| id.name.clone())
}

pub fn mangle_name(base: &str, args: &[Type]) -> String {
    let mut parts = Vec::new();
    for arg in args {
        let key = arg
            .canonical_key()
            .replace('*', "ptr_")
            .replace('&', "ref_");
        parts.push(sanitize(&key));
    }
    if parts.is_empty() {
        base.to_string()
    } else {
        format!("{}__{}", base, parts.join("_"))
    }
}

fn sanitize(value: &str) -> String {
    let mut out = String::new();
    let mut last_underscore = false;
    for ch in value.chars() {
        let is_ok = ch.is_ascii_alphanumeric();
        if is_ok {
            out.push(ch);
            last_underscore = false;
        } else if !last_underscore {
            out.push('_');
            last_underscore = true;
        }
    }
    if out.is_empty() { "_".to_string() } else { out }
}

fn ordered_args(type_params: &[String], mapping: &HashMap<String, Type>) -> Vec<Type> {
    type_params
        .iter()
        .filter_map(|name| mapping.get(name).cloned())
        .collect()
}

/// Collect names of all generic functions (functions with generic type parameters).
fn collect_generic_fns(program: &ast::Program) -> HashSet<String> {
    let mut fns = HashSet::new();
    for item in &program.items {
        if let ast::ItemKind::Function(func) = &item.kind {
            if func.generics.is_some() {
                fns.insert(func.name.name.clone());
            }
        }
    }
    fns
}

/// Find a generic function by name and parameter count.
fn find_generic_fn<'a>(program: &'a ast::Program, name: &str, param_count: usize) -> Option<&'a ast::FunctionItem> {
    for item in &program.items {
        if let ast::ItemKind::Function(func) = &item.kind {
            if func.name.name == name && func.generics.is_some() && func.parameters.len() == param_count {
                return Some(func);
            }
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
        ast::ExpressionKind::Call { function, arguments } => {
            if let ast::ExpressionKind::TypeName(ty) = function.kind.as_ref() {
                if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                    if named.path.len() == 1 {
                        let fn_name = &named.path[0].name;
                        if generic_fns.contains(fn_name) {
                            if let Some(generics) = &named.generics {
                                let concrete_args: Vec<Type> =
                                    generics.iter().map(|g| Type::from_ast(g)).collect();
                                // Only process if all type args are concrete
                                if concrete_args.iter().all(|arg| is_concrete(arg)) {
                                    results.push((
                                        fn_name.clone(),
                                        concrete_args,
                                        expr.span.clone(),
                                        arguments.len(),
                                    ));
                                }
                            }
                        }
                    }
                }
            }
            // Recurse into arguments
            for arg in arguments {
                results.extend(collect_expression_remaining_calls(arg, generic_fns));
            }
        }
        ast::ExpressionKind::MethodCall { receiver, arguments, .. } => {
            results.extend(collect_expression_remaining_calls(receiver, generic_fns));
            for arg in arguments {
                results.extend(collect_expression_remaining_calls(arg, generic_fns));
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. } => {
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
        ast::ExpressionKind::If { condition, then_branch, else_branch } => {
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
        ast::ExpressionKind::For { init, condition, increment, body } => {
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
                results.extend(collect_expression_remaining_calls(&field.value, generic_fns));
            }
        }
        ast::ExpressionKind::Move(inner)
        | ast::ExpressionKind::Comptime(inner)
        | ast::ExpressionKind::Reference { expression: inner, .. } => {
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
        | ast::ExpressionKind::Asm(_) => {}
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
            generics.iter().all(|inner| is_concrete(inner))
        }
        Type::Primitive(_) | Type::Unit => true,
        Type::Pointer { inner, .. } | Type::Reference { inner, .. } => is_concrete(inner),
        Type::Slice { element } => is_concrete(element),
        Type::Optional { inner } => is_concrete(inner),
        Type::Tuple(items) => items.iter().all(|inner| is_concrete(inner)),
        Type::Function { params, return_type } => {
            params.iter().all(|inner| is_concrete(inner)) && is_concrete(return_type)
        }
        Type::Array { element, .. } => is_concrete(element),
        Type::Unknown => false,
    }
}

/// Collect remaining concrete generic function calls that need monomorphization.
/// Returns MonomorphRequest values to be processed in a subsequent round.
fn collect_remaining_function_requests(
    program: &ast::Program,
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
        let mangled_base = mangle_name(&fn_name, &concrete_args);
        let key = format!("fn::{mangled_base}");
        if generated.contains(&key) {
            continue;
        }
        if let Some(source) = find_generic_fn(program, &fn_name, param_count) {
            let mapping = build_mapping_from_generics(source.generics.as_ref(), &concrete_args);
            // Use source's type param order for deterministic mangling
            let type_params: Vec<String> = source
                .generics
                .as_ref()
                .map(|g| {
                    g.params
                        .iter()
                        .filter_map(|p| {
                            if let ast::GenericParam::Type(tp) = p {
                                mapping.contains_key(&tp.name.name).then(|| tp.name.name.clone())
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
                    source: source.clone(),
                    type_params,
                    mapping,
                    call_span,
                });
            }
        }
    }
    requests
}
