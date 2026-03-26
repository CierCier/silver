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
    let mut generated = HashSet::new();
    let mut new_items = Vec::new();

    let mut instantiations = HashMap::new();
    collect_struct_instantiations(program, &generic_types, &mut instantiations);

    for (key, inst) in instantiations.iter() {
        if generated.insert(key.clone()) {
            new_items.push(inst.item.clone());
        }
    }

    let impl_items = instantiate_impls(&generic_impls, &instantiations, &mut generated);
    new_items.extend(impl_items);

    let request_items = instantiate_requests(program, requests, &mut generated);
    new_items.extend(request_items);

    program.items.extend(new_items.clone());
    new_items
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
        ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::Asm(_)
        | ast::ExpressionKind::MacroCall { .. } => {}
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
                let mangled = mangle_name(&source.name.name, &args);
                let key = format!("fn::{mangled}");
                if !generated.insert(key) {
                    continue;
                }
                rewrite_function_calls(program, &source.name.name, &args, &mangled, call_span);
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
                if !generated.insert(key) {
                    continue;
                }
                rewrite_method_calls(program, &base, &method.name.name, &args, call_span);
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
        ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Asm(_)
        | ast::ExpressionKind::MacroCall { .. } => {}
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
            if expr.span == *span {
                if let ast::ExpressionKind::Identifier(ident) = function.kind.as_mut() {
                    if ident.name == name {
                        *function = Box::new(ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                                name: mangled.to_string(),
                                span: ident.span.clone(),
                            })),
                            span: function.span.clone(),
                        });
                    }
                }
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
            target_type: _,
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
        ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::Asm(_)
        | ast::ExpressionKind::MacroCall { .. } => {}
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
                if let ast::ExpressionKind::TypeName(ty) = receiver.kind.as_ref() {
                    if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                        if let Some(last) = named.path.last() {
                            if last.name == base {
                                if args.len()
                                    == named.generics.as_ref().map(|g| g.len()).unwrap_or(0)
                                {
                                    let new_args = args
                                        .iter()
                                        .map(|arg| type_to_ast(arg, span.clone()))
                                        .collect::<Vec<_>>();
                                    let mut new_named = named.clone();
                                    new_named.generics = Some(new_args);
                                    let new_ty = ast::Type {
                                        kind: Box::new(ast::TypeKind::Named(new_named)),
                                        span: ty.span.clone(),
                                    };
                                    *receiver = Box::new(ast::Expression {
                                        kind: Box::new(ast::ExpressionKind::TypeName(new_ty)),
                                        span: receiver.span.clone(),
                                    });
                                }
                            }
                        }
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
        Type::Array { element, length } => {
            let size = length.map(|len| ast::Expression {
                kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(
                    len as i128,
                ))),
                span: span.clone(),
            });
            ast::TypeKind::Array(Box::new(ast::ArrayType {
                element_type: Box::new(type_to_ast(element, span.clone())),
                size: size.map(Box::new),
            }))
        }
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
        Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
            is_concrete_type(inner, scopes)
        }
        Type::Array { element, .. } => is_concrete_type(element, scopes),
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
    use crate::lexer::lex;
    use crate::parser::Parser;

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
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

fn mangle_name(base: &str, args: &[Type]) -> String {
    let mut parts = Vec::new();
    for arg in args {
        parts.push(sanitize(&arg.canonical_key()));
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
