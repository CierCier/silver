use crate::parser::ast;

#[derive(Debug, Clone)]
struct Node {
    label: String,
    children: Vec<Node>,
}

impl Node {
    fn new(label: impl Into<String>) -> Self {
        Self {
            label: label.into(),
            children: Vec::new(),
        }
    }
}

pub fn render_program(program: &ast::Program) -> String {
    let root = program_node(program);
    let mut lines = vec![root.label];
    for (index, child) in root.children.iter().enumerate() {
<<<<<<< HEAD
        render_node(
            child,
            "",
            index + 1 == root.children.len(),
            &mut lines,
        );
=======
        render_node(child, "", index + 1 == root.children.len(), &mut lines);
>>>>>>> cc823df (shift to LL3)
    }
    lines.join("\n")
}

fn render_node(node: &Node, prefix: &str, is_last: bool, lines: &mut Vec<String>) {
    let branch = if is_last { "`-- " } else { "|-- " };
    lines.push(format!("{prefix}{branch}{}", node.label));
    let child_prefix = if is_last {
        format!("{prefix}    ")
    } else {
        format!("{prefix}|   ")
    };
    for (index, child) in node.children.iter().enumerate() {
        render_node(
            child,
            &child_prefix,
            index + 1 == node.children.len(),
            lines,
        );
    }
}

fn program_node(program: &ast::Program) -> Node {
    let mut node = Node::new(format!(
        "Program [{}..{}]",
        program.span.start, program.span.end
    ));
    node.children = program.items.iter().map(item_node).collect();
    node
}

fn item_node(item: &ast::Item) -> Node {
    let mut node = match &item.kind {
        ast::ItemKind::Function(function) => function_node(function),
        ast::ItemKind::Struct(strukt) => struct_node(strukt),
        ast::ItemKind::Enum(enm) => enum_node(enm),
        ast::ItemKind::Impl(impl_item) => impl_node(impl_item),
        ast::ItemKind::Trait(trait_item) => trait_node(trait_item),
        ast::ItemKind::Import(import) => import_node(import),
        ast::ItemKind::ExternFunction(extern_fn) => extern_fn_node(extern_fn),
<<<<<<< HEAD
        ast::ItemKind::ExternBlock(extern_block) => extern_block_node(extern_block),
    };

    node.label = format!(
        "{} [{}..{}]",
        node.label, item.span.start, item.span.end
    );

    let mut children = Vec::new();
    children.push(Node::new(format!("visibility: {}", visibility_name(&item.visibility))));
=======
        ast::ItemKind::ExternVariable(extern_var) => extern_var_node(extern_var),
        ast::ItemKind::ExternBlock(extern_block) => extern_block_node(extern_block),
    };

    node.label = format!("{} [{}..{}]", node.label, item.span.start, item.span.end);

    let mut children = Vec::new();
    children.push(Node::new(format!(
        "visibility: {}",
        visibility_name(&item.visibility)
    )));
>>>>>>> cc823df (shift to LL3)
    if !item.attributes.is_empty() {
        let mut attrs = Node::new("attributes");
        attrs.children = item.attributes.iter().map(attribute_node).collect();
        children.push(attrs);
    }
    children.extend(node.children);
    node.children = children;
    node
}

fn function_node(function: &ast::FunctionItem) -> Node {
    let mut node = Node::new(format!("Function {}", function.name.name));
    if let Some(generics) = &function.generics {
        node.children.push(generics_node(generics));
    }
    if !function.parameters.is_empty() {
        let mut params = Node::new("parameters");
        params.children = function.parameters.iter().map(parameter_node).collect();
        node.children.push(params);
    }
    if let Some(return_type) = &function.return_type {
        let mut ret = Node::new("return_type");
        ret.children.push(type_node(return_type));
        node.children.push(ret);
    }
    node.children.push(block_node(&function.body));
    node
}

fn struct_node(strukt: &ast::StructItem) -> Node {
    let mut node = Node::new(format!("Struct {}", strukt.name.name));
    if let Some(generics) = &strukt.generics {
        node.children.push(generics_node(generics));
    }
    if !strukt.fields.is_empty() {
        let mut fields = Node::new("fields");
        fields.children = strukt.fields.iter().map(field_node).collect();
        node.children.push(fields);
    }
    node
}

fn enum_node(enm: &ast::EnumItem) -> Node {
    let mut node = Node::new(format!("Enum {}", enm.name.name));
    if let Some(generics) = &enm.generics {
        node.children.push(generics_node(generics));
    }
    if !enm.variants.is_empty() {
        let mut variants = Node::new("variants");
        variants.children = enm.variants.iter().map(enum_variant_node).collect();
        node.children.push(variants);
    }
    node
}

fn trait_node(trait_item: &ast::TraitItem) -> Node {
    let mut node = Node::new(format!("Trait {}", trait_item.name.name));
    if let Some(generics) = &trait_item.generics {
        node.children.push(generics_node(generics));
    }
    if !trait_item.super_traits.is_empty() {
        let mut supers = Node::new("super_traits");
<<<<<<< HEAD
        supers.children = trait_item.super_traits.iter().map(trait_bound_node).collect();
=======
        supers.children = trait_item
            .super_traits
            .iter()
            .map(trait_bound_node)
            .collect();
>>>>>>> cc823df (shift to LL3)
        node.children.push(supers);
    }
    if !trait_item.items.is_empty() {
        let mut items = Node::new("items");
        items.children = trait_item.items.iter().map(trait_item_node).collect();
        node.children.push(items);
    }
    node
}

fn impl_node(impl_item: &ast::ImplItem) -> Node {
    let mut node = Node::new("Impl");
    if let Some(generics) = &impl_item.generics {
        node.children.push(generics_node(generics));
    }
    if let Some(trait_ref) = &impl_item.trait_ref {
        node.children.push(trait_ref_node(trait_ref, "trait_ref"));
    }
    let mut self_type = Node::new("self_type");
    self_type.children.push(type_node(&impl_item.self_type));
    node.children.push(self_type);
    if !impl_item.items.is_empty() {
        let mut items = Node::new("items");
        items.children = impl_item.items.iter().map(impl_item_node).collect();
        node.children.push(items);
    }
    node
}

fn import_node(import: &ast::ImportItem) -> Node {
<<<<<<< HEAD
    let mut node = Node::new(format!(
        "Import {}",
        path_name(&import.path)
    ));
    if let Some(alias) = &import.alias {
        node.children.push(Node::new(format!("alias: {}", alias.name)));
=======
    let mut node = Node::new(format!("Import {}", path_name(&import.path)));
    if let Some(alias) = &import.alias {
        node.children
            .push(Node::new(format!("alias: {}", alias.name)));
>>>>>>> cc823df (shift to LL3)
    }
    if let Some(items) = &import.items {
        let mut imported = Node::new("items");
        imported.children = items
            .iter()
            .map(|item| {
                let label = if let Some(alias) = &item.alias {
                    format!("{} as {}", item.name.name, alias.name)
                } else {
                    item.name.name.clone()
                };
                Node::new(label)
            })
            .collect();
        node.children.push(imported);
    }
    node
}

fn extern_fn_node(extern_fn: &ast::ExternFunctionItem) -> Node {
    let mut node = Node::new(format!(
        "ExternFunction {} ({})",
        extern_fn.name.name,
        linkage_name(&extern_fn.linkage)
    ));
<<<<<<< HEAD
    node.children.push(function_signature_node(&extern_fn.signature));
=======
    node.children
        .push(function_signature_node(&extern_fn.signature));
    node
}

fn extern_var_node(extern_var: &ast::ExternVariableItem) -> Node {
    let mut node = Node::new(format!(
        "ExternVariable {} ({})",
        extern_var.name.name,
        linkage_name(&extern_var.linkage)
    ));
    let mut ty = Node::new("type");
    ty.children.push(type_node(&extern_var.var_type));
    node.children.push(ty);
>>>>>>> cc823df (shift to LL3)
    node
}

fn extern_block_node(block: &ast::ExternBlockItem) -> Node {
    let mut node = Node::new(format!("ExternBlock ({})", linkage_name(&block.linkage)));
    if !block.functions.is_empty() {
        let mut functions = Node::new("functions");
        functions.children = block.functions.iter().map(extern_fn_node).collect();
        node.children.push(functions);
    }
    node
}

fn function_signature_node(sig: &ast::FunctionSignature) -> Node {
    let mut node = Node::new(if sig.is_variadic {
        "signature (variadic)"
    } else {
        "signature"
    });
    if !sig.parameters.is_empty() {
        let mut params = Node::new("parameters");
        params.children = sig.parameters.iter().map(parameter_node).collect();
        node.children.push(params);
    }
    if let Some(return_type) = &sig.return_type {
        let mut ret = Node::new("return_type");
        ret.children.push(type_node(return_type));
        node.children.push(ret);
    }
    node
}

fn trait_item_node(item: &ast::TraitItemKind) -> Node {
    match item {
        ast::TraitItemKind::Function(function) => {
            let mut node = Node::new(format!("TraitFunction {}", function.name.name));
            if let Some(generics) = &function.generics {
                node.children.push(generics_node(generics));
            }
            if !function.parameters.is_empty() {
                let mut params = Node::new("parameters");
                params.children = function.parameters.iter().map(parameter_node).collect();
                node.children.push(params);
            }
            if let Some(return_type) = &function.return_type {
                let mut ret = Node::new("return_type");
                ret.children.push(type_node(return_type));
                node.children.push(ret);
            }
            if let Some(body) = &function.default_body {
                let mut default_body = block_node(body);
                default_body.label = "default_body".to_string();
                node.children.push(default_body);
            }
            node
        }
        ast::TraitItemKind::AssociatedType(assoc) => {
            let mut node = Node::new(format!("AssociatedType {}", assoc.name.name));
            if !assoc.bounds.is_empty() {
                let mut bounds = Node::new("bounds");
                bounds.children = assoc.bounds.iter().map(trait_bound_node).collect();
                node.children.push(bounds);
            }
            if let Some(default) = &assoc.default {
                let mut def = Node::new("default");
                def.children.push(type_node(default));
                node.children.push(def);
            }
            node
        }
    }
}

fn impl_item_node(item: &ast::ImplItemKind) -> Node {
    match item {
        ast::ImplItemKind::Function(function) => {
            let mut node = Node::new(format!(
                "ImplFunction {} ({}, {})",
                function.name.name,
                method_kind_name(&function.method_kind),
                visibility_name(&function.visibility)
            ));
            if let Some(generics) = &function.generics {
                node.children.push(generics_node(generics));
            }
            if !function.parameters.is_empty() {
                let mut params = Node::new("parameters");
                params.children = function.parameters.iter().map(parameter_node).collect();
                node.children.push(params);
            }
            if let Some(return_type) = &function.return_type {
                let mut ret = Node::new("return_type");
                ret.children.push(type_node(return_type));
                node.children.push(ret);
            }
            node.children.push(block_node(&function.body));
            node
        }
        ast::ImplItemKind::AssociatedType(assoc) => {
            let mut node = Node::new(format!("ImplAssociatedType {}", assoc.name.name));
            node.children.push(type_node(&assoc.type_def));
            node
        }
        ast::ImplItemKind::Cast(cast) => {
            let mut node = Node::new("ImplCast");
            let mut target = Node::new("target_type");
            target.children.push(type_node(&cast.target_type));
            node.children.push(target);
            if !cast.parameters.is_empty() {
                let mut params = Node::new("parameters");
                params.children = cast.parameters.iter().map(parameter_node).collect();
                node.children.push(params);
            }
            node.children.push(block_node(&cast.body));
            node
        }
    }
}

fn enum_variant_node(variant: &ast::EnumVariant) -> Node {
    let mut node = Node::new(format!("Variant {}", variant.name.name));
    match &variant.data {
        ast::EnumVariantData::Unit => {}
        ast::EnumVariantData::Tuple(items) => {
            let mut tuple = Node::new("tuple");
            tuple.children = items.iter().map(type_node).collect();
            node.children.push(tuple);
        }
        ast::EnumVariantData::Struct(fields) => {
            let mut strukt = Node::new("struct");
            strukt.children = fields.iter().map(field_node).collect();
            node.children.push(strukt);
        }
    }
    node
}

fn field_node(field: &ast::Field) -> Node {
    let mut node = Node::new(format!(
        "Field {} ({})",
        field.name.name,
        visibility_name(&field.visibility)
    ));
    node.children.push(type_node(&field.field_type));
    node
}

fn parameter_node(param: &ast::Parameter) -> Node {
    let mut node = Node::new(if param.is_mutable {
        format!("param mut {}", param.name.name)
    } else {
        format!("param {}", param.name.name)
    });
    node.children.push(type_node(&param.param_type));
    node
}

fn block_node(block: &ast::Block) -> Node {
    let mut node = Node::new(format!("Block [{}..{}]", block.span.start, block.span.end));
    node.children = block.statements.iter().map(statement_node).collect();
    node
}

fn statement_node(statement: &ast::Statement) -> Node {
    let mut node = match &statement.kind {
        ast::StatementKind::Block(block) => {
            let mut inner = block_node(block);
            inner.label = "Statement::Block".to_string();
            inner
        }
        ast::StatementKind::Expression(expression) => {
            let mut n = Node::new("Statement::Expression");
            n.children.push(expression_node(expression));
            n
        }
        ast::StatementKind::Let(let_stmt) => let_node(let_stmt),
        ast::StatementKind::Return(value) => {
            let mut n = Node::new("Statement::Return");
            if let Some(value) = value {
                n.children.push(expression_node(value));
            }
            n
        }
        ast::StatementKind::Break(value) => {
            let mut n = Node::new("Statement::Break");
            if let Some(value) = value {
                n.children.push(expression_node(value));
            }
            n
        }
        ast::StatementKind::Continue => Node::new("Statement::Continue"),
    };
    node.label = format!(
        "{} [{}..{}]",
        node.label, statement.span.start, statement.span.end
    );
    node
}

fn let_node(let_stmt: &ast::LetStatement) -> Node {
    let mut node = Node::new(if let_stmt.is_mutable {
        "Statement::Let (mut)"
    } else {
        "Statement::Let"
    });
    node.children.push(pattern_node(&let_stmt.pattern));
    if let Some(annotation) = &let_stmt.type_annotation {
        let mut ann = Node::new("type_annotation");
        ann.children.push(type_node(annotation));
        node.children.push(ann);
    }
    if let Some(initializer) = &let_stmt.initializer {
        let mut init = Node::new("initializer");
        init.children.push(expression_node(initializer));
        node.children.push(init);
    }
    node
}

fn pattern_node(pattern: &ast::Pattern) -> Node {
    match &pattern.kind {
<<<<<<< HEAD
        ast::PatternKind::Identifier(ident) => Node::new(format!("Pattern::Identifier {}", ident.name)),
=======
        ast::PatternKind::Identifier(ident) => {
            Node::new(format!("Pattern::Identifier {}", ident.name))
        }
>>>>>>> cc823df (shift to LL3)
        ast::PatternKind::Tuple(items) => {
            let mut node = Node::new("Pattern::Tuple");
            node.children = items.iter().map(pattern_node).collect();
            node
        }
        ast::PatternKind::Struct { path, fields } => {
            let mut node = Node::new(format!("Pattern::Struct {}", path_name(path)));
            node.children = fields.iter().map(field_pattern_node).collect();
            node
        }
        ast::PatternKind::Enum {
            path,
            variant,
            data,
        } => {
            let mut node = Node::new(format!(
                "Pattern::Enum {}::{}",
                path_name(path),
                variant.name
            ));
            if let Some(data) = data {
                node.children.push(pattern_node(data));
            }
            node
        }
<<<<<<< HEAD
        ast::PatternKind::Literal(literal) => Node::new(format!("Pattern::Literal {}", literal_name(literal))),
=======
        ast::PatternKind::Literal(literal) => {
            Node::new(format!("Pattern::Literal {}", literal_name(literal)))
        }
>>>>>>> cc823df (shift to LL3)
        ast::PatternKind::Wildcard => Node::new("Pattern::Wildcard"),
    }
}

fn field_pattern_node(field_pattern: &ast::FieldPattern) -> Node {
    let mut node = Node::new(format!("FieldPattern {}", field_pattern.name.name));
    if let Some(pattern) = &field_pattern.pattern {
        node.children.push(pattern_node(pattern));
    }
    node
}

fn expression_node(expression: &ast::Expression) -> Node {
    let mut node = match expression.kind.as_ref() {
        ast::ExpressionKind::Literal(literal) => {
            Node::new(format!("Expr::Literal {}", literal_name(literal)))
        }
<<<<<<< HEAD
        ast::ExpressionKind::Identifier(ident) => Node::new(format!("Expr::Identifier {}", ident.name)),
=======
        ast::ExpressionKind::Identifier(ident) => {
            Node::new(format!("Expr::Identifier {}", ident.name))
        }
>>>>>>> cc823df (shift to LL3)
        ast::ExpressionKind::TypeName(ty) => {
            let mut n = Node::new("Expr::TypeName");
            n.children.push(type_node(ty));
            n
        }
        ast::ExpressionKind::Binary {
            left,
            operator,
            right,
        } => {
            let mut n = Node::new(format!("Expr::Binary {:?}", operator));
            n.children.push(expression_node(left));
            n.children.push(expression_node(right));
            n
        }
        ast::ExpressionKind::Unary { operator, operand } => {
            let mut n = Node::new(format!("Expr::Unary {:?}", operator));
            n.children.push(expression_node(operand));
            n
        }
        ast::ExpressionKind::Postfix { operator, operand } => {
            let mut n = Node::new(format!("Expr::Postfix {:?}", operator));
            n.children.push(expression_node(operand));
            n
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            let mut n = Node::new("Expr::Call");
            n.children.push(expression_node(function));
            if !arguments.is_empty() {
                let mut args = Node::new("args");
                args.children = arguments.iter().map(expression_node).collect();
                n.children.push(args);
            }
            n
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            method,
            arguments,
        } => {
            let mut n = Node::new(format!("Expr::MethodCall {}", method.name));
            n.children.push(expression_node(receiver));
            if !arguments.is_empty() {
                let mut args = Node::new("args");
                args.children = arguments.iter().map(expression_node).collect();
                n.children.push(args);
            }
            n
        }
        ast::ExpressionKind::FieldAccess { object, field } => {
            let mut n = Node::new(format!("Expr::FieldAccess {}", field.name));
            n.children.push(expression_node(object));
            n
        }
        ast::ExpressionKind::Index { object, index } => {
            let mut n = Node::new("Expr::Index");
            n.children.push(expression_node(object));
            n.children.push(expression_node(index));
            n
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let mut n = Node::new("Expr::If");
            let mut cond = Node::new("condition");
            cond.children.push(expression_node(condition));
            n.children.push(cond);
            let mut then_branch_node = block_node(then_branch);
            then_branch_node.label = "then".to_string();
            n.children.push(then_branch_node);
            if let Some(else_branch) = else_branch {
                let mut else_node = block_node(else_branch);
                else_node.label = "else".to_string();
                n.children.push(else_node);
            }
            n
        }
        ast::ExpressionKind::While { condition, body } => {
            let mut n = Node::new("Expr::While");
            n.children.push(expression_node(condition));
            n.children.push(block_node(body));
            n
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            let mut n = Node::new("Expr::For");
            n.children.push(let_node(init));
            n.children.push(expression_node(condition));
            n.children.push(expression_node(increment));
            n.children.push(block_node(body));
            n
        }
        ast::ExpressionKind::Match { expression, arms } => {
            let mut n = Node::new("Expr::Match");
            n.children.push(expression_node(expression));
            if !arms.is_empty() {
                let mut arm_nodes = Node::new("arms");
                arm_nodes.children = arms.iter().map(match_arm_node).collect();
                n.children.push(arm_nodes);
            }
            n
        }
        ast::ExpressionKind::Block(block) => {
            let mut n = block_node(block);
            n.label = "Expr::Block".to_string();
            n
        }
        ast::ExpressionKind::Initializer { items } => {
            let mut n = Node::new("Expr::Initializer");
            n.children = items.iter().map(initializer_item_node).collect();
            n
        }
        ast::ExpressionKind::Asm(code) => Node::new(format!("Expr::Asm \"{}\"", code)),
        ast::ExpressionKind::Array(items) => {
            let mut n = Node::new("Expr::Array");
            n.children = items.iter().map(expression_node).collect();
            n
        }
        ast::ExpressionKind::Tuple(items) => {
            let mut n = Node::new("Expr::Tuple");
            n.children = items.iter().map(expression_node).collect();
            n
        }
        ast::ExpressionKind::StructLiteral { path, fields } => {
            let mut n = Node::new(format!("Expr::StructLiteral {}", path_name(path)));
            n.children = fields.iter().map(field_init_node).collect();
            n
        }
        ast::ExpressionKind::Cast {
            expression,
            target_type,
        } => {
            let mut n = Node::new("Expr::Cast");
            n.children.push(expression_node(expression));
            let mut target = Node::new("target");
            target.children.push(type_node(target_type));
            n.children.push(target);
            n
        }
        ast::ExpressionKind::Move(inner) => {
            let mut n = Node::new("Expr::Move");
            n.children.push(expression_node(inner));
            n
        }
        ast::ExpressionKind::Reference {
            is_mutable,
            expression,
        } => {
            let mut n = Node::new(if *is_mutable {
                "Expr::Reference mut"
            } else {
                "Expr::Reference"
            });
            n.children.push(expression_node(expression));
            n
        }
        ast::ExpressionKind::Comptime(inner) => {
            let mut n = Node::new("Expr::Comptime");
            n.children.push(expression_node(inner));
            n
        }
        ast::ExpressionKind::MacroCall { name, args } => {
            let mut n = Node::new(format!("Expr::MacroCall {}", name.name));
            n.children = args.iter().map(macro_arg_node).collect();
            n
        }
    };
    node.label = format!(
        "{} [{}..{}]",
        node.label, expression.span.start, expression.span.end
    );
    node
}

fn match_arm_node(arm: &ast::MatchArm) -> Node {
    let mut node = Node::new("arm");
    node.children.push(pattern_node(&arm.pattern));
    if let Some(guard) = &arm.guard {
        let mut guard_node = Node::new("guard");
        guard_node.children.push(expression_node(guard));
        node.children.push(guard_node);
    }
    node.children.push(expression_node(&arm.body));
    node
}

fn initializer_item_node(item: &ast::InitializerItem) -> Node {
    match item {
        ast::InitializerItem::Positional(value) => {
            let mut node = Node::new("Init::Positional");
            node.children.push(expression_node(value));
            node
        }
        ast::InitializerItem::Field { name, value } => {
            let mut node = Node::new(format!("Init::Field {}", name.name));
            node.children.push(expression_node(value));
            node
        }
        ast::InitializerItem::Index { index, value } => {
            let mut node = Node::new("Init::Index");
            node.children.push(expression_node(index));
            node.children.push(expression_node(value));
            node
        }
    }
}

fn field_init_node(init: &ast::FieldInit) -> Node {
    let mut node = Node::new(format!("FieldInit {}", init.name.name));
    node.children.push(expression_node(&init.value));
    node
}

fn macro_arg_node(arg: &ast::MacroArg) -> Node {
    match arg {
        ast::MacroArg::Expression(expression) => {
            let mut node = Node::new("MacroArg::Expression");
            node.children.push(expression_node(expression));
            node
        }
        ast::MacroArg::Type(ty) => {
            let mut node = Node::new("MacroArg::Type");
            node.children.push(type_node(ty));
            node
        }
        ast::MacroArg::Pattern(pattern) => {
            let mut node = Node::new("MacroArg::Pattern");
            node.children.push(pattern_node(pattern));
            node
        }
        ast::MacroArg::Statement(statement) => {
            let mut node = Node::new("MacroArg::Statement");
            node.children.push(statement_node(statement));
            node
        }
        ast::MacroArg::Item(item) => {
            let mut node = Node::new("MacroArg::Item");
            node.children.push(item_node(item));
            node
        }
<<<<<<< HEAD
        ast::MacroArg::Literal(literal) => Node::new(format!("MacroArg::Literal {}", literal_name(literal))),
        ast::MacroArg::Identifier(ident) => Node::new(format!("MacroArg::Identifier {}", ident.name)),
=======
        ast::MacroArg::Literal(literal) => {
            Node::new(format!("MacroArg::Literal {}", literal_name(literal)))
        }
        ast::MacroArg::Identifier(ident) => {
            Node::new(format!("MacroArg::Identifier {}", ident.name))
        }
>>>>>>> cc823df (shift to LL3)
    }
}

fn type_node(ty: &ast::Type) -> Node {
    let mut node = match ty.kind.as_ref() {
<<<<<<< HEAD
        ast::TypeKind::Primitive(primitive) => Node::new(format!("Type::Primitive {:?}", primitive)),
=======
        ast::TypeKind::Primitive(primitive) => {
            Node::new(format!("Type::Primitive {:?}", primitive))
        }
>>>>>>> cc823df (shift to LL3)
        ast::TypeKind::Named(named) => {
            let mut n = Node::new(format!("Type::Named {}", path_name(&named.path)));
            if let Some(generics) = &named.generics {
                let mut args = Node::new("generics");
                args.children = generics.iter().map(type_node).collect();
                n.children.push(args);
            }
            n
        }
        ast::TypeKind::Generic(generic) => {
            let mut n = Node::new(format!("Type::Generic {}", generic.name.name));
            if !generic.args.is_empty() {
                let mut args = Node::new("args");
                args.children = generic.args.iter().map(type_node).collect();
                n.children.push(args);
            }
            n
        }
        ast::TypeKind::Reference(reference) => {
            let mut n = Node::new(if reference.is_mutable {
                "Type::Reference mut"
            } else {
                "Type::Reference"
            });
            if let Some(lifetime) = &reference.lifetime {
<<<<<<< HEAD
                n.children.push(Node::new(format!("lifetime '{}'", lifetime.name)));
=======
                n.children
                    .push(Node::new(format!("lifetime '{}'", lifetime.name)));
>>>>>>> cc823df (shift to LL3)
            }
            n.children.push(type_node(&reference.inner));
            n
        }
        ast::TypeKind::Pointer(pointer) => {
            let mut n = Node::new(if pointer.is_mutable {
                "Type::Pointer mut"
            } else {
                "Type::Pointer"
            });
            n.children.push(type_node(&pointer.inner));
            n
        }
        ast::TypeKind::Array(array) => {
            let mut n = Node::new("Type::Array");
            n.children.push(type_node(&array.element_type));
            if let Some(size) = &array.size {
                let mut size_node = Node::new("size");
                size_node.children.push(expression_node(size));
                n.children.push(size_node);
            }
            n
        }
        ast::TypeKind::Optional(inner) => {
            let mut n = Node::new("Type::Optional");
            n.children.push(type_node(inner));
            n
        }
        ast::TypeKind::Function(function) => {
            let mut n = Node::new("Type::Function");
            if !function.parameters.is_empty() {
                let mut params = Node::new("parameters");
                params.children = function.parameters.iter().map(type_node).collect();
                n.children.push(params);
            }
            let mut ret = Node::new("return_type");
            ret.children.push(type_node(&function.return_type));
            n.children.push(ret);
            n
        }
        ast::TypeKind::Tuple(items) => {
            let mut n = Node::new("Type::Tuple");
            n.children = items.iter().map(type_node).collect();
            n
        }
    };
    node.label = format!("{} [{}..{}]", node.label, ty.span.start, ty.span.end);
    node
}

fn generics_node(generics: &ast::Generics) -> Node {
    let mut node = Node::new("generics");
    node.children = generics.params.iter().map(generic_param_node).collect();
    if let Some(where_clause) = &generics.where_clause {
        let mut where_node = Node::new("where");
        where_node.children = where_clause
            .predicates
            .iter()
            .map(where_predicate_node)
            .collect();
        node.children.push(where_node);
    }
    node
}

fn generic_param_node(param: &ast::GenericParam) -> Node {
    match param {
        ast::GenericParam::Type(type_param) => {
            let mut node = Node::new(format!("type {}", type_param.name.name));
            if !type_param.bounds.is_empty() {
                let mut bounds = Node::new("bounds");
                bounds.children = type_param.bounds.iter().map(trait_bound_node).collect();
                node.children.push(bounds);
            }
            if let Some(default) = &type_param.default {
                let mut def = Node::new("default");
                def.children.push(type_node(default));
                node.children.push(def);
            }
            node
        }
        ast::GenericParam::Lifetime(lifetime) => {
            let mut node = Node::new(format!("lifetime '{}'", lifetime.name.name));
            if !lifetime.bounds.is_empty() {
                let mut bounds = Node::new("bounds");
                bounds.children = lifetime
                    .bounds
                    .iter()
                    .map(|bound| Node::new(format!("'{}", bound.name)))
                    .collect();
                node.children.push(bounds);
            }
            node
        }
    }
}

fn where_predicate_node(predicate: &ast::WherePredicate) -> Node {
    match predicate {
        ast::WherePredicate::Type {
            bounded_type,
            bounds,
        } => {
            let mut node = Node::new("type_predicate");
            node.children.push(type_node(bounded_type));
            let mut bound_node = Node::new("bounds");
            bound_node.children = bounds.iter().map(trait_bound_node).collect();
            node.children.push(bound_node);
            node
        }
        ast::WherePredicate::Lifetime { lifetime, bounds } => {
            let mut node = Node::new(format!("lifetime_predicate '{}'", lifetime.name));
            if !bounds.is_empty() {
                let mut bound_node = Node::new("bounds");
                bound_node.children = bounds
                    .iter()
                    .map(|bound| Node::new(format!("'{}", bound.name)))
                    .collect();
                node.children.push(bound_node);
            }
            node
        }
    }
}

fn trait_bound_node(bound: &ast::TraitBound) -> Node {
    let prefix = if bound.is_optional { "?" } else { "" };
    trait_ref_node(&bound.trait_ref, format!("bound {prefix}").as_str())
}

fn trait_ref_node(trait_ref: &ast::TraitRef, label_prefix: &str) -> Node {
    let mut node = Node::new(format!("{label_prefix}{}", path_name(&trait_ref.path)));
    if let Some(generics) = &trait_ref.generics {
        let mut args = Node::new("generics");
        args.children = generics.iter().map(type_node).collect();
        node.children.push(args);
    }
    node
}

fn attribute_node(attribute: &ast::Attribute) -> Node {
    let mut node = Node::new(format!("#[{}]", attribute.name.name));
    node.children = attribute.args.iter().map(attribute_arg_node).collect();
    node
}

fn attribute_arg_node(arg: &ast::AttributeArg) -> Node {
    match arg {
        ast::AttributeArg::Identifier(ident) => Node::new(format!("ident {}", ident.name)),
<<<<<<< HEAD
        ast::AttributeArg::Literal(literal) => Node::new(format!("literal {}", literal_name(literal))),
=======
        ast::AttributeArg::Literal(literal) => {
            Node::new(format!("literal {}", literal_name(literal)))
        }
>>>>>>> cc823df (shift to LL3)
    }
}

fn literal_name(literal: &ast::Literal) -> String {
    match literal {
        ast::Literal::Integer(value) => value.to_string(),
        ast::Literal::Float(value) => value.to_string(),
        ast::Literal::Complex(real, imag) => format!("{real}+{imag}i"),
        ast::Literal::String(value) => format!("{value:?}"),
        ast::Literal::Char(value) => format!("{value:?}"),
        ast::Literal::Bool(value) => value.to_string(),
    }
}

fn path_name(path: &[ast::Identifier]) -> String {
    path.iter()
        .map(|segment| segment.name.as_str())
        .collect::<Vec<_>>()
        .join("::")
}

fn visibility_name(visibility: &ast::Visibility) -> &'static str {
    match visibility {
        ast::Visibility::Public => "pub",
        ast::Visibility::Private => "priv",
    }
}

fn linkage_name(linkage: &ast::ExternLinkage) -> &'static str {
    match linkage {
        ast::ExternLinkage::C => "C",
        ast::ExternLinkage::Silver => "Silver",
        ast::ExternLinkage::System => "system",
        ast::ExternLinkage::Rust => "Rust",
        ast::ExternLinkage::Cdecl => "cdecl",
        ast::ExternLinkage::Stdcall => "stdcall",
        ast::ExternLinkage::Fastcall => "fastcall",
    }
}

fn method_kind_name(method_kind: &ast::MethodKind) -> &'static str {
    match method_kind {
        ast::MethodKind::Static => "static",
        ast::MethodKind::InstanceValue => "value",
        ast::MethodKind::InstancePointer { is_mutable } => {
            if *is_mutable {
                "ptr mut"
            } else {
                "ptr"
            }
        }
    }
}
