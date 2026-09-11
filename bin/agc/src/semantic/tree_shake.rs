//! AST-level Dead Code Elimination (Tree-Shaking).
//!
//! Analyzes reachability starting from program entry points (`main`, runtime hooks,
//! user source file items, volatile/export attributes) and prunes unreferenced
//! standalone functions and method bodies inlined from imported modules before
//! monomorphization and LLVM codegen.

use rustc_hash::FxHashSet as HashSet;
use crate::parser::ast::*;
use crate::semantic::monomorph::MonomorphRequest;

/// Prune unreferenced standalone functions and impl methods from `program`.
///
/// Returns the number of items/methods eliminated.
pub fn eliminate_dead_ast_items(
    program: &mut Program,
    root_file_id: Option<u32>,
    is_module: bool,
    monomorphs: &[MonomorphRequest],
) -> usize {
    let mut collector = NameCollector::new();

    // 1. Mark roots and seed initial referenced names.
    let mut live_functions: HashSet<String> = HashSet::default();

    for req in monomorphs {
        match req {
            MonomorphRequest::Function { source, .. } => {
                live_functions.insert(source.name.name.clone());
                collector.walk_function_item(source);
            }
            MonomorphRequest::ImplMethod {
                impl_item,
                method,
                ..
            } => {
                collector.names.insert(method.name.name.clone());
                if let Some(owner) = base_type_name(&impl_item.self_type) {
                    collector.names.insert(owner);
                }
                collector.walk_impl_function(method);
            }
        }
    }

    for item in &program.items {
        if is_root_item(item, root_file_id, is_module) {
            collector.walk_item(item);
            if let ItemKind::Function(f) = &item.kind {
                live_functions.insert(f.name.name.clone());
            }
        } else if let ItemKind::GlobalVariable(_) = &item.kind {
            collector.walk_item(item);
        }
    }

    // 2. Iterative reachability fixpoint.
    loop {
        let prev_names_count = collector.names.len();
        let prev_funcs_count = live_functions.len();

        for item in &program.items {
            match &item.kind {
                ItemKind::Function(f) => {
                    if !live_functions.contains(&f.name.name) && collector.names.contains(&f.name.name) {
                        live_functions.insert(f.name.name.clone());
                        collector.walk_function_item(f);
                    }
                }
                ItemKind::Impl(imp) => {
                    let owner = base_type_name(&imp.self_type);
                    let owner_live = owner.as_ref().map_or(false, |o| collector.names.contains(o))
                        || root_file_id.map_or(false, |id| item.span.file == id);

                    if owner_live {
                        let is_generic = is_generic_impl(imp);
                        for member in &imp.items {
                            if let ImplItemKind::Function(f) = member {
                                let is_always_live = is_generic
                                    || f.name.name == "drop"
                                    || f.name.name.starts_with("__")
                                    || imp.trait_ref.is_some()
                                    || has_keep_attribute(&f.attributes)
                                    || root_file_id.map_or(false, |id| f.name.span.file == id);

                                if is_always_live || collector.names.contains(&f.name.name) {
                                    collector.walk_impl_function(f);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        if collector.names.len() == prev_names_count && live_functions.len() == prev_funcs_count {
            break;
        }
    }

    // 3. Prune unreferenced items.
    let mut pruned_count = 0usize;
    let mut retained_items = Vec::with_capacity(program.items.len());

    for mut item in program.items.drain(..) {
        let is_root = is_root_item(&item, root_file_id, is_module);
        match &mut item.kind {
            ItemKind::Function(f) => {
                if is_root || live_functions.contains(&f.name.name) {
                    retained_items.push(item);
                } else {
                    pruned_count += 1;
                }
            }
            ItemKind::Impl(imp) => {
                let owner = base_type_name(&imp.self_type);
                let owner_live = owner.as_ref().map_or(false, |o| collector.names.contains(o))
                    || root_file_id.map_or(false, |id| item.span.file == id);

                if !owner_live && !root_file_id.map_or(false, |id| item.span.file == id) {
                    pruned_count += imp.items.len().max(1);
                    continue;
                }

                if is_generic_impl(imp) {
                    retained_items.push(item);
                } else {
                    let original_count = imp.items.len();
                    imp.items.retain(|member| {
                        match member {
                            ImplItemKind::Function(f) => {
                                let keep = f.name.name == "drop"
                                    || f.name.name.starts_with("__")
                                    || imp.trait_ref.is_some()
                                    || has_keep_attribute(&f.attributes)
                                    || root_file_id.map_or(false, |id| f.name.span.file == id)
                                    || collector.names.contains(&f.name.name);
                                if !keep {
                                    pruned_count += 1;
                                }
                                keep
                            }
                            _ => true,
                        }
                    });

                    if !imp.items.is_empty() || imp.trait_ref.is_some() || original_count == 0 {
                        retained_items.push(item);
                    } else {
                        pruned_count += 1;
                    }
                }
            }
            _ => {
                // Structs, Enums, Traits, TypeAliases, Globals, Externs are preserved.
                retained_items.push(item);
            }
        }
    }

    program.items = retained_items;
    pruned_count
}

fn has_keep_attribute(attributes: &[Attribute]) -> bool {
    attributes.iter().any(|attr| {
        attr.name.name == "volatile"
            || attr.name.name == "export"
            || attr.name.name == "test"
            || attr.name.name == "used"
    })
}

fn is_root_item(item: &Item, root_file_id: Option<u32>, is_module: bool) -> bool {
    if root_file_id.map_or(false, |id| item.span.file == id) {
        return true;
    }
    if has_keep_attribute(&item.attributes) {
        return true;
    }
    if is_module && item.visibility == Visibility::Public {
        return true;
    }

    match &item.kind {
        ItemKind::Function(f) => is_root_function_name(&f.name.name),
        ItemKind::GlobalVariable(g) => is_root_function_name(&g.name.name),
        ItemKind::ExternFunction(_) | ItemKind::ExternVariable(_) | ItemKind::ExternBlock(_) => true,
        _ => false,
    }
}


fn is_root_function_name(name: &str) -> bool {
    name == "main"
        || name == "_start"
        || name.starts_with("__")
        || matches!(
            name,
            "memset"
                | "memcpy"
                | "memmove"
                | "strlen"
                | "strcmp"
                | "abort"
                | "malloc"
                | "calloc"
                | "free"
                | "realloc"
                | "realloc_bytes"
                | "hash_bytes"
                | "hash_str"
                | "mem_alloc_raw_impl"
        )
}

pub fn is_generic_impl(imp: &ImplItem) -> bool {
    if imp.generics.is_some() {
        return true;
    }
    is_generic_type(&imp.self_type)
}

pub fn is_generic_type(ty: &Type) -> bool {
    match ty.kind.as_ref() {
        TypeKind::Named(n) => n.generics.is_some(),
        TypeKind::Generic(_) => true,
        TypeKind::Optional(_) => true,
        TypeKind::Pointer(p) => is_generic_type(&p.inner),
        TypeKind::Reference(r) => is_generic_type(&r.inner),
        _ => false,
    }
}

pub fn base_type_name(ty: &Type) -> Option<String> {
    match ty.kind.as_ref() {
        TypeKind::Named(n) => n.path.last().map(|id| id.name.clone()),
        TypeKind::Generic(g) => Some(g.name.name.clone()),
        TypeKind::Pointer(p) => base_type_name(&p.inner),
        TypeKind::Reference(r) => base_type_name(&r.inner),
        TypeKind::Optional(_) => Some("Optional".to_string()),
        TypeKind::Primitive(p) => Some(format!("{p:?}")),
        _ => None,
    }
}

struct NameCollector {
    pub names: HashSet<String>,
}

impl NameCollector {
    pub fn new() -> Self {
        let mut names = HashSet::default();
        names.insert("main".to_string());
        names.insert("_start".to_string());
        names.insert("drop".to_string());
        Self { names }
    }

    pub fn walk_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Function(f) => self.walk_function_item(f),
            ItemKind::GlobalVariable(g) => {
                self.walk_type(&g.var_type);
                if let Some(init) = &g.initializer {
                    self.walk_expr(init);
                }
            }
            ItemKind::Struct(s) => {
                self.names.insert(s.name.name.clone());
                for f in &s.fields {
                    self.walk_type(&f.field_type);
                }
            }
            ItemKind::Enum(e) => {
                self.names.insert(e.name.name.clone());
                for v in &e.variants {
                    self.names.insert(v.name.name.clone());
                    match &v.data {
                        EnumVariantData::Unit => {}
                        EnumVariantData::Tuple(types) => {
                            for t in types {
                                self.walk_type(t);
                            }
                        }
                        EnumVariantData::Struct(fields) => {
                            for f in fields {
                                self.walk_type(&f.field_type);
                            }
                        }
                    }
                }
            }
            ItemKind::Impl(imp) => {
                self.walk_type(&imp.self_type);
                if let Some(tr) = &imp.trait_ref {
                    self.walk_trait_ref(tr);
                }
                for member in &imp.items {
                    if let ImplItemKind::Function(f) = member {
                        self.walk_impl_function(f);
                    }
                }
            }
            ItemKind::Trait(t) => {
                self.names.insert(t.name.name.clone());
            }
            ItemKind::TypeAlias(a) => {
                self.names.insert(a.name.name.clone());
                self.walk_type(&a.type_def);
            }
            _ => {}
        }
    }

    pub fn walk_function_item(&mut self, func: &FunctionItem) {
        self.names.insert(func.name.name.clone());
        for p in &func.parameters {
            self.walk_type(&p.param_type);
        }
        if let Some(ret) = &func.return_type {
            self.walk_type(ret);
        }
        self.walk_block(&func.body);
    }

    pub fn walk_impl_function(&mut self, func: &ImplFunction) {
        self.names.insert(func.name.name.clone());
        for p in &func.parameters {
            self.walk_type(&p.param_type);
        }
        if let Some(ret) = &func.return_type {
            self.walk_type(ret);
        }
        self.walk_block(&func.body);
    }

    pub fn walk_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.walk_stmt(stmt);
        }
    }

    pub fn walk_stmt(&mut self, stmt: &Statement) {
        match &stmt.kind {
            StatementKind::Block(b) => self.walk_block(b),
            StatementKind::Expression(e) => self.walk_expr(e),
            StatementKind::Let(ls) => {
                if let Some(ty) = &ls.type_annotation {
                    self.walk_type(ty);
                }
                if let Some(init) = &ls.initializer {
                    self.walk_expr(init);
                }
                self.walk_pattern(&ls.pattern);
            }
            StatementKind::Return(Some(e)) | StatementKind::Break(Some(e)) => {
                self.walk_expr(e);
            }
            StatementKind::Return(None) | StatementKind::Break(None) | StatementKind::Continue => {}
            StatementKind::Defer(s) => self.walk_stmt(s),
        }
    }

    pub fn walk_pattern(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Identifier(id) | PatternKind::Move(id) => {
                self.names.insert(id.name.clone());
            }
            PatternKind::Tuple(pats) => {
                for p in pats {
                    self.walk_pattern(p);
                }
            }
            PatternKind::Struct { path, fields } => {
                for id in path {
                    self.names.insert(id.name.clone());
                }
                for f in fields {
                    if let Some(p) = &f.pattern {
                        self.walk_pattern(p);
                    }
                }
            }
            PatternKind::Enum { path, variant, data } => {
                for id in path {
                    self.names.insert(id.name.clone());
                }
                self.names.insert(variant.name.clone());
                if let Some(p) = data {
                    self.walk_pattern(p);
                }
            }
            PatternKind::Range { start, end, .. } => {
                self.walk_expr(start);
                self.walk_expr(end);
            }
            PatternKind::Literal(_) | PatternKind::Wildcard => {}
        }
    }

    pub fn walk_expr(&mut self, expr: &Expression) {
        match expr.kind.as_ref() {
            ExpressionKind::Identifier(id) => {
                self.names.insert(id.name.clone());
            }
            ExpressionKind::TypeName(t) => {
                self.walk_type(t);
            }
            ExpressionKind::Binary { left, operator, right } => {
                self.walk_expr(left);
                self.walk_expr(right);
                match operator {
                    BinaryOperator::Add => { self.names.insert("__add".to_string()); }
                    BinaryOperator::Subtract => { self.names.insert("__sub".to_string()); }
                    BinaryOperator::Multiply => { self.names.insert("__mul".to_string()); }
                    BinaryOperator::Divide => { self.names.insert("__div".to_string()); }
                    BinaryOperator::Modulo => { self.names.insert("__rem".to_string()); }
                    BinaryOperator::Equal => { self.names.insert("__eq".to_string()); }
                    BinaryOperator::NotEqual => { self.names.insert("__ne".to_string()); }
                    BinaryOperator::Less => { self.names.insert("__lt".to_string()); }
                    BinaryOperator::LessEqual => { self.names.insert("__le".to_string()); }
                    BinaryOperator::Greater => { self.names.insert("__gt".to_string()); }
                    BinaryOperator::GreaterEqual => { self.names.insert("__ge".to_string()); }
                    _ => {}
                }
            }
            ExpressionKind::Unary { operator, operand }
            | ExpressionKind::Postfix { operator, operand } => {
                self.walk_expr(operand);
                match operator {
                    UnaryOperator::Minus => { self.names.insert("__neg".to_string()); }
                    UnaryOperator::Not => { self.names.insert("__not".to_string()); }
                    UnaryOperator::BitwiseNot => { self.names.insert("__bit_not".to_string()); }
                    _ => {}
                }
            }
            ExpressionKind::Call { function, arguments } => {
                self.walk_expr(function);
                for arg in arguments {
                    self.walk_expr(arg);
                }
            }
            ExpressionKind::MethodCall { receiver, method, arguments } => {
                self.walk_expr(receiver);
                self.names.insert(method.name.clone());
                for arg in arguments {
                    self.walk_expr(arg);
                }
            }
            ExpressionKind::FieldAccess { object, field } => {
                self.walk_expr(object);
                self.names.insert(field.name.clone());
            }
            ExpressionKind::Index { object, index } => {
                self.walk_expr(object);
                self.walk_expr(index);
                self.names.insert("__index_get".to_string());
                self.names.insert("__index_set".to_string());
            }
            ExpressionKind::Slice { object, start, end, step } => {
                self.walk_expr(object);
                if let Some(s) = start { self.walk_expr(s); }
                if let Some(e) = end { self.walk_expr(e); }
                if let Some(st) = step { self.walk_expr(st); }
                self.names.insert("__slice".to_string());
            }
            ExpressionKind::If { condition, then_branch, else_branch } => {
                self.walk_expr(condition);
                self.walk_block(then_branch);
                if let Some(eb) = else_branch {
                    self.walk_block(eb);
                }
            }
            ExpressionKind::Ternary { condition, then_expr, else_expr } => {
                self.walk_expr(condition);
                self.walk_expr(then_expr);
                self.walk_expr(else_expr);
            }
            ExpressionKind::UnwrapOr { value, fallback } => {
                self.walk_expr(value);
                self.walk_expr(fallback);
            }
            ExpressionKind::While { condition, body } => {
                self.walk_expr(condition);
                self.walk_block(body);
            }
            ExpressionKind::ForIn { iterable, body, iterator_type, item_type, .. } => {
                self.walk_expr(iterable);
                self.walk_block(body);
                if let Some(it) = iterator_type { self.walk_type(it); }
                if let Some(it) = item_type { self.walk_type(it); }
                self.names.insert("__next".to_string());
                self.names.insert("next".to_string());
            }
            ExpressionKind::For { init, condition, increment, body } => {
                if let Some(ty) = &init.type_annotation { self.walk_type(ty); }
                if let Some(ini) = &init.initializer { self.walk_expr(ini); }
                self.walk_expr(condition);
                self.walk_expr(increment);
                self.walk_block(body);
            }
            ExpressionKind::Match { expression, arms } => {
                self.walk_expr(expression);
                for arm in arms {
                    self.walk_pattern(&arm.pattern);
                    if let Some(g) = &arm.guard { self.walk_expr(g); }
                    self.walk_expr(&arm.body);
                }
            }
            ExpressionKind::Block(b) => self.walk_block(b),
            ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        InitializerItem::Positional(e) | InitializerItem::Field { value: e, .. } => {
                            self.walk_expr(e);
                        }
                        InitializerItem::Index { index, value } => {
                            self.walk_expr(index);
                            self.walk_expr(value);
                        }
                    }
                }
            }
            ExpressionKind::Array(exprs) | ExpressionKind::Tuple(exprs) => {
                for e in exprs {
                    self.walk_expr(e);
                }
            }
            ExpressionKind::StructLiteral { path, fields } => {
                for id in path {
                    self.names.insert(id.name.clone());
                }
                for f in fields {
                    self.names.insert(f.name.name.clone());
                    self.walk_expr(&f.value);
                }
            }
            ExpressionKind::Cast { expression, target_type } => {
                self.walk_expr(expression);
                self.walk_type(target_type);
            }
            ExpressionKind::Move(e)
            | ExpressionKind::Reference { expression: e, .. }
            | ExpressionKind::Launch(e)
            | ExpressionKind::Wait(e)
            | ExpressionKind::Comptime(e) => {
                self.walk_expr(e);
            }
            ExpressionKind::EnumVariant { path, variant, fields } => {
                for id in path {
                    self.names.insert(id.name.clone());
                }
                self.names.insert(variant.name.clone());
                for f in fields {
                    self.walk_expr(f);
                }
            }
            ExpressionKind::MacroCall { name, args } => {
                self.names.insert(name.name.clone());
                if matches!(name.name.as_str(), "print" | "println" | "eprint" | "eprintln" | "format") {
                    self.names.insert("write_str".to_string());
                    self.names.insert("write_i64".to_string());
                    self.names.insert("write_u64".to_string());
                    self.names.insert("write_f64".to_string());
                    self.names.insert("write_bool".to_string());
                    self.names.insert("write_u8".to_string());
                    self.names.insert("write_i128".to_string());
                    self.names.insert("write_u128".to_string());
                    self.names.insert("flush".to_string());
                    self.names.insert("Stdout".to_string());
                    self.names.insert("Stderr".to_string());
                    self.names.insert("BufWriter".to_string());
                    self.names.insert("stdout".to_string());
                    self.names.insert("stderr".to_string());
                    self.names.insert("STDOUT".to_string());
                    self.names.insert("STDERR".to_string());
                    self.names.insert("__promote_in_memory".to_string());
                } else if name.name == "vec" {
                    self.names.insert("Vec".to_string());
                    self.names.insert("new".to_string());
                    self.names.insert("push".to_string());
                }
                for arg in args {
                    match arg {
                        MacroArg::Expression(e) => self.walk_expr(e),
                        _ => {}
                    }
                }
            }
            ExpressionKind::Asm { inputs, .. } => {
                for inp in inputs {
                    self.walk_expr(inp);
                }
            }
            ExpressionKind::Literal(_) => {}
        }
    }

    pub fn walk_type(&mut self, ty: &Type) {
        match ty.kind.as_ref() {
            TypeKind::Named(n) => {
                for id in &n.path {
                    self.names.insert(id.name.clone());
                }
                if let Some(generics) = &n.generics {
                    for g in generics {
                        self.walk_type(g);
                    }
                }
            }
            TypeKind::Generic(g) => {
                self.names.insert(g.name.name.clone());
                for arg in &g.args {
                    self.walk_type(arg);
                }
            }
            TypeKind::Pointer(p) => self.walk_type(&p.inner),
            TypeKind::Reference(r) => self.walk_type(&r.inner),
            TypeKind::Optional(o) => {
                self.names.insert("Optional".to_string());
                self.walk_type(o);
            }
            TypeKind::Slice(s) => self.walk_type(&s.element_type),
            TypeKind::Array(a) => self.walk_type(&a.element_type),
            TypeKind::Tuple(types) => {
                for t in types {
                    self.walk_type(t);
                }
            }
            TypeKind::Function(f) => {
                for p in &f.parameters {
                    self.walk_type(p);
                }
                self.walk_type(&f.return_type);
            }
            TypeKind::Primitive(_) => {}
        }
    }

    pub fn walk_trait_ref(&mut self, tr: &TraitRef) {
        for id in &tr.path {
            self.names.insert(id.name.clone());
        }
        if let Some(args) = &tr.generics {
            for arg in args {
                self.walk_type(arg);
            }
        }
    }
}
