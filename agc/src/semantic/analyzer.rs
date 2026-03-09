use std::collections::{HashMap, HashSet};

use crate::lexer::Span;
use crate::module_artifact::{ExportKind, ModuleArtifact};
use crate::parser::ast;
use crate::symbol_table::{BindVarResult, CompilerPhase, CompilerSymbolTable, VarRecord};

pub trait SemanticAnalyzerHook {
    fn before_analysis(&mut self, _program: &ast::Program) {}
    fn on_item_analyzed(&mut self, _item: &ast::Item) {}
    fn after_analysis(&mut self, _program: &mut ast::Program, _errors: &[SemanticError]) {}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticError {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolKind {
    Struct,
    Enum,
    Trait,
    Function,
    GlobalVariable,
    ExternVariable,
}

pub struct Analyzer {
    errors: Vec<SemanticError>,
    symbols: HashMap<String, SymbolKind>,
    type_params: Vec<HashSet<String>>,
    table_backend: CompilerSymbolTable,
    imported_symbols: HashMap<String, SymbolKind>,
    imported_types: HashSet<String>,
    imported_traits: HashSet<String>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            symbols: HashMap::new(),
            type_params: Vec::new(),
            table_backend: CompilerSymbolTable::new(),
            imported_symbols: HashMap::new(),
            imported_types: HashSet::new(),
            imported_traits: HashSet::new(),
        }
    }

    pub fn inject_imported_modules(&mut self, modules: &[ModuleArtifact]) {
        for module in modules {
            for export in &module.exports {
                match export.kind {
                    ExportKind::Function => {
                        self.imported_symbols
                            .insert(export.name.clone(), SymbolKind::Function);
                    }
                    ExportKind::Struct => {
                        self.imported_symbols
                            .insert(export.name.clone(), SymbolKind::Struct);
                        self.imported_types.insert(export.name.clone());
                    }
                    ExportKind::Enum => {
                        self.imported_symbols
                            .insert(export.name.clone(), SymbolKind::Enum);
                        self.imported_types.insert(export.name.clone());
                    }
                    ExportKind::Trait => {
                        self.imported_symbols
                            .insert(export.name.clone(), SymbolKind::Trait);
                        self.imported_traits.insert(export.name.clone());
                    }
                }
            }
        }
    }

    pub fn analyze_program(&mut self, program: &ast::Program) -> Vec<SemanticError> {
        let mut table = CompilerSymbolTable::new();
        self.analyze_program_with_table(program, &mut table)
    }

    pub fn analyze_program_with_table(
        &mut self,
        program: &ast::Program,
        table: &mut CompilerSymbolTable,
    ) -> Vec<SemanticError> {
        self.table_backend = CompilerSymbolTable::new();
        self.table_backend
            .touch_phase(CompilerPhase::SemanticAnalyze, "semantic analysis start");
        self.collect_symbols(program);
        self.table_backend
            .record_program_symbols(program, CompilerPhase::SemanticAnalyze);
        for item in &program.items {
            self.check_item(item);
        }
        self.table_backend.touch_phase(
            CompilerPhase::SemanticAnalyze,
            format!("semantic analysis done (errors={})", self.errors.len()),
        );
        table.absorb_from(&self.table_backend);
        self.errors.clone()
    }

    pub fn analyze_program_with_hooks(
        &mut self,
        program: &mut ast::Program,
        hooks: &mut [&mut dyn SemanticAnalyzerHook],
    ) -> Vec<SemanticError> {
        let mut table = CompilerSymbolTable::new();
        self.analyze_program_with_hooks_and_table(program, hooks, &mut table)
    }

    pub fn analyze_program_with_hooks_and_table(
        &mut self,
        program: &mut ast::Program,
        hooks: &mut [&mut dyn SemanticAnalyzerHook],
        table: &mut CompilerSymbolTable,
    ) -> Vec<SemanticError> {
        self.table_backend = CompilerSymbolTable::new();
        for hook in hooks.iter_mut() {
            hook.before_analysis(program);
        }

        self.table_backend.touch_phase(
            CompilerPhase::SemanticAnalyze,
            "semantic analysis with hooks start",
        );
        self.collect_symbols(program);
        self.table_backend
            .record_program_symbols(program, CompilerPhase::SemanticAnalyze);
        for item in &program.items {
            self.check_item(item);
            for hook in hooks.iter_mut() {
                hook.on_item_analyzed(item);
            }
        }

        let errors = self.errors.clone();
        for hook in hooks.iter_mut() {
            hook.after_analysis(program, &errors);
        }

        self.table_backend.touch_phase(
            CompilerPhase::SemanticAnalyze,
            format!(
                "semantic analysis with hooks done (errors={})",
                errors.len()
            ),
        );
        table.absorb_from(&self.table_backend);
        errors
    }

    pub fn var_records(&self) -> &[VarRecord] {
        self.table_backend.var_records()
    }

    fn collect_symbols(&mut self, program: &ast::Program) {
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Struct(struct_item) => {
                    self.insert_symbol(&struct_item.name, SymbolKind::Struct);
                }
                ast::ItemKind::Enum(enum_item) => {
                    self.insert_symbol(&enum_item.name, SymbolKind::Enum);
                }
                ast::ItemKind::Trait(trait_item) => {
                    self.insert_symbol(&trait_item.name, SymbolKind::Trait);
                }
                ast::ItemKind::Function(func) => {
                    self.insert_symbol(&func.name, SymbolKind::Function);
                }
                ast::ItemKind::GlobalVariable(var) => {
                    self.insert_symbol(&var.name, SymbolKind::GlobalVariable);
                }
                ast::ItemKind::ExternFunction(func) => {
                    self.insert_symbol(&func.name, SymbolKind::Function);
                }
                ast::ItemKind::ExternVariable(var) => {
                    self.insert_symbol(&var.name, SymbolKind::ExternVariable);
                }
                ast::ItemKind::ExternBlock(block) => {
                    for func in &block.functions {
                        self.insert_symbol(&func.name, SymbolKind::Function);
                    }
                }
                _ => {}
            }
        }
    }

    fn insert_symbol(&mut self, ident: &ast::Identifier, kind: SymbolKind) {
        if let Some(existing) = self.symbols.get(&ident.name) {
            if matches!(existing, SymbolKind::Function) && matches!(kind, SymbolKind::Function) {
                return;
            }
            let message = match existing {
                SymbolKind::Struct => format!("duplicate struct '{}'", ident.name),
                SymbolKind::Enum => format!("duplicate enum '{}'", ident.name),
                SymbolKind::Trait => format!("duplicate trait '{}'", ident.name),
                SymbolKind::Function => format!("duplicate function '{}'", ident.name),
                SymbolKind::GlobalVariable => format!("duplicate global variable '{}'", ident.name),
                SymbolKind::ExternVariable => {
                    format!("duplicate extern variable '{}'", ident.name)
                }
            };
            self.errors.push(SemanticError {
                message,
                span: ident.span.clone(),
            });
            return;
        }
        self.symbols.insert(ident.name.clone(), kind);
    }

    fn has_symbol(&self, name: &str) -> Option<SymbolKind> {
        self.symbols
            .get(name)
            .copied()
            .or_else(|| self.imported_symbols.get(name).copied())
    }

    fn check_item(&mut self, item: &ast::Item) {
        match &item.kind {
            ast::ItemKind::Struct(struct_item) => {
                self.push_type_params(struct_item.generics.as_ref());
                self.check_generics_bounds(struct_item.generics.as_ref());
                for field in &struct_item.fields {
                    self.check_type(&field.field_type);
                }
                self.pop_type_params();
            }
            ast::ItemKind::Enum(enum_item) => {
                self.push_type_params(enum_item.generics.as_ref());
                self.check_generics_bounds(enum_item.generics.as_ref());
                for variant in &enum_item.variants {
                    match &variant.data {
                        ast::EnumVariantData::Unit => {}
                        ast::EnumVariantData::Tuple(items) => {
                            for ty in items {
                                self.check_type(ty);
                            }
                        }
                        ast::EnumVariantData::Struct(fields) => {
                            for field in fields {
                                self.check_type(&field.field_type);
                            }
                        }
                    }
                }
                self.pop_type_params();
            }
            ast::ItemKind::Trait(trait_item) => {
                self.push_type_params(trait_item.generics.as_ref());
                self.check_generics_bounds(trait_item.generics.as_ref());
                for item in &trait_item.items {
                    match item {
                        ast::TraitItemKind::Function(func) => {
                            self.push_type_params(func.generics.as_ref());
                            self.check_generics_bounds(func.generics.as_ref());
                            for param in &func.parameters {
                                self.check_type(&param.param_type);
                            }
                            if let Some(return_type) = &func.return_type {
                                self.check_type(return_type);
                            }
                            self.pop_type_params();
                        }
                        ast::TraitItemKind::AssociatedType(assoc) => {
                            self.check_trait_bounds(&assoc.bounds);
                            if let Some(default) = &assoc.default {
                                self.check_type(default);
                            }
                        }
                    }
                }
                self.pop_type_params();
            }
            ast::ItemKind::Function(func) => {
                self.push_type_params(func.generics.as_ref());
                self.check_generics_bounds(func.generics.as_ref());
                self.push_var_scope();
                for param in &func.parameters {
                    self.check_type(&param.param_type);
                    self.bind_var(&param.name, param.span.clone());
                }
                if let Some(return_type) = &func.return_type {
                    self.check_type(return_type);
                }
                self.check_block(&func.body);
                self.pop_var_scope();
                self.pop_type_params();
            }
            ast::ItemKind::GlobalVariable(var) => {
                self.check_type(&var.var_type);
                if let Some(initializer) = &var.initializer {
                    self.check_expression(initializer);
                }
            }
            ast::ItemKind::Impl(impl_item) => {
                let mut implicit = HashSet::new();
                if impl_item.generics.is_none() {
                    self.collect_implicit_type_params(&impl_item.self_type, &mut implicit);
                }
                self.push_type_params(impl_item.generics.as_ref());
                if !implicit.is_empty() {
                    if let Some(scope) = self.type_params.last_mut() {
                        scope.extend(implicit);
                    }
                }
                if let Some(scope) = self.type_params.last_mut() {
                    scope.insert("Self".to_string());
                }
                self.check_generics_bounds(impl_item.generics.as_ref());
                if let Some(trait_ref) = &impl_item.trait_ref {
                    if trait_ref.path.len() == 1 {
                        let name = trait_ref.path[0].name.clone();
                        if self.has_symbol(&name).is_none() {
                            self.errors.push(SemanticError {
                                message: format!("unknown trait '{}'", name),
                                span: trait_ref.span.clone(),
                            });
                        }
                    }
                }
                self.check_type(&impl_item.self_type);
                for item in &impl_item.items {
                    match item {
                        ast::ImplItemKind::Function(func) => {
                            self.push_type_params(func.generics.as_ref());
                            self.check_generics_bounds(func.generics.as_ref());
                            self.push_var_scope();
                            for param in &func.parameters {
                                self.check_type(&param.param_type);
                                self.bind_var(&param.name, param.span.clone());
                            }
                            if let Some(return_type) = &func.return_type {
                                self.check_type(return_type);
                            }
                            self.check_block(&func.body);
                            self.pop_var_scope();
                            self.pop_type_params();
                        }
                        ast::ImplItemKind::AssociatedType(assoc) => {
                            self.check_type(&assoc.type_def);
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            self.check_type(&cast.target_type);
                            self.push_var_scope();
                            for param in &cast.parameters {
                                self.check_type(&param.param_type);
                                self.bind_var(&param.name, param.span.clone());
                            }
                            self.check_block(&cast.body);
                            self.pop_var_scope();
                        }
                    }
                }
                self.pop_type_params();
            }
            ast::ItemKind::ExternVariable(var) => {
                self.check_type(&var.var_type);
            }
            _ => {}
        }
    }

    fn check_block(&mut self, block: &ast::Block) {
        self.push_var_scope();
        for stmt in &block.statements {
            self.check_statement(stmt);
        }
        self.pop_var_scope();
    }

    fn check_statement(&mut self, stmt: &ast::Statement) {
        match &stmt.kind {
            ast::StatementKind::Block(block) => self.check_block(block),
            ast::StatementKind::Let(let_stmt) => {
                if let Some(init) = &let_stmt.initializer {
                    self.check_expression(init);
                }
                self.bind_pattern(&let_stmt.pattern);
            }
            ast::StatementKind::Expression(expr) => self.check_expression(expr),
            ast::StatementKind::Return(value) => {
                if let Some(expr) = value {
                    self.check_expression(expr);
                }
            }
            ast::StatementKind::Break(value) => {
                if let Some(expr) = value {
                    self.check_expression(expr);
                }
            }
            ast::StatementKind::Continue => {}
        }
    }

    fn check_type(&mut self, ty: &ast::Type) {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1 {
                    let name = &named.path[0].name;
                    if self.is_type_param(name) {
                        return;
                    }
                    if self.has_symbol(name).is_none() && !self.imported_types.contains(name) {
                        self.errors.push(SemanticError {
                            message: format!("unknown type '{}'", name),
                            span: named.path[0].span.clone(),
                        });
                    }
                } else {
                    let name = named
                        .path
                        .last()
                        .map(|id| id.name.clone())
                        .unwrap_or_default();
                    if !name.is_empty()
                        && self.has_symbol(&name).is_none()
                        && !self.imported_types.contains(&name)
                    {
                        self.errors.push(SemanticError {
                            message: format!("unknown type '{}'", name),
                            span: named
                                .path
                                .last()
                                .map(|id| id.span.clone())
                                .unwrap_or_else(|| Span { start: 0, end: 0 }),
                        });
                    }
                }

                if let Some(generics) = &named.generics {
                    for arg in generics {
                        self.check_type(arg);
                    }
                }
            }
            ast::TypeKind::Generic(generic) => {
                if !self.is_type_param(&generic.name.name)
                    && self.has_symbol(&generic.name.name).is_none()
                    && !self.imported_types.contains(&generic.name.name)
                {
                    self.errors.push(SemanticError {
                        message: format!("unknown type '{}'", generic.name.name),
                        span: generic.name.span.clone(),
                    });
                }
                for arg in &generic.args {
                    self.check_type(arg);
                }
            }
            ast::TypeKind::Reference(reference) => self.check_type(&reference.inner),
            ast::TypeKind::Pointer(pointer) => self.check_type(&pointer.inner),
            ast::TypeKind::Array(array) => {
                self.check_type(&array.element_type);
                if let Some(size) = &array.size {
                    self.check_expression(size);
                }
            }
            ast::TypeKind::Optional(inner) => self.check_type(inner),
            ast::TypeKind::Tuple(items) => {
                for item in items {
                    self.check_type(item);
                }
            }
            ast::TypeKind::Function(func) => {
                for param in &func.parameters {
                    self.check_type(param);
                }
                self.check_type(&func.return_type);
            }
            ast::TypeKind::Primitive(_) => {}
        }
    }

    fn check_expression(&mut self, expr: &ast::Expression) {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Literal(_) => {}
            ast::ExpressionKind::Identifier(ident) => self.resolve_var(ident),
            ast::ExpressionKind::TypeName(_) => {}
            ast::ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                if matches!(
                    operator,
                    ast::BinaryOperator::Assign
                        | ast::BinaryOperator::AddAssign
                        | ast::BinaryOperator::SubtractAssign
                        | ast::BinaryOperator::MultiplyAssign
                        | ast::BinaryOperator::DivideAssign
                        | ast::BinaryOperator::ModuloAssign
                ) {
                    self.mark_mutated(left);
                }
                self.check_expression(left);
                self.check_expression(right);
            }
            ast::ExpressionKind::Unary { operator, operand } => {
                if matches!(
                    operator,
                    ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement
                ) {
                    self.mark_mutated(operand);
                }
                self.check_expression(operand);
            }
            ast::ExpressionKind::Postfix { operator, operand } => {
                if matches!(
                    operator,
                    ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement
                ) {
                    self.mark_mutated(operand);
                }
                self.check_expression(operand);
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                self.check_expression(function);
                for arg in arguments {
                    self.check_expression(arg);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                self.check_expression(receiver);
                for arg in arguments {
                    self.check_expression(arg);
                }
            }
            ast::ExpressionKind::FieldAccess { object, .. } => {
                self.check_expression(object);
            }
            ast::ExpressionKind::Index { object, index } => {
                self.check_expression(object);
                self.check_expression(index);
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expression(condition);
                self.check_block(then_branch);
                if let Some(block) = else_branch {
                    self.check_block(block);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                self.check_expression(condition);
                self.check_block(body);
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.push_var_scope();
                if let Some(init_expr) = &init.initializer {
                    self.check_expression(init_expr);
                }
                self.bind_pattern(&init.pattern);
                self.check_expression(condition);
                self.check_expression(increment);
                self.check_block(body);
                self.pop_var_scope();
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.check_expression(expression);
                for arm in arms {
                    self.push_var_scope();
                    self.bind_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.check_expression(guard);
                    }
                    self.check_expression(&arm.body);
                    self.pop_var_scope();
                }
            }
            ast::ExpressionKind::Block(block) => self.check_block(block),
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(expr) => self.check_expression(expr),
                        ast::InitializerItem::Field { value, .. } => self.check_expression(value),
                        ast::InitializerItem::Index { index, value } => {
                            self.check_expression(index);
                            self.check_expression(value);
                        }
                    }
                }
            }
            ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
                for item in items {
                    self.check_expression(item);
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.check_expression(&field.value);
                }
            }
            ast::ExpressionKind::Cast { expression, .. } => self.check_expression(expression),
            ast::ExpressionKind::Move(expr)
            | ast::ExpressionKind::Comptime(expr)
            | ast::ExpressionKind::Reference {
                expression: expr, ..
            } => self.check_expression(expr),
            ast::ExpressionKind::Asm(_) => {}
            ast::ExpressionKind::MacroCall { .. } => {}
        }
    }

    fn check_generics_bounds(&mut self, generics: Option<&ast::Generics>) {
        let Some(generics) = generics else {
            return;
        };
        for param in &generics.params {
            if let ast::GenericParam::Type(type_param) = param {
                self.check_trait_bounds(&type_param.bounds);
            }
        }
        if let Some(where_clause) = &generics.where_clause {
            for predicate in &where_clause.predicates {
                match predicate {
                    ast::WherePredicate::Type {
                        bounded_type,
                        bounds,
                    } => {
                        self.check_type(bounded_type);
                        self.check_trait_bounds(bounds);
                    }
                    ast::WherePredicate::Lifetime { .. } => {}
                }
            }
        }
    }

    fn check_trait_bounds(&mut self, bounds: &[ast::TraitBound]) {
        for bound in bounds {
            let path = &bound.trait_ref.path;
            if path.is_empty() {
                continue;
            }
            if path.len() == 1 {
                let name = &path[0].name;
                match self.has_symbol(name) {
                    Some(SymbolKind::Trait) => {}
                    Some(_) => {
                        self.errors.push(SemanticError {
                            message: format!("'{}' is not a trait", name),
                            span: path[0].span.clone(),
                        });
                    }
                    None => {
                        self.errors.push(SemanticError {
                            message: format!("unknown trait '{}'", name),
                            span: path[0].span.clone(),
                        });
                    }
                }
            } else if let Some(last) = path.last() {
                let name = &last.name;
                match self.has_symbol(name) {
                    Some(SymbolKind::Trait) => {}
                    Some(_) => {
                        self.errors.push(SemanticError {
                            message: format!("'{}' is not a trait", name),
                            span: last.span.clone(),
                        });
                    }
                    None => {
                        self.errors.push(SemanticError {
                            message: format!("unknown trait '{}'", name),
                            span: last.span.clone(),
                        });
                    }
                }
            }
        }
    }

    fn push_type_params(&mut self, generics: Option<&ast::Generics>) {
        let mut params = HashSet::new();
        if let Some(generics) = generics {
            for param in &generics.params {
                if let ast::GenericParam::Type(type_param) = param {
                    params.insert(type_param.name.name.clone());
                }
            }
        }
        self.type_params.push(params);
    }

    fn collect_implicit_type_params(&self, ty: &ast::Type, params: &mut HashSet<String>) {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1 {
                    let name = &named.path[0].name;
                    if !self.symbols.contains_key(name) {
                        params.insert(name.clone());
                    }
                }
                if let Some(generics) = &named.generics {
                    for arg in generics {
                        self.collect_implicit_type_params(arg, params);
                    }
                }
            }
            ast::TypeKind::Generic(generic) => {
                if !self.symbols.contains_key(&generic.name.name) {
                    params.insert(generic.name.name.clone());
                }
                for arg in &generic.args {
                    self.collect_implicit_type_params(arg, params);
                }
            }
            ast::TypeKind::Reference(reference) => {
                self.collect_implicit_type_params(&reference.inner, params)
            }
            ast::TypeKind::Pointer(pointer) => {
                self.collect_implicit_type_params(&pointer.inner, params)
            }
            ast::TypeKind::Array(array) => {
                self.collect_implicit_type_params(&array.element_type, params)
            }
            ast::TypeKind::Optional(inner) => self.collect_implicit_type_params(inner, params),
            ast::TypeKind::Tuple(items) => {
                for item in items {
                    self.collect_implicit_type_params(item, params);
                }
            }
            ast::TypeKind::Function(func) => {
                for param in &func.parameters {
                    self.collect_implicit_type_params(param, params);
                }
                self.collect_implicit_type_params(&func.return_type, params)
            }
            ast::TypeKind::Primitive(_) => {}
        }
    }

    fn pop_type_params(&mut self) {
        self.type_params.pop();
    }

    fn is_type_param(&self, name: &str) -> bool {
        for scope in self.type_params.iter().rev() {
            if scope.contains(name) {
                return true;
            }
        }
        false
    }

    fn push_var_scope(&mut self) {
        self.table_backend.push_var_scope();
    }

    fn pop_var_scope(&mut self) {
        self.table_backend.pop_var_scope();
    }

    fn bind_var(&mut self, ident: &ast::Identifier, span: Span) {
        let bind = self.table_backend.bind_var(&ident.name, span.clone());
        if bind == BindVarResult::DuplicateInScope {
            self.errors.push(SemanticError {
                message: format!("duplicate variable '{}'", ident.name),
                span,
            });
        }
    }

    fn bind_pattern(&mut self, pattern: &ast::Pattern) {
        match &pattern.kind {
            ast::PatternKind::Identifier(ident) => self.bind_var(ident, pattern.span.clone()),
            ast::PatternKind::Tuple(items) => {
                for item in items {
                    self.bind_pattern(item);
                }
            }
            ast::PatternKind::Struct { fields, .. } => {
                for field in fields {
                    if let Some(inner) = &field.pattern {
                        self.bind_pattern(inner);
                    }
                }
            }
            ast::PatternKind::Enum { data, .. } => {
                if let Some(inner) = data {
                    self.bind_pattern(inner);
                }
            }
            ast::PatternKind::Literal(_) | ast::PatternKind::Wildcard => {}
        }
    }

    fn resolve_var(&mut self, ident: &ast::Identifier) {
        if self.table_backend.resolve_var(&ident.name) {
            return;
        }

        match self.has_symbol(&ident.name) {
            Some(SymbolKind::Function)
            | Some(SymbolKind::GlobalVariable)
            | Some(SymbolKind::Struct)
            | Some(SymbolKind::Enum)
            | Some(SymbolKind::Trait)
            | Some(SymbolKind::ExternVariable) => return,
            None => {}
        }

        self.errors.push(SemanticError {
            message: format!("unknown identifier '{}'", ident.name),
            span: ident.span.clone(),
        });
    }

    fn mark_mutated(&mut self, expr: &ast::Expression) {
        if let Some(name) = self.lvalue_root(expr) {
            if self.table_backend.mark_var_mutated(&name) {
                return;
            }
            if matches!(
                self.has_symbol(&name),
                Some(SymbolKind::GlobalVariable) | Some(SymbolKind::ExternVariable)
            ) {
                return;
            }
            self.errors.push(SemanticError {
                message: format!("unknown identifier '{}'", name),
                span: expr.span.clone(),
            });
        }
    }

    fn lvalue_root(&self, expr: &ast::Expression) -> Option<String> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => Some(ident.name.clone()),
            ast::ExpressionKind::FieldAccess { object, .. } => self.lvalue_root(object),
            ast::ExpressionKind::Index { object, .. } => self.lvalue_root(object),
            ast::ExpressionKind::Reference { expression, .. } => self.lvalue_root(expression),
            ast::ExpressionKind::Move(inner) => self.lvalue_root(inner),
            _ => None,
        }
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
    fn reports_unknown_type_in_field() {
        let program = parse("struct Foo { Bar b; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(!errors.is_empty(), "expected errors");
    }

    #[test]
    fn allows_generic_type_params() {
        let program = parse("struct Box<T> { T value; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn allows_implicit_impl_type_params() {
        let program = parse(
            "struct Dim<T> { T x; } impl Dim<T> { Dim<T> new(T x) { Dim<T> d = { .x = x }; return d; } }",
        );
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn allows_trait_bounds_and_array_bounds() {
        let program = parse("trait Copy {} struct Foo<T> where [T; 4]: Copy { [T; 4] x; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn reports_unknown_trait_bound() {
        let program = parse("struct Foo<T: Missing> { T x; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(!errors.is_empty(), "expected errors");
    }

    #[test]
    fn reports_duplicate_local_bindings() {
        let program = parse("i32 main() { i32 x = 1; i32 x = 2; return 0; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(!errors.is_empty(), "expected errors");
    }

    #[test]
    fn reports_unknown_identifier() {
        let program = parse("i32 main() { i32 x = 1; y; return 0; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(!errors.is_empty(), "expected errors");
    }

    #[test]
    fn resolves_parameter_identifier() {
        let program = parse("i32 main(i32 x) { return x; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn marks_mutated_variable() {
        let program = parse("i32 main() { i32 x = 1; x = 2; return x; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        let mutated = analyzer
            .var_records()
            .iter()
            .find(|record| record.name == "x")
            .map(|record| record.mutated)
            .unwrap_or(false);
        assert!(mutated, "expected x to be marked mutated");
    }

    #[test]
    fn keeps_const_when_not_mutated() {
        let program = parse("i32 main() { i32 x = 1; return x; }");
        let mut analyzer = Analyzer::new();
        let errors = analyzer.analyze_program(&program);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        let mutated = analyzer
            .var_records()
            .iter()
            .find(|record| record.name == "x")
            .map(|record| record.mutated)
            .unwrap_or(true);
        assert!(!mutated, "expected x to be const");
    }

    struct CountingHook {
        before_calls: usize,
        items_seen: usize,
        after_calls: usize,
        saw_errors: bool,
    }

    impl SemanticAnalyzerHook for CountingHook {
        fn before_analysis(&mut self, _program: &ast::Program) {
            self.before_calls += 1;
        }

        fn on_item_analyzed(&mut self, _item: &ast::Item) {
            self.items_seen += 1;
        }

        fn after_analysis(&mut self, _program: &mut ast::Program, errors: &[SemanticError]) {
            self.after_calls += 1;
            self.saw_errors = !errors.is_empty();
        }
    }

    #[test]
    fn hook_interface_runs_callbacks() {
        let mut program = parse("struct Foo { i32 x; } i32 main() { return 0; }");
        let mut analyzer = Analyzer::new();
        let mut hook = CountingHook {
            before_calls: 0,
            items_seen: 0,
            after_calls: 0,
            saw_errors: false,
        };
        let mut hooks: [&mut dyn SemanticAnalyzerHook; 1] = [&mut hook];

        let errors = analyzer.analyze_program_with_hooks(&mut program, &mut hooks);

        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        assert_eq!(hook.before_calls, 1);
        assert_eq!(hook.items_seen, 2);
        assert_eq!(hook.after_calls, 1);
        assert!(!hook.saw_errors);
    }

    struct MutatingHook;

    impl SemanticAnalyzerHook for MutatingHook {
        fn after_analysis(&mut self, program: &mut ast::Program, _errors: &[SemanticError]) {
            program.items.clear();
        }
    }

    #[test]
    fn hook_can_mutate_program_after_analysis() {
        let mut program = parse("i32 main() { return 0; }");
        let mut analyzer = Analyzer::new();
        let mut hook = MutatingHook;
        let mut hooks: [&mut dyn SemanticAnalyzerHook; 1] = [&mut hook];

        let errors = analyzer.analyze_program_with_hooks(&mut program, &mut hooks);

        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        assert!(program.items.is_empty(), "expected hook to mutate program");
    }

    fn is_reserved(name: &str) -> bool {
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
                | "Vec"
                | "Optional"
                | "struct"
                | "enum"
                | "impl"
                | "trait"
                | "fn"
                | "let"
                | "mut"
                | "const"
                | "if"
                | "else"
                | "while"
                | "for"
                | "break"
                | "continue"
                | "return"
                | "import"
                | "comptime"
                | "cast"
                | "move"
                | "ref"
                | "extern"
                | "pub"
                | "private"
                | "true"
                | "false"
        )
    }

    fn valid_ident(raw: &str) -> Option<String> {
        let mut name = String::new();
        let mut started = false;
        for ch in raw.chars() {
            if !started {
                if ch.is_ascii_alphabetic() || ch == '_' {
                    name.push(ch);
                    started = true;
                }
            } else if ch.is_ascii_alphanumeric() || ch == '_' {
                name.push(ch);
            } else {
                break;
            }
        }
        if name.is_empty() || is_reserved(&name) {
            None
        } else {
            Some(name)
        }
    }

    #[test]
    fn prop_reports_unknown_type_name() {
        fn property(raw: String) -> TestResult {
            let Some(name) = valid_ident(&raw) else {
                return TestResult::discard();
            };
            let program = parse(&format!("struct Foo {{ {name} field; }}"));
            let mut analyzer = Analyzer::new();
            let errors = analyzer.analyze_program(&program);
            if errors.is_empty() {
                return TestResult::failed();
            }
            TestResult::passed()
        }

        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(String) -> TestResult);
    }

    #[test]
    fn prop_reports_duplicate_top_level_name() {
        fn property(raw: String) -> TestResult {
            let Some(name) = valid_ident(&raw) else {
                return TestResult::discard();
            };
            let program = parse(&format!(
                "struct {name} {{ i32 x; }} struct {name} {{ i32 y; }}"
            ));
            let mut analyzer = Analyzer::new();
            let errors = analyzer.analyze_program(&program);
            if errors.is_empty() {
                return TestResult::failed();
            }
            TestResult::passed()
        }

        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(String) -> TestResult);
    }

    #[test]
    fn prop_reports_duplicate_local_binding() {
        fn property(raw: String) -> TestResult {
            let Some(name) = valid_ident(&raw) else {
                return TestResult::discard();
            };
            let program = parse(&format!(
                "i32 main() {{ i32 {name} = 1; i32 {name} = 2; return 0; }}"
            ));
            let mut analyzer = Analyzer::new();
            let errors = analyzer.analyze_program(&program);
            if errors.is_empty() {
                return TestResult::failed();
            }
            TestResult::passed()
        }

        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(String) -> TestResult);
    }

    #[test]
    fn prop_reports_unknown_identifier_use() {
        fn property(raw: String) -> TestResult {
            let Some(name) = valid_ident(&raw) else {
                return TestResult::discard();
            };
            let program = parse(&format!("i32 main() {{ {name}; return 0; }}"));
            let mut analyzer = Analyzer::new();
            let errors = analyzer.analyze_program(&program);
            if errors.is_empty() {
                return TestResult::failed();
            }
            TestResult::passed()
        }

        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(String) -> TestResult);
    }
}
