use std::collections::{HashMap, HashSet};

<<<<<<< HEAD
=======
use crate::attributes::validate_global_attributes;
>>>>>>> cc823df (shift to LL3)
use crate::lexer::Span;
use crate::module_artifact::ModuleArtifact;
use crate::parser::ast;
use crate::semantic::analyzer::Analyzer;
use crate::semantic::monomorph::MonomorphRequest;
use crate::symbol_table::{CompilerPhase, CompilerSymbolTable, SymbolId, SymbolKind};
use crate::traits::validate_traits_with_imports;
use crate::types::{
<<<<<<< HEAD
    is_bool, is_integer, is_numeric, is_string, parse_struct_attributes, struct_layout,
    StructAttrError, Type, TypeContext,
=======
    StructAttrError, Type, TypeContext, is_bool, is_integer, is_numeric, is_string,
    parse_struct_attributes, struct_layout,
>>>>>>> cc823df (shift to LL3)
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

#[derive(Default)]
pub struct TypeChecker {
    errors: Vec<TypeError>,
    scopes: Vec<HashMap<String, Type>>,
    current_return: Option<Type>,
    method_symbols: HashMap<SymbolId, MethodSig>,
    methods: HashMap<(String, String), Vec<SymbolId>>,
    type_ctx: TypeContext,
    function_symbols: HashMap<SymbolId, FunctionSig>,
    functions: HashMap<String, Vec<SymbolId>>,
    known_type_ids: HashSet<SymbolId>,
    known_types: HashMap<String, SymbolId>,
    struct_defs: HashMap<String, StructDef>,
    trait_impls: HashMap<String, HashSet<String>>,
    monomorph_requests: Vec<MonomorphRequest>,
    imported_functions: HashMap<String, Vec<FunctionSig>>,
<<<<<<< HEAD
=======
    extern_variables: HashMap<String, Type>,
>>>>>>> cc823df (shift to LL3)
    imported_types: HashSet<String>,
    imported_traits: HashSet<String>,
    imported_modules: Vec<ModuleArtifact>,
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
    span: Span,
    bounds: Vec<TypeBoundPredicate>,
    source: ast::FunctionItem,
    is_variadic: bool,
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum MethodCallStyle {
    Instance,
    Static,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_imported_modules(mut self, modules: &[ModuleArtifact]) -> Self {
        self.imported_modules = modules.to_vec();
        for module in modules {
            self.ingest_module(module);
        }
        self
    }

    pub fn check_program(self, program: &ast::Program) -> (Vec<TypeError>, Vec<MonomorphRequest>) {
        let mut table = CompilerSymbolTable::new();
        self.check_program_with_table(program, &mut table)
    }

    pub fn check_program_with_table(
        mut self,
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
<<<<<<< HEAD
=======
        self.collect_extern_variables(program, table);
>>>>>>> cc823df (shift to LL3)
        self.collect_imported_functions(table);
        self.collect_impl_methods(program, table);
        self.collect_struct_layouts(program);
        for item in &program.items {
<<<<<<< HEAD
=======
            self.check_global_attributes(&item.attributes);
>>>>>>> cc823df (shift to LL3)
            if let ast::ItemKind::Struct(_) = &item.kind {
                self.check_struct_attributes(&item.attributes);
            }
            if let ast::ItemKind::Function(func) = &item.kind {
                self.check_function(func);
            }
        }
        table.touch_phase(
            CompilerPhase::TypeCheck,
            format!("type checking done (errors={})", self.errors.len()),
        );
        let requests = std::mem::take(&mut self.monomorph_requests);
        (self.errors, requests)
    }

    fn ingest_module(&mut self, module: &ModuleArtifact) {
        for export in &module.exports {
            match export.kind {
                crate::module_artifact::ExportKind::Function => {
                    if let Ok((params, return_type)) =
                        crate::types::parse_canonical_function_signature(&export.signature)
                    {
                        let sig = FunctionSig {
                            params,
                            return_type,
                            type_params: Vec::new(),
                            span: Span { start: 0, end: 0 },
                            bounds: Vec::new(),
                            source: ast::FunctionItem {
                                name: ast::Identifier {
                                    name: export.name.clone(),
                                    span: Span { start: 0, end: 0 },
                                },
                                generics: None,
                                parameters: Vec::new(),
                                return_type: None,
                                body: ast::Block {
                                    statements: Vec::new(),
                                    span: Span { start: 0, end: 0 },
                                },
                            },
                            is_variadic: false,
                        };
                        self.imported_functions
                            .entry(export.name.clone())
                            .or_default()
                            .push(sig);
                    }
                }
                crate::module_artifact::ExportKind::Struct
                | crate::module_artifact::ExportKind::Enum => {
                    self.imported_types.insert(export.name.clone());
                }
                crate::module_artifact::ExportKind::Trait => {
                    self.imported_traits.insert(export.name.clone());
                }
            }
        }
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
                        Some(struct_item.name.span.clone()),
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
                    let type_id = table.intern_symbol(
                        format!("type::{}", enum_item.name.name),
                        SymbolKind::Enum,
                        Some(enum_item.name.span.clone()),
                        CompilerPhase::TypeCheck,
                    );
                    self.known_type_ids.insert(type_id);
                    self.known_types
                        .insert(enum_item.name.name.clone(), type_id);
                }
                ast::ItemKind::Trait(trait_item) => {
                    let type_id = table.intern_symbol(
                        format!("type::{}", trait_item.name.name),
                        SymbolKind::Trait,
                        Some(trait_item.name.span.clone()),
                        CompilerPhase::TypeCheck,
                    );
                    self.known_type_ids.insert(type_id);
                    self.known_types
                        .insert(trait_item.name.name.clone(), type_id);
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
            if !self.is_concrete_type(&self_ty) {
                continue;
            }
            let key = self_ty.canonical_key();
            self.trait_impls.entry(name).or_default().insert(key);
        }
    }

    fn check_function(&mut self, func: &ast::FunctionItem) {
        let return_type = func
            .return_type
            .as_ref()
            .map(Type::from_ast)
            .unwrap_or(Type::Unit);
        self.current_return = Some(return_type);

        self.push_scope();
        for param in &func.parameters {
            let param_type = Type::from_ast(&param.param_type);
            self.bind(&param.name.name, param_type, param.span.clone());
        }
        self.check_block(&func.body);
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
                let Some(annotation) = let_stmt.type_annotation.as_ref() else {
                    self.error(
                        "local bindings must include a type annotation",
                        stmt.span.clone(),
                    );
                    return;
                };

                let declared = Type::from_ast(annotation);

                if let Some(init) = &let_stmt.initializer {
                    let init_type = self.check_expr(init, Some(&declared));
                    if !self.is_assignable(&declared, &init_type)
                        && !self.is_implicitly_castable(&init_type, &declared)
                    {
                        self.error(
                            format!(
                                "type mismatch: expected {:?}, found {:?}",
                                declared, init_type
                            ),
                            init.span.clone(),
                        );
                    }
                }

                match &let_stmt.pattern.kind {
                    ast::PatternKind::Identifier(ident) => {
                        self.bind(&ident.name, declared, let_stmt.pattern.span.clone());
                    }
                    _ => {
                        self.error(
                            "type checking for complex let patterns is not implemented",
                            let_stmt.pattern.span.clone(),
                        );
                    }
                }
            }
            ast::StatementKind::Expression(expr) => {
                self.check_expr(expr, None);
            }
            ast::StatementKind::Return(value) => {
                let expected = self.current_return.clone().unwrap_or(Type::Unit);
                match value {
                    Some(expr) => {
                        let found = self.check_expr(expr, Some(&expected));
                        if !self.is_assignable(&expected, &found)
                            && !self.is_implicitly_castable(&found, &expected)
                        {
                            self.error(
                                format!(
                                    "return type mismatch: expected {:?}, found {:?}",
                                    expected, found
                                ),
                                expr.span.clone(),
                            );
                        }
                    }
                    None => {
                        if expected != Type::Unit {
                            self.error(
                                format!(
                                    "return type mismatch: expected {:?}, found unit",
                                    expected
                                ),
                                stmt.span.clone(),
                            );
                        }
                    }
                }
            }
            ast::StatementKind::Break(value) => {
                if let Some(expr) = value {
                    self.check_expr(expr, None);
                }
            }
            ast::StatementKind::Continue => {}
        }
    }

    fn check_expr(&mut self, expr: &ast::Expression, expected: Option<&Type>) -> Type {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Literal(literal) => self.literal_type(literal, expected),
            ast::ExpressionKind::Identifier(ident) => match self.lookup(&ident.name) {
                Some(ty) => ty,
                None => {
                    if self.known_types.contains_key(&ident.name) {
                        return Type::Named {
                            path: vec![ident.name.clone()],
                            generics: Vec::new(),
                        };
                    }
                    self.error(
                        format!("unknown identifier '{}'", ident.name),
                        ident.span.clone(),
                    );
                    Type::Unknown
                }
            },
            ast::ExpressionKind::TypeName(ty) => Type::from_ast(ty),
            ast::ExpressionKind::Unary { operator, operand } => {
                let operand_expected = expected.filter(|ty| is_numeric(ty) || is_bool(ty));
                let operand_ty = self.check_expr(operand, operand_expected);
                match operator {
                    ast::UnaryOperator::Plus | ast::UnaryOperator::Minus => {
                        if !is_numeric(&operand_ty) {
                            self.error("unary +/- requires numeric operand", expr.span.clone());
                        }
                        operand_ty
                    }
                    ast::UnaryOperator::Not => {
                        if !is_bool(&operand_ty) {
                            self.error("logical not requires bool", expr.span.clone());
                        }
                        Type::Primitive(ast::PrimitiveType::Bool)
                    }
                    ast::UnaryOperator::BitwiseNot => {
                        if !is_integer(&operand_ty) {
                            self.error("bitwise not requires integer", expr.span.clone());
                        }
                        operand_ty
                    }
                    ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement => {
                        if !self.is_incdec_type(&operand_ty) {
                            self.error(
                                "increment/decrement requires numeric or pointer operand",
                                expr.span.clone(),
                            );
                        }
                        operand_ty
                    }
                }
            }
            ast::ExpressionKind::Postfix { operator, operand } => {
                let operand_ty = self.check_expr(operand, None);
                match operator {
                    ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement => {
                        if !self.is_incdec_type(&operand_ty) {
                            self.error(
                                "increment/decrement requires numeric or pointer operand",
                                expr.span.clone(),
                            );
                        }
                        operand_ty
                    }
                    _ => {
                        self.error("invalid postfix operator", expr.span.clone());
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
                    let numeric_expected = expected.filter(|ty| is_numeric(ty));
                    let left_ty = self.check_expr(left, numeric_expected);
                    let right_ty = self.check_expr(right, numeric_expected);
                    if is_numeric(&left_ty) && is_numeric(&right_ty) {
                        if let Some(common) = self.common_numeric_type(&left_ty, &right_ty) {
                            return common;
                        }
                    }

                    if let Some(ty) =
                        self.resolve_operator_overload(&left_ty, &right_ty, operator, expr)
                    {
                        return ty;
                    }

                    self.error(
                        "binary operator requires numeric operands",
                        expr.span.clone(),
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
                    if is_numeric(&left_ty) && is_numeric(&right_ty) {
                        if self.common_numeric_type(&left_ty, &right_ty).is_none() {
                            self.error("comparison operands must be compatible", expr.span.clone());
                        }
                        return Type::Primitive(ast::PrimitiveType::Bool);
                    }

                    if let Some(ty) =
                        self.resolve_operator_overload(&left_ty, &right_ty, operator, expr)
                    {
                        let bool_ty = Type::Primitive(ast::PrimitiveType::Bool);
                        if !self.is_implicitly_castable(&ty, &bool_ty) {
                            self.error("comparison operator must return bool", expr.span.clone());
                        }
                        return bool_ty;
                    }

                    if left_ty != right_ty {
                        self.error("comparison operands must match types", expr.span.clone());
                    }
                    Type::Primitive(ast::PrimitiveType::Bool)
                }
                ast::BinaryOperator::LogicalAnd | ast::BinaryOperator::LogicalOr => {
                    let left_ty = self.check_expr(left, None);
                    let right_ty = self.check_expr(right, None);
                    let bool_ty = Type::Primitive(ast::PrimitiveType::Bool);
                    if !is_bool(&left_ty) && !self.is_implicitly_castable(&left_ty, &bool_ty) {
                        self.error("logical operator requires bool operands", expr.span.clone());
                    }
                    if !is_bool(&right_ty) && !self.is_implicitly_castable(&right_ty, &bool_ty) {
                        self.error("logical operator requires bool operands", expr.span.clone());
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
                        self.error("bitwise operands must match types", expr.span.clone());
                    }
                    if !is_integer(&left_ty) || !is_integer(&right_ty) {
                        self.error(
                            "bitwise operator requires integer operands",
                            expr.span.clone(),
                        );
                    }
                    left_ty
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
                        self.error("assignment operands must match types", expr.span.clone());
                    }
                    left_ty
                }
            },
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.check_expr(condition, None);
                if !is_bool(&cond_ty) {
                    self.error("if condition must be bool", condition.span.clone());
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
                    self.error("while condition must be bool", condition.span.clone());
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
                    span: init.pattern.span.clone(),
                };
                self.check_statement(&init_stmt);
                let cond_ty = self.check_expr(condition, None);
                if !is_bool(&cond_ty) {
                    self.error("for condition must be bool", condition.span.clone());
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
                if let Some(Type::Array { element, length }) = expected {
                    for element_expr in elements {
                        let item_ty = self.check_expr(element_expr, Some(element));
                        if !self.is_assignable(element, &item_ty) {
                            self.error("array element type mismatch", element_expr.span.clone());
                        }
                    }
                    Type::Array {
                        element: element.clone(),
                        length: *length,
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
                    self.error(
                        format!("invalid cast: {:?} -> {:?}", from, to),
                        expr.span.clone(),
                    );
                }
                to
            }
            ast::ExpressionKind::Reference {
                expression,
                is_mutable: _is_mutable,
            } => Type::Pointer {
                is_mutable: true,
                inner: Box::new(self.check_expr(expression, None)),
            },
            ast::ExpressionKind::Move(inner) | ast::ExpressionKind::Comptime(inner) => {
                self.check_expr(inner, None)
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                if let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref() {
                    return self.resolve_overload(ident, arguments, expr.span.clone());
                }
                for arg in arguments {
                    self.check_expr(arg, None);
                }
                self.error(
                    "type checking for this call expression is not implemented",
                    expr.span.clone(),
                );
                Type::Unknown
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                let style = self.method_call_style(receiver);
                let receiver_ty = self.check_expr(receiver, None);
                return self.resolve_method_overload(
                    &receiver_ty,
                    method,
                    arguments,
                    style,
                    expr.span.clone(),
                );
            }
            ast::ExpressionKind::Index { object, index } => {
                let object_ty = self.check_expr(object, None);
                let index_ty = self.check_expr(index, None);
                if !is_integer(&index_ty) {
                    self.error("index expression must be integer", index.span.clone());
                }
                match object_ty {
                    Type::Array { element, .. } => (*element).clone(),
                    Type::Pointer { inner, .. } => match inner.as_ref() {
                        Type::Array { element, .. } => (**element).clone(),
                        _ => inner.as_ref().clone(),
                    },
                    Type::Reference { inner, .. } => match inner.as_ref() {
                        Type::Array { element, .. } => (**element).clone(),
                        Type::Pointer { inner, .. } => match inner.as_ref() {
                            Type::Array { element, .. } => (**element).clone(),
                            _ => inner.as_ref().clone(),
                        },
                        _ => {
                            self.error(
                                "indexing requires array or pointer type",
                                object.span.clone(),
                            );
                            Type::Unknown
                        }
                    },
                    _ => {
                        self.error(
                            "indexing requires array or pointer type",
                            object.span.clone(),
                        );
                        Type::Unknown
                    }
                }
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                let object_ty = self.check_expr(object, None);
                if let Some(field_ty) = self.resolve_field_access_type(&object_ty, &field.name) {
                    field_ty
                } else {
                    self.error(
                        format!("unknown field '{}' on type {:?}", field.name, object_ty),
                        field.span.clone(),
                    );
                    Type::Unknown
                }
            }
            ast::ExpressionKind::Match { .. }
            | ast::ExpressionKind::StructLiteral { .. }
            | ast::ExpressionKind::MacroCall { .. } => {
                self.error(
                    "type checking for this expression is not implemented",
                    expr.span.clone(),
                );
                Type::Unknown
            }
            ast::ExpressionKind::Asm(_) => Type::Unit,
        }
    }

    fn resolve_overload(
        &mut self,
        ident: &ast::Identifier,
        arguments: &[ast::Expression],
        span: Span,
    ) -> Type {
        let arg_types = arguments
            .iter()
            .map(|arg| self.check_expr(arg, None))
            .collect::<Vec<_>>();

        let Some(candidate_ids) = self.functions.get(&ident.name).cloned() else {
            self.error(format!("unknown function '{}'", ident.name), span);
            return Type::Unknown;
        };

        let mut matches: Vec<(usize, Type, HashMap<String, Type>, FunctionSig)> = Vec::new();

        for candidate_id in candidate_ids {
            let Some(candidate) = self.function_symbols.get(&candidate_id).cloned() else {
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
            let mut mapping = HashMap::new();

            for (param_ty, arg_ty) in candidate.params.iter().zip(arg_types.iter()) {
                if !self.infer_type_params(param_ty, &arg_ty, &candidate.type_params, &mut mapping)
                {
                    ok = false;
                    break;
                }
                let substituted = self.substitute_type(param_ty, &mapping);
                if self.is_assignable(&substituted, &arg_ty) {
                    continue;
                }
                if self.is_implicitly_castable(&arg_ty, &substituted) {
                    score += 1;
                } else {
                    ok = false;
                    break;
                }
            }

            if ok {
                if !self.bounds_satisfied(&candidate.bounds, &mapping, span.clone()) {
                    continue;
                }
                let return_type = self.substitute_type(&candidate.return_type, &mapping);
                matches.push((score, return_type, mapping, candidate));
            }
        }

        if matches.is_empty() {
            self.error(format!("no matching overload for '{}'", ident.name), span);
            return Type::Unknown;
        }
        matches.sort_by_key(|(score, _, _, _)| *score);
        let best_score = matches[0].0;
        let best_matches: Vec<_> = matches
            .into_iter()
            .filter(|(score, _, _, _)| *score == best_score)
            .collect();

        if best_matches.len() > 1 {
            self.error(format!("ambiguous overload for '{}'", ident.name), span);
            return Type::Unknown;
        }

        let (_, return_type, mapping, candidate) = &best_matches[0];
        self.record_function_monomorph(candidate, mapping, span.clone());
        return_type.clone()
    }

    fn resolve_method_overload(
        &mut self,
        receiver_ty: &Type,
        method: &ast::Identifier,
        arguments: &[ast::Expression],
        style: MethodCallStyle,
        span: Span,
    ) -> Type {
        let arg_types = arguments
            .iter()
            .map(|arg| self.check_expr(arg, None))
            .collect::<Vec<_>>();

        match self.resolve_method_overload_types(
            receiver_ty,
            &method.name,
            &arg_types,
            style,
            span.clone(),
        ) {
            Some(ty) => ty,
            None => {
                self.error(format!("no matching overload for '{}'", method.name), span);
                Type::Unknown
            }
        }
    }

    fn resolve_method_overload_types(
        &mut self,
        receiver_ty: &Type,
        name: &str,
        arg_types: &[Type],
        style: MethodCallStyle,
        span: Span,
    ) -> Option<Type> {
        let key = (self.method_key(receiver_ty), name.to_string());
        let Some(candidate_ids) = self.methods.get(&key).cloned() else {
            return None;
        };

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
            let mut mapping = HashMap::new();

            if !self.infer_type_params(
                &candidate.owner,
                owner_ty,
                &candidate.type_params,
                &mut mapping,
            ) {
                continue;
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
                for (param_ty, arg_ty) in iter.zip(arg_types.iter()) {
                    let param_ty = self.substitute_self_type(param_ty, receiver_ty);
                    let mut matched = false;

                    // First try with inferred type-parameter mapping.
                    let mut inferred_mapping = mapping.clone();
                    if self.infer_type_params(
                        &param_ty,
                        arg_ty,
                        &candidate.type_params,
                        &mut inferred_mapping,
                    ) {
                        let substituted = self.substitute_type(&param_ty, &inferred_mapping);
                        if self.is_assignable(&substituted, arg_ty) {
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
                if !self.bounds_satisfied(&candidate.bounds, &mapping, span.clone()) {
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
            self.error(format!("ambiguous overload for '{}'", name), span);
            return None;
        }

        let (_, return_type, mapping, candidate) = &best_matches[0];
        self.record_method_monomorph(candidate, mapping, span.clone());
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
                        parameters: func.signature.parameters.clone(),
                        return_type: func.signature.return_type.clone(),
                        body: ast::Block {
                            statements: Vec::new(),
                            span: func.name.span.clone(),
                        },
                    };
                    self.collect_function_item(&stub, func.signature.is_variadic, table);
                }
                ast::ItemKind::ExternBlock(block) => {
                    for func in &block.functions {
                        let stub = ast::FunctionItem {
                            name: func.name.clone(),
                            generics: None,
                            parameters: func.signature.parameters.clone(),
                            return_type: func.signature.return_type.clone(),
                            body: ast::Block {
                                statements: Vec::new(),
                                span: func.name.span.clone(),
                            },
                        };
                        self.collect_function_item(&stub, func.signature.is_variadic, table);
                    }
                }
                _ => {}
            }
        }
    }

<<<<<<< HEAD
=======
    fn collect_extern_variables(
        &mut self,
        program: &ast::Program,
        table: &mut CompilerSymbolTable,
    ) {
        for item in &program.items {
            let ast::ItemKind::ExternVariable(var) = &item.kind else {
                continue;
            };
            let symbol_key = format!("extern_var::{}", var.name.name);
            table.intern_symbol(
                symbol_key,
                SymbolKind::ExternVariable,
                Some(var.name.span.clone()),
                CompilerPhase::TypeCheck,
            );
            self.extern_variables
                .insert(var.name.name.clone(), Type::from_ast(&var.var_type));
        }
    }

>>>>>>> cc823df (shift to LL3)
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
            Some(func.name.span.clone()),
            CompilerPhase::TypeCheck,
        );
        debug_assert_eq!(table.symbol_id(&symbol_key), Some(symbol_id));
        debug_assert_eq!(table.symbol_key(symbol_id), Some(symbol_key.as_str()));
        self.function_symbols.insert(
            symbol_id,
            FunctionSig {
                params,
                return_type,
                type_params,
                span: func.name.span.clone(),
                bounds,
                source: func.clone(),
                is_variadic,
            },
        );
        self.functions
            .entry(func.name.name.clone())
            .or_default()
            .push(symbol_id);
    }

    fn literal_type(&mut self, literal: &ast::Literal, expected: Option<&Type>) -> Type {
        if let Some(expected_ty) = expected {
            if self.literal_matches_expected(literal, expected_ty) {
                return expected_ty.clone();
            }
        }

        match literal {
            ast::Literal::Integer(_) => Type::Primitive(ast::PrimitiveType::I32),
            ast::Literal::Float(_) => Type::Primitive(ast::PrimitiveType::F64),
            ast::Literal::Complex(_, _) => Type::Primitive(ast::PrimitiveType::C64),
            ast::Literal::String(_) => Type::Primitive(ast::PrimitiveType::Str),
            ast::Literal::Char(_) => Type::Primitive(ast::PrimitiveType::Char),
            ast::Literal::Bool(_) => Type::Primitive(ast::PrimitiveType::Bool),
        }
    }

    fn literal_matches_expected(&self, literal: &ast::Literal, expected: &Type) -> bool {
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
            ast::Literal::Char(_) => matches!(expected, Type::Primitive(ast::PrimitiveType::Char)),
            ast::Literal::Bool(_) => is_bool(expected),
        }
    }

    fn collect_bounds(&self, generics: Option<&ast::Generics>) -> Vec<TypeBoundPredicate> {
        let Some(generics) = generics else {
            return Vec::new();
        };
        let mut bounds = Vec::new();
        for param in &generics.params {
            if let ast::GenericParam::Type(type_param) = param {
                if !type_param.bounds.is_empty() {
                    bounds.push(TypeBoundPredicate {
                        bounded: Type::Named {
                            path: vec![type_param.name.name.clone()],
                            generics: Vec::new(),
                        },
                        bounds: type_param.bounds.clone(),
                    });
                }
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
                        span.clone(),
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
            source: candidate.source.clone(),
            type_params: candidate.type_params.clone(),
            mapping: mapping.clone(),
            call_span: span,
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
            impl_item: candidate.source_impl.clone(),
            method: candidate.source_method.clone(),
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
        let mut seen = HashSet::new();
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
        expected == found
    }

    fn infer_type_params(
        &self,
        expected: &Type,
        found: &Type,
        type_params: &[String],
        mapping: &mut HashMap<String, Type>,
    ) -> bool {
        if let Type::Named { path, generics } = expected {
            if path.len() == 1 && type_params.contains(&path[0]) && generics.is_empty() {
                if let Some(existing) = mapping.get(&path[0]) {
                    return existing == found;
                }
                mapping.insert(path[0].clone(), found.clone());
                return true;
            }
        }

        match (expected, found) {
            (Type::Primitive(_), _) | (Type::Unit, _) => expected == found,
            (Type::Pointer { is_mutable, inner }, Type::Primitive(ast::PrimitiveType::Str)) => {
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
                if path != found_path || generics.len() != found_generics.len() {
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
                is_mutable == found_mut
                    && self.infer_type_params(inner, found_inner, type_params, mapping)
            }
            (
                Type::Pointer { is_mutable, inner },
                Type::Pointer {
                    is_mutable: found_mut,
                    inner: found_inner,
                },
            ) => {
                is_mutable == found_mut
                    && self.infer_type_params(inner, found_inner, type_params, mapping)
            }
            (
                Type::Array { element, length },
                Type::Array {
                    element: found_elem,
                    length: found_len,
                },
            ) => {
                if length != found_len {
                    return false;
                }
                self.infer_type_params(element, found_elem, type_params, mapping)
            }
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
        self.is_primitive_type(from) && self.is_primitive_type(to)
    }

    fn is_implicitly_castable(&self, from: &Type, to: &Type) -> bool {
        if self.is_castable(from, to) {
            return true;
        }

        match (from, to) {
            (Type::Primitive(ast::PrimitiveType::Str), Type::Pointer { is_mutable, inner }) => {
                if *is_mutable {
                    return false;
                }
                matches!(inner.as_ref(), Type::Primitive(ast::PrimitiveType::Char))
            }
            _ => false,
        }
    }

    fn is_numeric_type(&self, ty: &Type) -> bool {
        is_numeric(ty)
    }

    fn receiver_compatible(&self, param: &Type, receiver: &Type, score: &mut usize) -> bool {
        match param {
            Type::Reference { inner, .. } => {
                if self.is_assignable(inner, receiver) {
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
            Type::Pointer { is_mutable, inner } => Type::Pointer {
                is_mutable: *is_mutable,
                inner: Box::new(self.substitute_self_type(inner, receiver)),
            },
            Type::Array { element, length } => Type::Array {
                element: Box::new(self.substitute_self_type(element, receiver)),
                length: *length,
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

    fn is_incdec_type(&self, ty: &Type) -> bool {
        is_numeric(ty) || matches!(ty, Type::Pointer { .. })
    }

    fn is_primitive_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Primitive(_))
    }

    fn common_numeric_type(&self, left: &Type, right: &Type) -> Option<Type> {
        if left == right {
            return Some(left.clone());
        }
        if self.is_implicitly_castable(right, left) {
            return Some(left.clone());
        }
        if self.is_implicitly_castable(left, right) {
            return Some(right.clone());
        }
        None
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

        let Some(name) = operator_method_name(operator) else {
            return None;
        };

        let result = self.resolve_method_overload_types(
            left,
            name,
            &[right.clone()],
            MethodCallStyle::Instance,
            expr.span.clone(),
        );
        if result.is_none() {
            self.error(
                format!("missing operator overload '{}' for {:?}", name, left),
                expr.span.clone(),
            );
        }
        result
    }

    fn method_call_style(&self, receiver: &ast::Expression) -> MethodCallStyle {
        match receiver.kind.as_ref() {
            ast::ExpressionKind::TypeName(_) => MethodCallStyle::Static,
            ast::ExpressionKind::Identifier(ident) => {
                if self.lookup(&ident.name).is_some() {
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
        for item in &program.items {
            let ast::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };

            let self_ty = Type::from_ast(&impl_item.self_type);
            let self_key = self.method_key(&self_ty);

            let mut impl_type_params = Vec::new();
            if let Some(generics) = &impl_item.generics {
                for param in &generics.params {
                    if let ast::GenericParam::Type(type_param) = param {
                        impl_type_params.push(type_param.name.name.clone());
                    }
                }
            } else {
                let mut implicit = HashSet::new();
                self.collect_implicit_type_params(&impl_item.self_type, &mut implicit);
                impl_type_params.extend(implicit.into_iter());
            }

            let bounds = self.collect_bounds(impl_item.generics.as_ref());

            for impl_member in &impl_item.items {
                let ast::ImplItemKind::Function(func) = impl_member else {
                    continue;
                };
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
                let symbol_key =
                    self.method_symbol_key(&self_ty, &func.name.name, &params, &return_type);
                let symbol_id = table.intern_symbol(
                    symbol_key.clone(),
                    SymbolKind::ImplMethod,
                    Some(func.name.span.clone()),
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
                        source_method: func.clone(),
                    },
                );
                self.methods
                    .entry((self_key.clone(), func.name.name.clone()))
                    .or_default()
                    .push(symbol_id);
            }
        }
    }

    fn collect_implicit_type_params(&self, ty: &ast::Type, params: &mut HashSet<String>) {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1 {
                    let name = &named.path[0].name;
                    if !self.known_types.contains_key(name) {
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
                if !self.known_types.contains_key(&generic.name.name) {
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

    fn type_key(&self, ty: &Type) -> String {
        ty.canonical_key()
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
            _ => ty.canonical_key(),
        }
    }

    fn resolve_field_access_type(&self, object_ty: &Type, field_name: &str) -> Option<Type> {
        let mut current = object_ty;
        while let Type::Reference { inner, .. } | Type::Pointer { inner, .. } = current {
            current = inner.as_ref();
        }

        let Type::Named { path, generics } = current else {
            return None;
        };

        let struct_name = path.last()?;
        let struct_def = self.struct_defs.get(struct_name)?;
        let field_ty = struct_def.fields.get(field_name)?;

        if struct_def.type_params.is_empty() || generics.is_empty() {
            return Some(field_ty.clone());
        }

        let mut mapping = HashMap::new();
        for (param, arg) in struct_def.type_params.iter().zip(generics.iter()) {
            mapping.insert(param.clone(), arg.clone());
        }
        Some(field_ty.substitute(&mapping))
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

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind(&mut self, name: &str, ty: Type, span: Span) {
        let mut duplicate = false;
        if let Some(scope) = self.scopes.last_mut() {
            duplicate = scope.contains_key(name);
            scope.insert(name.to_string(), ty);
        }
        if duplicate {
            self.error(format!("duplicate binding for '{}'", name), span.clone());
        }
    }

    fn lookup(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
<<<<<<< HEAD
=======
        if let Some(ty) = self.extern_variables.get(name) {
            return Some(ty.clone());
        }
>>>>>>> cc823df (shift to LL3)
        None
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
<<<<<<< HEAD
=======

    fn check_global_attributes(&mut self, attributes: &[ast::Attribute]) {
        for error in validate_global_attributes(attributes) {
            self.error(error.message, error.span);
        }
    }
>>>>>>> cc823df (shift to LL3)
}

fn operator_method_name(operator: &ast::BinaryOperator) -> Option<&'static str> {
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
        _ => None,
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
    fn type_checks_local_assignment() {
        let program = parse("i32 main() { i32 x = 1; return x; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
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
            errors.iter().any(|error| error.message.contains("Pointer")),
            "expected pointer type in diagnostics: {errors:?}"
        );
        assert!(
            !errors
                .iter()
                .any(|error| error.message.contains("Reference")),
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
            "i32 add(i32 a, i32 b) { return a; } i32 main() { i32 x = add(1, true); return x; }",
        );
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(!errors.is_empty(), "expected type errors");
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
<<<<<<< HEAD
=======

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
>>>>>>> cc823df (shift to LL3)
}
