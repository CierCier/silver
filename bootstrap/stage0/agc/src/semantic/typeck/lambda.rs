use super::{MethodSig, StructDef, TypeChecker};
use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::{ast, visit};
use crate::semantic::type_properties::{TypePropertiesContext, is_copy_with};
use crate::symbol_table::{CompilerPhase, CompilerSymbolTable, SymbolKind};
use crate::types::{Type, struct_layout};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

type ExprKey = (u32, usize, usize);

#[derive(Default)]
pub(super) struct LambdaState {
    pub symbols: CompilerSymbolTable,
    pub frames: Vec<CaptureFrame>,
    replacements: HashMap<ExprKey, ast::Expression>,
    items: Vec<ast::Item>,
    callable_types: HashSet<String>,
    method_receivers: HashMap<ExprKey, ast::Expression>,
    checked_types: HashMap<ExprKey, Type>,
    /// Keyed by span and node address: macro expansions copy body spans, so
    /// the span alone conflates identifiers resolved in different scopes.
    binding_origins: HashMap<(ExprKey, usize), (usize, Type)>,
}

pub(super) struct CaptureFrame {
    scope_start: usize,
    captures: HashMap<String, Capture>,
    occurrences: HashMap<ExprKey, String>,
    implicit_moves: HashSet<ExprKey>,
    pub returns: Vec<Type>,
}

struct Capture {
    ty: Type,
    mode: CaptureMode,
    span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum CaptureMode {
    Shared,
    Mutable,
    Owned,
}

fn key(span: Span) -> ExprKey {
    (span.file, span.start, span.end)
}

fn identifier(name: &str, span: Span) -> ast::Identifier {
    ast::Identifier {
        name: name.to_string(),
        span,
    }
}

fn variable(name: &str, span: Span) -> ast::Expression {
    ast::Expression {
        kind: Box::new(ast::ExpressionKind::Identifier(identifier(name, span))),
        span,
    }
}

fn item(kind: ast::ItemKind, span: Span) -> ast::Item {
    ast::Item {
        kind,
        span,
        visibility: ast::Visibility::Private,
        attributes: Vec::new(),
    }
}

fn place_root(expression: &ast::Expression) -> Option<&ast::Identifier> {
    match expression.kind.as_ref() {
        ast::ExpressionKind::Identifier(name) => Some(name),
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. } => place_root(object),
        ast::ExpressionKind::Move(value) => place_root(value),
        ast::ExpressionKind::Unary {
            operator: ast::UnaryOperator::Dereference,
            operand,
        } => place_root(operand),
        _ => None,
    }
}

impl TypeChecker {
    pub(super) fn record_lambda_type(&mut self, expression: &ast::Expression, ty: &Type) {
        if !self.lambdas.frames.is_empty() {
            self.lambdas
                .checked_types
                .insert(key(expression.span), ty.clone());
        }
    }

    pub(super) fn lambda_copy_type(&self, ty: &Type) -> bool {
        let mut context = TypePropertiesContext::new();
        context
            .drop_bases
            .extend(self.drop_owner_bases.iter().cloned());
        for (name, definition) in &self.struct_defs {
            context.insert_generic_struct(
                name.clone(),
                definition.type_params.clone(),
                definition.fields.values().map(Type::to_ast).collect(),
            );
        }
        for (name, definition) in &self.enum_defs {
            context.insert_generic_struct(
                name.clone(),
                definition.type_params.clone(),
                definition
                    .variants
                    .values()
                    .flat_map(|variant| variant.payload.iter().cloned())
                    .collect(),
            );
        }
        is_copy_with(&ty.to_ast(), &context)
    }

    pub(super) fn consume_capture(&mut self, expression: &ast::Expression) {
        if self.lambdas.frames.is_empty() {
            return;
        }
        if self
            .lambdas
            .checked_types
            .get(&key(expression.span))
            .is_some_and(|ty| self.lambda_copy_type(ty))
        {
            self.demand_capture(expression, CaptureMode::Shared);
            return;
        }
        match expression.kind.as_ref() {
            ast::ExpressionKind::Match { arms, .. } => {
                for arm in arms {
                    self.consume_capture(&arm.body);
                }
                return;
            }
            ast::ExpressionKind::Block(block) => {
                if let Some(ast::Statement {
                    kind: ast::StatementKind::Expression(value),
                    ..
                }) = block.statements.last()
                {
                    self.consume_capture(value);
                }
                return;
            }
            ast::ExpressionKind::Ternary {
                then_expr,
                else_expr,
                ..
            } => {
                self.consume_capture(then_expr);
                self.consume_capture(else_expr);
                return;
            }
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                self.consume_capture(value);
                self.consume_capture(fallback);
                return;
            }
            ast::ExpressionKind::Array(items)
            | ast::ExpressionKind::Tuple(items)
            | ast::ExpressionKind::EnumVariant { fields: items, .. } => {
                for item in items {
                    self.consume_capture(item);
                }
                return;
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.consume_capture(&field.value);
                }
                return;
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    let (ast::InitializerItem::Positional(value)
                    | ast::InitializerItem::Field { value, .. }
                    | ast::InitializerItem::Index { value, .. }) = item;
                    self.consume_capture(value);
                }
                return;
            }
            _ => {}
        }
        self.demand_capture(expression, CaptureMode::Owned);
        if matches!(expression.kind.as_ref(), ast::ExpressionKind::Move(_)) {
            return;
        }
        let Some(name) = place_root(expression) else {
            return;
        };
        for frame in &mut self.lambdas.frames {
            if frame
                .captures
                .get(&name.name)
                .is_some_and(|capture| capture.mode == CaptureMode::Owned)
                && frame.occurrences.contains_key(&key(name.span))
            {
                frame.implicit_moves.insert(key(expression.span));
            }
        }
    }

    pub(super) fn capture_argument(&mut self, expression: &ast::Expression, parameter: &Type) {
        if self.lambdas.frames.is_empty() {
            return;
        }
        match parameter {
            Type::Reference {
                is_mutable: true, ..
            }
            | Type::Pointer {
                is_mutable: true, ..
            } => self.demand_capture(expression, CaptureMode::Mutable),
            Type::Reference { .. } | Type::Pointer { .. } => {
                self.demand_capture(expression, CaptureMode::Shared)
            }
            _ if self.lambda_copy_type(parameter) => {
                self.demand_capture(expression, CaptureMode::Shared)
            }
            _ => self.consume_capture(expression),
        }
    }

    pub(super) fn capture_method_receiver(&mut self, span: Span, parameter: &Type) {
        if let Some(receiver) = self.lambdas.method_receivers.get(&key(span)).cloned() {
            self.capture_argument(&receiver, parameter);
        }
    }

    pub(super) fn demand_capture(&mut self, expression: &ast::Expression, mode: CaptureMode) {
        let Some(name) = place_root(expression) else {
            return;
        };
        let binding_key = (key(name.span), std::ptr::from_ref(name) as usize);
        // Result walks can revisit identifiers after their arm scopes have closed.
        let (scope, ty) = if let Some(binding) = self.lambdas.binding_origins.get(&binding_key) {
            binding.clone()
        } else {
            let Some(binding) = self
                .scopes
                .iter()
                .enumerate()
                .rev()
                .find_map(|(index, scope)| scope.get(&name.name).map(|(ty, _)| (index, ty.clone())))
            else {
                return;
            };
            self.lambdas.binding_origins.insert(binding_key, binding.clone());
            binding
        };
        let mode = if mode == CaptureMode::Owned && self.lambda_copy_type(&ty) {
            CaptureMode::Shared
        } else {
            mode
        };
        for frame in &mut self.lambdas.frames {
            if scope >= frame.scope_start {
                continue;
            }
            let capture = frame
                .captures
                .entry(name.name.clone())
                .or_insert_with(|| Capture {
                    ty: ty.clone(),
                    mode,
                    span: name.span,
                });
            capture.mode = capture.mode.max(mode);
        }
    }

    pub(super) fn observe_lambda_access(&mut self, expression: &ast::Expression) {
        if self.lambdas.frames.is_empty() {
            return;
        }
        use ast::ExpressionKind as E;
        match expression.kind.as_ref() {
            E::Identifier(name) => {
                self.demand_capture(expression, CaptureMode::Shared);
                for frame in &mut self.lambdas.frames {
                    if frame.captures.contains_key(&name.name)
                        && self
                            .scopes
                            .iter()
                            .enumerate()
                            .rev()
                            .find_map(|(index, scope)| {
                                scope.contains_key(&name.name).then_some(index)
                            })
                            .is_some_and(|index| index < frame.scope_start)
                    {
                        frame
                            .occurrences
                            .insert(key(expression.span), name.name.clone());
                    }
                }
            }
            E::Move(value) => self.demand_capture(value, CaptureMode::Owned),
            E::Reference {
                is_mutable: true,
                expression: value,
            } => self.demand_capture(value, CaptureMode::Mutable),
            E::Binary { left, operator, .. }
                if matches!(
                    operator,
                    ast::BinaryOperator::Assign
                        | ast::BinaryOperator::AddAssign
                        | ast::BinaryOperator::SubtractAssign
                        | ast::BinaryOperator::MultiplyAssign
                        | ast::BinaryOperator::DivideAssign
                        | ast::BinaryOperator::ModuloAssign
                        | ast::BinaryOperator::BitwiseAndAssign
                        | ast::BinaryOperator::BitwiseOrAssign
                        | ast::BinaryOperator::BitwiseXorAssign
                        | ast::BinaryOperator::LeftShiftAssign
                        | ast::BinaryOperator::RightShiftAssign
                ) =>
            {
                self.demand_capture(left, CaptureMode::Mutable)
            }
            E::Unary {
                operator: ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement,
                operand,
            }
            | E::Postfix {
                operator: ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement,
                operand,
            } => {
                self.demand_capture(operand, CaptureMode::Mutable);
            }
            E::MethodCall { receiver, .. } => {
                self.lambdas
                    .method_receivers
                    .insert(key(expression.span), receiver.as_ref().clone());
            }
            _ => {}
        }
    }

    pub(super) fn check_lambda(
        &mut self,
        parameters: &[ast::LambdaParameter],
        body: &ast::Block,
        expected: Option<&Type>,
        span: Span,
    ) -> Type {
        if !self.current_type_params.is_empty() {
            self.error(msg::lambda_in_generic_scope(), span);
            return Type::Unknown;
        }
        if let Some(replacement) = self.lambdas.replacements.get(&key(span)) {
            return match replacement.kind.as_ref() {
                ast::ExpressionKind::Identifier(name) => self
                    .function_symbols
                    .values()
                    .find(|sig| sig.source.name.name == name.name)
                    .map(|sig| Type::Function {
                        params: sig.params.clone(),
                        return_type: Box::new(sig.return_type.clone()),
                    })
                    .unwrap_or(Type::Unknown),
                ast::ExpressionKind::StructLiteral { path, .. } => Type::Named {
                    path: path.iter().map(|name| name.name.clone()).collect(),
                    generics: Vec::new(),
                },
                _ => Type::Unknown,
            };
        }
        let expected_signature = match expected {
            Some(Type::Function {
                params,
                return_type,
            }) => Some((params, return_type.as_ref())),
            _ => None,
        };
        let outer_return = self.current_return.take();
        let outer_defer = std::mem::replace(&mut self.defer_depth, 0);
        self.current_return = expected_signature.map(|(_, result)| result.clone());
        self.lambdas.frames.push(CaptureFrame {
            scope_start: self.scopes.len(),
            captures: HashMap::default(),
            occurrences: HashMap::default(),
            implicit_moves: HashSet::default(),
            returns: Vec::new(),
        });
        self.push_scope();
        let mut resolved_parameters = Vec::new();
        for (index, parameter) in parameters.iter().enumerate() {
            let contextual = expected_signature
                .and_then(|(params, _)| params.get(index))
                .cloned();
            let mut ty = parameter
                .type_annotation
                .as_ref()
                .map(|ty| self.type_from_ast(ty))
                .or(contextual)
                .unwrap_or_else(|| {
                    self.error(
                        msg::lambda_parameter_needs_type(&parameter.name.name),
                        parameter.span,
                    );
                    Type::Unknown
                });
            if matches!(
                parameter.mode,
                ast::LambdaParameterMode::Shared | ast::LambdaParameterMode::Mutable
            ) {
                let mutable = parameter.mode == ast::LambdaParameterMode::Mutable;
                if parameter.type_annotation.is_some() || !matches!(ty, Type::Reference { .. }) {
                    ty = Type::Reference {
                        is_mutable: mutable,
                        inner: Box::new(ty),
                    };
                }
            }
            self.bind(&parameter.name.name, ty.clone(), true, parameter.span);
            self.expr_types.insert(
                (parameter.name.span.start, parameter.name.span.end),
                ty.to_string(),
            );
            resolved_parameters.push(ast::Parameter {
                name: parameter.name.clone(),
                param_type: ty.to_ast(),
                is_mutable: true,
                is_variadic: false,
                span: parameter.span,
            });
        }
        self.check_block(body);
        self.pop_scope();
        let Some(frame) = self.lambdas.frames.pop() else {
            return Type::Unknown;
        };
        if self.lambdas.frames.is_empty() {
            // Node addresses are only stable while the enclosing body is borrowed.
            self.lambdas.binding_origins.clear();
        }
        self.current_return = outer_return;
        self.defer_depth = outer_defer;
        let return_type = if frame.returns.is_empty() {
            Type::Unit
        } else {
            expected_signature
                .map(|(_, result)| result.clone())
                .or_else(|| frame.returns.first().cloned())
                .unwrap_or(Type::Unit)
        };
        let return_type = if Self::is_void_like(&return_type) {
            Type::Primitive(ast::PrimitiveType::Void)
        } else {
            return_type
        };
        for returned in &frame.returns {
            if !self.is_assignable(&return_type, returned)
                && !self.is_implicitly_castable(returned, &return_type)
            {
                self.error(msg::return_type_mismatch(&return_type, returned), span);
            }
        }
        let parameter_types: Vec<_> = resolved_parameters
            .iter()
            .map(|parameter| Type::from_ast(&parameter.param_type))
            .collect();
        let mut name = format!("__silver_lambda_{}_{}", span.file, span.start);
        while self.functions.contains_key(&name) || self.known_types.contains_key(&name) {
            name.push('_');
        }
        let mut lowered_body = body.clone();
        visit::expressions_in_block(&mut lowered_body, &mut |expression| {
            if matches!(
                expression.kind.as_ref(),
                ast::ExpressionKind::Lambda { .. } | ast::ExpressionKind::Call { .. }
            ) && let Some(replacement) = self.lambdas.replacements.get(&key(expression.span))
            {
                *expression = replacement.clone();
            }
        });
        if Self::is_void_like(&return_type) {
            normalize_void_returns(&mut lowered_body);
        }
        if frame.captures.is_empty() {
            let function = ast::FunctionItem {
                name: identifier(&name, span),
                generics: None,
                is_variadic: false,
                parameters: resolved_parameters,
                return_type: Some(return_type.to_ast()),
                body: lowered_body,
            };
            let mut symbols = std::mem::take(&mut self.lambdas.symbols);
            self.collect_function_item(&function, false, &mut symbols);
            self.lambdas.symbols = symbols;
            self.lambdas
                .items
                .push(item(ast::ItemKind::Function(function), span));
            self.lambdas
                .replacements
                .insert(key(span), variable(&name, span));
            return Type::Function {
                params: parameter_types,
                return_type: Box::new(return_type),
            };
        }
        if matches!(expected, Some(Type::Function { .. })) {
            self.error(msg::capturing_lambda_not_function_pointer(), span);
        }
        let mut captures: Vec<_> = frame.captures.into_iter().collect();
        captures.sort_by_key(|(_, capture)| (capture.span.file, capture.span.start));
        let mode = captures
            .iter()
            .map(|(_, capture)| capture.mode)
            .max()
            .unwrap_or(CaptureMode::Shared);
        let closure_type = Type::Named {
            path: vec![name.clone()],
            generics: Vec::new(),
        };
        let mut fields = Vec::new();
        let mut initializers = Vec::new();
        let mut replacements = HashMap::default();
        let mut body_names: HashSet<_> = parameters
            .iter()
            .map(|parameter| parameter.name.name.clone())
            .collect();
        visit::expressions_in_block(&mut lowered_body, &mut |expression| {
            if let ast::ExpressionKind::Identifier(name) = expression.kind.as_ref() {
                body_names.insert(name.name.clone());
            }
        });
        let mut environment_name = format!("{name}_env");
        while body_names.contains(&environment_name) {
            environment_name.push('_');
        }
        for (capture_name, capture) in captures {
            let owned = capture.mode == CaptureMode::Owned;
            let ty = if owned {
                capture.ty.clone()
            } else {
                Type::Reference {
                    is_mutable: capture.mode == CaptureMode::Mutable,
                    inner: Box::new(capture.ty.clone()),
                }
            };
            fields.push(ast::Field {
                name: identifier(&capture_name, capture.span),
                field_type: ty.to_ast(),
                visibility: ast::Visibility::Private,
                tags: HashMap::default(),
                span: capture.span,
            });
            let source = variable(&capture_name, capture.span);
            let value = ast::Expression {
                span: capture.span,
                kind: Box::new(if owned {
                    ast::ExpressionKind::Move(Box::new(source))
                } else {
                    ast::ExpressionKind::Reference {
                        is_mutable: capture.mode == CaptureMode::Mutable,
                        expression: Box::new(source),
                    }
                }),
            };
            initializers.push(ast::FieldInit {
                name: identifier(&capture_name, capture.span),
                value,
            });
            let access = ast::Expression {
                span: capture.span,
                kind: Box::new(ast::ExpressionKind::FieldAccess {
                    object: Box::new(variable(&environment_name, span)),
                    field: identifier(&capture_name, capture.span),
                }),
            };
            replacements.insert(
                capture_name,
                if owned {
                    access
                } else {
                    ast::Expression {
                        span: capture.span,
                        kind: Box::new(ast::ExpressionKind::Unary {
                            operator: ast::UnaryOperator::Dereference,
                            operand: Box::new(access),
                        }),
                    }
                },
            );
        }
        visit::expressions_in_block(&mut lowered_body, &mut |expression| {
            if matches!(expression.kind.as_ref(), ast::ExpressionKind::Identifier(_))
                && let Some(name) = frame.occurrences.get(&key(expression.span))
                && let Some(replacement) = replacements.get(name)
            {
                let span = expression.span;
                *expression = replacement.clone();
                expression.span = span;
            }
            if frame.implicit_moves.contains(&key(expression.span)) {
                expression.kind = Box::new(ast::ExpressionKind::Move(Box::new(expression.clone())));
            }
        });
        let receiver_type = if mode == CaptureMode::Owned {
            closure_type.clone()
        } else {
            Type::Reference {
                is_mutable: mode == CaptureMode::Mutable,
                inner: Box::new(closure_type.clone()),
            }
        };
        resolved_parameters.insert(
            0,
            ast::Parameter {
                name: identifier(&environment_name, span),
                param_type: receiver_type.to_ast(),
                is_mutable: true,
                is_variadic: false,
                span,
            },
        );
        let method = ast::ImplFunction {
            name: identifier("__call", span),
            generics: None,
            is_variadic: false,
            parameters: resolved_parameters,
            return_type: Some(return_type.to_ast()),
            body: lowered_body,
            visibility: ast::Visibility::Private,
            method_kind: if mode == CaptureMode::Owned {
                ast::MethodKind::InstanceValue
            } else {
                ast::MethodKind::InstancePointer {
                    is_mutable: mode == CaptureMode::Mutable,
                }
            },
            attributes: Vec::new(),
            span,
        };
        let implementation = ast::ImplItem {
            generics: None,
            trait_ref: None,
            self_type: closure_type.to_ast(),
            items: vec![ast::ImplItemKind::Function(Box::new(method.clone()))],
            implicit_type_params: Vec::new(),
        };
        let definition = ast::StructItem {
            name: identifier(&name, span),
            generics: None,
            fields,
        };
        let type_symbol = self.lambdas.symbols.intern_symbol(
            format!("type::{name}"),
            SymbolKind::Struct,
            Some(span),
            CompilerPhase::TypeCheck,
        );
        self.known_types.insert(name.clone(), type_symbol);
        self.known_type_ids.insert(type_symbol);
        self.struct_defs.insert(
            name.clone(),
            StructDef {
                type_params: Vec::new(),
                fields: definition
                    .fields
                    .iter()
                    .map(|field| (field.name.name.clone(), Type::from_ast(&field.field_type)))
                    .collect(),
            },
        );
        let field_types: Vec<_> = definition
            .fields
            .iter()
            .map(|field| Type::from_ast(&field.field_type))
            .collect();
        let layout = struct_layout(
            &self.type_ctx,
            &field_types,
            &crate::types::StructAttributes::default(),
        );
        self.type_ctx.register_named(&[name.clone()], layout);
        let method_parameters: Vec<_> = method
            .parameters
            .iter()
            .map(|parameter| Type::from_ast(&parameter.param_type))
            .collect();
        let symbol_key =
            self.method_symbol_key(&closure_type, "__call", &method_parameters, &return_type);
        let symbol = self.lambdas.symbols.intern_symbol(
            symbol_key,
            SymbolKind::ImplMethod,
            Some(span),
            CompilerPhase::TypeCheck,
        );
        self.method_symbols.insert(
            symbol,
            MethodSig {
                params: method
                    .parameters
                    .iter()
                    .map(|param| Type::from_ast(&param.param_type))
                    .collect(),
                return_type: return_type.clone(),
                type_params: Vec::new(),
                owner: closure_type.clone(),
                bounds: Vec::new(),
                source_impl: std::sync::Arc::new(implementation.clone()),
                source_method: std::sync::Arc::new(method),
            },
        );
        self.methods
            .insert((name.clone(), "__call".to_string()), vec![symbol]);
        self.lambdas.callable_types.insert(name.clone());
        self.lambdas
            .items
            .push(item(ast::ItemKind::Struct(definition), span));
        self.lambdas
            .items
            .push(item(ast::ItemKind::Impl(implementation), span));
        self.lambdas.replacements.insert(
            key(span),
            ast::Expression {
                span,
                kind: Box::new(ast::ExpressionKind::StructLiteral {
                    path: vec![identifier(&name, span)],
                    fields: initializers,
                }),
            },
        );
        closure_type
    }

    pub(super) fn check_lambda_call(
        &mut self,
        function: &ast::Expression,
        arguments: &[ast::Expression],
        span: Span,
        checked_callee: Option<&Type>,
    ) -> Option<Type> {
        let ty = match function.kind.as_ref() {
            ast::ExpressionKind::Identifier(name) => self.lookup_type(&name.name)?,
            _ => checked_callee?.clone(),
        };
        let Type::Named { path, .. } = &ty else {
            return None;
        };
        if !path
            .last()
            .is_some_and(|name| self.lambdas.callable_types.contains(name))
        {
            return None;
        }
        let call = ast::Expression {
            span,
            kind: Box::new(ast::ExpressionKind::MethodCall {
                receiver: Box::new(function.clone()),
                method: identifier("__call", span),
                arguments: arguments.to_vec(),
            }),
        };
        let result = self.check_expr(&call, None);
        self.lambdas.replacements.insert(key(span), call);
        Some(result)
    }

    /// Materialize checked lambdas before ownership analysis and code generation.
    pub fn lower_lambdas(&mut self, program: &mut ast::Program) {
        if self.lambdas.replacements.is_empty() {
            return;
        }
        program.items.append(&mut self.lambdas.items);
        let mut changed = true;
        while changed {
            changed = false;
            visit::expressions_in_program(program, &mut |expression| {
                if matches!(
                    expression.kind.as_ref(),
                    ast::ExpressionKind::Lambda { .. } | ast::ExpressionKind::Call { .. }
                ) && let Some(replacement) = self.lambdas.replacements.get(&key(expression.span))
                {
                    *expression = replacement.clone();
                    changed = true;
                }
            });
        }
    }
}

fn normalize_void_returns(body: &mut ast::Block) {
    fn normalize_block(block: &mut ast::Block) {
        for statement in &mut block.statements {
            if matches!(statement.kind, ast::StatementKind::Return(Some(_))) {
                let old = std::mem::replace(&mut statement.kind, ast::StatementKind::Return(None));
                if let ast::StatementKind::Return(Some(expression)) = old {
                    statement.kind = ast::StatementKind::Block(ast::Block {
                        statements: vec![
                            ast::Statement {
                                span: expression.span,
                                kind: ast::StatementKind::Expression(expression),
                            },
                            ast::Statement {
                                span: statement.span,
                                kind: ast::StatementKind::Return(None),
                            },
                        ],
                        span: statement.span,
                    });
                }
            }
        }
    }
    visit::expressions_in_block(body, &mut |expression| match expression.kind.as_mut() {
        ast::ExpressionKind::Block(block)
        | ast::ExpressionKind::While { body: block, .. }
        | ast::ExpressionKind::ForIn { body: block, .. }
        | ast::ExpressionKind::For { body: block, .. } => normalize_block(block),
        ast::ExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            normalize_block(then_branch);
            if let Some(block) = else_branch {
                normalize_block(block);
            }
        }
        _ => {}
    });
    normalize_block(body);
}
