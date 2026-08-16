//! Linter and compiler warning passes.
//!
//! Checks Silver AST programs for:
//! 1. Unused local variables and parameters (with `_` prefix ignore support).
//! 2. Unreachable statements following unconditional terminators (return, break, continue).
//!
//! Warnings are filtered to user source files (inlined standard library files
//! do not produce lint warnings).

use crate::diagnostics::messages as msg;
use crate::lexer::{Span, source_file};
use crate::parser::ast;

#[derive(Debug, Clone)]
pub struct LintWarning {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WarningConfig {
    pub unused_variables: bool,
    pub unused_parameters: bool,
    pub unreachable_code: bool,
    pub warnings_as_errors: bool,
}

impl Default for WarningConfig {
    fn default() -> Self {
        Self {
            unused_variables: true,
            unused_parameters: true,
            unreachable_code: true,
            warnings_as_errors: false,
        }
    }
}

impl WarningConfig {
    pub fn from_flags(flags: &[String]) -> Self {
        let mut cfg = Self::default();
        for flag in flags {
            match flag.as_str() {
                "all" | "Wall" | "extra" | "Wextra" => {
                    cfg.unused_variables = true;
                    cfg.unused_parameters = true;
                    cfg.unreachable_code = true;
                }
                "unused" | "Wunused" | "unused-variables" | "Wunused-variables" => {
                    cfg.unused_variables = true;
                    cfg.unused_parameters = true;
                }
                "unreachable-code" | "Wunreachable-code" => {
                    cfg.unreachable_code = true;
                }
                "error" | "Werror" => {
                    cfg.warnings_as_errors = true;
                }
                "no-unused" | "Wno-unused" | "no-unused-variables" | "Wno-unused-variables" => {
                    cfg.unused_variables = false;
                    cfg.unused_parameters = false;
                }
                "no-unreachable-code" | "Wno-unreachable-code" => {
                    cfg.unreachable_code = false;
                }
                "no-error" | "Wno-error" => {
                    cfg.warnings_as_errors = false;
                }
                "no-all" | "Wno-all" => {
                    cfg.unused_variables = false;
                    cfg.unused_parameters = false;
                    cfg.unreachable_code = false;
                }
                _ => {}
            }
        }
        cfg
    }
}

#[derive(Debug, Clone)]
struct Binding {
    name: String,
    span: Span,
    is_param: bool,
    used: bool,
}

struct Linter<'a> {
    config: &'a WarningConfig,
    warnings: Vec<LintWarning>,
    scopes: Vec<Vec<Binding>>,
}

impl<'a> Linter<'a> {
    fn new(config: &'a WarningConfig) -> Self {
        Self {
            config,
            warnings: Vec::new(),
            scopes: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            for b in scope {
                if !b.used && !b.name.starts_with('_') && self.is_user_file(b.span.file) {
                    if b.is_param {
                        if self.config.unused_parameters {
                            self.warnings.push(LintWarning {
                                message: msg::unused_parameter(&b.name),
                                span: b.span,
                            });
                        }
                    } else if self.config.unused_variables {
                        self.warnings.push(LintWarning {
                            message: msg::unused_variable(&b.name),
                            span: b.span,
                        });
                    }
                }
            }
        }
    }

    fn is_user_file(&self, file_id: u32) -> bool {
        if file_id == 0 {
            return true;
        }
        if let Some(sf) = source_file(file_id) {
            !sf.path.contains("/std/") && !sf.path.starts_with("std/")
        } else {
            true
        }
    }

    fn add_binding(&mut self, name: &str, span: Span, is_param: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.push(Binding {
                name: name.to_string(),
                span,
                is_param,
                used: false,
            });
        }
    }

    fn mark_used(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            for b in scope.iter_mut().rev() {
                if b.name == name {
                    b.used = true;
                    return;
                }
            }
        }
    }

    fn lint_program(&mut self, program: &ast::Program) {
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Function(func) => {
                    self.lint_function(func);
                }
                ast::ItemKind::Impl(imp) => {
                    for member in &imp.items {
                        if let ast::ImplItemKind::Function(func) = member {
                            self.lint_impl_function(func);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn lint_function(&mut self, func: &ast::FunctionItem) {
        self.push_scope();
        for param in &func.parameters {
            self.add_binding(&param.name.name, param.name.span, true);
        }
        self.lint_block(&func.body);
        self.pop_scope();
    }

    fn lint_impl_function(&mut self, func: &ast::ImplFunction) {
        self.push_scope();
        for param in &func.parameters {
            // Do not warn on "self" parameter
            if param.name.name != "self" {
                self.add_binding(&param.name.name, param.name.span, true);
            }
        }
        self.lint_block(&func.body);
        self.pop_scope();
    }

    fn lint_block(&mut self, block: &ast::Block) {
        self.push_scope();
        let mut terminated = false;
        let mut warned_unreachable = false;

        for stmt in &block.statements {
            if terminated && self.config.unreachable_code && !warned_unreachable {
                if self.is_user_file(stmt.span.file) {
                    self.warnings.push(LintWarning {
                        message: msg::unreachable_statement().to_string(),
                        span: stmt.span,
                    });
                }
                warned_unreachable = true;
            }

            self.lint_statement(stmt);

            if statement_terminates(stmt) {
                terminated = true;
            }
        }
        self.pop_scope();
    }

    fn lint_let(&mut self, let_stmt: &ast::LetStatement) {
        if let Some(init) = &let_stmt.initializer {
            self.lint_expr(init);
        }
        if let ast::PatternKind::Identifier(ident) = &let_stmt.pattern.kind {
            self.add_binding(&ident.name, ident.span, false);
        }
    }

    fn lint_statement(&mut self, stmt: &ast::Statement) {
        match &stmt.kind {
            ast::StatementKind::Block(block) => {
                self.lint_block(block);
            }
            ast::StatementKind::Let(let_stmt) => {
                self.lint_let(let_stmt);
            }
            ast::StatementKind::Expression(expr) => {
                self.lint_expr(expr);
            }
            ast::StatementKind::Return(Some(expr)) => {
                self.lint_expr(expr);
            }
            ast::StatementKind::Return(None)
            | ast::StatementKind::Break(_)
            | ast::StatementKind::Continue => {}
            ast::StatementKind::Defer(inner) => {
                self.lint_statement(inner);
            }
        }
    }

    fn lint_expr(&mut self, expr: &ast::Expression) {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                self.mark_used(&ident.name);
            }
            ast::ExpressionKind::Literal(_) | ast::ExpressionKind::TypeName(_) => {}
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. }
            | ast::ExpressionKind::Reference {
                expression: operand,
                ..
            }
            | ast::ExpressionKind::Move(operand)
            | ast::ExpressionKind::Cast {
                expression: operand,
                ..
            }
            | ast::ExpressionKind::Comptime(operand)
            | ast::ExpressionKind::Launch(operand)
            | ast::ExpressionKind::Wait(operand) => {
                self.lint_expr(operand);
            }
            ast::ExpressionKind::Binary { left, right, .. } => {
                self.lint_expr(left);
                self.lint_expr(right);
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.lint_expr(condition);
                self.lint_expr(then_expr);
                self.lint_expr(else_expr);
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.lint_expr(condition);
                self.lint_block(then_branch);
                if let Some(else_branch) = else_branch {
                    self.lint_block(else_branch);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                self.lint_expr(condition);
                self.lint_block(body);
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.push_scope();
                self.lint_let(init);
                self.lint_expr(condition);
                self.lint_expr(increment);
                self.lint_block(body);
                self.pop_scope();
            }
            ast::ExpressionKind::ForIn {
                binding,
                iterable,
                body,
                ..
            } => {
                self.lint_expr(iterable);
                self.push_scope();
                self.add_binding(&binding.name, binding.span, false);
                self.lint_block(body);
                self.pop_scope();
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                self.lint_expr(function);
                for arg in arguments {
                    self.lint_expr(arg);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                self.lint_expr(receiver);
                for arg in arguments {
                    self.lint_expr(arg);
                }
            }
            ast::ExpressionKind::FieldAccess { object, .. } => {
                self.lint_expr(object);
            }
            ast::ExpressionKind::Index { object, index } => {
                self.lint_expr(object);
                self.lint_expr(index);
            }
            ast::ExpressionKind::Block(block) => {
                self.lint_block(block);
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.lint_expr(expression);
                for arm in arms {
                    self.push_scope();
                    if let ast::PatternKind::Identifier(ident) = &arm.pattern.kind {
                        self.add_binding(&ident.name, ident.span, false);
                    }
                    self.lint_expr(&arm.body);
                    self.pop_scope();
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for f in fields {
                    self.lint_expr(&f.value);
                }
            }
            ast::ExpressionKind::EnumVariant { fields, .. } => {
                for f in fields {
                    self.lint_expr(f);
                }
            }
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args {
                    if let ast::MacroArg::Expression(e) = arg {
                        self.lint_expr(e);
                    }
                }
            }
            ast::ExpressionKind::Array(elems) | ast::ExpressionKind::Tuple(elems) => {
                for elem in elems {
                    self.lint_expr(elem);
                }
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(e) => self.lint_expr(e),
                        ast::InitializerItem::Field { value, .. } => self.lint_expr(value),
                        ast::InitializerItem::Index { index, value } => {
                            self.lint_expr(index);
                            self.lint_expr(value);
                        }
                    }
                }
            }
            ast::ExpressionKind::Asm { inputs, .. } => {
                for input in inputs {
                    self.lint_expr(input);
                }
            }
        }
    }
}

fn statement_terminates(stmt: &ast::Statement) -> bool {
    match &stmt.kind {
        ast::StatementKind::Return(_)
        | ast::StatementKind::Break(_)
        | ast::StatementKind::Continue => true,
        ast::StatementKind::Expression(expr) => match expr.kind.as_ref() {
            ast::ExpressionKind::Call { function, .. } => {
                matches!(function.kind.as_ref(), ast::ExpressionKind::Identifier(id) if id.name == "abort")
            }
            _ => false,
        },
        _ => false,
    }
}

/// Run all lint warning checks on `program`.
pub fn lint_program(program: &ast::Program, config: &WarningConfig) -> Vec<LintWarning> {
    let mut linter = Linter::new(config);
    linter.lint_program(program);
    linter.warnings
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> ast::Program {
        let tokens = crate::lexer::lex(source).expect("lex failed");
        let mut parser = crate::parser::Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    #[test]
    fn warns_on_unused_variable() {
        let program = parse("i32 f() { i32 unused_val = 42; return 0; }");
        let warnings = lint_program(&program, &WarningConfig::default());
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("unused variable 'unused_val'"));
    }

    #[test]
    fn ignores_underscored_variable() {
        let program = parse("i32 f() { i32 _unused = 42; return 0; }");
        let warnings = lint_program(&program, &WarningConfig::default());
        assert!(warnings.is_empty());
    }

    #[test]
    fn warns_on_unused_parameter() {
        let program = parse("i32 f(i32 unused_param) { return 0; }");
        let warnings = lint_program(&program, &WarningConfig::default());
        assert_eq!(warnings.len(), 1);
        assert!(
            warnings[0]
                .message
                .contains("unused parameter 'unused_param'")
        );
    }

    #[test]
    fn warns_on_unreachable_statement() {
        let program = parse("i32 f() { return 0; i32 _x = 1; }");
        let warnings = lint_program(&program, &WarningConfig::default());
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("unreachable statement"));
    }

    #[test]
    fn test_warning_config_flags() {
        let cfg = WarningConfig::from_flags(&["no-unused".to_string(), "error".to_string()]);
        assert!(!cfg.unused_variables);
        assert!(!cfg.unused_parameters);
        assert!(cfg.warnings_as_errors);

        let cfg_all = WarningConfig::from_flags(&["Wall".to_string(), "Werror".to_string()]);
        assert!(cfg_all.unused_variables);
        assert!(cfg_all.unreachable_code);
        assert!(cfg_all.warnings_as_errors);
    }
}
