use std::collections::{HashMap, HashSet};

use crate::lexer::{LexToken, Span, Token};

use super::ast;
use super::error::ParseError;

#[derive(Debug, Clone)]
pub enum GrammarAtom {
    Token(Token),
    Rule(String),
}

#[derive(Debug, Clone)]
pub enum GrammarExpr {
    Seq(Vec<GrammarExpr>), // Simple Sequence as [1, 2, ,3] => 1 then 2 then 3
    Choice(Vec<GrammarExpr>), // Choice of Rules [Import, Fucntion, Definition] => 1 | 2 | 3
    Optional(Box<GrammarExpr>), // Optional, pretty self explanatory
    Repeat(Box<GrammarExpr>), // Klein Closure Repeat => 0 or more times
    Repeat1(Box<GrammarExpr>), // + Closure Releat => 1 or more times
    Atom(GrammarAtom), // Basic Unit,
}

pub fn tok(token: Token) -> GrammarExpr {
    GrammarExpr::Atom(GrammarAtom::Token(token))
}

pub fn rule(name: impl Into<String>) -> GrammarExpr {
    GrammarExpr::Atom(GrammarAtom::Rule(name.into()))
}

pub fn seq(parts: Vec<GrammarExpr>) -> GrammarExpr {
    GrammarExpr::Seq(parts)
}

pub fn choice(parts: Vec<GrammarExpr>) -> GrammarExpr {
    GrammarExpr::Choice(parts)
}

pub fn opt(part: GrammarExpr) -> GrammarExpr {
    GrammarExpr::Optional(Box::new(part))
}

pub fn repeat(part: GrammarExpr) -> GrammarExpr {
    GrammarExpr::Repeat(Box::new(part))
}

pub fn repeat1(part: GrammarExpr) -> GrammarExpr {
    GrammarExpr::Repeat1(Box::new(part))
}

pub fn sep1(item: GrammarExpr, separator: GrammarExpr) -> GrammarExpr {
    seq(vec![item.clone(), repeat(seq(vec![separator, item]))])
}

#[derive(Debug, Clone)]
pub struct GrammarRule {
    pub name: String,
    pub expr: GrammarExpr,
}

#[derive(Debug, Clone)]
pub struct GrammarGraph {
    pub rules: HashMap<String, GrammarRule>,
}

impl GrammarGraph {
    pub fn new() -> Self {
        Self {
            rules: HashMap::new(),
        }
    }

    pub fn add_rule(&mut self, name: impl Into<String>, expr: GrammarExpr) {
        let name = name.into();
        self.rules.insert(name.clone(), GrammarRule { name, expr });
    }

    pub fn compile_rule(&self, name: &str) -> Result<CompiledRuleGraph, String> {
        let rule = self
            .rules
            .get(name)
            .ok_or_else(|| format!("unknown grammar rule `{name}`"))?;
        let mut builder = GraphBuilder::new();
        let start = builder.new_state();
        let accept = builder.new_state();
        builder.compile_expr(&rule.expr, start, accept);
        Ok(builder.finish(rule.name.clone(), start, accept))
    }
}

#[derive(Debug, Clone)]
pub struct CompiledRuleGraph {
    pub rule_name: String,
    pub start_state: usize,
    pub accept_state: usize,
    pub states: Vec<CompiledState>,
}

#[derive(Debug, Clone)]
pub struct CompiledState {
    pub transitions: Vec<Transition>,
}

#[derive(Debug, Clone)]
pub enum Transition {
    Epsilon { to: usize },
    MatchToken { token: Token, to: usize },
    EnterRule { rule_name: String, to: usize },
}

struct GraphBuilder {
    states: Vec<CompiledState>,
}

impl GraphBuilder {
    fn new() -> Self {
        Self { states: Vec::new() }
    }

    fn finish(
        self,
        rule_name: String,
        start_state: usize,
        accept_state: usize,
    ) -> CompiledRuleGraph {
        CompiledRuleGraph {
            rule_name,
            start_state,
            accept_state,
            states: self.states,
        }
    }

    fn new_state(&mut self) -> usize {
        let id = self.states.len();
        self.states.push(CompiledState {
            transitions: Vec::new(),
        });
        id
    }

    fn add_transition(&mut self, from: usize, transition: Transition) {
        if let Some(state) = self.states.get_mut(from) {
            state.transitions.push(transition);
        }
    }

    fn compile_expr(&mut self, expr: &GrammarExpr, start: usize, end: usize) {
        match expr {
            GrammarExpr::Atom(atom) => match atom {
                GrammarAtom::Token(token) => self.add_transition(
                    start,
                    Transition::MatchToken {
                        token: token.clone(),
                        to: end,
                    },
                ),
                GrammarAtom::Rule(rule_name) => self.add_transition(
                    start,
                    Transition::EnterRule {
                        rule_name: rule_name.clone(),
                        to: end,
                    },
                ),
            },
            GrammarExpr::Seq(parts) => {
                if parts.is_empty() {
                    self.add_transition(start, Transition::Epsilon { to: end });
                    return;
                }

                let mut current_start = start;
                for (index, part) in parts.iter().enumerate() {
                    let current_end = if index + 1 == parts.len() {
                        end
                    } else {
                        self.new_state()
                    };
                    self.compile_expr(part, current_start, current_end);
                    current_start = current_end;
                }
            }
            GrammarExpr::Choice(parts) => {
                for part in parts {
                    self.compile_expr(part, start, end);
                }
            }
            GrammarExpr::Optional(part) => {
                self.add_transition(start, Transition::Epsilon { to: end });
                self.compile_expr(part, start, end);
            }
            GrammarExpr::Repeat(part) => {
                self.add_transition(start, Transition::Epsilon { to: end });
                let loop_end = self.new_state();
                self.compile_expr(part, start, loop_end);
                self.add_transition(loop_end, Transition::Epsilon { to: start });
                self.add_transition(loop_end, Transition::Epsilon { to: end });
            }
            GrammarExpr::Repeat1(part) => {
                let loop_entry = self.new_state();
                self.compile_expr(part, start, loop_entry);
                self.compile_expr(&GrammarExpr::Repeat(part.clone()), loop_entry, end);
            }
        }
    }
}

pub struct GraphParser {
    source_name: Option<String>,
    grammar: GrammarGraph,
    compiled_rules: HashMap<String, CompiledRuleGraph>,
    memo: HashMap<(String, usize), Option<usize>>,
}

enum ParsedExternDeclaration {
    Function(ast::ExternFunctionItem),
    Variable(ast::ExternVariableItem),
}

impl GraphParser {
    pub fn new(source_name: Option<String>) -> Self {
        let mut parser = Self {
            source_name,
            grammar: GrammarGraph::new(),
            compiled_rules: HashMap::new(),
            memo: HashMap::new(),
        };
        parser.install_bootstrap_grammar();
        parser
    }

    pub fn grammar_mut(&mut self) -> &mut GrammarGraph {
        &mut self.grammar
    }

    pub fn render_grammar_tree(&mut self) -> Result<String, String> {
        let mut lines = vec!["GrammarGraph".to_string()];
        let mut rule_names = self.grammar.rules.keys().cloned().collect::<Vec<_>>();
        rule_names.sort();

        for (rule_index, rule_name) in rule_names.iter().enumerate() {
            let compiled = self.rule_graph(rule_name)?.clone();
            let rule_branch = if rule_index + 1 == rule_names.len() {
                "`-- "
            } else {
                "|-- "
            };
            lines.push(format!(
                "{rule_branch}Rule {} [start={}, accept={}]",
                compiled.rule_name, compiled.start_state, compiled.accept_state
            ));

            let rule_prefix = if rule_index + 1 == rule_names.len() {
                "    "
            } else {
                "|   "
            };

            for (state_index, state) in compiled.states.iter().enumerate() {
                let state_last = state_index + 1 == compiled.states.len();
                let state_branch = if state_last { "`-- " } else { "|-- " };
                lines.push(format!(
                    "{rule_prefix}{state_branch}State {state_index}"
                ));

                let state_prefix = if state_last {
                    format!("{rule_prefix}    ")
                } else {
                    format!("{rule_prefix}|   ")
                };

                if state.transitions.is_empty() {
                    lines.push(format!("{state_prefix}`-- <no transitions>"));
                    continue;
                }

                for (trans_index, trans) in state.transitions.iter().enumerate() {
                    let trans_last = trans_index + 1 == state.transitions.len();
                    let trans_branch = if trans_last { "`-- " } else { "|-- " };
                    lines.push(format!(
                        "{state_prefix}{trans_branch}{}",
                        Self::transition_label(trans)
                    ));
                }
            }
        }

        Ok(lines.join("\n"))
    }

    pub fn render_grammar_pretty(&self) -> String {
        let mut lines = vec!["Grammar".to_string()];
        let mut rule_names = self.grammar.rules.keys().cloned().collect::<Vec<_>>();
        rule_names.sort();

        for rule_name in rule_names {
            let Some(rule) = self.grammar.rules.get(&rule_name) else {
                continue;
            };
            let alternatives = Self::rule_alternatives(&rule.expr);
            if alternatives.is_empty() {
                lines.push(format!("{rule_name} -> <empty> = {rule_name}"));
                continue;
            }

            let head = format!("{rule_name} -> ");
            let indent = " ".repeat(head.len());

            for (index, alt) in alternatives.iter().enumerate() {
                let rendered = Self::format_expr(alt);
                if index == 0 {
                    lines.push(format!("{head}{rendered} = {rule_name}"));
                } else {
                    lines.push(format!("{indent}| - {rendered} = {rule_name}"));
                }
            }
        }

        lines.join("\n")
    }

    fn install_bootstrap_grammar(&mut self) {
        // Program := Item*
        self.grammar.add_rule("Program", repeat(rule("Item")));

        // Item := Import | Function | GlobalVariable
        self.grammar.add_rule(
            "Item",
            choice(vec![rule("Import"), rule("Function"), rule("GlobalVariable")]),
        );

        // Import := import Identifier ('.' Identifier)* ';'
        self.grammar.add_rule(
            "Import",
            seq(vec![
                tok(Token::Import),
                tok(Token::Identifier(String::new())),
                repeat(seq(vec![
                    tok(Token::Dot),
                    tok(Token::Identifier(String::new())),
                ])),
                tok(Token::Semicolon),
            ]),
        );

        // Function := Type Identifier '(' ParamList? ')' Block
        self.grammar.add_rule(
            "Function",
            seq(vec![
                rule("Type"),
                tok(Token::Identifier(String::new())),
                tok(Token::LeftParen),
                opt(rule("ParamList")),
                tok(Token::RightParen),
                rule("Block"),
            ]),
        );

        // GlobalVariable := Type Identifier ('=' Expression)? ';'
        self.grammar.add_rule(
            "GlobalVariable",
            seq(vec![
                rule("Type"),
                tok(Token::Identifier(String::new())),
                opt(seq(vec![tok(Token::Assign), rule("Expression")])),
                tok(Token::Semicolon),
            ]),
        );

        // ParamList := Param (',' Param)*
        self.grammar
            .add_rule("ParamList", sep1(rule("Param"), tok(Token::Comma)));

        // Param := Type Identifier
        self.grammar.add_rule(
            "Param",
            seq(vec![rule("Type"), tok(Token::Identifier(String::new()))]),
        );

        // Type := primitive | identifier
        self.grammar.add_rule(
            "Type",
            seq(vec![
                opt(tok(Token::Const)),
                choice(vec![
                    tok(Token::I8),
                    tok(Token::I16),
                    tok(Token::I32),
                    tok(Token::I64),
                    tok(Token::I128),
                    tok(Token::U8),
                    tok(Token::U16),
                    tok(Token::U32),
                    tok(Token::U64),
                    tok(Token::U128),
                    tok(Token::F32),
                    tok(Token::F64),
                    tok(Token::F80),
                    tok(Token::Bool),
                    tok(Token::Str),
                    tok(Token::Char),
                    tok(Token::Void),
                    tok(Token::Identifier(String::new())),
                ]),
                repeat(tok(Token::Star)),
            ]),
        );

        // Block := '{' Statement* '}'
        self.grammar.add_rule(
            "Block",
            seq(vec![
                tok(Token::LeftBrace),
                repeat(rule("Statement")),
                tok(Token::RightBrace),
            ]),
        );

        // Statement := ReturnStmt | ExprStmt
        self.grammar.add_rule(
            "Statement",
            choice(vec![rule("ReturnStmt"), rule("ExprStmt")]),
        );

        // ReturnStmt := return Expression? ';'
        self.grammar.add_rule(
            "ReturnStmt",
            seq(vec![
                tok(Token::Return),
                opt(rule("Expression")),
                tok(Token::Semicolon),
            ]),
        );

        // ExprStmt := Expression ';'
        self.grammar
            .add_rule("ExprStmt", seq(vec![rule("Expression"), tok(Token::Semicolon)]));

        // Expression := Primary ('(' ArgList? ')')*
        self.grammar.add_rule(
            "Expression",
            seq(vec![
                rule("Primary"),
                repeat(seq(vec![
                    tok(Token::LeftParen),
                    opt(rule("ArgList")),
                    tok(Token::RightParen),
                ])),
            ]),
        );

        // ArgList := Expression (',' Expression)*
        self.grammar
            .add_rule("ArgList", sep1(rule("Expression"), tok(Token::Comma)));

        // Primary := identifier | literal | '(' Expression ')'
        self.grammar.add_rule(
            "Primary",
            choice(vec![
                tok(Token::Identifier(String::new())),
                tok(Token::IntLiteral(0)),
                tok(Token::FloatLiteral(0.0)),
                tok(Token::StringLiteral(String::new())),
                tok(Token::CharLiteral('\0')),
                tok(Token::True),
                tok(Token::False),
                seq(vec![tok(Token::LeftParen), rule("Expression"), tok(Token::RightParen)]),
            ]),
        );
    }

    fn rule_graph(&mut self, name: &str) -> Result<&CompiledRuleGraph, String> {
        if !self.compiled_rules.contains_key(name) {
            let compiled = self.grammar.compile_rule(name)?;
            self.compiled_rules.insert(name.to_string(), compiled);
        }
        self.compiled_rules
            .get(name)
            .ok_or_else(|| format!("failed to cache compiled rule `{name}`"))
    }

    fn token_label(token: &Token) -> String {
        match token {
            Token::Identifier(_) => "ID".to_string(),
            Token::IntLiteral(_) => "IntLiteral".to_string(),
            Token::FloatLiteral(_) => "FloatLiteral".to_string(),
            Token::ComplexLiteral(_, _) => "ComplexLiteral".to_string(),
            Token::StringLiteral(_) => "StringLiteral".to_string(),
            Token::CharLiteral(_) => "CharLiteral".to_string(),
            Token::BoolLiteral(_) => "BoolLiteral".to_string(),
            Token::LeftParen => "LParen".to_string(),
            Token::RightParen => "RParen".to_string(),
            Token::LeftBrace => "LBrace".to_string(),
            Token::RightBrace => "RBrace".to_string(),
            Token::Semicolon => "Semicolon".to_string(),
            Token::Comma => "Comma".to_string(),
            Token::Dot => "Dot".to_string(),
            Token::Star => "Star".to_string(),
            _ => format!("{token:?}"),
        }
    }

    fn transition_label(transition: &Transition) -> String {
        match transition {
            Transition::Epsilon { to } => format!("epsilon -> {to}"),
            Transition::MatchToken { token, to } => {
                format!("match {} -> {to}", Self::token_label(token))
            }
            Transition::EnterRule { rule_name, to } => {
                format!("enter {rule_name} -> {to}")
            }
        }
    }

    fn rule_alternatives<'a>(expr: &'a GrammarExpr) -> Vec<&'a GrammarExpr> {
        match expr {
            GrammarExpr::Choice(parts) => parts.iter().collect(),
            _ => vec![expr],
        }
    }

    fn format_expr(expr: &GrammarExpr) -> String {
        match expr {
            GrammarExpr::Seq(parts) => {
                if parts.is_empty() {
                    return "<empty>".to_string();
                }
                parts
                    .iter()
                    .map(Self::format_expr_atom)
                    .collect::<Vec<_>>()
                    .join(" -> ")
            }
            _ => Self::format_expr_atom(expr),
        }
    }

    fn format_expr_atom(expr: &GrammarExpr) -> String {
        match expr {
            GrammarExpr::Atom(GrammarAtom::Token(token)) => Self::token_label(token),
            GrammarExpr::Atom(GrammarAtom::Rule(name)) => name.clone(),
            GrammarExpr::Seq(parts) => {
                if parts.is_empty() {
                    "<empty>".to_string()
                } else {
                    let inner = parts
                        .iter()
                        .map(Self::format_expr_atom)
                        .collect::<Vec<_>>()
                        .join(" -> ");
                    format!("({inner})")
                }
            }
            GrammarExpr::Choice(parts) => {
                let inner = parts
                    .iter()
                    .map(Self::format_expr_atom)
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!("({inner})")
            }
            GrammarExpr::Optional(inner) => format!("{}?", Self::format_expr_atom(inner)),
            GrammarExpr::Repeat(inner) => format!("{}*", Self::format_expr_atom(inner)),
            GrammarExpr::Repeat1(inner) => format!("{}+", Self::format_expr_atom(inner)),
        }
    }

    fn token_matches(pattern: &Token, actual: &Token) -> bool {
        match (pattern, actual) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::IntLiteral(_), Token::IntLiteral(_)) => true,
            (Token::FloatLiteral(_), Token::FloatLiteral(_)) => true,
            (Token::ComplexLiteral(_, _), Token::ComplexLiteral(_, _)) => true,
            (Token::StringLiteral(_), Token::StringLiteral(_)) => true,
            (Token::CharLiteral(_), Token::CharLiteral(_)) => true,
            (Token::BoolLiteral(_), Token::BoolLiteral(_)) => true,
            _ => std::mem::discriminant(pattern) == std::mem::discriminant(actual),
        }
    }

    fn parse_rule_longest(&mut self, rule_name: &str, start: usize, tokens: &[LexToken]) -> Option<usize> {
        if let Some(cached) = self.memo.get(&(rule_name.to_string(), start)).cloned() {
            return cached;
        }

        let compiled = self.rule_graph(rule_name).ok()?.clone();
        let mut queue = vec![(compiled.start_state, start)];
        let mut visited: HashSet<(usize, usize)> = HashSet::new();
        let mut best: Option<usize> = None;

        while let Some((state_id, position)) = queue.pop() {
            if !visited.insert((state_id, position)) {
                continue;
            }

            if state_id == compiled.accept_state {
                best = Some(best.map(|current| current.max(position)).unwrap_or(position));
                continue;
            }

            let Some(state) = compiled.states.get(state_id) else {
                continue;
            };

            for transition in &state.transitions {
                match transition {
                    Transition::Epsilon { to } => queue.push((*to, position)),
                    Transition::MatchToken { token, to } => {
                        if let Some(actual) = tokens.get(position).map(|t| &t.kind)
                            && Self::token_matches(token, actual)
                        {
                            queue.push((*to, position + 1));
                        }
                    }
                    Transition::EnterRule {
                        rule_name: inner,
                        to,
                    } => {
                        if let Some(end) = self.parse_rule_longest(inner, position, tokens) {
                            queue.push((*to, end));
                        }
                    }
                }
            }
        }

        self.memo.insert((rule_name.to_string(), start), best);
        best
    }

    fn at_eof(tokens: &[LexToken], position: usize) -> bool {
        if position >= tokens.len() {
            return true;
        }
        matches!(tokens[position].kind, Token::Eof)
    }

    fn parse_base_type_from_token(token: &LexToken) -> Option<ast::Type> {
        let primitive = match token.kind {
            Token::I8 => Some(ast::PrimitiveType::I8),
            Token::I16 => Some(ast::PrimitiveType::I16),
            Token::I32 => Some(ast::PrimitiveType::I32),
            Token::I64 => Some(ast::PrimitiveType::I64),
            Token::I128 => Some(ast::PrimitiveType::I128),
            Token::U8 => Some(ast::PrimitiveType::U8),
            Token::U16 => Some(ast::PrimitiveType::U16),
            Token::U32 => Some(ast::PrimitiveType::U32),
            Token::U64 => Some(ast::PrimitiveType::U64),
            Token::U128 => Some(ast::PrimitiveType::U128),
            Token::F32 => Some(ast::PrimitiveType::F32),
            Token::F64 => Some(ast::PrimitiveType::F64),
            Token::F80 => Some(ast::PrimitiveType::F80),
            Token::Bool => Some(ast::PrimitiveType::Bool),
            Token::Str => Some(ast::PrimitiveType::Str),
            Token::Char => Some(ast::PrimitiveType::Char),
            Token::Void => Some(ast::PrimitiveType::Void),
            _ => None,
        };

        if let Some(primitive) = primitive {
            return Some(ast::Type {
                kind: Box::new(ast::TypeKind::Primitive(primitive)),
                span: token.span.clone(),
            });
        }

        if let Token::Identifier(ref name) = token.kind {
            return Some(ast::Type {
                kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                    path: vec![ast::Identifier {
                        name: name.clone(),
                        span: token.span.clone(),
                    }],
                    generics: None,
                })),
                span: token.span.clone(),
            });
        }

        None
    }

    fn parse_type_prefix(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Option<(ast::Type, usize)> {
        if start >= end {
            return None;
        }

        let mut cursor = start;
        let mut is_const = false;
        if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Const)) {
            is_const = true;
            cursor += 1;
            if cursor >= end {
                return None;
            }
        }

        let mut ty = if matches!(tokens[cursor].kind, Token::LeftBracket) {
            let (element_type, after_element) = self.parse_type_prefix(tokens, cursor + 1, end)?;
            if !matches!(tokens.get(after_element).map(|t| &t.kind), Some(Token::Semicolon)) {
                return None;
            }
            let size_start = after_element + 1;
            let Some(size_end) = self.find_matching_token(
                tokens,
                cursor,
                end,
                Token::LeftBracket,
                Token::RightBracket,
            ) else {
                return None;
            };
            if size_start > size_end {
                return None;
            }
            let size = self
                .parse_expression_reduction(tokens, size_start, size_end)
                .ok()
                .map(Box::new);
            cursor = size_end + 1;
            ast::Type {
                kind: Box::new(ast::TypeKind::Array(Box::new(ast::ArrayType {
                    element_type: Box::new(element_type),
                    size,
                }))),
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[size_end].span.end,
                },
            }
        } else {
            let mut path = Vec::new();
            let base_start = cursor;
            if let Token::Identifier(name) = &tokens[cursor].kind {
                path.push(ast::Identifier {
                    name: name.clone(),
                    span: tokens[cursor].span.clone(),
                });
                cursor += 1;
                while cursor + 1 < end && matches!(tokens[cursor].kind, Token::DoubleColon) {
                    cursor += 1;
                    let Token::Identifier(seg) = &tokens[cursor].kind else {
                        return None;
                    };
                    path.push(ast::Identifier {
                        name: seg.clone(),
                        span: tokens[cursor].span.clone(),
                    });
                    cursor += 1;
                }
            }

            if !path.is_empty() {
                let mut generics = None;
                if cursor < end && matches!(tokens[cursor].kind, Token::Less) {
                    let close = self.find_matching_token(
                        tokens,
                        cursor,
                        end,
                        Token::Less,
                        Token::Greater,
                    )?;
                    let mut args = Vec::new();
                    let mut arg_cursor = cursor + 1;
                    while arg_cursor < close {
                        let (arg, next_arg) = self.parse_type_prefix(tokens, arg_cursor, close)?;
                        args.push(arg);
                        arg_cursor = next_arg;
                        if arg_cursor < close {
                            if !matches!(tokens[arg_cursor].kind, Token::Comma) {
                                return None;
                            }
                            arg_cursor += 1;
                        }
                    }
                    generics = Some(args);
                    cursor = close + 1;
                }

                ast::Type {
                    kind: Box::new(ast::TypeKind::Named(ast::NamedType { path, generics })),
                    span: Span {
                        start: tokens[base_start].span.start,
                        end: tokens[cursor - 1].span.end,
                    },
                }
            } else {
                let base = Self::parse_base_type_from_token(tokens.get(cursor)?)?;
                cursor += 1;
                base
            }
        };

        while cursor < end && matches!(tokens[cursor].kind, Token::Star) {
            let is_mutable = !is_const;
            ty = ast::Type {
                kind: Box::new(ast::TypeKind::Pointer(ast::PointerType {
                    is_mutable,
                    inner: Box::new(ty),
                })),
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[cursor].span.end,
                },
            };
            cursor += 1;
        }

        Some((ty, cursor))
    }

    fn find_matching_token(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
        open: Token,
        close: Token,
    ) -> Option<usize> {
        if start >= end {
            return None;
        }
        if !Self::token_matches(&open, &tokens[start].kind) {
            return None;
        }
        let mut depth = 0usize;
        for (idx, token) in tokens.iter().enumerate().take(end).skip(start) {
            if Self::token_matches(&open, &token.kind) {
                depth += 1;
            } else if Self::token_matches(&close, &token.kind) {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
        }
        None
    }

    fn find_statement_terminator(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Option<usize> {
        let mut paren = 0usize;
        let mut bracket = 0usize;
        let mut brace = 0usize;
        for (idx, token) in tokens.iter().enumerate().take(end).skip(start) {
            match token.kind {
                Token::LeftParen => paren += 1,
                Token::RightParen => {
                    paren = paren.saturating_sub(1);
                }
                Token::LeftBracket => bracket += 1,
                Token::RightBracket => {
                    bracket = bracket.saturating_sub(1);
                }
                Token::LeftBrace => brace += 1,
                Token::RightBrace => {
                    brace = brace.saturating_sub(1);
                }
                Token::Semicolon if paren == 0 && bracket == 0 && brace == 0 => return Some(idx),
                _ => {}
            }
        }
        None
    }

    fn find_semicolon_in_range(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Option<usize> {
        let mut paren = 0usize;
        let mut bracket = 0usize;
        let mut brace = 0usize;
        for (idx, token) in tokens.iter().enumerate().take(end).skip(start) {
            match token.kind {
                Token::LeftParen => paren += 1,
                Token::RightParen => paren = paren.saturating_sub(1),
                Token::LeftBracket => bracket += 1,
                Token::RightBracket => bracket = bracket.saturating_sub(1),
                Token::LeftBrace => brace += 1,
                Token::RightBrace => brace = brace.saturating_sub(1),
                Token::Semicolon if paren == 0 && bracket == 0 && brace == 0 => return Some(idx),
                _ => {}
            }
        }
        None
    }

    fn parse_if_statement_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::Statement, ParseError> {
        let cond_open = start + 1;
        let Some(cond_close) =
            self.find_matching_token(tokens, cond_open, end, Token::LeftParen, Token::RightParen)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "unterminated if condition".to_string(),
                span: tokens[start].span.clone(),
            });
        };
        let condition = self.parse_expression_reduction(tokens, cond_open + 1, cond_close)?;

        let then_start = cond_close + 1;
        let Some(then_end_inclusive) =
            self.find_matching_token(tokens, then_start, end, Token::LeftBrace, Token::RightBrace)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "missing if block".to_string(),
                span: tokens[then_start.min(end - 1)].span.clone(),
            });
        };
        let then_end = then_end_inclusive + 1;
        let then_branch = self.parse_block_reduction(tokens, then_start, then_end)?;

        let mut else_branch = None;
        let mut cursor = then_end;
        if cursor < end && matches!(tokens[cursor].kind, Token::Else) {
            cursor += 1;
            let Some(else_end_inclusive) =
                self.find_matching_token(tokens, cursor, end, Token::LeftBrace, Token::RightBrace)
            else {
                return Err(ParseError::InvalidSyntax {
                    message: "missing else block".to_string(),
                    span: tokens[cursor.min(end - 1)].span.clone(),
                });
            };
            let else_end = else_end_inclusive + 1;
            else_branch = Some(self.parse_block_reduction(tokens, cursor, else_end)?);
            cursor = else_end;
        }

        if cursor != end {
            return Err(ParseError::InvalidSyntax {
                message: "unexpected tokens after if statement".to_string(),
                span: tokens[cursor].span.clone(),
            });
        }

        Ok(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression {
                kind: Box::new(ast::ExpressionKind::If {
                    condition: Box::new(condition),
                    then_branch,
                    else_branch,
                }),
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[end - 1].span.end,
                },
            }),
            span: Span {
                start: tokens[start].span.start,
                end: tokens[end - 1].span.end,
            },
        })
    }

    fn parse_while_statement_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::Statement, ParseError> {
        let cond_open = start + 1;
        let Some(cond_close) =
            self.find_matching_token(tokens, cond_open, end, Token::LeftParen, Token::RightParen)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "unterminated while condition".to_string(),
                span: tokens[start].span.clone(),
            });
        };
        let condition = self.parse_expression_reduction(tokens, cond_open + 1, cond_close)?;

        let body_start = cond_close + 1;
        let Some(body_end_inclusive) =
            self.find_matching_token(tokens, body_start, end, Token::LeftBrace, Token::RightBrace)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "missing while block".to_string(),
                span: tokens[body_start.min(end - 1)].span.clone(),
            });
        };
        let body_end = body_end_inclusive + 1;
        if body_end != end {
            return Err(ParseError::InvalidSyntax {
                message: "unexpected tokens after while statement".to_string(),
                span: tokens[body_end].span.clone(),
            });
        }

        let body = self.parse_block_reduction(tokens, body_start, body_end)?;
        Ok(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression {
                kind: Box::new(ast::ExpressionKind::While {
                    condition: Box::new(condition),
                    body,
                }),
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[end - 1].span.end,
                },
            }),
            span: Span {
                start: tokens[start].span.start,
                end: tokens[end - 1].span.end,
            },
        })
    }

    fn parse_for_statement_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::Statement, ParseError> {
        let header_open = start + 1;
        let Some(header_close) =
            self.find_matching_token(tokens, header_open, end, Token::LeftParen, Token::RightParen)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "unterminated for header".to_string(),
                span: tokens[start].span.clone(),
            });
        };

        let semicolon1 = self
            .find_semicolon_in_range(tokens, header_open + 1, header_close)
            .ok_or_else(|| ParseError::InvalidSyntax {
                message: "missing first ';' in for header".to_string(),
                span: tokens[start].span.clone(),
            })?;
        let semicolon2 = self
            .find_semicolon_in_range(tokens, semicolon1 + 1, header_close)
            .ok_or_else(|| ParseError::InvalidSyntax {
                message: "missing second ';' in for header".to_string(),
                span: tokens[start].span.clone(),
            })?;

        let init_stmt = self.parse_statement_reduction(tokens, header_open + 1, semicolon1 + 1)?;
        let ast::StatementKind::Let(init) = init_stmt.kind else {
            return Err(ParseError::InvalidSyntax {
                message: "for init must be a declaration in bootstrap parser".to_string(),
                span: init_stmt.span,
            });
        };

        let condition = if semicolon1 + 1 < semicolon2 {
            self.parse_expression_reduction(tokens, semicolon1 + 1, semicolon2)?
        } else {
            ast::Expression {
                kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Bool(true))),
                span: tokens[semicolon1].span.clone(),
            }
        };

        let increment = if semicolon2 + 1 < header_close {
            self.parse_expression_reduction(tokens, semicolon2 + 1, header_close)?
        } else {
            ast::Expression {
                kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(0))),
                span: tokens[semicolon2].span.clone(),
            }
        };

        let body_start = header_close + 1;
        let Some(body_end_inclusive) =
            self.find_matching_token(tokens, body_start, end, Token::LeftBrace, Token::RightBrace)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "missing for block".to_string(),
                span: tokens[body_start.min(end - 1)].span.clone(),
            });
        };
        let body_end = body_end_inclusive + 1;
        if body_end != end {
            return Err(ParseError::InvalidSyntax {
                message: "unexpected tokens after for statement".to_string(),
                span: tokens[body_end].span.clone(),
            });
        }
        let body = self.parse_block_reduction(tokens, body_start, body_end)?;

        Ok(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression {
                kind: Box::new(ast::ExpressionKind::For {
                    init,
                    condition: Box::new(condition),
                    increment: Box::new(increment),
                    body,
                }),
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[end - 1].span.end,
                },
            }),
            span: Span {
                start: tokens[start].span.start,
                end: tokens[end - 1].span.end,
            },
        })
    }

    fn parse_expression_reduction(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::Expression, ParseError> {
        struct ExprCursor<'a> {
            tokens: &'a [LexToken],
            pos: usize,
            end: usize,
        }

        impl<'a> ExprCursor<'a> {
            fn current(&self) -> Option<&'a LexToken> {
                if self.pos < self.end {
                    self.tokens.get(self.pos)
                } else {
                    None
                }
            }

            fn bump(&mut self) {
                self.pos += 1;
            }
        }

        fn parse_type_in_parens(tokens: &[LexToken], start: usize, end: usize) -> Option<ast::Type> {
            if start >= end {
                return None;
            }

            let mut cursor = start;
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Const)) {
                cursor += 1;
                if cursor >= end {
                    return None;
                }
            }

            let base = match tokens.get(cursor)?.kind {
                Token::I8 => ast::TypeKind::Primitive(ast::PrimitiveType::I8),
                Token::I16 => ast::TypeKind::Primitive(ast::PrimitiveType::I16),
                Token::I32 => ast::TypeKind::Primitive(ast::PrimitiveType::I32),
                Token::I64 => ast::TypeKind::Primitive(ast::PrimitiveType::I64),
                Token::I128 => ast::TypeKind::Primitive(ast::PrimitiveType::I128),
                Token::U8 => ast::TypeKind::Primitive(ast::PrimitiveType::U8),
                Token::U16 => ast::TypeKind::Primitive(ast::PrimitiveType::U16),
                Token::U32 => ast::TypeKind::Primitive(ast::PrimitiveType::U32),
                Token::U64 => ast::TypeKind::Primitive(ast::PrimitiveType::U64),
                Token::U128 => ast::TypeKind::Primitive(ast::PrimitiveType::U128),
                Token::F32 => ast::TypeKind::Primitive(ast::PrimitiveType::F32),
                Token::F64 => ast::TypeKind::Primitive(ast::PrimitiveType::F64),
                Token::F80 => ast::TypeKind::Primitive(ast::PrimitiveType::F80),
                Token::Bool => ast::TypeKind::Primitive(ast::PrimitiveType::Bool),
                Token::Str => ast::TypeKind::Primitive(ast::PrimitiveType::Str),
                Token::Char => ast::TypeKind::Primitive(ast::PrimitiveType::Char),
                Token::Void => ast::TypeKind::Primitive(ast::PrimitiveType::Void),
                Token::Identifier(ref name) => ast::TypeKind::Named(ast::NamedType {
                    path: vec![ast::Identifier {
                        name: name.clone(),
                        span: tokens[cursor].span.clone(),
                    }],
                    generics: None,
                }),
                _ => return None,
            };
            let mut ty = ast::Type {
                kind: Box::new(base),
                span: tokens[cursor].span.clone(),
            };
            cursor += 1;

            while cursor < end {
                if !matches!(tokens[cursor].kind, Token::Star) {
                    return None;
                }
                ty = ast::Type {
                    kind: Box::new(ast::TypeKind::Pointer(ast::PointerType {
                        is_mutable: true,
                        inner: Box::new(ty),
                    })),
                    span: Span {
                        start: tokens[start].span.start,
                        end: tokens[cursor].span.end,
                    },
                };
                cursor += 1;
            }

            Some(ty)
        }

        fn parse_primary(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let token = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected expression".to_string(),
                span: Span { start: 0, end: 0 },
            })?;

            let expr = match &token.kind {
                Token::Identifier(name) => ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: name.clone(),
                        span: token.span.clone(),
                    })),
                    span: token.span.clone(),
                },
                Token::IntLiteral(value) => ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(*value))),
                    span: token.span.clone(),
                },
                Token::FloatLiteral(value) => ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Float(*value))),
                    span: token.span.clone(),
                },
                Token::StringLiteral(value) => ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::String(
                        value.clone(),
                    ))),
                    span: token.span.clone(),
                },
                Token::CharLiteral(value) => ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Char(*value))),
                    span: token.span.clone(),
                },
                Token::True => ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Bool(true))),
                    span: token.span.clone(),
                },
                Token::False => ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Bool(false))),
                    span: token.span.clone(),
                },
                Token::LeftParen => {
                    let left_span = token.span.clone();
                    cursor.bump();
                    let inner = parse_assignment(cursor)?;
                    let Some(close) = cursor.current() else {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected ')'".to_string(),
                            span: left_span,
                        });
                    };
                    if !matches!(close.kind, Token::RightParen) {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected ')'".to_string(),
                            span: close.span.clone(),
                        });
                    }
                    let span = Span {
                        start: left_span.start,
                        end: close.span.end,
                    };
                    cursor.bump();
                    return Ok(ast::Expression {
                        kind: inner.kind,
                        span,
                    });
                }
                Token::Asm => {
                    let asm_start = token.span.start;
                    cursor.bump();
                    let Some(open) = cursor.current() else {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected '(' after asm".to_string(),
                            span: token.span.clone(),
                        });
                    };
                    if !matches!(open.kind, Token::LeftParen) {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected '(' after asm".to_string(),
                            span: open.span.clone(),
                        });
                    }
                    cursor.bump();
                    let Some(arg) = cursor.current() else {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected asm string literal".to_string(),
                            span: open.span.clone(),
                        });
                    };
                    let Token::StringLiteral(ref code) = arg.kind else {
                        return Err(ParseError::InvalidSyntax {
                            message: "asm expects a string literal".to_string(),
                            span: arg.span.clone(),
                        });
                    };
                    cursor.bump();
                    let Some(close) = cursor.current() else {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected ')' after asm string".to_string(),
                            span: arg.span.clone(),
                        });
                    };
                    if !matches!(close.kind, Token::RightParen) {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected ')' after asm string".to_string(),
                            span: close.span.clone(),
                        });
                    }
                    let span = Span {
                        start: asm_start,
                        end: close.span.end,
                    };
                    cursor.bump();
                    return Ok(ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Asm(code.clone())),
                        span,
                    });
                }
                Token::LeftBrace => {
                    let init_start = token.span.start;
                    cursor.bump();
                    let mut items = Vec::new();
                    while !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightBrace)) {
                        let Some(dot) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "unterminated initializer".to_string(),
                                span: token.span.clone(),
                            });
                        };
                        if !matches!(dot.kind, Token::Dot) {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected '.' in initializer field".to_string(),
                                span: dot.span.clone(),
                            });
                        }
                        cursor.bump();

                        let Some(name_token) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected field name in initializer".to_string(),
                                span: dot.span.clone(),
                            });
                        };
                        let Token::Identifier(field_name) = &name_token.kind else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected field name in initializer".to_string(),
                                span: name_token.span.clone(),
                            });
                        };
                        let field_ident = ast::Identifier {
                            name: field_name.clone(),
                            span: name_token.span.clone(),
                        };
                        cursor.bump();

                        let Some(assign) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected '=' in initializer field".to_string(),
                                span: field_ident.span.clone(),
                            });
                        };
                        if !matches!(assign.kind, Token::Assign) {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected '=' in initializer field".to_string(),
                                span: assign.span.clone(),
                            });
                        }
                        cursor.bump();

                        let value = parse_assignment(cursor)?;
                        items.push(ast::InitializerItem::Field {
                            name: field_ident,
                            value,
                        });

                        if matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                            cursor.bump();
                        } else {
                            break;
                        }
                    }

                    let Some(close) = cursor.current() else {
                        return Err(ParseError::InvalidSyntax {
                            message: "unterminated initializer".to_string(),
                            span: token.span.clone(),
                        });
                    };
                    if !matches!(close.kind, Token::RightBrace) {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected '}' after initializer".to_string(),
                            span: close.span.clone(),
                        });
                    }
                    let span = Span {
                        start: init_start,
                        end: close.span.end,
                    };
                    cursor.bump();
                    return Ok(ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Initializer { items }),
                        span,
                    });
                }
                _ => {
                    return Err(ParseError::InvalidSyntax {
                        message: "unsupported primary expression in bootstrap parser".to_string(),
                        span: token.span.clone(),
                    });
                }
            };

            cursor.bump();
            Ok(expr)
        }

        fn parse_postfix(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_primary(cursor)?;

            loop {
                let Some(token) = cursor.current() else {
                    break;
                };
                match token.kind {
                    Token::LeftParen => {
                        let call_start = expr.span.start;
                        cursor.bump();
                        let mut arguments = Vec::new();
                        if !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightParen)) {
                            loop {
                                let arg = parse_assignment(cursor)?;
                                arguments.push(arg);
                                match cursor.current().map(|t| &t.kind) {
                                    Some(Token::Comma) => cursor.bump(),
                                    Some(Token::RightParen) => break,
                                    Some(_) => {
                                        return Err(ParseError::InvalidSyntax {
                                            message: "expected ',' or ')' in call arguments"
                                                .to_string(),
                                            span: cursor.current().unwrap().span.clone(),
                                        });
                                    }
                                    None => {
                                        return Err(ParseError::InvalidSyntax {
                                            message: "unterminated call expression".to_string(),
                                            span: Span {
                                                start: call_start,
                                                end: call_start,
                                            },
                                        });
                                    }
                                }
                            }
                        }

                        let close = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
                            message: "expected ')' in call expression".to_string(),
                            span: Span {
                                start: call_start,
                                end: call_start,
                            },
                        })?;
                        let call_span = Span {
                            start: call_start,
                            end: close.span.end,
                        };
                        cursor.bump();

                        expr = ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Call {
                                function: Box::new(expr),
                                arguments,
                            }),
                            span: call_span,
                        };
                    }
                    Token::Dot => {
                        cursor.bump();
                        let Some(field_token) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected identifier after '.'".to_string(),
                                span: token.span.clone(),
                            });
                        };
                        let Token::Identifier(ref field_name) = field_token.kind else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected identifier after '.'".to_string(),
                                span: field_token.span.clone(),
                            });
                        };
                        let field_ident = ast::Identifier {
                            name: field_name.clone(),
                            span: field_token.span.clone(),
                        };
                        cursor.bump();

                        if matches!(cursor.current().map(|t| &t.kind), Some(Token::LeftParen)) {
                            cursor.bump();
                            let mut arguments = Vec::new();
                            if !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightParen)) {
                                loop {
                                    arguments.push(parse_assignment(cursor)?);
                                    match cursor.current().map(|t| &t.kind) {
                                        Some(Token::Comma) => cursor.bump(),
                                        Some(Token::RightParen) => break,
                                        Some(_) => {
                                            return Err(ParseError::InvalidSyntax {
                                                message: "expected ',' or ')' in method arguments"
                                                    .to_string(),
                                                span: cursor.current().unwrap().span.clone(),
                                            });
                                        }
                                        None => {
                                            return Err(ParseError::InvalidSyntax {
                                                message: "unterminated method call expression"
                                                    .to_string(),
                                                span: field_ident.span.clone(),
                                            });
                                        }
                                    }
                                }
                            }
                            let close = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
                                message: "expected ')' in method call".to_string(),
                                span: field_ident.span.clone(),
                            })?;
                            let span = Span {
                                start: expr.span.start,
                                end: close.span.end,
                            };
                            cursor.bump();
                            expr = ast::Expression {
                                kind: Box::new(ast::ExpressionKind::MethodCall {
                                    receiver: Box::new(expr),
                                    method: field_ident,
                                    arguments,
                                }),
                                span,
                            };
                        } else {
                            let span = Span {
                                start: expr.span.start,
                                end: field_ident.span.end,
                            };
                            expr = ast::Expression {
                                kind: Box::new(ast::ExpressionKind::FieldAccess {
                                    object: Box::new(expr),
                                    field: field_ident,
                                }),
                                span,
                            };
                        }
                    }
                    Token::LeftBracket => {
                        let open_span = token.span.clone();
                        cursor.bump();
                        let index = parse_assignment(cursor)?;
                        let Some(close) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected ']' in index expression".to_string(),
                                span: open_span,
                            });
                        };
                        if !matches!(close.kind, Token::RightBracket) {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected ']' in index expression".to_string(),
                                span: close.span.clone(),
                            });
                        }
                        let span = Span {
                            start: expr.span.start,
                            end: close.span.end,
                        };
                        cursor.bump();
                        expr = ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Index {
                                object: Box::new(expr),
                                index: Box::new(index),
                            }),
                            span,
                        };
                    }
                    Token::Increment => {
                        let end_span = token.span.end;
                        cursor.bump();
                        expr = ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Postfix {
                                operator: ast::UnaryOperator::Increment,
                                operand: Box::new(expr.clone()),
                            }),
                            span: Span {
                                start: expr.span.start,
                                end: end_span,
                            },
                        };
                    }
                    Token::Decrement => {
                        let end_span = token.span.end;
                        cursor.bump();
                        expr = ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Postfix {
                                operator: ast::UnaryOperator::Decrement,
                                operand: Box::new(expr.clone()),
                            }),
                            span: Span {
                                start: expr.span.start,
                                end: end_span,
                            },
                        };
                    }
                    _ => break,
                }
            }

            Ok(expr)
        }

        fn parse_unary(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let Some(token) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected expression".to_string(),
                    span: Span { start: 0, end: 0 },
                });
            };

            let operator = match token.kind {
                Token::Minus => Some(ast::UnaryOperator::Minus),
                Token::Plus => Some(ast::UnaryOperator::Plus),
                Token::Not => Some(ast::UnaryOperator::Not),
                Token::BitwiseNot => Some(ast::UnaryOperator::BitwiseNot),
                Token::Increment => Some(ast::UnaryOperator::Increment),
                Token::Decrement => Some(ast::UnaryOperator::Decrement),
                _ => None,
            };

            if let Some(operator) = operator {
                let span_start = token.span.start;
                cursor.bump();
                let operand = parse_unary(cursor)?;
                return Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Unary {
                        operator,
                        operand: Box::new(operand.clone()),
                    }),
                    span: Span {
                        start: span_start,
                        end: operand.span.end,
                    },
                });
            }

            if matches!(token.kind, Token::BitwiseAnd | Token::And) {
                let start = token.span.start;
                cursor.bump();
                let inner = parse_unary(cursor)?;
                return Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Reference {
                        is_mutable: false,
                        expression: Box::new(inner.clone()),
                    }),
                    span: Span {
                        start,
                        end: inner.span.end,
                    },
                });
            }

            if matches!(token.kind, Token::Move) {
                let start = token.span.start;
                cursor.bump();
                let inner = parse_unary(cursor)?;
                return Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Move(Box::new(inner.clone()))),
                    span: Span {
                        start,
                        end: inner.span.end,
                    },
                });
            }

            if matches!(token.kind, Token::Comptime) {
                let start = token.span.start;
                cursor.bump();
                let inner = parse_unary(cursor)?;
                return Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Comptime(Box::new(inner.clone()))),
                    span: Span {
                        start,
                        end: inner.span.end,
                    },
                });
            }

            if matches!(token.kind, Token::LeftParen) {
                let cast_start = token.span.start;
                let mut i = cursor.pos + 1;
                while i < cursor.end {
                    if matches!(cursor.tokens[i].kind, Token::RightParen) {
                        break;
                    }
                    i += 1;
                }
                if i < cursor.end && i > cursor.pos + 1 {
                    if let Some(target_type) =
                        parse_type_in_parens(cursor.tokens, cursor.pos + 1, i)
                    {
                        cursor.pos = i + 1;
                        let operand = parse_unary(cursor)?;
                        return Ok(ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Cast {
                                expression: Box::new(operand.clone()),
                                target_type: Box::new(target_type),
                            }),
                            span: Span {
                                start: cast_start,
                                end: operand.span.end,
                            },
                        });
                    }
                }
            }

            parse_postfix(cursor)
        }

        fn parse_multiplicative(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_unary(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                let operator = match token.kind {
                    Token::Star => Some(ast::BinaryOperator::Multiply),
                    Token::Slash => Some(ast::BinaryOperator::Divide),
                    Token::Percent => Some(ast::BinaryOperator::Modulo),
                    _ => None,
                };
                let Some(operator) = operator else { break };
                cursor.bump();
                let rhs = parse_unary(cursor)?;
                let span = Span { start: expr.span.start, end: rhs.span.end };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_additive(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_multiplicative(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                let operator = match token.kind {
                    Token::Plus => Some(ast::BinaryOperator::Add),
                    Token::Minus => Some(ast::BinaryOperator::Subtract),
                    _ => None,
                };
                let Some(operator) = operator else { break };
                cursor.bump();
                let rhs = parse_multiplicative(cursor)?;
                let span = Span { start: expr.span.start, end: rhs.span.end };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_relational(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_additive(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                let operator = match token.kind {
                    Token::Less => Some(ast::BinaryOperator::Less),
                    Token::Greater => Some(ast::BinaryOperator::Greater),
                    Token::LessEqual => Some(ast::BinaryOperator::LessEqual),
                    Token::GreaterEqual => Some(ast::BinaryOperator::GreaterEqual),
                    _ => None,
                };
                let Some(operator) = operator else { break };
                cursor.bump();
                let rhs = parse_additive(cursor)?;
                let span = Span { start: expr.span.start, end: rhs.span.end };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_equality(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_relational(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                let operator = match token.kind {
                    Token::Equal => Some(ast::BinaryOperator::Equal),
                    Token::NotEqual => Some(ast::BinaryOperator::NotEqual),
                    _ => None,
                };
                let Some(operator) = operator else { break };
                cursor.bump();
                let rhs = parse_relational(cursor)?;
                let span = Span { start: expr.span.start, end: rhs.span.end };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_bitwise_and(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_equality(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                if !matches!(token.kind, Token::BitwiseAnd) {
                    break;
                }
                cursor.bump();
                let rhs = parse_equality(cursor)?;
                let span = Span {
                    start: expr.span.start,
                    end: rhs.span.end,
                };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator: ast::BinaryOperator::BitwiseAnd,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_bitwise_xor(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_bitwise_and(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                if !matches!(token.kind, Token::BitwiseXor) {
                    break;
                }
                cursor.bump();
                let rhs = parse_bitwise_and(cursor)?;
                let span = Span {
                    start: expr.span.start,
                    end: rhs.span.end,
                };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator: ast::BinaryOperator::BitwiseXor,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_bitwise_or(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_bitwise_xor(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                if !matches!(token.kind, Token::BitwiseOr) {
                    break;
                }
                cursor.bump();
                let rhs = parse_bitwise_xor(cursor)?;
                let span = Span {
                    start: expr.span.start,
                    end: rhs.span.end,
                };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator: ast::BinaryOperator::BitwiseOr,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_logical_and(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_bitwise_or(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                if !matches!(token.kind, Token::And) {
                    break;
                }
                cursor.bump();
                let rhs = parse_bitwise_or(cursor)?;
                let span = Span {
                    start: expr.span.start,
                    end: rhs.span.end,
                };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator: ast::BinaryOperator::LogicalAnd,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_logical_or(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let mut expr = parse_logical_and(cursor)?;
            loop {
                let Some(token) = cursor.current() else { break };
                if !matches!(token.kind, Token::Or) {
                    break;
                }
                cursor.bump();
                let rhs = parse_logical_and(cursor)?;
                let span = Span {
                    start: expr.span.start,
                    end: rhs.span.end,
                };
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Binary {
                        left: Box::new(expr),
                        operator: ast::BinaryOperator::LogicalOr,
                        right: Box::new(rhs),
                    }),
                    span,
                };
            }
            Ok(expr)
        }

        fn parse_assignment(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
            let lhs = parse_logical_or(cursor)?;
            let Some(token) = cursor.current() else {
                return Ok(lhs);
            };

            let operator = match token.kind {
                Token::Assign => Some(ast::BinaryOperator::Assign),
                Token::PlusAssign => Some(ast::BinaryOperator::AddAssign),
                Token::MinusAssign => Some(ast::BinaryOperator::SubtractAssign),
                Token::StarAssign => Some(ast::BinaryOperator::MultiplyAssign),
                Token::SlashAssign => Some(ast::BinaryOperator::DivideAssign),
                Token::PercentAssign => Some(ast::BinaryOperator::ModuloAssign),
                _ => None,
            };

            let Some(operator) = operator else {
                return Ok(lhs);
            };

            cursor.bump();
            let rhs = parse_assignment(cursor)?;
            let span = Span {
                start: lhs.span.start,
                end: rhs.span.end,
            };
            Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Binary {
                    left: Box::new(lhs),
                    operator,
                    right: Box::new(rhs),
                }),
                span,
            })
        }

        let mut cursor = ExprCursor { tokens, pos: start, end };
        let expr = parse_assignment(&mut cursor)?;
        if cursor.pos != end {
            let span = cursor
                .current()
                .map(|t| t.span.clone())
                .unwrap_or_else(|| expr.span.clone());
            return Err(ParseError::InvalidSyntax {
                message: "unsupported expression form in bootstrap parser".to_string(),
                span,
            });
        }
        Ok(expr)
    }

    fn parse_statement_reduction(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::Statement, ParseError> {
        if start >= end {
            return Err(ParseError::InvalidSyntax {
                message: "empty statement range".to_string(),
                span: Span { start: 0, end: 0 },
            });
        }

        if matches!(tokens[start].kind, Token::Return) {
            if end == start + 2 && matches!(tokens[start + 1].kind, Token::Semicolon) {
                return Ok(ast::Statement {
                    kind: ast::StatementKind::Return(None),
                    span: Span {
                        start: tokens[start].span.start,
                        end: tokens[end - 1].span.end,
                    },
                });
            }

            let expr = self.parse_expression_reduction(tokens, start + 1, end - 1)?;
            return Ok(ast::Statement {
                kind: ast::StatementKind::Return(Some(expr)),
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[end - 1].span.end,
                },
            });
        }

        if let Some((decl_ty, after_type)) = self.parse_type_prefix(tokens, start, end - 1)
            && after_type < end
            && matches!(tokens.get(after_type).map(|t| &t.kind), Some(Token::Identifier(_)))
        {
            let name_token = &tokens[after_type];
            let Token::Identifier(name) = &name_token.kind else {
                unreachable!();
            };

            let mut initializer = None;
            if after_type + 1 < end - 1 {
                if !matches!(tokens[after_type + 1].kind, Token::Assign) {
                    return Err(ParseError::InvalidSyntax {
                        message: "unsupported declaration syntax in bootstrap parser".to_string(),
                        span: tokens[after_type + 1].span.clone(),
                    });
                }
                initializer = Some(self.parse_expression_reduction(tokens, after_type + 2, end - 1)?);
            }

            return Ok(ast::Statement {
                kind: ast::StatementKind::Let(ast::LetStatement {
                    pattern: ast::Pattern {
                        kind: ast::PatternKind::Identifier(ast::Identifier {
                            name: name.clone(),
                            span: name_token.span.clone(),
                        }),
                        span: name_token.span.clone(),
                    },
                    type_annotation: Some(decl_ty),
                    initializer,
                    is_mutable: true,
                }),
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[end - 1].span.end,
                },
            });
        }

        let expr = self.parse_expression_reduction(tokens, start, end - 1)?;
        Ok(ast::Statement {
            kind: ast::StatementKind::Expression(expr),
            span: Span {
                start: tokens[start].span.start,
                end: tokens[end - 1].span.end,
            },
        })
    }

    fn parse_block_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::Block, ParseError> {
        if end <= start + 1 {
            return Err(ParseError::InvalidSyntax {
                message: "invalid block range".to_string(),
                span: tokens
                    .get(start)
                    .map(|t| t.span.clone())
                    .unwrap_or(Span { start: 0, end: 0 }),
            });
        }

        let mut statements = Vec::new();
        let mut cursor = start + 1;
        while cursor + 1 < end {
            if matches!(tokens[cursor].kind, Token::RightBrace) {
                break;
            }

            if matches!(tokens[cursor].kind, Token::If) {
                let cond_open = cursor + 1;
                let Some(cond_close) =
                    self.find_matching_token(tokens, cond_open, end, Token::LeftParen, Token::RightParen)
                else {
                    return Err(ParseError::InvalidSyntax {
                        message: "unterminated if condition".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                };
                let then_start = cond_close + 1;
                let Some(then_end_inclusive) = self
                    .find_matching_token(tokens, then_start, end, Token::LeftBrace, Token::RightBrace)
                else {
                    return Err(ParseError::InvalidSyntax {
                        message: "missing if block".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                };
                let mut stmt_end = then_end_inclusive + 1;
                if stmt_end < end && matches!(tokens[stmt_end].kind, Token::Else) {
                    let else_start = stmt_end + 1;
                    let Some(else_end_inclusive) = self
                        .find_matching_token(tokens, else_start, end, Token::LeftBrace, Token::RightBrace)
                    else {
                        return Err(ParseError::InvalidSyntax {
                            message: "missing else block".to_string(),
                            span: tokens[stmt_end].span.clone(),
                        });
                    };
                    stmt_end = else_end_inclusive + 1;
                }
                statements.push(self.parse_if_statement_reduction(tokens, cursor, stmt_end)?);
                cursor = stmt_end;
                continue;
            }

            if matches!(tokens[cursor].kind, Token::While) {
                let cond_open = cursor + 1;
                let Some(cond_close) =
                    self.find_matching_token(tokens, cond_open, end, Token::LeftParen, Token::RightParen)
                else {
                    return Err(ParseError::InvalidSyntax {
                        message: "unterminated while condition".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                };
                let body_start = cond_close + 1;
                let Some(body_end_inclusive) = self
                    .find_matching_token(tokens, body_start, end, Token::LeftBrace, Token::RightBrace)
                else {
                    return Err(ParseError::InvalidSyntax {
                        message: "missing while block".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                };
                let stmt_end = body_end_inclusive + 1;
                statements.push(self.parse_while_statement_reduction(tokens, cursor, stmt_end)?);
                cursor = stmt_end;
                continue;
            }

            if matches!(tokens[cursor].kind, Token::For) {
                let header_open = cursor + 1;
                let Some(header_close) =
                    self.find_matching_token(tokens, header_open, end, Token::LeftParen, Token::RightParen)
                else {
                    return Err(ParseError::InvalidSyntax {
                        message: "unterminated for header".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                };
                let body_start = header_close + 1;
                let Some(body_end_inclusive) = self
                    .find_matching_token(tokens, body_start, end, Token::LeftBrace, Token::RightBrace)
                else {
                    return Err(ParseError::InvalidSyntax {
                        message: "missing for block".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                };
                let stmt_end = body_end_inclusive + 1;
                statements.push(self.parse_for_statement_reduction(tokens, cursor, stmt_end)?);
                cursor = stmt_end;
                continue;
            }

            if matches!(tokens[cursor].kind, Token::LeftBrace) {
                let Some(close) = self.find_matching_token(tokens, cursor, end, Token::LeftBrace, Token::RightBrace) else {
                    return Err(ParseError::InvalidSyntax {
                        message: "unterminated nested block".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                };
                let nested = self.parse_block_reduction(tokens, cursor, close + 1)?;
                statements.push(ast::Statement {
                    kind: ast::StatementKind::Block(nested),
                    span: Span {
                        start: tokens[cursor].span.start,
                        end: tokens[close].span.end,
                    },
                });
                cursor = close + 1;
                continue;
            }

            let Some(semicolon) = self.find_statement_terminator(tokens, cursor, end) else {
                return Err(ParseError::InvalidSyntax {
                    message: "missing ';' for statement".to_string(),
                    span: tokens[cursor].span.clone(),
                });
            };
            let statement_end = semicolon + 1;
            statements.push(self.parse_statement_reduction(tokens, cursor, statement_end)?);
            cursor = statement_end;
        }

        Ok(ast::Block {
            statements,
            span: Span {
                start: tokens[start].span.start,
                end: tokens[end - 1].span.end,
            },
        })
    }

    fn parse_import_reduction(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::ImportItem, ParseError> {
        let mut path = Vec::new();
        let mut cursor = start + 1;

        while cursor < end {
            let token = &tokens[cursor];
            if let Token::Identifier(name) = &token.kind {
                if name == "as" {
                    break;
                }
                path.push(ast::Identifier {
                    name: name.clone(),
                    span: token.span.clone(),
                });
                cursor += 1;
                if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Dot)) {
                    cursor += 1;
                    continue;
                }
                break;
            } else {
                return Err(ParseError::InvalidSyntax {
                    message: "invalid import path segment".to_string(),
                    span: token.span.clone(),
                });
            }
        }

        if path.is_empty() {
            return Err(ParseError::InvalidSyntax {
                message: "import path cannot be empty".to_string(),
                span: tokens[start].span.clone(),
            });
        }

        let mut alias = None;
        if cursor < end {
            if let Token::Identifier(keyword) = &tokens[cursor].kind {
                if keyword == "as" {
                    cursor += 1;
                    let token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                        message: "expected alias after `as`".to_string(),
                        span: tokens[end.saturating_sub(1)].span.clone(),
                    })?;
                    let Token::Identifier(name) = &token.kind else {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected identifier after `as`".to_string(),
                            span: token.span.clone(),
                        });
                    };
                    alias = Some(ast::Identifier {
                        name: name.clone(),
                        span: token.span.clone(),
                    });
                    cursor += 1;
                }
            }
        }

        let mut items = None;
        if cursor < end && !matches!(tokens[cursor].kind, Token::Semicolon) {
            if !matches!(tokens[cursor].kind, Token::LeftBrace) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected `{` or `;` after import path".to_string(),
                    span: tokens[cursor].span.clone(),
                });
            }
            cursor += 1;
            let mut imported_items = Vec::new();
            while cursor < end {
                if matches!(tokens[cursor].kind, Token::RightBrace) {
                    break;
                }
                let token = &tokens[cursor];
                let Token::Identifier(name) = &token.kind else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected imported item name".to_string(),
                        span: token.span.clone(),
                    });
                };
                let mut item_alias = None;
                let item_name = ast::Identifier {
                    name: name.clone(),
                    span: token.span.clone(),
                };
                cursor += 1;
                if cursor < end {
                    if let Token::Identifier(keyword) = &tokens[cursor].kind {
                        if keyword == "as" {
                            cursor += 1;
                            let alias_token = tokens.get(cursor).ok_or_else(|| {
                                ParseError::InvalidSyntax {
                                    message: "expected alias after `as`".to_string(),
                                    span: token.span.clone(),
                                }
                            })?;
                            let Token::Identifier(alias_name) = &alias_token.kind else {
                                return Err(ParseError::InvalidSyntax {
                                    message: "expected identifier after `as`".to_string(),
                                    span: alias_token.span.clone(),
                                });
                            };
                            item_alias = Some(ast::Identifier {
                                name: alias_name.clone(),
                                span: alias_token.span.clone(),
                            });
                            cursor += 1;
                        }
                    }
                }
                imported_items.push(ast::ImportedItem {
                    name: item_name,
                    alias: item_alias,
                });
                if cursor < end && matches!(tokens[cursor].kind, Token::Comma) {
                    cursor += 1;
                    continue;
                }
                if cursor < end && matches!(tokens[cursor].kind, Token::RightBrace) {
                    continue;
                }
                if cursor < end {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected `,` or `}` in import item list".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                }
            }
            items = Some(imported_items);
        }

        Ok(ast::ImportItem {
            path,
            alias,
            items,
        })
    }

    fn parse_global_variable_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::GlobalVariableItem, ParseError> {
        let decl_end = end.saturating_sub(1);
        let (var_type, after_type) =
            self.parse_type_prefix(tokens, start, decl_end)
                .ok_or_else(|| ParseError::InvalidSyntax {
                    message: "expected global variable type".to_string(),
                    span: tokens[start].span.clone(),
                })?;
        if !matches!(
            tokens.get(after_type).map(|token| &token.kind),
            Some(Token::Identifier(_))
        ) {
            return Err(ParseError::InvalidSyntax {
                message: "expected global variable name".to_string(),
                span: tokens
                    .get(after_type)
                    .map(|token| token.span.clone())
                    .unwrap_or_else(|| tokens[start].span.clone()),
            });
        }

        let name_token = &tokens[after_type];
        let Token::Identifier(name) = &name_token.kind else {
            unreachable!();
        };

        let mut initializer = None;
        if after_type + 1 < decl_end {
            if !matches!(tokens[after_type + 1].kind, Token::Assign) {
                return Err(ParseError::InvalidSyntax {
                    message: "unsupported global variable declaration syntax".to_string(),
                    span: tokens[after_type + 1].span.clone(),
                });
            }
            initializer = Some(self.parse_expression_reduction(tokens, after_type + 2, decl_end)?);
        }

        Ok(ast::GlobalVariableItem {
            name: ast::Identifier {
                name: name.clone(),
                span: name_token.span.clone(),
            },
            var_type,
            initializer,
            is_mutable: true,
        })
    }

    fn parse_trait_ref_range(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::TraitRef, ParseError> {
        if start >= end {
            return Err(ParseError::InvalidSyntax {
                message: "expected trait name".to_string(),
                span: tokens
                    .get(start)
                    .map(|t| t.span.clone())
                    .unwrap_or(Span { start: 0, end: 0 }),
            });
        }

        let mut path = Vec::new();
        let mut cursor = start;
        let Token::Identifier(first) = &tokens[cursor].kind else {
            return Err(ParseError::InvalidSyntax {
                message: "invalid trait path segment".to_string(),
                span: tokens[cursor].span.clone(),
            });
        };
        path.push(ast::Identifier {
            name: first.clone(),
            span: tokens[cursor].span.clone(),
        });
        cursor += 1;
        while cursor + 1 < end && matches!(tokens[cursor].kind, Token::DoubleColon) {
            cursor += 1;
            let Token::Identifier(segment) = &tokens[cursor].kind else {
                return Err(ParseError::InvalidSyntax {
                    message: "invalid trait path segment".to_string(),
                    span: tokens[cursor].span.clone(),
                });
            };
            path.push(ast::Identifier {
                name: segment.clone(),
                span: tokens[cursor].span.clone(),
            });
            cursor += 1;
        }

        let mut generics = None;
        if cursor < end && matches!(tokens[cursor].kind, Token::Less) {
            let Some(close) =
                self.find_matching_token(tokens, cursor, end, Token::Less, Token::Greater)
            else {
                return Err(ParseError::InvalidSyntax {
                    message: "unterminated trait generic arguments".to_string(),
                    span: tokens[cursor].span.clone(),
                });
            };
            let mut args = Vec::new();
            let mut arg_cursor = cursor + 1;
            while arg_cursor < close {
                let (arg, next_arg) =
                    self.parse_type_prefix(tokens, arg_cursor, close)
                        .ok_or_else(|| ParseError::InvalidSyntax {
                            message: "invalid trait generic argument".to_string(),
                            span: tokens[arg_cursor].span.clone(),
                        })?;
                args.push(arg);
                arg_cursor = next_arg;
                if arg_cursor < close {
                    if !matches!(tokens[arg_cursor].kind, Token::Comma) {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected ',' between trait generic arguments".to_string(),
                            span: tokens[arg_cursor].span.clone(),
                        });
                    }
                    arg_cursor += 1;
                }
            }
            generics = Some(args);
            cursor = close + 1;
        }

        if cursor != end || path.is_empty() {
            return Err(ParseError::InvalidSyntax {
                message: "invalid trait reference".to_string(),
                span: tokens[start].span.clone(),
            });
        }

        Ok(ast::TraitRef {
            path,
            generics,
            span: Span {
                start: tokens[start].span.start,
                end: tokens[end - 1].span.end,
            },
        })
    }

    fn parse_trait_ref_prefix(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<(ast::TraitRef, usize), ParseError> {
        if start >= end {
            return Err(ParseError::InvalidSyntax {
                message: "expected trait name".to_string(),
                span: Span { start: 0, end: 0 },
            });
        }
        if !matches!(tokens[start].kind, Token::Identifier(_)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected trait name".to_string(),
                span: tokens[start].span.clone(),
            });
        }

        let mut cursor = start + 1;
        while cursor + 1 < end && matches!(tokens[cursor].kind, Token::DoubleColon) {
            if !matches!(tokens[cursor + 1].kind, Token::Identifier(_)) {
                break;
            }
            cursor += 2;
        }

        if cursor < end && matches!(tokens[cursor].kind, Token::Less) {
            let Some(close) =
                self.find_matching_token(tokens, cursor, end, Token::Less, Token::Greater)
            else {
                return Err(ParseError::InvalidSyntax {
                    message: "unterminated trait generic arguments".to_string(),
                    span: tokens[cursor].span.clone(),
                });
            };
            cursor = close + 1;
        }

        let trait_ref = self.parse_trait_ref_range(tokens, start, cursor)?;
        Ok((trait_ref, cursor))
    }

    fn parse_generics_prefix(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<(Option<ast::Generics>, usize), ParseError> {
        if start >= end || !matches!(tokens[start].kind, Token::Less) {
            return Ok((None, start));
        }

        let Some(close) = self.find_matching_token(tokens, start, end, Token::Less, Token::Greater)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "unterminated generic parameter list".to_string(),
                span: tokens[start].span.clone(),
            });
        };

        let mut params = Vec::new();
        let mut cursor = start + 1;
        while cursor < close {
            let name_token = &tokens[cursor];
            let Token::Identifier(name) = &name_token.kind else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected generic parameter name".to_string(),
                    span: name_token.span.clone(),
                });
            };
            cursor += 1;

            let mut bounds = Vec::new();
            if cursor < close && matches!(tokens[cursor].kind, Token::Colon) {
                cursor += 1;
                loop {
                    let bound_start = cursor;
                    let mut bound_end = cursor;
                    while bound_end < close
                        && !matches!(tokens[bound_end].kind, Token::Plus | Token::Comma)
                    {
                        bound_end += 1;
                    }
                    if bound_start == bound_end {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected trait bound after ':'".to_string(),
                            span: tokens[cursor.min(close - 1)].span.clone(),
                        });
                    }
                    let trait_ref = self.parse_trait_ref_range(tokens, bound_start, bound_end)?;
                    bounds.push(ast::TraitBound {
                        trait_ref,
                        is_optional: false,
                    });
                    cursor = bound_end;
                    if cursor < close && matches!(tokens[cursor].kind, Token::Plus) {
                        cursor += 1;
                        continue;
                    }
                    break;
                }
            }

            let param_span = Span {
                start: name_token.span.start,
                end: tokens[cursor.saturating_sub(1)].span.end,
            };
            params.push(ast::GenericParam::Type(ast::TypeParam {
                name: ast::Identifier {
                    name: name.clone(),
                    span: name_token.span.clone(),
                },
                bounds,
                default: None,
                span: param_span,
            }));

            if cursor < close && matches!(tokens[cursor].kind, Token::Comma) {
                cursor += 1;
            }
        }

        Ok((
            Some(ast::Generics {
                params,
                where_clause: None,
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[close].span.end,
                },
            }),
            close + 1,
        ))
    }

    fn parse_where_clause_prefix(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<(Option<ast::WhereClause>, usize), ParseError> {
        if start >= end {
            return Ok((None, start));
        }
        let Token::Identifier(keyword) = &tokens[start].kind else {
            return Ok((None, start));
        };
        if keyword != "where" {
            return Ok((None, start));
        }

        let mut predicates = Vec::new();
        let mut cursor = start + 1;
        while cursor < end {
            let (bounded_type, next_after_type) = self
                .parse_type_prefix(tokens, cursor, end)
                .ok_or_else(|| ParseError::InvalidSyntax {
                    message: "expected where-clause type".to_string(),
                    span: tokens[cursor].span.clone(),
                })?;
            cursor = next_after_type;
            if cursor >= end || !matches!(tokens[cursor].kind, Token::Colon) {
                break;
            }
            cursor += 1;

            let mut bounds = Vec::new();
            loop {
                let (trait_ref, next_after_bound) =
                    self.parse_trait_ref_prefix(tokens, cursor, end)?;
                bounds.push(ast::TraitBound {
                    trait_ref,
                    is_optional: false,
                });
                cursor = next_after_bound;
                if cursor < end && matches!(tokens[cursor].kind, Token::Plus) {
                    cursor += 1;
                    continue;
                }
                break;
            }

            if bounds.is_empty() {
                return Err(ParseError::InvalidSyntax {
                    message: "where-clause predicate must include at least one trait bound"
                        .to_string(),
                    span: tokens[cursor.min(end - 1)].span.clone(),
                });
            }

            predicates.push(ast::WherePredicate::Type {
                bounded_type,
                bounds,
            });

            if cursor < end && matches!(tokens[cursor].kind, Token::Comma) {
                cursor += 1;
                continue;
            }
            break;
        }

        if predicates.is_empty() {
            return Err(ParseError::InvalidSyntax {
                message: "where clause requires at least one predicate".to_string(),
                span: tokens[start].span.clone(),
            });
        }

        Ok((
            Some(ast::WhereClause {
                predicates,
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[cursor - 1].span.end,
                },
            }),
            cursor,
        ))
    }

    fn merge_generics_where(
        &self,
        mut generics: Option<ast::Generics>,
        where_clause: Option<ast::WhereClause>,
    ) -> Option<ast::Generics> {
        if let Some(clause) = where_clause {
            if let Some(existing) = generics.as_mut() {
                existing.where_clause = Some(clause);
            } else {
                generics = Some(ast::Generics {
                    params: Vec::new(),
                    where_clause: Some(clause.clone()),
                    span: clause.span,
                });
            }
        }
        generics
    }

    fn parse_attribute_args(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<Vec<ast::AttributeArg>, ParseError> {
        let mut args = Vec::new();
        let mut cursor = start;
        while cursor < end {
            match &tokens[cursor].kind {
                Token::Identifier(name) => {
                    args.push(ast::AttributeArg::Identifier(ast::Identifier {
                        name: name.clone(),
                        span: tokens[cursor].span.clone(),
                    }));
                    cursor += 1;
                }
                Token::IntLiteral(value) => {
                    args.push(ast::AttributeArg::Literal(ast::Literal::Integer(*value)));
                    cursor += 1;
                }
                Token::StringLiteral(value) => {
                    args.push(ast::AttributeArg::Literal(ast::Literal::String(value.clone())));
                    cursor += 1;
                }
                Token::True => {
                    args.push(ast::AttributeArg::Literal(ast::Literal::Bool(true)));
                    cursor += 1;
                }
                Token::False => {
                    args.push(ast::AttributeArg::Literal(ast::Literal::Bool(false)));
                    cursor += 1;
                }
                _ => {
                    return Err(ParseError::InvalidSyntax {
                        message: "invalid attribute argument".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                }
            }
            if cursor < end {
                if !matches!(tokens[cursor].kind, Token::Comma) {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected ',' between attribute arguments".to_string(),
                        span: tokens[cursor].span.clone(),
                    });
                }
                cursor += 1;
            }
        }
        Ok(args)
    }

    fn parse_attributes_prefix(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<(Vec<ast::Attribute>, usize), ParseError> {
        let mut attributes = Vec::new();
        let mut cursor = start;

        while cursor + 2 < end && matches!(tokens[cursor].kind, Token::Hash) {
            if !matches!(tokens[cursor + 1].kind, Token::LeftBracket) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected '[' after '#'".to_string(),
                    span: tokens[cursor + 1].span.clone(),
                });
            }
            let bracket_start = cursor + 1;
            let Some(bracket_end) =
                self.find_matching_token(tokens, bracket_start, end, Token::LeftBracket, Token::RightBracket)
            else {
                return Err(ParseError::InvalidSyntax {
                    message: "unterminated attribute list".to_string(),
                    span: tokens[cursor].span.clone(),
                });
            };

            let mut inner = bracket_start + 1;
            while inner < bracket_end {
                let name_token = &tokens[inner];
                let Token::Identifier(name) = &name_token.kind else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected attribute name".to_string(),
                        span: name_token.span.clone(),
                    });
                };
                inner += 1;

                let mut args = Vec::new();
                if inner < bracket_end && matches!(tokens[inner].kind, Token::LeftParen) {
                    let Some(arg_close) = self.find_matching_token(
                        tokens,
                        inner,
                        bracket_end,
                        Token::LeftParen,
                        Token::RightParen,
                    ) else {
                        return Err(ParseError::InvalidSyntax {
                            message: "unterminated attribute arguments".to_string(),
                            span: tokens[inner].span.clone(),
                        });
                    };
                    args = self.parse_attribute_args(tokens, inner + 1, arg_close)?;
                    inner = arg_close + 1;
                }

                let attr_end_span = if inner > bracket_start + 1 {
                    tokens[inner - 1].span.end
                } else {
                    name_token.span.end
                };
                attributes.push(ast::Attribute {
                    name: ast::Identifier {
                        name: name.clone(),
                        span: name_token.span.clone(),
                    },
                    args,
                    span: Span {
                        start: name_token.span.start,
                        end: attr_end_span,
                    },
                });

                if inner < bracket_end && matches!(tokens[inner].kind, Token::Comma) {
                    inner += 1;
                }
            }

            cursor = bracket_end + 1;
        }

        Ok((attributes, cursor))
    }

    fn parse_visibility_prefix(
        &self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> (ast::Visibility, usize) {
        if start >= end {
            return (ast::Visibility::Public, start);
        }

        match tokens[start].kind {
            Token::Pub => (ast::Visibility::Public, start + 1),
            Token::Private => (ast::Visibility::Private, start + 1),
            _ => (ast::Visibility::Public, start),
        }
    }

    fn parse_struct_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::StructItem, ParseError> {
        let mut cursor = start + 1;
        let name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
            message: "missing struct name".to_string(),
            span: tokens[start].span.clone(),
        })?;
        let Token::Identifier(struct_name) = &name_token.kind else {
            return Err(ParseError::InvalidSyntax {
                message: "expected struct identifier".to_string(),
                span: name_token.span.clone(),
            });
        };
        cursor += 1;

        let (mut generics, next) = self.parse_generics_prefix(tokens, cursor, end)?;
        cursor = next;
        let (where_clause, next_after_where) = self.parse_where_clause_prefix(tokens, cursor, end)?;
        cursor = next_after_where;
        generics = self.merge_generics_where(generics, where_clause);

        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftBrace)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected '{' after struct name".to_string(),
                span: tokens[cursor.min(end - 1)].span.clone(),
            });
        }
        cursor += 1;

        let mut fields = Vec::new();
        while cursor < end - 1 {
            let field_type_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected struct field type".to_string(),
                span: name_token.span.clone(),
            })?;
            let (field_type, after_type) = self
                .parse_type_prefix(tokens, cursor, end - 1)
                .ok_or_else(|| ParseError::InvalidSyntax {
                    message: "invalid struct field type".to_string(),
                    span: field_type_token.span.clone(),
                })?;
            cursor = after_type;
            let field_name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected struct field name".to_string(),
                span: field_type.span.clone(),
            })?;
            let Token::Identifier(field_name) = &field_name_token.kind else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected struct field name".to_string(),
                    span: field_name_token.span.clone(),
                });
            };
            cursor += 1;
            if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Semicolon)) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected ';' after struct field".to_string(),
                    span: tokens[cursor.min(end - 1)].span.clone(),
                });
            }
            let field_span = Span {
                start: field_type.span.start,
                end: field_name_token.span.end,
            };
            fields.push(ast::Field {
                name: ast::Identifier {
                    name: field_name.clone(),
                    span: field_name_token.span.clone(),
                },
                field_type,
                visibility: ast::Visibility::Private,
                span: field_span,
            });
            cursor += 1;
        }

        Ok(ast::StructItem {
            name: ast::Identifier {
                name: struct_name.clone(),
                span: name_token.span.clone(),
            },
            generics,
            fields,
        })
    }

    fn parse_enum_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::EnumItem, ParseError> {
        let mut cursor = start + 1;
        let name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
            message: "missing enum name".to_string(),
            span: tokens[start].span.clone(),
        })?;
        let Token::Identifier(enum_name) = &name_token.kind else {
            return Err(ParseError::InvalidSyntax {
                message: "expected enum identifier".to_string(),
                span: name_token.span.clone(),
            });
        };
        cursor += 1;

        let (mut generics, next) = self.parse_generics_prefix(tokens, cursor, end)?;
        cursor = next;
        let (where_clause, next_after_where) = self.parse_where_clause_prefix(tokens, cursor, end)?;
        cursor = next_after_where;
        generics = self.merge_generics_where(generics, where_clause);

        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftBrace)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected '{' after enum name".to_string(),
                span: tokens[cursor.min(end - 1)].span.clone(),
            });
        }
        cursor += 1;

        let mut variants = Vec::new();
        while cursor < end - 1 {
            let variant_name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected enum variant".to_string(),
                span: name_token.span.clone(),
            })?;
            let Token::Identifier(variant_name) = &variant_name_token.kind else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected enum variant identifier".to_string(),
                    span: variant_name_token.span.clone(),
                });
            };
            cursor += 1;

            let mut discriminant = None;
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Assign)) {
                cursor += 1;
                let mut sign = 1i128;
                if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Minus)) {
                    sign = -1;
                    cursor += 1;
                }
                let Some(value_token) = tokens.get(cursor) else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected integer literal after '='".to_string(),
                        span: variant_name_token.span.clone(),
                    });
                };
                let Token::IntLiteral(value) = value_token.kind else {
                    return Err(ParseError::InvalidSyntax {
                        message: "enum discriminant must be an integer literal".to_string(),
                        span: value_token.span.clone(),
                    });
                };
                discriminant = Some(sign.saturating_mul(value));
                cursor += 1;
            }

            let Some(semicolon_token) = tokens.get(cursor) else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected ';' after enum variant".to_string(),
                    span: variant_name_token.span.clone(),
                });
            };
            if !matches!(semicolon_token.kind, Token::Semicolon) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected ';' after enum variant".to_string(),
                    span: semicolon_token.span.clone(),
                });
            }
            cursor += 1;

            variants.push(ast::EnumVariant {
                name: ast::Identifier {
                    name: variant_name.clone(),
                    span: variant_name_token.span.clone(),
                },
                data: ast::EnumVariantData::Unit,
                discriminant,
                span: Span {
                    start: variant_name_token.span.start,
                    end: semicolon_token.span.end,
                },
            });
        }

        Ok(ast::EnumItem {
            name: ast::Identifier {
                name: enum_name.clone(),
                span: name_token.span.clone(),
            },
            generics,
            variants,
        })
    }

    fn parse_trait_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::TraitItem, ParseError> {
        let mut cursor = start + 1;
        let name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
            message: "missing trait name".to_string(),
            span: tokens[start].span.clone(),
        })?;
        let Token::Identifier(trait_name) = &name_token.kind else {
            return Err(ParseError::InvalidSyntax {
                message: "expected trait identifier".to_string(),
                span: name_token.span.clone(),
            });
        };
        cursor += 1;

        let (mut generics, next) = self.parse_generics_prefix(tokens, cursor, end)?;
        cursor = next;

        let mut super_traits = Vec::new();
        if cursor < end && matches!(tokens[cursor].kind, Token::Colon) {
            cursor += 1;
            loop {
                let bound_start = cursor;
                let mut bound_end = cursor;
                while bound_end < end && !matches!(tokens[bound_end].kind, Token::Comma | Token::LeftBrace)
                {
                    bound_end += 1;
                }
                if bound_start == bound_end {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected super trait after ':'".to_string(),
                        span: tokens[cursor.min(end - 1)].span.clone(),
                    });
                }
                let trait_ref = self.parse_trait_ref_range(tokens, bound_start, bound_end)?;
                super_traits.push(ast::TraitBound {
                    trait_ref,
                    is_optional: false,
                });
                cursor = bound_end;
                if cursor < end && matches!(tokens[cursor].kind, Token::Comma) {
                    cursor += 1;
                    continue;
                }
                break;
            }
        }

        let (where_clause, next_after_where) = self.parse_where_clause_prefix(tokens, cursor, end)?;
        cursor = next_after_where;
        generics = self.merge_generics_where(generics, where_clause);

        if cursor >= end || !matches!(tokens[cursor].kind, Token::LeftBrace) {
            return Err(ParseError::InvalidSyntax {
                message: "expected trait body".to_string(),
                span: tokens[cursor.min(end - 1)].span.clone(),
            });
        }

        let mut items = Vec::new();
        cursor += 1;
        while cursor < end - 1 {
            if matches!(tokens[cursor].kind, Token::Fn) {
                let (item, next_cursor) = self.parse_trait_function_item(tokens, cursor, end - 1)?;
                items.push(ast::TraitItemKind::Function(item));
                cursor = next_cursor;
                continue;
            }
            if matches!(&tokens[cursor].kind, Token::Identifier(name) if name == "type") {
                let (item, next_cursor) = self.parse_trait_assoc_type_item(tokens, cursor, end - 1)?;
                items.push(ast::TraitItemKind::AssociatedType(item));
                cursor = next_cursor;
                continue;
            }
            return Err(ParseError::InvalidSyntax {
                message: "unsupported trait item".to_string(),
                span: tokens[cursor].span.clone(),
            });
        }

        Ok(ast::TraitItem {
            name: ast::Identifier {
                name: trait_name.clone(),
                span: name_token.span.clone(),
            },
            generics,
            super_traits,
            items,
        })
    }

    fn parse_trait_function_item(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<(ast::TraitFunction, usize), ParseError> {
        let name_token = tokens.get(start + 1).ok_or_else(|| ParseError::InvalidSyntax {
            message: "missing trait method name".to_string(),
            span: tokens[start].span.clone(),
        })?;
        let Token::Identifier(method_name) = &name_token.kind else {
            return Err(ParseError::InvalidSyntax {
                message: "expected trait method name".to_string(),
                span: name_token.span.clone(),
            });
        };

        let mut cursor = start + 2;
        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftParen)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected '(' after trait method name".to_string(),
                span: tokens[cursor.min(end - 1)].span.clone(),
            });
        }
        cursor += 1;

        let mut parameters = Vec::new();
        while cursor < end {
            if matches!(tokens[cursor].kind, Token::RightParen) {
                cursor += 1;
                break;
            }
            let (param_type, after_type) =
                self.parse_type_prefix(tokens, cursor, end)
                    .ok_or_else(|| ParseError::InvalidSyntax {
                        message: "invalid trait method parameter type".to_string(),
                        span: tokens[cursor].span.clone(),
                    })?;
            cursor = after_type;
            let param_name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected trait method parameter name".to_string(),
                span: param_type.span.clone(),
            })?;
            let Token::Identifier(param_name) = &param_name_token.kind else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected trait method parameter name".to_string(),
                    span: param_name_token.span.clone(),
                });
            };
            parameters.push(ast::Parameter {
                name: ast::Identifier {
                    name: param_name.clone(),
                    span: param_name_token.span.clone(),
                },
                param_type: param_type.clone(),
                is_mutable: false,
                span: Span {
                    start: param_type.span.start,
                    end: param_name_token.span.end,
                },
            });
            cursor += 1;
            if cursor < end && matches!(tokens[cursor].kind, Token::Comma) {
                cursor += 1;
            }
        }

        let mut return_type = None;
        if cursor < end && matches!(tokens[cursor].kind, Token::Arrow) {
            cursor += 1;
            let (ret, next_cursor) =
                self.parse_type_prefix(tokens, cursor, end)
                    .ok_or_else(|| ParseError::InvalidSyntax {
                        message: "invalid trait method return type".to_string(),
                        span: tokens[cursor].span.clone(),
                    })?;
            return_type = Some(ret);
            cursor = next_cursor;
        }

        let mut default_body = None;
        let item_end = if cursor < end && matches!(tokens[cursor].kind, Token::Semicolon) {
            cursor + 1
        } else {
            let Some(close) =
                self.find_matching_token(tokens, cursor, end, Token::LeftBrace, Token::RightBrace)
            else {
                return Err(ParseError::InvalidSyntax {
                    message: "unterminated trait method body".to_string(),
                    span: tokens[cursor.min(end - 1)].span.clone(),
                });
            };
            default_body = Some(self.parse_block_reduction(tokens, cursor, close + 1)?);
            close + 1
        };

        Ok((
            ast::TraitFunction {
                name: ast::Identifier {
                    name: method_name.clone(),
                    span: name_token.span.clone(),
                },
                generics: None,
                parameters,
                return_type,
                default_body,
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[item_end - 1].span.end,
                },
            },
            item_end,
        ))
    }

    fn parse_trait_assoc_type_item(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<(ast::AssociatedType, usize), ParseError> {
        let name_token = tokens.get(start + 1).ok_or_else(|| ParseError::InvalidSyntax {
            message: "missing associated type name".to_string(),
            span: tokens[start].span.clone(),
        })?;
        let Token::Identifier(type_name) = &name_token.kind else {
            return Err(ParseError::InvalidSyntax {
                message: "expected associated type name".to_string(),
                span: name_token.span.clone(),
            });
        };

        let mut bounds = Vec::new();
        let mut cursor = start + 2;
        if cursor < end && matches!(tokens[cursor].kind, Token::Colon) {
            cursor += 1;
            loop {
                let bound_start = cursor;
                let mut bound_end = cursor;
                while bound_end < end && !matches!(tokens[bound_end].kind, Token::Plus | Token::Assign | Token::Semicolon)
                {
                    bound_end += 1;
                }
                let trait_ref = self.parse_trait_ref_range(tokens, bound_start, bound_end)?;
                bounds.push(ast::TraitBound {
                    trait_ref,
                    is_optional: false,
                });
                cursor = bound_end;
                if cursor < end && matches!(tokens[cursor].kind, Token::Plus) {
                    cursor += 1;
                    continue;
                }
                break;
            }
        }

        let mut default = None;
        if cursor < end && matches!(tokens[cursor].kind, Token::Assign) {
            cursor += 1;
            let (value, next_cursor) =
                self.parse_type_prefix(tokens, cursor, end)
                    .ok_or_else(|| ParseError::InvalidSyntax {
                        message: "invalid associated type default".to_string(),
                        span: tokens[cursor].span.clone(),
                    })?;
            default = Some(value);
            cursor = next_cursor;
        }

        if cursor >= end || !matches!(tokens[cursor].kind, Token::Semicolon) {
            return Err(ParseError::InvalidSyntax {
                message: "expected ';' after associated type".to_string(),
                span: tokens[cursor.min(end - 1)].span.clone(),
            });
        }
        let item_end = cursor + 1;

        Ok((
            ast::AssociatedType {
                name: ast::Identifier {
                    name: type_name.clone(),
                    span: name_token.span.clone(),
                },
                bounds,
                default,
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[item_end - 1].span.end,
                },
            },
            item_end,
        ))
    }

    fn parse_impl_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::ImplItem, ParseError> {
        let mut cursor = start + 1;
        let (mut generics, next) = self.parse_generics_prefix(tokens, cursor, end)?;
        cursor = next;
        let (where_clause, next_after_where) = self.parse_where_clause_prefix(tokens, cursor, end)?;
        cursor = next_after_where;
        generics = self.merge_generics_where(generics, where_clause);

        let body_open = tokens
            .iter()
            .enumerate()
            .take(end)
            .skip(cursor)
            .find_map(|(idx, token)| {
                if matches!(token.kind, Token::LeftBrace) {
                    Some(idx)
                } else {
                    None
                }
            })
            .ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected impl body".to_string(),
                span: tokens[start].span.clone(),
            })?;

        let for_index = tokens
            .iter()
            .enumerate()
            .take(body_open)
            .skip(cursor)
            .find_map(|(idx, token)| {
                if matches!(token.kind, Token::For) {
                    Some(idx)
                } else {
                    None
                }
            });

        let (trait_ref, self_type) = if let Some(for_idx) = for_index {
            let trait_ref = self.parse_trait_ref_range(tokens, cursor, for_idx)?;
            let (self_ty, after_self) = self
                .parse_type_prefix(tokens, for_idx + 1, body_open)
                .ok_or_else(|| ParseError::InvalidSyntax {
                    message: "invalid impl self type".to_string(),
                    span: tokens[for_idx].span.clone(),
                })?;
            if after_self != body_open {
                return Err(ParseError::InvalidSyntax {
                    message: "unexpected tokens before impl body".to_string(),
                    span: tokens[after_self].span.clone(),
                });
            }
            (Some(trait_ref), self_ty)
        } else {
            let (self_ty, after_self) = self
                .parse_type_prefix(tokens, cursor, body_open)
                .ok_or_else(|| ParseError::InvalidSyntax {
                    message: "invalid impl self type".to_string(),
                    span: tokens[cursor].span.clone(),
                })?;
            if after_self != body_open {
                return Err(ParseError::InvalidSyntax {
                    message: "unexpected tokens before impl body".to_string(),
                    span: tokens[after_self].span.clone(),
                });
            }
            (None, self_ty)
        };

        let mut items = Vec::new();
        cursor = body_open + 1;
        while cursor < end - 1 {
            let Some((method, next_cursor)) = self.parse_impl_method(
                tokens,
                cursor,
                end - 1,
                &self_type,
            )?
            else {
                return Err(ParseError::InvalidSyntax {
                    message: "invalid impl item".to_string(),
                    span: tokens[cursor].span.clone(),
                });
            };
            items.push(ast::ImplItemKind::Function(method));
            cursor = next_cursor;
        }

        Ok(ast::ImplItem {
            generics,
            trait_ref,
            self_type,
            items,
        })
    }

    fn parse_impl_method(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
        impl_self_type: &ast::Type,
    ) -> Result<Option<(ast::ImplFunction, usize)>, ParseError> {
        if start >= end {
            return Ok(None);
        }
        let (visibility, item_start) = self.parse_visibility_prefix(tokens, start, end);
        let (_, after_type) = self
            .parse_type_prefix(tokens, item_start, end)
            .ok_or_else(|| ParseError::InvalidSyntax {
                message: "invalid method return type".to_string(),
                span: tokens[item_start.min(end - 1)].span.clone(),
            })?;
        if !matches!(tokens.get(after_type).map(|t| &t.kind), Some(Token::Identifier(_))) {
            return Err(ParseError::InvalidSyntax {
                message: "expected method identifier".to_string(),
                span: tokens[after_type.min(end - 1)].span.clone(),
            });
        }

        let mut cursor = after_type + 1;
        let (_, generics_end) = self.parse_generics_prefix(tokens, cursor, end)?;
        cursor = generics_end;
        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftParen)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected '(' after method name".to_string(),
                span: tokens[cursor.min(end - 1)].span.clone(),
            });
        }
        let Some(rparen) = self.find_matching_token(tokens, cursor, end, Token::LeftParen, Token::RightParen)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "unterminated method parameter list".to_string(),
                span: tokens[cursor].span.clone(),
            });
        };
        let lbrace = rparen + 1;
        let Some(rbrace) = self.find_matching_token(tokens, lbrace, end, Token::LeftBrace, Token::RightBrace)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "unterminated method block".to_string(),
                span: tokens[lbrace.min(end - 1)].span.clone(),
            });
        };
        let method_end = rbrace + 1;
        let function = self.parse_function_reduction(tokens, item_start, method_end)?;
        let method_kind = Self::classify_impl_method(&function.parameters, impl_self_type);
        Ok(Some((
            ast::ImplFunction {
                name: function.name,
                generics: function.generics,
                parameters: function.parameters,
                method_kind,
                visibility,
                return_type: function.return_type,
                body: function.body,
                span: Span {
                    start: tokens[start].span.start,
                    end: tokens[method_end - 1].span.end,
                },
            },
            method_end,
        )))
    }

    fn classify_impl_method(parameters: &[ast::Parameter], impl_self_type: &ast::Type) -> ast::MethodKind {
        let Some(first_param) = parameters.first() else {
            return ast::MethodKind::Static;
        };
        if first_param.name.name != "self" {
            return ast::MethodKind::Static;
        }

        if Self::same_type_shape(&first_param.param_type, impl_self_type) {
            return ast::MethodKind::InstanceValue;
        }

        if let ast::TypeKind::Pointer(pointer) = first_param.param_type.kind.as_ref()
            && Self::same_type_shape(&pointer.inner, impl_self_type)
        {
            return ast::MethodKind::InstancePointer {
                is_mutable: pointer.is_mutable,
            };
        }

        ast::MethodKind::Static
    }

    fn same_type_shape(left: &ast::Type, right: &ast::Type) -> bool {
        match (left.kind.as_ref(), right.kind.as_ref()) {
            (ast::TypeKind::Primitive(a), ast::TypeKind::Primitive(b)) => a == b,
            (ast::TypeKind::Generic(a), ast::TypeKind::Generic(b)) => {
                a.name.name == b.name.name
                    && a.args.len() == b.args.len()
                    && a.args
                        .iter()
                        .zip(b.args.iter())
                        .all(|(x, y)| Self::same_type_shape(x, y))
            }
            (ast::TypeKind::Named(a), ast::TypeKind::Named(b)) => {
                if a.path.len() != b.path.len() {
                    return false;
                }
                if !a.path.iter().zip(b.path.iter()).all(|(x, y)| x.name == y.name) {
                    return false;
                }
                match (&a.generics, &b.generics) {
                    (None, None) => true,
                    (Some(ax), Some(bx)) => {
                        ax.len() == bx.len()
                            && ax
                                .iter()
                                .zip(bx.iter())
                                .all(|(x, y)| Self::same_type_shape(x, y))
                    }
                    _ => false,
                }
            }
            (ast::TypeKind::Reference(a), ast::TypeKind::Reference(b)) => {
                a.is_mutable == b.is_mutable && Self::same_type_shape(&a.inner, &b.inner)
            }
            (ast::TypeKind::Pointer(a), ast::TypeKind::Pointer(b)) => {
                a.is_mutable == b.is_mutable && Self::same_type_shape(&a.inner, &b.inner)
            }
            (ast::TypeKind::Array(a), ast::TypeKind::Array(b)) => {
                Self::same_type_shape(&a.element_type, &b.element_type)
            }
            (ast::TypeKind::Optional(a), ast::TypeKind::Optional(b)) => Self::same_type_shape(a, b),
            (ast::TypeKind::Tuple(a), ast::TypeKind::Tuple(b)) => {
                a.len() == b.len()
                    && a
                        .iter()
                        .zip(b.iter())
                        .all(|(x, y)| Self::same_type_shape(x, y))
            }
            (ast::TypeKind::Function(a), ast::TypeKind::Function(b)) => {
                a.parameters.len() == b.parameters.len()
                    && a
                        .parameters
                        .iter()
                        .zip(b.parameters.iter())
                        .all(|(x, y)| Self::same_type_shape(x, y))
                    && Self::same_type_shape(&a.return_type, &b.return_type)
            }
            _ => false,
        }
    }

    fn parse_function_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::FunctionItem, ParseError> {
        let Some(return_type_token) = tokens.get(start) else {
            return Err(ParseError::InvalidSyntax {
                message: "missing function return type".to_string(),
                span: Span { start: 0, end: 0 },
            });
        };
        let (return_type, return_type_end) = self
            .parse_type_prefix(tokens, start, end)
            .ok_or_else(|| ParseError::InvalidSyntax {
                message: "invalid function return type".to_string(),
                span: return_type_token.span.clone(),
            })?;

        let Some(name_token) = tokens.get(return_type_end) else {
            return Err(ParseError::InvalidSyntax {
                message: "missing function name".to_string(),
                span: return_type_token.span.clone(),
            });
        };
        let Token::Identifier(ref function_name) = name_token.kind else {
            return Err(ParseError::InvalidSyntax {
                message: "expected function identifier".to_string(),
                span: name_token.span.clone(),
            });
        };

        let mut cursor = return_type_end + 1;
        let (mut generics, next) = self.parse_generics_prefix(tokens, cursor, end)?;
        cursor = next;
        let (where_clause, next_after_where) = self.parse_where_clause_prefix(tokens, cursor, end)?;
        cursor = next_after_where;
        generics = self.merge_generics_where(generics, where_clause);
        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftParen)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected '(' after function name".to_string(),
                span: name_token.span.clone(),
            });
        }
        cursor += 1;

        let mut parameters = Vec::new();
        while cursor < end {
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::RightParen)) {
                cursor += 1;
                break;
            }

            let param_type_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected parameter type".to_string(),
                span: name_token.span.clone(),
            })?;
            let (param_type, next_after_type) = self
                .parse_type_prefix(tokens, cursor, end)
                .ok_or_else(|| ParseError::InvalidSyntax {
                    message: "invalid parameter type".to_string(),
                    span: param_type_token.span.clone(),
                })?;
            cursor = next_after_type;

            let param_name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected parameter identifier".to_string(),
                span: param_type_token.span.clone(),
            })?;
            let Token::Identifier(ref parameter_name) = param_name_token.kind else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected parameter identifier".to_string(),
                    span: param_name_token.span.clone(),
                });
            };
            cursor += 1;

            parameters.push(ast::Parameter {
                name: ast::Identifier {
                    name: parameter_name.clone(),
                    span: param_name_token.span.clone(),
                },
                param_type: param_type.clone(),
                is_mutable: false,
                span: Span {
                    start: param_type.span.start,
                    end: param_name_token.span.end,
                },
            });

            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Comma)) {
                cursor += 1;
            }
        }

        let Some(block_end_inclusive) =
            self.find_matching_token(tokens, cursor, end, Token::LeftBrace, Token::RightBrace)
        else {
            return Err(ParseError::InvalidSyntax {
                message: "expected function block".to_string(),
                span: tokens
                    .get(cursor)
                    .map(|t| t.span.clone())
                    .unwrap_or_else(|| name_token.span.clone()),
            });
        };
        let block_end = block_end_inclusive + 1;
        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftBrace)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected function block".to_string(),
                span: tokens
                    .get(cursor)
                    .map(|t| t.span.clone())
                    .unwrap_or_else(|| name_token.span.clone()),
            });
        }
        if block_end != end {
            return Err(ParseError::InvalidSyntax {
                message: "unexpected tokens after function block".to_string(),
                span: tokens
                    .get(block_end)
                    .map(|t| t.span.clone())
                    .unwrap_or_else(|| tokens[end - 1].span.clone()),
            });
        }
        let body = self.parse_block_reduction(tokens, cursor, block_end)?;

        Ok(ast::FunctionItem {
            name: ast::Identifier {
                name: function_name.clone(),
                span: name_token.span.clone(),
            },
            generics,
            parameters,
            return_type: Some(return_type),
            body,
        })
    }

    fn parse_extern_linkage(linkage: &str) -> ast::ExternLinkage {
        match linkage {
            "C" => ast::ExternLinkage::C,
            "Silver" => ast::ExternLinkage::Silver,
            "system" => ast::ExternLinkage::System,
            "Rust" => ast::ExternLinkage::Rust,
            "cdecl" => ast::ExternLinkage::Cdecl,
            "stdcall" => ast::ExternLinkage::Stdcall,
            "fastcall" => ast::ExternLinkage::Fastcall,
            _ => ast::ExternLinkage::C,
        }
    }

    fn extern_block_body_open(&self, tokens: &[LexToken], start: usize) -> Option<usize> {
        if !matches!(tokens.get(start).map(|t| &t.kind), Some(Token::Extern)) {
            return None;
        }
        let mut cursor = start + 1;
        if matches!(
            tokens.get(cursor).map(|t| &t.kind),
            Some(Token::StringLiteral(_))
        ) {
            cursor += 1;
        }
        if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftBrace)) {
            Some(cursor)
        } else {
            None
        }
    }

    fn parse_extern_member_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
        linkage: ast::ExternLinkage,
    ) -> Result<ParsedExternDeclaration, ParseError> {
        let mut cursor = start;

        let (return_type, after_return_type) = self
            .parse_type_prefix(tokens, cursor, end)
            .ok_or_else(|| ParseError::InvalidSyntax {
                message: "invalid extern return type".to_string(),
                span: tokens[cursor].span.clone(),
            })?;
        cursor = after_return_type;

        let name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
            message: "expected extern function name".to_string(),
            span: return_type.span.clone(),
        })?;
        let Token::Identifier(function_name) = &name_token.kind else {
            return Err(ParseError::InvalidSyntax {
                message: "expected extern function name".to_string(),
                span: name_token.span.clone(),
            });
        };
        cursor += 1;

        if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Semicolon)) {
            return Ok(ParsedExternDeclaration::Variable(ast::ExternVariableItem {
                name: ast::Identifier {
                    name: function_name.clone(),
                    span: name_token.span.clone(),
                },
                var_type: return_type,
                linkage,
            }));
        }

        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftParen)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected '(' or ';' after extern declaration name".to_string(),
                span: name_token.span.clone(),
            });
        }
        cursor += 1;

        let mut parameters = Vec::new();
        let mut is_variadic = false;
        while cursor < end {
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::RightParen)) {
                cursor += 1;
                break;
            }

            let is_ellipsis = cursor + 2 < end
                && matches!(tokens[cursor].kind, Token::Dot)
                && matches!(tokens[cursor + 1].kind, Token::Dot)
                && matches!(tokens[cursor + 2].kind, Token::Dot);
            if is_ellipsis {
                is_variadic = true;
                cursor += 3;
                if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::RightParen)) {
                    return Err(ParseError::InvalidSyntax {
                        message: "variadic marker must be last parameter".to_string(),
                        span: tokens[cursor - 1].span.clone(),
                    });
                }
                continue;
            }

            let param_type_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected extern parameter type".to_string(),
                span: name_token.span.clone(),
            })?;
            let (param_type, next_after_type) = self
                .parse_type_prefix(tokens, cursor, end)
                .ok_or_else(|| ParseError::InvalidSyntax {
                    message: "invalid extern parameter type".to_string(),
                    span: param_type_token.span.clone(),
                })?;
            cursor = next_after_type;

            let param_name_token = tokens.get(cursor).ok_or_else(|| ParseError::InvalidSyntax {
                message: "expected extern parameter name".to_string(),
                span: param_type.span.clone(),
            })?;
            let Token::Identifier(parameter_name) = &param_name_token.kind else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected extern parameter name".to_string(),
                    span: param_name_token.span.clone(),
                });
            };
            cursor += 1;

            parameters.push(ast::Parameter {
                name: ast::Identifier {
                    name: parameter_name.clone(),
                    span: param_name_token.span.clone(),
                },
                param_type,
                is_mutable: false,
                span: Span {
                    start: param_type_token.span.start,
                    end: param_name_token.span.end,
                },
            });

            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Comma)) {
                cursor += 1;
            }
        }

        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Semicolon)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected ';' after extern declaration".to_string(),
                span: tokens
                    .get(cursor)
                    .map(|t| t.span.clone())
                    .unwrap_or_else(|| name_token.span.clone()),
            });
        }

        Ok(ParsedExternDeclaration::Function(ast::ExternFunctionItem {
            name: ast::Identifier {
                name: function_name.clone(),
                span: name_token.span.clone(),
            },
            signature: ast::FunctionSignature {
                parameters,
                return_type: Some(return_type),
                is_variadic,
            },
            linkage,
        }))
    }

    fn parse_extern_declaration_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ParsedExternDeclaration, ParseError> {
        let mut cursor = start;
        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Extern)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected 'extern'".to_string(),
                span: tokens[start].span.clone(),
            });
        }
        cursor += 1;

        let linkage = if let Some(abi_token) = tokens.get(cursor) {
            if let Token::StringLiteral(abi_string) = &abi_token.kind {
                cursor += 1;
                Self::parse_extern_linkage(abi_string)
            } else {
                ast::ExternLinkage::C
            }
        } else {
            ast::ExternLinkage::C
        };

        if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftBrace)) {
            return Err(ParseError::InvalidSyntax {
                message: "extern block is not a declaration".to_string(),
                span: tokens[cursor].span.clone(),
            });
        }

        self.parse_extern_member_reduction(tokens, cursor, end, linkage)
    }

    fn parse_extern_block_reduction(
        &mut self,
        tokens: &[LexToken],
        start: usize,
        end: usize,
    ) -> Result<ast::ExternBlockItem, ParseError> {
        let mut cursor = start;
        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Extern)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected 'extern'".to_string(),
                span: tokens[start].span.clone(),
            });
        }
        cursor += 1;

        let linkage = if let Some(abi_token) = tokens.get(cursor) {
            if let Token::StringLiteral(abi_string) = &abi_token.kind {
                cursor += 1;
                Self::parse_extern_linkage(abi_string)
            } else {
                ast::ExternLinkage::C
            }
        } else {
            ast::ExternLinkage::C
        };

        if !matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::LeftBrace)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected '{' after extern ABI".to_string(),
                span: tokens
                    .get(cursor)
                    .map(|t| t.span.clone())
                    .unwrap_or_else(|| tokens[start].span.clone()),
            });
        }

        let body_start = cursor + 1;
        let body_end = end.saturating_sub(1);
        let mut functions = Vec::new();
        let mut variables = Vec::new();
        cursor = body_start;
        while cursor < body_end {
            let Some(member_end) = self.find_statement_terminator(tokens, cursor, body_end).map(|idx| idx + 1) else {
                return Err(ParseError::InvalidSyntax {
                    message: "unterminated extern block declaration".to_string(),
                    span: tokens[cursor].span.clone(),
                });
            };

            match self.parse_extern_member_reduction(tokens, cursor, member_end, linkage.clone())? {
                ParsedExternDeclaration::Function(function) => functions.push(function),
                ParsedExternDeclaration::Variable(variable) => variables.push(variable),
            }

            cursor = member_end;
        }

        Ok(ast::ExternBlockItem {
            linkage,
            functions,
            variables,
        })
    }

    pub fn parse_program(&mut self, tokens: &[LexToken]) -> Result<ast::Program, ParseError> {
        self.memo.clear();

        let span = tokens
            .last()
            .map(|token| token.span.clone())
            .unwrap_or(Span { start: 0, end: 0 });
        let source = self
            .source_name
            .clone()
            .unwrap_or_else(|| "<unknown>".to_string());

        if let Err(message) = self.rule_graph("Program") {
            return Err(ParseError::InvalidSyntax {
                message: format!(
                    "graph parser bootstrap failed for source `{source}`: {message}"
                ),
                span,
            });
        }

        let mut position = 0usize;
        let mut program_attributes = Vec::new();
        let mut items = Vec::new();

        while !Self::at_eof(tokens, position) {
            let (attributes, item_start) = self.parse_attributes_prefix(tokens, position, tokens.len())?;
            let (visibility, item_start) =
                self.parse_visibility_prefix(tokens, item_start, tokens.len());
            if Self::at_eof(tokens, item_start) {
                break;
            }

            let item_end = if matches!(tokens[item_start].kind, Token::Import) {
                self.find_statement_terminator(tokens, item_start, tokens.len())
                    .map(|idx| idx + 1)
            } else if matches!(tokens[item_start].kind, Token::Extern) {
                if let Some(body_open) = self.extern_block_body_open(tokens, item_start) {
                    self.find_matching_token(
                        tokens,
                        body_open,
                        tokens.len(),
                        Token::LeftBrace,
                        Token::RightBrace,
                    )
                    .map(|idx| idx + 1)
                } else {
                    self.find_statement_terminator(tokens, item_start, tokens.len())
                        .map(|idx| idx + 1)
                }
            } else if matches!(
                tokens[item_start].kind,
                Token::Struct | Token::Enum | Token::Trait | Token::Impl
            ) {
                let Some(body_open) = tokens
                    .iter()
                    .enumerate()
                    .take(tokens.len())
                    .skip(item_start)
                    .find_map(|(idx, token)| {
                        if matches!(token.kind, Token::LeftBrace) {
                            Some(idx)
                        } else {
                            None
                        }
                    })
                else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected item body".to_string(),
                        span: tokens[item_start].span.clone(),
                    });
                };
                self.find_matching_token(tokens, body_open, tokens.len(), Token::LeftBrace, Token::RightBrace)
                    .map(|idx| idx + 1)
            } else {
                let (_, after_type) = self.parse_type_prefix(tokens, item_start, tokens.len()).ok_or_else(|| {
                    ParseError::InvalidSyntax {
                        message: format!(
                            "graph parser could not parse item return type at token index {item_start} in source `{source}`"
                        ),
                        span: tokens.get(item_start).map(|t| t.span.clone()).unwrap_or(span.clone()),
                    }
                })?;
                if !matches!(tokens.get(after_type).map(|t| &t.kind), Some(Token::Identifier(_))) {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected item identifier after return type".to_string(),
                        span: tokens.get(after_type).map(|t| t.span.clone()).unwrap_or(span.clone()),
                    });
                }
                let after_name = after_type + 1;
                if matches!(
                    tokens.get(after_name).map(|t| &t.kind),
                    Some(Token::Assign | Token::Semicolon)
                ) {
                    self.find_statement_terminator(tokens, item_start, tokens.len())
                        .map(|idx| idx + 1)
                } else {
                    let mut lparen = after_name;
                    if matches!(tokens.get(lparen).map(|t| &t.kind), Some(Token::Less)) {
                        let Some(generics_close) = self.find_matching_token(
                            tokens,
                            lparen,
                            tokens.len(),
                            Token::Less,
                            Token::Greater,
                        ) else {
                            return Err(ParseError::InvalidSyntax {
                                message: "unterminated generic parameter list".to_string(),
                                span: tokens.get(lparen).map(|t| t.span.clone()).unwrap_or(span.clone()),
                            });
                        };
                        lparen = generics_close + 1;
                    }
                    let Some(rparen) = self.find_matching_token(
                        tokens,
                        lparen,
                        tokens.len(),
                        Token::LeftParen,
                        Token::RightParen,
                    ) else {
                        return Err(ParseError::InvalidSyntax {
                            message: "unterminated function parameter list".to_string(),
                            span: tokens.get(lparen).map(|t| t.span.clone()).unwrap_or(span.clone()),
                        });
                    };
                    let lbrace = rparen + 1;
                    let Some(rbrace) = self.find_matching_token(
                        tokens,
                        lbrace,
                        tokens.len(),
                        Token::LeftBrace,
                        Token::RightBrace,
                    ) else {
                        return Err(ParseError::InvalidSyntax {
                            message: "unterminated function block".to_string(),
                            span: tokens.get(lbrace).map(|t| t.span.clone()).unwrap_or(span.clone()),
                        });
                    };
                    Some(rbrace + 1)
                }
            };

            let Some(item_end) = item_end else {
                return Err(ParseError::InvalidSyntax {
                    message: format!(
                        "graph parser could not find item terminator at token index {position} in source `{source}`"
                    ),
                    span: tokens.get(position).map(|t| t.span.clone()).unwrap_or(span.clone()),
                });
            };

            let item_span = Span {
                start: tokens[position].span.start,
                end: tokens[item_end - 1].span.end,
            };
            let kind = match tokens[item_start].kind {
                Token::Import => {
                    ast::ItemKind::Import(self.parse_import_reduction(tokens, item_start, item_end)?)
                }
                Token::Extern => {
                    if self.extern_block_body_open(tokens, item_start).is_some() {
                        ast::ItemKind::ExternBlock(
                            self.parse_extern_block_reduction(tokens, item_start, item_end)?,
                        )
                    } else {
                        match self.parse_extern_declaration_reduction(tokens, item_start, item_end)? {
                            ParsedExternDeclaration::Function(function_item) => {
                                ast::ItemKind::ExternFunction(function_item)
                            }
                            ParsedExternDeclaration::Variable(variable_item) => {
                                ast::ItemKind::ExternVariable(variable_item)
                            }
                        }
                    }
                }
                Token::Struct => ast::ItemKind::Struct(self.parse_struct_reduction(tokens, item_start, item_end)?),
                Token::Enum => ast::ItemKind::Enum(self.parse_enum_reduction(tokens, item_start, item_end)?),
                Token::Trait => ast::ItemKind::Trait(self.parse_trait_reduction(tokens, item_start, item_end)?),
                Token::Impl => ast::ItemKind::Impl(self.parse_impl_reduction(tokens, item_start, item_end)?),
                _ => {
                    let (_, after_type) = self.parse_type_prefix(tokens, item_start, item_end).ok_or_else(|| {
                        ParseError::InvalidSyntax {
                            message: "expected item type".to_string(),
                            span: tokens.get(item_start).map(|t| t.span.clone()).unwrap_or(span.clone()),
                        }
                    })?;
                    if matches!(
                        tokens.get(after_type + 1).map(|t| &t.kind),
                        Some(Token::Assign | Token::Semicolon)
                    ) {
                        ast::ItemKind::GlobalVariable(
                            self.parse_global_variable_reduction(tokens, item_start, item_end)?,
                        )
                    } else {
                        ast::ItemKind::Function(
                            self.parse_function_reduction(tokens, item_start, item_end)?,
                        )
                    }
                }
            };

            let mut retained = Vec::new();
            for attr in attributes {
                if attr.name.name == "link" {
                    program_attributes.push(attr);
                } else {
                    retained.push(attr);
                }
            }

            items.push(ast::Item {
                kind,
                span: item_span,
                visibility,
                attributes: retained,
            });

            position = item_end;
        }

        let program_span = if items.is_empty() {
            span.clone()
        } else {
            Span {
                start: items.first().map(|item| item.span.start).unwrap_or(span.start),
                end: items.last().map(|item| item.span.end).unwrap_or(span.end),
            }
        };

        Ok(ast::Program {
            attributes: program_attributes,
            items,
            span: program_span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compiled_shape(compiled: &CompiledRuleGraph) -> String {
        let mut lines = vec![format!(
            "rule={} start={} accept={}",
            compiled.rule_name, compiled.start_state, compiled.accept_state
        )];
        for (state_id, state) in compiled.states.iter().enumerate() {
            lines.push(format!("state {state_id}"));
            if state.transitions.is_empty() {
                lines.push("  <none>".to_string());
                continue;
            }
            for transition in &state.transitions {
                let line = match transition {
                    Transition::Epsilon { to } => format!("  epsilon -> {to}"),
                    Transition::MatchToken { token, to } => {
                        format!("  match {:?} -> {to}", token)
                    }
                    Transition::EnterRule { rule_name, to } => {
                        format!("  enter {rule_name} -> {to}")
                    }
                };
                lines.push(line);
            }
        }
        lines.join("\n")
    }

    #[test]
    fn compiles_sequence_rule_to_graph() {
        let mut grammar = GrammarGraph::new();
        grammar.add_rule(
            "Program",
            seq(vec![
                tok(Token::Fn),
                tok(Token::Identifier("main".to_string())),
            ]),
        );

        let compiled = grammar
            .compile_rule("Program")
            .expect("compile should succeed");
        assert!(!compiled.states.is_empty());
        assert!(compiled.start_state < compiled.states.len());
        assert!(compiled.accept_state < compiled.states.len());
    }

    #[test]
    fn sep1_expands_into_sequence_plus_repeat() {
        let expr = sep1(rule("Param"), tok(Token::Comma));
        let GrammarExpr::Seq(parts) = expr else {
            panic!("sep1 should expand into sequence");
        };
        assert_eq!(parts.len(), 2);
        assert!(matches!(parts[1], GrammarExpr::Repeat(_)));
    }

    #[test]
    fn golden_choice_rule_compiles_to_parallel_match_edges() {
        let mut grammar = GrammarGraph::new();
        grammar.add_rule(
            "Alt",
            choice(vec![tok(Token::True), tok(Token::False)]),
        );

        let compiled = grammar.compile_rule("Alt").expect("compile should succeed");
        let got = compiled_shape(&compiled);
        let expected = [
            "rule=Alt start=0 accept=1",
            "state 0",
            "  match True -> 1",
            "  match False -> 1",
            "state 1",
            "  <none>",
        ]
        .join("\n");
        assert_eq!(got, expected);
    }

    #[test]
    fn golden_optional_rule_has_epsilon_and_match_paths() {
        let mut grammar = GrammarGraph::new();
        grammar.add_rule("MaybeConst", opt(tok(Token::Const)));

        let compiled = grammar
            .compile_rule("MaybeConst")
            .expect("compile should succeed");
        let got = compiled_shape(&compiled);
        let expected = [
            "rule=MaybeConst start=0 accept=1",
            "state 0",
            "  epsilon -> 1",
            "  match Const -> 1",
            "state 1",
            "  <none>",
        ]
        .join("\n");
        assert_eq!(got, expected);
    }

    #[test]
    fn golden_repeat_rule_has_loop_and_exit_epsilons() {
        let mut grammar = GrammarGraph::new();
        grammar.add_rule("Stars", repeat(tok(Token::Star)));

        let compiled = grammar.compile_rule("Stars").expect("compile should succeed");
        let got = compiled_shape(&compiled);
        let expected = [
            "rule=Stars start=0 accept=1",
            "state 0",
            "  epsilon -> 1",
            "  match Star -> 2",
            "state 1",
            "  <none>",
            "state 2",
            "  epsilon -> 0",
            "  epsilon -> 1",
        ]
        .join("\n");
        assert_eq!(got, expected);
    }

    #[test]
    fn golden_seq_optional_repeat_shape_is_stable() {
        let mut grammar = GrammarGraph::new();
        grammar.add_rule("TypeTail", seq(vec![opt(tok(Token::Const)), repeat(tok(Token::Star))]));

        let compiled = grammar
            .compile_rule("TypeTail")
            .expect("compile should succeed");
        let got = compiled_shape(&compiled);
        let expected = [
            "rule=TypeTail start=0 accept=1",
            "state 0",
            "  epsilon -> 2",
            "  match Const -> 2",
            "state 1",
            "  <none>",
            "state 2",
            "  epsilon -> 1",
            "  match Star -> 3",
            "state 3",
            "  epsilon -> 2",
            "  epsilon -> 1",
        ]
        .join("\n");
        assert_eq!(got, expected);
    }
}
