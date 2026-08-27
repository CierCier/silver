//! PRT (Predictive Reduction Table) machinery: the grammar DSL and the
//! top-level item prediction table. The parser itself (token walking,
//! item/statement/expression reduction) lives in `prt_parser` and
//! `expr_parser`.

use rustc_hash::FxHashMap as HashMap;

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum GrammarAtom {
    Token(Token),
    Rule(String),
}

#[derive(Debug, Clone)]
pub enum GrammarExpr {
    Seq(Vec<GrammarExpr>),      // Simple Sequence as [1, 2, ,3] => 1 then 2 then 3
    Choice(Vec<GrammarExpr>),   // Choice of Rules [Import, Fucntion, Definition] => 1 | 2 | 3
    Optional(Box<GrammarExpr>), // Optional, pretty self explanatory
    Repeat(Box<GrammarExpr>),   // Klein Closure Repeat => 0 or more times
    Repeat1(Box<GrammarExpr>),  // + Closure Releat => 1 or more times
    Atom(GrammarAtom),          // Basic Unit,
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
pub struct GrammarSpec {
    pub rules: HashMap<String, GrammarRule>,
}

impl Default for GrammarSpec {
    fn default() -> Self {
        Self::new()
    }
}

impl GrammarSpec {
    pub fn new() -> Self {
        Self {
            rules: HashMap::default(),
        }
    }

    pub fn add_rule(&mut self, name: impl Into<String>, expr: GrammarExpr) {
        let name = name.into();
        self.rules.insert(name.clone(), GrammarRule { name, expr });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum NonTerminal {
    Item,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum TokenClass {
    Import,
    Extern,
    Struct,
    Enum,
    Trait,
    Impl,
    TypeStart,
    Macro,
    TypeKeyword,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ItemProduction {
    Import,
    ExternDeclaration,
    ExternBlock,
    Struct,
    Enum,
    Trait,
    Impl,
    Function,
    GlobalVariable,
    Macro,
    TypeAlias,
}

#[derive(Debug, Clone)]
pub(crate) struct TransitionTable {
    pub(crate) max_lookahead: usize,
    rows: HashMap<(NonTerminal, Vec<TokenClass>), ItemProduction>,
}

impl TransitionTable {
    pub(crate) fn for_bootstrap(max_lookahead: usize) -> Self {
        let mut rows = HashMap::default();
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::Import]),
            ItemProduction::Import,
        );
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::Extern]),
            ItemProduction::ExternDeclaration,
        );
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::Struct]),
            ItemProduction::Struct,
        );
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::Enum]),
            ItemProduction::Enum,
        );
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::Trait]),
            ItemProduction::Trait,
        );
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::Impl]),
            ItemProduction::Impl,
        );
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::TypeStart]),
            ItemProduction::Function,
        );
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::Macro]),
            ItemProduction::Macro,
        );
        rows.insert(
            (NonTerminal::Item, vec![TokenClass::TypeKeyword]),
            ItemProduction::TypeAlias,
        );
        Self {
            max_lookahead: max_lookahead.max(1),
            rows,
        }
    }

    pub(crate) fn predict_item(&self, lookahead: &[TokenClass]) -> Option<ItemProduction> {
        let width_limit = self.max_lookahead.min(lookahead.len());
        for width in (1..=width_limit).rev() {
            if let Some(prod) = self
                .rows
                .get(&(NonTerminal::Item, lookahead[..width].to_vec()))
                .copied()
            {
                return Some(prod);
            }
        }
        None
    }
}
