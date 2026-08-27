//! Typed AST projection layer over the Elise SourceGraph / Green Tree.
//!
//! Provides position-aware, zero-copy, typed views for all Silver items,
//! statements, expressions, and attributes directly over `NodeRef`.

use elise_core::{AstChildren, AstNode, NodeRef, SourceGraph};
use super::body::BodyKind;
use super::lexspec::Tok;
use super::parser::NodeKind;

/// Typed view over the root file node.
#[derive(Debug, Clone, Copy)]
pub struct AstSourceFile<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstSourceFile<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::File as u16 {
            Some(AstSourceFile(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

impl<'g> AstSourceFile<'g> {
    /// Wrap a full [`SourceGraph`] as a typed root AST view.
    pub fn from_graph(graph: &'g SourceGraph) -> Self {
        AstSourceFile(graph.root())
    }

    /// Iterator over all top-level items in the file.
    pub fn items(&self) -> impl Iterator<Item = AstItem<'g>> {
        self.0.children().filter_map(AstItem::cast)
    }

    /// Iterator over function declarations in the file.
    pub fn functions(&self) -> AstChildren<'g, AstFunction<'g>> {
        self.children()
    }

    /// Iterator over struct declarations in the file.
    pub fn structs(&self) -> AstChildren<'g, AstStruct<'g>> {
        self.children()
    }

    /// Iterator over enum declarations in the file.
    pub fn enums(&self) -> AstChildren<'g, AstEnum<'g>> {
        self.children()
    }

    /// Iterator over trait declarations in the file.
    pub fn traits(&self) -> AstChildren<'g, AstTrait<'g>> {
        self.children()
    }

    /// Iterator over impl blocks in the file.
    pub fn impls(&self) -> AstChildren<'g, AstImpl<'g>> {
        self.children()
    }

    /// Iterator over imports in the file.
    pub fn imports(&self) -> AstChildren<'g, AstImport<'g>> {
        self.children()
    }

    /// Iterator over global variable declarations in the file.
    pub fn globals(&self) -> AstChildren<'g, AstGlobalVar<'g>> {
        self.children()
    }
}

/// Any top-level declaration or statement item.
#[derive(Debug, Clone, Copy)]
pub enum AstItem<'g> {
    Import(AstImport<'g>),
    ExternDecl(AstExternDecl<'g>),
    ExternBlock(AstExternBlock<'g>),
    Struct(AstStruct<'g>),
    Enum(AstEnum<'g>),
    Trait(AstTrait<'g>),
    Impl(AstImpl<'g>),
    Macro(AstMacro<'g>),
    TypeAlias(AstTypeAlias<'g>),
    Function(AstFunction<'g>),
    Global(AstGlobalVar<'g>),
}

impl<'g> AstItem<'g> {
    pub fn cast(node: NodeRef<'g>) -> Option<Self> {
        if node.is_leaf() {
            return None;
        }
        let kind = node.kind();
        if kind == NodeKind::Import as u16 {
            Some(AstItem::Import(AstImport(node)))
        } else if kind == NodeKind::ExternDecl as u16 {
            Some(AstItem::ExternDecl(AstExternDecl(node)))
        } else if kind == NodeKind::ExternBlock as u16 {
            Some(AstItem::ExternBlock(AstExternBlock(node)))
        } else if kind == NodeKind::Struct as u16 {
            Some(AstItem::Struct(AstStruct(node)))
        } else if kind == NodeKind::Enum as u16 {
            Some(AstItem::Enum(AstEnum(node)))
        } else if kind == NodeKind::Trait as u16 {
            Some(AstItem::Trait(AstTrait(node)))
        } else if kind == NodeKind::Impl as u16 {
            Some(AstItem::Impl(AstImpl(node)))
        } else if kind == NodeKind::Macro as u16 {
            Some(AstItem::Macro(AstMacro(node)))
        } else if kind == NodeKind::TypeAlias as u16 {
            Some(AstItem::TypeAlias(AstTypeAlias(node)))
        } else if kind == NodeKind::Function as u16 {
            Some(AstItem::Function(AstFunction(node)))
        } else if kind == NodeKind::GlobalVariable as u16 {
            Some(AstItem::Global(AstGlobalVar(node)))
        } else {
            None
        }
    }

    pub fn raw(&self) -> NodeRef<'g> {
        match self {
            AstItem::Import(it) => it.0,
            AstItem::ExternDecl(it) => it.0,
            AstItem::ExternBlock(it) => it.0,
            AstItem::Struct(it) => it.0,
            AstItem::Enum(it) => it.0,
            AstItem::Trait(it) => it.0,
            AstItem::Impl(it) => it.0,
            AstItem::Macro(it) => it.0,
            AstItem::TypeAlias(it) => it.0,
            AstItem::Function(it) => it.0,
            AstItem::Global(it) => it.0,
        }
    }

    #[inline]
    pub fn span(&self) -> (usize, usize) {
        self.raw().span()
    }

    #[inline]
    pub fn text(&self) -> &'g str {
        self.raw().text()
    }
}

// ---------------------------------------------------------------------------
// Item View Implementations
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
pub struct AstFunction<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstFunction<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::Function as u16 {
            Some(AstFunction(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

impl<'g> AstFunction<'g> {
    /// Return the declared function name identifier if present.
    pub fn name(&self) -> Option<&'g str> {
        let mut prev_ident: Option<&'g str> = None;
        for child in self.0.children() {
            if child.kind() == Tok::LParen as u16 {
                return prev_ident;
            }
            if child.kind() == Tok::Ident as u16 {
                prev_ident = Some(child.text());
            }
        }
        prev_ident
    }

    /// True if marked with `pub`.
    pub fn is_pub(&self) -> bool {
        self.0
            .children()
            .any(|c| c.kind() == Tok::Ident as u16 && c.text() == "pub")
    }

    /// Iterator over attributes applied to this function.
    pub fn attributes(&self) -> AstChildren<'g, AstAttribute<'g>> {
        self.children()
    }

    /// Iterator over statements in the function body.
    pub fn statements(&self) -> impl Iterator<Item = AstStmt<'g>> {
        self.0.children().filter_map(AstStmt::cast)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstStruct<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstStruct<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::Struct as u16 {
            Some(AstStruct(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

impl<'g> AstStruct<'g> {
    pub fn name(&self) -> Option<&'g str> {
        let mut struct_kw = false;
        for child in self.0.children() {
            if child.kind() == Tok::Struct as u16 {
                struct_kw = true;
                continue;
            }
            if struct_kw && child.kind() == Tok::Ident as u16 {
                return Some(child.text());
            }
        }
        None
    }

    pub fn is_pub(&self) -> bool {
        self.0
            .children()
            .any(|c| c.kind() == Tok::Ident as u16 && c.text() == "pub")
    }

    pub fn attributes(&self) -> AstChildren<'g, AstAttribute<'g>> {
        self.children()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstEnum<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstEnum<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::Enum as u16 {
            Some(AstEnum(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

impl<'g> AstEnum<'g> {
    pub fn name(&self) -> Option<&'g str> {
        let mut enum_kw = false;
        for child in self.0.children() {
            if child.kind() == Tok::Enum as u16 {
                enum_kw = true;
                continue;
            }
            if enum_kw && child.kind() == Tok::Ident as u16 {
                return Some(child.text());
            }
        }
        None
    }

    pub fn is_pub(&self) -> bool {
        self.0
            .children()
            .any(|c| c.kind() == Tok::Ident as u16 && c.text() == "pub")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstTrait<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstTrait<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::Trait as u16 {
            Some(AstTrait(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

impl<'g> AstTrait<'g> {
    pub fn name(&self) -> Option<&'g str> {
        let mut trait_kw = false;
        for child in self.0.children() {
            if child.kind() == Tok::Trait as u16 {
                trait_kw = true;
                continue;
            }
            if trait_kw && child.kind() == Tok::Ident as u16 {
                return Some(child.text());
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstImpl<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstImpl<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::Impl as u16 {
            Some(AstImpl(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

impl<'g> AstImpl<'g> {
    /// Iterator over methods inside the impl block.
    pub fn methods(&self) -> AstChildren<'g, AstFunction<'g>> {
        self.children()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstImport<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstImport<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::Import as u16 {
            Some(AstImport(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstGlobalVar<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstGlobalVar<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::GlobalVariable as u16 {
            Some(AstGlobalVar(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstExternDecl<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstExternDecl<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::ExternDecl as u16 {
            Some(AstExternDecl(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstExternBlock<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstExternBlock<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::ExternBlock as u16 {
            Some(AstExternBlock(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstMacro<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstMacro<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::Macro as u16 {
            Some(AstMacro(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstTypeAlias<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstTypeAlias<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::TypeAlias as u16 {
            Some(AstTypeAlias(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AstAttribute<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstAttribute<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && node.kind() == NodeKind::Attribute as u16 {
            Some(AstAttribute(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

// ---------------------------------------------------------------------------
// Body & Statement Views
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
pub struct AstBlock<'g>(pub NodeRef<'g>);

impl<'g> AstNode<'g> for AstBlock<'g> {
    fn cast(node: NodeRef<'g>) -> Option<Self> {
        if !node.is_leaf() && (node.kind() == BodyKind::Block as u16 || node.kind() == BodyKind::Body as u16) {
            Some(AstBlock(node))
        } else {
            None
        }
    }

    #[inline]
    fn raw(&self) -> NodeRef<'g> {
        self.0
    }
}

impl<'g> AstBlock<'g> {
    /// Iterator over statements within the block.
    pub fn statements(&self) -> impl Iterator<Item = AstStmt<'g>> {
        self.0.children().filter_map(AstStmt::cast)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AstStmt<'g> {
    Let(NodeRef<'g>),
    Return(NodeRef<'g>),
    Break(NodeRef<'g>),
    Continue(NodeRef<'g>),
    If(NodeRef<'g>),
    While(NodeRef<'g>),
    ForC(NodeRef<'g>),
    ForIn(NodeRef<'g>),
    Match(NodeRef<'g>),
    Expr(NodeRef<'g>),
    Block(NodeRef<'g>),
    Defer(NodeRef<'g>),
}

impl<'g> AstStmt<'g> {
    pub fn cast(node: NodeRef<'g>) -> Option<Self> {
        if node.is_leaf() {
            return None;
        }
        let kind = node.kind();
        if kind == BodyKind::StmtLet as u16 || kind == BodyKind::StmtLocalDecl as u16 {
            Some(AstStmt::Let(node))
        } else if kind == BodyKind::StmtReturn as u16 {
            Some(AstStmt::Return(node))
        } else if kind == BodyKind::StmtBreak as u16 {
            Some(AstStmt::Break(node))
        } else if kind == BodyKind::StmtContinue as u16 {
            Some(AstStmt::Continue(node))
        } else if kind == BodyKind::StmtIf as u16 {
            Some(AstStmt::If(node))
        } else if kind == BodyKind::StmtWhile as u16 {
            Some(AstStmt::While(node))
        } else if kind == BodyKind::StmtForC as u16 {
            Some(AstStmt::ForC(node))
        } else if kind == BodyKind::StmtForIn as u16 {
            Some(AstStmt::ForIn(node))
        } else if kind == BodyKind::StmtMatch as u16 {
            Some(AstStmt::Match(node))
        } else if kind == BodyKind::StmtExpr as u16 {
            Some(AstStmt::Expr(node))
        } else if kind == BodyKind::Block as u16 {
            Some(AstStmt::Block(node))
        } else if kind == BodyKind::Defer as u16 {
            Some(AstStmt::Defer(node))
        } else {
            None
        }
    }

    #[inline]
    pub fn span(&self) -> (usize, usize) {
        match self {
            AstStmt::Let(n)
            | AstStmt::Return(n)
            | AstStmt::Break(n)
            | AstStmt::Continue(n)
            | AstStmt::If(n)
            | AstStmt::While(n)
            | AstStmt::ForC(n)
            | AstStmt::ForIn(n)
            | AstStmt::Match(n)
            | AstStmt::Expr(n)
            | AstStmt::Block(n)
            | AstStmt::Defer(n) => n.span(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::parse_ag;

    #[test]
    fn test_typed_ast_file_projection() {
        let src = r#"
            import std.io;

            #[inline]
            pub struct Point {
                i64 x;
                i64 y;
            }

            enum Status {
                Active,
                Inactive,
            }

            pub i64 calculate(i64 a, i64 b) {
                let sum = a + b;
                return sum;
            }
        "#;

        let graph = parse_ag(src);
        assert!(!graph.has_errors());

        let file = AstSourceFile::from_graph(&graph);

        // Count imports
        let imports: Vec<_> = file.imports().collect();
        assert_eq!(imports.len(), 1);

        // Find struct
        let structs: Vec<_> = file.structs().collect();
        assert_eq!(structs.len(), 1);
        assert_eq!(structs[0].name(), Some("Point"));
        assert!(structs[0].is_pub());

        // Find enum
        let enums: Vec<_> = file.enums().collect();
        assert_eq!(enums.len(), 1);
        assert_eq!(enums[0].name(), Some("Status"));

        // Find function
        let functions: Vec<_> = file.functions().collect();
        assert_eq!(functions.len(), 1);
        assert_eq!(functions[0].name(), Some("calculate"));
        assert!(functions[0].is_pub());

        // Inspect body statements
        let stmts: Vec<_> = functions[0].statements().collect();
        assert_eq!(stmts.len(), 2);
    }
}
