use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Loc {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeName {
    pub name: String,
    pub pointer_depth: u32,
    pub array_dims: Vec<Option<u64>>,
    pub generic_args: Vec<TypeName>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String),
    Int(u64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Unary {
        op: Token,
        rhs: Box<Expr>,
    },
    Binary {
        op: Token,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Assign {
        op: Token,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Cond {
        cond: Box<Expr>,
        then_e: Box<Expr>,
        else_e: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
        generic_args: Vec<TypeName>,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Member {
        base: Box<Expr>,
        member: String,
        ptr: bool,
    },
    MethodCall {
        base: Box<Expr>,
        method: String,
        args: Vec<Expr>,
        ptr: bool,
    },
    Comptime(Box<Expr>),
    AddressOf(Box<Expr>),
    Deref(Box<Expr>),
    Cast {
        expr: Box<Expr>,
        target: TypeName,
    },
    New {
        target: TypeName,
    },
    Drop(Box<Expr>),
    Alloc {
        target: TypeName,
        count: Option<Box<Expr>>,
    },
    Free(Box<Expr>),
    InitList(Vec<InitItem>),
    Type(TypeName),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InitItem {
    pub designator: Option<Box<Expr>>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Box<Expr>),
    Return(Option<Box<Expr>>),
    Decl(StmtDecl),
    Block(Vec<Stmt>),
    For {
        init: Option<Box<Stmt>>,
        cond: Option<Box<Expr>>,
        iter: Option<Box<Expr>>,
        body: Box<Stmt>,
    },
    If {
        cond: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Stmt>,
    },
    Break,
    Continue,
    Asm {
        code: String,
        is_volatile: bool,
    },
    Switch {
        cond: Box<Expr>,
        cases: Vec<Case>,
        default_case: Option<Box<Stmt>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtDecl {
    pub ty: TypeName,
    pub declarators: Vec<Declarator>,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declarator {
    pub name: String,
    pub array_dims: Vec<Option<u64>>,
    pub init: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub values: Vec<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub ty: TypeName,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub ty: TypeName,
    pub names: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumItem {
    pub name: String,
    pub value: Option<u64>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Struct {
        name: String,
        fields: Vec<StructField>,
        generic_params: Vec<String>,
        attributes: Vec<Attribute>,
    },
    Enum {
        name: String,
        items: Vec<EnumItem>,
    },
    Var {
        ty: TypeName,
        declarators: Vec<Declarator>,
        is_extern: bool,
        is_static: bool,
        is_const: bool,
    },
    Func {
        ret: TypeName,
        name: String,
        params: Vec<Param>,
        body: Option<Vec<Stmt>>, // Block
        is_extern: bool,
        is_variadic: bool,
        generic_params: Vec<String>,
    },
    Cast {
        target: TypeName,
        params: Vec<Param>,
        body: Option<Vec<Stmt>>,
        is_implicit: bool,
    },
    Impl {
        ty: TypeName,
        methods: Vec<Decl>,
    },
    Import {
        path: Vec<String>,
    },
    Link {
        lib: String,
    },
    Trait {
        name: String,
        generic_params: Vec<String>,
        methods: Vec<TraitMethod>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethod {
    pub return_type: TypeName,
    pub name: String,
    pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decls: Vec<Decl>,
}
