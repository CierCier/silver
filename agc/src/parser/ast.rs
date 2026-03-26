// Abstract Syntax Tree node definitions for Silver language

use crate::lexer::Span;

/// Top-level AST node representing a complete Silver program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub attributes: Vec<Attribute>,
    pub items: Vec<Item>,
    pub span: Span,
}

/// Top-level items in a Silver program
#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
    pub visibility: Visibility,
    pub attributes: Vec<Attribute>,
}

/// Different kinds of top-level items
#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Function(FunctionItem),
    GlobalVariable(GlobalVariableItem),
    Struct(StructItem),
    Enum(EnumItem),
    Impl(ImplItem),
    Trait(TraitItem),
    Import(ImportItem),
    ExternFunction(ExternFunctionItem),
    ExternVariable(ExternVariableItem),
    ExternBlock(ExternBlockItem),
}

/// Function definition
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
}

/// Top-level global variable declaration
#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariableItem {
    pub name: Identifier,
    pub var_type: Type,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
}

/// Struct definition
#[derive(Debug, Clone, PartialEq)]
pub struct StructItem {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub fields: Vec<Field>,
}

/// Enum definition
#[derive(Debug, Clone, PartialEq)]
pub struct EnumItem {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub variants: Vec<EnumVariant>,
}

/// Trait definition
#[derive(Debug, Clone, PartialEq)]
pub struct TraitItem {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub super_traits: Vec<TraitBound>,
    pub items: Vec<TraitItemKind>,
}

/// Implementation block
#[derive(Debug, Clone, PartialEq)]
pub struct ImplItem {
    pub generics: Option<Generics>,
    pub trait_ref: Option<TraitRef>,
    pub self_type: Type,
    pub items: Vec<ImplItemKind>,
}

/// Import statement
#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub path: Vec<Identifier>,
    pub alias: Option<Identifier>,
    pub items: Option<Vec<ImportedItem>>,
}

/// External function declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ExternFunctionItem {
    pub name: Identifier,
    pub signature: FunctionSignature,
    pub linkage: ExternLinkage,
}

/// External variable declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ExternVariableItem {
    pub name: Identifier,
    pub var_type: Type,
    pub linkage: ExternLinkage,
}

/// External block with multiple function declarations
#[derive(Debug, Clone, PartialEq)]
pub struct ExternBlockItem {
    pub linkage: ExternLinkage,
    pub functions: Vec<ExternFunctionItem>,
    pub variables: Vec<ExternVariableItem>,
}

/// External linkage types
#[derive(Debug, Clone, PartialEq)]
pub enum ExternLinkage {
    C,        // extern "C" - C ABI
    Silver,   // extern "Silver" - Silver ABI
    System,   // extern "system" - System ABI (Windows)
    Rust,     // extern "Rust" - Rust ABI (for interop)
    Cdecl,    // extern "cdecl" - Explicit cdecl
    Stdcall,  // extern "stdcall" - Windows stdcall
    Fastcall, // extern "fastcall" - Fast calling convention
}

/// Visibility modifiers
#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

/// Attributes (like #[derive(...)])
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: Identifier,
    pub args: Vec<AttributeArg>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttributeArg {
    Identifier(Identifier),
    Literal(Literal),
}

/// Identifiers
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

/// Generic parameters
#[derive(Debug, Clone, PartialEq)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub where_clause: Option<WhereClause>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericParam {
    Type(TypeParam),
    Lifetime(LifetimeParam),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: Identifier,
    pub bounds: Vec<TraitBound>,
    pub default: Option<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LifetimeParam {
    pub name: Identifier,
    pub bounds: Vec<Lifetime>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum WherePredicate {
    Type {
        bounded_type: Type,
        bounds: Vec<TraitBound>,
    },
    Lifetime {
        lifetime: Lifetime,
        bounds: Vec<Lifetime>,
    },
}

/// Function parameters
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub param_type: Type,
    pub is_mutable: bool,
    pub span: Span,
}

/// Function signature (for extern functions)
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub is_variadic: bool,
}

/// Type expressions
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: Box<TypeKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    Named(NamedType),
    Generic(GenericType),
    Reference(ReferenceType),
    Pointer(PointerType),
    Array(Box<ArrayType>),
    Optional(Box<Type>),
    Function(FunctionType),
    Tuple(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    // Signed integers
    I8,
    I16,
    I32,
    I64,
    I128,
    // Unsigned integers
    U8,
    U16,
    U32,
    U64,
    U128,
    // Floating point
    F32,
    F64,
    F80,
    // Complex numbers
    C32,
    C64,
    C80,
    // Other primitives
    Bool,
    Str,
    Char,
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedType {
    pub path: Vec<Identifier>,
    pub generics: Option<Vec<Type>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericType {
    pub name: Identifier,
    pub args: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceType {
    pub is_mutable: bool,
    pub lifetime: Option<Lifetime>,
    pub inner: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PointerType {
    pub is_mutable: bool,
    pub inner: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub size: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,
}

/// Lifetime annotations
#[derive(Debug, Clone, PartialEq)]
pub struct Lifetime {
    pub name: String,
    pub span: Span,
}

/// Trait references and bounds
#[derive(Debug, Clone, PartialEq)]
pub struct TraitRef {
    pub path: Vec<Identifier>,
    pub generics: Option<Vec<Type>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitBound {
    pub trait_ref: TraitRef,
    pub is_optional: bool,
}

/// Struct fields
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Identifier,
    pub field_type: Type,
    pub visibility: Visibility,
    pub span: Span,
}

/// Enum variants
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Identifier,
    pub data: EnumVariantData,
    pub discriminant: Option<i128>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantData {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<Field>),
}

/// Trait items
#[derive(Debug, Clone, PartialEq)]
pub enum TraitItemKind {
    Function(TraitFunction),
    AssociatedType(AssociatedType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitFunction {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub default_body: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssociatedType {
    pub name: Identifier,
    pub bounds: Vec<TraitBound>,
    pub default: Option<Type>,
    pub span: Span,
}

/// Implementation items
#[derive(Debug, Clone, PartialEq)]
pub enum ImplItemKind {
    Function(ImplFunction),
    AssociatedType(ImplAssociatedType),
    Cast(ImplCast),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplFunction {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub parameters: Vec<Parameter>,
    pub method_kind: MethodKind,
    pub visibility: Visibility,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MethodKind {
    Static,
    InstanceValue,
    InstancePointer { is_mutable: bool },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplAssociatedType {
    pub name: Identifier,
    pub type_def: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplCast {
    pub target_type: Type,
    pub parameters: Vec<Parameter>,
    pub body: Block,
    pub span: Span,
}

/// Import items
#[derive(Debug, Clone, PartialEq)]
pub struct ImportedItem {
    pub name: Identifier,
    pub alias: Option<Identifier>,
}

/// Code blocks
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

/// Statements
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Block(Block),
    Expression(Expression),
    Let(LetStatement),
    Return(Option<Expression>),
    Break(Option<Expression>),
    Continue,
}

/// Let statements (variable declarations)
#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub pattern: Pattern,
    pub type_annotation: Option<Type>,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
}

/// Patterns for destructuring
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Identifier(Identifier),
    Tuple(Vec<Pattern>),
    Struct {
        path: Vec<Identifier>,
        fields: Vec<FieldPattern>,
    },
    Enum {
        path: Vec<Identifier>,
        variant: Identifier,
        data: Option<Box<Pattern>>,
    },
    Literal(Literal),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldPattern {
    pub name: Identifier,
    pub pattern: Option<Pattern>,
}

/// Expressions
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: Box<ExpressionKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
    Identifier(Identifier),
    TypeName(Type),
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Postfix {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    MethodCall {
        receiver: Box<Expression>,
        method: Identifier,
        arguments: Vec<Expression>,
    },
    FieldAccess {
        object: Box<Expression>,
        field: Identifier,
    },
    Index {
        object: Box<Expression>,
        index: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    While {
        condition: Box<Expression>,
        body: Block,
    },
    For {
        init: LetStatement,
        condition: Box<Expression>,
        increment: Box<Expression>,
        body: Block,
    },
    Match {
        expression: Box<Expression>,
        arms: Vec<MatchArm>,
    },
    Block(Block),
    Initializer {
        items: Vec<InitializerItem>,
    },
    Asm(String),
    Array(Vec<Expression>),
    Tuple(Vec<Expression>),
    StructLiteral {
        path: Vec<Identifier>,
        fields: Vec<FieldInit>,
    },
    Cast {
        expression: Box<Expression>,
        target_type: Box<Type>,
    },
    Move(Box<Expression>),
    Reference {
        is_mutable: bool,
        expression: Box<Expression>,
    },
    Comptime(Box<Expression>),
    MacroCall {
        name: Identifier,
        args: Vec<MacroArg>,
    },
}

/// Literals
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    Complex(f64, f64),
    String(String),
    Char(char),
    Bool(bool),
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
    BitwiseNot,
    Increment,
    Decrement,
}

/// Match arms
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: Expression,
    pub span: Span,
}

/// Field initialization in struct literals
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
    pub name: Identifier,
    pub value: Expression,
}

/// Initializer items (C-style designated or positional)
#[derive(Debug, Clone, PartialEq)]
pub enum InitializerItem {
    Positional(Expression),
    Field {
        name: Identifier,
        value: Expression,
    },
    Index {
        index: Expression,
        value: Expression,
    },
}

/// Macro arguments
#[derive(Debug, Clone, PartialEq)]
pub enum MacroArg {
    Expression(Expression),
    Type(Type),
    Pattern(Pattern),
    Statement(Statement),
    Item(Item),
    Literal(Literal),
    Identifier(Identifier),
}
