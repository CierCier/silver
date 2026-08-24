// Abstract Syntax Tree node definitions for Silver language

use crate::lexer::{CommentKind, Span};
use rustc_hash::FxHashMap;

/// Top-level AST node representing a complete Silver program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub attributes: Vec<Attribute>,
    pub items: Vec<Item>,
    /// Comments captured by the lexer in source order, kept as a separate AST
    /// node so tooling (LLVM IR comments, the aglsp docstrings) can dispatch
    /// them without them interfering with parsing.
    pub comments: Vec<CommentItem>,
    pub span: Span,
}

/// A comment captured from the source: line (`//`), doc line (`///`),
/// block (`/* */`), or doc block (`/** */`), with the delimiter-stripped
/// content and its source span.
#[derive(Debug, Clone, PartialEq)]
pub struct CommentItem {
    pub kind: CommentKind,
    pub text: String,
    pub span: Span,
}

impl Program {
    /// The doc comment (`///` or `/** */`) immediately preceding `item` in
    /// source order, with consecutive doc lines joined by newlines. Returns
    /// None when the item has no doc comment, when a non-doc comment or
    /// another item separates them, or when the comment lives inside another
    /// item (e.g. a body comment). Used by LLVM IR comment emission and by
    /// tooling (aglsp) for hover docstrings.
    pub fn doc_comment_for(&self, item: &Item) -> Option<String> {
        let mut lines: Vec<&str> = Vec::new();
        for comment in &self.comments {
            if comment.span.end > item.span.start {
                break; // comments are sorted in source order
            }
            let inside_item = self.items.iter().any(|other| {
                !std::ptr::eq(other, item)
                    && other.span.start <= comment.span.start
                    && comment.span.end <= other.span.end
            });
            let separated = self.items.iter().any(|other| {
                !std::ptr::eq(other, item)
                    && other.span.start >= comment.span.end
                    && other.span.start < item.span.start
            });
            if inside_item || separated || !comment.kind.is_doc() {
                lines.clear();
            } else {
                lines.push(comment.text.trim());
            }
        }
        if lines.is_empty() {
            None
        } else {
            Some(lines.join("\n"))
        }
    }
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
pub struct TypeAliasItem {
    pub name: Identifier,
    pub type_def: Type,
}

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
    Macro(MacroDef),
    TypeAlias(TypeAliasItem),
}

/// Macro definition
#[derive(Debug, Clone, PartialEq)]
pub struct MacroDef {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

/// Function definition
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub is_variadic: bool,
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
    pub is_static: bool,
    pub is_volatile: bool,
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
    /// Bare identifiers in `self_type` that this impl intends as generic
    /// parameters, decided at PARSE time from file-local type knowledge
    /// (`impl Result<T, E>` records `[T, E]`; `impl Wrapper<MyStruct>`
    /// records nothing for `MyStruct`). Typeck must use this list instead
    /// of re-inferring against the global type registry, where a user
    /// struct named like a parameter would otherwise win the collision —
    /// generic parameters shadow global types inside their context.
    pub implicit_type_params: Vec<String>,
}

/// Import statement.
/// `import std.io;` imports every public item from the module. The selective
/// form `import std.io { print, println as pln };` imports only the listed
/// items, each optionally renamed (`local_name`).
#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub path: Vec<Identifier>,
    /// `None` for whole-module imports; `Some(items)` for selective imports.
    pub selection: Option<Vec<ImportedName>>,
}

/// One name inside a selective import: `print` or `println as pln`.
#[derive(Debug, Clone, PartialEq)]
pub struct ImportedName {
    /// Name as exported by the module.
    pub name: Identifier,
    /// Local alias when written `name as alias`; equals `name` otherwise.
    pub local_name: Identifier,
}

/// External function declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ExternFunctionItem {
    pub name: Identifier,
    pub signature: FunctionSignature,
    pub linkage: ExternLinkage,
    /// Per-declaration attributes (e.g. `#[link_name("strlen")]` inside an
    /// `extern "C" { }` block).
    pub attributes: Vec<Attribute>,
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
    /// Dotted path, e.g. `cpu.sse41` in `#[cfg(cpu.sse41)]`.
    Path(Vec<Identifier>),
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
    Slice(Box<SliceType>),
    Array(Box<ArrayType>),
    Optional(Box<Type>),
    Function(FunctionType),
    Tuple(Vec<Type>),
}

impl TypeKind {
    /// True for the bare `Self` type name (receiver-sugar marker).
    pub fn is_self_named(&self) -> bool {
        matches!(
            self,
            TypeKind::Named(named) if named.path.len() == 1 && named.path[0].name == "Self"
        )
    }
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
    /// Pointee volatility (`volatile T*`): loads/stores through this pointer
    /// are emitted as volatile, so they cannot be reordered, cached, or
    /// eliminated by the optimizer (MMIO/device memory, video buffers).
    pub is_volatile: bool,
    pub inner: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SliceType {
    pub element_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub size: i64,
    pub span: Span,
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
/// Fill omitted trailing type arguments for a local struct or enum reference.
/// The caller supplies only local aggregate definitions and owns diagnostics.
pub fn apply_generic_defaults(
    ty: &mut Type,
    defaults: &[(String, Vec<Option<Type>>)],
) -> Result<(), String> {
    fn visit(ty: &mut Type, defaults: &[(String, Vec<Option<Type>>)]) -> Result<(), String> {
        match ty.kind.as_mut() {
            TypeKind::Named(named) => {
                if let Some(args) = named.generics.as_mut() {
                    for arg in args {
                        visit(arg, defaults)?;
                    }
                }
                for (name, params) in defaults {
                    if named.path.len() != 1 || named.path[0].name != *name {
                        continue;
                    }
                    let args = named.generics.get_or_insert_with(Vec::new);
                    if args.len() > params.len() {
                        return Err(format!(
                            "too many type arguments for '{}': expected at most {}, found {}",
                            name,
                            params.len(),
                            args.len()
                        ));
                    }
                    for index in args.len()..params.len() {
                        let Some(default) = &params[index] else {
                            return Err(format!(
                                "missing required type argument {} for '{}'",
                                index + 1,
                                name
                            ));
                        };
                        let mut default = default.clone();
                        visit(&mut default, defaults)?;
                        args.push(default);
                    }
                    break;
                }
            }
            TypeKind::Generic(generic) => {
                for arg in &mut generic.args {
                    visit(arg, defaults)?;
                }
            }
            TypeKind::Reference(reference) => visit(&mut reference.inner, defaults)?,
            TypeKind::Pointer(pointer) => visit(&mut pointer.inner, defaults)?,
            TypeKind::Slice(slice) => visit(&mut slice.element_type, defaults)?,
            TypeKind::Array(array) => visit(&mut array.element_type, defaults)?,
            TypeKind::Optional(inner) => visit(inner, defaults)?,
            TypeKind::Function(function) => {
                for param in &mut function.parameters {
                    visit(param, defaults)?;
                }
                visit(&mut function.return_type, defaults)?;
            }
            TypeKind::Tuple(items) => {
                for item in items {
                    visit(item, defaults)?;
                }
            }
            TypeKind::Primitive(_) => {}
        }
        Ok(())
    }
    visit(ty, defaults)
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitBound {
    pub trait_ref: TraitRef,
    pub is_optional: bool,
}

/// Named aggregate field metadata.
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Identifier,
    pub field_type: Type,
    pub visibility: Visibility,
    pub tags: FxHashMap<String, String>,
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
    AssociatedFunctionValue(AssociatedFunctionValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssociatedFunctionValue {
    pub name: Identifier,
    pub fn_type: FunctionType,
    pub span: Span,
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
    Function(Box<ImplFunction>),
    AssociatedType(ImplAssociatedType),
    Cast(ImplCast),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplFunction {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub is_variadic: bool,
    pub parameters: Vec<Parameter>,
    pub method_kind: MethodKind,
    pub visibility: Visibility,
    pub return_type: Option<Type>,
    pub body: Block,
    /// Per-method attributes (e.g. `#[inline(always)]`, `#[cfg(...)]`).
    pub attributes: Vec<Attribute>,
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

// (ImportedItem removed — selective imports are not supported)

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
#[expect(
    clippy::large_enum_variant,
    reason = "variant sizes are inherent to the AST; boxing ripples through the compiler and LSP for no gain"
)]
pub enum StatementKind {
    Block(Block),
    Expression(Expression),
    Let(LetStatement),
    Return(Option<Expression>),
    Break(Option<Expression>),
    Continue,
    Defer(Box<Statement>),
}

/// Let statements (variable declarations)
#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub pattern: Pattern,
    pub type_annotation: Option<Type>,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
    pub is_static: bool,
    pub is_volatile: bool,
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
    /// `move x` binding: extracts the value OUT of the matched location,
    /// zeroing the source so ownership transfers (used on enum payloads:
    /// `Event(move ev)` takes the payload without leaving a dangling copy).
    Move(Identifier),
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
    Range {
        start: Expression,
        end: Expression,
        inclusive: bool,
    },
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IterAccessMode {
    ByValue,
    ByPtr,
    ByConstPtr,
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
    /// `cond ? then_expr : else_expr` — the first value-producing
    /// conditional: both branches must have the same type.
    Ternary {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    /// `value ? fallback` — unwrap Optional<T>, Result<T, E>, or nullable pointer
    /// yielding the inner T/pointer on success or evaluating fallback on absence/error.
    UnwrapOr {
        value: Box<Expression>,
        fallback: Box<Expression>,
    },
    While {
        condition: Box<Expression>,
        body: Block,
    },
    ForIn {
        binding: Identifier,
        is_mutable: bool,
        iterable: Box<Expression>,
        body: Block,
        item_type: Option<Box<Type>>,
        mode: IterAccessMode,
        iterator_type: Option<Box<Type>>,
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
    Asm {
        code: String,
        inputs: Vec<Expression>,
        /// Extra clobber registers beyond the hardcoded `rcx`/`r11`
        /// (e.g. `["rbx", "rdx"]` for `cpuid`). Names without braces.
        clobbers: Vec<String>,
    },
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
    /// `launch f(args...)` — spawn a detached OS thread running the wrapped
    /// call, moving every argument into the child. Yields a `Task<T>` handle
    /// (T = callee return type).
    Launch(Box<Expression>),
    /// `wait t` — join the Task `t` (consuming it) and yield its result.
    Wait(Box<Expression>),
    EnumVariant {
        path: Vec<Identifier>,
        variant: Identifier,
        fields: Vec<Expression>,
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
    Range,
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
    Dereference,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    #[test]
    fn comments_are_captured_and_docs_attach_to_items() {
        let source = "/// Adds two integers.\n/// Second line.\ni32 add(i32 a, i32 b) { return a + b; }\n\n// not a doc\ni32 main() { return add(1, 2); }\n";
        let tokens = lex(source).expect("lex ok");
        let mut parser = crate::parser::Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        assert_eq!(
            program.comments.len(),
            3,
            "all comments captured: {:?}",
            program.comments
        );

        let add = program
            .items
            .iter()
            .find(|item| matches!(&item.kind, ItemKind::Function(f) if f.name.name == "add"))
            .expect("add fn");
        let main = program
            .items
            .iter()
            .find(|item| matches!(&item.kind, ItemKind::Function(f) if f.name.name == "main"))
            .expect("main fn");

        assert_eq!(
            program.doc_comment_for(add).as_deref(),
            Some("Adds two integers.\nSecond line."),
            "consecutive doc lines join"
        );
        assert_eq!(
            program.doc_comment_for(main),
            None,
            "plain comment is not a doc"
        );
    }

    #[test]
    fn inner_comments_do_not_attach_to_following_items() {
        let source =
            "i32 f() {\n    /// inner\n    return 0;\n}\n/// real doc\ni32 g() { return 1; }\n";
        let tokens = lex(source).expect("lex ok");
        let mut parser = crate::parser::Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");

        let g = program
            .items
            .iter()
            .find(|item| matches!(&item.kind, ItemKind::Function(f) if f.name.name == "g"))
            .expect("g fn");
        assert_eq!(
            program.doc_comment_for(g).as_deref(),
            Some("real doc"),
            "top-level doc attaches, inner body comment does not leak"
        );
    }
}
