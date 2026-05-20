# Full Parser Expression Coverage — Design Spec

## Context

The Silver parser (`prt_parser.rs`) has a functional core for items, functions, structs, enums, traits, impls, and basic expressions. However, several AST variants are defined but never produced by the parser, and several statement forms are missing. This spec covers bringing the parser to feature parity with the existing AST.

## Scope

### In Scope
1. `break` / `continue` statement parsing
2. Block expressions as expressions
3. Array/tuple literals (`[...]` syntax)
4. Struct literal expressions (`Point { x: 1 }`)
5. Match expressions with range patterns
6. Macro definitions (`macro name ...`) and invocation (`@NAME(...)`)
7. Tuple-like and struct-like enum variants
8. Imported type name discovery for statement disambiguation
9. Shift operator precedence level
10. Associated types in impls
11. Range patterns in match (`..` inclusive, `...` exclusive)

### Out of Scope
- Codegen for new expression forms (already exists or deferred)
- Type checker changes beyond match completeness validation
- Semantic analysis of macros (invocation is parsed, expansion is later)

---

## 1. Lexer Additions (`lexer.rs`)

### New Tokens

| Token | Syntax | Description |
|-------|--------|-------------|
| `DotDot` | `..` | Inclusive range operator |
| `DotDotDot` | `...` | Exclusive range operator |
| `Macro` | `macro` | Macro definition keyword |

### Changes

Modify the `'.'` character handler at line ~279 to check for `..` and `...` before emitting `Dot`:

```
'.' → peek next char:
  if '.' → peek again:
    if '.' → Token::DotDotDot
    else → Token::DotDot
  else → Token::Dot
```

Add `Macro` to the identifier/keyword scanner alongside existing keywords.

---

## 2. AST Additions (`ast.rs`)

### `ItemKind` — Add `Macro`

```rust
pub struct MacroDef {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

// In ItemKind enum:
Macro(MacroDef),
```

### `PatternKind` — Add `Range`

```rust
Range {
    start: Expression,
    end: Expression,
    inclusive: bool,  // true for .., false for ...
},
```

### Existing AST (No Changes Needed)

The following already exist and just need parser support:
- `ExpressionKind::Array(Vec<Expression>)`
- `ExpressionKind::Tuple(Vec<Expression>)`
- `ExpressionKind::Block(Block)`
- `ExpressionKind::StructLiteral { path, fields }`
- `ExpressionKind::Match { expression, arms }`
- `ExpressionKind::MacroCall { name, args }`
- `EnumVariantData::Tuple(Vec<Type>)`
- `EnumVariantData::Struct(Vec<Field>)`
- `StatementKind::Break(Option<Expression>)`
- `StatementKind::Continue`
- `ImplItemKind::AssociatedType(ImplAssociatedType)`
- `BinaryOperator::LeftShift`, `BinaryOperator::RightShift`

---

## 3. Statement Layer: `break` / `continue`

### Location
`parse_block_reduction` (~line 2732 in `prt_parser.rs`)

### Behavior

Add dispatch branches before the generic statement terminator logic:

```
break;           → StatementKind::Break(None)
break expr;      → StatementKind::Break(Some(expr))
continue;        → StatementKind::Continue
```

Both are terminated by semicolons. `break` optionally takes an expression parsed via `parse_expression_reduction`.

### Parse Flow
1. Check `Token::Break` or `Token::Continue` at cursor
2. For `break`: advance, check if next token is `;` → `Break(None)`, otherwise parse expression → `Break(Some(expr))`
3. For `continue`: advance → `Continue`
4. Consume semicolon, advance cursor, continue loop

---

## 4. Expression Layer: New Primary Forms

### Location
`parse_primary` function inside `parse_expression_reduction` (~line 1762)

### 4a. Block Expressions

**Syntax:** `{ let x = 1; x + 1 }`

**Disambiguation from initializers:**
- If first token after `{` is `.` → designated initializer (existing logic)
- If first token after `{` is `[` followed by digit and `]` → designated array initializer (existing logic)
- If first token after `{` is `}` → empty block
- Otherwise → block expression

**Implementation:**
- `Token::LeftBrace` → call `parse_block_reduction` → `ExpressionKind::Block(Block)`

### 4b. Array/Tuple Literals

**Syntax:** `[1, 2, 3]`

Both arrays and tuples use the same `[...]` syntax. The parser produces `ExpressionKind::Array(Vec<Expression>)`. The type system distinguishes array vs tuple based on context.

**Implementation:**
- `Token::LeftBracket` → parse comma-separated expressions until `RightBracket`
- → `ExpressionKind::Array(Vec<Expression>)`

### 4c. Struct Literals

**Syntax:**
```
Point { x: 1, y: 2 }
Point { .x = 1, .y = 2 }
```

**Implementation:**
- After parsing an identifier as a primary expression, check if next token is `LeftBrace`
- If yes → parse struct literal fields → `ExpressionKind::StructLiteral { path, fields }`
- Field syntax: `name: expr` (colon form) or `.name = expr` (designated form)
- Trailing comma allowed

### 4d. Match Expressions

**Syntax:**
```silver
match x {
    1: println("one")
    2..10: println("inclusive range")
    2...10: println("exclusive range")
    "hello": println("greeting")
    _: println("default")
}
```

**Arm body:** Either a single statement or a block.

**Implementation:**
1. `Token::Identifier("match")` → advance
2. Parse scrutinee expression via `parse_expression_reduction`
3. Expect `LeftBrace`
4. Parse arms in loop until `RightBrace`:
   - Parse pattern (see Pattern section below)
   - Expect `:` (colon)
   - Parse body: if `LeftBrace` → `parse_block_reduction`, otherwise → parse single statement up to next arm or closing brace
5. → `ExpressionKind::Match { expression, arms }`

**Pattern parsing:**
- **Literal:** `Token::IntLiteral`, `Token::FloatLiteral`, `Token::StringLiteral`, `Token::CharLiteral`, `Token::True`, `Token::False` → `PatternKind::Literal`
- **Wildcard:** `Token::Identifier("_")` → `PatternKind::Wildcard`
- **Identifier binding:** `Token::Identifier(name)` where `name != "_"` → `PatternKind::Identifier`
- **Range:** `expr DotDot expr` or `expr DotDotDot expr` → `PatternKind::Range { start, end, inclusive }`

**Note:** The parser does NOT enforce that a `_` default arm exists. This is a semantic error handled by the type checker.

### 4e. Macro Invocations

**Syntax:**
```silver
@MACRO(args)
@MACRO
```

**Implementation:**
- `Token::At` → advance
- Parse identifier (macro name)
- If next token is `LeftParen` → parse arguments as `MacroArg` variants → `ExpressionKind::MacroCall { name, args }`
- Otherwise → `ExpressionKind::MacroCall { name, args: [] }`

**MacroArg parsing:** Attempt to parse as expression first; the full `MacroArg` enum (Expression, Type, Pattern, Statement, Item, Literal, Identifier) is available for future expansion.

---

## 5. Top-Level: Macro Definitions

### Location
`parse_program` — add `Token::Macro` to transition table and implement `parse_macro_reduction`

### Syntax
```silver
macro name body              // simple substitution, no params
macro name() { statements }  // parameterized, empty params
macro name(a, b) { statements }  // parameterized
```

### Implementation
```rust
fn parse_macro_reduction(&mut self, tokens: &[LexToken], start: usize, end: usize) -> Result<ast::MacroDef, ParseError>
```

1. Advance past `macro` keyword
2. Parse name (identifier)
3. If next token is `LeftParen` → parse comma-separated parameters → advance past `RightParen`
4. If next token is `LeftBrace` → parse body via `parse_block_reduction`
5. Otherwise → treat remaining tokens as a single-statement body
6. Return `MacroDef { name, parameters, body }`

### Transition Table
Add `(NonTerminal::Item, vec![TokenClass::Macro])` → `ItemProduction::Macro`

Add `TokenClass::Macro` to the enum.

---

## 6. Enum Variants: Tuple and Struct Forms

### Location
`parse_enum_reduction` (~line 3689)

### Syntax
```silver
enum Color {
    Red;                        // unit
    Rgb { i8 r; i8 g; i8 b; }  // struct-like
    Named(str);                 // tuple-like
}
```

### Implementation
After parsing the variant name identifier:
1. If next token is `LeftParen` → parse comma-separated types → `EnumVariantData::Tuple(Vec<Type>)` → expect `RightParen` → expect `Semicolon`
2. If next token is `LeftBrace` → parse semicolon-terminated field declarations (reuse `parse_declarator_group` pattern) → `EnumVariantData::Struct(Vec<Field>)` → expect `RightBrace` → expect `Semicolon`
3. Otherwise → expect `Semicolon` → `EnumVariantData::Unit`

---

## 7. Type Discovery: Imported Types

### Location
`seed_known_types_from_tokens` (~line 5035)

### Problem
`File f = File.open(...)` fails because `File` isn't in `known_type_names`. The scanner only finds types defined with `struct`/`enum`/`trait` in the current file.

### Fix
After scanning for struct/enum/trait names, also scan `import` statements:

- `import std.io;` → add `io` to `known_type_names`
- `import std.io as io;` → add `io` to `known_type_names`
- `import std.io::{File, println};` → add `File` to `known_type_names`

This is a lexical scan (not semantic resolution), so it's approximate but sufficient for statement/type disambiguation.

### Implementation
Add a second pass in `seed_known_types_from_tokens` that scans for `Token::Import` at top level (depth == 0) and extracts:
1. The last path segment as a type name
2. Any `{ ... }` imported names as type names
3. Any `as` alias as a type name

---

## 8. Shift Operators

### Location
Expression precedence chain in `parse_expression_reduction`

### Implementation
Add a new precedence level between `parse_additive` and `parse_relational`:

```
parse_relational → parse_shift → parse_additive → ...
```

```rust
fn parse_shift(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_additive(cursor)?;
    loop {
        let Some(token) = cursor.current() else { break };
        let operator = match token.kind {
            Token::LeftShift => Some(ast::BinaryOperator::LeftShift),
            Token::RightShift => Some(ast::BinaryOperator::RightShift),
            _ => None,
        };
        let Some(operator) = operator else { break };
        cursor.bump();
        let rhs = parse_additive(cursor)?;
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary { left: Box::new(expr), operator, right: Box::new(rhs) }),
            span: Span { start: expr.span.start, end: rhs.span.end },
        };
    }
    Ok(expr)
}
```

---

## 9. Associated Types in Impls

### Location
`parse_impl_reduction` (~line 4113), inside the impl body parsing loop

### Syntax
```silver
impl Add for Point {
    type Output = i32;
    fn add(self, other: Point) -> Point { ... }
}
```

### Implementation
In the impl body parsing loop, add a branch:
- If `Token::Identifier("type")` → parse associated type item → `ImplItemKind::AssociatedType`
- If `Token::Cast` → parse cast item → `ImplItemKind::Cast`
- Otherwise → parse method → `ImplItemKind::Function`

**Associated type parsing:**
1. Advance past `type`
2. Parse name (identifier)
3. If `=` → parse type via `parse_type_prefix`
4. Expect `;`

---

## 10. Type Checker: Match Completeness

### Location
`typeck.rs` — add match expression validation

### Behavior
When type-checking a `match` expression:

1. Determine the scrutinee type
2. If the scrutinee is an **enum type**:
   - Collect all enum variants
   - Check that each variant is covered by at least one arm
   - Error if any variant is unhandled
3. If the scrutinee is an **open type** (integers, strings, floats, etc.):
   - Require a `_` wildcard arm
   - Error if no default arm exists
4. If the scrutinee type cannot be determined:
   - Require a `_` wildcard arm (conservative)

---

## 11. Testing Strategy

### Unit Tests (in `prt_parser.rs` tests module)

| Test | Validates |
|------|-----------|
| `parses_break_statement` | `break;` and `break expr;` in blocks |
| `parses_continue_statement` | `continue;` in blocks |
| `parses_array_literal` | `[1, 2, 3]` |
| `parses_block_expression` | `{ let x = 1; x }` |
| `parses_struct_literal` | `Point { x: 1, y: 2 }` |
| `parses_match_expression` | `match x { 1: a, _: b }` |
| `parses_match_with_range_patterns` | `2..10:` and `2...10:` |
| `parses_macro_definition` | `macro name { body }` and `macro name() { body }` |
| `parses_macro_invocation` | `@MACRO(args)` and `@MACRO` |
| `parses_enum_tuple_variant` | `Named(str);` |
| `parses_enum_struct_variant` | `Rgb { i8 r; i8 g; i8 b; }` |
| `parses_imported_type_disambiguation` | `import std.io; File f = ...;` |
| `parses_shift_operators` | `x << 2`, `y >> 1` |
| `parses_impl_associated_type` | `type Output = i32;` |

### Example Validation

| Example | Unblocked By |
|---------|-------------|
| `control_flow.ag` | break/continue |
| `file_handling.ag` | imported type discovery |
| `initializers.ag` | struct literal parsing |
| `array_init.ag` | array literal parsing |

---

## Files Modified

| File | Changes |
|------|---------|
| `agc/src/lexer.rs` | Add `DotDot`, `DotDotDot`, `Macro` tokens |
| `agc/src/parser/ast.rs` | Add `MacroDef`, `Range` pattern variant |
| `agc/src/parser/prt_parser.rs` | All parser implementations |
| `agc/src/semantic/typeck.rs` | Match completeness validation |
