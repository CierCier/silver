; Later patterns take precedence, so general rules come first.

; ---- Identifiers by convention (bodies are token groups) ----

((identifier) @type
  (#match? @type "^[A-Z][A-Za-z0-9]*[a-z][A-Za-z0-9]*$"))

((identifier) @constant
  (#match? @constant "^[A-Z][A-Z0-9_]+$"))

((identifier) @variable.special
  (#eq? @variable.special "self"))

(self) @variable.special

; A name directly followed by an argument list is a call.
(_
  (identifier) @function
  .
  (parenthesized))

(_
  "."
  .
  (identifier) @property)

(_
  "."
  .
  (identifier) @function
  .
  (parenthesized))

(macro_invocation
  "@" @function
  name: (identifier) @function)

; ---- Types ----

(type_identifier) @type
(primitive_type) @type.builtin
(self_type) @type.builtin
(lifetime) @label

; ---- Definitions ----

(function_definition
  name: (identifier) @function)

(macro_definition
  name: (identifier) @function)

(struct_definition
  name: (identifier) @type)

(enum_definition
  name: (identifier) @type)

(trait_definition
  name: (identifier) @type)

(type_alias
  name: (identifier) @type)

(type_parameter
  name: (identifier) @type)

(enum_variant
  name: (identifier) @variant)

(field_declaration
  name: (identifier) @property)

(associated_value
  name: (identifier) @property)

(parameter
  name: (identifier) @variable.parameter)

(variable_declaration
  name: (identifier) @variable)

(import_item
  name: (identifier) @variable)

; ---- Literals ----

(string_literal) @string
(escape_sequence) @string.escape
(char_literal) @string
(number_literal) @number
(boolean_literal) @boolean

; ---- Attributes ----

(attribute) @attribute

; ---- Keywords ----

[
  "as"
  "asm"
  "break"
  "cast"
  "comptime"
  "const"
  "continue"
  "defer"
  "else"
  "enum"
  "extern"
  "for"
  "if"
  "impl"
  "import"
  "in"
  "launch"
  "let"
  "lock"
  "macro"
  "match"
  "move"
  "mut"
  "private"
  "return"
  "shared"
  "static"
  "struct"
  "trait"
  "type"
  "volatile"
  "wait"
  "while"
] @keyword

(visibility) @keyword
(where) @keyword

; ---- Operators and punctuation ----

[
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "!"
  "="
  "=>"
  "+="
  "-="
  "*="
  "/="
  "%="
  "&="
  "|="
  "^="
  "<<="
  ">>="
  "<<"
  ">>"
  "&"
  "|"
  "^"
  "~"
  "++"
  "--"
  ".."
  "..."
  "?"
] @operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

(type_parameters
  [
    "<"
    ">"
  ] @punctuation.bracket)

(type_arguments
  [
    "<"
    ">"
  ] @punctuation.bracket)

[
  ","
  ";"
  "."
  ":"
  "::"
] @punctuation.delimiter

; ---- Comments ----

[
  (line_comment)
  (block_comment)
] @comment

((line_comment) @comment.doc
  (#match? @comment.doc "^///"))

((block_comment) @comment.doc
  (#match? @comment.doc "^/\\*\\*"))
