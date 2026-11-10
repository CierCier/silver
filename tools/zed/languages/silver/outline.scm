(struct_definition
  (visibility)? @context
  "struct" @context
  name: (identifier) @name) @item

(field_declaration
  name: (identifier) @name) @item

(enum_definition
  (visibility)? @context
  "enum" @context
  name: (identifier) @name) @item

(enum_variant
  name: (identifier) @name) @item

(trait_definition
  (visibility)? @context
  "trait" @context
  name: (identifier) @name) @item

(impl_block
  "impl" @context
  trait: (_)? @name
  "for"? @context
  type: (_) @name) @item

(type_alias
  "type" @context
  name: (identifier) @name) @item

(function_definition
  return_type: (_) @context
  name: (identifier) @name
  parameters: (parameters
    "(" @context
    ")" @context)) @item

; Custom casts have no name: `cast f64(Vector2 self)`.
(function_definition
  "cast" @context
  return_type: (_) @name
  parameters: (parameters
    "(" @context
    ")" @context)) @item

(macro_definition
  "macro" @context
  name: (identifier) @name) @item

(source_file
  (variable_declaration
    type: (_) @context
    name: (identifier) @name) @item)

(associated_value
  name: (identifier) @name) @item
