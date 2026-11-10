/**
 * Tree-sitter grammar for the Silver programming language.
 *
 * It mirrors the compiler's structural parser: items, types, and parameter
 * lists are parsed precisely, while bodies and initializers are nested token
 * groups. Editors get exact outlines, folds, and indentation, and the grammar
 * stays stable while a body is half-typed. Expression-level meaning comes from
 * the language server.
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PRIMITIVE_TYPES = [
  'i8', 'i16', 'i32', 'i64', 'i128',
  'u8', 'u16', 'u32', 'u64', 'u128',
  'f32', 'f64', 'f80', 'c32', 'c64', 'c80',
  'bool', 'str', 'char', 'void',
];

// Keywords that can appear inside a body. `Self`, `true`, and `false` are
// named nodes instead.
const BODY_KEYWORDS = [
  'struct', 'enum', 'impl', 'trait', 'mut', 'const', 'static', 'volatile',
  'if', 'else', 'while', 'for', 'match', 'break', 'continue', 'return',
  'type', 'let', 'defer', 'import', 'comptime', 'cast', 'move', 'extern',
  'private', 'asm', 'as', 'in', 'macro', 'launch', 'wait',
];

const OPERATORS = [
  '+', '-', '*', '/', '%', '==', '!=', '<', '>', '<=', '>=', '&&', '||', '!',
  '=', '=>', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
  '<<', '>>', '&', '|', '^', '~', '++', '--', '..', '...', '?', '#',
];

/** @param {RuleOrLiteral} separator @param {RuleOrLiteral} rule */
const sepBy1 = (separator, rule) => seq(rule, repeat(seq(separator, rule)));
/** @param {RuleOrLiteral} separator @param {RuleOrLiteral} rule */
const sepBy = (separator, rule) => optional(sepBy1(separator, rule));
/** Comma-separated list that tolerates a trailing comma. */
/** @param {RuleOrLiteral} rule */
const commaList = rule => optional(seq(sepBy1(',', rule), optional(',')));

module.exports = grammar({
  name: 'silver',

  externals: $ => [$.block_comment],

  extras: $ => [/\s/, $.line_comment, $.block_comment],

  word: $ => $.identifier,

  conflicts: $ => [
    // `macro swap(a, b)`: `swap` is the name, or a return type that starts a
    // function type. The parameter list decides.
    [$._type_identifier, $.macro_definition],
    // `macro [T] Vec<T> vec(...)`: a leading bracket group is the macro's type
    // parameters, not a slice return type.
    [$.macro_type_parameters, $.bracket_type],
    [$.type_parameter, $._type_identifier],
  ],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.import_declaration,
      $.struct_definition,
      $.enum_definition,
      $.trait_definition,
      $.impl_block,
      $.type_alias,
      $.extern_block,
      $.macro_definition,
      $.function_definition,
      $.variable_declaration,
      $.conditional_items,
      $.attribute,
      ';',
    ),

    // ---- Imports -----------------------------------------------------------

    import_declaration: $ => seq(
      'import',
      field('path', $.module_path),
      optional($.import_selection),
      ';',
    ),

    module_path: $ => sepBy1('.', $.identifier),

    import_selection: $ => seq('{', commaList($.import_item), '}'),

    import_item: $ => seq(
      field('name', $.identifier),
      optional(seq('as', field('alias', $.identifier))),
    ),

    // ---- Attributes and conditional compilation ----------------------------

    attribute: $ => seq('#', '[', repeat($._token), ']'),

    // Top-level `if (@cfg(...)) { items } else { items }`.
    conditional_items: $ => seq(
      'if',
      field('condition', $.parenthesized),
      field('consequence', $.item_block),
      optional(seq('else', field('alternative', choice($.conditional_items, $.item_block)))),
    ),

    item_block: $ => seq('{', repeat($._item), '}'),

    // ---- Type definitions --------------------------------------------------

    struct_definition: $ => seq(
      optional($.visibility),
      'struct',
      field('name', $.identifier),
      optional(field('type_parameters', $.type_parameters)),
      optional($.where_clause),
      choice(field('body', $.field_list), ';'),
    ),

    field_list: $ => seq('{', repeat(choice($.field_declaration, $.attribute)), '}'),

    // One type may declare several fields: `i32 x, y;`.
    field_declaration: $ => seq(
      optional($.visibility),
      field('type', $._type),
      sepBy1(',', $._field_declarator),
      ';',
    ),

    _field_declarator: $ => seq(
      field('name', $.identifier),
      repeat($.array_dimension),
      // Serialization tag: `i32 id "json:id";`.
      optional(field('tag', $.string_literal)),
      optional(seq('=', field('value', $.initializer))),
    ),

    enum_definition: $ => seq(
      optional($.visibility),
      'enum',
      field('name', $.identifier),
      optional(field('type_parameters', $.type_parameters)),
      optional($.where_clause),
      field('body', $.variant_list),
    ),

    variant_list: $ => seq('{', repeat(choice($.enum_variant, $.attribute)), '}'),

    // Unit `A;`, tuple `B(f64);`, and struct `C { f64 w; }` variants.
    enum_variant: $ => choice(
      seq(
        field('name', $.identifier),
        optional(field('payload', $.variant_types)),
        optional(seq('=', field('value', $.initializer))),
        ';',
      ),
      seq(field('name', $.identifier), field('payload', $.field_list), optional(';')),
    ),

    variant_types: $ => seq('(', commaList($.parameter), ')'),

    trait_definition: $ => seq(
      optional($.visibility),
      'trait',
      field('name', $.identifier),
      optional(field('type_parameters', $.type_parameters)),
      optional(seq(':', field('bounds', $.type_bounds))),
      optional($.where_clause),
      field('body', $.member_list),
    ),

    impl_block: $ => seq(
      'impl',
      optional(field('type_parameters', $.type_parameters)),
      choice(
        seq(field('trait', $._type), 'for', field('type', $._type)),
        field('type', $._type),
      ),
      optional($.where_clause),
      field('body', $.member_list),
    ),

    member_list: $ => seq('{', repeat($._member), '}'),

    _member: $ => choice(
      $.function_definition,
      $.type_alias,
      $.variable_declaration,
      $.associated_value,
      $.attribute,
      ';',
    ),

    // Trait requirement written name-first: `handler: i32(i32);`.
    associated_value: $ => seq(field('name', $.identifier), ':', field('type', $._type), ';'),

    type_alias: $ => seq(
      optional($.visibility),
      'type',
      field('name', $.identifier),
      optional(field('type_parameters', $.type_parameters)),
      optional(seq('=', field('type', $._type))),
      ';',
    ),

    // ---- Functions, macros, externs ----------------------------------------

    function_definition: $ => seq(
      repeat($._modifier),
      choice(
        seq(
          field('return_type', $._type),
          field('name', $.identifier),
          optional(field('type_parameters', $.type_parameters)),
        ),
        // Custom cast: `cast f64(Vector2 self) { ... }`.
        seq('cast', field('return_type', $._type)),
      ),
      field('parameters', $.parameters),
      optional($.where_clause),
      choice(field('body', $.block), ';'),
    ),

    _modifier: $ => choice($.visibility, $.linkage, 'static', 'comptime'),

    visibility: _ => 'private',

    linkage: $ => seq('extern', optional($.string_literal)),

    extern_block: $ => seq(
      'extern',
      optional($.string_literal),
      field('body', $.item_block),
    ),

    macro_definition: $ => seq(
      'macro',
      optional(field('type_parameters', $.macro_type_parameters)),
      optional(field('return_type', $._type)),
      field('name', $.identifier),
      field('parameters', $.parameters),
      field('body', $.block),
    ),

    macro_type_parameters: $ => prec.dynamic(1, seq('[', commaList($.type_parameter), ']')),

    parameters: $ => seq('(', commaList(choice($.self_parameter, $.parameter, $.variadic)), ')'),

    // `self`, `&self`, `&mut self`, and the raw-pointer shorthand `*self`.
    self_parameter: $ => seq(
      optional(choice('*', seq('&', optional('mut')))),
      $.self,
    ),

    self: _ => 'self',

    // The name is optional: extern signatures and function types omit it, and
    // untyped macro parameters are a lone identifier.
    parameter: $ => seq(
      field('type', $._type),
      optional('...'),
      optional(field('name', $.identifier)),
      repeat($.array_dimension),
      optional(seq('=', field('default', $.initializer))),
    ),

    variadic: _ => '...',

    where_clause: $ => seq(
      alias('where', $.where),
      sepBy1(',', seq($._type, ':', $.type_bounds)),
    ),

    type_parameters: $ => seq('<', commaList($.type_parameter), '>'),

    type_parameter: $ => seq(
      field('name', choice($.identifier, $.lifetime)),
      optional(seq(':', field('bounds', $.type_bounds))),
      optional(seq('=', field('default', $._type))),
    ),

    type_bounds: $ => sepBy1('+', $._type),

    // ---- Variables ---------------------------------------------------------

    variable_declaration: $ => seq(
      repeat($._modifier),
      field('type', $._type),
      field('name', $.identifier),
      repeat($.array_dimension),
      optional(seq('=', field('value', $.initializer))),
      ';',
    ),

    array_dimension: $ => seq('[', repeat($._token), ']'),

    // Everything up to the terminating `;` or `,`.
    initializer: $ => repeat1($._initializer_token),

    // ---- Types -------------------------------------------------------------

    _type: $ => choice(
      $.primitive_type,
      $.self_type,
      $._type_identifier,
      $.scoped_type,
      $.generic_type,
      $.pointer_type,
      $.reference_type,
      $.qualified_type,
      $.bracket_type,
      $.function_type,
    ),

    primitive_type: _ => choice(...PRIMITIVE_TYPES),

    self_type: _ => 'Self',

    _type_identifier: $ => alias($.identifier, $.type_identifier),

    scoped_type: $ => seq(
      field('scope', choice($._type_identifier, $.scoped_type, $.generic_type, $.self_type)),
      choice('.', '::'),
      field('name', $._type_identifier),
    ),

    generic_type: $ => seq(
      field('type', choice($._type_identifier, $.scoped_type)),
      field('arguments', $.type_arguments),
    ),

    type_arguments: $ => seq('<', commaList(choice($._type, $.lifetime, $.number_literal)), '>'),

    pointer_type: $ => prec.left(seq($._type, '*')),

    reference_type: $ => prec.right(1, seq('&', optional($.lifetime), optional('mut'), $._type)),

    qualified_type: $ => prec.right(seq(choice('const', 'mut', 'volatile'), $._type)),

    // `[T]` is a slice and `[A, B]` a tuple.
    bracket_type: $ => seq('[', commaList($._type), ']'),

    function_type: $ => prec.left(seq(
      field('return_type', $._type),
      field('parameters', $.parameters),
    )),

    // ---- Bodies: nested token groups ---------------------------------------

    block: $ => seq('{', repeat($._token), '}'),
    parenthesized: $ => seq('(', repeat($._token), ')'),
    bracketed: $ => seq('[', repeat($._token), ']'),

    _token: $ => choice($._initializer_token, ';', ','),

    _initializer_token: $ => choice(
      $.block,
      $.parenthesized,
      $.bracketed,
      $.macro_invocation,
      $.string_literal,
      $.char_literal,
      $.number_literal,
      $.boolean_literal,
      $.primitive_type,
      $.self_type,
      $.lifetime,
      $.identifier,
      ...BODY_KEYWORDS,
      ...OPERATORS,
      '.', ':', '::',
    ),

    // `@println(...)`: the arguments are the parenthesized group that follows.
    macro_invocation: $ => seq('@', field('name', $.identifier)),

    // ---- Lexical -----------------------------------------------------------

    identifier: _ => /[A-Za-z_][A-Za-z0-9_]*/,

    lifetime: _ => token(seq("'", /[A-Za-z_][A-Za-z0-9_]*/)),

    boolean_literal: _ => choice('true', 'false'),

    number_literal: _ => token(choice(
      /0[xX][0-9a-fA-F_]+/,
      /0[bB][01_]+/,
      /0[oO][0-7_]+/,
      /[0-9][0-9_]*(\.[0-9][0-9_]*)?([eE][+-]?[0-9]+)?i?/,
    )),

    string_literal: $ => seq(
      '"',
      repeat(choice($.escape_sequence, $._string_content)),
      token.immediate('"'),
    ),

    _string_content: _ => token.immediate(prec(1, /[^"\\]+/)),

    // One token, so `'a'` wins over the shorter lifetime `'a`.
    char_literal: _ => token(seq(
      "'",
      choice(/[^'\\\n]/, /\\x[0-9a-fA-F]{2}/, /\\u\{[0-9a-fA-F]+\}/, /\\[^xu]/),
      "'",
    )),

    escape_sequence: _ => token.immediate(seq(
      '\\',
      choice(/x[0-9a-fA-F]{2}/, /u\{[0-9a-fA-F]+\}/, /[^xu]/),
    )),

    // `///` doc lines are line comments; queries tell them apart by prefix.
    line_comment: _ => token(seq('//', /[^\n]*/)),
  },
});
