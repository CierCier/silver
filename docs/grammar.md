# Silver Language Grammar

This grammar is based on the current parser implementation.

```ebnf
program              = { external_declaration } ;

external_declaration = import_declaration
                     | link_declaration
                     | attribute_declaration
                     | struct_declaration
                     | enum_declaration
                     | trait_declaration
                     | impl_declaration
                     | function_declaration
                     | variable_declaration
                     ;

import_declaration   = "import" identifier { "." identifier } ";" ;
link_declaration     = "link" string_literal ";" ;

attribute_declaration= attribute { attribute } ;
attribute            = "@" identifier [ "(" identifier_list ")" ] ;
identifier_list      = identifier { "," identifier } ;

struct_declaration   = { attribute } "struct" identifier [ generic_params ] "{" { struct_member } "}" ;
generic_params       = "<" identifier { "," identifier } ">" ;
struct_member        = type_name declarator { "," declarator } ( ";" | "," ) ;

enum_declaration     = "enum" identifier "{" { enum_item } "}" ;
enum_item            = identifier [ "=" integer_literal ] ";" ;

trait_declaration    = "trait" identifier [ generic_params ] "{" { trait_method } "}" ;
trait_method         = type_name identifier [ generic_params ] "(" [ parameter_list ] ")" ";" ;

impl_declaration     = "impl" type_name "{" { impl_member } "}" ;
impl_member          = function_declaration | cast_declaration ;

cast_declaration     = [ "implicit" ] "cast" type_name "(" parameter_list ")" compound_statement ;

function_declaration = [ "extern" | "static" ] type_name declarator [ generic_params ] "(" [ parameter_list [ "," "..." ] ] ")" ( compound_statement | ";" ) ;
variable_declaration = [ "extern" | "static" ] type_name init_declarator_list ";" ;

init_declarator_list = init_declarator { "," init_declarator } ;
init_declarator      = declarator [ "=" initializer ] ;
declarator           = { "*" } identifier { "[" [ constant_expression ] "]" } ;
parameter_list       = parameter_declaration { "," parameter_declaration } ;
parameter_declaration= type_name [ declarator ] ;

type_name            = identifier [ generic_arguments ] { "*" } ;
generic_arguments    = "<" type_name { "," type_name } ">" ;

initializer          = expression | initializer_list ;
initializer_list     = "{" [ init_item { "," init_item } [","] ] "}" ;
init_item            = [ "[" constant_expression "]" "=" ] initializer ;

statement            = expression_statement
                     | compound_statement
                     | selection_statement
                     | iteration_statement
                     | jump_statement
                     | declaration_statement
                     | asm_statement
                     ;

declaration_statement= [ "const" ] type_name init_declarator_list ";" ;

expression_statement = [ expression ] ";" ;
compound_statement   = "{" { statement } "}" ;

selection_statement  = "if" "(" expression ")" statement [ "else" statement ]
                     | "switch" "(" expression ")" "{" { case_statement } "}"
                     ;
case_statement       = "case" expression { "," expression } ":" { statement }
                     | "default" ":" { statement }
                     ;

iteration_statement  = "while" "(" expression ")" statement
                     | "for" "(" ( declaration_statement | expression_statement ) [ expression ] ";" [ expression ] ")" statement
                     ;

jump_statement       = "return" [ expression ] ";"
                     | "break" ";"
                     | "continue" ";"
                     ;

asm_statement        = "asm" "(" string_literal ")" ";" ;

expression           = assignment_expression ;
assignment_expression= conditional_expression [ assignment_op assignment_expression ] ;
assignment_op        = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "^=" | "|=" ;

conditional_expression = logical_or_expression [ "?" expression ":" conditional_expression ] ;
logical_or_expression  = logical_and_expression { "||" logical_and_expression } ;
logical_and_expression = bitwise_or_expression { "&&" bitwise_or_expression } ;
bitwise_or_expression  = bitwise_xor_expression { "|" bitwise_xor_expression } ;
bitwise_xor_expression = bitwise_and_expression { "^" bitwise_and_expression } ;
bitwise_and_expression = equality_expression { "&" equality_expression } ;
equality_expression    = relational_expression { ( "==" | "!=" ) relational_expression } ;
relational_expression  = shift_expression { ( "<" | ">" | "<=" | ">=" ) shift_expression } ;
shift_expression     = additive_expression { ( "<<" | ">>" ) additive_expression } ;
additive_expression    = multiplicative_expression { ( "+" | "-" ) multiplicative_expression } ;
multiplicative_expression = unary_expression { ( "*" | "/" | "%" ) unary_expression } ;

unary_expression     = postfix_expression
                     | ( "++" | "--" | "&" | "*" | "+" | "-" | "~" | "!" ) unary_expression
                     | "comptime" unary_expression
                     | "(" type_name ")" unary_expression (* C-style cast *)
                     ;

postfix_expression   = primary_expression { postfix_op } ;
postfix_op           = "[" expression "]"
                     | "(" [ argument_list ] ")"
                     | "." identifier
                     | "->" identifier
                     | "++"
                     | "--"
                     | generic_arguments "(" [ argument_list ] ")" (* Generic call *)
                     ;

primary_expression   = identifier
                     | integer_literal
                     | float_literal
                     | string_literal
                     | char_literal
                     | "true" | "false"
                     | "(" expression ")"
                     | "new" "<" type_name ">" "(" ")"
                     | "alloc" "<" type_name ">" "(" [ expression ] ")"
                     | "free" "(" expression ")"
                     | "drop" "(" expression ")"
                     | initializer_list
                     ;

argument_list        = expression { "," expression } ;

constant_expression  = conditional_expression ;

(* Lexical elements *)
identifier           = letter { letter | digit | "_" } ;
integer_literal      = digit { digit } ;
float_literal        = digit "." digit ;
string_literal       = '"' { string_char } '"' ;
char_literal         = "'" ( escape_sequence | utf8_char ) "'" ;
escape_sequence      = "\\" ( "n" | "r" | "t" | "0" | "\\" | "'" | "x" hex hex | "u" hex hex hex hex | "U" hex hex hex hex hex hex hex hex ) ;
utf8_char            = any_utf8_character ;
```