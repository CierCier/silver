## Silver Language Grammar (based on current `examples/basic.ag`)

This grammar captures the current syntax used in `examples/basic.ag`: C-like declarations and statements, but with struct members separated by commas (with an optional trailing comma) and no semicolon after a struct definition.

Notation: [] optional, {} zero or more, | alternation, () grouping.

```ebnf
program              = { external_declaration } ;

external_declaration = function_definition | declaration | struct_declaration | trait_declaration ;

(* Attributes - Python-style decorators *)
attribute            = "@" identifier [ "(" identifier_list? ")" ] ;
identifier_list      = identifier { "," identifier } ;

(* Trait definitions *)
trait_declaration    = "trait" identifier [ generic_params ] "{" { trait_method } "}" ;
trait_method         = type identifier "(" [ param_list ] ")" ";" ;

(* Structs: comma-separated members, optional trailing comma, no semicolon after the closing brace *)
(* Structs can have attributes like @trait(copy, drop) *)

struct_declaration   = { attribute } "struct" identifier [ generic_params ] "{" struct_member_list? "}" ;
generic_params       = "<" identifier { "," identifier } ">" ;
struct_member_list   = struct_member { "," struct_member } [ "," ] ;
struct_member        = type declarator ;              (* e.g., i32 x  |  i32 y  *)

(* Declarations (variables and forward decls) *)
declaration          = type init_declarator_list? ";" ;
init_declarator_list = init_declarator { "," init_declarator } ;
init_declarator      = declarator [ "=" initializer ] ;

(* Functions *)
function_definition  = type declarator compound_statement ;

parameter_list       = parameter_declaration { "," parameter_declaration } ;
parameter_declaration= type declarator ;             (* e.g., i32 argc, str *argv *)

(* Declarators — C-like pointer binding; arrays supported as suffix or leading prefix form *)
declarator           = array_prefix? pointer? direct_declarator ;
pointer              = "*" { "*" } ;
direct_declarator    = ( identifier | "(" declarator ")" )
					   { "[" constant_expression? "]"
					   | "(" parameter_list? ")" } ;
array_prefix         = "[" constant_expression? "]" { "[" constant_expression? "]" } ;

initializer          = assignment_expression ;

(* Statements *)
compound_statement   = "{" { declaration | statement } "}" ;

statement            = expression_statement
										 | compound_statement
										 | selection_statement
										 | iteration_statement
										 | jump_statement ;

expression_statement = [ expression ] ";" ;

selection_statement  = "if" "(" expression ")" statement [ "else" statement ] ;

iteration_statement  = "while" "(" expression ")" statement
										 | "for" "(" for_init for_cond for_iter ")" statement ;

for_init             = ( declaration | [ expression ] ";" ) ;
for_cond             = [ expression ] ";" ;
for_iter             = [ expression ] ;

jump_statement       = "return" [ expression ] ";"
										 | "break" ";"
										 | "continue" ";" ;

(* Expressions — C-like precedence (simplified) *)
expression           = assignment_expression { "," assignment_expression } ;

assignment_expression= conditional_expression
										 | unary_expression assignment_op assignment_expression ;
assignment_op        = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "^=" | "|=" ;

conditional_expression = logical_or_expression [ "?" expression ":" conditional_expression ] ;
constant_expression  = conditional_expression ;
logical_or_expression  = logical_and_expression { "||" logical_and_expression } ;
logical_and_expression = equality_expression { "&&" equality_expression } ;
equality_expression    = relational_expression { ( "==" | "!=" ) relational_expression } ;
relational_expression  = additive_expression { ( "<" | ">" | "<=" | ">=" ) additive_expression } ;
additive_expression    = multiplicative_expression { ( "+" | "-" ) multiplicative_expression } ;
multiplicative_expression = unary_expression { ( "*" | "/" | "%" ) unary_expression } ;

unary_expression     = postfix_expression
										 | ( "++" | "--" ) unary_expression
										 | unary_op unary_expression ;
unary_op             = "&" | "*" | "+" | "-" | "~" | "!" ;

postfix_expression   = primary_expression { "[" expression "]"
																				 | "(" argument_list? ")"
																				 | "." identifier
																				 | "++" | "--" } ;

argument_list        = assignment_expression { "," assignment_expression } ;

primary_expression   = identifier
										 | integer_literal
										 | string_literal
										 | "(" expression ")" ;

(* Types present in example + a few common builtins *)
type                 = "void" | "bool"
										 | "i8" | "i16" | "i32" | "i64"
										 | "u8" | "u16" | "u32" | "u64"
										 | "f32" | "f64"
										 | "char" | "str"         (* str commonly used as char* equivalent *)
										 | identifier ;            (* user-defined types like struct names *)

(* Lexical elements *)
identifier           = letter { letter | digit | "_" } ;
integer_literal      = digit { digit } ;
string_literal       = '"' { string_char } '"' ;
string_char          = any_non_quote_backslash | "\\" escape_seq ;
escape_seq           = '"' | "\\" | "n" | "r" | "t" | "0" | "x" hex_digit { hex_digit } ;

letter               = "A".."Z" | "a".."z" | "_" ;
digit                = "0".."9" ;
hex_digit            = digit | "a".."f" | "A".."F" ;

comment              = "//" non_newline* newline | "/*" any* "*/" ;
```

### Example compatibility
- Struct:

struct Point {
    i32 x,
    i32 y,
}

- Function:
```
i32 main(i32 argc, str *argv) {
	for (i32 i = 0; i < argc; i++) {
        println("%s%s", argv[i], (i < argc - 1) ? "" : "\n");
	}
	println("Hello world");
	return 0;
}
```

- Arrays:
```
	i32 arr[8];         // C-like suffix form
	i32 [8]arr;         // prefix form supported by grammar
	i32 *p[4];          // array of 4 pointers to i32
	i32 (*pa)[4];       // pointer to array of 4 i32 (future if you add parentheses binding)
```
- Pointers:
```
	i32 *p;             // pointer to i32
	i32 **grid;         // pointer to pointer to i32
	i32 *matrix[16];    // array of 16 pointers to i32
	i32 (*row)[16];     // pointer to array of 16 i32
```

- Traits (memory management markers):
```silver
// Define a trait with method signatures
trait Drop {
    void drop();
}

trait Clone {
    Self clone();
}

// Generic trait
trait Into<T> {
    T into();
}

// Mark a struct as copyable (bitwise copy, no destructor)
@trait(copy)
struct Point {
    i32 x;
    i32 y;
}

// Mark a struct as needing cleanup (drop) and cloneable
@trait(drop, clone)
struct Box<T> {
    T* ptr;
}

// Multiple trait attributes
@trait(copy, debug)
@trait(default)
struct Config {
    i32 width;
    i32 height;
}
```

Built-in traits:
- `copy` - Type is bitwise copyable, no destructor called
- `clone` - Type supports explicit deep copy via `.clone()`
- `drop` - Type has a destructor that will be called when it goes out of scope
- `default` - Type supports default construction
- `debug` - Type supports debug printing
- `drop` - Type has a destructor that will be called when it goes out of scope
- `default` - Type supports default construction
- `debug` - Type supports debug printing
