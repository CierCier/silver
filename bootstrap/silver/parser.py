from silver.preprocess import Preprocessor
from silver.tokenizer import Token, TokenType, Tokenizer
from silver.silver_types import (
    BoolType,
    FloatType,
    IntType,
    SilverEnum,
    SilverPointer,
    SilverStruct,
    SilverType,
    SilverUnion,
    StringType,
    UIntType,
    VoidType,
)

from typing import List, Optional, Dict
from pathlib import Path


class SymbolTable:
    def __init__(self):
        self.symbols = {}

    def add_symbol(self, name: str, symbol_type: str, value: any = None):
        self.symbols[name] = {"type": symbol_type, "value": value}

    def get_symbol(self, name: str) -> dict:
        return self.symbols.get(name, None)

    def generate_debug_markers(self) -> str:
        markers = []
        for name, info in self.symbols.items():
            markers.append(
                f"Symbol: {name}, Type: {info['type']}, Value: {info['value']}"
            )
        return "\n".join(markers)


class GlobalTable:
    def __init__(self):
        self.globals = {}
        self.string_literals = []

    def add_global(self, name: str, value: any):
        self.globals[name] = value

    def get_global(self, name: str) -> any:
        return self.globals.get(name, None)

    def add_string_literal(self, value: str):
        self.string_literals.append(value)

    def get_string_literals(self) -> List[str]:
        return self.string_literals

    def generate_debug_markers(self) -> str:
        markers = []
        for name, value in self.globals.items():
            markers.append(f"Global: {name}, Value: {value}")
        for i, literal in enumerate(self.string_literals):
            markers.append(f"String Literal {i}: {literal}")
        return "\n".join(markers)


class GlobalTypesTable:
    def __init__(self):

        ## prepopulate with primitive types
        self.types = {
            "i8": SilverType("i8", 1),
            "i16": SilverType("i16", 2),
            "i32": SilverType("i32", 4),
            "i64": SilverType("i64", 8),
            "u8": SilverType("u8", 1),
            "u16": SilverType("u16", 2),
            "u32": SilverType("u32", 4),
            "u64": SilverType("u64", 8),
            "f32": SilverType("f32", 4),
            "f64": SilverType("f64", 8),
            "bool": SilverType("bool", 1),
            "char": SilverType("char", 1),
            "string": SilverType("string", 0),
            "void": SilverType("void", 0),
            "*void": SilverPointer("*void", VoidType()),
            "*i8": SilverPointer("*i8", IntType(8)),
            "*i16": SilverPointer("*i16", IntType(16)),
            "*i32": SilverPointer("*i32", IntType(32)),
            "*i64": SilverPointer("*i64", IntType(64)),
            "*u8": SilverPointer("*u8", UIntType(8)),
            "*u16": SilverPointer("*u16", UIntType(16)),
            "*u32": SilverPointer("*u32", UIntType(32)),
            "*u64": SilverPointer("*u64", UIntType(64)),
            "*f32": SilverPointer("*f32", FloatType(32)),
            "*f64": SilverPointer("*f64", FloatType(64)),
            "*bool": SilverPointer("*bool", BoolType()),
            "*string": SilverPointer("*string", StringType()),
        }

    def add_type(self, name: str, type_def: SilverType):
        if name in self.types:
            raise (f"Type '{name}' already exists")
        self.types[name] = type_def

    def get_type(self, name: str) -> any:
        return self.types.get(name, None)

    def generate_debug_markers(self) -> str:
        markers = []
        for name, type_def in self.types.items():
            markers.append(f"Type: {name}, Definition: {type_def}")
        return "\n".join(markers)


class Program:
    def __init__(
        self,
        functions: List["Function"] = [],
        globals: List["VariableDecl"] = [],
        structs: List["SilverStruct"] = [],
        enums: List["SilverEnum"] = [],
        unions: List["SilverUnion"] = [],
    ):
        self.functions = functions
        self.globals = globals
        self.structs = structs
        self.enums = enums
        self.unions = unions

    def add_function(self, function: "Function"):
        self.functions.append(function)

    def get_function(self, name: str) -> "Function":
        for function in self.functions:
            if function.name == name:
                return function
        return None

    def add_global(self, global_var: "VariableDecl"):
        self.globals.append(global_var)

    def get_globals(self) -> List["VariableDecl"]:
        return self.globals

    def add_struct(self, struct: "SilverStruct"):
        self.structs.append(struct)

    def get_struct(self, name: str) -> "SilverStruct":
        for struct in self.structs:
            if struct.name == name:
                return struct
        return None

    def add_enum(self, enum: "SilverEnum"):
        self.enums.append(enum)

    def get_enum(self, name: str) -> "SilverEnum":
        for enum in self.enums:
            if enum.name == name:
                return enum
        return None

    def add_union(self, union: "SilverUnion"):
        self.unions.append(union)

    def get_union(self, name: str) -> "SilverUnion":
        for union in self.unions:
            if union.name == name:
                return union
        return None


class Function:

    def __init__(
        self,
        name: str,
        parameters: List["Parameter"],
        return_type: "Type",
        body: Optional["Block"] = None,
    ):
        self.name = name
        self.parameters = parameters
        self.return_type = return_type
        self.body = body
        self.is_variadic = any(param.name == "..." for param in parameters)


class Parameter:
    def __init__(self, param_type: "Type", name: str):
        self.param_type = param_type
        self.name = name


class Type:
    def __init__(self, name: str):
        self.name = name

    def __str__(self):
        return f"Type({self.name})"


class Block:
    def __init__(self, statements: List["Statement"]):
        self.statements = statements
        self.symbol_table = SymbolTable()

    def add_symbol(self, name: str, symbol_type: str, value: any = None):
        self.symbol_table.add_symbol(name, symbol_type, value)

    def get_symbol(self, name: str) -> dict:
        return self.symbol_table.get_symbol(name)

    def __str__(self):
        result = "{\n"
        for stmt in self.statements:
            result += f"  {stmt}\n"
        result += "}"
        return result


class Null:
    def __str__(self):
        return "null"


class Statement:
    pass


class VariableDecl(Statement):
    def __init__(self, var_type: "Type", name: str, value: "Expr"):
        self.var_type = var_type
        self.name = name
        self.value = value

    def __str__(self):
        return f"{self.var_type} {self.name} = {self.value}"


class IfStmt(Statement):
    def __init__(self, condition: "Expr", body: "Block"):
        self.condition = condition
        self.body = body

    def __str__(self):
        return f"if ({self.condition}) {self.body}"


class ReturnStmt(Statement):
    def __init__(self, value: Optional["Expr"]):
        self.value = value

    def __str__(self):
        return f"return {self.value}"


class ExprStmt(Statement):
    def __init__(self, expr: "Expr"):
        self.expr = expr

    def __str__(self):
        return f"{self.expr}"


class Expr:
    pass


class BinOp(Expr):
    def __init__(self, left: "Expr", op: str, right: "Expr"):
        self.left = left
        self.op = op
        self.right = right

    def __str__(self):
        return f"({self.left} {self.op} {self.right})"


class Identifier(Expr):
    def __init__(self, name: str):
        self.name = name

    def __str__(self):
        return self.name


class Number(Expr):
    def __init__(self, value: str):
        self.value = value
        # Keep the original string representation for floats
        self.is_float = "e" in value.lower() or "." in value

    def __str__(self):
        return f"Number({self.value})"


class String(Expr):
    def __init__(self, value: str):
        self.value = value

    def __str__(self):
        _repr = self.value.replace("\n", "\\n")
        _repr = _repr.replace("\t", "\\t")
        _repr = _repr.replace("\r", "\\r")
        _repr = _repr.replace("\f", "\\f")
        _repr = _repr.replace("\b", "\\b")
        _repr = _repr.replace("\a", "\\a")
        _repr = _repr.replace("\v", "\\v")
        _repr = _repr.replace("\0", "\\0")
        return f'"{_repr}"'


class ParenExpr(Expr):
    def __init__(self, expr: "Expr"):
        self.expr = expr

    def __str__(self):
        return f"({self.expr})"


class FunctionCall(Expr):
    def __init__(self, name: str, args: List["Expr"]):
        self.name = name
        self.args = args

    def __str__(self):
        return f"{self.name}({', '.join(str(arg) for arg in self.args)})"


class MemberAccess(Expr):
    def __init__(self, struct: Expr, field: str):
        self.struct = struct
        self.field = field

    def __str__(self):
        return f"{self.struct}.{self.field}"


class StructInit(Expr):
    def __init__(self, fields: Dict[str, Expr]):
        self.fields = fields

    def __str__(self):
        fields_str = ", ".join(
            f"{name}: {value}" for name, value in self.fields.items()
        )
        return f"{{{fields_str}}}"


class AssignStmt(Statement):
    def __init__(self, target: Expr, value: Expr):
        self.target = target
        self.value = value

    def __str__(self):
        return f"{self.target} = {self.value}"


class DerefExpr(Expr):
    def __init__(self, expr: Expr):
        self.expr = expr

    def __str__(self):
        return f"*{self.expr}"


class AddrOfExpr(Expr):
    def __init__(self, expr: Expr):
        self.expr = expr

    def __str__(self):
        return f"&{self.expr}"


class WhileStmt(Statement):
    def __init__(self, condition: Expr, body: Block):
        self.condition = condition
        self.body = body


class ForStmt(Statement):
    def __init__(
        self,
        init: Optional[Statement],
        condition: ExprStmt,
        increment: Optional[ExprStmt],
        body: Block,
    ):
        self.init = init
        self.condition = condition
        self.increment = increment
        self.body = body

    def __str__(self):
        return f"for ( {self.init};{self.condition}; {self.increment} ) {self.body}"


class ExternalDecl(Statement):
    def __init__(self, var_type: "Type", name: str):
        self.var_type = var_type
        self.name = name

    def __str__(self):
        return f"extern {self.var_type} {self.name}"


class StaticDecl(Statement):
    def __init__(self, var_type: "Type", name: str, value: "Expr"):
        self.var_type = var_type
        self.name = name
        self.value = value

    def __str__(self):
        return f"static {self.var_type} {self.name} = {self.value}"


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.position = 0
        self.symbol_table = SymbolTable()
        self.global_table = GlobalTable()
        self.types_table = GlobalTypesTable()
        self.current_block = None

    def parse(self) -> Program:
        program = Program()
        while (
            self.position < len(self.tokens)
            and self.current_token().type != TokenType.EOF
        ):
            if self.match(TokenType.KEYWORD, "func"):
                function = self.parse_function()
                program.add_function(function)
            elif self.match(TokenType.KEYWORD, "let"):
                global_var = self.parse_variable_decl()
                program.add_global(global_var)
            elif self.match(TokenType.KEYWORD, "extern"):
                extern_var = self.parse_external_decl()
                program.add_global(extern_var)
            elif self.match(TokenType.KEYWORD, "struct"):
                struct = self.parse_struct()
                program.add_struct(struct)
            elif self.match(TokenType.KEYWORD, "enum"):
                enum = self.parse_enum()
                program.add_enum(enum)
            elif self.match(TokenType.KEYWORD, "union"):
                union = self.parse_union()
                program.add_union(union)
            else:
                self.error(
                    "Expected 'func', 'let', 'extern', 'struct', 'enum', or 'union' keyword in global scope"
                )
        return program

    def parse_function(self) -> Function:
        self.consume(TokenType.KEYWORD, "func")
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.LPAREN)
        parameters = self.parse_parameters()
        self.consume(TokenType.RPAREN)
        return_type = self.parse_type()

        if self.match(TokenType.SEMICOLON):
            self.consume(TokenType.SEMICOLON)
            return Function(name, parameters, return_type)
        else:
            body = self.parse_block()
            return Function(name, parameters, return_type, body)

    def parse_parameters(self) -> List[Parameter]:
        parameters = []
        if not self.match(TokenType.RPAREN):
            while True:
                if self.match(TokenType.VARIANT):
                    self.consume(TokenType.VARIANT)
                    parameters.append(Parameter(Type("variadic"), "..."))
                    break
                param_type = self.parse_type()
                name = self.consume(TokenType.IDENTIFIER).value
                parameters.append(Parameter(param_type, name))
                if not self.match(TokenType.COMMA):
                    break
                self.consume(TokenType.COMMA)
        return parameters

    def parse_type(self) -> Type:
        # Handle pointer types first
        if self.match(TokenType.MULTIPLY):
            self.consume(TokenType.MULTIPLY)
            base_type = self.parse_type()
            return Type(f"*{base_type.name}")

        # Handle regular types
        if self.match(TokenType.IDENTIFIER):
            return Type(self.consume(TokenType.IDENTIFIER).value)

        self.error("Expected type identifier")

    def parse_block(self) -> Block:
        self.consume(TokenType.LBRACE)
        statements = []
        block = Block(statements)
        previous_block = self.current_block
        self.current_block = block

        while not self.match(TokenType.RBRACE):
            statements.append(self.parse_statement())

        self.consume(TokenType.RBRACE)
        self.current_block = previous_block
        return block

    def parse_statement(self) -> Statement:
        if self.match(TokenType.KEYWORD, "let"):
            return self.parse_variable_decl()
        elif self.match(TokenType.KEYWORD, "static"):
            return self.parse_static_decl()
        elif self.match(TokenType.KEYWORD, "if"):
            return self.parse_if_stmt()
        elif self.match(TokenType.KEYWORD, "return"):
            return self.parse_return_stmt()
        elif self.match(TokenType.KEYWORD, "while"):
            return self.parse_while_stmt()
        elif self.match(TokenType.KEYWORD, "for"):
            return self.parse_for_stmt()
        else:
            # Try to parse an assignment or expression statement
            expr = self.parse_expr()
            if self.match(TokenType.ASSIGN):
                self.consume(TokenType.ASSIGN)
                value = self.parse_expr()
                self.consume(TokenType.SEMICOLON)
                return AssignStmt(expr, value)
            elif self.match(TokenType.INCREMENT) or self.match(TokenType.DECREMENT):
                op = self.consume(self.current_token().type).value
                self.consume(TokenType.SEMICOLON)
                return BinOp(expr, op, Number("1"))
            elif self.match(TokenType.INCREMENT_ASSIGN):
                self.consume(TokenType.INCREMENT_ASSIGN)
                value = self.parse_expr()
                self.consume(TokenType.SEMICOLON)
                return BinOp(expr, "+=", value)
            elif self.match(TokenType.DECREMENT_ASSIGN):
                self.consume(TokenType.DECREMENT_ASSIGN)
                value = self.parse_expr()
                self.consume(TokenType.SEMICOLON)
                return BinOp(expr, "-=", value)
            else:
                self.consume(TokenType.SEMICOLON)
                return ExprStmt(expr)

    def parse_variable_decl(self) -> VariableDecl:
        # Check for static or external modifiers
        is_static = False
        is_external = False

        if self.match(TokenType.KEYWORD, "static"):
            self.consume(TokenType.KEYWORD, "static")
            is_static = True
        elif self.match(TokenType.KEYWORD, "extern"):
            self.consume(TokenType.KEYWORD, "extern")
            is_external = True

        self.consume(TokenType.KEYWORD, "let")
        var_type = self.parse_type()
        name = self.consume(TokenType.IDENTIFIER).value

        # For external declarations, the value is optional
        if is_external:
            if self.match(TokenType.ASSIGN):
                self.consume(TokenType.ASSIGN)
                value = self.parse_expr()
            else:
                value = None
        else:
            self.consume(TokenType.ASSIGN)
            value = self.parse_expr()

        self.consume(TokenType.SEMICOLON)

        # Add the variable to the current block's symbol table
        if isinstance(self.current_block, Block):
            self.current_block.add_symbol(name, var_type.name, value)

        decl = VariableDecl(var_type, name, value)
        decl.is_static = is_static
        decl.is_external = is_external
        return decl

    def parse_if_stmt(self) -> IfStmt:
        self.consume(TokenType.KEYWORD, "if")
        self.consume(TokenType.LPAREN)
        condition = self.parse_expr()
        self.consume(TokenType.RPAREN)
        body = self.parse_block()
        return IfStmt(condition, body)

    def parse_return_stmt(self) -> ReturnStmt:
        self.consume(TokenType.KEYWORD, "return")
        value = None
        if not self.match(TokenType.SEMICOLON):
            value = self.parse_expr()
        self.consume(TokenType.SEMICOLON)
        return ReturnStmt(value)

    def parse_while_stmt(self) -> WhileStmt:
        self.consume(TokenType.KEYWORD, "while")
        self.consume(TokenType.LPAREN)
        condition = self.parse_expr()
        self.consume(TokenType.RPAREN)
        body = self.parse_block()
        return WhileStmt(condition, body)

    def parse_for_stmt(self) -> ForStmt:
        self.consume(TokenType.KEYWORD, "for")
        self.consume(TokenType.LPAREN)

        # Parse initialization
        init = None
        if not self.match(TokenType.SEMICOLON):
            if self.match(TokenType.KEYWORD, "let"):
                init = self.parse_variable_decl()
            else:
                init = self.parse_statement()
        ## dont need to consume semicolon here, variable_decl already consumes it

        # Parse condition
        condition = None
        if not self.match(TokenType.SEMICOLON):
            condition = self.parse_expr()
            self.consume(TokenType.SEMICOLON)

        # Parse increment
        increment = None
        if not self.match(TokenType.RPAREN):
            increment = self.parse_expr()
        self.consume(TokenType.RPAREN)

        body = self.parse_block()

        return ForStmt(init, condition, increment, body)

    def parse_expr(self) -> Expr:
        return self.parse_comparison()

    def parse_comparison(self) -> Expr:
        left = self.parse_additive()
        while (
            self.match(TokenType.EQUAL)
            or self.match(TokenType.NOT_EQUAL)
            or self.match(TokenType.LESS)
            or self.match(TokenType.LESS_EQUAL)
            or self.match(TokenType.GREATER)
            or self.match(TokenType.GREATER_EQUAL)
        ):
            op = self.consume(self.current_token().type).value
            right = self.parse_additive()
            left = BinOp(left, op, right)
        return left

    def parse_additive(self) -> Expr:
        left = self.parse_multiplicative()
        while self.match(TokenType.PLUS) or self.match(TokenType.MINUS):
            op = self.consume(self.current_token().type).value
            right = self.parse_multiplicative()
            left = BinOp(left, op, right)
        return left

    def parse_multiplicative(self) -> Expr:
        left = self.parse_primary()
        while self.match(TokenType.MULTIPLY) or self.match(TokenType.DIVIDE):
            op = self.consume(self.current_token().type).value
            right = self.parse_primary()
            left = BinOp(left, op, right)
        return left

    def parse_primary(self) -> Expr:
        if self.match(TokenType.LBRACE):
            # Handle struct initialization
            self.consume(TokenType.LBRACE)
            fields = {}

            if not self.match(TokenType.RBRACE):
                while True:
                    field_name = self.consume(TokenType.IDENTIFIER).value
                    self.consume(TokenType.COLON)
                    field_value = self.parse_expr()
                    fields[field_name] = field_value

                    if not self.match(TokenType.COMMA):
                        break
                    self.consume(TokenType.COMMA)

            self.consume(TokenType.RBRACE)
            return StructInit(fields)

        # Handle increment/decrement operators
        if self.match(TokenType.INCREMENT) or self.match(TokenType.DECREMENT):
            op = self.consume(self.current_token().type).value
            expr = self.parse_primary()
            return BinOp(expr, op, Number("1"))

        # Handle pointer operations
        if self.match(TokenType.MULTIPLY):
            self.consume(TokenType.MULTIPLY)
            expr = self.parse_primary()
            return DerefExpr(expr)
        elif self.match(TokenType.AND):
            self.consume(TokenType.AND)
            expr = self.parse_primary()
            return AddrOfExpr(expr)

        expr = self.parse_identifier_or_call()

        # Handle postfix increment/decrement
        if self.match(TokenType.INCREMENT) or self.match(TokenType.DECREMENT):
            op = self.consume(self.current_token().type).value
            return BinOp(expr, op, Number("1"))

        # Handle member access
        while self.match(TokenType.DOT):
            self.consume(TokenType.DOT)
            field = self.consume(TokenType.IDENTIFIER).value
            expr = MemberAccess(expr, field)

        return expr

    def parse_identifier_or_call(self) -> Expr:
        if self.match(TokenType.IDENTIFIER):
            name = self.consume(TokenType.IDENTIFIER).value
            if self.match(TokenType.LPAREN):
                self.consume(TokenType.LPAREN)
                args = []
                if not self.match(TokenType.RPAREN):
                    while True:
                        args.append(self.parse_expr())
                        if not self.match(TokenType.COMMA):
                            break
                        self.consume(TokenType.COMMA)
                self.consume(TokenType.RPAREN)
                return FunctionCall(name, args)
            return Identifier(name)
        elif self.match(TokenType.KEYWORD, "true"):
            self.consume(TokenType.KEYWORD, "true")
            return Number("1")
        elif self.match(TokenType.KEYWORD, "false"):
            self.consume(TokenType.KEYWORD, "false")
            return Number("0")
        elif self.match(TokenType.KEYWORD, "null"):
            self.consume(TokenType.KEYWORD, "null")
            return Null()
        elif self.match(TokenType.MINUS):
            self.consume(TokenType.MINUS)
            val = self.consume(TokenType.NUMBER).value
            val = int(val) * -1
            return Number(str(val))
        elif self.match(TokenType.NUMBER):
            return Number(self.consume(TokenType.NUMBER).value)
        elif self.match(TokenType.STRING):
            return String(self.consume(TokenType.STRING).value)
        elif self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            expr = self.parse_expr()
            self.consume(TokenType.RPAREN)
            return ParenExpr(expr)
        self.error("Expected primary expression")

    def parse_struct(self) -> "SilverStruct":
        self.consume(TokenType.KEYWORD, "struct")
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.LBRACE)
        struct = SilverStruct(name)

        self.types_table.add_type(name, struct)
        self.types_table.add_type(f"*{name}", SilverPointer(name, struct))

        while not self.match(TokenType.RBRACE):
            f_type = self.parse_type()
            f_type = self.types_table.get_type(f_type.name)
            f_name = self.consume(TokenType.IDENTIFIER).value
            self.consume(TokenType.SEMICOLON)
            struct.add_field(f_name, f_type)

        self.consume(TokenType.RBRACE)
        return struct

    def parse_enum(self) -> "SilverEnum":
        self.consume(TokenType.KEYWORD, "enum")
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.LBRACE)
        variants = []

        while not self.match(TokenType.RBRACE):
            variant_name = self.consume(TokenType.IDENTIFIER).value
            variants.append(variant_name)
            if not self.match(TokenType.COMMA):
                break
            self.consume(TokenType.COMMA)
        self.consume(TokenType.RBRACE)
        return SilverEnum(name, variants)

    def parse_union(self) -> "SilverUnion":
        self.consume(TokenType.KEYWORD, "union")
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.LBRACE)
        fields = []
        while not self.match(TokenType.RBRACE):
            field_type = self.parse_type()
            field_name = self.consume(TokenType.IDENTIFIER).value
            self.consume(TokenType.SEMICOLON)
            fields.append((field_type, field_name))
        self.consume(TokenType.RBRACE)
        return SilverUnion(name, fields)

    def parse_external_decl(self) -> ExternalDecl:
        self.consume(TokenType.KEYWORD, "extern")
        var_type = self.parse_type()
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.SEMICOLON)
        return ExternalDecl(var_type, name)

    def parse_static_decl(self) -> StaticDecl:
        if not isinstance(self.current_block, Block):
            self.error("Static declarations are only allowed in function scope")

        self.consume(TokenType.KEYWORD, "static")
        var_type = self.parse_type()
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.ASSIGN)
        value = self.parse_expr()
        self.consume(TokenType.SEMICOLON)

        if isinstance(self.current_block, Block):
            self.current_block.add_symbol(name, var_type.name, value)

        return StaticDecl(var_type, name, value)

    def match(self, token_type: TokenType, value: str = None) -> bool:
        if self.position >= len(self.tokens):
            return False
        token = self.tokens[self.position]
        if token.type != token_type:
            return False
        if value is not None and token.value != value:
            return False
        return True

    def consume(self, token_type: TokenType, value: str = None) -> Token:
        if self.match(token_type, value):
            token = self.tokens[self.position]
            self.position += 1
            return token
        self.error(f"Expected {token_type}")

    def current_token(self) -> Token:
        if self.position >= len(self.tokens):
            return Token(TokenType.EOF, "", None)
        return self.tokens[self.position]

    def error(self, message: str):
        token = self.current_token()
        if token.position:
            error_msg = f"Parser error at {token.position}: {message}\n"
            error_msg += f"Token: {token.type}, Value: {token.value}\n"
            if token.position.file:
                try:
                    with open(token.position.file, "r") as f:
                        lines = f.readlines()
                        if 0 <= token.position.line - 1 < len(lines):
                            line_counter = f"Line {token.position.line}: "
                            error_msg += f"{line_counter}"
                            error_msg += f"{lines[token.position.line - 1].strip()}\n"
                            error_msg += (
                                " " * (len(line_counter) + token.position.column - 1)
                                + "^"
                            )
                except Exception as e:
                    error_msg += f"Could not read file: {e}"
            raise Exception(error_msg)
        else:
            raise Exception(f"Parser error: {message}")


def print_Expr(expr: Expr) -> str:
    if isinstance(expr, BinOp):
        return f"({print_Expr(expr.left)} {expr.op} {print_Expr(expr.right)})"
    elif isinstance(expr, Identifier):
        return expr.name
    elif isinstance(expr, Number):
        return expr.value
    elif isinstance(expr, String):
        return expr.value
    elif isinstance(expr, ParenExpr):
        return f"({print_Expr(expr.expr)})"
    elif isinstance(expr, FunctionCall):
        args = ", ".join(print_Expr(arg) for arg in expr.args)
        return f"{expr.name}({args})"
    elif isinstance(expr, MemberAccess):
        return f"{print_Expr(expr.struct)}.{expr.field}"
    elif isinstance(expr, StructInit):
        fields = ", ".join(
            f"{name}: {print_Expr(value)}" for name, value in expr.fields.items()
        )
        return f"{{{fields}}}"
    return str(expr)


def pretty_print(program: Program):
    print("Program {")

    # Print structs
    for struct in program.structs:
        print(f"struct {struct.name} {{")
        for field_name, field_info in struct.fields.items():
            print(f"  {field_info['type']} {field_name};")
        print("}")

    # Print enums
    for enum in program.enums:
        print(f"enum {enum.name} {{")
        for variant in enum.variants:
            print(f"  {variant},")
        print("}")

    # Print unions
    for union in program.unions:
        print(f"union {union.name} {{")
        for field_type, field_name in union.fields:
            print(f"  {field_type} {field_name};")
        print("}")

    # Print globals
    for global_var in program.globals:
        print(f"  {global_var}")

    # Print functions
    for function in program.functions:
        print(f"{function.name}(")
        for param in function.parameters:
            print(f"  {param.param_type} {param.name}")
        print(f") -> {function.return_type} {{")
        if function.body:
            for stmt in function.body.statements:
                print(f"  {stmt}")
        print("}")

    print("}")


def test():
    import sys

    files = [Path(x) for x in sys.argv[1:]]
    preprocessor = Preprocessor(*files)

    data = preprocessor.preprocess()

    tokenizer = Tokenizer(data)

    tokens = tokenizer.get_all()

    parser = Parser(tokens)
    program = parser.parse()

    pretty_print(program)


if __name__ == "__main__":
    test()
