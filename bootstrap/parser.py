from enum import Enum
from typing import Iterable, List, Optional, Union
from dataclasses import dataclass

from lang98 import Type
from lexer import Token, TokenType


@dataclass
class ASTNode:
    """Base class for all AST nodes"""

    line: int = -1
    column: int = -1

    def __init__(self, type: str, line: int = -1, column: int = -1):
        self.type = type
        self.line = line
        self.column = column

    def __str__(self) -> str:
        return self.__class__.__name__


@dataclass
class BlockNode(ASTNode):
    def __init__(self, line: int = -1, column: int = -1):
        super().__init__("Block", line, column)
        self.statements: List[ASTNode] = []

    def add_statement(self, statement: ASTNode) -> None:
        if statement is not None:
            self.statements.append(statement)

    def __str__(self) -> str:
        statements = ", ".join(str(s) for s in self.statements)
        return f"BlockNode[ {statements} ]"


@dataclass
class TypeNode(ASTNode):
    def __init__(self, type_name: Type, line: int = -1, column: int = -1):
        super().__init__("Type", line, column)
        self.type_name = type_name

    def __str__(self) -> str:
        return f"TypeNode({self.type_name})"


@dataclass
class FunctionParameterNode(TypeNode):
    def __init__(self, name: str, type_name: Type, line: int = -1, column: int = -1):
        super().__init__(type_name, line, column)
        self.name = name

    def __str__(self) -> str:
        return f"FunctionParameterNode({self.name}: {self.type_name})"

    def __repr__(self) -> str:
        return self.__str__()


@dataclass
class FunctionDeclarationNode(ASTNode):
    def __init__(
        self,
        name: str,
        return_type: TypeNode,
        parameters: List[FunctionParameterNode],
        line: int = -1,
        column: int = -1,
    ):
        super().__init__("FunctionDeclaration", line, column)
        self.name = name
        self.return_type = return_type
        self.parameters = parameters
        self.body: BlockNode = BlockNode()

    def set_body(self, body: BlockNode) -> None:
        self.body = body

    def __str__(self) -> str:
        params = ", ".join(str(p) for p in self.parameters)
        return f"FunctionDeclarationNode({self.name}({params}): {self.return_type} body={self.body})"


@dataclass
class VariableDeclarationNode(ASTNode):
    def __init__(
        self,
        name: str,
        var_type: TypeNode,
        value: Optional[ASTNode] = None,
        line: int = -1,
        column: int = -1,
    ):
        super().__init__("VariableDeclaration", line, column)
        self.name = name
        self.var_type = var_type
        self.value = value

    def __str__(self) -> str:
        return f"VariableDeclarationNode({self.name}: {self.var_type} = {self.value})"


@dataclass
class IdentifierNode(ASTNode):
    def __init__(self, name: str, line: int = -1, column: int = -1):
        super().__init__("Identifier", line, column)
        self.name = name

    def __str__(self) -> str:
        return f"IdentifierNode({self.name})"


@dataclass
class LiteralNode(ASTNode):
    def __init__(
        self,
        value: Union[int, float, str, bool],
        type_name: Type = Type.i32,
        line: int = -1,
        column: int = -1,
    ):
        super().__init__("Literal", line, column)
        self.value = value
        self.type_name = type_name

    def __str__(self) -> str:
        return f"LiteralNode({self.value}: {self.type_name})"


@dataclass
class AssignmentNode(ASTNode):
    def __init__(
        self,
        identifier: IdentifierNode,
        value: ASTNode,
        line: int = -1,
        column: int = -1,
    ):
        super().__init__("Assignment", line, column)
        self.identifier = identifier
        self.value = value

    def __str__(self) -> str:
        return f"AssignmentNode({self.identifier} = {self.value})"


@dataclass
class ExpressionNode(ASTNode):
    def __init__(
        self,
        operator: str,
        left: ASTNode,
        right: ASTNode,
        line: int = -1,
        column: int = -1,
    ):
        super().__init__("Expression", line, column)
        self.operator = operator
        self.left = left
        self.right = right

    def __str__(self) -> str:
        return f"ExpressionNode({self.left} {self.operator} {self.right})"


@dataclass
class IfNode(ASTNode):
    def __init__(self, condition: ExpressionNode, line: int = -1, column: int = -1):
        super().__init__("If", line, column)
        self.condition = condition
        self.then_block = BlockNode()
        self.else_block: Optional[BlockNode] = None

    def set_else_block(self, else_block: BlockNode) -> None:
        self.else_block = else_block

    def __str__(self) -> str:
        return (
            f"IfNode({self.condition}, then={self.then_block}, else={self.else_block})"
        )


@dataclass
class WhileNode(ASTNode):
    def __init__(self, condition: ExpressionNode, line: int = -1, column: int = -1):
        super().__init__("While", line, column)
        self.condition = condition
        self.body = BlockNode()

    def __str__(self) -> str:
        return f"WhileNode({self.condition}, body={self.body})"


@dataclass
class ReturnNode(ASTNode):
    def __init__(self, value: ASTNode, line: int = -1, column: int = -1):
        super().__init__("Return", line, column)
        self.value = value

    def __str__(self) -> str:
        return f"ReturnNode({self.value})"


@dataclass
class FunctionCallNode(ASTNode):
    def __init__(
        self,
        function_name: str,
        arguments: List[ASTNode],
        line: int = -1,
        column: int = -1,
    ):
        super().__init__("FunctionCall", line, column)
        self.function_name = function_name
        self.arguments = arguments

    def __str__(self) -> str:
        args = ", ".join(str(arg) for arg in self.arguments)
        return f"FunctionCallNode({self.function_name}({args}))"


class ExternalDeclarationNode(ASTNode):
    """Represents an external function declaration"""

    def __init__(
        self,
        name: str,
        return_type: TypeNode,
        parameters: List[FunctionParameterNode],
        line: int = -1,
        column: int = -1,
    ):
        super().__init__("ExternalDeclaration", line, column)
        self.name = name
        self.parameters = parameters
        self.return_type: TypeNode = return_type

        # no need for a body, llvm-ir will handle the declare statement
        self.body: Optional[BlockNode] = None

    def __str__(self) -> str:
        params = ", ".join(str(p) for p in self.parameters)
        return f"ExternalDeclarationNode({self.name}({params}): {self.return_type})"


class SyntaxTokenTypes(Enum):
    # Keywords
    LET = "let"
    FUNCTION = "function"
    RETURN = "return"
    IF = "if"
    ELSE = "else"
    WHILE = "while"
    FOR = "for"
    DECLARE = "declare"

    # Delimiters
    LBRACE = "{"
    RBRACE = "}"
    LPAREN = "("
    RPAREN = ")"
    LBRACKET = "["
    RBRACKET = "]"
    COMMA = ","
    SEMICOLON = ";"  # statement terminator
    COLON = ":"

    # Operators
    ## Arithmetic
    ADD = "+"
    SUB = "-"
    MUL = "*"
    DIV = "/"
    MOD = "%"
    INC = "++"
    DEC = "--"

    ## Comparison
    LT = "<"
    LTE = "<="
    GT = ">"
    GTE = ">="
    EQ = "=="
    NEQ = "!="

    ## Logical
    AND = "&&"
    OR = "||"
    NOT = "!"

    ## Assignment
    ASSIGN = "="
    ADD_ASSIGN = "+="
    SUB_ASSIGN = "-="
    MUL_ASSIGN = "*="
    DIV_ASSIGN = "/="

    ## Bitwise
    BIT_AND = "&"
    BIT_OR = "|"
    BIT_XOR = "^"
    BIT_NOT = "~"
    BIT_SHIFT_LEFT = "<<"
    BIT_SHIFT_RIGHT = ">>"
    BIT_ROTATE_LEFT = "<<<"
    BIT_ROTATE_RIGHT = ">>>"
    BIT_AND_ASSIGN = "&="
    BIT_OR_ASSIGN = "|="
    BIT_XOR_ASSIGN = "^="
    BIT_SHIFT_LEFT_ASSIGN = "<<="
    BIT_SHIFT_RIGHT_ASSIGN = ">>="
    BIT_ROTATE_LEFT_ASSIGN = "<<<="
    BIT_ROTATE_RIGHT_ASSIGN = ">>>="

    # Identifiers
    IDENTIFIER = "IDENTIFIER"

    # Literals
    INTEGER_LITERAL = "i64_literal"
    FLOAT_LITERAL = "f64_literal"
    STRING_LITERAL = "string_literal"
    CHAR_LITERAL = "char_literal"
    BOOL_LITERAL = "bool_literal"

    # Types
    I8 = "i8"
    I16 = "i16"
    I32 = "i32"
    I64 = "i64"
    U8 = "u8"
    U16 = "u16"
    U32 = "u32"
    U64 = "u64"
    F32 = "f32"
    F64 = "f64"
    BOOL = "bool"
    CHAR = "char"
    STRING = "string"
    VARARGS = "..."  # varargs

    # End of file
    EOF = "EOF"
    ERROR = "ERROR"


class SyntaxToken:
    keyword_map = {
        "let": SyntaxTokenTypes.LET,
        "fn": SyntaxTokenTypes.FUNCTION,
        "return": SyntaxTokenTypes.RETURN,
        "if": SyntaxTokenTypes.IF,
        "while": SyntaxTokenTypes.WHILE,
        "else": SyntaxTokenTypes.ELSE,
        "for": SyntaxTokenTypes.FOR,
        "declare": SyntaxTokenTypes.DECLARE,
    }

    type_map = {
        "i8": SyntaxTokenTypes.I8,
        "i16": SyntaxTokenTypes.I16,
        "i32": SyntaxTokenTypes.I32,
        "i64": SyntaxTokenTypes.I64,
        "u8": SyntaxTokenTypes.U8,
        "u16": SyntaxTokenTypes.U16,
        "u32": SyntaxTokenTypes.U32,
        "u64": SyntaxTokenTypes.U64,
        "f32": SyntaxTokenTypes.F32,
        "f64": SyntaxTokenTypes.F64,
        "bool": SyntaxTokenTypes.BOOL,
        "char": SyntaxTokenTypes.CHAR,
        "string": SyntaxTokenTypes.STRING,
        "...": SyntaxTokenTypes.VARARGS,
    }

    operator_map = {
        "+": SyntaxTokenTypes.ADD,
        "-": SyntaxTokenTypes.SUB,
        "*": SyntaxTokenTypes.MUL,
        "/": SyntaxTokenTypes.DIV,
        "%": SyntaxTokenTypes.MOD,
        "<": SyntaxTokenTypes.LT,
        "<=": SyntaxTokenTypes.LTE,
        ">": SyntaxTokenTypes.GT,
        ">=": SyntaxTokenTypes.GTE,
        "==": SyntaxTokenTypes.EQ,
        "!=": SyntaxTokenTypes.NEQ,
        "&&": SyntaxTokenTypes.AND,
        "||": SyntaxTokenTypes.OR,
        "=": SyntaxTokenTypes.ASSIGN,
        "+=": SyntaxTokenTypes.ADD_ASSIGN,
        "-=": SyntaxTokenTypes.SUB_ASSIGN,
        "*=": SyntaxTokenTypes.MUL_ASSIGN,
        "/=": SyntaxTokenTypes.DIV_ASSIGN,
        "++": SyntaxTokenTypes.INC,
        "--": SyntaxTokenTypes.DEC,
        "&": SyntaxTokenTypes.BIT_AND,
        "|": SyntaxTokenTypes.BIT_OR,
        "^": SyntaxTokenTypes.BIT_XOR,
        "~": SyntaxTokenTypes.BIT_NOT,
        "<<": SyntaxTokenTypes.BIT_SHIFT_LEFT,
        ">>": SyntaxTokenTypes.BIT_SHIFT_RIGHT,
        "<<<": SyntaxTokenTypes.BIT_ROTATE_LEFT,
        ">>>": SyntaxTokenTypes.BIT_ROTATE_RIGHT,
        "&=": SyntaxTokenTypes.BIT_AND_ASSIGN,
        "|=": SyntaxTokenTypes.BIT_OR_ASSIGN,
        "^=": SyntaxTokenTypes.BIT_XOR_ASSIGN,
        "<<=": SyntaxTokenTypes.BIT_SHIFT_LEFT_ASSIGN,
        ">>=": SyntaxTokenTypes.BIT_SHIFT_RIGHT_ASSIGN,
        "<<<=": SyntaxTokenTypes.BIT_ROTATE_LEFT_ASSIGN,
        ">>>=": SyntaxTokenTypes.BIT_ROTATE_RIGHT_ASSIGN,
    }

    delimiter_map = {
        "{": SyntaxTokenTypes.LBRACE,
        "}": SyntaxTokenTypes.RBRACE,
        "(": SyntaxTokenTypes.LPAREN,
        ")": SyntaxTokenTypes.RPAREN,
        "[": SyntaxTokenTypes.LBRACKET,
        "]": SyntaxTokenTypes.RBRACKET,
        ",": SyntaxTokenTypes.COMMA,
        ";": SyntaxTokenTypes.SEMICOLON,
        ":": SyntaxTokenTypes.COLON,
    }

    def __init__(self, tkn: Token):
        self.token_type = self._determine_token_type(tkn)
        self.value = tkn.value
        self.line = tkn.line
        self.column = tkn.column

    def _determine_token_type(self, tkn: Token) -> SyntaxTokenTypes:
        if tkn.token_type == TokenType.KEYWORD:
            return self.keyword_map.get(tkn.value, SyntaxTokenTypes.ERROR)
        elif tkn.token_type == TokenType.TYPE:
            return self.type_map.get(tkn.value, SyntaxTokenTypes.ERROR)
        elif tkn.token_type == TokenType.OPERATOR:
            return self.operator_map.get(tkn.value, SyntaxTokenTypes.ERROR)
        elif tkn.token_type == TokenType.DELIMITER:
            return self.delimiter_map.get(tkn.value, SyntaxTokenTypes.ERROR)
        elif tkn.token_type == TokenType.IDENTIFIER:
            return SyntaxTokenTypes.IDENTIFIER
        elif tkn.token_type == TokenType.NUMERICAL_LITERAL:
            return SyntaxTokenTypes.INTEGER_LITERAL
        elif tkn.token_type == TokenType.STRING_LITERAL:
            return SyntaxTokenTypes.STRING_LITERAL
        elif tkn.token_type == TokenType.CHAR_LITERAL:
            return SyntaxTokenTypes.CHAR_LITERAL
        elif tkn.token_type == TokenType.BOOLEAN_LITERAL:
            return SyntaxTokenTypes.BOOL_LITERAL
        elif tkn.token_type == TokenType.EOF:
            return SyntaxTokenTypes.EOF
        return SyntaxTokenTypes.ERROR

    def __str__(self) -> str:
        return f"SyntaxToken({self.token_type}, {self.value})"

    def __repr__(self) -> str:
        return self.__str__()


class Parser:
    types = [
        SyntaxTokenTypes.I8,
        SyntaxTokenTypes.I16,
        SyntaxTokenTypes.I32,
        SyntaxTokenTypes.I64,
        SyntaxTokenTypes.U8,
        SyntaxTokenTypes.U16,
        SyntaxTokenTypes.U32,
        SyntaxTokenTypes.U64,
        SyntaxTokenTypes.F32,
        SyntaxTokenTypes.F64,
        SyntaxTokenTypes.BOOL,
        SyntaxTokenTypes.CHAR,
        SyntaxTokenTypes.STRING,
        SyntaxTokenTypes.VARARGS,
    ]

    def __init__(self, tokens: List[Token] = []):
        self.tokens = [SyntaxToken(t) for t in tokens]
        self.current_token = None
        self.position = -1
        self.advance()

    def advance(self):
        self.position += 1
        if self.position < len(self.tokens):
            self.current_token = self.tokens[self.position]
        else:
            self.current_token = None
        return self.current_token

    def peek(self, n=1):
        pos = self.position + n
        if pos < len(self.tokens):
            return self.tokens[pos]
        return None

    def expect(
        self, token_type: SyntaxTokenTypes | List[SyntaxTokenTypes], err_msg: str = None
    ):
        """Get the next token and check its type
        If the token type matches, advance to the next token.
        If it doesn't match, raise a syntax error.

        Args:
            token_type (SyntaxTokenTypes | List[SyntaxTokenTypes]): The expected token type(s)
            err_msg (str): Optional error message
        Raises:
            SyntaxError: If the current token does not match the expected type
        Returns:
            SyntaxToken: The current token if it matches the expected type

        Note:
            The token_type can be a single type or a list of types.
            If it's a list, the current token must match any one of them.
        """
        if isinstance(token_type, Iterable):
            token_type = [t for t in token_type if isinstance(t, SyntaxTokenTypes)]
        else:
            token_type = [token_type]

        if self.current_token and self.current_token.token_type in token_type:
            token = self.current_token
            self.advance()
            return token
        else:
            err = f"Expected {token_type}, got {self.current_token.token_type if self.current_token else 'EOF'}"
            raise SyntaxError(err)

    def parse(self) -> BlockNode:
        """Parse the entire program"""
        program = BlockNode()
        while (
            self.current_token and self.current_token.token_type != SyntaxTokenTypes.EOF
        ):
            statement = self.parse_statement()
            if statement:
                program.add_statement(statement)
        return program

    def parse_statement(self) -> ASTNode:
        """Parse a single statement"""
        if not self.current_token:
            return None

        token = self.current_token
        if token.token_type == SyntaxTokenTypes.LET:
            return self.parse_variable_declaration()
        elif token.token_type == SyntaxTokenTypes.FUNCTION:
            return self.parse_function_declaration()
        elif token.token_type == SyntaxTokenTypes.DECLARE:
            return self.parse_declare_statement()
        elif token.token_type == SyntaxTokenTypes.RETURN:
            return self.parse_return_statement()
        elif token.token_type == SyntaxTokenTypes.IF:
            return self.parse_if_statement()
        elif token.token_type == SyntaxTokenTypes.WHILE:
            return self.parse_while_statement()
        elif token.token_type == SyntaxTokenTypes.LBRACE:
            return self.parse_block()
        else:
            # Could be an expression or assignment
            expr = self.parse_expression()
            if (
                self.current_token
                and self.current_token.token_type == SyntaxTokenTypes.ASSIGN
            ):
                return self.parse_assignment(expr)
            self.expect(
                SyntaxTokenTypes.SEMICOLON, "Expected semicolon after expression"
            )
            return expr

    def parse_declare_statement(self) -> ExternalDeclarationNode:
        """Parse an external function declaration"""
        self.expect(SyntaxTokenTypes.DECLARE, "Expected 'declare' keyword")

        name_token = self.expect(SyntaxTokenTypes.IDENTIFIER, "Expected function name")
        name = name_token.value

        # Parse parameters
        self.expect(SyntaxTokenTypes.LPAREN, "Expected '(' after function name")
        parameters: List[FunctionParameterNode] = []

        while (
            self.current_token
            and self.current_token.token_type != SyntaxTokenTypes.RPAREN
        ):
            param_name = self.expect(
                SyntaxTokenTypes.IDENTIFIER, "Expected parameter name"
            ).value
            self.expect(SyntaxTokenTypes.COLON, "Expected ':' after parameter name")

            type_token = self.expect(self.types, "Expected parameter type")
            param_type = TypeNode(type_token.value, type_token.line, type_token.column)

            parameters.append(
                FunctionParameterNode(
                    param_name, param_type.type_name, param_type.line, param_type.column
                )
            )

            if (
                self.current_token
                and self.current_token.token_type == SyntaxTokenTypes.COMMA
            ):
                self.advance()

        self.expect(SyntaxTokenTypes.RPAREN, "Expected ')' after parameters")
        self.expect(SyntaxTokenTypes.COLON, "Expected ':' after parameters")
        # Parse return type
        type_token = self.expect(self.types, "Expected return type")
        return_type = TypeNode(type_token.value, type_token.line, type_token.column)
        # Create external function node
        func_node = ExternalDeclarationNode(
            name, return_type, parameters, name_token.line, name_token.column
        )

        self.expect(
            SyntaxTokenTypes.SEMICOLON,
            "Expected ';' after external function declaration",
        )

        return func_node

    def parse_block(self) -> BlockNode:
        """Parse a block of statements enclosed in braces"""
        self.expect(SyntaxTokenTypes.LBRACE, "Expected '{' at start of block")
        block = BlockNode(self.current_token.line, self.current_token.column)

        while (
            self.current_token
            and self.current_token.token_type != SyntaxTokenTypes.RBRACE
            and self.current_token.token_type != SyntaxTokenTypes.EOF
        ):
            statement = self.parse_statement()
            if statement:
                block.add_statement(statement)

        self.expect(SyntaxTokenTypes.RBRACE, "Expected '}' at end of block")
        return block

    def parse_function_declaration(self) -> FunctionDeclarationNode:
        """Parse a function declaration"""
        self.expect(SyntaxTokenTypes.FUNCTION, "Expected 'fn' keyword")

        name_token = self.expect(SyntaxTokenTypes.IDENTIFIER, "Expected function name")
        name = name_token.value

        # Parse parameters
        self.expect(SyntaxTokenTypes.LPAREN, "Expected '(' after function name")
        parameters = []

        while (
            self.current_token
            and self.current_token.token_type != SyntaxTokenTypes.RPAREN
        ):

            param_name = self.expect(
                SyntaxTokenTypes.IDENTIFIER, "Expected parameter name"
            ).value
            self.expect(SyntaxTokenTypes.COLON, "Expected ':' after parameter name")

            type_token = self.expect(
                self.types,
                "Expected parameter type",
            )

            param_type = TypeNode(type_token.value, type_token.line, type_token.column)

            parameters.append(
                FunctionParameterNode(
                    param_name, param_type.type_name, param_type.line, param_type.column
                )
            )

            if (
                self.current_token
                and self.current_token.token_type == SyntaxTokenTypes.COMMA
            ):
                self.advance()

        self.expect(SyntaxTokenTypes.RPAREN, "Expected ')' after parameters")

        # Parse return type if specified
        return_type = TypeNode(Type.i32)  # Default return type
        if (
            self.current_token
            and self.current_token.token_type == SyntaxTokenTypes.COLON
        ):
            self.advance()
            type_token = self.expect(self.types, "Expected return type")
            return_type = TypeNode(type_token.value, type_token.line, type_token.column)

        # Create function node
        func_node = FunctionDeclarationNode(
            name, return_type, parameters, name_token.line, name_token.column
        )

        # Parse function body
        func_node.set_body(self.parse_block())
        return func_node

    def parse_variable_declaration(self) -> VariableDeclarationNode:
        """Parse a variable declaration"""
        self.expect(SyntaxTokenTypes.LET, "Expected 'let' keyword")

        name_token = self.expect(SyntaxTokenTypes.IDENTIFIER, "Expected variable name")
        name = name_token.value

        self.expect(SyntaxTokenTypes.COLON, "Expected ':' after variable name")

        type_token = self.expect(self.types, "Expected variable type")
        var_type = TypeNode(type_token.value, type_token.line, type_token.column)

        # Optional initialization
        value = None
        if (
            self.current_token
            and self.current_token.token_type == SyntaxTokenTypes.ASSIGN
        ):
            self.advance()
            value = self.parse_expression()

        self.expect(
            SyntaxTokenTypes.SEMICOLON, "Expected ';' after variable declaration"
        )
        return VariableDeclarationNode(
            name, var_type, value, name_token.line, name_token.column
        )

    def parse_return_statement(self) -> ReturnNode:
        """Parse a return statement"""
        token = self.expect(SyntaxTokenTypes.RETURN, "Expected 'return' keyword")
        value = self.parse_expression()
        self.expect(SyntaxTokenTypes.SEMICOLON, "Expected ';' after return statement")
        return ReturnNode(value, token.line, token.column)

    def parse_if_statement(self) -> IfNode:
        """Parse an if statement"""
        token = self.expect(SyntaxTokenTypes.IF, "Expected 'if' keyword")
        condition = self.parse_expression()

        if_node = IfNode(condition, token.line, token.column)
        if_node.then_block = self.parse_block()

        # Optional else clause
        if (
            self.current_token
            and self.current_token.token_type == SyntaxTokenTypes.ELSE
        ):
            self.advance()
            if_node.else_block = self.parse_block()

        return if_node

    def parse_while_statement(self) -> WhileNode:
        """Parse a while statement"""
        token = self.expect(SyntaxTokenTypes.WHILE, "Expected 'while' keyword")
        condition = self.parse_expression()

        while_node = WhileNode(condition, token.line, token.column)
        while_node.body = self.parse_block()
        return while_node

    def parse_assignment(self, left: ASTNode) -> AssignmentNode:
        """Parse an assignment statement"""
        if not isinstance(left, IdentifierNode):
            raise SyntaxError("Left side of assignment must be an identifier")

        token = self.expect(SyntaxTokenTypes.ASSIGN, "Expected '=' in assignment")
        right = self.parse_expression()
        self.expect(SyntaxTokenTypes.SEMICOLON, "Expected ';' after assignment")
        return AssignmentNode(left, right, token.line, token.column)

    def parse_expression(self) -> ASTNode:
        """Parse an expression"""
        return self.parse_logical_or()

    def parse_logical_or(self) -> ASTNode:
        """Parse logical OR expressions"""
        left = self.parse_logical_and()

        while (
            self.current_token and self.current_token.token_type == SyntaxTokenTypes.OR
        ):
            op_token = self.current_token
            self.advance()
            right = self.parse_logical_and()
            left = ExpressionNode(
                op_token.value, left, right, op_token.line, op_token.column
            )

        return left

    def parse_logical_and(self) -> ASTNode:
        """Parse logical AND expressions"""
        left = self.parse_equality()

        while (
            self.current_token and self.current_token.token_type == SyntaxTokenTypes.AND
        ):
            op_token = self.current_token
            self.advance()
            right = self.parse_equality()
            left = ExpressionNode(
                op_token.value, left, right, op_token.line, op_token.column
            )

        return left

    def parse_equality(self) -> ASTNode:
        """Parse equality expressions (==, !=)"""
        left = self.parse_relational()

        while self.current_token and self.current_token.token_type in (
            SyntaxTokenTypes.EQ,
            SyntaxTokenTypes.NEQ,
        ):
            op_token = self.current_token
            self.advance()
            right = self.parse_relational()
            left = ExpressionNode(
                op_token.value, left, right, op_token.line, op_token.column
            )

        return left

    def parse_relational(self) -> ASTNode:
        """Parse relational expressions (<, <=, >, >=)"""
        left = self.parse_additive()

        while self.current_token and self.current_token.token_type in (
            SyntaxTokenTypes.LT,
            SyntaxTokenTypes.LTE,
            SyntaxTokenTypes.GT,
            SyntaxTokenTypes.GTE,
        ):
            op_token = self.current_token
            self.advance()
            right = self.parse_additive()
            left = ExpressionNode(
                op_token.value, left, right, op_token.line, op_token.column
            )

        return left

    def parse_additive(self) -> ASTNode:
        """Parse additive expressions (+, -)"""
        left = self.parse_multiplicative()

        while self.current_token and self.current_token.token_type in (
            SyntaxTokenTypes.ADD,
            SyntaxTokenTypes.SUB,
        ):
            op_token = self.current_token
            self.advance()
            right = self.parse_multiplicative()
            left = ExpressionNode(
                op_token.value, left, right, op_token.line, op_token.column
            )

        return left

    def parse_multiplicative(self) -> ASTNode:
        """Parse multiplicative expressions (*, /, %)"""
        left = self.parse_unary()

        while self.current_token and self.current_token.token_type in (
            SyntaxTokenTypes.MUL,
            SyntaxTokenTypes.DIV,
            SyntaxTokenTypes.MOD,
        ):
            op_token = self.current_token
            self.advance()
            right = self.parse_unary()
            left = ExpressionNode(
                op_token.value, left, right, op_token.line, op_token.column
            )

        return left

    def parse_unary(self) -> ASTNode:
        """Parse unary expressions (!, -)"""
        if self.current_token and self.current_token.token_type in (
            SyntaxTokenTypes.NOT,
            SyntaxTokenTypes.SUB,
        ):
            op_token = self.current_token
            self.advance()
            operand = self.parse_unary()
            return ExpressionNode(
                op_token.value, operand, None, op_token.line, op_token.column
            )
        return self.parse_primary()

    def parse_primary(self) -> ASTNode:
        """Parse primary expressions (literals, identifiers, parenthesized expressions)"""
        if not self.current_token:
            raise SyntaxError("Unexpected end of input")

        token = self.current_token

        if token.token_type == SyntaxTokenTypes.LPAREN:
            self.advance()
            expr = self.parse_expression()
            self.expect(SyntaxTokenTypes.RPAREN, "Expected ')' after expression")
            return expr
        elif token.token_type == SyntaxTokenTypes.IDENTIFIER:
            # Could be a variable or function call
            name = token.value
            self.advance()

            if (
                self.current_token
                and self.current_token.token_type == SyntaxTokenTypes.LPAREN
            ):
                # Function call
                self.advance()
                args = []

                while (
                    self.current_token
                    and self.current_token.token_type != SyntaxTokenTypes.RPAREN
                ):
                    args.append(self.parse_expression())
                    if (
                        self.current_token
                        and self.current_token.token_type == SyntaxTokenTypes.COMMA
                    ):
                        self.advance()

                self.expect(
                    SyntaxTokenTypes.RPAREN, "Expected ')' after function arguments"
                )
                return FunctionCallNode(name, args, token.line, token.column)
            else:
                # Variable reference
                return IdentifierNode(name, token.line, token.column)
        elif token.token_type in (
            SyntaxTokenTypes.INTEGER_LITERAL,
            SyntaxTokenTypes.FLOAT_LITERAL,
            SyntaxTokenTypes.STRING_LITERAL,
            SyntaxTokenTypes.CHAR_LITERAL,
            SyntaxTokenTypes.BOOL_LITERAL,
        ):
            self.advance()
            type_name = {
                SyntaxTokenTypes.INTEGER_LITERAL: Type.i32,
                SyntaxTokenTypes.FLOAT_LITERAL: Type.f64,
                SyntaxTokenTypes.STRING_LITERAL: Type.string,
                SyntaxTokenTypes.CHAR_LITERAL: Type.char,
                SyntaxTokenTypes.BOOL_LITERAL: Type.bool,
            }[token.token_type]
            return LiteralNode(token.value, type_name, token.line, token.column)
        else:
            err = (
                f"Unexpected token: {token} at line {token.line}, column {token.column}"
            )
            raise SyntaxError(err)


if __name__ == "__main__":
    from pathlib import Path
    from lexer import Lexer

    f = Path("examples/basic.l98")
    lexer = Lexer()
    lexer.set_input(f)
    tokens = lexer.tokenize()

    parser = Parser(tokens)

    ast = parser.parse()
    print(ast)
