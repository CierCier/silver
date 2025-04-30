from pathlib import Path
import sys
from tokenizer import Token, TokenType, Tokenizer

import bootstrap.silver.silver_types as silver_types
from bootstrap.silver.silver_types import Type

import llvmlite

from __init__ import MODULE_PATH

SYMBOLS = dict()


def add_symbol(symbol: str, type: Type):
    """Add a symbol to the symbol table"""
    if symbol in SYMBOLS:
        raise Exception(f"Symbol {symbol} already defined")
    SYMBOLS[symbol] = type


def get_symbol(symbol: str):
    if symbol not in SYMBOLS:
        return None
    else:
        return SYMBOLS[symbol]


MODULES = set()


def add_module(module: str):
    """Simply keeping track of imported modules"""
    ## I should probably change this to use realpath of files
    if module not in MODULES:
        MODULES.add(module)


class ExprAST:
    def __init__(self):
        pass

    def prefix(self):
        """Get the prefix of the expression.
        Used to store vars in SYMBOLS.
        like \".<scope>.var.<id>.<type>\" etc,
        or \"<id>.<type>\" for global vars
        """
        return ""


class ProgramAST(ExprAST):
    def __init__(self, statements=list[ExprAST]):
        super().__init__()
        self.statements = statements

    def codegen(self):
        """Main Entry point for code generation"""
        for statement in self.statements:
            if statement.codegen:
                statement.codegen()


class ImportAST(ExprAST):
    def __init__(self, module: str):
        super().__init__()
        self.module = module
        """ Ah so import modules, a bit tricky,
            Something like importing C headers, except its our own lang files
            So I'm thinking something like: import <module>.<submodule>.<subsubmodule> ....
            So we need to keep track of the current module, and the imported modules

            anyways heres the process for this ig
                1. Check if module is already imported
                2. if not try to find it in the module path
                3. if not found, raise an error, else goto (4)
                4. add module to the list of imported modules
                5. spawn a new tokenizer instance with the module file
                6. spawn a new parser instance with the new tokens
                7. parse the modules and add statements from the returned ProgramAST to the main ProgramAST
        """

    def __repr__(self):
        return f"ImportAST(module={self.module})"


class NumberExprAST(ExprAST):
    def __init__(self, value: float):
        super().__init__()
        self.value = value

    def __repr__(self):
        return f"NumberExprAST(value={self.value})"


class StringExprAST(ExprAST):
    def __init__(self, value: str):
        super().__init__()
        self.value = value

    def __repr__(self):
        return f"StringExprAST(value={self.value})"

    def prefix(self):
        return f"{self.value}"  # TODO: Add string type


class VariableDeclAST(ExprAST):
    def __init__(self, var_type: Type, var_name: str, init_value: ExprAST):
        super().__init__()
        self.var_type = var_type
        self.var_name = var_name
        self.init_value = init_value

    def __repr__(self):
        return f"VariableDeclAST(var_type={self.var_type}, var_name={self.var_name}, init_value={self.init_value})"


class VariableExprAST(ExprAST):
    def __init__(self, name: str):
        super().__init__()
        self.name = name

    def __repr__(self):
        return f"VariableExprAST(name={self.name})"

    def prefix(self):
        return f"{self.name}"


class BinaryExprAST(ExprAST):
    def __init__(self, op: str, lhs: ExprAST, rhs: ExprAST):
        super().__init__()
        self.op = op
        self.lhs = lhs
        self.rhs = rhs


class UnaryExprAST(ExprAST):
    def __init__(self, op: str, rhs: ExprAST):
        super().__init__()
        self.op = op
        self.rhs = rhs


class CallExprAST(ExprAST):
    def __init__(self, callee: str, args: list[ExprAST]):
        super().__init__()
        self.callee = callee
        self.args = args

    def __repr__(self):
        return f"CallExprAST(callee={self.callee}, args={self.args})"


class FunctionPrototypeAST(ExprAST):
    """Represents the prototype of a function. ie The Declaration of a function"""

    def __init__(self, name: str, args: list[(str, Type)], ret_type: Type):
        super().__init__()
        self.name = name
        self.args = args
        self.ret_type = ret_type
        self.is_variadic = False

    def __repr__(self):
        return f"FunctionPrototypeAST(name={self.name}, args={self.args}, ret_type={self.ret_type})"

    def set_variadic(self):
        """Set the function as variadic"""
        self.is_variadic = True
        self.args.append(("...", silver_types.C_Variatic))

    def prefix(self):
        return f"{self.name}.{self.ret_type}"


class FunctionDefinitionAST(ExprAST):
    """Represents the definition of a function. ie The Implementation of a function"""

    def __init__(
        self, proto: FunctionPrototypeAST, body: ExprAST, defer_body: ExprAST = None
    ):
        super().__init__()
        self.proto = proto
        self.body = body
        self.defer_body = defer_body
        ## Self note for defer statements
        # upon parsing a defer statement, we should add the defer body to the function body
        # the defer_body will be converted into its own Label. which will run before the function return
        ## How though?
        # upon encountering a return statement, we move the value to a return variable(Which always exists), and then
        # jump to the defer Label, run the defer block, then jump to return
        # if defer is empty, we jump to return, (this should be known at compile time)

    def __repr__(self):
        return f"FunctionDefinitionAST(proto={self.proto}, body={self.body})"


class BlockAST(ExprAST):
    """Represents a block of code. ie A function body or a block of statements"""

    def __init__(self, statements: list[ExprAST]):
        super().__init__()
        self.statements = statements

    def __repr__(self):
        return f"BlockAST(statements={self.statements})"


class IfAST(ExprAST):
    def __init__(self, cond: ExprAST, then: ExprAST, else_: ExprAST = None):
        super().__init__()
        self.cond = cond
        self.then = then
        self.else_ = else_

    def __repr__(self):
        return f"IfAST(cond={self.cond}, then={self.then}, else_={self.else_})"


class WhileAST(ExprAST):
    def __init__(self, cond: ExprAST, body: ExprAST):
        super().__init__()
        self.cond = cond
        self.body = body

    def __repr__(self):
        return f"WhileAST(cond={self.cond}, body={self.body})"


class ForAST(ExprAST):
    def __init__(
        self, var: VariableExprAST, start: ExprAST, end: ExprAST, body: ExprAST
    ):
        super().__init__()
        self.var = var
        self.start = start
        self.end = end
        self.body = body

    def __repr__(self):
        return f"ForAST(var={self.var}, start={self.start}, end={self.end}, body={self.body})"


class ReturnAST(ExprAST):
    def __init__(self, value: ExprAST):
        super().__init__()
        self.value = value

    def __repr__(self):
        return f"ReturnAST(value={self.value})"


class Parser:

    OPERATORS = {
        TokenType.SHIFT_LEFT_ASSIGN,
        TokenType.SHIFT_RIGHT_ASSIGN,
        TokenType.SHIFT_LEFT,
        TokenType.SHIFT_RIGHT,
        TokenType.EQUAL,
        TokenType.NOT_EQUAL,
        TokenType.LESS_EQUAL,
        TokenType.GREATER_EQUAL,
        TokenType.AND,
        TokenType.OR,
        TokenType.INCREMENT,
        TokenType.DECREMENT,
        TokenType.INCREMENT_ASSIGN,
        TokenType.DECREMENT_ASSIGN,
        TokenType.MULTIPLY_ASSIGN,
        TokenType.DIVIDE_ASSIGN,
        TokenType.PLUS,
        TokenType.MINUS,
        TokenType.MULTIPLY,
        TokenType.DIVIDE,
        TokenType.MODULO,
        TokenType.ASSIGN,
        TokenType.LESS,
        TokenType.GREATER,
        TokenType.NOT,
    }

    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.token_idx = -1
        self.cur_token = None
        self.next_token()  # Initialize the first token

    def next_token(self):
        """Get the next token from the list of tokens"""
        if self.token_idx >= len(self.tokens):
            self.cur_token = Token(TokenType.EOF, "")
            return
        self.token_idx += 1
        self.cur_token = self.tokens[self.token_idx]

    def parse_program(self) -> ProgramAST:
        statements = []
        while self.cur_token.type != TokenType.EOF:
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
        return ProgramAST(statements)

    def parse_statement(self):
        if self.cur_token.type == TokenType.KEYWORD:
            if self.cur_token.value == "import":
                return self.parse_import()
            elif self.cur_token.value == "func":
                return self.parse_function()
            elif self.cur_token.value == "let":
                return self.parse_variable_declaration()
            elif self.cur_token.value == "return":
                return self.parse_return()
            elif self.cur_token.value == "for":
                return self.parse_for()
            elif self.cur_token.value == "while":
                return self.parse_while()
            elif self.cur_token.value == "if":
                return self.parse_if()
            elif self.cur_token.value == "defer":
                return self.parse_defer()
            else:
                raise Exception(f"Unknown keyword {self.cur_token.value}")
        else:
            return self.parse_expression()

    def parse_import(self) -> ImportAST:
        self.next_token()  # eat 'import'
        module_name = ""

        if self.cur_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected module name after 'import'")
        module_name += self.cur_token.value
        self.next_token()  # eat module name

        while self.cur_token.type == TokenType.DOT:
            self.next_token()
            if self.cur_token.type != TokenType.IDENTIFIER:
                raise Exception("Expected submodule name after '.'")
            module_name += "." + self.cur_token.value
            self.next_token()

        self.next_token()  # eat ';'
        return ImportAST(module_name)

    def parse_function(self) -> FunctionDefinitionAST | FunctionPrototypeAST:
        self.next_token()  # eat 'func'

        if self.cur_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected function name after 'func'")
        func_name = self.cur_token.value
        self.next_token()

        if self.cur_token.type != TokenType.LPAREN:
            raise Exception("Expected '(' after function name")
        self.next_token()
        args = []
        is_variadic = False

        while self.cur_token.type != TokenType.RPAREN:
            if self.cur_token.type == TokenType.VARIANT:
                is_variadic = True
                self.next_token()
                break
            if self.cur_token.type != TokenType.IDENTIFIER:
                raise Exception("Expected Type after '(")
            arg_type = self.cur_token.value
            self.next_token()
            if self.cur_token.type != TokenType.IDENTIFIER:
                raise Exception("Expected argument name after type")
            arg_name = self.cur_token.value
            self.next_token()
            args.append((arg_name, arg_type))
            if self.cur_token.type == TokenType.COMMA:
                self.next_token()
        self.next_token()  # eat ')'
        if self.cur_token.type != TokenType.IDENTIFIER:
            return_type = "void"
        else:
            return_type = self.cur_token.value
            self.next_token()

        proto = FunctionPrototypeAST(func_name, args, return_type)
        if is_variadic:
            proto.set_variadic()
        add_symbol(func_name, proto)

        if self.cur_token.type == TokenType.LBRACE:
            # Function definition
            body = self.parse_block()
            return FunctionDefinitionAST(proto, body)
        else:
            # Function prototype
            if self.cur_token.type != TokenType.SEMICOLON:
                raise Exception("Expected ';' after function prototype")
            self.next_token()
            return proto

    def parse_block(self) -> BlockAST:
        if self.cur_token.type != TokenType.LBRACE:
            raise Exception("Expected '{' to start a block")
        self.next_token()

        statements = []
        while self.cur_token.type != TokenType.RBRACE:
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)

        self.next_token()  # eat '}'
        return BlockAST(statements)

    def parse_if(self) -> IfAST:
        self.next_token()  # eat 'if'
        cond = self.parse_expression()
        then = self.parse_block()
        else_ = None
        if self.cur_token.type == TokenType.KEYWORD and self.cur_token.value == "else":
            self.next_token()
            else_ = self.parse_block()
        return IfAST(cond, then, else_)

    def parse_while(self) -> WhileAST:
        self.next_token()  # eat 'while'
        cond = self.parse_expression()
        body = self.parse_block()
        return WhileAST(cond, body)

    def parse_for(self) -> ForAST:
        self.next_token()  # eat 'for'
        if self.cur_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected variable name after 'for'")
        var_name = self.cur_token.value
        var = VariableExprAST(var_name)
        self.next_token()

        if self.cur_token.type != TokenType.COLON:
            raise Exception("Expected ':' after variable in 'for'")
        self.next_token()

        start = self.parse_expression()
        if self.cur_token.type != TokenType.DOTDOT:
            raise Exception("Expected '..' in for range")
        self.next_token()

        end = self.parse_expression()
        body = self.parse_block()
        return ForAST(var, start, end, body)

    def parse_return(self) -> ReturnAST:
        self.next_token()  # eat 'return'
        value = self.parse_expression()
        return ReturnAST(value)

    def parse_expression(self) -> ExprAST:
        lhs = self.parse_primary()
        bop = self.parse_binary_op_rhs(0, lhs)
        if self.cur_token.type == TokenType.SEMICOLON:
            self.next_token()
        return bop

    def parse_primary(self) -> ExprAST:
        if self.cur_token.type == TokenType.NUMBER:
            value = float(self.cur_token.value)
            self.next_token()
            return NumberExprAST(value)
        elif self.cur_token.type == TokenType.STRING:
            value = self.cur_token.value
            self.next_token()
            return StringExprAST(value)

        elif self.cur_token.type == TokenType.IDENTIFIER:
            id_name = self.cur_token.value
            self.next_token()
            if self.cur_token.type == TokenType.LPAREN:
                # Call expression
                self.next_token()
                args = []
                while self.cur_token.type != TokenType.RPAREN:
                    arg = self.parse_expression()
                    args.append(arg)
                    if self.cur_token.type == TokenType.COMMA:
                        self.next_token()
                self.next_token()  # eat ')'
                return CallExprAST(id_name, args)
            else:
                return VariableExprAST(id_name)
        elif self.cur_token.type == TokenType.LPAREN:
            self.next_token()
            expr = self.parse_expression()
            if self.cur_token.type != TokenType.RPAREN:
                raise Exception("Expected ')'")
            self.next_token()
            return expr
        else:
            raise Exception(
                f"Unknown token when expecting an expression: {self.cur_token}"
            )

    def parse_binary_op_rhs(self, expr_prec: int, lhs: ExprAST) -> ExprAST:
        while True:
            tok_prec = self.get_token_precedence()

            if tok_prec < expr_prec:
                return lhs

            bin_op = self.cur_token.value
            self.next_token()

            rhs = self.parse_primary()
            next_prec = self.get_token_precedence()
            if tok_prec < next_prec:
                rhs = self.parse_binary_op_rhs(tok_prec + 1, rhs)

            lhs = BinaryExprAST(bin_op, lhs, rhs)

    def get_token_precedence(self) -> int:
        if self.cur_token.type not in self.OPERATORS:
            return -1

        prec = {
            "=": 2,
            "<": 10,
            ">": 10,
            "+": 20,
            "-": 20,
            "*": 40,
            "/": 40,
        }

        return prec.get(self.cur_token.value, -1)

    def parse_variable_declaration(self) -> VariableDeclAST:
        self.next_token()  # eat 'let'
        if self.cur_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected Type after 'let'")
        var_type = self.cur_token.value
        self.next_token()
        if self.cur_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected variable name after type")

        var_name = self.cur_token.value
        self.next_token()

        if self.cur_token.type == TokenType.ASSIGN:
            self.next_token()
            init_value = self.parse_expression()
        else:
            init_value = None

        # self.next_token()
        var_decl = VariableDeclAST(var_type, var_name, init_value)
        add_symbol(var_name, var_type)
        return var_decl

    def parse_defer(self) -> ExprAST:
        self.next_token()  # eat 'defer'
        if self.cur_token.type == TokenType.LBRACE:
            # Defer block
            self.next_token()
            statements = []
            while self.cur_token.type != TokenType.RBRACE:
                stmt = self.parse_statement()
                if stmt:
                    statements.append(stmt)
            self.next_token()
            return BlockAST(statements)
        else:
            # Defer expression
            expr = self.parse_expression()
            if self.cur_token.type != TokenType.SEMICOLON:
                raise Exception("Expected ';' after defer expression")
            self.next_token()
            return expr


def print_ast(ast: ExprAST, indent: int = 0):
    """Print the AST in a readable format"""

    if isinstance(ast, ProgramAST):
        print(" " * indent + "Program:")
        for stmt in ast.statements:
            print_ast(stmt, indent + 2)
    elif isinstance(ast, ImportAST):
        print(" " * indent + f"Import: {ast.module}")
    elif isinstance(ast, FunctionPrototypeAST):
        print(
            " " * indent
            + f"FunctionPrototype: {ast.name}({', '.join([f'{arg[0]}: {arg[1]}' for arg in ast.args])}) -> {ast.ret_type}"
        )
    elif isinstance(ast, FunctionDefinitionAST):
        print(" " * indent + f"FunctionDefinition: {ast.proto.name}")
        print_ast(ast.body, indent + 2)
    elif isinstance(ast, VariableDeclAST):
        print(
            " " * indent
            + f"VariableDecl: {ast.var_name} ({ast.var_type}) = {ast.init_value}"
        )
    elif isinstance(ast, VariableExprAST):
        print(" " * indent + f"VariableExpr: {ast.name}")
    elif isinstance(ast, NumberExprAST):
        print(" " * indent + f"NumberExpr: {ast.value}")
    elif isinstance(ast, StringExprAST):
        print(" " * indent + f"StringExpr: {ast.value}")
    elif isinstance(ast, BinaryExprAST):
        print(" " * indent + f"BinaryExpr: {ast.op}")
        print_ast(ast.lhs, indent + 2)
        print_ast(ast.rhs, indent + 2)
    elif isinstance(ast, CallExprAST):
        print(
            " " * indent
            + f"CallExpr: {ast.callee}({', '.join([str(arg) for arg in ast.args])})"
        )
    elif isinstance(ast, BlockAST):
        print(" " * indent + "Block:")
        for stmt in ast.statements:
            print_ast(stmt, indent + 2)
    elif isinstance(ast, ReturnAST):
        print(" " * indent + f"Return: {ast.value}")
    else:
        raise Exception(f"Unknown AST node type: {type(ast)}")


if __name__ == "__main__":
    from preprocess import Preprocessor

    files = [Path(x) for x in sys.argv[1:]]
    preprocessor = Preprocessor(*files)
    f_data = preprocessor.preprocess()

    tokenizer = Tokenizer(f_data)
    tokens = tokenizer.get_all()

    # for token in tokens:
    #     print(token)

    parser = Parser(tokens)

    print_ast(parser.parse_program())
