from enum import Enum
from pathlib import Path
from typing import List


class TokenType(Enum):
    KEYWORD = 0
    IDENTIFIER = 1
    OPERATOR = 2  # +, -, *, /, <, <=, ==, >=, >, !=, &&, ||, =, +=, -=, *=, /=, ++, --, &, |, ^, ~
    NUMERICAL_LITERAL = 3  # 123, 123.456
    CHAR_LITERAL = 4  # 'a' or 'A'
    STRING_LITERAL = 5  # "hello world"
    BOOLEAN_LITERAL = 6  # true, false

    DELIMITER = 7  # () {} [] , ; . :

    COMMENT_SINGLE = 8  # //
    COMMENT_MULTI_START = 9  # /*
    COMMENT_MULTI_END = 10  # */

    TYPE = 11

    EOF = 9999


class Token:
    keywords = {"let", "if", "else", "while", "for", "return", "fn", "declare"}
    delimiters = {"(", ")", "{", "}", "[", "]", ",", ";", ":"}
    operators = {
        "+",
        "-",
        "*",
        "/",
        "<",
        "<=",
        "==",
        ">=",
        ">",
        "!=",
        "&&",
        "||",
        "=",
        "+=",
        "-=",
        "*=",
        "/=",
        "++",
        "--",
        "&",
        "|",
        "^",
        "~",
        "!",
    }

    types = {
        "i8",
        "i16",
        "i32",
        "i64",
        "u8",
        "u16",
        "u32",
        "u64",
        "f32",
        "f64",
        "bool",
        "char",
        "string",
        "...",  # varargs
    }

    identifiers = []
    tokens = []

    def __init__(self, token_type: TokenType, value: str, line: int, column: int):
        self.token_type = token_type
        self.value = value
        self.line = line
        self.column = column


class Lexer:
    def __init__(self):
        self.code = []
        self.c_file = None
        self.c_line = None
        self.c_column = None

    def set_input(self, file: Path):
        self.c_file = file
        self.c_line = 0
        self.c_column = 0

        with open(file, "r") as f:
            self.code = f.readlines()

    def advance(self, n: int):
        self.c_column += n
        if self.c_column >= len(self.code[self.c_line]):
            self.c_column -= len(self.code[self.c_line])
            self.c_line += 1

    def peek(self, n: int = 0) -> str:
        if self.is_eof():
            return None
        effective_x = self.c_column + n
        effective_y = self.c_line
        if effective_x >= len(self.code[self.c_line]):
            effective_x -= len(self.code[self.c_line])
            effective_y += 1

        if effective_y >= len(self.code):
            return None

        return self.code[effective_y][effective_x]

    def skip_whitespace(self):
        while (
            self.c_line < len(self.code)
            and self.c_column < len(self.code[self.c_line])
            and self.peek().isspace()
        ):
            if self.peek() == "\n":
                self.c_line += 1
                self.c_column = 0
            else:
                self.c_column += 1
        if self.c_line >= len(self.code):
            self.c_column = len(self.code[self.c_line - 1])
            self.c_line = len(self.code)
            return

    def skip_comment(self):
        if self.peek() == "/" and self.peek(1) == "/":
            while (
                self.c_line < len(self.code)
                and self.c_column < len(self.code[self.c_line])
                and self.peek() != "\n"
            ):
                self.advance(1)
            self.c_column = 0
            self.c_line += 1
        self.skip_whitespace()

    def skip_multiline_comment(self):
        if self.peek() == "/" and self.peek(1) == "*":
            self.advance(2)
            while (
                self.c_line < len(self.code)
                and self.c_column < len(self.code[self.c_line])
                and not (self.peek() == "*" and self.peek(1) == "/")
            ):
                if self.peek() == "\n":
                    self.c_line += 1
                    self.c_column = 0
                else:
                    self.c_column += 1
            if not self.is_eof():
                self.advance(2)
            else:
                raise Exception("Unterminated multiline comment")

        self.skip_whitespace()

    def is_eof(self) -> bool:
        return self.c_line >= len(self.code)

    def is_keyword(self, token: str) -> bool:
        return token in Token.keywords

    def is_identifier(self, token: str) -> bool:
        return (
            token.isidentifier()
            and token not in Token.keywords
            and token not in Token.operators
            and token not in Token.delimiters
        )

    def is_boolean_literal(self, token: str) -> bool:
        return token in {"true", "false"}

    def is_operator(self, token: str) -> bool:
        return token in Token.operators

    def is_delimiter(self, token: str) -> bool:
        return token in Token.delimiters

    def get_number(self) -> str:
        number = ""
        while (
            self.c_line < len(self.code)
            and self.c_column < len(self.code[self.c_line])
            and self.peek().isdigit()
        ):
            number += self.peek()
            self.advance(1)
        return number

    def get_identifier(self) -> str:
        identifier = ""
        while (
            self.c_line < len(self.code)
            and self.c_column < len(self.code[self.c_line])
            and (self.peek().isalnum() or self.peek() == "_" or self.peek() == ".")
        ):
            identifier += self.peek()
            self.advance(1)
        return identifier

    def get_string(self) -> str:
        string = ""
        self.advance(1)
        while (
            self.c_line < len(self.code)
            and self.c_column < len(self.code[self.c_line])
            and self.peek() != '"'
        ):
            string += self.peek()
            self.advance(1)
        self.advance(1)
        return string

    def get_char(self) -> str:
        char = ""
        self.advance(1)
        while (
            self.c_line < len(self.code)
            and self.c_column < len(self.code[self.c_line])
            and self.peek() != "'"
        ):
            char += self.peek()
            self.advance(1)
        self.advance(1)
        return char

    def get_operator(self) -> str:
        operator = ""
        while (
            self.c_line < len(self.code)
            and self.c_column < len(self.code[self.c_line])
            and self.peek() in Token.operators
        ):
            operator += self.peek()
            self.advance(1)
        return operator

    def get_next_token(self) -> Token:
        self.skip_whitespace()
        self.skip_comment()
        self.skip_multiline_comment()

        if self.is_eof():
            return Token(TokenType.EOF, "EOF", self.c_line, self.c_column)

        char = self.peek()

        if char.isdigit():
            number = self.get_number()
            return Token(
                TokenType.NUMERICAL_LITERAL, number, self.c_line, self.c_column
            )

        if char == "t" or char == "f":
            boolean = self.get_identifier()
            if boolean in {"true", "false"}:
                return Token(
                    TokenType.BOOLEAN_LITERAL, boolean, self.c_line, self.c_column
                )
            else:
                l = len(boolean)
                # rollback the advance
                self.advance(-l)

        if char == '"':
            string = self.get_string()
            return Token(TokenType.STRING_LITERAL, string, self.c_line, self.c_column)

        if char == "'":
            char = self.get_char()
            return Token(TokenType.CHAR_LITERAL, char, self.c_line, self.c_column)

        if char.isalpha() or char == "_" or char == ".":
            identifier = self.get_identifier()
            if identifier in Token.keywords:
                return Token(TokenType.KEYWORD, identifier, self.c_line, self.c_column)
            elif identifier in Token.types:
                return Token(TokenType.TYPE, identifier, self.c_line, self.c_column)
            else:
                return Token(
                    TokenType.IDENTIFIER, identifier, self.c_line, self.c_column
                )

        if char in Token.operators:
            operator = self.get_operator()
            return Token(TokenType.OPERATOR, operator, self.c_line, self.c_column)

        if char in Token.delimiters:
            delimiter = char
            self.advance(1)
            return Token(TokenType.DELIMITER, delimiter, self.c_line, self.c_column)

        ex = Exception(f"Unknown character: {char}")
        raise ex

    def tokenize(self) -> List[Token]:
        tokens = []
        while not self.is_eof():
            token = self.get_next_token()
            if token is not None:
                tokens.append(token)
        return tokens


if __name__ == "__main__":
    lexer = Lexer()
    lexer.set_input(Path("examples/basic.l98"))
    tokens = lexer.tokenize()
    for token in tokens:
        print(f"{token.token_type.name}: {token.value} ({token.line}, {token.column})")
