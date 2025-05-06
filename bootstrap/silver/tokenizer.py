import re
from typing import List, Tuple, Dict
from enum import Enum
import sys

from pathlib import Path


class Position:

    def __init__(self, line: int, column: int, file: Path = None):
        self.line = line
        self.column = column
        self.file = file

    def __str__(self):
        return f"{self.file}:{self.line}:{self.column}"


STRING_ESCAPES = {
    "\\n": "\n",
    "\\r": "\r",
    "\\t": "\t",
    "\\v": "\v",
    "\\f": "\f",
    "\\b": "\b",
    "\\a": "\a",
    "\\'": "'",
    '\\"': '"',
    "\\\\": "\\",
    "\\?": "?",
    "\\0": "\0",
}


class TokenType(Enum):
    # Semantics

    KEYWORD = "keyword"
    # We will only categorize the tokens into identifiers for now
    # The parser can determine if it is a variable, function, type etc...
    IDENTIFIER = "identifier"

    # Values
    NUMBER = "number"
    STRING = "string"

    # OPERATORS
    PLUS = "+"
    MINUS = "-"
    MULTIPLY = "*"
    DIVIDE = "/"
    MODULO = "%"
    ASSIGN = "="
    EQUAL = "=="
    NOT_EQUAL = "!="
    LESS = "<"
    LESS_EQUAL = "<="
    GREATER = ">"
    GREATER_EQUAL = ">="
    AND = "&&"
    OR = "||"
    NOT = "!"

    INCREMENT = "++"
    DECREMENT = "--"

    INCREMENT_ASSIGN = "+="
    DECREMENT_ASSIGN = "-="
    MULTIPLY_ASSIGN = "*="
    DIVIDE_ASSIGN = "/="

    SHIFT_LEFT = "<<"
    SHIFT_RIGHT = ">>"
    SHIFT_LEFT_ASSIGN = "<<="
    SHIFT_RIGHT_ASSIGN = ">>="

    # Delimiters
    LPAREN = "("
    RPAREN = ")"
    LBRACE = "{"
    RBRACE = "}"
    LBRACKET = "["
    RBRACKET = "]"
    COMMA = ","
    SEMICOLON = ";"
    COLON = ":"
    DOT = "."
    VARIANT = "..."

    # Special
    EOF = "EOF"
    COMMENT_SINGLE = "//"
    COMMENT_MULTI_START = "/*"
    COMMENT_MULTI_END = "*/"

    WHITESPACE = "whitespace"
    NEWLINE = "newline"
    INVALID = "ERROR"

    FILECHANGE = "FILECHANGE"


class Token:

    def __init__(self, type: TokenType, value: str, position: Position):
        self.type = type
        self.value = value
        self.position = position

    def __str__(self):
        return f'{self.type.name} "{self.value}" at {self.position}'


class Tokenizer:

    token_patterns: Dict[TokenType, re.Pattern] = {
        # tokenizer specific to send figure out the source file
        # //FILE <file>
        # //FILE END
        TokenType.FILECHANGE: re.compile(r"//FILE (.*)"),
        # Whitespace (we'll skip this)
        TokenType.WHITESPACE: re.compile(r"[ \t\r]+"),
        TokenType.NEWLINE: re.compile(r"[\n\r]"),
        # Comments
        TokenType.COMMENT_SINGLE: re.compile(r"//.*?(?=\n|$)"),
        TokenType.COMMENT_MULTI_START: re.compile(r"/\*.*?\*/", re.DOTALL),
        # Identifiers and keywords
        TokenType.IDENTIFIER: re.compile(r"[a-zA-Z_][a-zA-Z0-9_]*"),
        # Literals
        ## Number Literals can be integers or floats in Decimal or scientific notation
        TokenType.NUMBER: re.compile(
            r"""
    (0x[0-9a-fA-F]+)       # Hexadecimal (e.g., 0x1A3F)
    |                       # OR
    (\d+\.?\d*              # Decimal with optional fractional part
    ([eE][+-]?\d+)?)        # Optional exponent part
    |                       # OR
    (\.\d+                  # Numbers starting with decimal point
    ([eE][+-]?\d+)?)        # Optional exponent part
""",
            re.VERBOSE,
        ),
        #
        #
        TokenType.STRING: re.compile(r'"(?:\\.|[^"\\])*"'),
        # Operators (ordered by precedence - longer first)
        TokenType.SHIFT_LEFT_ASSIGN: re.compile(r"<<="),
        TokenType.SHIFT_RIGHT_ASSIGN: re.compile(r">>="),
        TokenType.SHIFT_LEFT: re.compile(r"<<"),
        TokenType.SHIFT_RIGHT: re.compile(r">>"),
        TokenType.EQUAL: re.compile(r"=="),
        TokenType.NOT_EQUAL: re.compile(r"!="),
        TokenType.LESS_EQUAL: re.compile(r"<="),
        TokenType.GREATER_EQUAL: re.compile(r">="),
        TokenType.AND: re.compile(r"&&"),
        TokenType.OR: re.compile(r"\|\|"),
        TokenType.INCREMENT: re.compile(r"\+\+"),
        TokenType.DECREMENT: re.compile(r"--"),
        TokenType.INCREMENT_ASSIGN: re.compile(r"\+="),
        TokenType.DECREMENT_ASSIGN: re.compile(r"-="),
        TokenType.MULTIPLY_ASSIGN: re.compile(r"\*="),
        TokenType.DIVIDE_ASSIGN: re.compile(r"/="),
        TokenType.PLUS: re.compile(r"\+"),
        TokenType.MINUS: re.compile(r"-"),
        TokenType.MULTIPLY: re.compile(r"\*"),
        TokenType.DIVIDE: re.compile(r"/"),
        TokenType.MODULO: re.compile(r"%"),
        TokenType.ASSIGN: re.compile(r"="),
        TokenType.LESS: re.compile(r"<"),
        TokenType.GREATER: re.compile(r">"),
        TokenType.NOT: re.compile(r"!"),
        # Delimiters
        TokenType.LPAREN: re.compile(r"\("),
        TokenType.RPAREN: re.compile(r"\)"),
        TokenType.LBRACE: re.compile(r"\{"),
        TokenType.RBRACE: re.compile(r"\}"),
        TokenType.LBRACKET: re.compile(r"\["),
        TokenType.RBRACKET: re.compile(r"\]"),
        TokenType.COMMA: re.compile(r","),
        TokenType.SEMICOLON: re.compile(r";"),
        TokenType.COLON: re.compile(r":"),
        TokenType.VARIANT: re.compile(r"\.\.\."),
        TokenType.DOT: re.compile(r"\."),
    }

    keywords = {
        "func": TokenType.KEYWORD,
        "let": TokenType.KEYWORD,
        "if": TokenType.KEYWORD,
        "else": TokenType.KEYWORD,
        "for": TokenType.KEYWORD,
        "while": TokenType.KEYWORD,
        "continue": TokenType.KEYWORD,
        "break": TokenType.KEYWORD,
        "return": TokenType.KEYWORD,
        "true": TokenType.KEYWORD,
        "false": TokenType.KEYWORD,
        "import": TokenType.KEYWORD,
        "struct": TokenType.KEYWORD,
        "enum": TokenType.KEYWORD,
        "union": TokenType.KEYWORD,
    }

    whitespace = set(" \t\r\n")

    def __init__(self, data: str):
        self.tokens = []

        self.file_stack = []

        self.current_file: Path = None
        self.data = data
        self.cursor = 0

        self.current_line = 1
        self.current_column = 1

    def get_next_token(self):
        """Get the next token from the input stream"""

        if self.cursor >= len(self.data):
            return Token(
                TokenType.EOF,
                "",
                Position(self.current_line, self.current_column, self.current_file),
            )

        # Skip whitespace first
        self.skip_whitespace()

        if self.cursor >= len(self.data):
            return Token(
                TokenType.EOF,
                "",
                Position(self.current_line, self.current_column, self.current_file),
            )

        remaining_text = self.data[self.cursor :]

        # Check for keywords first
        for keyword, tok_type in self.keywords.items():
            if remaining_text.startswith(keyword):
                # Ensure it's not part of a larger identifier
                next_char_pos = len(keyword)
                if (next_char_pos >= len(remaining_text)) or not remaining_text[
                    next_char_pos
                ].isalnum():
                    token = Token(
                        tok_type,
                        keyword,
                        Position(
                            self.current_line, self.current_column, self.current_file
                        ),
                    )
                    self.seek(len(keyword))
                    return token

        # Check all token patterns in order
        for token_type, pattern in self.token_patterns.items():
            match = pattern.match(remaining_text)
            if match:
                value = match.group(0)
                if token_type == TokenType.FILECHANGE:
                    v = match.group(1)
                    if v == "END":
                        self.current_file = (
                            self.file_stack[-1] if self.file_stack else None
                        )
                        if self.current_file is not None:
                            (
                                self.current_file,
                                self.current_line,
                                self.current_column,
                            ) = self.current_file
                            self.file_stack.pop()
                            self.current_line -= (
                                1  # Adjust to fix the removal of the import statement
                            )
                    else:
                        self.current_file = (
                            self.current_file,
                            self.current_line,
                            self.current_column,
                        )
                        self.file_stack.append(self.current_file)
                        self.current_file = Path(v)
                        self.current_line = 1
                        self.current_column = 1

                token = Token(
                    token_type,
                    value,
                    Position(self.current_line, self.current_column, self.current_file),
                )
                self.seek(len(value))

                if token_type == TokenType.NUMBER:
                    # Process number literals
                    token.value = self.process_number(token)

                # Special handling for comments to skip them
                if token_type in (
                    TokenType.COMMENT_SINGLE,
                    TokenType.COMMENT_MULTI_START,
                ):
                    return self.get_next_token()  # Skip comments and get next token

                if token_type == TokenType.STRING:
                    token.value = token.value[1:-1]  # Remove the quotes
                    for k, v in STRING_ESCAPES.items():
                        token.value = token.value.replace(k, v)

                return token

        # If no pattern matched, it's an invalid token
        invalid_char = remaining_text[0]
        token = Token(
            TokenType.INVALID,
            invalid_char,
            Position(self.current_line, self.current_column, self.current_file),
        )
        self.seek(1)
        return token

    def skip_to(self, tok: str):
        """Skip to the next token of type tok, returns everything consumed in the path"""
        v = ""
        while self.cursor < len(self.data):

            # if equal, add the token to the string and return
            if self.__is_equal(self.data[self.cursor :], tok):
                v += tok
                self.seek(len(tok))
                break
            v += self.data[self.cursor]
            self.seek(1)
        return v

    def skip_whitespace(self):
        while self.cursor < len(self.data) and (
            self.data[self.cursor] in self.whitespace
        ):
            self.seek(1)

    def seek(self, n: int):
        """Moves the cursor forward by n characters in the file"""
        while n > 0 and self.cursor < len(self.data):
            if self.data[self.cursor] == "\n":
                self.current_line += 1
                self.current_column = 0
            self.cursor += 1
            self.current_column += 1
            n -= 1

    def __is_equal(self, a: str, b: str) -> bool:
        """Check if A is equal to B. only checks the first len(b) characters of a"""
        l = len(b)
        if len(a) < l:
            return False

        for i, (_a, _b) in enumerate(zip(a, b)):
            if i >= l:
                break
            if _a != _b:
                return False
        return True

    def get_all(self):
        tokens: list[Token] = []

        current_token = self.get_next_token()

        while current_token is not None and current_token.type != TokenType.EOF:
            tokens.append(current_token)
            current_token = self.get_next_token()

        ## Remove EOF's in the middle of the token stream, only keep the last one
        tokens = [
            tok
            for tok in tokens
            if tok.type != TokenType.EOF and tok.type != TokenType.FILECHANGE
        ]
        tokens.append(
            Token(
                TokenType.EOF,
                "",
                Position(self.current_line, self.current_column, self.current_file),
            )
        )

        return tokens

    def process_number(self, token):
        """Process a number token to determine if it's a float or integer."""
        value = token.value
        if "e" in value.lower() or "." in value:
            # It's a float, keep it as is
            return value
        elif value.startswith("0x") or value.startswith("0X") or value.startswith("x"):
            # It's a hex number, convert to decimal
            return str(int(value, 16))
        else:
            # It's a decimal integer
            return value


def test():
    samples = [Path(x) for x in sys.argv[1::]]
    preprocessor = Preprocessor(*samples)
    data = preprocessor.preprocess()

    tokenizer = Tokenizer(data)

    tokens = tokenizer.get_all()

    for tok in tokens:
        print(tok)


if __name__ == "__main__":
    from preprocess import Preprocessor

    test()
