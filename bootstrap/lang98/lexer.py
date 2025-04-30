import sys
from tokenizer import Token, TokenType, Tokenizer

from pathlib import Path


class Lexer:
    """Lexer Class, not really a lexer but i use it for lexical analysis"""

    def __init__(self, tokens: list[Token]):
        """
        Args:
            tokens (list[Token]): List of tokens to be lexed
        """
        self.tokens = tokens 
        

    def is_valid(self) -> bool:
        """
        Check if the tokens are valid

        Args:
            None
        Returns:
            bool: True if the tokens are valid, False otherwise
        """
        self.__test_brackets()
        # self.test_keywords()
        # self.test_identifiers()
        # self.test_numbers()
        return True

    def __test_brackets(self):
        """
        Test if the brackets are balanced,
        Tests for the following brackets:
        - ()
        - []
        - {}

        Args:
            None
        Returns:
            None
        """
        stack: list[Token] = []

        opening = {TokenType.LPAREN, TokenType.LBRACKET, TokenType.LBRACE}
        closing = {TokenType.RPAREN, TokenType.RBRACKET, TokenType.RBRACE}

        def __match(op, cl):
            if op == TokenType.LPAREN and cl == TokenType.RPAREN:
                return True
            if op == TokenType.LBRACKET and cl == TokenType.RBRACKET:
                return True
            if op == TokenType.LBRACE and cl == TokenType.RBRACE:
                return True
            return False

        def __reverse(op):
            if op == TokenType.LPAREN:
                return TokenType.RPAREN
            if op == TokenType.LBRACKET:
                return TokenType.RBRACKET
            if op == TokenType.LBRACE:
                return TokenType.RBRACE
            return None

        for token in self.tokens:
            if token.type in opening:
                stack.append(token)
            elif token.type in closing:
                if not stack:
                    self.__print_error(
                        token, message=f"Unmatched closing bracket {token.value}"
                    )
                else:
                    last = stack.pop()
                    if not __match(last.type, token.type):
                        self.__print_error(
                            last,
                            token,
                            f"Expected { __reverse(last.type).value } but got {token.value}",
                        )
                        sys.exit(1)
        if stack:
            for token in stack:
                self.__print_error(
                    token, message=f"Unmatched opening bracket {token.value}"
                )
            sys.exit(1)

    def __get_char_width(self, char: str):
        """
        Get the width of the character

        Args:
            char (str): The character to get the width of
        Returns:
            int: The width of the character
        """
        if char == "\t":
            return 4
        elif char == "\n":
            return 1
        else:
            return 1

    def __print_error(self, SToken: Token, EToken: Token = None, message: str = None):
        """
        Print the error message

        Args:
            Start token (Token)
            End token (Token)
            message (str): The error message
        """
        message = message or "Unhandled error"

        source = SToken.source

        start_pos = SToken.position

        end_pos = EToken.position if EToken else None

        print(
            f"At {source} "
            + (
                f"{end_pos.line}:{end_pos.column}"
                if end_pos
                else f"{start_pos.line}:{start_pos.column}"
            )
        )

        with open(source, "r") as f:
            lines = f.readlines()
            for i in range(
                start_pos.line - 1, end_pos.line if end_pos else start_pos.line
            ):
                line = lines[i]
                print(f"{i + 1}: {line.strip()}")

                if i == start_pos.line - 1:
                    start_col = start_pos.column
                    end_col = (
                        end_pos.column if end_pos else start_col + len(SToken.value)
                    )
                elif i == end_pos.line - 1:
                    print(" " * (end_pos.column - 1) + "^")
        print(f"{message}\n")


if __name__ == "__main__":
    files = [Path(x) for x in sys.argv[1:]]
    if not files:
        print("No files provided")
        sys.exit(1)

    tokenizer = Tokenizer(*files)
    tokens = tokenizer.get_all()

    lexer = Lexer(tokens)
    if not lexer.is_valid():
        print("Invalid tokens")
        sys.exit(1)
