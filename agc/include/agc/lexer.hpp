#pragma once
#include <string>
#include <string_view>
#include <vector>
#include "agc/token.hpp"

namespace agc {

class Lexer {
public:
    explicit Lexer(std::string_view input, std::string filename = {})
        : input_(input), filename_(std::move(filename)) {
            loc_.file = filename_;
            loc_.line = 1;
            loc_.col = 1;
        }

    // Produce a full token stream (appends End token at end)
    std::vector<Token> lex();

private:
    char peek(size_t off = 0) const { return pos_ + off < input_.size() ? input_[pos_ + off] : '\0'; }
    char get();
    void newline();

    void skipSpacesAndComments();
    Token lexIdentifierOrKeyword();
    Token lexNumber();
    Token lexString();
    Token make(TokenKind k, std::string s); 

    bool match(char c);

    std::string_view input_;
    std::string filename_;
    size_t pos_{0};
    SourceLoc loc_{};
};

} // namespace agc
