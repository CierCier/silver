#include <gtest/gtest.h>
#include "agc/codegen.hpp"
#include "agc/parser.hpp"
#include "agc/lexer.hpp"
#include <sstream>

using namespace agc;

static std::string generateCSharp(const std::string& code) {
    Lexer lexer(code);
    auto tokens = lexer.lex();
    Parser parser(tokens);
    auto prog = parser.parseProgram();
    
    auto backend = create_backend("csharp");
    std::stringstream ss;
    std::string err;
    CodegenOptions opts;
    bool ok = backend->generate(prog, ss, err, opts);
    EXPECT_TRUE(ok) << "Codegen failed: " << err;
    return ss.str();
}

TEST(CodegenTest, CSharpBasic) {
    std::string src = "i32 main() { return 0; }";
    std::string out = generateCSharp(src);
    EXPECT_NE(out.find("public static class Program"), std::string::npos);
    EXPECT_NE(out.find("public static int main()"), std::string::npos);
}

TEST(CodegenTest, CSharpExtern) {
    std::string src = "extern i32 puts(str s);";
    std::string out = generateCSharp(src);
    EXPECT_NE(out.find("[System.Runtime.InteropServices.DllImport(\"__Internal\")]"), std::string::npos);
    EXPECT_NE(out.find("public static extern int puts(string s);"), std::string::npos);
}

TEST(CodegenTest, CSharpOps) {
    std::string src = "void f() { i32 x; ++x; }";
    std::string out = generateCSharp(src);
    EXPECT_NE(out.find("++x;"), std::string::npos);
}
