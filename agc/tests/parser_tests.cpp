#include "agc/diagnostics.hpp"
#include "agc/lexer.hpp"
#include "agc/parser.hpp"
#include <gtest/gtest.h>

using namespace agc;

// Helper to parse an expression
static ExprPtr parseExpr(const std::string &code) {
  Lexer lexer(code);
  auto tokens = lexer.lex();
  DiagnosticEngine diag = DiagnosticEngine();

  Parser parser(tokens, diag);

  std::string src = "i32 x = " + code + ";";
  Lexer l(src);
  auto toks = l.lex();
  Parser p(toks, diag);
  auto prog = p.parseProgram();
  if (prog.decls.empty())
    return nullptr;
  auto *d = std::get_if<DeclVar>(&prog.decls[0]->v);
  if (!d || d->declarators.empty() || !d->declarators[0].init)
    return nullptr;
  return std::move(*d->declarators[0].init);
}

TEST(ParserTest, PrefixOperators) {
  auto e = parseExpr("++x");
  ASSERT_NE(e, nullptr);
  auto *u = std::get_if<ExprUnary>(&e->v);
  ASSERT_NE(u, nullptr);
  EXPECT_EQ(u->op, TokenKind::PlusPlus);

  e = parseExpr("--x");
  ASSERT_NE(e, nullptr);
  u = std::get_if<ExprUnary>(&e->v);
  ASSERT_NE(u, nullptr);
  EXPECT_EQ(u->op, TokenKind::MinusMinus);

  e = parseExpr("~x");
  ASSERT_NE(e, nullptr);
  u = std::get_if<ExprUnary>(&e->v);
  ASSERT_NE(u, nullptr);
  EXPECT_EQ(u->op, TokenKind::Tilde);

  e = parseExpr("+x");
  ASSERT_NE(e, nullptr);
  u = std::get_if<ExprUnary>(&e->v);
  ASSERT_NE(u, nullptr);
  EXPECT_EQ(u->op, TokenKind::Plus);
}

TEST(ParserTest, ComptimeModifier) {
  // "comptime expr" should be parsed as ExprComptime wrapping the inner expr
  auto e = parseExpr("comptime x");
  ASSERT_NE(e, nullptr);
  // Should result in ExprComptime containing ExprIdent("x")
  auto *ct = std::get_if<ExprComptime>(&e->v);
  ASSERT_NE(ct, nullptr);
  auto *id = std::get_if<ExprIdent>(&ct->expr->v);
  ASSERT_NE(id, nullptr);
  EXPECT_EQ(id->name, "x");
}

// Helper to parse a program
static Program parseProgram(const std::string &code) {
  Lexer l(code);
  auto toks = l.lex();
  auto diag = DiagnosticEngine();
  Parser p(toks, diag);
  return p.parseProgram();
}

TEST(ParserTest, IfStatement) {
  auto prog = parseProgram(R"(
        i32 foo() {
            if (x > 0) {
                return 1;
            }
        }
    )");
  ASSERT_EQ(prog.decls.size(), 1);
  auto *f = std::get_if<DeclFunc>(&prog.decls[0]->v);
  ASSERT_NE(f, nullptr);
  ASSERT_TRUE(f->body.has_value());
  ASSERT_EQ(f->body->stmts.size(), 1);
  auto *ifStmt = std::get_if<StmtIf>(&f->body->stmts[0]->v);
  ASSERT_NE(ifStmt, nullptr);
  EXPECT_NE(ifStmt->cond, nullptr);
  EXPECT_NE(ifStmt->thenBranch, nullptr);
  EXPECT_FALSE(ifStmt->elseBranch.has_value());
}

TEST(ParserTest, IfElseStatement) {
  auto prog = parseProgram(R"(
        i32 foo() {
            if (x > 0) {
                return 1;
            } else {
                return 0;
            }
        }
    )");
  ASSERT_EQ(prog.decls.size(), 1);
  auto *f = std::get_if<DeclFunc>(&prog.decls[0]->v);
  ASSERT_NE(f, nullptr);
  ASSERT_TRUE(f->body.has_value());
  ASSERT_EQ(f->body->stmts.size(), 1);
  auto *ifStmt = std::get_if<StmtIf>(&f->body->stmts[0]->v);
  ASSERT_NE(ifStmt, nullptr);
  EXPECT_TRUE(ifStmt->elseBranch.has_value());
}

TEST(ParserTest, WhileStatement) {
  auto prog = parseProgram(R"(
        i32 foo() {
            while (x < 10) {
                x = x + 1;
            }
        }
    )");
  ASSERT_EQ(prog.decls.size(), 1);
  auto *f = std::get_if<DeclFunc>(&prog.decls[0]->v);
  ASSERT_NE(f, nullptr);
  ASSERT_TRUE(f->body.has_value());
  ASSERT_EQ(f->body->stmts.size(), 1);
  auto *whileStmt = std::get_if<StmtWhile>(&f->body->stmts[0]->v);
  ASSERT_NE(whileStmt, nullptr);
  EXPECT_NE(whileStmt->cond, nullptr);
  EXPECT_NE(whileStmt->body, nullptr);
}

TEST(ParserTest, BreakContinue) {
  auto prog = parseProgram(R"(
        i32 foo() {
            while (1) {
                if (x > 10) {
                    break;
                }
                if (x % 2 == 0) {
                    continue;
                }
            }
        }
    )");
  ASSERT_EQ(prog.decls.size(), 1);
  auto *f = std::get_if<DeclFunc>(&prog.decls[0]->v);
  ASSERT_NE(f, nullptr);
  ASSERT_TRUE(f->body.has_value());
  auto *whileStmt = std::get_if<StmtWhile>(&f->body->stmts[0]->v);
  ASSERT_NE(whileStmt, nullptr);
  // Check the body has if statements with break/continue
  auto *block = std::get_if<StmtBlock>(&whileStmt->body->v);
  ASSERT_NE(block, nullptr);
  ASSERT_EQ(block->stmts.size(), 2);
  // First if contains break
  auto *if1 = std::get_if<StmtIf>(&block->stmts[0]->v);
  ASSERT_NE(if1, nullptr);
  auto *thenBlock1 = std::get_if<StmtBlock>(&if1->thenBranch->v);
  ASSERT_NE(thenBlock1, nullptr);
  auto *breakStmt = std::get_if<StmtBreak>(&thenBlock1->stmts[0]->v);
  EXPECT_NE(breakStmt, nullptr);
  // Second if contains continue
  auto *if2 = std::get_if<StmtIf>(&block->stmts[1]->v);
  ASSERT_NE(if2, nullptr);
  auto *thenBlock2 = std::get_if<StmtBlock>(&if2->thenBranch->v);
  ASSERT_NE(thenBlock2, nullptr);
  auto *contStmt = std::get_if<StmtContinue>(&thenBlock2->stmts[0]->v);
  EXPECT_NE(contStmt, nullptr);
}

TEST(ParserTest, ConstVariable) {
  auto prog = parseProgram(R"(
        i32 foo() {
            const i32 x = 42;
            i32 y = x;
        }
    )");
  ASSERT_EQ(prog.decls.size(), 1);
  auto *f = std::get_if<DeclFunc>(&prog.decls[0]->v);
  ASSERT_NE(f, nullptr);
  ASSERT_TRUE(f->body.has_value());
  ASSERT_EQ(f->body->stmts.size(), 2);
  // First statement should be const declaration
  auto *constDecl = std::get_if<StmtDecl>(&f->body->stmts[0]->v);
  ASSERT_NE(constDecl, nullptr);
  EXPECT_TRUE(constDecl->isConst);
  EXPECT_EQ(constDecl->type.name, "i32");
  // Second statement should be non-const declaration
  auto *varDecl = std::get_if<StmtDecl>(&f->body->stmts[1]->v);
  ASSERT_NE(varDecl, nullptr);
  EXPECT_FALSE(varDecl->isConst);
}

TEST(ParserTest, NewExpression) {
  auto e = parseExpr("new<Point>()");
  ASSERT_NE(e, nullptr);
  auto *newExpr = std::get_if<ExprNew>(&e->v);
  ASSERT_NE(newExpr, nullptr);
  EXPECT_EQ(newExpr->targetType.name, "Point");
}

TEST(ParserTest, DropExpression) {
  auto e = parseExpr("drop(x)");
  ASSERT_NE(e, nullptr);
  auto *dropExpr = std::get_if<ExprDrop>(&e->v);
  ASSERT_NE(dropExpr, nullptr);
  auto *id = std::get_if<ExprIdent>(&dropExpr->operand->v);
  ASSERT_NE(id, nullptr);
  EXPECT_EQ(id->name, "x");
}

TEST(ParserTest, DropMethodCall) {
  // Test that .drop() method calls work
  auto prog = parseProgram(R"(
        void foo() {
            r.drop();
        }
    )");
  ASSERT_EQ(prog.decls.size(), 1);
  auto *f = std::get_if<DeclFunc>(&prog.decls[0]->v);
  ASSERT_NE(f, nullptr);
  ASSERT_TRUE(f->body.has_value());
  ASSERT_EQ(f->body->stmts.size(), 1);
  auto *exprStmt = std::get_if<StmtExpr>(&f->body->stmts[0]->v);
  ASSERT_NE(exprStmt, nullptr);
  auto *mc = std::get_if<ExprMethodCall>(&exprStmt->expr->v);
  ASSERT_NE(mc, nullptr);
  EXPECT_EQ(mc->method, "drop");
}

TEST(ParserTest, AllocSingleExpression) {
  auto e = parseExpr("alloc<i32>()");
  ASSERT_NE(e, nullptr);
  auto *allocExpr = std::get_if<ExprAlloc>(&e->v);
  ASSERT_NE(allocExpr, nullptr);
  EXPECT_EQ(allocExpr->targetType.name, "i32");
  EXPECT_FALSE(allocExpr->count.has_value());
}

TEST(ParserTest, AllocArrayExpression) {
  auto e = parseExpr("alloc<Point>(10)");
  ASSERT_NE(e, nullptr);
  auto *allocExpr = std::get_if<ExprAlloc>(&e->v);
  ASSERT_NE(allocExpr, nullptr);
  EXPECT_EQ(allocExpr->targetType.name, "Point");
  ASSERT_TRUE(allocExpr->count.has_value());
  auto *countExpr = std::get_if<ExprInt>(&(*allocExpr->count)->v);
  ASSERT_NE(countExpr, nullptr);
  EXPECT_EQ(countExpr->value, 10);
}

TEST(ParserTest, FreeExpression) {
  auto e = parseExpr("free(ptr)");
  ASSERT_NE(e, nullptr);
  auto *freeExpr = std::get_if<ExprFree>(&e->v);
  ASSERT_NE(freeExpr, nullptr);
  auto *id = std::get_if<ExprIdent>(&freeExpr->operand->v);
  ASSERT_NE(id, nullptr);
  EXPECT_EQ(id->name, "ptr");
}

TEST(ParserTest, CharLiteralASCII) {
  auto e = parseExpr("'A'");
  ASSERT_NE(e, nullptr);
  auto *charExpr = std::get_if<ExprChar>(&e->v);
  ASSERT_NE(charExpr, nullptr);
  EXPECT_EQ(charExpr->codepoint, 65); // ASCII 'A'
}

TEST(ParserTest, CharLiteralEscape) {
  auto e = parseExpr("'\\n'");
  ASSERT_NE(e, nullptr);
  auto *charExpr = std::get_if<ExprChar>(&e->v);
  ASSERT_NE(charExpr, nullptr);
  EXPECT_EQ(charExpr->codepoint, 10); // newline

  e = parseExpr("'\\t'");
  ASSERT_NE(e, nullptr);
  charExpr = std::get_if<ExprChar>(&e->v);
  ASSERT_NE(charExpr, nullptr);
  EXPECT_EQ(charExpr->codepoint, 9); // tab

  e = parseExpr("'\\''");
  ASSERT_NE(e, nullptr);
  charExpr = std::get_if<ExprChar>(&e->v);
  ASSERT_NE(charExpr, nullptr);
  EXPECT_EQ(charExpr->codepoint, 39); // single quote
}

TEST(ParserTest, CharLiteralUnicode) {
  auto e = parseExpr("'\\u0041'"); // Unicode for 'A'
  ASSERT_NE(e, nullptr);
  auto *charExpr = std::get_if<ExprChar>(&e->v);
  ASSERT_NE(charExpr, nullptr);
  EXPECT_EQ(charExpr->codepoint, 0x0041);

  e = parseExpr("'\\u263A'"); // Smiley face ☺
  ASSERT_NE(e, nullptr);
  charExpr = std::get_if<ExprChar>(&e->v);
  ASSERT_NE(charExpr, nullptr);
  EXPECT_EQ(charExpr->codepoint, 0x263A);
}

TEST(ParserTest, CharTypeDeclaration) {
  auto prog = parseProgram(R"(
        i32 foo() {
            char c = 'x';
            return 0;
        }
    )");
  ASSERT_EQ(prog.decls.size(), 1);
  auto *f = std::get_if<DeclFunc>(&prog.decls[0]->v);
  ASSERT_NE(f, nullptr);
  ASSERT_TRUE(f->body.has_value());
  ASSERT_GE(f->body->stmts.size(), 1);
  auto *declStmt = std::get_if<StmtDecl>(&f->body->stmts[0]->v);
  ASSERT_NE(declStmt, nullptr);
  EXPECT_EQ(declStmt->type.name, "char");
}
