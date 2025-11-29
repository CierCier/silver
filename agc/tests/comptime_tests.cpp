#include "agc/comptime.hpp"
#include "agc/lexer.hpp"
#include "agc/parser.hpp"
#include <gtest/gtest.h>

using namespace agc;

// Helper to parse an expression from code
static ExprPtr parseExpr(const std::string &code) {
  std::string src = "i32 x = " + code + ";";
  Lexer l(src);
  auto toks = l.lex();
  DiagnosticEngine diags;
  Parser p(toks, diags);
  auto prog = p.parseProgram();
  if (prog.decls.empty())
    return nullptr;
  auto *d = std::get_if<DeclVar>(&prog.decls[0]->v);
  if (!d || d->declarators.empty() || !d->declarators[0].init)
    return nullptr;
  return std::move(*d->declarators[0].init);
}

TEST(ComptimeTest, IntegerLiteral) {
  ComptimeEvaluator eval;
  auto e = parseExpr("42");
  ASSERT_NE(e, nullptr);

  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  auto i = getInt(*result.value);
  ASSERT_TRUE(i.has_value());
  EXPECT_EQ(*i, 42);
}

TEST(ComptimeTest, StringLiteral) {
  ComptimeEvaluator eval;
  auto e = parseExpr("\"hello\"");
  ASSERT_NE(e, nullptr);

  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  auto s = getStr(*result.value);
  ASSERT_TRUE(s.has_value());
  EXPECT_EQ(*s, "hello");
}

TEST(ComptimeTest, BinaryArithmetic) {
  ComptimeEvaluator eval;

  // Addition
  auto e = parseExpr("3 + 4");
  ASSERT_NE(e, nullptr);
  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 7);

  // Subtraction
  e = parseExpr("10 - 3");
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 7);

  // Multiplication
  e = parseExpr("6 * 7");
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 42);

  // Division
  e = parseExpr("20 / 4");
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 5);

  // Modulo
  e = parseExpr("17 % 5");
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 2);
}

TEST(ComptimeTest, ComplexExpression) {
  ComptimeEvaluator eval;

  // (3 + 4) * 2 - 1
  auto e = parseExpr("(3 + 4) * 2 - 1");
  ASSERT_NE(e, nullptr);
  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 13);
}

TEST(ComptimeTest, UnaryOperators) {
  ComptimeEvaluator eval;

  // Negation
  auto e = parseExpr("-42");
  ASSERT_NE(e, nullptr);
  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), -42);

  // Bitwise NOT
  e = parseExpr("~0");
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), -1);
}

TEST(ComptimeTest, ComparisonOperators) {
  ComptimeEvaluator eval;

  auto e = parseExpr("5 < 10");
  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getBool(*result.value), true);

  e = parseExpr("5 > 10");
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getBool(*result.value), false);

  e = parseExpr("5 == 5");
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getBool(*result.value), true);
}

TEST(ComptimeTest, BitwiseOperators) {
  ComptimeEvaluator eval;

  auto e = parseExpr("5 & 3"); // 101 & 011 = 001
  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 1);

  e = parseExpr("5 | 3"); // 101 | 011 = 111
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 7);

  e = parseExpr("5 ^ 3"); // 101 ^ 011 = 110
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 6);

  e = parseExpr("1 << 3"); // 1 << 3 = 8
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 8);
}

TEST(ComptimeTest, Constants) {
  ComptimeEvaluator eval;
  eval.setConst("PI_INT", ComptimeInt{3});
  eval.setConst("TWO", ComptimeInt{2});

  auto e = parseExpr("PI_INT * TWO");
  ASSERT_NE(e, nullptr);
  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 6);
}

TEST(ComptimeTest, TernaryOperator) {
  ComptimeEvaluator eval;

  auto e = parseExpr("1 ? 42 : 0");
  ASSERT_NE(e, nullptr);
  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 42);

  e = parseExpr("0 ? 42 : 99");
  result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 99);
}

TEST(ComptimeTest, ComptimeFunction) {
  ComptimeEvaluator eval;

  // Register a simple factorial function
  eval.registerFunc(
      "factorial",
      [](const std::vector<ComptimeValue> &args) -> ComptimeResult {
        if (args.size() != 1)
          return ComptimeResult::fail("factorial takes 1 argument");
        auto n = getInt(args[0]);
        if (!n)
          return ComptimeResult::fail("factorial requires integer argument");
        int64_t result = 1;
        for (int64_t i = 2; i <= *n; ++i) {
          result *= i;
        }
        return ComptimeResult::success(ComptimeInt{result});
      });

  auto e = parseExpr("factorial(5)");
  ASSERT_NE(e, nullptr);
  auto result = eval.evaluate(*e);
  ASSERT_TRUE(result.ok());
  EXPECT_EQ(*getInt(*result.value), 120);
}

TEST(ComptimeTest, DivisionByZero) {
  ComptimeEvaluator eval;

  auto e = parseExpr("10 / 0");
  ASSERT_NE(e, nullptr);
  auto result = eval.evaluate(*e);
  ASSERT_FALSE(result.ok());
  EXPECT_EQ(result.error, "division by zero");
}

TEST(ComptimeTest, IsComptime) {
  ComptimeEvaluator eval;
  eval.setConst("CONST", ComptimeInt{42});

  // Literals are comptime
  auto e = parseExpr("42");
  EXPECT_TRUE(eval.isComptime(*e));

  // Constants are comptime
  e = parseExpr("CONST");
  EXPECT_TRUE(eval.isComptime(*e));

  // Unknown identifiers are not comptime
  e = parseExpr("unknown_var");
  EXPECT_FALSE(eval.isComptime(*e));

  // Binary ops on comptime values are comptime
  e = parseExpr("CONST + 1");
  EXPECT_TRUE(eval.isComptime(*e));
}
