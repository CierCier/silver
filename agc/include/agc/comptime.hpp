#pragma once
#include "agc/ast.hpp"
#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>

namespace agc {

// Value types that can be computed at compile time
struct ComptimeInt {
  int64_t value;
};
struct ComptimeFloat {
  double value;
};
struct ComptimeStr {
  std::string value;
};
struct ComptimeBool {
  bool value;
};
struct ComptimeNull {};

using ComptimeValue = std::variant<ComptimeInt, ComptimeFloat, ComptimeStr,
                                   ComptimeBool, ComptimeNull>;

enum class ComptimeFlow { Normal, Return, Break, Continue };

// Result of compile-time evaluation
struct ComptimeResult {
  std::optional<ComptimeValue> value;
  std::string error; // If evaluation failed
  ComptimeFlow flow{ComptimeFlow::Normal};

  bool ok() const { return value.has_value(); }

  static ComptimeResult success(ComptimeValue v,
                                ComptimeFlow f = ComptimeFlow::Normal) {
    return ComptimeResult{std::move(v), "", f};
  }
  static ComptimeResult fail(const std::string &msg) {
    return ComptimeResult{std::nullopt, msg, ComptimeFlow::Normal};
  }
};

// Compile-time function signature
using ComptimeFunc =
    std::function<ComptimeResult(const std::vector<ComptimeValue> &)>;

// Compile-time evaluator
class ComptimeEvaluator {
public:
  ComptimeEvaluator() = default;

  // Register a user-defined function for comptime execution
  void registerUserFunc(const std::string &name, const DeclFunc *func);

  // Register a builtin/test function
  void registerFunc(const std::string &name, ComptimeFunc func);

  // Evaluate an expression at compile time
  // Returns nullopt if the expression cannot be evaluated at compile time
  ComptimeResult evaluate(const Expr &expr);

  // Traverse the program and evaluate all comptime expressions
  void evaluateProgram(Program &prog);

  // Check if an expression is compile-time evaluable
  bool isComptime(const Expr &expr) const;

  // Set a compile-time constant (global)
  void setConst(const std::string &name, ComptimeValue value);

private:
  std::unordered_map<std::string, ComptimeValue> constants_; // Global constants
  std::unordered_map<std::string, ComptimeFunc> functions_;  // Builtins
  std::unordered_map<std::string, const DeclFunc *>
      userFunctions_; // User defined

  // Stack frames for local variables
  std::vector<std::unordered_map<std::string, ComptimeValue>> scopes_;

  void pushScope();
  void popScope();
  void setVar(const std::string &name, ComptimeValue value);
  std::optional<ComptimeValue> getVar(const std::string &name);

  ComptimeResult evalStmt(const Stmt &stmt);
  ComptimeResult evalBlock(const StmtBlock &block);

  ComptimeResult evalBinary(TokenKind op, const ComptimeValue &lhs,
                            const ComptimeValue &rhs);
  ComptimeResult evalUnary(TokenKind op, const ComptimeValue &operand);
};

// Helper functions to extract values
inline std::optional<int64_t> getInt(const ComptimeValue &v) {
  if (auto *i = std::get_if<ComptimeInt>(&v))
    return i->value;
  return std::nullopt;
}

inline std::optional<double> getFloat(const ComptimeValue &v) {
  if (auto *f = std::get_if<ComptimeFloat>(&v))
    return f->value;
  if (auto *i = std::get_if<ComptimeInt>(&v))
    return static_cast<double>(i->value);
  return std::nullopt;
}

inline std::optional<bool> getBool(const ComptimeValue &v) {
  if (auto *b = std::get_if<ComptimeBool>(&v))
    return b->value;
  if (auto *i = std::get_if<ComptimeInt>(&v))
    return i->value != 0;
  return std::nullopt;
}

inline std::optional<std::string> getStr(const ComptimeValue &v) {
  if (auto *s = std::get_if<ComptimeStr>(&v))
    return s->value;
  return std::nullopt;
}

} // namespace agc
