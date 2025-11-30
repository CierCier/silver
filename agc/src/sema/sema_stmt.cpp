#include "agc/sema.hpp"

namespace agc {

void SemanticAnalyzer::visit(Stmt &stmt) {
  std::visit(
      [this, &stmt](auto &s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
          visit(*s.expr);
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
          if (s.expr) {
            visit(**s.expr);
            // TODO: Check against current function return type
          } else {
            // Check if void
          }
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
          Type *baseType = resolveType(s.type);
          s.resolvedType = baseType;
          for (auto &decl : s.declarators) {
            if (decl.init && *decl.init) {
              // Check if this is an init list - assign type from context
              if (auto *initList =
                      std::get_if<ExprInitList>(&(*decl.init)->v)) {

                uint64_t currentIndex = 0;
                uint64_t maxIndex = 0;
                bool isArray = baseType && baseType->isArray();
                uint64_t arraySize =
                    isArray ? static_cast<ArrayType *>(baseType)->size() : 0;

                for (auto &item : initList->values) {
                  if (item.designator) {
                    if (auto *idx =
                            std::get_if<ExprInt>(&(*item.designator)->v)) {
                      currentIndex = idx->value;
                    } else {
                      diags_.report(DiagLevel::Error, (*item.designator)->loc,
                                    "designator must be an integer constant");
                    }
                  }

                  if (isArray) {
                    if (currentIndex >= arraySize) {
                      diags_.report(DiagLevel::Error, item.value->loc,
                                    "array index " +
                                        std::to_string(currentIndex) +
                                        " is out of bounds (size " +
                                        std::to_string(arraySize) + ")");
                    }
                    // Check element type
                    checkType(static_cast<ArrayType *>(baseType)->element(),
                              item.value->type, item.value->loc);
                    visit(*item.value); // Visit value to resolve its type

                    if (currentIndex > maxIndex)
                      maxIndex = currentIndex;
                    currentIndex++;
                  } else if (baseType && baseType->isStruct()) {
                    // Struct initialization
                    if (item.designator) {
                      diags_.report(DiagLevel::Error, (*item.designator)->loc,
                                    "designated initializers not supported for "
                                    "structs yet");
                    }

                    auto *st = static_cast<StructType *>(baseType);
                    const auto &fields = st->fields();
                    if (currentIndex < fields.size()) {
                      visit(*item.value);
                      checkType(fields[currentIndex].type, item.value->type,
                                item.value->loc);
                      currentIndex++;
                    } else {
                      diags_.report(DiagLevel::Error, item.value->loc,
                                    "too many initializers for struct '" +
                                        st->name() + "'");
                    }
                  } else {
                    if (currentIndex > 0) {
                      diags_.report(DiagLevel::Error, item.value->loc,
                                    "too many initializers for scalar type");
                    }
                    visit(*item.value);
                    checkType(baseType, item.value->type, item.value->loc);
                    currentIndex++;
                  }
                }
                (*decl.init)->type = baseType;
              } else {
                visit(**decl.init);
                checkType(baseType, (*decl.init)->type, decl.loc);
              }
            }
            declareVar(decl.name, decl.loc, &s.isConst, baseType);
          }
        } else if constexpr (std::is_same_v<T, StmtIf>) {
          visit(*s.cond);
          checkType(typeCtx_.getBool(), s.cond->type, s.cond->loc);
          visit(*s.thenBranch);
          if (s.elseBranch)
            visit(**s.elseBranch);
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
          visit(*s.cond);
          checkType(typeCtx_.getBool(), s.cond->type, s.cond->loc);
          visit(*s.body);
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
          pushScope();
          for (auto &st : s.stmts)
            visit(*st);
          popScope();
        } else if constexpr (std::is_same_v<T, StmtFor>) {
          pushScope();
          if (s.init)
            visit(**s.init);
          if (s.cond) {
            visit(**s.cond);
            checkType(typeCtx_.getBool(), (*s.cond)->type, (*s.cond)->loc);
          }
          if (s.iter)
            visit(**s.iter);
          visit(*s.body);
          popScope();
        }
      },
      stmt.v);
}

} // namespace agc
