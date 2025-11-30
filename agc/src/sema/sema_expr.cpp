#include "agc/sema.hpp"

namespace agc {

void SemanticAnalyzer::visit(Expr &expr) {
  std::visit(
      [this, &expr](auto &n) {
        using T = std::decay_t<decltype(n)>;
        if constexpr (std::is_same_v<T, ExprUnary>) {
          visit(*n.rhs);
          expr.type =
              n.rhs->type; // Propagate type (e.g. -x has same type as x)
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
          visit(*n.lhs);
          visit(*n.rhs);
          if (checkType(n.lhs->type, n.rhs->type, expr.loc)) {
            // Result type depends on op
            // Comparison -> bool
            // Arithmetic -> same as operand
            if (n.op == TokenKind::Eq || n.op == TokenKind::Ne ||
                n.op == TokenKind::Lt || n.op == TokenKind::Le ||
                n.op == TokenKind::Gt || n.op == TokenKind::Ge) {
              expr.type = typeCtx_.getBool();
            } else {
              expr.type = n.lhs->type;
            }
          } else {
            expr.type = typeCtx_.getVoid();
          }
        } else if constexpr (std::is_same_v<T, ExprAssign>) {
          visit(*n.rhs);

          Type *lhsType = nullptr;
          if (auto *id = std::get_if<ExprIdent>(&n.lhs->v)) {
            markMutated(id->name, n.lhs->loc);
            lhsType = checkVar(id->name, n.lhs->loc);
            n.lhs->type = lhsType;
          } else if (auto *idx = std::get_if<ExprIndex>(&n.lhs->v)) {
            visit(*n.lhs); // Visit LHS to resolve its type
            lhsType = n.lhs->type;
            // Mark mutated?
            if (auto *baseId = std::get_if<ExprIdent>(&idx->base->v)) {
              markMutated(baseId->name, idx->base->loc);
            }
          } else if (auto *dr = std::get_if<ExprDeref>(&n.lhs->v)) {
            visit(*n.lhs);
            lhsType = n.lhs->type;
          } else if (auto *mem = std::get_if<ExprMember>(&n.lhs->v)) {
            visit(*n.lhs);
            lhsType = n.lhs->type;
            // Mark base mutated?
          }

          checkType(lhsType, n.rhs->type, expr.loc);
          expr.type = lhsType;

        } else if constexpr (std::is_same_v<T, ExprCond>) {
          visit(*n.cond);
          checkType(typeCtx_.getBool(), n.cond->type, n.cond->loc);
          visit(*n.thenE);
          visit(*n.elseE);
          checkType(n.thenE->type, n.elseE->type, expr.loc);
          expr.type = n.thenE->type;
        } else if constexpr (std::is_same_v<T, ExprCall>) {
          for (auto &arg : n.args)
            visit(*arg);

          // Check for generic instantiation
          if (!n.genericArgs.empty()) {
            // It's a generic call like foo<i32>(...)
            // We need to instantiate it.
            if (genericFunctions_.count(n.callee)) {
              std::vector<Type *> typeArgs;
              for (auto &arg : n.genericArgs) {
                typeArgs.push_back(resolveType(arg));
              }
              instantiateFunction(genericFunctions_[n.callee], typeArgs,
                                  expr.loc);

              // Update callee name to mangled name
              std::string mangledName = n.callee;
              for (auto *t : typeArgs)
                mangledName += "_" + t->toString();
              n.callee = mangledName;
            } else {
              diags_.report(DiagLevel::Error, expr.loc,
                            "unknown generic function '" + n.callee + "'");
            }
          } else {
            // Check if we can infer generic args?
            // Not implemented yet.
          }

          // TODO: Check function signature
          // For now, assume i32 return if unknown
          bool isVariadic = false;
          if (functions_.count(n.callee)) {
            auto &info = functions_[n.callee];
            expr.type = info.returnType;
            n.mangledCallee = info.mangledName;
            // Check arg count
            if (n.args.size() != info.paramTypes.size()) {
              // TODO: handle variadic generics
              // isVariadic = info.isVariadic;
            }
            // Check arg types
            for (size_t i = 0; i < n.args.size() && i < info.paramTypes.size();
                 ++i) {
              checkType(info.paramTypes[i], n.args[i]->type, n.args[i]->loc);
            }
          } else {
            diags_.report(DiagLevel::Error, expr.loc,
                          "undefined function '" + n.callee + "'");
            expr.type = typeCtx_.getInt(); // Recovery
          }
        } else if constexpr (std::is_same_v<T, ExprIndex>) {
          visit(*n.base);
          visit(*n.index);
          checkType(typeCtx_.getInt(), n.index->type,
                    n.index->loc); // Index must be int
          if (n.base->type && n.base->type->isArray()) {
            expr.type = static_cast<ArrayType *>(n.base->type)->element();
          } else if (n.base->type && n.base->type->isPointer()) {
            expr.type = static_cast<PointerType *>(n.base->type)->pointee();
          } else {
            diags_.report(DiagLevel::Error, expr.loc,
                          "indexing non-array/pointer");
            expr.type = typeCtx_.getVoid();
          }
        } else if constexpr (std::is_same_v<T, ExprMember>) {
          visit(*n.base);
          Type *baseType = n.base->type;
          if (n.ptr) {
            if (baseType && baseType->isPointer()) {
              baseType = static_cast<PointerType *>(baseType)->pointee();
            } else {
              diags_.report(DiagLevel::Error, expr.loc,
                            "arrow operator on non-pointer");
              baseType = nullptr;
            }
          }

          if (baseType && baseType->isStruct()) {
            auto *st = static_cast<StructType *>(baseType);
            bool found = false;
            for (auto &f : st->fields()) {
              if (f.name == n.member) {
                expr.type = f.type;
                found = true;
                break;
              }
            }
            if (!found) {
              diags_.report(DiagLevel::Error, expr.loc,
                            "struct '" + st->name() + "' has no member '" +
                                n.member + "'");
              expr.type = typeCtx_.getVoid();
            }
          } else if (baseType && baseType->isMeta()) {
            // Accessing static member or enum value
            auto *mt = static_cast<MetaType *>(baseType);
            if (mt->representedType()->isEnum()) {
              auto *et = static_cast<EnumType *>(mt->representedType());
              bool found = false;
              for (auto &item : et->items()) {
                if (item.name == n.member) {
                  // Enum value type is the Enum type itself (or int?)
                  // Usually Enum values are of the Enum type.
                  expr.type = et;
                  // We might want to transform this expression into an ExprInt
                  // or similar but for now let's just type it. Actually,
                  // codegen needs to know the value. We can replace the
                  // ExprMember with ExprInt in the AST? Or just store the value
                  // in Expr? Let's keep it as ExprMember but with type Enum.
                  // Codegen will need to resolve it.
                  found = true;
                  break;
                }
              }
              if (!found) {
                diags_.report(DiagLevel::Error, expr.loc,
                              "enum '" + et->name() + "' has no item '" +
                                  n.member + "'");
                expr.type = typeCtx_.getVoid();
              }
            } else {
              diags_.report(DiagLevel::Error, expr.loc,
                            "type '" + mt->representedType()->toString() +
                                "' has no static members");
              expr.type = typeCtx_.getVoid();
            }
          } else {
            if (baseType)
              diags_.report(DiagLevel::Error, expr.loc,
                            "member access on non-struct/non-enum");
            expr.type = typeCtx_.getVoid();
          }
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
          visit(*n.operand);
          expr.type = typeCtx_.getPointer(n.operand->type);
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
          visit(*n.operand);
          if (n.operand->type && n.operand->type->isPointer()) {
            expr.type = static_cast<PointerType *>(n.operand->type)->pointee();
          } else {
            diags_.report(DiagLevel::Error, expr.loc,
                          "dereference of non-pointer");
            expr.type = typeCtx_.getVoid();
          }
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
          visit(*n.expr);
          expr.type = n.expr->type;
        } else if constexpr (std::is_same_v<T, ExprIdent>) {
          expr.type = checkVar(n.name, expr.loc);
        } else if constexpr (std::is_same_v<T, ExprInt>) {
          expr.type = typeCtx_.getInt();
        } else if constexpr (std::is_same_v<T, ExprFloat>) {
          expr.type = typeCtx_.getFloat();
        } else if (std::holds_alternative<ExprCast>(expr.v)) {
          auto &n = std::get<ExprCast>(expr.v);
          visit(*n.expr);
          Type *target = resolveType(n.target);
          Type *sourceType = n.expr->type;

          // Check for custom cast from struct types
          if (sourceType && sourceType->isStruct()) {
            auto *st = static_cast<StructType *>(sourceType);
            const CastInfo *castInfo = st->findCast(target);
            if (castInfo) {
              // Found a custom cast - store the function name
              n.customCastFunc = castInfo->functionName;
            }
          }

          // TODO: Check if cast is valid (either primitive or custom cast
          // exists)
          expr.type = target;
        } else if constexpr (std::is_same_v<T, ExprStr>) {
          expr.type = typeCtx_.getString();
        } else if constexpr (std::is_same_v<T, ExprInitList>) {
          // Visit all values in the initializer list
          for (auto &v : n.values) {
            if (v.designator)
              visit(**v.designator);
            visit(*v.value);
          }
          // Type will be resolved based on context (e.g., declaration type)
          // For now, leave it as void - the declaration will handle proper type
          // checking
          expr.type = typeCtx_.getVoid();
        }
      },
      expr.v);
}

} // namespace agc
