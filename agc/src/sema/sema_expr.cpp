#include "agc/sema.hpp"
#include "agc/overloaded.hpp"

namespace agc {

void SemanticAnalyzer::visit(Expr &expr) {
  std::visit(
      overloaded{
          [this, &expr](ExprUnary &n) {
            visit(*n.rhs);
            expr.type = n.rhs->type;
          },
          [this, &expr](ExprBinary &n) {
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
          },
          [this, &expr](ExprAssign &n) {
            visit(*n.rhs);

            Type *lhsType = nullptr;
            if (auto *id = std::get_if<ExprIdent>(&n.lhs->v)) {
              markMutated(id->name, n.lhs->loc);
              lhsType = checkVar(id->name, n.lhs->loc);
              n.lhs->type = lhsType;
            } else if (auto *idx = std::get_if<ExprIndex>(&n.lhs->v)) {
              visit(*n.lhs);
              lhsType = n.lhs->type;
              if (auto *baseId = std::get_if<ExprIdent>(&idx->base->v)) {
                markMutated(baseId->name, idx->base->loc);
              }
            } else if (std::holds_alternative<ExprDeref>(n.lhs->v)) {
              visit(*n.lhs);
              lhsType = n.lhs->type;
            } else if (std::holds_alternative<ExprMember>(n.lhs->v)) {
              visit(*n.lhs);
              lhsType = n.lhs->type;
            }

            checkType(lhsType, n.rhs->type, expr.loc);
            expr.type = lhsType;
          },
          [this, &expr](ExprCond &n) {
            visit(*n.cond);
            checkType(typeCtx_.getBool(), n.cond->type, n.cond->loc);
            visit(*n.thenE);
            visit(*n.elseE);
            checkType(n.thenE->type, n.elseE->type, expr.loc);
            expr.type = n.thenE->type;
          },
          [this, &expr](ExprCall &n) {
            for (auto &arg : n.args)
              visit(*arg);

            // Check for generic instantiation
            if (!n.genericArgs.empty()) {
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
            }

            // Check function signature
            if (functions_.count(n.callee)) {
              auto &info = functions_[n.callee];
              expr.type = info.returnType;
              n.mangledCallee = info.mangledName;
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
          },
          [this, &expr](ExprIndex &n) {
            visit(*n.base);
            visit(*n.index);
            checkType(typeCtx_.getInt(), n.index->type, n.index->loc);
            if (n.base->type && n.base->type->isArray()) {
              expr.type = static_cast<ArrayType *>(n.base->type)->element();
            } else if (n.base->type && n.base->type->isPointer()) {
              expr.type = static_cast<PointerType *>(n.base->type)->pointee();
            } else {
              diags_.report(DiagLevel::Error, expr.loc,
                            "indexing non-array/pointer");
              expr.type = typeCtx_.getVoid();
            }
          },
          [this, &expr](ExprMember &n) {
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
              auto *mt = static_cast<MetaType *>(baseType);
              if (mt->representedType()->isEnum()) {
                auto *et = static_cast<EnumType *>(mt->representedType());
                bool found = false;
                for (auto &item : et->items()) {
                  if (item.name == n.member) {
                    expr.type = et;
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
          },
          [this, &expr](ExprAddressOf &n) {
            visit(*n.operand);
            expr.type = typeCtx_.getPointer(n.operand->type);
          },
          [this, &expr](ExprDeref &n) {
            visit(*n.operand);
            if (n.operand->type && n.operand->type->isPointer()) {
              expr.type = static_cast<PointerType *>(n.operand->type)->pointee();
            } else {
              diags_.report(DiagLevel::Error, expr.loc,
                            "dereference of non-pointer");
              expr.type = typeCtx_.getVoid();
            }
          },
          [this, &expr](ExprComptime &n) {
            visit(*n.expr);
            expr.type = n.expr->type;
          },
          [this, &expr](ExprIdent &n) {
            expr.type = checkVar(n.name, expr.loc);
          },
          [this, &expr](ExprInt &) { expr.type = typeCtx_.getInt(); },
          [this, &expr](ExprFloat &) { expr.type = typeCtx_.getFloat(); },
          [this, &expr](ExprCast &n) {
            visit(*n.expr);
            Type *target = resolveType(n.target);
            Type *sourceType = n.expr->type;

            // Check for custom cast from struct types
            if (sourceType && sourceType->isStruct()) {
              auto *st = static_cast<StructType *>(sourceType);
              const CastInfo *castInfo = st->findCast(target);
              if (castInfo) {
                n.customCastFunc = castInfo->functionName;
              }
            }

            expr.type = target;
          },
          [this, &expr](ExprStr &) { expr.type = typeCtx_.getString(); },
          [this, &expr](ExprInitList &n) {
            for (auto &v : n.values) {
              if (v.designator)
                visit(**v.designator);
              visit(*v.value);
            }
            expr.type = typeCtx_.getVoid();
          },
      },
      expr.v);
}

} // namespace agc