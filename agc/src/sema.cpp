#include "agc/sema.hpp"

namespace agc {

void SemanticAnalyzer::analyze(Program &prog) {
  pushScope(); // Global scope

  // Pass 0: Register Structs and Enums (names only first for structs)
  for (auto &d : prog.decls) {
    if (auto *ds = std::get_if<DeclStruct>(&d->v)) {
      if (structTypes_.count(ds->name)) {
        diags_.report(DiagLevel::Error, d->loc,
                      "redefinition of struct '" + ds->name + "'");
      } else {
        structTypes_[ds->name] = typeCtx_.getOpaqueStruct(ds->name);
      }
    } else if (auto *de = std::get_if<DeclEnum>(&d->v)) {
      if (enumTypes_.count(de->name)) {
        diags_.report(DiagLevel::Error, d->loc,
                      "redefinition of enum '" + de->name + "'");
      } else {
        std::vector<TypeEnumItem> items;
        uint64_t val = 0;
        for (auto &item : de->items) {
          if (item.value)
            val = *item.value;
          items.push_back({item.name, val++});
        }
        enumTypes_[de->name] = typeCtx_.getEnum(de->name, std::move(items));
      }
    }
  }

  // Pass 0.5: Resolve Struct fields
  for (auto &d : prog.decls) {
    if (auto *ds = std::get_if<DeclStruct>(&d->v)) {
      auto *st = dynamic_cast<StructType *>(structTypes_[ds->name]);
      if (st) {
        std::vector<Field> fields;
        for (auto &f : ds->fields) {
          Type *ft = resolveType(f.type);
          for (auto &name : f.names) {
            fields.push_back({name, ft});
          }
        }
        st->setFields(std::move(fields));
      }
    }
  }

  // Pass 1: Register globals
  for (auto &d : prog.decls) {
    if (auto *dv = std::get_if<DeclVar>(&d->v)) {
      Type *t = resolveType(dv->type);
      for (auto &decl : dv->declarators) {
        declareVar(decl.name, decl.loc, &dv->isConst, t);
        if (decl.init && *decl.init) {
          visit(**decl.init);
          checkType(t, (*decl.init)->type, decl.loc);
        }
      }
    } else if (auto *df = std::get_if<DeclFunc>(&d->v)) {
      // Register function (TODO: Function overloading? For now just name)
      // We don't have a separate function table, but we can treat them as vars
      // in global scope? Or just rely on them being found. For now, let's just
      // visit them in Pass 2.
    } else if (auto *di = std::get_if<DeclImpl>(&d->v)) {
      // Register impl methods/casts
      // For now, just visit them to check bodies
      // We need to associate them with the type.
    }
  }

  // Pass 2: Visit functions
  for (auto &d : prog.decls) {
    visit(*d);
  }

  popScope();
}

void SemanticAnalyzer::pushScope() { scopes_.emplace_back(); }

void SemanticAnalyzer::popScope() {
  if (scopes_.empty())
    return;

  // Before popping, check for non-mutated variables and mark them const
  for (auto &pair : scopes_.back()) {
    if (!pair.second.isMutated && pair.second.isConstFlag) {
      *pair.second.isConstFlag = true;
    }
  }
  scopes_.pop_back();
}

Type *SemanticAnalyzer::checkVar(const std::string &name, const DiagLoc &loc) {
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    auto vit = it->find(name);
    if (vit != it->end())
      return vit->second.type;
  }

  // Check if it is a type name (Struct or Enum)
  if (structTypes_.count(name)) {
    return typeCtx_.getMeta(structTypes_[name]);
  }
  if (enumTypes_.count(name)) {
    return typeCtx_.getMeta(enumTypes_[name]);
  }

  diags_.report(DiagLevel::Error, loc,
                "undefined variable or type '" + name + "'");
  return typeCtx_.getVoid(); // Error recovery
}

void SemanticAnalyzer::declareVar(const std::string &name, const DiagLoc &loc,
                                  bool *isConstFlag, Type *type) {
  if (scopes_.empty())
    return;
  if (scopes_.back().count(name)) {
    diags_.report(DiagLevel::Error, loc, "redefinition of '" + name + "'");
    return;
  }
  scopes_.back()[name] = VarInfo{false, isConstFlag, type};
  if (isConstFlag)
    *isConstFlag = false;
}

void SemanticAnalyzer::markMutated(const std::string &name,
                                   const DiagLoc &loc) {
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    auto vit = it->find(name);
    if (vit != it->end()) {
      vit->second.isMutated = true;
      return;
    }
  }
  diags_.report(DiagLevel::Error, loc, "undefined variable '" + name + "'");
}

Type *SemanticAnalyzer::resolveType(const TypeName &typeName) {
  Type *base = nullptr;
  if (typeName.name == "void")
    base = typeCtx_.getVoid();
  else if (typeName.name == "bool")
    base = typeCtx_.getBool();
  else if (typeName.name == "i32")
    base = typeCtx_.getInt();
  else if (typeName.name == "f64")
    base = typeCtx_.getFloat();
  else if (typeName.name == "str")
    base = typeCtx_.getString();
  else if (structTypes_.count(typeName.name))
    base = structTypes_[typeName.name];
  else if (enumTypes_.count(typeName.name))
    base = enumTypes_[typeName.name];
  else {
    // Check if it's a generic type or something else?
    // For now, error.
    // Actually, we might want to support "i64", "i8" etc later.
    // For now, map all ints to i32? No, let's stick to strict.
    diags_.report(DiagLevel::Error, {}, "unknown type '" + typeName.name + "'");
    return typeCtx_.getVoid();
  }

  // Apply pointers
  for (unsigned i = 0; i < typeName.pointerDepth; ++i) {
    base = typeCtx_.getPointer(base);
  }

  // Apply arrays (reverse order?)
  // TypeName: int x[10][20] -> arrayDims = {10, 20}
  // Type: Array(Array(int, 20), 10)
  for (auto it = typeName.arrayDims.rbegin(); it != typeName.arrayDims.rend();
       ++it) {
    uint64_t size = 0;
    if (*it)
      size = **it;
    base = typeCtx_.getArray(base, size);
  }

  return base;
}

bool SemanticAnalyzer::checkType(Type *expected, Type *actual,
                                 const DiagLoc &loc) {
  if (!expected || !actual)
    return false;
  if (expected->equals(actual))
    return true;

  // Implicit conversions?
  // void* can accept any pointer?
  // For now, strict.

  diags_.report(DiagLevel::Error, loc,
                "type mismatch: expected '" + expected->toString() +
                    "', got '" + actual->toString() + "'");
  return false;
}

void SemanticAnalyzer::visit(Decl &decl) {
  if (auto *df = std::get_if<DeclFunc>(&decl.v)) {
    // Function already registered? No, we didn't register functions in pass 1
    // fully. Let's register params and body.
    Type *retType = resolveType(df->ret);

    if (df->body) {
      pushScope();
      for (auto &p : df->params) {
        Type *pt = resolveType(p.type);
        declareVar(p.name, decl.loc, nullptr, pt);
        // Params are mutated?
        if (!scopes_.empty())
          scopes_.back()[p.name].isMutated = true;
      }

      for (auto &s : df->body->stmts) {
        visit(*s);
      }

      // Check return type?
      // We need to check returns in body match retType.
      // This requires passing expected return type to visit(Stmt).
      // For now, let's skip explicit return check here, rely on StmtReturn
      // visit. But StmtReturn needs to know expected type. We can store
      // "current function return type" in a member.

      popScope();
    }
  } else if (auto *dv = std::get_if<DeclVar>(&decl.v)) {
    // Already handled in Pass 1 for globals.
    // But if this is called for local decls (via StmtDecl), we need logic.
    // Wait, StmtDecl is a Stmt. DeclVar is a top-level Decl.
    // So this visit(Decl) is only for top-level.
    // Pass 1 handled DeclVar.
  } else if (auto *di = std::get_if<DeclImpl>(&decl.v)) {
    Type *type = resolveType(di->type);
    StructType *structType = nullptr;
    if (type && type->isStruct()) {
      structType = static_cast<StructType *>(type);
    }

    for (auto &m : di->methods) {
      // Register casts with the struct type
      if (auto *dc = std::get_if<DeclCast>(&m->v)) {
        if (structType && !dc->params.empty()) {
          Type *targetType = resolveType(dc->target);
          // Generate mangled name: StructName_cast_TargetType
          std::string mangledName = di->type.name + "_cast_" + dc->target.name;
          structType->addCast(
              CastInfo{targetType, mangledName, dc->isImplicit});
        }
      } else if (auto *df = std::get_if<DeclFunc>(&m->v)) {
        if (structType) {
          Type *retType = resolveType(df->ret);
          std::vector<Type *> paramTypes;
          for (auto &p : df->params) {
            paramTypes.push_back(resolveType(p.type));
          }
          std::string mangledName = di->type.name + "_" + df->name;
          structType->addMethod(
              MethodInfo{df->name, mangledName, retType, paramTypes});
        }
      }
      visit(*m);
    }
  } else if (auto *dc = std::get_if<DeclCast>(&decl.v)) {
    // Similar to DeclFunc
    Type *target = resolveType(dc->target);
    if (dc->body) {
      pushScope();
      for (auto &p : dc->params) {
        Type *pt = resolveType(p.type);
        declareVar(p.name, decl.loc, nullptr, pt);
      }
      for (auto &s : dc->body->stmts) {
        visit(*s);
      }
      popScope();
    }
  }
}

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
          for (auto &decl : s.declarators) {
            if (decl.init && *decl.init) {
              // Check if this is an init list - assign type from context
              if (auto *initList =
                      std::get_if<ExprInitList>(&(*decl.init)->v)) {
                // Visit each value in the init list
                for (auto &v : initList->values) {
                  visit(*v);
                }
                // Set the init list type to the target type
                (*decl.init)->type = baseType;

                // For struct types, check field types match
                if (baseType && baseType->isStruct()) {
                  auto *st = static_cast<StructType *>(baseType);
                  const auto &fields = st->fields();
                  for (size_t i = 0;
                       i < initList->values.size() && i < fields.size(); ++i) {
                    checkType(fields[i].type, initList->values[i]->type,
                              initList->values[i]->loc);
                  }
                  if (initList->values.size() > fields.size()) {
                    diags_.report(DiagLevel::Error, (*decl.init)->loc,
                                  "too many initializers for struct '" +
                                      st->name() + "'");
                  }
                }
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

void SemanticAnalyzer::visit(Expr &expr) {
  std::visit(
      [this, &expr](auto &n) {
        using T = std::decay_t<decltype(n)>;
        if constexpr (std::is_same_v<T, ExprUnary>) {
          visit(*n.rhs);
          expr.type =
              n.rhs->type; // Propagate type (e.g. -x has same type as x)
          // Check op compatibility
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
          visit(*n.lhs);
          visit(*n.rhs);
          // Check types match
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
          // Check LHS
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
          // TODO: Check function signature
          // For now, assume i32 return if unknown
          expr.type = typeCtx_.getInt();
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
            visit(*v);
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
