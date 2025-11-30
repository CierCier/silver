#include "agc/sema.hpp"

namespace agc {

void SemanticAnalyzer::instantiateStruct(DeclStruct *ds,
                                         const std::vector<Type *> &args) {
  if (args.size() != ds->genericParams.size()) {
    diags_.report(DiagLevel::Error, {},
                  "generic arg count mismatch for '" + ds->name + "'");
    return;
  }

  std::string mangledName = ds->name;
  for (const auto &arg : args) {
    mangledName += "_" + arg->toString(); // Simple mangling
  }

  // Check cache
  if (structTypes_.count(mangledName)) {
    return;
  }

  // Create new struct type
  StructType *st =
      static_cast<StructType *>(typeCtx_.getOpaqueStruct(mangledName));
  structTypes_[mangledName] = st; // Register before body to handle recursion

  // Instantiate body
  pushTypeScope();
  for (size_t i = 0; i < ds->genericParams.size(); ++i) {
    declareTypeAlias(ds->genericParams[i], args[i]);
  }

  std::vector<Field> fields;
  for (auto &f : ds->fields) {
    Type *ft = resolveType(f.type);
    for (auto &name : f.names) {
      fields.push_back({name, ft});
    }
  }
  st->setFields(std::move(fields));

  popTypeScope();
}

// Helper to convert Type* back to TypeName for substitution
TypeName fromType(Type *t) {
  TypeName tn;
  while (t->isPointer()) {
    tn.pointerDepth++;
    t = static_cast<PointerType *>(t)->pointee();
  }
  while (t->isArray()) {
    auto *at = static_cast<ArrayType *>(t);
    tn.arrayDims.push_back(at->size());
    t = at->element();
  }
  // Reverse array dims because we pushed them in order of peeling
  // But TypeName stores them as declared: int x[10][20]
  // ArrayType is Array(Array(int, 20), 10)
  // So peeling gives 10, then 20.
  // TypeName expects {10, 20}. So order is correct?
  // Let's check resolveType:
  // for (auto it = typeName.arrayDims.rbegin(); ... )
  // It iterates reverse to build Array types.
  // So {10, 20} -> Array(Array(int, 20), 10).
  // So peeling gives 10, then 20. Correct.

  if (t->isStruct()) {
    tn.name = static_cast<StructType *>(t)->name();
    // Generic args are already baked into the mangled name for StructType
  } else if (t->isInt()) {
    tn.name = "i32";
  } else if (t->isFloat()) {
    tn.name = "f64";
  } else if (t->isBool()) {
    tn.name = "bool";
  } else if (t->isString()) {
    tn.name = "str";
  } else if (t->isVoid()) {
    tn.name = "void";
  } else {
    tn.name = "unknown"; // Should not happen for concrete types
  }
  return tn;
}

void substitute(TypeName &tn,
                const std::unordered_map<std::string, TypeName> &map) {
  if (map.count(tn.name)) {
    TypeName replacement = map.at(tn.name);
    // Merge pointers and arrays
    replacement.pointerDepth += tn.pointerDepth;
    replacement.arrayDims.insert(replacement.arrayDims.end(),
                                 tn.arrayDims.begin(), tn.arrayDims.end());
    // What about generic args?
    // If T is replaced by Box<i32>, and we have T<f64> (invalid if T is type)
    // But if we have Box<T>, and T -> i32.
    // Box is not in map. T is in map.
    // We need to recurse on genericArgs.
    tn = replacement;
    return;
  }

  for (auto &arg : tn.genericArgs) {
    substitute(arg, map);
  }
}

void substitute(Expr &e, const std::unordered_map<std::string, TypeName> &map);
void substitute(Stmt &s, const std::unordered_map<std::string, TypeName> &map);

void substitute(Expr &e, const std::unordered_map<std::string, TypeName> &map) {
  std::visit(
      [&](auto &&arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, ExprUnary>) {
          substitute(*arg.rhs, map);
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
          substitute(*arg.lhs, map);
          substitute(*arg.rhs, map);
        } else if constexpr (std::is_same_v<T, ExprAssign>) {
          substitute(*arg.lhs, map);
          substitute(*arg.rhs, map);
        } else if constexpr (std::is_same_v<T, ExprCond>) {
          substitute(*arg.cond, map);
          substitute(*arg.thenE, map);
          substitute(*arg.elseE, map);
        } else if constexpr (std::is_same_v<T, ExprCall>) {
          for (auto &a : arg.args)
            substitute(*a, map);
        } else if constexpr (std::is_same_v<T, ExprIndex>) {
          substitute(*arg.base, map);
          substitute(*arg.index, map);
        } else if constexpr (std::is_same_v<T, ExprMember>) {
          substitute(*arg.base, map);
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
          substitute(*arg.expr, map);
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
          substitute(*arg.operand, map);
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
          substitute(*arg.operand, map);
        } else if constexpr (std::is_same_v<T, ExprCast>) {
          substitute(*arg.expr, map);
          substitute(arg.target, map);
        } else if constexpr (std::is_same_v<T, ExprInitList>) {
          for (auto &v : arg.values) {
            if (v.designator)
              substitute(**v.designator, map);
            substitute(*v.value, map);
          }
        }
      },
      e.v);
}

void substitute(Stmt &s, const std::unordered_map<std::string, TypeName> &map) {
  std::visit(
      [&](auto &&arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
          substitute(*arg.expr, map);
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
          if (arg.expr)
            substitute(**arg.expr, map);
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
          substitute(arg.type, map);
          for (auto &d : arg.declarators) {
            if (d.init)
              substitute(**d.init, map);
          }
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
          for (auto &stmt : arg.stmts)
            substitute(*stmt, map);
        } else if constexpr (std::is_same_v<T, StmtFor>) {
          if (arg.init)
            substitute(**arg.init, map);
          if (arg.cond)
            substitute(**arg.cond, map);
          if (arg.iter)
            substitute(**arg.iter, map);
          substitute(*arg.body, map);
        } else if constexpr (std::is_same_v<T, StmtIf>) {
          substitute(*arg.cond, map);
          substitute(*arg.thenBranch, map);
          if (arg.elseBranch)
            substitute(**arg.elseBranch, map);
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
          substitute(*arg.cond, map);
          substitute(*arg.body, map);
        } else if constexpr (std::is_same_v<T, StmtSwitch>) {
          substitute(*arg.cond, map);
          for (auto &c : arg.cases) {
            for (auto &v : c.values)
              substitute(*v, map);
            substitute(*c.body, map);
          }
          if (arg.defaultCase)
            substitute(**arg.defaultCase, map);
        }
      },
      s.v);
}

void SemanticAnalyzer::instantiateFunction(DeclFunc *df,
                                           const std::vector<Type *> &args,
                                           const DiagLoc &loc) {
  if (args.size() != df->genericParams.size()) {
    diags_.report(DiagLevel::Error, {},
                  "generic arg count mismatch for '" + df->name + "'");
    return;
  }

  std::string mangledName = df->name;
  for (const auto &arg : args) {
    mangledName += "_" + arg->toString();
  }

  // Check if already instantiated
  if (functions_.count(mangledName)) {
    return;
  }

  // Create concrete DeclFunc
  auto concreteDecl = std::make_unique<Decl>();
  concreteDecl->loc = loc; // Use original location?

  DeclFunc newFunc;
  newFunc.name = mangledName;
  newFunc.isExtern = df->isExtern;
  newFunc.isVariadic = df->isVariadic;
  // genericParams are empty for the concrete instance

  pushTypeScope();
  for (size_t i = 0; i < df->genericParams.size(); ++i) {
    declareTypeAlias(df->genericParams[i], args[i]);
  }

  newFunc.ret = df->ret; // Will be resolved during analysis
  // We need to resolve types NOW for the signature to register it correctly?
  // Or we let the normal analysis handle it?
  // Normal analysis visits declarations.
  // But we need to substitute types in the body too.
  // The `resolveType` uses `typeScopes_`.
  // So if we just clone the AST and analyze it with the type scope active...
  // But we can't keep the type scope active indefinitely.

  // Create substitution map
  std::unordered_map<std::string, TypeName> subMap;
  for (size_t i = 0; i < df->genericParams.size(); ++i) {
    subMap[df->genericParams[i]] = fromType(args[i]);
  }

  // Clone body
  if (df->body) {
    newFunc.body = df->body->clone();
    // Apply substitution
    for (auto &stmt : newFunc.body->stmts) {
      substitute(*stmt, subMap);
    }
  }

  // Substitute return type
  substitute(newFunc.ret, subMap);
  // Substitute params
  for (auto &p : df->params) {
    Param newP = p;
    substitute(newP.type, subMap);
    newFunc.params.push_back(newP);
  }

  concreteDecl->v = std::move(newFunc);
  Decl *rawDecl = concreteDecl.get();

  // Register
  instantiatedDecls_.push_back(std::move(concreteDecl));

  // Register in functions_ map so it can be found by subsequent calls
  visit(*rawDecl);
}

} // namespace agc
