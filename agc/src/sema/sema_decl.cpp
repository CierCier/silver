#include "agc/mangle.hpp"
#include "agc/sema.hpp"

namespace agc {

void SemanticAnalyzer::visit(Decl &decl) {
  if (auto *df = std::get_if<DeclFunc>(&decl.v)) {
    if (!df->genericParams.empty())
      return; // Skip generic functions
    // Function already registered? No, we didn't register functions in pass 1
    // fully. Let's register params and body.
    Type *retType = resolveType(df->ret);
    if (df->mangledName.empty()) {
      df->mangledName = mangle_function(*df);
    }

    // Ensure function is registered (needed for instantiated functions)
    if (functions_.find(df->name) == functions_.end()) {
      std::vector<Type *> paramTypes;
      for (auto &p : df->params) {
        Type *pt = resolveType(p.type);
        p.resolvedType = pt;
        paramTypes.push_back(pt);
      }
      functions_[df->name] =
          FuncInfo{retType, std::move(paramTypes), df->mangledName};
    }

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
          // Generate mangled name
          std::string mangledName = mangle_cast(di->type.name, *dc);
          dc->mangledName = mangledName;
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
          std::string mangledName = mangle_method(di->type.name, *df);
          df->mangledName = mangledName;
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
        p.resolvedType = pt;
        declareVar(p.name, decl.loc, nullptr, pt);
      }
      for (auto &s : dc->body->stmts) {
        visit(*s);
      }
      popScope();
    }
  } else if (auto *dt = std::get_if<DeclTrait>(&decl.v)) {
    // Trait definitions are processed during trait registration
    // Nothing to do here for now - validation happens elsewhere
    (void)dt;
  }
}

} // namespace agc
