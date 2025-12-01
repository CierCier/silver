#include "agc/sema.hpp"
#include "agc/mangle.hpp"

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
        if (!ds->genericParams.empty()) {
          genericStructs_[ds->name] = ds;
        } else {
          structTypes_[ds->name] = typeCtx_.getOpaqueStruct(ds->name);
        }
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

  // Pass 0.5: Resolve Struct fields and traits
  for (auto &d : prog.decls) {
    if (auto *ds = std::get_if<DeclStruct>(&d->v)) {
      if (!ds->genericParams.empty())
        continue; // Skip generic templates
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

        // Process @trait attributes
        for (const auto &attr : ds->attributes) {
          if (attr.name == "trait") {
            for (const auto &traitName : attr.args) {
              // Validate known traits
              if (traitName != "copy" && traitName != "clone" &&
                  traitName != "drop" && traitName != "default" &&
                  traitName != "debug") {
                diags_.report(DiagLevel::Warning, d->loc,
                              "unknown trait '" + traitName + "'");
              }
              st->addTrait(traitName);
            }
          } else {
            diags_.report(DiagLevel::Warning, d->loc,
                          "unknown attribute '@" + attr.name + "'");
          }
        }
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
      if (!df->genericParams.empty()) {
        genericFunctions_[df->name] = df;
        continue;
      }
      // Register function (TODO: Function overloading? For now just name)
      Type *retType = resolveType(df->ret);
      std::vector<Type *> paramTypes;
      for (auto &p : df->params) {
        Type *pt = resolveType(p.type);
        p.resolvedType = pt;
        paramTypes.push_back(pt);
      }
      df->mangledName = mangle_function(*df);
      functions_[df->name] =
          FuncInfo{retType, std::move(paramTypes), df->mangledName};
    } else if (auto *di = std::get_if<DeclImpl>(&d->v)) {
      // Register impl methods as callable functions
      for (auto &m : di->methods) {
        if (auto *df = std::get_if<DeclFunc>(&m->v)) {
          Type *retType = resolveType(df->ret);
          std::vector<Type *> paramTypes;
          for (auto &p : df->params) {
            Type *pt = resolveType(p.type);
            p.resolvedType = pt;
            paramTypes.push_back(pt);
          }
          std::string mangledName = mangle_method(di->type.name, *df);
          df->mangledName = mangledName;

          // Register with TypeName_methodName as the callable name
          std::string callableName = di->type.name + "_" + df->name;
          functions_[callableName] =
              FuncInfo{retType, std::move(paramTypes), mangledName};
        }
      }
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

void SemanticAnalyzer::pushTypeScope() { typeScopes_.emplace_back(); }

void SemanticAnalyzer::popTypeScope() {
  if (!typeScopes_.empty())
    typeScopes_.pop_back();
}

void SemanticAnalyzer::declareTypeAlias(const std::string &name, Type *type) {
  if (typeScopes_.empty())
    return;
  typeScopes_.back()[name] = type;
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

} // namespace agc
