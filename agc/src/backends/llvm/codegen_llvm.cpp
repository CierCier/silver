#include "llvm_emitter.hpp"
#include <algorithm>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>

#include "agc/sema.hpp" // For SemanticAnalyzer and Type

namespace agc {

llvm::Type *to_llvm_type(llvm::LLVMContext &ctx, Type *t) {
  if (!t)
    return llvm::Type::getVoidTy(ctx);
  switch (t->kind()) {
  case TypeKind::Void:
    return llvm::Type::getVoidTy(ctx);
  case TypeKind::Bool:
    return llvm::Type::getInt8Ty(ctx); // Match C's _Bool (1 byte)
  case TypeKind::Int8:
    return llvm::Type::getInt8Ty(ctx);
  case TypeKind::Int16:
    return llvm::Type::getInt16Ty(ctx);
  case TypeKind::Int32:
    return llvm::Type::getInt32Ty(ctx);
  case TypeKind::Int64:
    return llvm::Type::getInt64Ty(ctx);
  case TypeKind::Float32:
    return llvm::Type::getFloatTy(ctx);
  case TypeKind::Float64:
    return llvm::Type::getDoubleTy(ctx);
  case TypeKind::String:
    return llvm::PointerType::getUnqual(ctx);
  case TypeKind::Pointer:
    return llvm::PointerType::getUnqual(ctx);
  case TypeKind::Array: {
    auto *at = static_cast<ArrayType *>(t);
    auto *elemTy = to_llvm_type(ctx, at->element());
    return llvm::ArrayType::get(elemTy, at->size());
  }
  case TypeKind::Struct: {
    auto *st = static_cast<StructType *>(t);
    auto *ty = llvm::StructType::getTypeByName(ctx, "struct." + st->name());
    if (!ty)
      ty = llvm::StructType::getTypeByName(ctx, st->name());
    if (!ty)
      return llvm::Type::getInt32Ty(ctx); // Fallback
    return ty;
  }
  case TypeKind::Enum:
    return llvm::Type::getInt32Ty(ctx); // Enums are i32
  case TypeKind::Function:
    return llvm::PointerType::getUnqual(ctx);
  case TypeKind::Meta:
    return llvm::Type::getVoidTy(ctx); // Should not happen in runtime
  }
  return llvm::Type::getVoidTy(ctx);
}

llvm::Type *to_llvm_type(llvm::LLVMContext &ctx, const TypeName &t) {
  std::string base = t.name;
  std::string lower = base;
  std::transform(lower.begin(), lower.end(), lower.begin(),
                 [](unsigned char c) { return (char)std::tolower(c); });

  llvm::Type *ty = nullptr;
  if (lower == "void")
    ty = llvm::Type::getVoidTy(ctx);
  else if (lower == "bool" || lower == "i1")
    ty = llvm::Type::getInt8Ty(ctx); // Match C's _Bool (1 byte)
  else if (lower == "i8" || lower == "u8" || lower == "char")
    ty = llvm::Type::getInt8Ty(ctx);
  else if (lower == "i16" || lower == "u16")
    ty = llvm::Type::getInt16Ty(ctx);
  else if (lower == "i32" || lower == "u32" || lower == "int")
    ty = llvm::Type::getInt32Ty(ctx);
  else if (lower == "i64" || lower == "u64" || lower == "long")
    ty = llvm::Type::getInt64Ty(ctx);
  else if (lower == "f32" || lower == "float")
    ty = llvm::Type::getFloatTy(ctx);
  else if (lower == "f64" || lower == "double")
    ty = llvm::Type::getDoubleTy(ctx);
  else if (lower == "str" || lower == "string")
    ty = llvm::PointerType::getUnqual(ctx);
  else {
    // Handle generic types by mangling: Optional<i32> -> Optional_i32
    std::string mangledName = base;
    if (!t.genericArgs.empty()) {
      for (const auto &arg : t.genericArgs) {
        mangledName += "_";
        // Handle pointer types in generic args
        for (unsigned i = 0; i < arg.pointerDepth; ++i) {
          mangledName += "*";
        }
        mangledName += arg.name;
      }
    }
    // Try original name for structs
    ty = llvm::StructType::getTypeByName(ctx, "struct." + mangledName);
    if (!ty)
      ty = llvm::StructType::getTypeByName(ctx, mangledName);
    if (!ty) {
      // Fallback to base name (for non-generic structs)
      ty = llvm::StructType::getTypeByName(ctx, "struct." + base);
      if (!ty)
        ty = llvm::StructType::getTypeByName(ctx, base);
    }
    if (!ty) {
      ty = llvm::Type::getInt32Ty(ctx);
    }
  }

  // arrays decay to pointer for now (opaque pointers)
  // arrays
  if (!t.arrayDims.empty()) {
    // Handle multi-dimensional arrays
    // TypeName dims are [10, 20] -> [10 x [20 x Ty]]
    // But wait, TypeName stores dims.
    // We need to wrap the base type.
    // If we are here, 'ty' is the base type (e.g. i32).
    for (auto it = t.arrayDims.rbegin(); it != t.arrayDims.rend(); ++it) {
      uint64_t size = 0;
      if (*it)
        size = **it;
      ty = llvm::ArrayType::get(ty, size);
    }
  }
  for (unsigned i = 0; i < t.pointerDepth; ++i) {
    ty = llvm::PointerType::getUnqual(ctx);
  }
  return ty;
}

llvm::Constant *default_const_for(llvm::Type *ty) {
  if (ty->isVoidTy())
    return nullptr;
  if (ty->isIntegerTy(1))
    return llvm::ConstantInt::getFalse(ty);
  if (ty->isIntegerTy())
    return llvm::ConstantInt::get(ty, 0);
  if (ty->isFloatTy())
    return llvm::ConstantFP::get(ty, 0.0);
  if (ty->isDoubleTy())
    return llvm::ConstantFP::get(ty, 0.0);
  if (ty->isPointerTy())
    return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ty));
  return llvm::Constant::getNullValue(ty);
}

static void emit_global_llvm(const DeclVar &v, llvm::Module &M) {
  auto &ctx = M.getContext();
  llvm::Type *ty = to_llvm_type(ctx, v.type);
  llvm::IRBuilder<> B(ctx);
  for (auto const &[name, loc, init] : v.declarators) {
    llvm::Constant *initConst = nullptr;
    if (init && (*init)) {
      // Handle different initializer types
      if (auto *ei = std::get_if<ExprInt>(&((*init)->v))) {
        if (ty->isIntegerTy())
          initConst = llvm::ConstantInt::get(ty, ei->value);
      } else if (auto *ef = std::get_if<ExprFloat>(&((*init)->v))) {
        if (ty->isFloatingPointTy())
          initConst = llvm::ConstantFP::get(ty, ef->value);
      } else if (auto *eb = std::get_if<ExprBool>(&((*init)->v))) {
        if (ty->isIntegerTy())
          initConst = llvm::ConstantInt::get(ty, eb->value ? 1 : 0);
      } else if (auto *es = std::get_if<ExprStr>(&((*init)->v))) {
        // Create a global string constant and get a pointer to it
        auto *strConst =
            llvm::ConstantDataArray::getString(ctx, es->value, true);
        auto *strGlobal = new llvm::GlobalVariable(
            M, strConst->getType(), true, llvm::GlobalValue::PrivateLinkage,
            strConst, name + ".str");
        initConst = llvm::ConstantExpr::getBitCast(strGlobal, ty);
      }
    }
    new llvm::GlobalVariable(
        M, ty,
        /*isConstant*/ false,
        v.isExtern ? llvm::GlobalValue::ExternalLinkage
                   : llvm::GlobalValue::ExternalLinkage,
        v.isExtern ? nullptr : (initConst ? initConst : default_const_for(ty)),
        name);
  }
}

static llvm::Function *declare_function_llvm_with_name(const DeclFunc &f,
                                                       const std::string &name,
                                                       llvm::Module &M) {
  auto &ctx = M.getContext();
  std::vector<llvm::Type *> paramTys;
  paramTys.reserve(f.params.size());
  for (auto const &p : f.params)
    paramTys.push_back(to_llvm_type(ctx, p.type));
  llvm::Type *retTy = to_llvm_type(ctx, f.ret);
  auto *fnTy = llvm::FunctionType::get(retTy, paramTys, f.isVariadic);
  auto *fn = M.getFunction(name);
  if (!fn)
    fn = llvm::Function::Create(fnTy, llvm::GlobalValue::ExternalLinkage, name,
                                M);
  else {
    // Update type if needed (simple case)
    if (fn->getFunctionType() != fnTy) {
      fn->deleteBody();
      fn->eraseFromParent();
      fn = llvm::Function::Create(fnTy, llvm::GlobalValue::ExternalLinkage,
                                  name, M);
    }
  }

  // Name parameters
  unsigned idx = 0;
  for (auto &arg : fn->args())
    arg.setName(f.params[idx++].name);

  return fn;
}

static llvm::Function *declare_function_llvm(const DeclFunc &f,
                                             llvm::Module &M) {
  return declare_function_llvm_with_name(
      f, f.mangledName.empty() ? f.name : f.mangledName, M);
}

// Declare a cast function with mangled name: TypeName_cast_TargetType
static llvm::Function *
declare_cast_function_llvm(const std::string &mangledName, const DeclCast &dc,
                           llvm::Module &M) {
  auto &ctx = M.getContext();
  std::vector<llvm::Type *> paramTys;
  for (auto const &p : dc.params)
    paramTys.push_back(to_llvm_type(ctx, p.type));
  llvm::Type *retTy = to_llvm_type(ctx, dc.target);
  auto *fnTy = llvm::FunctionType::get(retTy, paramTys, false);
  auto *fn = M.getFunction(mangledName);
  if (!fn)
    fn = llvm::Function::Create(fnTy, llvm::GlobalValue::ExternalLinkage,
                                mangledName, M);

  // Name parameters
  unsigned idx = 0;
  for (auto &arg : fn->args())
    arg.setName(dc.params[idx++].name);

  return fn;
}

static bool emit_cast_body_llvm(
    const std::string &mangledName, const DeclCast &dc, llvm::Module &M,
    std::string &err, DiagnosticEngine *diags,
    const std::unordered_map<std::string, Type *> &structTypes) {
  llvm::Function *fn = M.getFunction(mangledName);
  if (!fn) {
    err = "cast function not found: " + mangledName;
    return false;
  }

  FunctionEmitter emitter(M, fn, err, diags, &structTypes);
  emitter.initParams(dc.params);
  if (dc.body) {
    return emitter.emitBody(*dc.body);
  }
  return true;
}

static bool emit_function_body_llvm_with_name(
    const DeclFunc &f, const std::string &name, llvm::Module &M,
    std::string &err, DiagnosticEngine *diags,
    const std::unordered_map<std::string, Type *> &structTypes) {
  if (diags)
    diags->report(DiagLevel::Debug, "emit_function_body_llvm: " + name);
  llvm::Function *fn = M.getFunction(name);
  if (!fn) {
    err = "function not found: " + name;
    return false;
  }

  FunctionEmitter emitter(M, fn, err, diags, &structTypes);
  emitter.initParams(f.params);
  if (f.body) {
    bool res = emitter.emitBody(*f.body);
    if (diags)
      diags->report(DiagLevel::Debug,
                    "emit_function_body_llvm: " + name +
                        " done, res=" + (res ? "true" : "false"));
    return res;
  }
  return true;
}

static bool emit_function_body_llvm(
    const DeclFunc &f, llvm::Module &M, std::string &err,
    DiagnosticEngine *diags,
    const std::unordered_map<std::string, Type *> &structTypes) {
  return emit_function_body_llvm_with_name(
      f, f.mangledName.empty() ? f.name : f.mangledName, M, err, diags,
      structTypes);
}

class LlvmBackend : public CodegenBackend {
public:
  std::string_view name() const override { return "llvm"; }

  bool generate(const Program &prog, std::ostream &os, std::string &err,
                const CodegenOptions &opts) override {
    DiagnosticEngine *diags = opts.diags;
    if (diags && diags->isVerbose()) {
      diags->report(DiagLevel::Note, "Generating LLVM IR...");
    }
    if (diags)
      diags->report(DiagLevel::Debug, "Starting generate...");
    llvm::LLVMContext ctx;
    llvm::Module module("silver_module", ctx);

    // Collect structs for member access resolution
    std::unordered_map<std::string, Type *> emptyStructTypes;
    const auto &structTypes =
        opts.sema ? opts.sema->getStructTypes() : emptyStructTypes;

    // Collect defined function names to avoid duplicate declares
    std::vector<std::string> defined;
    defined.reserve(prog.decls.size());
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (!f->genericParams.empty())
          continue;
        if (f->body)
          defined.push_back(f->mangledName.empty() ? f->name : f->mangledName);
      }
    }

    // Create struct types (Opaque)
    std::vector<StructType *> sortedStructs;
    if (opts.sema) {
      for (const auto &pair : opts.sema->getStructTypes()) {
        if (pair.second->isStruct()) {
          sortedStructs.push_back(static_cast<StructType *>(pair.second));
        }
      }
    }
    std::sort(
        sortedStructs.begin(), sortedStructs.end(),
        [](StructType *a, StructType *b) { return a->name() < b->name(); });

    for (auto *st : sortedStructs) {
      llvm::StructType::create(ctx, "struct." + st->name());
    }

    // Set struct bodies
    for (auto *st : sortedStructs) {
      auto *stTy = llvm::StructType::getTypeByName(ctx, "struct." + st->name());
      if (stTy) {
        std::vector<llvm::Type *> elements;
        for (const auto &f : st->fields()) {
          elements.push_back(to_llvm_type(ctx, f.type));
        }
        stTy->setBody(elements);
      }
    }

    // Globals first
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *v = std::get_if<DeclVar>(&d.v))
        emit_global_llvm(*v, module);
    }

    // Prototypes without definitions (MUST be before definitions for calls to
    // work)
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (!f->genericParams.empty())
          continue;
        if (!f->body &&
            std::find(defined.begin(), defined.end(),
                      f->mangledName.empty() ? f->name : f->mangledName) ==
                defined.end())
          declare_function_llvm(*f, module);
      }
    }

    // Function definitions - Pass 1: Declare
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (!f->genericParams.empty())
          continue;
        if (f->body)
          declare_function_llvm(*f, module);
      }
    }

    // Instantiated functions - Pass 1: Declare
    if (opts.sema) {
      for (auto const &dptr : opts.sema->getInstantiatedDecls()) {
        const Decl &d = *dptr;
        if (auto *f = std::get_if<DeclFunc>(&d.v)) {
          if (!f->genericParams.empty())
            continue;
          if (f->body)
            declare_function_llvm(*f, module);
        }
      }
    }

    // Function definitions - Pass 2: Emit Body
    int declIdx = 0;
    for (auto const &dptr : prog.decls) {
      if (diags)
        diags->report(DiagLevel::Debug, "Decl " + std::to_string(declIdx++));
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (!f->genericParams.empty())
          continue;
        if (f->body)
          if (!emit_function_body_llvm(*f, module, err, diags, structTypes))
            return false;
      } else if (auto *impl = std::get_if<DeclImpl>(&d.v)) {
        // Emit cast and method function bodies from impl blocks
        // Skip generic impls (their methods have empty mangledName)
        for (auto const &m : impl->methods) {
          if (auto *dc = std::get_if<DeclCast>(&m->v)) {
            if (dc->body && !dc->mangledName.empty()) {
              if (!emit_cast_body_llvm(dc->mangledName, *dc, module, err, diags,
                                       structTypes))
                return false;
            }
          } else if (auto *df = std::get_if<DeclFunc>(&m->v)) {
            if (df->body && !df->mangledName.empty()) {
              if (!emit_function_body_llvm_with_name(
                      *df, df->mangledName, module, err, diags, structTypes))
                return false;
            }
          }
        }
      }
    }

    // Instantiated functions - Pass 2: Emit Body
    if (opts.sema) {
      for (auto const &dptr : opts.sema->getInstantiatedDecls()) {
        const Decl &d = *dptr;
        if (auto *f = std::get_if<DeclFunc>(&d.v)) {
          if (f->body) {
            if (!emit_function_body_llvm(*f, module, err, diags, structTypes))
              return false;
          }
        } else if (auto *dc = std::get_if<DeclCast>(&d.v)) {
          if (dc->body && !dc->mangledName.empty()) {
            if (!emit_cast_body_llvm(dc->mangledName, *dc, module, err, diags,
                                     structTypes))
              return false;
          }
        }
      }
    }
    if (diags)
      diags->report(DiagLevel::Debug, "Finished emitting bodies");

    // Verify module (non-fatal for now)
    if (diags)
      diags->report(DiagLevel::Debug, "Starting verification");
    if (llvm::verifyModule(module, &llvm::errs())) {
      err = "LLVM verification reported issues";
      if (diags)
        diags->report(DiagLevel::Debug, "LLVM verification failed");
    } else {
      if (diags)
        diags->report(DiagLevel::Debug, "LLVM verification passed");
    }

    // Optimization passes
    if (diags)
      diags->report(DiagLevel::Debug, "Running optimization passes...");
    llvm::legacy::PassManager pm;
    pm.add(llvm::createPromoteMemoryToRegisterPass());
    pm.add(llvm::createInstructionCombiningPass());
    pm.add(llvm::createReassociatePass());
    pm.add(llvm::createEarlyCSEPass());
    pm.add(llvm::createCFGSimplificationPass());
    pm.run(module);

    std::string buf;
    llvm::raw_string_ostream rso(buf);
    module.print(rso, nullptr);
    rso.flush();
    os << buf;
    return true;
  }

  bool emit_object_file(const Program &prog, const std::string &filename,
                        std::string &err, const CodegenOptions &opts) override {
    DiagnosticEngine *diags = opts.diags;
    if (diags && diags->isVerbose()) {
      diags->report(DiagLevel::Note, "Emitting object file: " + filename);
    }
    if (diags)
      diags->report(DiagLevel::Debug, "Starting emit_object_file...");

    llvm::LLVMContext ctx;
    llvm::Module module("silver_module", ctx);

    // Collect structs for member access resolution
    std::unordered_map<std::string, Type *> emptyStructTypes;
    const auto &structTypes =
        opts.sema ? opts.sema->getStructTypes() : emptyStructTypes;

    // Collect defined function names to avoid duplicate declares
    std::vector<std::string> defined;
    defined.reserve(prog.decls.size());
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (!f->genericParams.empty())
          continue;
        if (f->body)
          defined.push_back(f->mangledName.empty() ? f->name : f->mangledName);
      }
    }

    // Create struct types (Opaque)
    std::vector<StructType *> sortedStructs;
    if (opts.sema) {
      for (const auto &pair : opts.sema->getStructTypes()) {
        if (pair.second->isStruct()) {
          sortedStructs.push_back(static_cast<StructType *>(pair.second));
        }
      }
    }
    std::sort(
        sortedStructs.begin(), sortedStructs.end(),
        [](StructType *a, StructType *b) { return a->name() < b->name(); });

    for (auto *st : sortedStructs) {
      llvm::StructType::create(ctx, "struct." + st->name());
    }

    // Set struct bodies
    for (auto *st : sortedStructs) {
      auto *stTy = llvm::StructType::getTypeByName(ctx, "struct." + st->name());
      if (stTy) {
        std::vector<llvm::Type *> elements;
        for (const auto &f : st->fields()) {
          elements.push_back(to_llvm_type(ctx, f.type));
        }
        stTy->setBody(elements);
      }
    }

    // Globals first
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *v = std::get_if<DeclVar>(&d.v))
        emit_global_llvm(*v, module);
    }

    // Prototypes without definitions (MUST be before definitions for calls
    // to work)
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (!f->genericParams.empty())
          continue;
        if (!f->body &&
            std::find(defined.begin(), defined.end(),
                      f->mangledName.empty() ? f->name : f->mangledName) ==
                defined.end())
          declare_function_llvm(*f, module);
      }
    }

    // Function definitions - Pass 1: Declare
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (!f->genericParams.empty())
          continue;
        if (f->body)
          declare_function_llvm(*f, module);
      } else if (auto *impl = std::get_if<DeclImpl>(&d.v)) {
        // Declare cast and method functions from impl blocks
        // Skip generic impls (their methods have empty mangledName)
        for (auto const &m : impl->methods) {
          if (auto *dc = std::get_if<DeclCast>(&m->v)) {
            if (dc->body && !dc->mangledName.empty()) {
              declare_cast_function_llvm(dc->mangledName, *dc, module);
            }
          } else if (auto *df = std::get_if<DeclFunc>(&m->v)) {
            if (df->body && !df->mangledName.empty()) {
              declare_function_llvm_with_name(*df, df->mangledName, module);
            }
          }
        }
      }
    }

    // Instantiated functions - Pass 1: Declare
    if (opts.sema) {
      for (auto const &dptr : opts.sema->getInstantiatedDecls()) {
        const Decl &d = *dptr;
        if (auto *f = std::get_if<DeclFunc>(&d.v)) {
          if (!f->genericParams.empty())
            continue;
          if (f->body)
            declare_function_llvm(*f, module);
        } else if (auto *dc = std::get_if<DeclCast>(&d.v)) {
          if (dc->body && !dc->mangledName.empty()) {
            declare_cast_function_llvm(dc->mangledName, *dc, module);
          }
        }
      }
    }

    // Function definitions - Pass 2: Emit Body
    if (diags)
      diags->report(DiagLevel::Debug, "Emitting bodies for " +
                                          std::to_string(prog.decls.size()) +
                                          " decls");
    int declIdx = 0;
    for (auto const &dptr : prog.decls) {
      if (diags)
        diags->report(DiagLevel::Debug, "Decl " + std::to_string(declIdx++));
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (!f->genericParams.empty())
          continue;
        if (f->body)
          if (!emit_function_body_llvm(*f, module, err, diags, structTypes))
            return false;
      } else if (auto *impl = std::get_if<DeclImpl>(&d.v)) {
        // Emit cast and method function bodies from impl blocks
        // Skip generic impls (their methods have empty mangledName)
        for (auto const &m : impl->methods) {
          if (auto *dc = std::get_if<DeclCast>(&m->v)) {
            if (dc->body && !dc->mangledName.empty()) {
              if (!emit_cast_body_llvm(dc->mangledName, *dc, module, err, diags,
                                       structTypes))
                return false;
            }
          } else if (auto *df = std::get_if<DeclFunc>(&m->v)) {
            if (df->body && !df->mangledName.empty()) {
              if (!emit_function_body_llvm_with_name(
                      *df, df->mangledName, module, err, diags, structTypes))
                return false;
            }
          }
        }
      }
    }

    // Instantiated functions - Pass 2: Emit Body
    if (opts.sema) {
      for (auto const &dptr : opts.sema->getInstantiatedDecls()) {
        const Decl &d = *dptr;
        if (auto *f = std::get_if<DeclFunc>(&d.v)) {
          if (f->body) {
            if (!emit_function_body_llvm(*f, module, err, diags, structTypes))
              return false;
          }
        } else if (auto *dc = std::get_if<DeclCast>(&d.v)) {
          if (dc->body && !dc->mangledName.empty()) {
            if (!emit_cast_body_llvm(dc->mangledName, *dc, module, err, diags,
                                     structTypes))
              return false;
          }
        }
      }
    }
    if (diags)
      diags->report(DiagLevel::Debug, "Finished emitting bodies");

    // Initialize native target only
    if (diags)
      diags->report(DiagLevel::Debug, "Initializing LLVM native target...");
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    if (diags)
      diags->report(DiagLevel::Debug, "LLVM native target initialized.");

    auto targetTripleStr = llvm::sys::getDefaultTargetTriple();
    if (diags)
      diags->report(DiagLevel::Debug, "Target triple: " + targetTripleStr);
    llvm::Triple targetTriple(targetTripleStr);
    module.setTargetTriple(targetTriple);

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTripleStr, error);

    if (!target) {
      err = error;
      return false;
    }

    auto cpu = llvm::sys::getHostCPUName();
    auto features = "";

    llvm::TargetOptions opt;
    auto rm = std::optional<llvm::Reloc::Model>(llvm::Reloc::PIC_);

    auto targetMachine =
        target->createTargetMachine(targetTripleStr, cpu, features, opt, rm);

    module.setDataLayout(targetMachine->createDataLayout());

    std::error_code ec;
    llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);

    if (ec) {
      err = "Could not open file: " + ec.message();
      return false;
    }

    llvm::legacy::PassManager pass;

    // Optimization passes
    if (diags)
      diags->report(DiagLevel::Debug, "Running optimization passes...");
    pass.add(llvm::createPromoteMemoryToRegisterPass());
    pass.add(llvm::createInstructionCombiningPass());
    pass.add(llvm::createReassociatePass());
    pass.add(llvm::createEarlyCSEPass());
    pass.add(llvm::createCFGSimplificationPass());

    auto fileType = llvm::CodeGenFileType::ObjectFile;

    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
      err = "TargetMachine can't emit a file of this type";
      return false;
    }

    pass.run(module);
    dest.flush();

    return true;
  }
};

std::unique_ptr<CodegenBackend> create_backend_llvm() {
  return std::make_unique<LlvmBackend>();
}

} // namespace agc
