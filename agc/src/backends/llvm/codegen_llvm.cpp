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

namespace agc {

llvm::Type *to_llvm_type(llvm::LLVMContext &ctx, const TypeName &t) {
  std::string base = t.name;
  std::transform(base.begin(), base.end(), base.begin(),
                 [](unsigned char c) { return (char)std::tolower(c); });
  llvm::Type *ty = nullptr;
  if (base == "void")
    ty = llvm::Type::getVoidTy(ctx);
  else if (base == "bool" || base == "i1")
    ty = llvm::Type::getInt1Ty(ctx);
  else if (base == "i8" || base == "u8" || base == "char")
    ty = llvm::Type::getInt8Ty(ctx);
  else if (base == "i16" || base == "u16")
    ty = llvm::Type::getInt16Ty(ctx);
  else if (base == "i32" || base == "u32" || base == "int")
    ty = llvm::Type::getInt32Ty(ctx);
  else if (base == "i64" || base == "u64" || base == "long")
    ty = llvm::Type::getInt64Ty(ctx);
  else if (base == "f32" || base == "float")
    ty = llvm::Type::getFloatTy(ctx);
  else if (base == "f64" || base == "double")
    ty = llvm::Type::getDoubleTy(ctx);
  else if (base == "str" || base == "string")
    ty = llvm::PointerType::getUnqual(ctx);
  else {
    ty = llvm::StructType::getTypeByName(ctx, "struct." + base);
    if (!ty)
      ty = llvm::StructType::getTypeByName(ctx, base);
    if (!ty)
      ty = llvm::Type::getInt32Ty(ctx);
  }

  // arrays decay to pointer for now (opaque pointers)
  if (!t.arrayDims.empty()) {
    ty = llvm::PointerType::getUnqual(ctx);
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
  for (auto const &[name, loc, init] : v.declarators) {
    llvm::Constant *initConst = nullptr;
    if (init && (*init)) {
      // Only handle simple integer initializers for now
      if (auto *ei = std::get_if<ExprInt>(&((*init)->v))) {
        if (ty->isIntegerTy())
          initConst = llvm::ConstantInt::get(ty, ei->value);
      } else if (auto *ef = std::get_if<ExprFloat>(&((*init)->v))) {
        if (ty->isFloatingPointTy())
          initConst = llvm::ConstantFP::get(ty, ef->value);
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

static llvm::Function *declare_function_llvm(const DeclFunc &f,
                                             llvm::Module &M) {
  auto &ctx = M.getContext();
  std::vector<llvm::Type *> paramTys;
  paramTys.reserve(f.params.size());
  for (auto const &p : f.params)
    paramTys.push_back(to_llvm_type(ctx, p.type));
  llvm::Type *retTy = to_llvm_type(ctx, f.ret);
  auto *fnTy = llvm::FunctionType::get(retTy, paramTys, f.isVariadic);
  auto *fn = M.getFunction(f.name);
  if (!fn)
    fn = llvm::Function::Create(fnTy, llvm::GlobalValue::ExternalLinkage,
                                f.name, M);
  else {
    // Update type if needed (simple case)
    if (fn->getFunctionType() != fnTy) {
      fn->deleteBody();
      fn->eraseFromParent();
      fn = llvm::Function::Create(fnTy, llvm::GlobalValue::ExternalLinkage,
                                  f.name, M);
    }
  }

  // Name parameters
  unsigned idx = 0;
  for (auto &arg : fn->args())
    arg.setName(f.params[idx++].name);

  return fn;
}

static bool emit_function_body_llvm(
    const DeclFunc &f, llvm::Module &M, std::string &err,
    DiagnosticEngine *diags,
    const std::unordered_map<std::string, DeclStruct> &structs) {
  if (diags)
    diags->report(DiagLevel::Debug, "emit_function_body_llvm: " + f.name);
  llvm::Function *fn = M.getFunction(f.name);
  if (!fn) {
    err = "function not found: " + f.name;
    return false;
  }

  FunctionEmitter emitter(M, fn, err, diags, &structs);
  emitter.initParams(f.params);
  if (f.body) {
    bool res = emitter.emitBody(*f.body);
    if (diags)
      diags->report(DiagLevel::Debug,
                    "emit_function_body_llvm: " + f.name +
                        " done, res=" + (res ? "true" : "false"));
    return res;
  }
  return true;
}

bool emit_object_file(const Program &prog, const std::string &filename,
                      std::string &err, const CodegenOptions &opts) {
  DiagnosticEngine *diags = opts.diags;
  if (diags && diags->isVerbose()) {
    diags->report(DiagLevel::Note, "Emitting object file: " + filename);
  }
  if (diags)
    diags->report(DiagLevel::Debug, "Starting emit_object_file...");
  llvm::LLVMContext ctx;
  llvm::Module module("silver_module", ctx);

  // Collect structs for member access resolution
  std::unordered_map<std::string, DeclStruct> structs;
  for (auto const &dptr : prog.decls) {
    if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
      structs[s->name] = *s;
    }
  }

  // Collect defined function names to avoid duplicate declares
  std::vector<std::string> defined;
  defined.reserve(prog.decls.size());
  for (auto const &dptr : prog.decls) {
    const Decl &d = *dptr;
    if (auto *f = std::get_if<DeclFunc>(&d.v)) {
      if (f->body)
        defined.push_back(f->name);
    }
  }

  // Create struct types (Opaque)
  for (auto const &dptr : prog.decls) {
    if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
      llvm::StructType::create(ctx, "struct." + s->name);
    }
  }

  // Set struct bodies
  for (auto const &dptr : prog.decls) {
    if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
      auto *stTy = llvm::StructType::getTypeByName(ctx, "struct." + s->name);
      if (stTy) {
        std::vector<llvm::Type *> elements;
        for (const auto &f : s->fields) {
          llvm::Type *fTy = to_llvm_type(ctx, f.type);
          for (size_t i = 0; i < f.names.size(); ++i) {
            elements.push_back(fTy);
          }
        }
        stTy->setBody(elements);
      }
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
      if (!f->body &&
          std::find(defined.begin(), defined.end(), f->name) == defined.end())
        declare_function_llvm(*f, module);
    }
  }

  // Function definitions - Pass 1: Declare
  for (auto const &dptr : prog.decls) {
    const Decl &d = *dptr;
    if (auto *f = std::get_if<DeclFunc>(&d.v)) {
      if (f->body)
        declare_function_llvm(*f, module);
    } else {
      if (diags)
        diags->report(DiagLevel::Error, "Unexpected declaration type");
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
      if (f->body)
        if (!emit_function_body_llvm(*f, module, err, diags, structs))
          return false;
    }
  }
  if (diags)
    diags->report(DiagLevel::Debug, "Finished emitting bodies");

  // Initialize targets
  if (diags)
    diags->report(DiagLevel::Debug, "Initializing LLVM targets...");
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();
  if (diags)
    diags->report(DiagLevel::Debug, "LLVM targets initialized.");

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

  auto cpu = "generic";
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
  auto fileType = llvm::CodeGenFileType::ObjectFile;

  if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
    err = "TargetMachine can't emit a file of this type";
    return false;
  }

  pass.run(module);
  dest.flush();

  return true;
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
    std::unordered_map<std::string, DeclStruct> structs;
    for (auto const &dptr : prog.decls) {
      if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
        structs[s->name] = *s;
      }
    }

    // Collect defined function names to avoid duplicate declares
    std::vector<std::string> defined;
    defined.reserve(prog.decls.size());
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (f->body)
          defined.push_back(f->name);
      }
    }

    // Create struct types (Opaque)
    for (auto const &dptr : prog.decls) {
      if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
        llvm::StructType::create(ctx, "struct." + s->name);
      }
    }

    // Set struct bodies
    for (auto const &dptr : prog.decls) {
      if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
        auto *stTy = llvm::StructType::getTypeByName(ctx, "struct." + s->name);
        if (stTy) {
          std::vector<llvm::Type *> elements;
          for (const auto &f : s->fields) {
            llvm::Type *fTy = to_llvm_type(ctx, f.type);
            for (size_t i = 0; i < f.names.size(); ++i) {
              elements.push_back(fTy);
            }
          }
          stTy->setBody(elements);
        }
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
        if (!f->body &&
            std::find(defined.begin(), defined.end(), f->name) == defined.end())
          declare_function_llvm(*f, module);
      }
    }

    // Function definitions - Pass 1: Declare
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (f->body)
          declare_function_llvm(*f, module);
      }
    }

    // Function definitions - Pass 2: Emit Body
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (f->body)
          if (!emit_function_body_llvm(*f, module, err, diags, structs))
            return false;
      }
    }
    if (diags)
      diags->report(DiagLevel::Debug, "Finished emitting bodies");

    // Verify module (non-fatal for now)
    if (diags)
      diags->report(DiagLevel::Debug, "Starting verification");
    llvm::raw_null_ostream nulls;
    if (llvm::verifyModule(module, &nulls)) {
      err = "LLVM verification reported issues";
      if (diags)
        diags->report(DiagLevel::Debug, "LLVM verification failed");
      module.print(llvm::errs(), nullptr);
    } else {
      if (diags)
        diags->report(DiagLevel::Debug, "LLVM verification passed");
    }

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
    std::unordered_map<std::string, DeclStruct> structs;
    for (auto const &dptr : prog.decls) {
      if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
        structs[s->name] = *s;
      }
    }

    // Collect defined function names to avoid duplicate declares
    std::vector<std::string> defined;
    defined.reserve(prog.decls.size());
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (f->body)
          defined.push_back(f->name);
      }
    }

    // Create struct types (Opaque)
    for (auto const &dptr : prog.decls) {
      if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
        llvm::StructType::create(ctx, "struct." + s->name);
      }
    }

    // Set struct bodies
    for (auto const &dptr : prog.decls) {
      if (auto *s = std::get_if<DeclStruct>(&dptr->v)) {
        auto *stTy = llvm::StructType::getTypeByName(ctx, "struct." + s->name);
        if (stTy) {
          std::vector<llvm::Type *> elements;
          for (const auto &f : s->fields) {
            llvm::Type *fTy = to_llvm_type(ctx, f.type);
            for (size_t i = 0; i < f.names.size(); ++i) {
              elements.push_back(fTy);
            }
          }
          stTy->setBody(elements);
        }
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
        if (!f->body &&
            std::find(defined.begin(), defined.end(), f->name) == defined.end())
          declare_function_llvm(*f, module);
      }
    }

    // Function definitions - Pass 1: Declare
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *f = std::get_if<DeclFunc>(&d.v)) {
        if (f->body)
          declare_function_llvm(*f, module);
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
        if (f->body)
          if (!emit_function_body_llvm(*f, module, err, diags, structs))
            return false;
      }
    }
    if (diags)
      diags->report(DiagLevel::Debug, "Finished emitting bodies");

    // Initialize targets
    if (diags)
      diags->report(DiagLevel::Debug, "Initializing LLVM targets...");
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();
    if (diags)
      diags->report(DiagLevel::Debug, "LLVM targets initialized.");

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

    auto cpu = "generic";
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
