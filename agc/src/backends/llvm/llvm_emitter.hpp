#pragma once

#include "agc/ast.hpp"
#include "agc/codegen.hpp"
#include "agc/diagnostics.hpp"
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <string>
#include <unordered_map>

namespace agc {

class FunctionEmitter {
  llvm::Module &M;
  llvm::Function *F;
  llvm::IRBuilder<> B;
  llvm::Type *RetTy;
  std::string &Err;
  DiagnosticEngine *Diags{nullptr};
  const std::unordered_map<std::string, Type *> *StructTypes{nullptr};

  struct VarInfo {
    llvm::AllocaInst *alloc;
    Type *type;
    bool isConst{false};
  };
  std::unordered_map<std::string, VarInfo> Locals;

  // For break/continue in loops
  llvm::BasicBlock *BreakBB{nullptr};
  llvm::BasicBlock *ContinueBB{nullptr};

  // Helpers
  llvm::AllocaInst *createAlloca(llvm::Type *ty, const std::string &name);
  llvm::Value *castTo(llvm::Value *V, llvm::Type *To);
  llvm::Value *emitLValue(const Expr &e, llvm::Type **outElemTy = nullptr);

public:
  FunctionEmitter(llvm::Module &m, llvm::Function *f, std::string &err,
                  DiagnosticEngine *diags,
                  const std::unordered_map<std::string, Type *> *structTypes);
  void initParams(const std::vector<Param> &params);
  llvm::Value *emitExpr(const Expr &e);
  bool emitStmt(const Stmt &s);
  bool emitBlock(const StmtBlock &body);
  bool emitBody(const StmtBlock &body);
};

// Helper functions exposed for codegen_llvm.cpp
llvm::Type *to_llvm_type(llvm::LLVMContext &ctx, const TypeName &t);
llvm::Type *to_llvm_type(llvm::LLVMContext &ctx, Type *t);
llvm::Constant *default_const_for(llvm::Type *ty);

} // namespace agc
