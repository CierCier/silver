#include "llvm_emitter.hpp"

namespace agc {

FunctionEmitter::FunctionEmitter(
    llvm::Module &m, llvm::Function *f, std::string &err,
    DiagnosticEngine *diags,
    const std::unordered_map<std::string, DeclStruct> *structs)
    : M(m), F(f), Err(err), Diags(diags), Structs(structs), B(m.getContext()) {
  RetTy = F->getReturnType();
  auto *entry = llvm::BasicBlock::Create(M.getContext(), "entry", F);
  B.SetInsertPoint(entry);
  // B.SetInsertPoint(llvm::BasicBlock::Create(M.getContext(), "entry", F));
}

llvm::AllocaInst *FunctionEmitter::createAlloca(llvm::Type *ty,
                                                const std::string &name) {
  llvm::IRBuilder<> tmp(&F->getEntryBlock(), F->getEntryBlock().begin());
  return tmp.CreateAlloca(ty, nullptr, name);
}

void FunctionEmitter::initParams(const std::vector<Param> &params) {
  unsigned idx = 0;
  for (auto &arg : F->args()) {
    std::string name = params[idx].name;
    TypeName ty = params[idx].type;
    auto *alloc = createAlloca(arg.getType(), name + ".addr");
    B.CreateStore(&arg, alloc);
    Locals[name] = {alloc, ty, false};
    idx++;
  }
}

bool FunctionEmitter::emitStmt(const Stmt &s) {
  if (auto *sb = std::get_if<StmtBlock>(&s.v)) {
    return emitBody(*sb);
  }
  if (auto *se = std::get_if<StmtExpr>(&s.v)) {
    if (!emitExpr(*se->expr))
      return false;
    return true;
  }
  if (auto *sr = std::get_if<StmtReturn>(&s.v)) {
    if (!sr->expr) {
      B.CreateRetVoid();
      return true;
    }
    auto *v = emitExpr(**sr->expr);
    if (!v)
      return false;
    // cast return value
    v = castTo(v, RetTy);
    B.CreateRet(v);
    return true;
  }
  if (auto *vd = std::get_if<StmtDecl>(&s.v)) {
    // Create alloca
    auto *ty = to_llvm_type(M.getContext(), vd->type);
    for (auto const &[name, loc, init] : vd->declarators) {
      auto *alloc = createAlloca(ty, name);
      Locals[name] = {alloc, vd->type, false};
      if (init && *init) {
        // Check if this is an init list for a struct
        if (auto *initList = std::get_if<ExprInitList>(&(*init)->v)) {
          // Handle struct initialization
          if (auto *structTy = llvm::dyn_cast<llvm::StructType>(ty)) {
            unsigned numElements = structTy->getNumElements();
            for (unsigned i = 0; i < initList->values.size() && i < numElements;
                 ++i) {
              auto *v = emitExpr(*initList->values[i]);
              if (!v)
                return false;
              auto *elemTy = structTy->getElementType(i);
              v = castTo(v, elemTy);
              auto *elemPtr = B.CreateStructGEP(structTy, alloc, i,
                                                "gep." + std::to_string(i));
              B.CreateStore(v, elemPtr);
            }
          } else if (initList->values.size() == 1) {
            // Single-value init list for non-struct type
            auto *v = emitExpr(*initList->values[0]);
            if (v) {
              v = castTo(v, ty);
              B.CreateStore(v, alloc);
            } else {
              return false;
            }
          } else {
            Err = "initializer list with multiple values for non-struct type";
            return false;
          }
        } else {
          auto *v = emitExpr(**init);
          if (v) {
            v = castTo(v, ty);
            B.CreateStore(v, alloc);
          } else {
            return false;
          }
        }
      }
    }
    return true;
  }
  if (auto *iff = std::get_if<StmtIf>(&s.v)) {
    auto *cond = emitExpr(*iff->cond);
    if (!cond)
      return false;
    cond = castTo(cond, llvm::Type::getInt1Ty(M.getContext()));

    llvm::Function *TheFunction = B.GetInsertBlock()->getParent();
    llvm::BasicBlock *ThenBB =
        llvm::BasicBlock::Create(M.getContext(), "then", TheFunction);
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(M.getContext(), "else");
    llvm::BasicBlock *MergeBB =
        llvm::BasicBlock::Create(M.getContext(), "ifcont");

    bool hasElse = (iff->elseBranch.has_value());
    if (!hasElse)
      ElseBB = MergeBB;

    B.CreateCondBr(cond, ThenBB, ElseBB);

    B.SetInsertPoint(ThenBB);
    if (!emitStmt(*iff->thenBranch))
      return false;
    if (!B.GetInsertBlock()->getTerminator())
      B.CreateBr(MergeBB);

    if (hasElse) {
      TheFunction->insert(TheFunction->end(), ElseBB);
      B.SetInsertPoint(ElseBB);
      if (!emitStmt(**iff->elseBranch))
        return false;
      if (!B.GetInsertBlock()->getTerminator())
        B.CreateBr(MergeBB);
    }

    TheFunction->insert(TheFunction->end(), MergeBB);
    B.SetInsertPoint(MergeBB);
    return true;
  }
  if (auto *wh = std::get_if<StmtWhile>(&s.v)) {
    llvm::Function *TheFunction = B.GetInsertBlock()->getParent();
    llvm::BasicBlock *CondBB =
        llvm::BasicBlock::Create(M.getContext(), "cond", TheFunction);
    llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(M.getContext(), "loop");
    llvm::BasicBlock *AfterBB =
        llvm::BasicBlock::Create(M.getContext(), "afterloop");

    B.CreateBr(CondBB);
    B.SetInsertPoint(CondBB);

    auto *cond = emitExpr(*wh->cond);
    if (!cond)
      return false;
    cond = castTo(cond, llvm::Type::getInt1Ty(M.getContext()));
    B.CreateCondBr(cond, LoopBB, AfterBB);

    TheFunction->insert(TheFunction->end(), LoopBB);
    B.SetInsertPoint(LoopBB);

    auto *OldBreak = BreakBB;
    auto *OldContinue = ContinueBB;
    BreakBB = AfterBB;
    ContinueBB = CondBB;

    if (!emitStmt(*wh->body))
      return false;
    if (!B.GetInsertBlock()->getTerminator())
      B.CreateBr(CondBB);

    BreakBB = OldBreak;
    ContinueBB = OldContinue;

    TheFunction->insert(TheFunction->end(), AfterBB);
    B.SetInsertPoint(AfterBB);
    return true;
  }
  if (std::holds_alternative<StmtBreak>(s.v)) {
    if (!BreakBB) {
      Err = "break statement not in loop";
      return false;
    }
    B.CreateBr(BreakBB);
    return true;
  }
  if (std::holds_alternative<StmtContinue>(s.v)) {
    if (!ContinueBB) {
      Err = "continue statement not in loop";
      return false;
    }
    B.CreateBr(ContinueBB);
    return true;
  }
  if (auto *sw = std::get_if<StmtSwitch>(&s.v)) {
    auto *cond = emitExpr(*sw->cond);
    if (!cond)
      return false;

    if (!cond->getType()->isIntegerTy()) {
      Err = "switch condition must be integer";
      return false;
    }

    llvm::Function *TheFunction = B.GetInsertBlock()->getParent();
    llvm::BasicBlock *MergeBB =
        llvm::BasicBlock::Create(M.getContext(), "switch.merge");
    llvm::BasicBlock *DefaultBB = nullptr;

    if (sw->defaultCase) {
      DefaultBB = llvm::BasicBlock::Create(M.getContext(), "switch.default",
                                           TheFunction);
    } else {
      DefaultBB = MergeBB;
    }

    unsigned numCases = 0;
    for (const auto &c : sw->cases)
      numCases += c.values.size();

    llvm::SwitchInst *Switch = B.CreateSwitch(cond, DefaultBB, numCases);

    auto *OldBreak = BreakBB;
    BreakBB = MergeBB;

    for (const auto &c : sw->cases) {
      llvm::BasicBlock *CaseBB =
          llvm::BasicBlock::Create(M.getContext(), "switch.case", TheFunction);

      for (const auto &val : c.values) {
        if (auto *ei = std::get_if<ExprInt>(&val->v)) {
          auto *caseVal = llvm::ConstantInt::get(
              llvm::cast<llvm::IntegerType>(cond->getType()), ei->value);
          Switch->addCase(caseVal, CaseBB);
        } else {
          Err = "case value must be integer constant";
          return false;
        }
      }

      B.SetInsertPoint(CaseBB);
      if (c.body) {
        if (!emitStmt(*c.body))
          return false;
      }
      if (!B.GetInsertBlock()->getTerminator()) {
        B.CreateBr(MergeBB);
      }
    }

    if (sw->defaultCase) {
      B.SetInsertPoint(DefaultBB);
      if (!emitStmt(**sw->defaultCase))
        return false;
      if (!B.GetInsertBlock()->getTerminator()) {
        B.CreateBr(MergeBB);
      }
    }

    BreakBB = OldBreak;

    TheFunction->insert(TheFunction->end(), MergeBB);
    B.SetInsertPoint(MergeBB);
    return true;
  }
  return true;
}

bool FunctionEmitter::emitBody(const StmtBlock &body) {
  for (auto &sp : body.stmts) {
    if (!emitStmt(*sp))
      return false;
    if (B.GetInsertBlock()->getTerminator())
      break; // function already returned
  }
  // If no terminator, add default return
  if (!B.GetInsertBlock()->getTerminator()) {
    if (RetTy->isVoidTy())
      B.CreateRetVoid();
    else
      B.CreateRet(default_const_for(RetTy));
  }
  return true;
}

} // namespace agc
