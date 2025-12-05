#include "llvm_emitter.hpp"
#include <llvm/IR/InlineAsm.h>

namespace agc {

FunctionEmitter::FunctionEmitter(
    llvm::Module &m, llvm::Function *f, std::string &err,
    DiagnosticEngine *diags,
    const std::unordered_map<std::string, Type *> *structTypes)
    : M(m), F(f), Err(err), Diags(diags), StructTypes(structTypes),
      B(m.getContext()) {
  // Check if function uses sret (first param has sret attribute)
  if (F->arg_size() > 0 &&
      F->hasParamAttribute(0, llvm::Attribute::StructRet)) {
    // Get the struct type from the sret attribute
    SretPtr = F->getArg(0);
    RetTy =
        F->getParamAttribute(0, llvm::Attribute::StructRet).getValueAsType();
  } else {
    RetTy = F->getReturnType();
  }
  auto *entry = llvm::BasicBlock::Create(M.getContext(), "entry", F);
  B.SetInsertPoint(entry);
}

llvm::AllocaInst *FunctionEmitter::createAlloca(llvm::Type *ty,
                                                const std::string &name) {
  llvm::IRBuilder<> tmp(&F->getEntryBlock(), F->getEntryBlock().begin());
  return tmp.CreateAlloca(ty, nullptr, name);
}

void FunctionEmitter::initParams(const std::vector<Param> &params) {
  unsigned idx = 0;
  unsigned argOffset = SretPtr ? 1 : 0; // Skip sret parameter
  for (auto &arg : F->args()) {
    if (SretPtr && idx == 0) {
      // Skip the sret pointer parameter - it's not a user-visible param
      idx++;
      continue;
    }
    unsigned paramIdx = idx - argOffset;
    if (paramIdx >= params.size())
      break;
    std::string name = params[paramIdx].name;
    Type *ty = params[paramIdx].resolvedType;
    auto *alloc = createAlloca(arg.getType(), name + ".addr");
    B.CreateStore(&arg, alloc);
    Locals[name] = {alloc, ty, false};
    idx++;
  }
}

bool FunctionEmitter::emitBlock(const StmtBlock &body) {
  for (auto &sp : body.stmts) {
    if (!emitStmt(*sp))
      return false;
    if (B.GetInsertBlock()->getTerminator())
      break; // block already has terminator (return/break/continue)
  }
  return true;
}

bool FunctionEmitter::emitStmt(const Stmt &s) {
  if (auto *sb = std::get_if<StmtBlock>(&s.v)) {
    return emitBlock(*sb);
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

    // If using sret, store to the sret pointer and return void
    if (SretPtr) {
      v = castTo(v, RetTy);
      B.CreateStore(v, SretPtr);
      B.CreateRetVoid();
    } else {
      // cast return value
      v = castTo(v, RetTy);
      B.CreateRet(v);
    }
    return true;
  }
  if (auto *vd = std::get_if<StmtDecl>(&s.v)) {
    // Create alloca
    auto *ty = to_llvm_type(M.getContext(), vd->resolvedType);
    for (auto const &decl : vd->declarators) {
      auto *alloc = createAlloca(ty, decl.name);
      Locals[decl.name] = {alloc, vd->resolvedType, false};
      auto &init = decl.init;
      if (init && *init) {
        // Check if this is an init list - assign type from context
        if (auto *initList = std::get_if<ExprInitList>(&(*init)->v)) {
          // Zero-initialize first
          // TODO: Use memset for large arrays
          B.CreateStore(llvm::Constant::getNullValue(ty), alloc);

          // Handle struct initialization
          if (auto *structTy = llvm::dyn_cast<llvm::StructType>(ty)) {
            unsigned currentIndex = 0;
            for (auto &item : initList->values) {
              // Structs don't support designated init yet (Sema checks this)
              if (currentIndex >= structTy->getNumElements())
                break;

              auto *v = emitExpr(*item.value);
              if (!v)
                return false;

              auto *elemTy = structTy->getElementType(currentIndex);
              v = castTo(v, elemTy);
              auto *elemPtr =
                  B.CreateStructGEP(structTy, alloc, currentIndex,
                                    "gep." + std::to_string(currentIndex));
              B.CreateStore(v, elemPtr);
              currentIndex++;
            }
          } else if (ty->isArrayTy()) {
            // Array initialization
            auto *arrayTy = llvm::cast<llvm::ArrayType>(ty);
            uint64_t currentIndex = 0;
            for (auto &item : initList->values) {
              if (item.designator) {
                if (auto *idx = std::get_if<ExprInt>(&(*item.designator)->v)) {
                  currentIndex = idx->value;
                }
              }

              if (currentIndex >= arrayTy->getNumElements())
                continue; // Should be caught by Sema

              auto *v = emitExpr(*item.value);
              if (!v)
                return false;

              auto *elemTy = arrayTy->getElementType();
              v = castTo(v, elemTy);

              std::vector<llvm::Value *> indices;
              indices.push_back(llvm::ConstantInt::get(
                  llvm::Type::getInt32Ty(M.getContext()), 0));
              indices.push_back(llvm::ConstantInt::get(
                  llvm::Type::getInt32Ty(M.getContext()), currentIndex));

              auto *elemPtr = B.CreateGEP(
                  ty, alloc, indices, "gep." + std::to_string(currentIndex));
              B.CreateStore(v, elemPtr);
              currentIndex++;
            }
          } else if (initList->values.size() == 1) {
            // Single-value init list for non-struct/non-array type
            auto *v = emitExpr(*initList->values[0].value);
            if (v) {
              v = castTo(v, ty);
              B.CreateStore(v, alloc);
            } else {
              return false;
            }
          } else {
            llvm::errs() << "LLVM StmtDecl type: ";
            ty->print(llvm::errs());
            llvm::errs() << " isArray: " << ty->isArrayTy() << "\n";
            Err = "LLVM: initializer list with multiple values for scalar type";
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
  if (auto *fr = std::get_if<StmtFor>(&s.v)) {
    llvm::Function *TheFunction = B.GetInsertBlock()->getParent();
    llvm::BasicBlock *CondBB =
        llvm::BasicBlock::Create(M.getContext(), "for.cond", TheFunction);
    llvm::BasicBlock *LoopBB =
        llvm::BasicBlock::Create(M.getContext(), "for.body");
    llvm::BasicBlock *IncBB =
        llvm::BasicBlock::Create(M.getContext(), "for.inc");
    llvm::BasicBlock *AfterBB =
        llvm::BasicBlock::Create(M.getContext(), "for.end");

    // Emit init
    if (fr->init) {
      if (!emitStmt(**fr->init))
        return false;
    }

    B.CreateBr(CondBB);
    B.SetInsertPoint(CondBB);

    // Emit condition
    if (fr->cond) {
      auto *cond = emitExpr(**fr->cond);
      if (!cond)
        return false;
      cond = castTo(cond, llvm::Type::getInt1Ty(M.getContext()));
      B.CreateCondBr(cond, LoopBB, AfterBB);
    } else {
      // No condition = always true
      B.CreateBr(LoopBB);
    }

    TheFunction->insert(TheFunction->end(), LoopBB);
    B.SetInsertPoint(LoopBB);

    auto *OldBreak = BreakBB;
    auto *OldContinue = ContinueBB;
    BreakBB = AfterBB;
    ContinueBB = IncBB;

    // Emit body
    if (!emitStmt(*fr->body))
      return false;
    if (!B.GetInsertBlock()->getTerminator())
      B.CreateBr(IncBB);

    BreakBB = OldBreak;
    ContinueBB = OldContinue;

    // Emit increment
    TheFunction->insert(TheFunction->end(), IncBB);
    B.SetInsertPoint(IncBB);
    if (fr->iter) {
      if (!emitExpr(**fr->iter))
        return false;
    }
    B.CreateBr(CondBB);

    TheFunction->insert(TheFunction->end(), AfterBB);
    B.SetInsertPoint(AfterBB);
    return true;
  }
  if (auto *asmStmt = std::get_if<StmtAsm>(&s.v)) {
    // Emit inline assembly
    // LLVM inline assembly format: asm [volatile] ("assembly code" : outputs :
    // inputs : clobbers) For simplicity, we emit with no
    // outputs/inputs/clobbers
    llvm::FunctionType *asmTy =
        llvm::FunctionType::get(llvm::Type::getVoidTy(M.getContext()), false);
    llvm::InlineAsm *ia = llvm::InlineAsm::get(
        asmTy, asmStmt->code, "", /*hasSideEffects=*/asmStmt->isVolatile);
    B.CreateCall(asmTy, ia);
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
