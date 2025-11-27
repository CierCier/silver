#include "agc/codegen.hpp"
#include "agc/diagnostics.hpp"
#include <algorithm>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <variant>
#include <iostream>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/TargetParser/Triple.h>

namespace agc {

namespace {

static llvm::Type *to_llvm_type(llvm::LLVMContext &ctx, const TypeName &t) {
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
  else
    ty = llvm::Type::getInt32Ty(ctx);

  // arrays decay to pointer for now (opaque pointers)
  if (!t.arrayDims.empty()) {
    ty = llvm::PointerType::getUnqual(ctx);
  }
  for (unsigned i = 0; i < t.pointerDepth; ++i) {
    ty = llvm::PointerType::getUnqual(ctx);
  }
  return ty;
}

static llvm::Constant *default_const_for(llvm::Type *ty) {
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
  for (auto const &[name, init] : v.declarators) {
    llvm::Constant *initConst = nullptr;
    if (init && (*init)) {
      // Only handle simple integer initializers for now
      if (auto *ei = std::get_if<ExprInt>(&((*init)->v))) {
        if (ty->isIntegerTy())
          initConst = llvm::ConstantInt::get(ty, ei->value);
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

// Helper to get or declare printf
static llvm::Function *ensure_printf(llvm::Module &M) {
  if (auto *F = M.getFunction("printf"))
    return F;
  auto &ctx = M.getContext();
  auto *i32Ty = llvm::Type::getInt32Ty(ctx);
  auto *ptrTy = llvm::PointerType::getUnqual(ctx);
  auto *FT = llvm::FunctionType::get(i32Ty, {ptrTy}, /*isVarArg*/ true);
  auto *F = llvm::Function::Create(FT, llvm::GlobalValue::ExternalLinkage,
                                   "printf", M);
  return F;
}

class FunctionEmitter {
  llvm::Module &M;
  llvm::Function *F;
  llvm::IRBuilder<> B;
  llvm::Type *RetTy;
  std::string &Err;

  struct VarInfo {
    llvm::AllocaInst *alloc;
    TypeName type;
    bool isConst{false};
  };
  std::unordered_map<std::string, VarInfo> Locals;

  // For break/continue in loops
  llvm::BasicBlock *BreakBB{nullptr};
  llvm::BasicBlock *ContinueBB{nullptr};

  // Create alloca in entry for stable lifetime
  llvm::AllocaInst *createAlloca(llvm::Type *ty, const std::string &name) {
    llvm::IRBuilder<> tmp(&F->getEntryBlock(), F->getEntryBlock().begin());
    return tmp.CreateAlloca(ty, nullptr, name);
  }

  // Basic casts to fit expected types
  llvm::Value *castTo(llvm::Value *V, llvm::Type *To) {
    if (!V)
      return nullptr;
    auto *From = V->getType();
    if (From == To)
      return V;
    if (To->isIntegerTy() && From->isIntegerTy()) {
      unsigned fw = From->getIntegerBitWidth();
      unsigned tw = To->getIntegerBitWidth();
      if (fw < tw)
        return B.CreateSExt(V, To);
      if (fw > tw)
        return B.CreateTrunc(V, To);
      return V;
    }
    if (To->isPointerTy() && From->isPointerTy()) {
      return B.CreateBitCast(V, To);
    }
    if (To->isIntegerTy(1) && From->isIntegerTy()) {
      return B.CreateICmpNE(V, llvm::ConstantInt::get(From, 0));
    }
    // Fallback: bitcast int<->ptr if sizes match
    if (To->isPointerTy() && From->isIntegerTy()) {
      return B.CreateIntToPtr(V, To);
    }
    if (To->isIntegerTy() && From->isPointerTy()) {
      return B.CreatePtrToInt(V, To);
    }
    return V; // best effort
  }

  // Try to get address for an lvalue expr (ident or index)
  llvm::Value *emitLValue(const Expr &e, llvm::Type **outElemTy = nullptr) {
    if (auto *id = std::get_if<ExprIdent>(&e.v)) {
      auto it = Locals.find(id->name);
      if (it != Locals.end()) {
        if (outElemTy)
          *outElemTy = it->second.alloc->getAllocatedType();
        return it->second.alloc;
      }
      if (auto *G = M.getGlobalVariable(id->name)) {
        if (outElemTy)
          *outElemTy = G->getValueType();
        return G;
      }
      // Could be a function name used in call; not an lvalue then
      Err = std::string("unknown identifier: ") + id->name;
      return nullptr;
    }
    if (auto *idx = std::get_if<ExprIndex>(&e.v)) {
      // base must be pointer; compute element address
      llvm::Type *baseElemTy = nullptr;
      llvm::Value *basePtr = nullptr;
      // If base is an identifier, try to load pointer value from local/global
      if (auto *baseId = std::get_if<ExprIdent>(&idx->base->v)) {
        auto it = Locals.find(baseId->name);
        if (it != Locals.end()) {
          // Load pointer value from local alloca
          auto *loaded = B.CreateLoad(it->second.alloc->getAllocatedType(),
                                      it->second.alloc, baseId->name + ".val");
          // Determine element type from declared TypeName (one level deref)
          TypeName tn = it->second.type;
          if (tn.pointerDepth > 0) tn.pointerDepth--;
          else if (!tn.arrayDims.empty()) tn.arrayDims.pop_back();
          
          baseElemTy = to_llvm_type(M.getContext(), tn);
          basePtr = loaded;
        } else if (auto *G = M.getGlobalVariable(baseId->name)) {
             // Global array/pointer
             // For now assume global is pointer
             basePtr = G; // This is pointer to global (pointer to pointer)
             baseElemTy = G->getValueType(); // This is pointer type
             // Need to load if it's a pointer variable?
             // Simplification: just use GEP on G if it's array
             // If G is global variable of type T, G is T*
             // If T is array, GEP works.
             // If T is pointer, we need to load.
             // Let's assume array for now or pointer.
             // TODO: proper type tracking for globals
             baseElemTy = llvm::Type::getInt32Ty(M.getContext()); // Hack
        }
      }
      
      if (!basePtr) {
          // Fallback: emit base expr
          basePtr = emitExpr(*idx->base);
          // We don't know type easily here without semantic analysis
          // Assume i32* for now
          baseElemTy = llvm::Type::getInt32Ty(M.getContext());
      }

      if (!basePtr) return nullptr;

      auto *idxVal = emitExpr(*idx->index);
      if (!idxVal) return nullptr;
      
      // GEP
      std::vector<llvm::Value*> indices;
      // If basePtr is pointer to array, we might need 0, idx
      // If basePtr is pointer to element, just idx
      // LLVM GEP: if pointer, just idx.
      indices.push_back(idxVal);
      
      if (outElemTy) *outElemTy = baseElemTy;
      return B.CreateGEP(baseElemTy, basePtr, indices, "gep");
    }
    if (auto *dr = std::get_if<ExprDeref>(&e.v)) {
      auto *ptr = emitExpr(*dr->operand);
      if (!ptr) return nullptr;
      // We need element type. Assume i8 for now if unknown
      if (outElemTy) *outElemTy = llvm::Type::getInt8Ty(M.getContext());
      return ptr;
    }
    Err = "expression is not an lvalue";
    return nullptr;
  }

  DiagnosticEngine* Diags{nullptr};

public:
  FunctionEmitter(llvm::Module &m, llvm::Function *f, std::string &err, DiagnosticEngine* diags)
      : M(m), F(f), B(m.getContext()), Err(err), Diags(diags) {
    RetTy = F->getReturnType();
    auto *entry = llvm::BasicBlock::Create(M.getContext(), "entry", F);
    B.SetInsertPoint(entry);
  }

  void initParams(const std::vector<Param> &params) {
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

  llvm::Value *emitExpr(const Expr &e) {
    if (auto *call = std::get_if<ExprCall>(&e.v)) {
      if (Diags && Diags->isVerbose()) {
          Diags->report(DiagLevel::Note, "Emitting call to " + call->callee + " with " + std::to_string(call->args.size()) + " args");
      }

      std::vector<llvm::Value *> args;
      args.reserve(call->args.size());
      
      llvm::Function *calleeF = M.getFunction(call->callee);
      if (!calleeF) {
        Err = std::string("unknown function: ") + call->callee;
        return nullptr;
      }
      auto *FTy = calleeF->getFunctionType();
      
      // Check arg count for non-variadic functions
      if (!calleeF->isVarArg() && call->args.size() != FTy->getNumParams()) {
        Err = "argument count mismatch for '" + call->callee + "'";
        return nullptr;
      }
      // For variadic, we need at least fixed args
      if (calleeF->isVarArg() && call->args.size() < FTy->getNumParams()) {
         Err = "too few arguments for variadic function '" + call->callee + "'";
         return nullptr;
      }

      for (size_t i = 0; i < call->args.size(); ++i) {
        auto *v = emitExpr(*call->args[i]);
        if (!v)
          return nullptr;

        if (Diags && Diags->isVerbose()) {
             if (auto* ci = llvm::dyn_cast<llvm::ConstantInt>(v)) {
                 Diags->report(DiagLevel::Note, "  Arg " + std::to_string(i) + ": " + std::to_string(ci->getSExtValue()));
             } else {
                 Diags->report(DiagLevel::Note, "  Arg " + std::to_string(i) + ": non-constant");
             }
        }
        
        if (i < FTy->getNumParams()) {
          v = castTo(v, FTy->getParamType((unsigned)i));
        } else {
             // Variadic argument promotion
             if (v->getType()->isFloatTy()) {
                 v = B.CreateFPExt(v, llvm::Type::getDoubleTy(M.getContext()));
             } else if (v->getType()->isIntegerTy(8) || v->getType()->isIntegerTy(16)) {
                 v = B.CreateSExt(v, llvm::Type::getInt32Ty(M.getContext()));
             }
        }
        args.push_back(v);
      }
      return B.CreateCall(calleeF, args);
    }
    if (auto *lit = std::get_if<ExprInt>(&e.v)) {
      return llvm::ConstantInt::get(llvm::Type::getInt32Ty(M.getContext()),
                                    lit->value);
    }
    if (auto *s = std::get_if<ExprStr>(&e.v)) {
      // Create global string
      auto *str = B.CreateGlobalStringPtr(s->value, "str");
      return str;
    }
    if (auto *id = std::get_if<ExprIdent>(&e.v)) {
      auto it = Locals.find(id->name);
      if (it != Locals.end()) {
        return B.CreateLoad(it->second.alloc->getAllocatedType(),
                            it->second.alloc, id->name);
      }
      // Check globals
      if (auto *G = M.getGlobalVariable(id->name)) {
        return B.CreateLoad(G->getValueType(), G, id->name);
      }
      Err = std::string("unknown identifier: ") + id->name;
      return nullptr;
    }
    if (auto *bin = std::get_if<ExprBinary>(&e.v)) {
      auto *L = emitExpr(*bin->lhs);
      auto *R = emitExpr(*bin->rhs);
      if (!L || !R)
        return nullptr;
      
      // Auto-cast to larger type (simple)
      if (L->getType() != R->getType()) {
          // Assume int for now
          L = castTo(L, llvm::Type::getInt32Ty(M.getContext()));
          R = castTo(R, llvm::Type::getInt32Ty(M.getContext()));
      }

      switch (bin->op) {
      case TokenKind::Plus:
        return B.CreateAdd(L, R, "addtmp");
      case TokenKind::Minus:
        return B.CreateSub(L, R, "subtmp");
      case TokenKind::Star:
        return B.CreateMul(L, R, "multmp");
      case TokenKind::Slash:
        return B.CreateSDiv(L, R, "divtmp");
      case TokenKind::Percent:
        return B.CreateSRem(L, R, "remtmp");
      case TokenKind::Eq:
        return B.CreateICmpEQ(L, R, "eqtmp");
      case TokenKind::Ne:
        return B.CreateICmpNE(L, R, "netmp");
      case TokenKind::Lt:
        return B.CreateICmpSLT(L, R, "lttmp");
      case TokenKind::Le:
        return B.CreateICmpSLE(L, R, "letmp");
      case TokenKind::Gt:
        return B.CreateICmpSGT(L, R, "gttmp");
      case TokenKind::Ge:
        return B.CreateICmpSGE(L, R, "getmp");
      case TokenKind::Amp:
        return B.CreateAnd(L, R, "andtmp");
      case TokenKind::Pipe:
        return B.CreateOr(L, R, "ortmp");
      case TokenKind::Caret:
        return B.CreateXor(L, R, "xortmp");
      case TokenKind::Shl:
        return B.CreateShl(L, R, "shltmp");
      case TokenKind::Shr:
        return B.CreateAShr(L, R, "ashrtmp"); // Arithmetic shift right for signed
      case TokenKind::AndAnd:
        // Logical AND (should be short-circuiting, but for now eager)
        // TODO: Implement short-circuiting
        return B.CreateAnd(L, R, "landtmp");
      case TokenKind::OrOr:
        // Logical OR
        // TODO: Implement short-circuiting
        return B.CreateOr(L, R, "lortmp");
      default:
        Err = "unsupported binary op";
        return nullptr;
      }
    }
    if (auto *assign = std::get_if<ExprAssign>(&e.v)) {
      llvm::Type *elemTy = nullptr;
      auto *addr = emitLValue(*assign->lhs, &elemTy);
      if (!addr)
        return nullptr;
      auto *val = emitExpr(*assign->rhs);
      if (!val)
        return nullptr;
      
      // Cast val to elemTy
      if (elemTy) val = castTo(val, elemTy);
      
      // Handle compound assignment
      llvm::Value *cur = nullptr;
      if (assign->op != TokenKind::Assign) {
          cur = B.CreateLoad(elemTy, addr, "loadass");
      }
      
      llvm::Value *toStore = val;
      switch (assign->op) {
      case TokenKind::Assign:
        toStore = val;
        break;
      case TokenKind::PlusAssign:
        toStore = B.CreateAdd(cur, val, "addas");
        break;
      case TokenKind::MinusAssign:
        toStore = B.CreateSub(cur, val, "subas");
        break;
      case TokenKind::StarAssign:
        toStore = B.CreateMul(cur, val, "mulas");
        break;
      case TokenKind::SlashAssign:
        toStore = B.CreateSDiv(cur, val, "divas");
        break;
      case TokenKind::PercentAssign:
        toStore = B.CreateSRem(cur, val, "remas");
        break;
      case TokenKind::ShlAssign:
        toStore = B.CreateShl(cur, val, "shlas");
        break;
      case TokenKind::ShrAssign:
        toStore = B.CreateAShr(cur, val, "shras");
        break;
      default:
        Err = "unsupported assignment op";
        return nullptr;
      }
      B.CreateStore(toStore, addr);
      return toStore;
    }
    if (auto *c = std::get_if<ExprCond>(&e.v)) {
      auto *cond = emitExpr(*c->cond);
      if (!cond)
        return nullptr;
      cond = castTo(cond, llvm::Type::getInt1Ty(M.getContext()));
      auto *tv = emitExpr(*c->thenE);
      auto *ev = emitExpr(*c->elseE);
      if (!tv || !ev)
        return nullptr;
      // untyped select: if mismatch and both pointers, cast else to then
      llvm::Type *ty = tv->getType();
      ev = castTo(ev, ty);
      return B.CreateSelect(cond, tv, ev, "sel");
    }
    if (auto *idx = std::get_if<ExprIndex>(&e.v)) {
      llvm::Type *elemTy = nullptr;
      auto *addr = emitLValue(e, &elemTy);
      if (!addr || !elemTy)
        return nullptr;
      return B.CreateLoad(elemTy, addr, "idxval");
    }
    if (auto *call = std::get_if<ExprCall>(&e.v)) {
      std::vector<llvm::Value *> args;
      args.reserve(call->args.size());
      
      llvm::Function *calleeF = M.getFunction(call->callee);
      if (!calleeF) {
        Err = std::string("unknown function: ") + call->callee;
        return nullptr;
      }
      auto *FTy = calleeF->getFunctionType();
      
      // Check arg count for non-variadic functions
      if (!calleeF->isVarArg() && call->args.size() != FTy->getNumParams()) {
        Err = "argument count mismatch for '" + call->callee + "'";
        return nullptr;
      }
      // For variadic, we need at least fixed args
      if (calleeF->isVarArg() && call->args.size() < FTy->getNumParams()) {
         Err = "too few arguments for variadic function '" + call->callee + "'";
         return nullptr;
      }

      for (size_t i = 0; i < call->args.size(); ++i) {
        auto *v = emitExpr(*call->args[i]);
        if (!v)
          return nullptr;
        
        if (i < FTy->getNumParams()) {
          v = castTo(v, FTy->getParamType((unsigned)i));
        } else {
             // Variadic argument promotion
             if (v->getType()->isFloatTy()) {
                 v = B.CreateFPExt(v, llvm::Type::getDoubleTy(M.getContext()));
             } else if (v->getType()->isIntegerTy(8) || v->getType()->isIntegerTy(16)) {
                 v = B.CreateSExt(v, llvm::Type::getInt32Ty(M.getContext()));
             }
        }
        args.push_back(v);
      }
      return B.CreateCall(calleeF, args);
    }
    if (auto *ct = std::get_if<ExprComptime>(&e.v)) {
      // Comptime: just emit the inner expression (constant folding would happen
      // earlier)
      return emitExpr(*ct->expr);
    }
    if (auto *ao = std::get_if<ExprAddressOf>(&e.v)) {
      // Address-of: return the address of the lvalue
      llvm::Type *elemTy = nullptr;
      auto *addr = emitLValue(*ao->operand, &elemTy);
      if (!addr) {
        Err = "cannot take address of non-lvalue";
        return nullptr;
      }
      return addr;
    }
    if (auto *dr = std::get_if<ExprDeref>(&e.v)) {
      // Dereference: load from pointer
      auto *ptrVal = emitExpr(*dr->operand);
      if (!ptrVal)
        return nullptr;
      if (!ptrVal->getType()->isPointerTy()) {
        Err = "cannot dereference non-pointer";
        return nullptr;
      }
      // For opaque pointers, we need to know the pointee type
      // Use i8 as default and let the user cast
      auto *elemTy = llvm::Type::getInt8Ty(M.getContext());
      return B.CreateLoad(elemTy, ptrVal, "deref");
    }
    if (auto *mem = std::get_if<ExprMember>(&e.v)) {
      // Member access - for now just error, struct support not complete
      Err = "member access not yet supported in LLVM backend";
      return nullptr;
    }
    if (auto *un = std::get_if<ExprUnary>(&e.v)) {
      auto *rhs = emitExpr(*un->rhs);
      if (!rhs) return nullptr;
      
      switch (un->op) {
      case TokenKind::Minus:
        if (rhs->getType()->isIntegerTy())
            return B.CreateNeg(rhs, "negtmp");
        if (rhs->getType()->isFloatingPointTy())
            return B.CreateFNeg(rhs, "fnegtmp");
        Err = "invalid operand type for unary minus";
        return nullptr;
      case TokenKind::Bang:
        // Logical NOT
        rhs = castTo(rhs, llvm::Type::getInt1Ty(M.getContext()));
        return B.CreateNot(rhs, "nottmp");
      case TokenKind::Tilde:
        // Bitwise NOT
        if (rhs->getType()->isIntegerTy())
            return B.CreateNot(rhs, "bitnottmp");
        Err = "invalid operand type for bitwise not";
        return nullptr;
      default:
        Err = "unsupported unary op";
        return nullptr;
      }
    }
    Err = "unsupported expression";
    return nullptr;
  }

  bool emitStmt(const Stmt &s) {
    if (auto *sb = std::get_if<StmtBlock>(&s.v)) {
      return emitBody(*sb);
    }
    if (auto *se = std::get_if<StmtExpr>(&s.v)) {
      if (!emitExpr(*se->expr)) return false;
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
      for (auto const &[name, init] : vd->declarators) {
        auto *alloc = createAlloca(ty, name);
        Locals[name] = {alloc, vd->type, false};
        if (init && *init) {
          auto *v = emitExpr(**init);
          if (v) {
            v = castTo(v, ty);
            B.CreateStore(v, alloc);
          } else {
             return false;
          }
        }
      }
      return true;
    }
    if (auto *iff = std::get_if<StmtIf>(&s.v)) {
      auto *cond = emitExpr(*iff->cond);
      if (!cond) return false;
      cond = castTo(cond, llvm::Type::getInt1Ty(M.getContext()));
      
      llvm::Function *TheFunction = B.GetInsertBlock()->getParent();
      llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(M.getContext(), "then", TheFunction);
      llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(M.getContext(), "else");
      llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(M.getContext(), "ifcont");

      bool hasElse = (iff->elseBranch.has_value());
      if (!hasElse) ElseBB = MergeBB;

      B.CreateCondBr(cond, ThenBB, ElseBB);

      B.SetInsertPoint(ThenBB);
      if (!emitStmt(*iff->thenBranch)) return false;
      if (!B.GetInsertBlock()->getTerminator())
        B.CreateBr(MergeBB);

      if (hasElse) {
        TheFunction->insert(TheFunction->end(), ElseBB);
        B.SetInsertPoint(ElseBB);
        if (!emitStmt(**iff->elseBranch)) return false;
        if (!B.GetInsertBlock()->getTerminator())
          B.CreateBr(MergeBB);
      }

      TheFunction->insert(TheFunction->end(), MergeBB);
      B.SetInsertPoint(MergeBB);
      return true;
    }
    if (auto *wh = std::get_if<StmtWhile>(&s.v)) {
      llvm::Function *TheFunction = B.GetInsertBlock()->getParent();
      llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(M.getContext(), "cond", TheFunction);
      llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(M.getContext(), "loop");
      llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(M.getContext(), "afterloop");

      B.CreateBr(CondBB);
      B.SetInsertPoint(CondBB);
      
      auto *cond = emitExpr(*wh->cond);
      if (!cond) return false;
      cond = castTo(cond, llvm::Type::getInt1Ty(M.getContext()));
      B.CreateCondBr(cond, LoopBB, AfterBB);

      TheFunction->insert(TheFunction->end(), LoopBB);
      B.SetInsertPoint(LoopBB);
      
      auto *OldBreak = BreakBB;
      auto *OldContinue = ContinueBB;
      BreakBB = AfterBB;
      ContinueBB = CondBB;

      if (!emitStmt(*wh->body)) return false;
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
    return true;
  }

  bool emitBody(const StmtBlock &body, bool isMainBody = false) {
    for (auto &sp : body.stmts) {
      if (!emitStmt(*sp)) return false;
      if (B.GetInsertBlock()->getTerminator())
        break; // function already returned
    }
    // If no terminator, add default return (ONLY for main function body)
    if (isMainBody && !B.GetInsertBlock()->getTerminator()) {
      if (RetTy->isVoidTy())
        B.CreateRetVoid();
      else
        B.CreateRet(default_const_for(RetTy));
    }
    return true;
  }
};

static llvm::Function* declare_function_llvm(const DeclFunc &f, llvm::Module &M) {
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

static bool emit_function_body_llvm(const DeclFunc &f, llvm::Module &M, std::string &err, DiagnosticEngine* diags) {
  if (!f.body) return true;
  
  auto *fn = M.getFunction(f.name);
  if (!fn) {
      err = "Function not found: " + f.name;
      return false;
  }

  std::string localErr;
  FunctionEmitter FE(M, fn, localErr, diags);
  FE.initParams(f.params);
  if (!FE.emitBody(*f.body, true)) {
      err = localErr;
      return false;
  }
  return true;
}

} // namespace

class LlvmBackend : public CodegenBackend {
public:
  std::string_view name() const override { return "llvm"; }
  bool generate(const Program &prog, std::ostream &os, std::string &err,
                const CodegenOptions &opts) override {
    DiagnosticEngine* diags = opts.diags;
    if (diags && diags->isVerbose()) {
        diags->report(DiagLevel::Note, "Generating LLVM IR...");
    }
    llvm::LLVMContext ctx;
    llvm::Module module("silver_module", ctx);

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

    // Globals first
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *v = std::get_if<DeclVar>(&d.v))
        emit_global_llvm(*v, module);
    }

    // Prototypes without definitions (MUST be before definitions for calls to work)
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
          if (!emit_function_body_llvm(*f, module, err, diags)) return false;
      }
    }

    // Verify module (non-fatal for now)
    llvm::raw_null_ostream nulls;
    if (llvm::verifyModule(module, &nulls)) {
      err = "LLVM verification reported issues";
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
    DiagnosticEngine* diags = opts.diags;
    if (diags && diags->isVerbose()) {
        diags->report(DiagLevel::Note, "Emitting object file: " + filename);
    }
    llvm::LLVMContext ctx;
    llvm::Module module("silver_module", ctx);

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


    // Globals first
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      if (auto *v = std::get_if<DeclVar>(&d.v))
        emit_global_llvm(*v, module);
    }

    // Prototypes without definitions (MUST be before definitions for calls to work)
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
          if (!emit_function_body_llvm(*f, module, err, diags)) return false;
      }
    }

    // Initialize targets
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto targetTripleStr = llvm::sys::getDefaultTargetTriple();
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
    
    auto targetMachine = target->createTargetMachine(targetTripleStr, cpu, features, opt, rm);
    
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
