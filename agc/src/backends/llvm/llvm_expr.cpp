#include "llvm_emitter.hpp"
#include <variant>
#include <vector>

namespace agc {

llvm::Value *FunctionEmitter::castTo(llvm::Value *V, llvm::Type *To) {
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

llvm::Value *FunctionEmitter::emitLValue(const Expr &e,
                                         llvm::Type **outElemTy) {
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
        if (tn.pointerDepth > 0)
          tn.pointerDepth--;
        else if (!tn.arrayDims.empty())
          tn.arrayDims.pop_back();

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

    if (!basePtr)
      return nullptr;

    auto *idxVal = emitExpr(*idx->index);
    if (!idxVal)
      return nullptr;

    // GEP
    std::vector<llvm::Value *> indices;
    // If basePtr is pointer to array, we might need 0, idx
    // If basePtr is pointer to element, just idx
    // LLVM GEP: if pointer, just idx.
    indices.push_back(idxVal);

    if (outElemTy)
      *outElemTy = baseElemTy;
    return B.CreateGEP(baseElemTy, basePtr, indices, "gep");
  }
  if (auto *dr = std::get_if<ExprDeref>(&e.v)) {
    auto *ptr = emitExpr(*dr->operand);
    if (!ptr)
      return nullptr;
    // We need element type. Assume i8 for now if unknown
    if (outElemTy)
      *outElemTy = llvm::Type::getInt8Ty(M.getContext());
    return ptr;
  }
  if (auto *mem = std::get_if<ExprMember>(&e.v)) {
    if (Diags)
      Diags->report(DiagLevel::Debug,
                    "emitLValue for ExprMember: " + mem->member);
    llvm::Type *baseElemTy = nullptr;
    llvm::Value *basePtr = nullptr;
    std::string structName;

    if (mem->ptr) {
      basePtr = emitExpr(*mem->base);
      if (!basePtr)
        return nullptr;
      if (auto *id = std::get_if<ExprIdent>(&mem->base->v)) {
        auto it = Locals.find(id->name);
        if (it != Locals.end())
          structName = it->second.type.name;
      }
    } else {
      basePtr = emitLValue(*mem->base, &baseElemTy);
      if (!basePtr)
        return nullptr;
      if (auto *id = std::get_if<ExprIdent>(&mem->base->v)) {
        auto it = Locals.find(id->name);
        if (it != Locals.end())
          structName = it->second.type.name;
      }
    }

    if (structName.empty() || !Structs) {
      Err = "cannot determine struct type for member access";
      return nullptr;
    }

    auto it = Structs->find(structName);
    if (it == Structs->end()) {
      Err = "unknown struct: " + structName;
      return nullptr;
    }

    unsigned llvmIdx = 0;
    bool found = false;
    for (const auto &f : it->second.fields) {
      for (const auto &name : f.names) {
        if (name == mem->member) {
          found = true;
          break;
        }
        llvmIdx++;
      }
      if (found)
        break;
    }

    if (!found) {
      Err = "unknown field: " + mem->member;
      return nullptr;
    }

    std::vector<llvm::Value *> indices;
    indices.push_back(
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(M.getContext()), 0));
    indices.push_back(llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(M.getContext()), llvmIdx));

    llvm::Type *structTy =
        llvm::StructType::getTypeByName(M.getContext(), "struct." + structName);
    if (!structTy)
      structTy = llvm::StructType::getTypeByName(M.getContext(), structName);
    if (!structTy) {
      Err = "struct type not found: " + structName;
      return nullptr;
    }

    if (outElemTy) {
      if (auto *stTy = llvm::dyn_cast<llvm::StructType>(structTy)) {
        if (llvmIdx < stTy->getNumElements())
          *outElemTy = stTy->getElementType(llvmIdx);
      }
    }

    return B.CreateGEP(structTy, basePtr, indices, "gep." + mem->member);
  }
  Err = "expression is not an lvalue";
  return nullptr;
}

llvm::Value *FunctionEmitter::emitExpr(const Expr &e) {
  if (Diags)
    Diags->report(DiagLevel::Debug,
                  "emitExpr: variant index " + std::to_string(e.v.index()));
  if (auto *call = std::get_if<ExprCall>(&e.v)) {
    if (Diags && Diags->isVerbose()) {
      Diags->report(DiagLevel::Note,
                    "Emitting call to " + call->callee + " with " +
                        std::to_string(call->args.size()) + " args");
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
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(v)) {
          Diags->report(DiagLevel::Note,
                        "  Arg " + std::to_string(i) + ": " +
                            std::to_string(ci->getSExtValue()));
        } else {
          Diags->report(DiagLevel::Note,
                        "  Arg " + std::to_string(i) + ": non-constant");
        }
      }

      if (i < FTy->getNumParams()) {
        v = castTo(v, FTy->getParamType((unsigned)i));
      } else {
        // Variadic argument promotion
        if (v->getType()->isFloatTy()) {
          v = B.CreateFPExt(v, llvm::Type::getDoubleTy(M.getContext()));
        } else if (v->getType()->isIntegerTy(8) ||
                   v->getType()->isIntegerTy(16)) {
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
  if (auto *flit = std::get_if<ExprFloat>(&e.v)) {
    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(M.getContext()),
                                 flit->value);
  }
  if (auto *s = std::get_if<ExprStr>(&e.v)) {
    // Create global string
    auto *str = B.CreateGlobalString(s->value);
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
    if (elemTy)
      val = castTo(val, elemTy);

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
    if (Diags)
      Diags->report(DiagLevel::Debug, "emitExpr for ExprCall: " + call->callee);
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
        } else if (v->getType()->isIntegerTy(8) ||
                   v->getType()->isIntegerTy(16)) {
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
    llvm::Type *elemTy = nullptr;
    auto *addr = emitLValue(e, &elemTy);
    if (!addr || !elemTy)
      return nullptr;
    return B.CreateLoad(elemTy, addr, "memval");
  }
  if (auto *un = std::get_if<ExprUnary>(&e.v)) {
    auto *rhs = emitExpr(*un->rhs);
    if (!rhs)
      return nullptr;

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

} // namespace agc
