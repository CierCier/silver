#include "agc/type.hpp"
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

  // Boolean -> Int
  // Must check this before generic Int<->Int to use ZExt instead of SExt
  if (To->isIntegerTy() && From->isIntegerTy(1)) {
    return B.CreateZExt(V, To);
  }

  // Int -> Boolean
  // Must check this before generic Int<->Int to use != 0 instead of Trunc
  if (To->isIntegerTy(1) && From->isIntegerTy()) {
    return B.CreateICmpNE(V, llvm::ConstantInt::get(From, 0));
  }

  // Int <-> Int
  if (To->isIntegerTy() && From->isIntegerTy()) {
    unsigned fw = From->getIntegerBitWidth();
    unsigned tw = To->getIntegerBitWidth();
    if (fw < tw)
      return B.CreateSExt(V, To);
    if (fw > tw)
      return B.CreateTrunc(V, To);
    return V;
  }

  // Float <-> Float
  if (To->isFloatingPointTy() && From->isFloatingPointTy()) {
    if (From->getTypeID() < To->getTypeID()) // e.g. float -> double
      return B.CreateFPExt(V, To);
    if (From->getTypeID() > To->getTypeID()) // e.g. double -> float
      return B.CreateFPTrunc(V, To);
    return V;
  }

  // Int -> Float
  if (To->isFloatingPointTy() && From->isIntegerTy()) {
    return B.CreateSIToFP(V, To);
  }

  // Float -> Int
  if (To->isIntegerTy() && From->isFloatingPointTy()) {
    return B.CreateFPToSI(V, To);
  }

  // Pointer <-> Pointer
  if (To->isPointerTy() && From->isPointerTy()) {
    return B.CreateBitCast(V, To);
  }

  // Fallback: bitcast int<->ptr if sizes match
  if (To->isPointerTy() && From->isIntegerTy()) {
    return B.CreateIntToPtr(V, To);
  }

  // Pointer -> Int
  if (To->isIntegerTy() && From->isPointerTy()) {
    return B.CreatePtrToInt(V, To);
  }

  // if we cant cast to the requested type
  // shoot self in foot
  return V;
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
        Type *t = it->second.type;
        if (t->isArray()) {
          // Array: use alloca directly, GEP 0, index
          basePtr = it->second.alloc;
          baseElemTy = to_llvm_type(M.getContext(), t); // [N x T]

          auto *idxVal = emitExpr(*idx->index);
          if (!idxVal)
            return nullptr;

          std::vector<llvm::Value *> indices;
          indices.push_back(llvm::ConstantInt::get(
              llvm::Type::getInt32Ty(M.getContext()), 0));
          indices.push_back(idxVal);

          if (outElemTy) {
            *outElemTy = to_llvm_type(M.getContext(),
                                      static_cast<ArrayType *>(t)->element());
          }
          return B.CreateGEP(baseElemTy, basePtr, indices, "gep");
        }

        // Pointer: load it
        auto *loaded = B.CreateLoad(it->second.alloc->getAllocatedType(),
                                    it->second.alloc, baseId->name + ".val");
        Type *elemT = t;
        if (t->isPointer()) {
          elemT = static_cast<PointerType *>(t)->pointee();
        }
        baseElemTy = to_llvm_type(M.getContext(), elemT);
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
      // Arrow operator: base is a pointer, emit it as value
      basePtr = emitExpr(*mem->base);
      if (!basePtr)
        return nullptr;
      // Get struct name from the base expression's type (which should be
      // pointer to struct)
      if (mem->base->type && mem->base->type->isPointer()) {
        auto *ptrType = static_cast<PointerType *>(mem->base->type);
        if (ptrType->pointee() && ptrType->pointee()->isStruct()) {
          structName = static_cast<StructType *>(ptrType->pointee())->name();
        }
      }
      // Fallback: try getting from local variable if identifier
      if (structName.empty()) {
        if (auto *id = std::get_if<ExprIdent>(&mem->base->v)) {
          auto it = Locals.find(id->name);
          if (it != Locals.end()) {
            if (it->second.type->isPointer()) {
              auto *ptrType = static_cast<PointerType *>(it->second.type);
              if (ptrType->pointee() && ptrType->pointee()->isStruct()) {
                structName =
                    static_cast<StructType *>(ptrType->pointee())->name();
              }
            }
          }
        }
      }
    } else {
      basePtr = emitLValue(*mem->base, &baseElemTy);
      if (!basePtr)
        return nullptr;
      // Get struct name from base expression's type
      if (mem->base->type && mem->base->type->isStruct()) {
        structName = static_cast<StructType *>(mem->base->type)->name();
      }
      // Fallback: try getting from local variable if identifier
      if (structName.empty()) {
        if (auto *id = std::get_if<ExprIdent>(&mem->base->v)) {
          auto it = Locals.find(id->name);
          if (it != Locals.end()) {
            if (it->second.type->isStruct()) {
              structName = static_cast<StructType *>(it->second.type)->name();
            }
          }
        }
      }
    }

    if (structName.empty() || !StructTypes) {
      Err = "cannot determine struct type for member access";
      return nullptr;
    }

    auto it = StructTypes->find(structName);
    if (it == StructTypes->end()) {
      Err = "unknown struct: " + structName;
      return nullptr;
    }

    if (!it->second->isStruct()) {
      Err = "type is not a struct: " + structName;
      return nullptr;
    }
    auto *st = static_cast<StructType *>(it->second);

    unsigned llvmIdx = 0;
    bool found = false;
    for (const auto &f : st->fields()) {
      if (f.name == mem->member) {
        found = true;
        break;
      }
      llvmIdx++;
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

    std::string calleeName =
        call->mangledCallee.empty() ? call->callee : call->mangledCallee;
    llvm::Function *calleeF = M.getFunction(calleeName);
    if (!calleeF) {
      Err = std::string("unknown function: ") + calleeName;
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
      if (it->second.type->isArray()) {
        // Decay array to pointer: GEP 0, 0
        llvm::Value *alloc = it->second.alloc;
        llvm::Type *arrayTy = to_llvm_type(M.getContext(), it->second.type);
        std::vector<llvm::Value *> indices;
        indices.push_back(
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(M.getContext()), 0));
        indices.push_back(
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(M.getContext()), 0));
        return B.CreateGEP(arrayTy, alloc, indices, "array.decay");
      }
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
      // Promote to float if one is float
      if (L->getType()->isFloatingPointTy()) {
        R = castTo(R, L->getType());
      } else if (R->getType()->isFloatingPointTy()) {
        L = castTo(L, R->getType());
      } else {
        // Assume int for now
        L = castTo(L, llvm::Type::getInt32Ty(M.getContext()));
        R = castTo(R, llvm::Type::getInt32Ty(M.getContext()));
      }
    }

    bool isFloat = L->getType()->isFloatingPointTy();

    switch (bin->op) {
    case TokenKind::Plus:
      return isFloat ? B.CreateFAdd(L, R, "addtmp")
                     : B.CreateAdd(L, R, "addtmp");
    case TokenKind::Minus:
      return isFloat ? B.CreateFSub(L, R, "subtmp")
                     : B.CreateSub(L, R, "subtmp");
    case TokenKind::Star:
      return isFloat ? B.CreateFMul(L, R, "multmp")
                     : B.CreateMul(L, R, "multmp");
    case TokenKind::Slash:
      return isFloat ? B.CreateFDiv(L, R, "divtmp")
                     : B.CreateSDiv(L, R, "divtmp");
    case TokenKind::Percent:
      return isFloat ? B.CreateFRem(L, R, "remtmp")
                     : B.CreateSRem(L, R, "remtmp");
    case TokenKind::Eq:
      return isFloat ? B.CreateFCmpOEQ(L, R, "eqtmp")
                     : B.CreateICmpEQ(L, R, "eqtmp");
    case TokenKind::Ne:
      return isFloat ? B.CreateFCmpONE(L, R, "netmp")
                     : B.CreateICmpNE(L, R, "netmp");
    case TokenKind::Lt:
      return isFloat ? B.CreateFCmpOLT(L, R, "lttmp")
                     : B.CreateICmpSLT(L, R, "lttmp");
    case TokenKind::Le:
      return isFloat ? B.CreateFCmpOLE(L, R, "letmp")
                     : B.CreateICmpSLE(L, R, "letmp");
    case TokenKind::Gt:
      return isFloat ? B.CreateFCmpOGT(L, R, "gttmp")
                     : B.CreateICmpSGT(L, R, "gttmp");
    case TokenKind::Ge:
      return isFloat ? B.CreateFCmpOGE(L, R, "getmp")
                     : B.CreateICmpSGE(L, R, "getmp");
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

    std::string calleeName =
        call->mangledCallee.empty() ? call->callee : call->mangledCallee;
    llvm::Function *calleeF = M.getFunction(calleeName);
    if (!calleeF) {
      Err = std::string("unknown function: ") + calleeName;
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
  if (auto *mc = std::get_if<ExprMethodCall>(&e.v)) {
    // Method call: base.method(args) -> method(base, args)
    if (Diags)
      Diags->report(DiagLevel::Debug,
                    "emitExpr for ExprMethodCall: " + mc->method);

    std::string calleeName = mc->mangledMethod;
    llvm::Function *calleeF = M.getFunction(calleeName);
    if (!calleeF) {
      Err = std::string("unknown method: ") + calleeName;
      return nullptr;
    }
    auto *FTy = calleeF->getFunctionType();

    std::vector<llvm::Value *> args;
    args.reserve(mc->args.size() + 1);

    // First arg is 'self' (the base)
    // Get the base value - for struct, we need to pass by value
    llvm::Type *baseElemTy = nullptr;
    llvm::Value *baseVal = nullptr;

    // Try to get base as lvalue first to load struct value
    llvm::Value *baseAddr = emitLValue(*mc->base, &baseElemTy);
    if (baseAddr && baseElemTy) {
      baseVal = B.CreateLoad(baseElemTy, baseAddr, "self");
    } else {
      // Fallback to emitExpr
      baseVal = emitExpr(*mc->base);
    }

    if (!baseVal) {
      Err = "failed to emit method receiver";
      return nullptr;
    }

    if (FTy->getNumParams() > 0) {
      baseVal = castTo(baseVal, FTy->getParamType(0));
    }
    args.push_back(baseVal);

    // Rest of the arguments
    for (size_t i = 0; i < mc->args.size(); ++i) {
      auto *v = emitExpr(*mc->args[i]);
      if (!v)
        return nullptr;
      if (i + 1 < FTy->getNumParams()) {
        v = castTo(v, FTy->getParamType((unsigned)(i + 1)));
      }
      args.push_back(v);
    }

    return B.CreateCall(calleeF, args);
  }
  if (auto *un = std::get_if<ExprUnary>(&e.v)) {
    // Handle increment/decrement separately since they need lvalue access
    if (un->op == TokenKind::PlusPlus || un->op == TokenKind::MinusMinus) {
      llvm::Type *elemTy = nullptr;
      auto *addr = emitLValue(*un->rhs, &elemTy);
      if (!addr || !elemTy) {
        Err = "increment/decrement requires an lvalue";
        return nullptr;
      }

      // Load current value
      auto *oldVal = B.CreateLoad(elemTy, addr, "incval");

      // Create the increment/decrement value
      llvm::Value *newVal = nullptr;
      if (elemTy->isIntegerTy()) {
        auto *one = llvm::ConstantInt::get(elemTy, 1);
        if (un->op == TokenKind::PlusPlus)
          newVal = B.CreateAdd(oldVal, one, "inctmp");
        else
          newVal = B.CreateSub(oldVal, one, "dectmp");
      } else if (elemTy->isFloatingPointTy()) {
        auto *one = llvm::ConstantFP::get(elemTy, 1.0);
        if (un->op == TokenKind::PlusPlus)
          newVal = B.CreateFAdd(oldVal, one, "finctmp");
        else
          newVal = B.CreateFSub(oldVal, one, "fdectmp");
      } else if (elemTy->isPointerTy()) {
        // Pointer arithmetic: increment by 1 element
        auto *one =
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(M.getContext()),
                                   un->op == TokenKind::PlusPlus ? 1 : -1);
        newVal = B.CreateGEP(llvm::Type::getInt8Ty(M.getContext()), oldVal, one,
                             "ptrinc");
      } else {
        Err = "invalid operand type for increment/decrement";
        return nullptr;
      }

      // Store the new value back
      B.CreateStore(newVal, addr);

      // Return the new value (pre-increment semantics)
      return newVal;
    }

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
  if (auto *cast = std::get_if<ExprCast>(&e.v)) {
    // Check if there's a custom cast function to call
    if (cast->customCastFunc) {
      // Call the custom cast function
      llvm::Function *castFunc = M.getFunction(*cast->customCastFunc);
      if (!castFunc) {
        Err = "custom cast function not found: " + *cast->customCastFunc;
        return nullptr;
      }

      // Emit the source expression as the argument
      auto *srcVal = emitExpr(*cast->expr);
      if (!srcVal)
        return nullptr;

      // If the source is a struct, we need to pass it by value
      // The cast function takes a struct value, not a pointer
      std::vector<llvm::Value *> args;
      args.push_back(srcVal);

      return B.CreateCall(castFunc, args, "cast.result");
    }

    // Primitive cast
    auto *val = emitExpr(*cast->expr);
    if (!val)
      return nullptr;
    llvm::Type *targetTy = to_llvm_type(M.getContext(), cast->target);
    return castTo(val, targetTy);
  }
  if (auto *initList = std::get_if<ExprInitList>(&e.v)) {
    // InitList requires a target type to be known (from context)
    // For now, we only support single-element init lists as a simple value
    if (initList->values.size() == 1) {
      return emitExpr(*initList->values[0].value);
    }
    Err = "initializer list with multiple values requires struct context";
    return nullptr;
  }
  if (auto *newExpr = std::get_if<ExprNew>(&e.v)) {
    // new<T>() - returns a zero-initialized value of type T
    llvm::Type *targetTy = to_llvm_type(M.getContext(), newExpr->targetType);
    if (!targetTy) {
      Err = "unknown type for new<T>()";
      return nullptr;
    }

    // For structs, we need to create an alloca, zero-initialize, and load
    if (targetTy->isStructTy()) {
      auto *alloc = createAlloca(targetTy, "new.tmp");
      // Zero-initialize the struct
      auto *zero = llvm::Constant::getNullValue(targetTy);
      B.CreateStore(zero, alloc);
      // Load and return the value
      return B.CreateLoad(targetTy, alloc, "new.val");
    }

    // For primitives, just return the zero value
    return llvm::Constant::getNullValue(targetTy);
  }
  if (auto *dropExpr = std::get_if<ExprDrop>(&e.v)) {
    // drop(val) - calls drop method if type has @trait(drop), otherwise no-op
    auto *operandVal = emitExpr(*dropExpr->operand);
    if (!operandVal)
      return nullptr;

    if (dropExpr->dropMethod) {
      // Call the drop method
      llvm::Function *dropFunc = M.getFunction(*dropExpr->dropMethod);
      if (!dropFunc) {
        Err = "drop method not found: " + *dropExpr->dropMethod;
        return nullptr;
      }

      std::vector<llvm::Value *> args;
      args.push_back(operandVal);
      B.CreateCall(dropFunc, args);
    }
    // drop() returns void, but we need to return something
    // Return undef of void type isn't allowed, so return i32 0
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(M.getContext()), 0);
  }
  if (auto *allocExpr = std::get_if<ExprAlloc>(&e.v)) {
    // alloc<T>() or alloc<T>(count) - heap allocate using malloc
    llvm::Type *targetTy = to_llvm_type(M.getContext(), allocExpr->targetType);
    if (!targetTy) {
      Err = "unknown type for alloc<T>()";
      return nullptr;
    }

    // Get or declare malloc function: void* malloc(size_t)
    llvm::FunctionCallee mallocFunc = M.getOrInsertFunction(
        "malloc", llvm::FunctionType::get(
                      llvm::PointerType::get(M.getContext(), 0),
                      {llvm::Type::getInt64Ty(M.getContext())}, false));

    // Calculate allocation size
    const llvm::DataLayout &DL = M.getDataLayout();
    uint64_t elemSize = DL.getTypeAllocSize(targetTy);
    llvm::Value *size = llvm::ConstantInt::get(
        llvm::Type::getInt64Ty(M.getContext()), elemSize);

    // If count is provided, multiply size by count
    if (allocExpr->count) {
      llvm::Value *countVal = emitExpr(**allocExpr->count);
      if (!countVal)
        return nullptr;
      // Extend count to i64 if needed
      if (countVal->getType()->getIntegerBitWidth() < 64) {
        countVal = B.CreateZExt(
            countVal, llvm::Type::getInt64Ty(M.getContext()), "count.ext");
      }
      size = B.CreateMul(size, countVal, "alloc.size");
    }

    // Call malloc
    llvm::Value *rawPtr = B.CreateCall(mallocFunc, {size}, "alloc.raw");

    // The result is already the right pointer type (opaque ptr in LLVM 15+)
    return rawPtr;
  }
  if (auto *freeExpr = std::get_if<ExprFree>(&e.v)) {
    // free(ptr) - deallocate using free()
    llvm::Value *ptrVal = emitExpr(*freeExpr->operand);
    if (!ptrVal)
      return nullptr;

    // Get or declare free function: void free(void*)
    llvm::FunctionCallee freeFunc = M.getOrInsertFunction(
        "free", llvm::FunctionType::get(
                    llvm::Type::getVoidTy(M.getContext()),
                    {llvm::PointerType::get(M.getContext(), 0)}, false));

    // Call free
    B.CreateCall(freeFunc, {ptrVal});

    // free() returns void, return i32 0 as placeholder
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(M.getContext()), 0);
  }
  Err = "unsupported expression";
  return nullptr;
}

} // namespace agc
