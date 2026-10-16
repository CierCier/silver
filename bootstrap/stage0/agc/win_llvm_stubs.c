/* No-op LLVM target initialization stubs for Windows.
 *
 * The official LLVM Windows installer ships LLVM-C.dll with only a subset of
 * targets (AArch64, ARM, BPF, NVPTX, RISCV, WebAssembly, X86), but inkwell
 * declares `LLVMInitialize*` externs for every LLVM target, so the final link
 * of `agc` fails with unresolved externals for the missing ones. These stubs
 * satisfy the linker; they are never called for targets Silver actually uses
 * (the all-targets init path goes through llvm-sys's target.c wrappers, which
 * enumerate only the targets really present in the DLL).
 */

#define STUB_TARGET(Name)                                                      \
    void LLVMInitialize##Name##TargetInfo(void) {}                             \
    void LLVMInitialize##Name##Target(void) {}                                 \
    void LLVMInitialize##Name##TargetMC(void) {}                               \
    void LLVMInitialize##Name##AsmParser(void) {}                              \
    void LLVMInitialize##Name##AsmPrinter(void) {}                             \
    void LLVMInitialize##Name##Disassembler(void) {}

STUB_TARGET(AMDGPU)
STUB_TARGET(ARC)
STUB_TARGET(AVR)
STUB_TARGET(CSKY)
STUB_TARGET(Hexagon)
STUB_TARGET(Lanai)
STUB_TARGET(LoongArch)
STUB_TARGET(M68k)
STUB_TARGET(Mips)
STUB_TARGET(MSP430)
STUB_TARGET(PowerPC)
STUB_TARGET(Sparc)
STUB_TARGET(SystemZ)
STUB_TARGET(VE)
STUB_TARGET(XCore)
STUB_TARGET(Xtensa)
