import llvmlite.ir as ir
import llvmlite.binding as llvm

import llvmlite.utils as utils

llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()


import llvmlite.ir as ir

int8 = ir.IntType(8)
int32 = ir.IntType(32)

ctx = ir.global_context
book_t = ctx.get_identified_type("struct.Book")
book_t.set_body(int8, int8)

m = ir.Module()
f = ir.Function(m, ir.FunctionType(int32, []), "main")
bldr = ir.IRBuilder(f.append_basic_block())

o = bldr.alloca(book_t)
book_ptr_0 = bldr.gep(o, [int32(0), int32(0)], inbounds=True)
bldr.store(int8(5), book_ptr_0, align=1)
book_ptr_1 = bldr.gep(o, [int32(0), int32(1)], inbounds=True)
bldr.store(int8(4), book_ptr_1, align=1)
bldr.ret(int32(0))


print(m)
