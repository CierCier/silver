import llvmlite.ir as ir
import llvmlite.binding as llvm

import llvmlite.utils as utils

llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()

module = ir.Module(name="test")

fn_foo_type = ir.FunctionType(ir.IntType(32), [ir.IntType(32), ir.IntType(32)])

fn_foo = ir.Function(module, fn_foo_type, name="foo")

builder = ir.IRBuilder()

bb_entry = fn_foo.append_basic_block(name="entry")
builder.position_at_end(bb_entry)


stack_int_ptr = builder.alloca(ir.IntType(32), name="stack_int")
builder.store(ir.Constant(stack_int_ptr.type.pointee, 0xD09), stack_int_ptr)

stack_int = builder.load(stack_int_ptr)

add_int = builder.add(stack_int, fn_foo.args[0])
mult_int = builder.mul(add_int, fn_foo.args[1])

builder.ret(mult_int)


print(module)
