import llvmlite

from llvmlite import ir
from llvmlite.ir import (
    Module,
    Function,
    FunctionType,
    IRBuilder,
    Constant,
    GlobalVariable,
    Value,
    Type,
)

from silver.parser import (
    Program,
    Block,
    Statement,
    Expr,
    ExprStmt,
    VariableDecl,
    IfStmt,
    ReturnStmt,
    BinOp,
    FunctionCall,
    Identifier,
    Number,
    ParenExpr,
    String,
)

from typing import Dict, List, Optional


def get_data_layout(triple: str):
    if triple == "x86_64-pc-linux-gnu":
        return "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
    elif triple == "aarch64-apple-macosx":
        return "e-m:o-i64:64-i128:128-n32:64-S128"
    else:
        raise Exception(f"Unsupported triple: {triple}")


class IRGenerator:

    def __init__(self, triple: str):
        self.module = Module("Silver")
        self.builder: Optional[IRBuilder] = None
        self.named_values: Dict[str, ir.Value] = {}
        self.functions: Dict[str, Function] = {}
        self.string_counter = (
            0  # Counter for unique string names `.str.0`, `.str.1`, etc.
        )

        self.module.triple = triple
        self.module.data_layout = get_data_layout(triple)

    def declare_external_global(self, name: str, type: ir.Type):
        """Declare an external global variable."""
        global_var = ir.GlobalVariable(self.module, type, name)
        global_var.linkage = "external"
        return global_var

    def declare_external_function(
        self,
        name: str,
        return_type: ir.Type,
        param_types: List[ir.Type],
        var_arg: bool = False,
    ) -> Function:
        """Declare an external function if it doesn't already exist.

        Args:
            name: The name of the function
            return_type: The return type of the function
            param_types: List of parameter types
            var_arg: Whether the function is variadic

        Returns:
            The declared function
        """
        if name in self.functions:
            return self.functions[name]

        func_type = FunctionType(return_type, param_types, var_arg=var_arg)
        func = Function(self.module, func_type, name)
        func.linkage = "external"

        # This just names the variables in the IR for debugging purposes
        for i, param in enumerate(func.args):
            param.name = f"arg{i}"

        self.functions[name] = func
        return func

    def generate(self, program) -> str:

        for function in program.functions:
            self.generate_function(function)

        for global_var in program.globals:
            self.generate_global(global_var)

        return str(self.module)

    def generate_function(self, function):
        param_types = []
        is_variadic = False
        for param in function.parameters:
            if param.name == "...":
                is_variadic = True
                break
            param_type = self.get_llvm_type(param.param_type)
            param_types.append(param_type)

        return_type = self.get_llvm_type(function.return_type)
        func_type = FunctionType(return_type, param_types)

        # This is what actually creates the function declaration in the IR
        func = Function(self.module, func_type, function.name)

        if is_variadic:
            func_type.var_arg = True

        self.functions[function.name] = func

        # return if we're in a declaration and not a definition
        if function.body is None:
            return

        ## Start the function body
        entry_block = func.append_basic_block("entry")
        self.builder = IRBuilder(entry_block)

        # Allocate space for the parameters
        for param, arg in zip(function.parameters, func.args):
            param_name = param.name
            param_type = self.get_llvm_type(param.param_type)
            alloca = self.builder.alloca(param_type, name=param_name)
            self.builder.store(arg, alloca)
            self.named_values[param_name] = alloca

        ## Generate code for the function body
        for stmt in function.body.statements:
            self.generate_statement(stmt)

        ## Add a return if needed
        if not self.builder.block.is_terminated:
            if return_type == ir.VoidType():
                self.builder.ret_void()
            else:
                self.builder.ret(Constant(return_type, 0))

    def generate_global(self, global_var):
        # Create global variable
        var_type = self.get_llvm_type(global_var.var_type)
        initializer = self.generate_expression(global_var.value)
        global_var = ir.GlobalVariable(self.module, var_type, global_var.name)
        global_var.initializer = initializer

    def generate_statement(self, stmt):
        if isinstance(stmt, VariableDecl):
            self.generate_variable_decl(stmt)
        elif isinstance(stmt, IfStmt):
            self.generate_if_stmt(stmt)
        elif isinstance(stmt, ReturnStmt):
            self.generate_return_stmt(stmt)
        elif isinstance(stmt, ExprStmt):
            self.generate_expression(stmt.expr)

    def generate_variable_decl(self, decl: VariableDecl):
        # Allocate space for the variable

        var_type = self.get_llvm_type(decl.var_type)
        alloca = self.builder.alloca(var_type, name=decl.name)

        # Generate code for the initial value
        value = self.generate_expression(decl.value)

        # Guess the type of the value (probable an expression that needs to be evaluated)
        value_type = self.guess_type(decl.value)

        # Cast the value to match the pointed-to type if needed
        pointed_type = alloca.type.pointee

        if self.needs_cast(value_type, pointed_type):
            value = self.cast_value(value, pointed_type)

        # Store the value
        self.builder.store(value, alloca)

        # Add to named values
        self.named_values[decl.name] = alloca

    def generate_if_stmt(self, stmt):
        # Generate condition
        cond = self.generate_expression(stmt.condition)

        # Create blocks
        then_block = self.builder.append_basic_block("then")
        else_block = self.builder.append_basic_block("else")
        merge_block = self.builder.append_basic_block("ifcont")

        # Create branch
        self.builder.cbranch(cond, then_block, else_block)

        # Generate then block
        self.builder.position_at_end(then_block)
        for stmt in stmt.body.statements:
            self.generate_statement(stmt)
        if not self.builder.block.is_terminated:
            self.builder.branch(merge_block)

        # Generate else block
        self.builder.position_at_end(else_block)
        if not self.builder.block.is_terminated:
            self.builder.branch(merge_block)

        # Continue at merge block
        self.builder.position_at_end(merge_block)

    def generate_return_stmt(self, stmt):
        if stmt.value is None:
            self.builder.ret_void()
        else:
            value = self.generate_expression(stmt.value)
            self.builder.ret(value)

    def generate_expression(self, expr):
        if isinstance(expr, Identifier):
            # Load the value from the alloca
            alloca = self.named_values[expr.name]
            return self.builder.load(alloca)
        elif isinstance(expr, Number):
            # Determine the type based on the number format
            if "." in expr.value:
                # Floating point number
                value = float(expr.value)
                if "f" in expr.value.lower():
                    return Constant(ir.FloatType(), value)
                else:
                    return Constant(ir.DoubleType(), value)
            else:
                # Integer
                value = int(expr.value)
                if value < 0:
                    return Constant(ir.IntType(32), value)
                else:
                    return Constant(ir.IntType(32), value)
        elif isinstance(expr, String):
            # Create a global string constant with a unique name
            str_name = f".str.{self.string_counter}"
            self.string_counter += 1

            # Create the string constant with proper null termination
            str_val = ir.Constant(
                ir.ArrayType(ir.IntType(8), len(expr.value) + 1),
                bytearray(expr.value.encode() + b"\0"),
            )
            str_global = ir.GlobalVariable(self.module, str_val.type, name=str_name)
            str_global.initializer = str_val
            str_global.global_constant = True
            str_global.linkage = "private"

            # Get pointer to the string
            zero = Constant(ir.IntType(32), 0)
            str_ptr = self.builder.gep(str_global, [zero, zero])
            return str_ptr
        elif isinstance(expr, BinOp):
            left = self.generate_expression(expr.left)
            right = self.generate_expression(expr.right)

            # Handle type promotion for binary operations
            if self.needs_cast(left.type, right.type):
                if isinstance(left.type, ir.FloatType) or isinstance(
                    right.type, ir.FloatType
                ):
                    # Promote to float if either operand is float
                    if isinstance(left.type, ir.FloatType):
                        right = self.cast_value(right, left.type)
                    else:
                        left = self.cast_value(left, right.type)
                else:
                    # Promote to larger integer type
                    if left.type.width < right.type.width:
                        left = self.cast_value(left, right.type)
                    else:
                        right = self.cast_value(right, left.type)

            if expr.op == "+":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fadd(left, right)
                else:
                    return self.builder.add(left, right)
            elif expr.op == "-":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fsub(left, right)
                else:
                    return self.builder.sub(left, right)
            elif expr.op == "*":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fmul(left, right)
                else:
                    return self.builder.mul(left, right)
            elif expr.op == "/":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fdiv(left, right)
                else:
                    return self.builder.sdiv(left, right)
            elif expr.op == "==":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered("==", left, right)
                else:
                    return self.builder.icmp_signed("==", left, right)
            elif expr.op == "!=":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered("!=", left, right)
                else:
                    return self.builder.icmp_signed("!=", left, right)
            elif expr.op == "<":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered("<", left, right)
                else:
                    return self.builder.icmp_signed("<", left, right)
            elif expr.op == "<=":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered("<=", left, right)
                else:
                    return self.builder.icmp_signed("<=", left, right)
            elif expr.op == ">":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered(">", left, right)
                else:
                    return self.builder.icmp_signed(">", left, right)
            elif expr.op == ">=":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered(">=", left, right)
                else:
                    return self.builder.icmp_signed(">=", left, right)
        elif isinstance(expr, FunctionCall):
            # Get the function
            func = self.functions[expr.name]

            # Special handling for printf
            if expr.name == "printf":
                # First argument is the format string
                format_str = self.generate_expression(expr.args[0])

                # Generate remaining arguments
                args = [format_str]
                for arg in expr.args[1:]:
                    arg_value = self.generate_expression(arg)
                    # Ensure integer arguments are at least 32 bits
                    if (
                        isinstance(arg_value.type, ir.IntType)
                        and arg_value.type.width < 32
                    ):
                        arg_value = self.builder.zext(arg_value, ir.IntType(32))
                    args.append(arg_value)

                return self.builder.call(func, args)
            else:
                # Generate arguments for other functions
                args = []
                for arg in expr.args:
                    arg_value = self.generate_expression(arg)
                    args.append(arg_value)
                return self.builder.call(func, args)
        elif isinstance(expr, ParenExpr):
            return self.generate_expression(expr.expr)

        raise Exception(f"Unsupported expression type: {type(expr)}")

    def get_llvm_type(self, type_node):
        type_name = type_node.name
        if type_name == "i8":
            return ir.IntType(8)
        elif type_name == "i16":
            return ir.IntType(16)
        elif type_name == "i32":
            return ir.IntType(32)
        elif type_name == "i64":
            return ir.IntType(64)
        elif type_name == "u8":
            return ir.IntType(8)
        elif type_name == "u16":
            return ir.IntType(16)
        elif type_name == "u32":
            return ir.IntType(32)
        elif type_name == "u64":
            return ir.IntType(64)
        elif type_name == "f32":
            return ir.FloatType()
        elif type_name == "f64":
            return ir.DoubleType()
        elif type_name == "void":
            return ir.VoidType()
        elif type_name == "string":
            return ir.PointerType(ir.IntType(8))
        else:
            ## pointer type
            return ir.PointerType(ir.IntType(8))

    def needs_cast(self, from_type: ir.Type, to_type: ir.Type) -> bool:
        """Check if a cast is needed between two types."""
        if from_type == to_type:
            return False

        # Both types are integers
        if isinstance(from_type, ir.IntType) and isinstance(to_type, ir.IntType):
            return from_type.width != to_type.width

        # Both types are floating point
        if isinstance(from_type, ir.FloatType) and isinstance(to_type, ir.FloatType):
            return True  # Always cast between float types

        # Integer to float or float to integer
        if (
            isinstance(from_type, ir.IntType) and isinstance(to_type, ir.FloatType)
        ) or (isinstance(from_type, ir.FloatType) and isinstance(to_type, ir.IntType)):
            return True

        # Integer to Double
        if isinstance(from_type, ir.IntType) and isinstance(to_type, ir.DoubleType):
            return True

        # Double to Integer
        if isinstance(from_type, ir.DoubleType) and isinstance(to_type, ir.IntType):
            return True

        # Pointer to pointer
        if isinstance(from_type, ir.PointerType) and isinstance(
            to_type, ir.PointerType
        ):
            return True

        # Pointer to integer
        if isinstance(from_type, ir.PointerType) and isinstance(to_type, ir.IntType):
            return True

        # Integer to pointer
        if isinstance(from_type, ir.IntType) and isinstance(to_type, ir.PointerType):
            return True

        return False

    def cast_value(self, value: ir.Value, to_type: ir.Type) -> ir.Value:
        """Cast a value to a different type."""
        from_type = value.type

        if not self.needs_cast(from_type, to_type):
            return value

        # Integer to integer
        if isinstance(from_type, ir.IntType) and isinstance(to_type, ir.IntType):
            if from_type.width < to_type.width:
                return self.builder.zext(value, to_type)  # Zero extend
            else:
                return self.builder.trunc(value, to_type)  # Truncate

        # Float to float
        if isinstance(from_type, ir.FloatType) and isinstance(to_type, ir.FloatType):
            if isinstance(from_type, ir.FloatType) and isinstance(
                to_type, ir.DoubleType
            ):
                return self.builder.fpext(value, to_type)  # Float to double
            else:
                return self.builder.fptrunc(value, to_type)  # Double to float

        # Integer to float
        if isinstance(from_type, ir.IntType) and isinstance(to_type, ir.FloatType):
            if from_type.signed:
                return self.builder.sitofp(value, to_type)  # Signed int to float
            else:
                return self.builder.uitofp(value, to_type)  # Unsigned int to float

        # Float to integer
        if isinstance(from_type, ir.FloatType) and isinstance(to_type, ir.IntType):
            if to_type.signed:
                return self.builder.fptosi(value, to_type)  # Float to signed int
            else:
                return self.builder.fptoui(value, to_type)  # Float to unsigned int

        # Double to Double
        if isinstance(from_type, ir.DoubleType) and isinstance(to_type, ir.DoubleType):
            return value

        # Double to Float
        if isinstance(from_type, ir.DoubleType) and isinstance(to_type, ir.FloatType):
            return self.builder.fptrunc(value, to_type)  # Double to float

        # Double to Integer
        if isinstance(from_type, ir.DoubleType) and isinstance(to_type, ir.IntType):
            return self.builder.fptosi(value, to_type)  # Double to signed int

        # Double to Pointer
        if isinstance(from_type, ir.DoubleType) and isinstance(to_type, ir.PointerType):
            return self.builder.ptrtoint(value, to_type)  # Double to pointer

        # Float to Pointer
        if isinstance(from_type, ir.FloatType) and isinstance(to_type, ir.PointerType):
            return self.builder.ptrtoint(value, to_type)  # Float to pointer

        # Integer to Pointer
        if isinstance(from_type, ir.IntType) and isinstance(to_type, ir.PointerType):
            return self.builder.inttoptr(value, to_type)  # Integer to pointer

        # Pointer to Pointer
        if isinstance(from_type, ir.PointerType) and isinstance(
            to_type, ir.PointerType
        ):
            return value

        raise Exception(f"Cannot cast from {from_type} to {to_type}")

    def guess_type(self, expr: Expr) -> ir.Type:
        """Guess the LLVM type of an expression.

        Args:
            expr: The expression to guess the type of

        Returns:
            The guessed LLVM type
        """
        if isinstance(expr, Number):
            # Check if it's a floating point number
            if "." in expr.value:
                if "f" in expr.value.lower():
                    return ir.FloatType()
                return ir.DoubleType()
            # Check if it's a negative number
            if expr.value.startswith("-"):
                return ir.IntType(32)  # Default to signed
            return ir.IntType(32)  # Default to signed

        elif isinstance(expr, String):
            return ir.PointerType(ir.IntType(8))  # char*

        elif isinstance(expr, Identifier):
            # Look up the type in our symbol table
            if expr.name in self.named_values:
                return self.named_values[expr.name].type.pointee
            # Default to i32 if not found
            return ir.IntType(32)

        elif isinstance(expr, BinOp):
            # Get types of operands
            left_type = self.guess_type(expr.left)
            right_type = self.guess_type(expr.right)

            # Handle arithmetic operations
            if expr.op in ["+", "-", "*", "/"]:
                # If either operand is float, result is float
                if isinstance(left_type, (ir.FloatType, ir.DoubleType)) or isinstance(
                    right_type, (ir.FloatType, ir.DoubleType)
                ):
                    # Promote to double if either is double
                    if isinstance(left_type, ir.DoubleType) or isinstance(
                        right_type, ir.DoubleType
                    ):
                        return ir.DoubleType()
                    return ir.FloatType()
                # Both are integers, result is integer
                # Use the larger of the two types
                if isinstance(left_type, ir.IntType) and isinstance(
                    right_type, ir.IntType
                ):
                    return ir.IntType(max(left_type.width, right_type.width))
                return ir.IntType(32)  # Default to i32

            # Handle comparison operations
            elif expr.op in ["==", "!=", "<", "<=", ">", ">="]:
                return ir.IntType(1)  # Boolean result

        elif isinstance(expr, FunctionCall):
            # Look up the function's return type
            if expr.name in self.functions:
                return self.functions[expr.name].return_value.type
            # Default to i32 if not found
            return ir.IntType(32)

        elif isinstance(expr, ParenExpr):
            return self.guess_type(expr.expr)

        # Default to i32 if we can't determine the type
        return ir.IntType(32)


def test():
    import sys
    from pathlib import Path

    from tokenizer import Tokenizer
    from preprocess import Preprocessor
    from parser import Parser

    files = [Path(x) for x in sys.argv[1:]]
    preprocessor = Preprocessor(*files)

    data = preprocessor.preprocess()

    tokenizer = Tokenizer(data)

    tokens = tokenizer.get_all()

    parser = Parser(tokens)

    program = parser.parse()

    ir_generator = IRGenerator()

    ir_code = ir_generator.generate(program)

    print(ir_code)


if __name__ == "__main__":
    test()
