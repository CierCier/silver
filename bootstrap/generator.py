from dataclasses import dataclass
from typing import List
from parser import (
    ExternalDeclarationNode,
    Parser,
    ASTNode,
    IfNode,
    TypeNode,
    BlockNode,
    WhileNode,
    ReturnNode,
    LiteralNode,
    AssignmentNode,
    FunctionCallNode,
    FunctionDeclarationNode,
    ExpressionNode,
    IdentifierNode,
    FunctionCallNode,
    FunctionDeclarationNode,
    VariableDeclarationNode,
)

from llvmlite import ir, utils, binding

from lang98 import Type


@dataclass
class Level:
    variables: dict
    functions: dict

    def __init__(self):
        self.variables = {}
        self.functions = {}

    def add_variable(self, name: str, var_type: Type):
        if name in self.variables:
            raise Exception(f"Variable {name} already declared")
        self.variables[name] = var_type

    def add_function(self, name: str, parameters: List[Type], return_type: Type):
        if name in self.functions:
            raise Exception(f"Function {name} already declared")
        self.functions[name] = (parameters, return_type)

    def get_variable(self, name: str):
        """Returns the variable type if it exists, otherwise None"""
        if name not in self.variables:
            return None
        return self.variables[name]

    def get_function(self, name: str, parameters: List[Type]):
        """Returns the function type if it exists, otherwise None
        Functions are stored as a tuple of (parameters, return_type)
        """
        funcs = [f for f in self.functions if f == name]
        if not funcs:
            return None

        # now we match the parameters, just the signature
        # we don't check the return type
        # there's probably a better way to do this
        for func in funcs:
            params, ret = self.functions[func]
            if len(params) != len(parameters):
                continue
            if all(p == t for p, t in zip(params, parameters)):
                return ret
        return None


def get_machine_target():
    import platform
    import sys

    if sys.platform.startswith("linux"):
        if platform.machine() == "x86_64":
            return "x86_64-pc-linux-gnu"
        elif platform.machine() == "aarch64":
            return "aarch64-pc-linux-gnu"
        else:
            raise Exception(f"Unsupported architecture: {platform.machine()}")
    elif sys.platform.startswith("darwin"):
        if platform.machine() == "x86_64":
            return "x86_64-apple-darwin"
        elif platform.machine() == "arm64":
            return "aarch64-apple-darwin"
        else:
            raise Exception(f"Unsupported architecture: {platform.machine()}")
    elif sys.platform.startswith("win32"):
        if platform.machine() == "AMD64":
            return "x86_64-pc-windows-msvc"
        elif platform.machine() == "ARM64":
            return "aarch64-pc-windows-msvc"
        else:
            raise Exception(f"Unsupported architecture: {platform.machine()}")
    else:
        raise Exception(f"Unsupported platform: {sys.platform}")


class Generator:
    def __init__(self, ast: ASTNode, target: str = get_machine_target()):
        self.ast = ast
        self.level_stack = [
            Level()  # we only use these to verify the scope of the variables
        ]
        self.level = 0  # start at global

        self.module = ir.Module(name="lang98")
        self.module.triple = target

        # generates position independent code
        self.module.data_layout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"

        self.builder = None
        self.named_values = {}  # map of variable names to LLVM values

        self.string_literals = {}  # cache for string literals
        self.string_counter = 0  # counter for string literals
        self.string_var = None  # current string variable

    def get_variable(self, name: str):
        """Returns the variable type if it exists, otherwise None"""
        # we only check the current level and the ones above it
        for i in range(self.level, -1, -1):
            var = self.level_stack[i].get_variable(name)
            if var:
                return var
        return None

    def get_function(self, name: str, parameters: List[Type]):
        """Returns the function type if it exists, otherwise None
        Functions are stored as a tuple of (parameters, return_type)
        """
        # we only check the current level and the ones above it
        for i in range(self.level, -1, -1):
            func = self.level_stack[i].get_function(name, parameters)
            if func:
                return func
        return None

    def add_variable(self, name: str, var_type: Type):
        """Adds a variable to the current level"""
        self.level_stack[self.level].add_variable(name, var_type)

    def add_function(self, name: str, parameters: List[Type], return_type: Type):
        """Adds a function to the current level"""
        self.level_stack[self.level].add_function(name, parameters, return_type)

    def generate_start(self):
        """Main entry point for code generation"""
        # First pass: process all function declarations
        for node in self.ast.statements:
            if isinstance(node, FunctionDeclarationNode):
                self.generate_function_declaration(node)
            elif isinstance(node, ExternalDeclarationNode):
                # Handle external function declarations
                self.generate_external_function(node)

        # Second pass: generate function bodies
        for node in self.ast.statements:
            if isinstance(node, FunctionDeclarationNode):
                self.generate_function_body(node)

        return str(self.module)

    def generate_function_declaration(self, func_node):
        """Generate function signature without body"""
        # Convert parameter types
        param_types = [self.type_to_llvm(p.type_name) for p in func_node.parameters]
        return_type = self.type_to_llvm(func_node.return_type.type_name)

        # Create function type
        func_type = ir.FunctionType(return_type, param_types)

        # Create function in module
        function = ir.Function(self.module, func_type, name=func_node.name)

        # Name the function arguments
        for i, arg in enumerate(function.args):
            arg.name = func_node.parameters[i].name
            # Register argument in current scope
            self.add_variable(arg.name, func_node.parameters[i].type_name)
            self.named_values[arg.name] = arg

        # Register function in current scope
        param_types = [p.type_name for p in func_node.parameters]
        self.add_function(func_node.name, param_types, func_node.return_type.type_name)

    def generate_function_body(self, func_node):
        """Generate the implementation for a function"""
        function = self.module.get_global(func_node.name)

        # Create entry block
        entry_block = function.append_basic_block(name="entry")
        self.builder = ir.IRBuilder(entry_block)

        # Create new scope level
        self.level_stack.append(Level())
        self.level += 1

        # Store function arguments in current scope
        for i, arg in enumerate(function.args):
            ptr = self.builder.alloca(
                self.type_to_llvm(func_node.parameters[i].type_name),
                name=f"{arg.name}_ptr",
            )
            self.builder.store(arg, ptr)
            self.add_variable(arg.name, func_node.parameters[i].type_name)
            self.named_values[arg.name] = ptr

        # Generate body statements
        for stmt in func_node.body.statements:
            self.generate_statement(stmt)

        # Add implicit return if needed
        if len(func_node.body.statements) > 0 and not isinstance(
            func_node.body.statements[len(func_node.body.statements) - 1], ReturnNode
        ):
            if func_node.return_type.type_name == "void":
                self.builder.ret_void()
            else:
                raise Exception(f"Function {func_node.name} missing return statement")

        # Exit scope
        self.level_stack.pop()
        self.level -= 1

    def generate_statement(self, stmt):
        """Dispatch statement generation"""
        if isinstance(stmt, VariableDeclarationNode):
            self.generate_variable_declaration(stmt)
        elif isinstance(stmt, ReturnNode):
            self.generate_return(stmt)
        elif isinstance(stmt, ExpressionNode):
            self.generate_expression(stmt)
        elif isinstance(stmt, FunctionCallNode):
            self.generate_function_call(stmt)
        elif isinstance(stmt, IfNode):
            self.generate_if_statement(stmt)
        elif isinstance(stmt, WhileNode):
            self.generate_while_loop(stmt)
        elif isinstance(stmt, ExpressionNode):
            self.generate_expression(stmt)
        elif isinstance(stmt, AssignmentNode):
            self.generate_assignment(stmt)
        else:
            raise Exception(f"Unknown statement type: {type(stmt)}")

    def generate_assignment(self, assign_node: AssignmentNode):
        """Generate code for variable assignment"""
        # Load the variable
        var_ptr = self.named_values.get(assign_node.identifier.name)
        if not var_ptr:
            raise Exception(f"Undefined variable: {assign_node.identifier}")

        # Generate the value to assign
        value = self.generate_expression(assign_node.value)

        # Store the value in the variable
        self.builder.store(value, var_ptr)

    def type_to_llvm(self, type_name):
        """Convert your language types to LLVM types"""
        type_map = {
            "i32": ir.IntType(32),
            "i64": ir.IntType(64),
            "f32": ir.FloatType(),
            "f64": ir.DoubleType(),
            "bool": ir.IntType(1),
            "void": ir.VoidType(),
            "char": ir.IntType(8),
            "string": ir.PointerType(ir.IntType(8)),
            "varargs": ir.PointerType(ir.IntType(8)),  # varargs are treated as pointers
        }
        return type_map.get(str(type_name), ir.IntType(32))  # Default to i32

    def generate_external_function(self, func_node: ExternalDeclarationNode):
        """Generate code for external function declarations

        Args:
            func_node: The external function declaration node

        Returns:
            The LLVM Function object that was created
        """
        # Handle varargs detection
        is_varargs = False
        for p in func_node.parameters:
            if p.type_name == "...":
                is_varargs = True
                break

        if is_varargs:
            func_node.parameters.pop()

        param_types = []
        for p in func_node.parameters:
            if p.type_name != Type.varargs or p.name != Type.varargs:
                llvm_type = self.type_to_llvm(p.type_name)
                if llvm_type is None:
                    raise TypeError(f"Unsupported parameter type: {p.type_name}")
                param_types.append(llvm_type)

        # Convert return type
        return_type = self.type_to_llvm(func_node.return_type.type_name)
        if return_type is None:
            raise TypeError(
                f"Unsupported return type: {func_node.return_type.type_name}"
            )

        # Create function type
        func_type = ir.FunctionType(return_type, param_types, var_arg=is_varargs)

        # Create the function
        func = ir.Function(self.module, func_type, name=func_node.name)

        # Name the parameters
        for idx, arg in enumerate(func.args):
            if idx < len(func_node.parameters):
                arg.name = func_node.parameters[idx].name

        # Add to function table
        self.add_function(
            func_node.name,
            [p.type_name for p in func_node.parameters],
            func_node.return_type.type_name,
        )

        return func

    def process_string_literal(self, literal: str) -> str:
        """Convert escape sequences in string literals to their actual characters"""
        ESCAPE_SEQUENCES = {
            "\\n": "\n",
            "\\t": "\t",
            "\\r": "\r",
            '\\"': '"',
            "\\'": "'",
            "\\\\": "\\",
            "\\0": "\0",
            "\\a": "\a",
            "\\b": "\b",
            "\\f": "\f",
            "\\v": "\v",
        }
        result = []
        i = 0
        while i < len(literal):
            if literal[i] == "\\" and i + 1 < len(literal):
                # Handle escape sequence
                seq = literal[i : i + 2]
                if seq in ESCAPE_SEQUENCES:
                    result.append(ESCAPE_SEQUENCES[seq])
                    i += 2
                    continue
            # Normal character
            result.append(literal[i])
            i += 1
        return "".join(result)

    def generate_string_literal(self, str_value: str):
        """Serialize a string literal to LLVM IR
        This function creates a global variable for the string
        and returns a pointer to it.
        , also cache the string literal
        """

        if str_value in self.string_literals:
            return self.string_literals[str_value]

        null_term = self.process_string_literal(str_value) + "\0"

        str_const = ir.Constant(
            ir.ArrayType(ir.IntType(8), len(null_term)),
            bytearray(null_term.encode("utf-8")),
        )

        global_name = f".str.{self.string_counter}.global"
        self.string_counter += 1

        gv = ir.GlobalVariable(self.module, str_const.type, name=global_name)
        gv.linkage = "private"
        gv.unnamed_addr = True
        gv.global_constant = True
        gv.initializer = str_const
        gv.section = ".rodata"

        gv.align = 1

        zero = ir.Constant(ir.IntType(32), 0)

        v = self.builder.gep(gv, [zero, zero], inbounds=True)
        self.string_literals[str_value] = v
        return v

    def generate_variable_declaration(self, decl_node: VariableDeclarationNode):
        """Generate code for variable declaration"""
        # Allocate memory
        llvm_type = self.type_to_llvm(decl_node.var_type.type_name)
        ptr = self.builder.alloca(llvm_type, name=decl_node.name)
        ptr.align = 4 if llvm_type == ir.IntType(32) else 8

        # Generate initial value
        value = self.generate_expression(decl_node.value)

        if value is None:
            # store a zero if no value is provided
            if decl_node.var_type.type_name == Type.i32:
                value = ir.Constant(ir.IntType(32), 0)
            elif decl_node.var_type.type_name == Type.f32:
                value = ir.Constant(ir.FloatType(), 0.0)
            elif decl_node.var_type.type_name == Type.bool:
                value = ir.Constant(ir.IntType(1), 0)
            elif decl_node.var_type.type_name == Type.void:
                value = ir.Constant(ir.VoidType(), None)
            elif decl_node.var_type.type_name == Type.string:
                value = self.generate_string_literal(decl_node.value)
            elif decl_node.var_type.type_name == "char":
                value = ir.Constant(ir.IntType(8), 0)

            else:
                raise Exception(
                    f"Cannot assign None to variable of type {decl_node.var_type.type_name}"
                )

        # Store value
        if decl_node.var_type.type_name == Type.string:
            # For strings, we need to ensure we're storing a pointer
            if isinstance(value, ir.instructions.LoadInstr):
                # If it's a load from another string variable, get the pointer directly
                value = value.operands[0]
            self.builder.store(value, ptr)
        else:
            self.builder.store(value, ptr)

        # Register variable in current scope
        self.add_variable(decl_node.name, decl_node.var_type.type_name)
        self.named_values[decl_node.name] = ptr

    def generate_return(self, ret_node):
        """Generate return statement"""
        if ret_node.value:
            value = self.generate_expression(ret_node.value)
            self.builder.ret(value)
        else:
            self.builder.ret_void()

    def generate_expression(self, expr_node):
        """Generate code for expressions"""
        if isinstance(expr_node, LiteralNode):
            return self.generate_literal(expr_node)
        elif isinstance(expr_node, IdentifierNode):
            return self.generate_identifier(expr_node)
        elif isinstance(expr_node, ExpressionNode):
            return self.generate_binary_op(expr_node)
        elif isinstance(expr_node, FunctionCallNode):
            return self.generate_function_call(expr_node)
        else:
            raise Exception(f"Unknown expression type: {type(expr_node)}")

    def generate_literal(self, lit_node: LiteralNode):
        """Generate code for literals"""
        if lit_node.type_name == Type.i8:
            return ir.Constant(ir.IntType(8), lit_node.value)
        elif lit_node.type_name == Type.i16:
            return ir.Constant(ir.IntType(16), lit_node.value)
        elif lit_node.type_name == Type.i32:
            return ir.Constant(ir.IntType(32), lit_node.value)
        elif lit_node.type_name == Type.i64:
            return ir.Constant(ir.IntType(64), lit_node.value)
        elif lit_node.type_name == Type.f32:
            return ir.Constant(ir.FloatType(), lit_node.value)
        elif lit_node.type_name == Type.f64:
            return ir.Constant(ir.DoubleType(), lit_node.value)
        elif lit_node.type_name == Type.bool:
            return ir.Constant(ir.IntType(1), 1 if lit_node.value else 0)
        elif lit_node.type_name == Type.string:
            return self.generate_string_literal(lit_node.value)
        elif lit_node.type_name == Type.char:
            return ir.Constant(ir.IntType(8), ord(lit_node.value))
        else:
            raise Exception(f"Unknown literal type: {lit_node.type_name}")

    def generate_identifier(self, id_node):
        """Generate code for variable references"""
        ptr = self.named_values.get(id_node.name)
        if not ptr:
            raise Exception(f"Undefined variable: {id_node.name}")

        var_type = self.get_variable(id_node.name)
        if var_type == Type.string:
            # For strings, we need to ensure we're loading a pointer
            if isinstance(ptr, ir.instructions.LoadInstr):
                ptr = ptr.operands[0]
            return ptr

        return self.builder.load(ptr, name=id_node.name)

    def generate_binary_op(self, binop_node):
        """Generate code for binary operations"""
        left = self.generate_expression(binop_node.left)
        right = self.generate_expression(binop_node.right)

        op_map = {
            "+": self.builder.add,
            "-": self.builder.sub,
            "*": self.builder.mul,
            "/": self.builder.sdiv,
            "==": lambda l, r, name: self.builder.icmp_signed("==", l, r, name),
            "<": lambda l, r, name: self.builder.icmp_signed("<", l, r, name),
            "<=": lambda l, r, name: self.builder.icmp_signed("<=", l, r, name),
            ">": lambda l, r, name: self.builder.icmp_signed(">", l, r, name),
            ">=": lambda l, r, name: self.builder.icmp_signed(">=", l, r, name),
            "!=": lambda l, r, name: self.builder.icmp_signed("!=", l, r, name),
        }

        if binop_node.operator not in op_map:
            raise Exception(f"Unknown operator: {binop_node.operator}")

        return op_map[binop_node.operator](left, right, name="tmp")

    def generate_function_call(self, call_node):
        """Generate code for function calls"""
        # Look up function
        func = self.module.get_global(call_node.function_name)
        if not func:
            raise Exception(f"Undefined function: {call_node.function_name}")

        # Generate arguments
        args = [self.generate_expression(arg) for arg in call_node.arguments]

        # Call function
        return self.builder.call(func, args, name="calltmp")

    def generate_if_statement(self, if_node):
        """Generate LLVM IR for if statements"""
        # Generate condition
        cond = self.generate_expression(if_node.condition)

        # Create basic blocks
        func = self.builder.block.function
        then_block = func.append_basic_block("then")
        else_block = func.append_basic_block("else")
        merge_block = func.append_basic_block("ifcont")

        # Create conditional branch
        self.builder.cbranch(cond, then_block, else_block)

        # Generate then block
        self.builder.position_at_end(then_block)
        self.generate_block(if_node.then_block)
        self.builder.branch(merge_block)

        # Generate else block (if exists)
        self.builder.position_at_end(else_block)
        if if_node.else_block:
            self.generate_block(if_node.else_block)
        self.builder.branch(merge_block)

        # Continue from merge point
        self.builder.position_at_end(merge_block)

    def generate_while_loop(self, while_node):
        """Generate code for while loops"""
        # Create basic blocks
        test_block = self.builder.append_basic_block("while_test")
        body_block = self.builder.append_basic_block("while_body")
        end_block = self.builder.append_basic_block("while_end")

        # Branch to test block
        self.builder.branch(test_block)

        # Generate test condition
        self.builder.position_at_end(test_block)
        cond = self.generate_expression(while_node.condition)
        self.builder.cbranch(cond, body_block, end_block)

        # Generate loop body
        self.builder.position_at_end(body_block)
        self.generate_block(while_node.body)
        self.builder.branch(test_block)  # Loop back

        # Position builder at end
        self.builder.position_at_end(end_block)

    def generate_block(self, block_node):
        """Generate code for a block of statements"""
        # Create new scope level
        self.level_stack.append(Level())
        self.level += 1

        # Generate all statements
        for stmt in block_node.statements:
            self.generate_statement(stmt)

        # Exit scope
        self.level_stack.pop()
        self.level -= 1


if __name__ == "__main__":
    from pathlib import Path
    from lexer import Lexer
    from parser import Parser

    lexer = Lexer()
    lexer.set_input(Path("examples/basic.l98"))
    tokens = lexer.tokenize()
    parser = Parser(tokens)

    ast = parser.parse()

    generator = Generator(
        ast,
    )

    llvm_ir = generator.generate_start()
    print(llvm_ir)
