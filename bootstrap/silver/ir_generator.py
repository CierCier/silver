import llvmlite

from llvmlite import ir
from llvmlite import binding as llvm
from llvmlite.ir import (
    Module,
    Function,
    FunctionType,
    IRBuilder,
    Constant,
    GlobalVariable,
    Value,
    Type,
    BaseStructType,
    IdentifiedStructType,
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
    MemberAccess,
    StructInit,
    AssignStmt,
)

from silver.silver_types import SilverStruct, SilverUnion

from typing import Dict, List, Optional, Any


llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()


def get_data_layout(triple: str):
    if triple == "x86_64-pc-linux-gnu":
        return "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
    else:
        raise Exception(f"Unsupported triple: {triple}")


class IRGenerator:
    """LLVM IR Generator for Silver language.

    This class is responsible for converting Silver AST nodes into LLVM IR. It handles:
    - Type system mapping between Silver and LLVM types
    - Function and variable declarations
    - Control flow (if statements, loops)
    - Expression generation
    - Struct and union handling
    - String constant management

    Attributes:
        module (Module): The LLVM module being generated
        builder (IRBuilder): Current IR builder for generating instructions
        named_values (Dict[str, ir.Value]): Map of variable names to their LLVM values
        functions (Dict[str, Function]): Map of function names to their LLVM declarations
        structs (Dict[str, tuple]): Map of struct names to (LLVM type, Silver type) pairs
        type_map (Dict[str, tuple]): Map of type names to (LLVM type, Silver type) pairs
        string_constants (Dict[str, ir.GlobalVariable]): Map of string literals to their global variables
    """

    def __init__(self, triple: str):
        """Initialize the IR generator.

        Args:
            triple: The target triple (e.g. "x86_64-pc-linux-gnu")
        """
        self.module = Module("Silver")
        self.builder: Optional[IRBuilder] = None
        self.named_values: Dict[str, ir.Value] = {}
        self.functions: Dict[str, Function] = {}
        # Store (LLVM struct type, SilverStruct type)
        self.structs: Dict[str, tuple[IdentifiedStructType, SilverStruct]] = {}
        # Store (LLVM type, Silver type)
        self.type_map: Dict[str, tuple[ir.Type, Any]] = {}

        # Map to store string constants
        self.string_constants: Dict[str, ir.GlobalVariable] = {}

        self.string_counter = (
            0  # Counter for unique string names `.str.0`, `.str.1`, etc.
        )

        self.module.triple = triple
        self.module.data_layout = get_data_layout(triple)

        # Initialize basic type mappings
        self._init_type_mappings()

    def _init_type_mappings(self):
        """Initialize the basic type mappings."""
        from silver.silver_types import (
            IntType,
            UIntType,
            FloatType,
            DoubleType,
            VoidType,
            StringType,
            SilverPointer,
        )

        # Integer types
        self.type_map["i8"] = (ir.IntType(8), IntType(8))
        self.type_map["i16"] = (ir.IntType(16), IntType(16))
        self.type_map["i32"] = (ir.IntType(32), IntType(32))
        self.type_map["i64"] = (ir.IntType(64), IntType(64))

        # Unsigned integer types
        self.type_map["u8"] = (ir.IntType(8), UIntType(8))
        self.type_map["u16"] = (ir.IntType(16), UIntType(16))
        self.type_map["u32"] = (ir.IntType(32), UIntType(32))
        self.type_map["u64"] = (ir.IntType(64), UIntType(64))

        # Floating point types
        self.type_map["f32"] = (ir.FloatType(), FloatType(32))
        self.type_map["f64"] = (ir.DoubleType(), DoubleType())

        # Other basic types
        self.type_map["void"] = (ir.VoidType(), VoidType())
        self.type_map["string"] = (ir.PointerType(ir.IntType(8)), StringType())

        # Pointer types
        self.type_map["*i8"] = (
            ir.PointerType(ir.IntType(8)),
            SilverPointer("*i8", IntType(8)),
        )
        self.type_map["*i16"] = (
            ir.PointerType(ir.IntType(16)),
            SilverPointer("*i16", IntType(16)),
        )
        self.type_map["*i32"] = (
            ir.PointerType(ir.IntType(32)),
            SilverPointer("*i32", IntType(32)),
        )
        self.type_map["*i64"] = (
            ir.PointerType(ir.IntType(64)),
            SilverPointer("*i64", IntType(64)),
        )
        self.type_map["*f32"] = (
            ir.PointerType(ir.FloatType()),
            SilverPointer("*f32", FloatType(32)),
        )
        self.type_map["*f64"] = (
            ir.PointerType(ir.DoubleType()),
            SilverPointer("*f64", DoubleType()),
        )

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
        """Generate LLVM IR for an entire program.

        This is the main entry point for code generation. It processes:
        1. Struct definitions
        2. Global variables
        3. Function definitions

        Args:
            program: The root AST node representing the program

        Returns:
            str: The generated LLVM IR as a string
        """
        for struct in program.structs:
            self.generate_struct(struct)

        for global_var in program.globals:
            self.generate_global(global_var)

        for function in program.functions:
            self.generate_function(function)

        return str(self.module)

    def generate_struct(self, struct: SilverStruct):
        """Generate LLVM IR for a struct definition.

        Creates an identified struct type in LLVM and maps it to the Silver struct type.

        Args:
            struct: The struct AST node to generate code for
        """
        ctx = ir.global_context
        name = f"Struct.{struct.name}"
        structure = ctx.get_identified_type(name)

        elements = []
        for field_name, field_info in struct.fields.items():
            # Get the field type, handling nested structs
            field_type = self.get_llvm_type(field_info["type"])
            elements.append(field_type)

        structure.set_body(*elements)
        self.structs[struct.name] = (structure, struct)

    def get_struct_field_index(self, struct_name: str, field_name: str) -> int:
        """Get the index of a field in a struct."""
        if struct_name not in self.structs:
            raise Exception(f"Unknown struct type: {struct_name}")

        struct_type, silver_struct = self.structs[struct_name]
        try:
            return list(silver_struct.fields.keys()).index(field_name)
        except ValueError:
            raise Exception(f"Unknown field {field_name} in struct {struct_name}")

    def get_struct_field_type(self, struct_name: str, field_name: str) -> ir.Type:
        """Get the LLVM type of a field in a struct."""
        if struct_name not in self.structs:
            raise Exception(f"Unknown struct type: {struct_name}")

        struct_type, silver_struct = self.structs[struct_name]
        try:
            field_index = list(silver_struct.fields.keys()).index(field_name)
            return struct_type.elements[field_index]
        except ValueError:
            raise Exception(f"Unknown field {field_name} in struct {struct_name}")

    def get_silver_struct_type(self, struct_name: str) -> SilverStruct:
        """Get the Silver type system representation of a struct."""
        if struct_name not in self.structs:
            raise Exception(f"Unknown struct type: {struct_name}")

        _, silver_struct = self.structs[struct_name]
        return silver_struct

    def generate_member_access_assign(self, stmt: AssignStmt):
        """Generate code for struct member assignment."""
        # Get the struct pointer directly from named_values
        struct_ptr = self.named_values[stmt.target.struct.name]

        # Get the struct type name from the variable's type
        struct_type_name = struct_ptr.type.pointee.name.replace("Struct.", "")
        silver_struct = self.get_silver_type(struct_type_name)

        # Get field information from Silver type
        if stmt.target.field not in silver_struct.fields:
            raise Exception(
                f"Unknown field {stmt.target.field} in struct {struct_type_name}"
            )

        field_info = silver_struct.fields[stmt.target.field]
        field_type = self.get_llvm_type(field_info["type"])
        field_index = self.get_struct_field_index(struct_type_name, stmt.target.field)

        # Generate the value to store
        value = self.generate_expression(stmt.value)
        if isinstance(value, StructInit):
            # Handle nested struct initialization
            value = self.generate_nested_struct_init(value, field_type)
        elif self.needs_cast(value.type, field_type):
            value = self.cast_value(value, field_type)

        # Create GEP to access the field
        indices = [
            Constant(ir.IntType(32), 0),
            Constant(ir.IntType(32), field_index),
        ]
        field_ptr = self.builder.gep(struct_ptr, indices, inbounds=True)
        self.builder.store(value, field_ptr)

    def generate_union(self, union: SilverUnion):
        ctx = ir.global_context
        name = f"Union.{union.name}"
        un = ctx.get_identified_type(name)

        element = None

        for field_name, field_info in union.fields:
            if element is None:
                element = field_info
            else:
                if element.size() < field_info.size():
                    element = field_info

        element = self.get_llvm_type(element)
        return un.set_body(element)

    def generate_function(self, function):
        """Generate LLVM IR for a function definition.

        Handles:
        - Function declaration
        - Parameter setup
        - Basic block creation
        - Function body generation
        - Return value handling

        Args:
            function: The function AST node to generate code for
        """
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

        # return if we dont have a function body, probably a function declaration
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
        elif isinstance(stmt, AssignStmt):
            if isinstance(stmt.target, MemberAccess):
                self.generate_member_access_assign(stmt)
            else:
                # Handle normal assignment
                target = self.named_values[stmt.target.name]
                value = self.generate_expression(stmt.value)
                self.builder.store(value, target)

    def generate_variable_decl(self, decl: VariableDecl):
        """Generate LLVM IR for a variable declaration.

        Handles:
        - Allocation of stack space for the variable
        - Initialization with a value
        - Special handling for struct initialization
        - Type casting when needed

        Args:
            decl: The variable declaration AST node
        """
        # Allocate space for the variable
        var_type = self.get_llvm_type(decl.var_type)
        alloca = self.builder.alloca(var_type, name=decl.name)

        # Generate code for the initial value
        if isinstance(decl.value, StructInit):
            # Handle struct initialization
            struct_type = var_type
            if not isinstance(struct_type, ir.IdentifiedStructType):
                raise Exception(
                    f"Expected struct type for struct initialization, got {struct_type}"
                )

            # Get the struct definition
            struct_name = struct_type.name.replace("Struct.", "")
            if struct_name not in self.structs:
                raise Exception(f"Unknown struct type: {struct_name}")

            struct_type, silver_struct = self.structs[struct_name]

            # Initialize each field
            for field_name, field_value in decl.value.fields.items():
                # Get the field index and type
                field_index = list(silver_struct.fields.keys()).index(field_name)
                field_info = silver_struct.fields[field_name]
                field_type = self.get_llvm_type(field_info["type"])

                # Generate the field value
                if isinstance(field_value, StructInit):
                    # Handle nested struct initialization
                    field_value_ir = self.generate_nested_struct_init(
                        field_value, field_type
                    )
                else:
                    field_value_ir = self.generate_expression(field_value)
                    if self.needs_cast(field_value_ir.type, field_type):
                        field_value_ir = self.cast_value(field_value_ir, field_type)

                # Create GEP to access the field
                indices = [
                    Constant(ir.IntType(32), 0),
                    Constant(ir.IntType(32), field_index),
                ]
                field_ptr = self.builder.gep(alloca, indices, inbounds=True)

                # Store the field value
                self.builder.store(field_value_ir, field_ptr)
        else:
            # Handle normal initialization
            value = self.generate_expression(decl.value)
            # Use the declared variable type instead of guessing
            if self.needs_cast(value.type, var_type):
                value = self.cast_value(value, var_type)

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

    def get_or_create_string_constant(self, value: str) -> ir.GlobalVariable:
        """Get an existing string constant or create a new one.

        Maintains a map of string literals to their global variables to avoid
        duplicating string constants in the generated code.

        Args:
            value: The string literal value

        Returns:
            ir.GlobalVariable: The global variable containing the string constant
        """
        if value in self.string_constants:
            return self.string_constants[value]

        # Create a new string constant
        str_name = f".str.{len(self.string_constants)}"
        str_val = ir.Constant(
            ir.ArrayType(ir.IntType(8), len(value) + 1),
            bytearray(value.encode() + b"\0"),
        )
        str_global = ir.GlobalVariable(self.module, str_val.type, name=str_name)
        str_global.initializer = str_val
        str_global.global_constant = True
        str_global.linkage = "private"

        # Store in our map
        self.string_constants[value] = str_global
        return str_global

    def generate_expression(self, expr):
        """Generate LLVM IR for an expression.

        Handles various expression types:
        - Member access (struct fields)
        - Identifiers (variables)
        - Numbers (integers and floats)
        - Strings
        - Binary operations
        - Function calls
        - Parenthesized expressions

        Args:
            expr: The expression AST node to generate code for

        Returns:
            ir.Value: The LLVM value representing the expression result
        """
        if isinstance(expr, MemberAccess):
            # Handle struct member access (e.g. point.x or line.p1.x)
            if isinstance(expr.struct, MemberAccess):
                # For nested access (e.g. line.p1.x), we need to:
                # 1. Generate the base value (line.p1)
                # 2. Create a temporary to store it
                # 3. Use that as the base for the next access

                ## TODO: This is a hack to get the base value
                ## LLVM allows us to access nested structs directly
                ## something like point.x.y
                ## IR: %0 = getelementptr %struct.Point, %struct.Point* %point, i32 0, i32 0, i32 0  (base offset, field offset, field offset)

                base_value = self.generate_expression(expr.struct)
                temp_alloca = self.builder.alloca(base_value.type)
                self.builder.store(base_value, temp_alloca)
                base_ptr = temp_alloca
            else:
                # For direct access (e.g. point.x), just get the pointer
                base_ptr = self.named_values[expr.struct.name]

            # Get the struct type and field index for the access
            struct_type_name = base_ptr.type.pointee.name.replace("Struct.", "")
            silver_struct = self.get_silver_type(struct_type_name)
            field_index = list(silver_struct.fields.keys()).index(expr.field)

            # Create GEP to access the field and load its value
            indices = [
                Constant(ir.IntType(32), 0),  # First index is always 0 for the pointer
                Constant(
                    ir.IntType(32), field_index
                ),  # Second index is the field offset
                ## subsequent indices are the field offsets of the nested struct
            ]
            field_ptr = self.builder.gep(base_ptr, indices, inbounds=True)
            return self.builder.load(field_ptr)

        elif isinstance(expr, Identifier):
            # Load the value from the alloca'd variable
            alloca = self.named_values[expr.name]
            return self.builder.load(alloca)

        elif isinstance(expr, Number):
            # Handle numeric literals (integers and floats)
            if "." in expr.value:
                # Floating point number
                value = float(expr.value)
                if "f" in expr.value.lower():
                    # Use float (32-bit) if 'f' suffix is present
                    return Constant(ir.FloatType(), value)
                else:
                    # Otherwise use double (64-bit)
                    return Constant(ir.DoubleType(), value)
            else:
                # Integer number
                value = int(expr.value)
                if value < 0:
                    # Negative numbers are signed
                    return Constant(ir.IntType(32), value)
                else:
                    # Positive numbers are also signed for consistency
                    return Constant(ir.IntType(32), value)

        elif isinstance(expr, String):
            # Handle string literals by creating or reusing global constants
            str_global = self.get_or_create_string_constant(expr.value)
            # Get pointer to the string data
            zero = Constant(ir.IntType(32), 0)
            str_ptr = self.builder.gep(str_global, [zero, zero])
            return str_ptr

        elif isinstance(expr, BinOp):
            # Handle binary operations (arithmetic and comparison)
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

            # Generate the appropriate operation based on the operator
            if expr.op == "+":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fadd(left, right)  # Float addition
                else:
                    return self.builder.add(left, right)  # Integer addition
            elif expr.op == "-":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fsub(left, right)  # Float subtraction
                else:
                    return self.builder.sub(left, right)  # Integer subtraction
            elif expr.op == "*":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fmul(left, right)  # Float multiplication
                else:
                    return self.builder.mul(left, right)  # Integer multiplication
            elif expr.op == "/":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fdiv(left, right)  # Float division
                else:
                    return self.builder.sdiv(left, right)  # Signed integer division
            elif expr.op == "==":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered(
                        "==", left, right
                    )  # Float comparison
                else:
                    return self.builder.icmp_signed(
                        "==", left, right
                    )  # Integer comparison
            elif expr.op == "!=":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered(
                        "!=", left, right
                    )  # Float comparison
                else:
                    return self.builder.icmp_signed(
                        "!=", left, right
                    )  # Integer comparison
            elif expr.op == "<":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered(
                        "<", left, right
                    )  # Float comparison
                else:
                    return self.builder.icmp_signed(
                        "<", left, right
                    )  # Integer comparison
            elif expr.op == "<=":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered(
                        "<=", left, right
                    )  # Float comparison
                else:
                    return self.builder.icmp_signed(
                        "<=", left, right
                    )  # Integer comparison
            elif expr.op == ">":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered(
                        ">", left, right
                    )  # Float comparison
                else:
                    return self.builder.icmp_signed(
                        ">", left, right
                    )  # Integer comparison
            elif expr.op == ">=":
                if isinstance(left.type, ir.FloatType):
                    return self.builder.fcmp_ordered(
                        ">=", left, right
                    )  # Float comparison
                else:
                    return self.builder.icmp_signed(
                        ">=", left, right
                    )  # Integer comparison

        elif isinstance(expr, FunctionCall):
            # Handle function calls
            func = self.functions[expr.name]

            # Generate arguments for the function call
            args = []
            if func.function_type.var_arg:
                # For variadic functions (like printf), handle each argument specially
                for i, arg in enumerate(expr.args):
                    arg_value = self.generate_expression(arg)
                    if i == 0:
                        # First argument is passed as is (format string for printf)
                        args.append(arg_value)
                    else:
                        # For subsequent arguments:
                        # - Promote integers to at least 32 bits
                        # - Convert floats to double
                        if (
                            isinstance(arg_value.type, ir.IntType)
                            and arg_value.type.width < 32
                        ):
                            arg_value = self.builder.zext(arg_value, ir.IntType(32))
                        elif isinstance(arg_value.type, ir.FloatType):
                            arg_value = self.builder.fpext(arg_value, ir.DoubleType())
                        args.append(arg_value)
            else:
                # For normal functions, cast arguments to match parameter types
                for arg, param_type in zip(expr.args, func.args):
                    arg_value = self.generate_expression(arg)
                    if self.needs_cast(arg_value.type, param_type.type):
                        arg_value = self.cast_value(arg_value, param_type.type)
                    args.append(arg_value)

            return self.builder.call(func, args)

        elif isinstance(expr, ParenExpr):
            # Handle parenthesized expressions by just generating the inner expression
            return self.generate_expression(expr.expr)

        raise Exception(f"Unsupported expression type: {type(expr)}")

    def get_llvm_type(self, type_node):
        """Get the LLVM type for a Silver type node."""
        type_name = type_node.name
        if type_name in self.type_map:
            return self.type_map[type_name][0]

        if type_name in self.structs:
            return self.structs[type_name][0]

        # Handle pointer types
        if type_name.startswith("*"):
            pointee_type = self.get_llvm_type(Type(type_name[1:]))
            return ir.PointerType(pointee_type)

        raise Exception(f"Unknown type: {type_name}")

    def get_silver_type(self, type_name: str) -> Any:
        """Get the Silver type representation for a type name."""
        if type_name in self.type_map:
            return self.type_map[type_name][1]
        if type_name in self.structs:
            return self.structs[type_name][1]
        raise Exception(f"Unknown type: {type_name}")

    def needs_cast(self, from_type: ir.Type, to_type: ir.Type) -> bool:
        """Check if a cast is needed between two types."""
        if from_type == to_type:
            return False

        # Both types are integers
        if isinstance(from_type, ir.IntType) and isinstance(to_type, ir.IntType):
            return from_type.width != to_type.width

        # Both types are floating point
        if isinstance(from_type, ir.FloatType) and isinstance(to_type, ir.FloatType):
            return False  # No cast needed between same float types

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

        # Double to Float
        if isinstance(from_type, ir.DoubleType) and isinstance(to_type, ir.FloatType):
            return True

        # Float to Double
        if isinstance(from_type, ir.FloatType) and isinstance(to_type, ir.DoubleType):
            return True

        return False

    def cast_value(self, value: ir.Value, to_type: ir.Type) -> ir.Value:
        """Cast a value to a different type.

        Handles various type conversions:
        - Integer to integer (extension/truncation)
        - Float to float (extension/truncation)
        - Integer to float (signed conversion)
        - Float to integer (signed conversion)
        - Pointer conversions

        Args:
            value: The LLVM value to cast
            to_type: The target LLVM type

        Returns:
            ir.Value: The casted value

        Raises:
            Exception: If the cast is not supported
        """
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
        if isinstance(from_type, (ir.FloatType, ir.DoubleType)) and isinstance(
            to_type, (ir.FloatType, ir.DoubleType)
        ):
            if isinstance(from_type, ir.FloatType) and isinstance(
                to_type, ir.DoubleType
            ):
                return self.builder.fpext(value, to_type)  # Float to double
            elif isinstance(from_type, ir.DoubleType) and isinstance(
                to_type, ir.FloatType
            ):
                return self.builder.fptrunc(value, to_type)  # Double to float
            else:
                return value  # Same type

        # Integer to float
        if isinstance(from_type, ir.IntType) and isinstance(
            to_type, (ir.FloatType, ir.DoubleType)
        ):
            # Always use signed conversion for integers to float
            return self.builder.sitofp(value, to_type)

        # Float to integer
        if isinstance(from_type, (ir.FloatType, ir.DoubleType)) and isinstance(
            to_type, ir.IntType
        ):
            # Always use signed conversion for float to integers
            return self.builder.fptosi(value, to_type)

        # Pointer to Pointer
        if isinstance(from_type, ir.PointerType) and isinstance(
            to_type, ir.PointerType
        ):
            return value

        # Integer to Pointer
        if isinstance(from_type, ir.IntType) and isinstance(to_type, ir.PointerType):
            return self.builder.inttoptr(value, to_type)

        # Pointer to Integer
        if isinstance(from_type, ir.PointerType) and isinstance(to_type, ir.IntType):
            return self.builder.ptrtoint(value, to_type)

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

    def generate_nested_struct_init(
        self, init: StructInit, struct_type: ir.Type
    ) -> ir.Value:
        """Generate code for initializing a nested struct."""
        if not isinstance(struct_type, ir.IdentifiedStructType):
            raise Exception(
                f"Expected struct type for struct initialization, got {struct_type}"
            )

        # Get the struct definition
        struct_name = struct_type.name.replace("Struct.", "")
        if struct_name not in self.structs:
            raise Exception(f"Unknown struct type: {struct_name}")

        struct_type, silver_struct = self.structs[struct_name]

        # Create a temporary alloca for the nested struct
        temp_alloca = self.builder.alloca(struct_type)

        # Initialize each field
        for field_name, field_value in init.fields.items():
            # Get the field index and type
            field_index = list(silver_struct.fields.keys()).index(field_name)
            field_info = silver_struct.fields[field_name]
            field_type = self.get_llvm_type(field_info["type"])

            # Generate the field value
            if isinstance(field_value, StructInit):
                # Recursively handle nested struct initialization
                field_value_ir = self.generate_nested_struct_init(
                    field_value, field_type
                )
            else:
                field_value_ir = self.generate_expression(field_value)
                if self.needs_cast(field_value_ir.type, field_type):
                    field_value_ir = self.cast_value(field_value_ir, field_type)

            # Create GEP to access the field
            indices = [
                Constant(ir.IntType(32), 0),
                Constant(ir.IntType(32), field_index),
            ]
            field_ptr = self.builder.gep(temp_alloca, indices, inbounds=True)

            # Store the field value
            self.builder.store(field_value_ir, field_ptr)

        # Load the initialized struct
        return self.builder.load(temp_alloca)


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
