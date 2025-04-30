class Type:
    def __init__(self, name: str, size: int = 0, is_pointer: bool = False):
        self.name = name
        self.is_pointer = is_pointer

        if is_pointer:
            self.size = 8
        else:
            self.size = size

    def __repr__(self):
        return f"Type(name={self.name}, size={self.size}, is_pointer={self.is_pointer})"


# Primitive Types

## Simple Data Stores
L98_void = Type("void", 0)  # Void type
L98_bool = Type("bool", 1)  # Boolean type

L98_i8 = Type("i8", 1)  # 8 bit signed integer
L98_i16 = Type("i16", 2)  # 16 bit signed integer
L98_i32 = Type("i32", 4)  # 32 bit signed integer
L98_i64 = Type("i64", 8)  # 64 bit signed integer

L98_u8 = Type("u8", 1)  # 8 bit unsigned integer
L98_u16 = Type("u16", 2)  # 16 bit unsigned integer
L98_u32 = Type("u32", 4)  # 32 bit unsigned integer
L98_u64 = Type("u64", 8)  # 64 bit unsigned integer

L98_f32 = Type("f32", 4)  # 32 bit floating point
L98_f64 = Type("f64", 8)  # 64 bit floating point
L98_f80 = Type("f80", 10)  # 80 bit floating point
L98_f128 = Type("f128", 16)  # 128 bit floating point

L98_char = Type("char", 1)  # Character type
L98_string = Type("string", 8, True)  # String type (pointer to char) ## these need to be different from ptr, since i'd like to make a string constant space

L98_ptr = Type("ptr", 8, True)  # Pointer type

## Pointer Types
C_string = L98_string  # Basically the same as a string in C
C_Variatic = Type("C_Variatic", 8, True)  # Pointer to a byte array


TYPE_MAP = {
        "void": L98_void,
        "bool": L98_bool,
        "i8": L98_i8,
        "i16": L98_i16,
        "i32": L98_i32,
        "i64": L98_i64,

        "u8": L98_u8,
        "u16": L98_u16,
        "u32": L98_u32,
        "u64": L98_u64,

        "f32": L98_f32,
        "f64": L98_f64,
        "f80": L98_f80,
        "f128": L98_f128,

        "char": L98_char,
        "string": L98_string,
        "..." : C_Variatic 
}


class DefinedType:
    """Base class to derive new Types from,
    Might be usefull in the future for structs, unions, enums, etc.
    For now, it is just a placeholder.
    """

    def __init__(self, name: str, size: int = 0, is_pointer: bool = False):
        self.name = name
        self.size = size
        self.is_pointer = is_pointer
        pass


class TypeError(Exception):
    def __init__(self, message: str):
        super().__init__(message)
        self.message = message

    def __str__(self):
        return self.message
