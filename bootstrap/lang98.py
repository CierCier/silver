from enum import Enum


class Type(Enum):
    "simple enum class that maps each type to their llvm-ir type"

    i8 = "i8"
    i16 = "i16"
    i32 = "i32"
    i64 = "i64"
    u8 = "i8"
    u16 = "i16"
    u32 = "i32"
    u64 = "i64"

    f32 = "float"
    f64 = "double"

    bool = "i1"
    char = "i8"
    string = "i8*"
    void = "void"

    varargs = "..."
    # Add more types as needed
