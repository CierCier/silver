use std::sync::Arc;

use super::registry::TypeId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: Arc<str>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: Arc<str>,
    pub discriminant: i64,
    pub payload: Vec<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumType {
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Unit,
    Bool,
    Int { bits: u16, signed: bool },
    Float { bits: u16 },
    String,
    Vec { elem: TypeId },
    Optional { inner: TypeId },
    Struct(StructType),
    Enum(EnumType),
    Function(FunctionType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub id: TypeId,
    pub name: Arc<str>,
    pub kind: TypeKind,
}
