mod cast;
mod error;
mod registry;
mod type_info;
mod value;

pub use cast::CastKind;
pub use error::{TypeError, ValueError};
pub use registry::{TypeId, TypeRegistry};
pub use type_info::{EnumType, EnumVariant, Field, FunctionType, StructType, Type, TypeKind};
pub use value::Value;

use std::fmt;

pub struct TypeDisplay<'a> {
    registry: &'a TypeRegistry,
    id: TypeId,
}

impl TypeRegistry {
    pub fn display_type<'a>(&'a self, id: TypeId) -> TypeDisplay<'a> {
        TypeDisplay { registry: self, id }
    }
}

impl fmt::Display for TypeDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.registry.name_of(self.id) {
            Ok(name) => write!(f, "{name}"),
            Err(_) => write!(f, "<unknown:{}>", self.id.0),
        }
    }
}

pub struct ValueDisplay<'a> {
    registry: &'a TypeRegistry,
    value: &'a Value,
}

impl TypeRegistry {
    pub fn display_value<'a>(&'a self, value: &'a Value) -> ValueDisplay<'a> {
        ValueDisplay {
            registry: self,
            value,
        }
    }
}

impl fmt::Display for ValueDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value as V;
        match self.value {
            V::Unit => write!(f, "()"),
            V::Bool(v) => write!(f, "{v}"),
            V::Int { value, .. } => write!(f, "{value}"),
            V::UInt { value, .. } => write!(f, "{value}"),
            V::Float { value, .. } => write!(f, "{value}"),
            V::String(s) => write!(f, "\"{}\"", s),
            V::Vec { ty, items } => {
                write!(f, "{}[", self.registry.display_type(*ty))?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", self.registry.display_value(item))?;
                }
                write!(f, "]")
            }
            V::Optional { ty, value } => {
                write!(f, "{}(", self.registry.display_type(*ty))?;
                if let Some(v) = value.as_deref() {
                    write!(f, "{}", self.registry.display_value(v))?;
                } else {
                    write!(f, "none")?;
                }
                write!(f, ")")
            }
            V::Struct { ty, fields } => {
                write!(f, "{}{{", self.registry.display_type(*ty))?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", self.registry.display_value(field))?;
                }
                write!(f, "}}")
            }
            V::Enum {
                ty,
                variant,
                fields,
            } => {
                write!(f, "{}::", self.registry.display_type(*ty))?;
                if let Ok(Some(v)) = self.registry.enum_variant(*ty, *variant) {
                    write!(f, "{}", v.name)?;
                } else {
                    write!(f, "#{}", variant)?;
                }

                if !fields.is_empty() {
                    write!(f, "(")?;
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", self.registry.display_value(field))?;
                    }
                    write!(f, ")")?;
                }

                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests;
