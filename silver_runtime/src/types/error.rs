use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    DuplicateTypeName {
        name: String,
    },
    DuplicateFieldName {
        type_name: String,
        field: String,
    },
    DuplicateVariantName {
        type_name: String,
        variant: String,
    },
    DuplicateVariantDiscriminant {
        type_name: String,
        discriminant: i64,
    },
    DiscriminantOverflow {
        type_name: String,
        last: i64,
    },
    UnknownTypeId {
        id: u32,
    },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::DuplicateTypeName { name } => {
                write!(f, "type name already defined: {name}")
            }
            TypeError::DuplicateFieldName { type_name, field } => {
                write!(f, "duplicate field '{field}' in type '{type_name}'")
            }
            TypeError::DuplicateVariantName { type_name, variant } => {
                write!(f, "duplicate variant '{variant}' in type '{type_name}'")
            }
            TypeError::DuplicateVariantDiscriminant {
                type_name,
                discriminant,
            } => write!(
                f,
                "duplicate enum discriminant {discriminant} in type '{type_name}'"
            ),
            TypeError::DiscriminantOverflow { type_name, last } => write!(
                f,
                "enum discriminant overflow in type '{type_name}' after {last}"
            ),
            TypeError::UnknownTypeId { id } => write!(f, "unknown type id: {id}"),
        }
    }
}

impl std::error::Error for TypeError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueError {
    TypeMismatch {
        expected: String,
        found: String,
    },
    InvalidStructFieldCount {
        expected: usize,
        found: usize,
    },
    InvalidEnumVariant {
        type_name: String,
        index: usize,
    },
    InvalidEnumPayloadCount {
        expected: usize,
        found: usize,
    },
    UnknownEnumVariantName {
        type_name: String,
        variant: String,
    },
    UnknownEnumDiscriminant {
        type_name: String,
        discriminant: i64,
    },
    CannotCast {
        from: String,
        to: String,
    },
}

impl fmt::Display for ValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueError::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected {expected}, found {found}")
            }
            ValueError::InvalidStructFieldCount { expected, found } => {
                write!(
                    f,
                    "invalid struct field count: expected {expected}, found {found}"
                )
            }
            ValueError::InvalidEnumVariant { type_name, index } => {
                write!(f, "invalid enum variant index {index} for '{type_name}'")
            }
            ValueError::InvalidEnumPayloadCount { expected, found } => {
                write!(
                    f,
                    "invalid enum payload count: expected {expected}, found {found}"
                )
            }
            ValueError::UnknownEnumVariantName { type_name, variant } => {
                write!(f, "unknown enum variant '{variant}' for '{type_name}'")
            }
            ValueError::UnknownEnumDiscriminant {
                type_name,
                discriminant,
            } => write!(
                f,
                "unknown enum discriminant {discriminant} for '{type_name}'"
            ),
            ValueError::CannotCast { from, to } => write!(f, "cannot cast from {from} to {to}"),
        }
    }
}

impl std::error::Error for ValueError {}
