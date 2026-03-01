use std::sync::Arc;

use super::error::ValueError;
use super::registry::{TypeId, TypeRegistry};
use super::type_info::TypeKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Int {
        ty: TypeId,
        value: i128,
    },
    UInt {
        ty: TypeId,
        value: u128,
    },
    Float {
        ty: TypeId,
        value: f64,
    },
    String(Arc<str>),
    Vec {
        ty: TypeId,
        items: Vec<Value>,
    },
    Optional {
        ty: TypeId,
        value: Option<Box<Value>>,
    },
    Struct {
        ty: TypeId,
        fields: Vec<Value>,
    },
    Enum {
        ty: TypeId,
        variant: usize,
        fields: Vec<Value>,
    },
}

impl Value {
    pub fn type_id(&self) -> TypeId {
        match self {
            Value::Unit => TypeId::UNIT,
            Value::Bool(_) => TypeId::BOOL,
            Value::Int { ty, .. } => *ty,
            Value::UInt { ty, .. } => *ty,
            Value::Float { ty, .. } => *ty,
            Value::String(_) => TypeId::STRING,
            Value::Vec { ty, .. } => *ty,
            Value::Optional { ty, .. } => *ty,
            Value::Struct { ty, .. } => *ty,
            Value::Enum { ty, .. } => *ty,
        }
    }

    pub fn i64(v: i64) -> Self {
        Value::Int {
            ty: TypeId::I64,
            value: v as i128,
        }
    }

    pub fn u64(v: u64) -> Self {
        Value::UInt {
            ty: TypeId::U64,
            value: v as u128,
        }
    }

    pub fn f64(v: f64) -> Self {
        Value::Float {
            ty: TypeId::F64,
            value: v,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Int { ty, value } if *ty == TypeId::I64 => Some(*value as i64),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::Float { ty, value } if *ty == TypeId::F64 => Some(*value),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(v) => Some(v.as_ref()),
            _ => None,
        }
    }

    pub fn new_vec(
        registry: &mut TypeRegistry,
        elem: TypeId,
        items: Vec<Value>,
    ) -> Result<Value, ValueError> {
        let vec_ty = registry
            .vec_of(elem)
            .map_err(|e| ValueError::TypeMismatch {
                expected: "valid element type".to_string(),
                found: e.to_string(),
            })?;

        for item in &items {
            if item.type_id() != elem {
                let expected = registry
                    .name_of(elem)
                    .ok()
                    .unwrap_or("<unknown>")
                    .to_string();
                let found = registry
                    .name_of(item.type_id())
                    .ok()
                    .unwrap_or("<unknown>")
                    .to_string();
                return Err(ValueError::TypeMismatch { expected, found });
            }
        }

        Ok(Value::Vec { ty: vec_ty, items })
    }

    pub fn new_optional(
        registry: &mut TypeRegistry,
        inner: TypeId,
        value: Option<Value>,
    ) -> Result<Value, ValueError> {
        let opt_ty = registry
            .optional_of(inner)
            .map_err(|e| ValueError::TypeMismatch {
                expected: "valid inner type".to_string(),
                found: e.to_string(),
            })?;

        if let Some(v) = value {
            if v.type_id() != inner {
                let expected = registry
                    .name_of(inner)
                    .ok()
                    .unwrap_or("<unknown>")
                    .to_string();
                let found = registry
                    .name_of(v.type_id())
                    .ok()
                    .unwrap_or("<unknown>")
                    .to_string();
                return Err(ValueError::TypeMismatch { expected, found });
            }
            Ok(Value::Optional {
                ty: opt_ty,
                value: Some(Box::new(v)),
            })
        } else {
            Ok(Value::Optional {
                ty: opt_ty,
                value: None,
            })
        }
    }

    pub fn new_struct(
        registry: &TypeRegistry,
        ty: TypeId,
        fields: Vec<Value>,
    ) -> Result<Value, ValueError> {
        let type_info = registry.get(ty).map_err(|e| ValueError::TypeMismatch {
            expected: "known struct type".to_string(),
            found: e.to_string(),
        })?;

        let (expected_count, field_types): (usize, Vec<TypeId>) = match &type_info.kind {
            TypeKind::Struct(st) => (st.fields.len(), st.fields.iter().map(|f| f.ty).collect()),
            other => {
                return Err(ValueError::TypeMismatch {
                    expected: "struct".to_string(),
                    found: format!("{other:?}"),
                });
            }
        };

        if fields.len() != expected_count {
            return Err(ValueError::InvalidStructFieldCount {
                expected: expected_count,
                found: fields.len(),
            });
        }

        for (idx, (v, expected_ty)) in fields.iter().zip(field_types.iter()).enumerate() {
            if v.type_id() != *expected_ty {
                let expected = registry
                    .name_of(*expected_ty)
                    .ok()
                    .unwrap_or("<unknown>")
                    .to_string();
                let found = registry
                    .name_of(v.type_id())
                    .ok()
                    .unwrap_or("<unknown>")
                    .to_string();
                return Err(ValueError::TypeMismatch {
                    expected: format!("field[{idx}] {expected}"),
                    found,
                });
            }
        }

        Ok(Value::Struct { ty, fields })
    }

    pub fn new_enum_by_index(
        registry: &TypeRegistry,
        ty: TypeId,
        variant: usize,
        fields: Vec<Value>,
    ) -> Result<Value, ValueError> {
        let type_info = registry.get(ty).map_err(|e| ValueError::TypeMismatch {
            expected: "known enum type".to_string(),
            found: e.to_string(),
        })?;

        let enum_def = match &type_info.kind {
            TypeKind::Enum(en) => en,
            other => {
                return Err(ValueError::TypeMismatch {
                    expected: "enum".to_string(),
                    found: format!("{other:?}"),
                });
            }
        };

        let Some(variant_def) = enum_def.variants.get(variant) else {
            return Err(ValueError::InvalidEnumVariant {
                type_name: type_info.name.to_string(),
                index: variant,
            });
        };

        if fields.len() != variant_def.payload.len() {
            return Err(ValueError::InvalidEnumPayloadCount {
                expected: variant_def.payload.len(),
                found: fields.len(),
            });
        }

        for (idx, (v, expected_ty)) in fields.iter().zip(variant_def.payload.iter()).enumerate() {
            if v.type_id() != *expected_ty {
                let expected = registry
                    .name_of(*expected_ty)
                    .ok()
                    .unwrap_or("<unknown>")
                    .to_string();
                let found = registry
                    .name_of(v.type_id())
                    .ok()
                    .unwrap_or("<unknown>")
                    .to_string();
                return Err(ValueError::TypeMismatch {
                    expected: format!("variant payload[{idx}] {expected}"),
                    found,
                });
            }
        }

        Ok(Value::Enum {
            ty,
            variant,
            fields,
        })
    }

    pub fn new_enum_by_name(
        registry: &TypeRegistry,
        ty: TypeId,
        variant: &str,
        fields: Vec<Value>,
    ) -> Result<Value, ValueError> {
        let type_name = registry.name_of(ty).ok().unwrap_or("<unknown>").to_string();

        let idx = registry
            .enum_variant_index(ty, variant)
            .map_err(|e| ValueError::TypeMismatch {
                expected: "known enum type".to_string(),
                found: e.to_string(),
            })?
            .ok_or_else(|| ValueError::UnknownEnumVariantName {
                type_name,
                variant: variant.to_string(),
            })?;

        Self::new_enum_by_index(registry, ty, idx, fields)
    }

    pub fn new_enum_by_discriminant(
        registry: &TypeRegistry,
        ty: TypeId,
        discriminant: i64,
        fields: Vec<Value>,
    ) -> Result<Value, ValueError> {
        let type_name = registry.name_of(ty).ok().unwrap_or("<unknown>").to_string();
        let idx = registry
            .enum_variant_by_discriminant(ty, discriminant)
            .map_err(|e| ValueError::TypeMismatch {
                expected: "known enum type".to_string(),
                found: e.to_string(),
            })?
            .ok_or_else(|| ValueError::UnknownEnumDiscriminant {
                type_name,
                discriminant,
            })?;

        Self::new_enum_by_index(registry, ty, idx, fields)
    }
}
