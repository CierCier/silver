use std::collections::HashMap;
use std::sync::Arc;

use super::error::RuntimeError;
use super::registry::TypeId;
use super::value::RtValue;

pub type NativeCast =
    Arc<dyn Fn(&mut super::Runtime, RtValue) -> Result<RtValue, RuntimeError> + Send + Sync>;

#[derive(Default)]
pub struct CastRegistry {
    by_type: HashMap<(TypeId, TypeId), NativeCast>,
}

impl std::fmt::Debug for CastRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CastRegistry")
            .field("by_type", &self.by_type.len())
            .finish()
    }
}

impl CastRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, from: TypeId, to: TypeId, func: NativeCast) {
        self.by_type.insert((from, to), func);
    }

    pub fn lookup(&self, from: TypeId, to: TypeId) -> Option<NativeCast> {
        self.by_type.get(&(from, to)).cloned()
    }
}

impl super::Runtime {
    pub fn register_cast(&mut self, from: TypeId, to: TypeId, func: NativeCast) {
        self.casts.register(from, to, func);
    }

    pub fn cast_value(&mut self, value: RtValue, to: TypeId) -> Result<RtValue, RuntimeError> {
        let from = value.type_id();
        if from == to {
            return Ok(value);
        }

        let Some(cast) = self.casts.lookup(from, to) else {
            let from_name = self.types.name_of(from).unwrap_or("<unknown>").to_string();
            let to_name = self.types.name_of(to).unwrap_or("<unknown>").to_string();
            return Err(RuntimeError::CastNotFound {
                from: from_name,
                to: to_name,
            });
        };

        (cast)(self, value)
    }

    pub fn install_builtin_casts(&mut self) {
        let numeric_types = [
            TypeId::BOOL,
            TypeId::I8,
            TypeId::I16,
            TypeId::I32,
            TypeId::I64,
            TypeId::I128,
            TypeId::U8,
            TypeId::U16,
            TypeId::U32,
            TypeId::U64,
            TypeId::U128,
            TypeId::F32,
            TypeId::F64,
        ];

        for from in numeric_types {
            for to in numeric_types {
                if from == to {
                    continue;
                }
                self.register_cast(
                    from,
                    to,
                    Arc::new(move |rt, value| cast_numeric(rt, value, to)),
                );
            }
        }
    }
}

fn cast_numeric(
    rt: &mut super::Runtime,
    value: RtValue,
    to: TypeId,
) -> Result<RtValue, RuntimeError> {
    if is_float_type(to) {
        let as_f64 = match value {
            RtValue::Bool(v) => {
                if v {
                    1.0
                } else {
                    0.0
                }
            }
            RtValue::Int { value, .. } => value as f64,
            RtValue::UInt { value, .. } => value as f64,
            RtValue::Float { value, .. } => value,
            other => {
                let found = rt.types.name_of(other.type_id()).unwrap_or("<unknown>");
                return Err(RuntimeError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: found.to_string(),
                });
            }
        };
        let value = if to == TypeId::F32 {
            (as_f64 as f32) as f64
        } else {
            as_f64
        };
        return Ok(RtValue::Float { ty: to, value });
    }

    if is_uint_type(to) {
        let as_u128 = match value {
            RtValue::Bool(v) => {
                if v {
                    1
                } else {
                    0
                }
            }
            RtValue::Int { value, .. } => value as u128,
            RtValue::UInt { value, .. } => value,
            RtValue::Float { value, .. } => value as u128,
            other => {
                let found = rt.types.name_of(other.type_id()).unwrap_or("<unknown>");
                return Err(RuntimeError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: found.to_string(),
                });
            }
        };
        return Ok(RtValue::UInt {
            ty: to,
            value: as_u128,
        });
    }

    if is_int_type(to) {
        let as_i128 = match value {
            RtValue::Bool(v) => {
                if v {
                    1
                } else {
                    0
                }
            }
            RtValue::Int { value, .. } => value,
            RtValue::UInt { value, .. } => value as i128,
            RtValue::Float { value, .. } => value as i128,
            other => {
                let found = rt.types.name_of(other.type_id()).unwrap_or("<unknown>");
                return Err(RuntimeError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: found.to_string(),
                });
            }
        };
        return Ok(RtValue::Int {
            ty: to,
            value: as_i128,
        });
    }

    if to == TypeId::BOOL {
        let value = match value {
            RtValue::Bool(v) => v,
            RtValue::Int { value, .. } => value != 0,
            RtValue::UInt { value, .. } => value != 0,
            RtValue::Float { value, .. } => value != 0.0,
            other => {
                let found = rt.types.name_of(other.type_id()).unwrap_or("<unknown>");
                return Err(RuntimeError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: found.to_string(),
                });
            }
        };
        return Ok(RtValue::Bool(value));
    }

    Err(RuntimeError::TypeMismatch {
        expected: "numeric".to_string(),
        found: rt
            .types
            .name_of(value.type_id())
            .unwrap_or("<unknown>")
            .to_string(),
    })
}

fn is_int_type(ty: TypeId) -> bool {
    matches!(
        ty,
        TypeId::I8 | TypeId::I16 | TypeId::I32 | TypeId::I64 | TypeId::I128
    )
}

fn is_uint_type(ty: TypeId) -> bool {
    matches!(
        ty,
        TypeId::U8 | TypeId::U16 | TypeId::U32 | TypeId::U64 | TypeId::U128
    )
}

fn is_float_type(ty: TypeId) -> bool {
    matches!(ty, TypeId::F32 | TypeId::F64)
}
