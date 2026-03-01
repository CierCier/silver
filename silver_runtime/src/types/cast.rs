use super::error::ValueError;
use super::registry::{TypeId, TypeRegistry};
use super::type_info::TypeKind;
use super::value::Value;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CastKind {
    Identity,
    IntToInt {
        from_bits: u16,
        from_signed: bool,
        to_bits: u16,
        to_signed: bool,
    },
    IntToFloat {
        from_bits: u16,
        from_signed: bool,
        to_bits: u16,
    },
    FloatToInt {
        from_bits: u16,
        to_bits: u16,
        to_signed: bool,
    },
    FloatToFloat {
        from_bits: u16,
        to_bits: u16,
    },
}

impl TypeRegistry {
    pub fn cast_kind(
        &self,
        from: TypeId,
        to: TypeId,
    ) -> Result<Option<CastKind>, super::error::TypeError> {
        if from == to {
            return Ok(Some(CastKind::Identity));
        }

        let from_kind = &self.get(from)?.kind;
        let to_kind = &self.get(to)?.kind;

        match (from_kind, to_kind) {
            (
                TypeKind::Int {
                    bits: fb,
                    signed: fs,
                },
                TypeKind::Int {
                    bits: tb,
                    signed: ts,
                },
            ) => Ok(Some(CastKind::IntToInt {
                from_bits: *fb,
                from_signed: *fs,
                to_bits: *tb,
                to_signed: *ts,
            })),
            (
                TypeKind::Int {
                    bits: fb,
                    signed: fs,
                },
                TypeKind::Float { bits: tb },
            ) => Ok(Some(CastKind::IntToFloat {
                from_bits: *fb,
                from_signed: *fs,
                to_bits: *tb,
            })),
            (
                TypeKind::Float { bits: fb },
                TypeKind::Int {
                    bits: tb,
                    signed: ts,
                },
            ) => Ok(Some(CastKind::FloatToInt {
                from_bits: *fb,
                to_bits: *tb,
                to_signed: *ts,
            })),
            (TypeKind::Float { bits: fb }, TypeKind::Float { bits: tb }) => {
                Ok(Some(CastKind::FloatToFloat {
                    from_bits: *fb,
                    to_bits: *tb,
                }))
            }
            _ => Ok(None),
        }
    }

    pub fn can_cast(&self, from: TypeId, to: TypeId) -> Result<bool, super::error::TypeError> {
        Ok(self.cast_kind(from, to)?.is_some())
    }

    pub fn cast_value(&self, value: &Value, to: TypeId) -> Result<Value, ValueError> {
        let from = value.type_id();
        let kind = self
            .cast_kind(from, to)
            .map_err(|e| ValueError::TypeMismatch {
                expected: "known type ids".to_string(),
                found: e.to_string(),
            })?;

        let Some(kind) = kind else {
            let from_name = self.name_of(from).ok().unwrap_or("<unknown>").to_string();
            let to_name = self.name_of(to).ok().unwrap_or("<unknown>").to_string();
            return Err(ValueError::CannotCast {
                from: from_name,
                to: to_name,
            });
        };

        match kind {
            CastKind::Identity => Ok(value.clone()),
            CastKind::IntToInt {
                to_bits, to_signed, ..
            } => match value {
                Value::Int { value: v, .. } => {
                    if to_signed {
                        Ok(Value::Int {
                            ty: to,
                            value: cast_i128_to_signed_bits(*v, to_bits),
                        })
                    } else {
                        Ok(Value::UInt {
                            ty: to,
                            value: cast_i128_to_unsigned_bits(*v, to_bits),
                        })
                    }
                }
                Value::UInt { value: v, .. } => {
                    if to_signed {
                        Ok(Value::Int {
                            ty: to,
                            value: cast_u128_to_signed_bits(*v, to_bits),
                        })
                    } else {
                        Ok(Value::UInt {
                            ty: to,
                            value: cast_u128_to_unsigned_bits(*v, to_bits),
                        })
                    }
                }
                other => Err(type_mismatch(self, "int", other.type_id())),
            },
            CastKind::IntToFloat { to_bits, .. } => match value {
                Value::Int { value: v, .. } => Ok(Value::Float {
                    ty: to,
                    value: normalize_float(*v as f64, to_bits),
                }),
                Value::UInt { value: v, .. } => Ok(Value::Float {
                    ty: to,
                    value: normalize_float(*v as f64, to_bits),
                }),
                other => Err(type_mismatch(self, "int", other.type_id())),
            },
            CastKind::FloatToFloat { to_bits, .. } => match value {
                Value::Float { value: v, .. } => Ok(Value::Float {
                    ty: to,
                    value: normalize_float(*v, to_bits),
                }),
                other => Err(type_mismatch(self, "float", other.type_id())),
            },
            CastKind::FloatToInt {
                to_bits, to_signed, ..
            } => match value {
                Value::Float { value: v, .. } => {
                    if to_signed {
                        Ok(Value::Int {
                            ty: to,
                            value: cast_f64_to_signed_bits(*v, to_bits),
                        })
                    } else {
                        Ok(Value::UInt {
                            ty: to,
                            value: cast_f64_to_unsigned_bits(*v, to_bits),
                        })
                    }
                }
                other => Err(type_mismatch(self, "float", other.type_id())),
            },
        }
    }
}

fn type_mismatch(reg: &TypeRegistry, expected: &str, found: TypeId) -> ValueError {
    let found = reg.name_of(found).ok().unwrap_or("<unknown>").to_string();
    ValueError::TypeMismatch {
        expected: expected.to_string(),
        found,
    }
}

fn normalize_float(v: f64, bits: u16) -> f64 {
    match bits {
        32 => (v as f32) as f64,
        64 => v,
        _ => v,
    }
}

fn cast_i128_to_signed_bits(v: i128, bits: u16) -> i128 {
    match bits {
        8 => (v as i8) as i128,
        16 => (v as i16) as i128,
        32 => (v as i32) as i128,
        64 => (v as i64) as i128,
        128 => v,
        _ => v,
    }
}

fn cast_i128_to_unsigned_bits(v: i128, bits: u16) -> u128 {
    match bits {
        8 => (v as u8) as u128,
        16 => (v as u16) as u128,
        32 => (v as u32) as u128,
        64 => (v as u64) as u128,
        128 => v as u128,
        _ => v as u128,
    }
}

fn cast_u128_to_signed_bits(v: u128, bits: u16) -> i128 {
    match bits {
        8 => (v as i8) as i128,
        16 => (v as i16) as i128,
        32 => (v as i32) as i128,
        64 => (v as i64) as i128,
        128 => v as i128,
        _ => v as i128,
    }
}

fn cast_u128_to_unsigned_bits(v: u128, bits: u16) -> u128 {
    match bits {
        8 => (v as u8) as u128,
        16 => (v as u16) as u128,
        32 => (v as u32) as u128,
        64 => (v as u64) as u128,
        128 => v,
        _ => v,
    }
}

fn cast_f64_to_signed_bits(v: f64, bits: u16) -> i128 {
    match bits {
        8 => (v as i8) as i128,
        16 => (v as i16) as i128,
        32 => (v as i32) as i128,
        64 => (v as i64) as i128,
        128 => v as i128,
        _ => v as i128,
    }
}

fn cast_f64_to_unsigned_bits(v: f64, bits: u16) -> u128 {
    match bits {
        8 => (v as u8) as u128,
        16 => (v as u16) as u128,
        32 => (v as u32) as u128,
        64 => (v as u64) as u128,
        128 => v as u128,
        _ => v as u128,
    }
}
