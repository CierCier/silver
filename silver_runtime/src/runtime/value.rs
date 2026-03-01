use super::heap::Handle;
use super::registry::TypeId;

#[derive(Debug, Clone, PartialEq)]
pub enum RtValue {
    Unit,
    Bool(bool),
    Int { ty: TypeId, value: i128 },
    UInt { ty: TypeId, value: u128 },
    Float { ty: TypeId, value: f64 },
    Obj { ty: TypeId, handle: Handle },
}

impl RtValue {
    pub fn type_id(&self) -> TypeId {
        match self {
            RtValue::Unit => TypeId::UNIT,
            RtValue::Bool(_) => TypeId::BOOL,
            RtValue::Int { ty, .. } => *ty,
            RtValue::UInt { ty, .. } => *ty,
            RtValue::Float { ty, .. } => *ty,
            RtValue::Obj { ty, .. } => *ty,
        }
    }

    pub fn push_handles(&self, out: &mut Vec<Handle>) {
        if let RtValue::Obj { handle, .. } = self {
            out.push(*handle);
        }
    }
}
