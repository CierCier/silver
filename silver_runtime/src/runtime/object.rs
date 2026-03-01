use super::heap::Handle;
use super::registry::TypeId;
use super::value::RtValue;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Vec {
        ty: TypeId,
        items: Vec<RtValue>,
    },
    Struct {
        ty: TypeId,
        fields: Vec<RtValue>,
    },
    Enum {
        ty: TypeId,
        variant: usize,
        fields: Vec<RtValue>,
    },
}

impl Object {
    pub fn push_handles(&self, out: &mut Vec<Handle>) {
        match self {
            Object::String(_) => {}
            Object::Vec { items, .. } => {
                for v in items {
                    v.push_handles(out);
                }
            }
            Object::Struct { fields, .. } => {
                for v in fields {
                    v.push_handles(out);
                }
            }
            Object::Enum { fields, .. } => {
                for v in fields {
                    v.push_handles(out);
                }
            }
        }
    }
}
