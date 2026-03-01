use std::collections::HashMap;
use std::sync::Arc;

use crate::types::TypeKind;

use super::error::RuntimeError;
use super::object::Object;
use super::registry::TypeId;
use super::value::RtValue;

pub type NativeMethod = Arc<
    dyn Fn(&mut super::Runtime, RtValue, &[RtValue]) -> Result<RtValue, RuntimeError> + Send + Sync,
>;

#[derive(Clone)]
pub struct MethodSpec {
    pub arity: Option<usize>,
    pub func: NativeMethod,
}

impl std::fmt::Debug for MethodSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MethodSpec")
            .field("arity", &self.arity)
            .finish_non_exhaustive()
    }
}

#[derive(Default)]
pub struct MethodRegistry {
    by_type: HashMap<(TypeId, Arc<str>), MethodSpec>,
    global: HashMap<Arc<str>, MethodSpec>,
}

impl std::fmt::Debug for MethodRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MethodRegistry")
            .field("by_type", &self.by_type.len())
            .field("global", &self.global.len())
            .finish()
    }
}

impl MethodRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(
        &mut self,
        ty: TypeId,
        name: impl Into<Arc<str>>,
        arity: Option<usize>,
        func: NativeMethod,
    ) {
        self.by_type
            .insert((ty, name.into()), MethodSpec { arity, func });
    }

    pub fn register_global(
        &mut self,
        name: impl Into<Arc<str>>,
        arity: Option<usize>,
        func: NativeMethod,
    ) {
        self.global.insert(name.into(), MethodSpec { arity, func });
    }

    pub fn lookup(&self, ty: TypeId, name: &str) -> Option<&MethodSpec> {
        self.by_type
            .get(&(ty, Arc::from(name)))
            .or_else(|| self.global.get(name))
    }
}

impl super::Runtime {
    pub fn alloc_string(&mut self, s: impl Into<String>) -> RtValue {
        let handle = self.heap.alloc(Object::String(s.into()));
        RtValue::Obj {
            ty: TypeId::STRING,
            handle,
        }
    }

    pub fn register_method(
        &mut self,
        ty: TypeId,
        name: &str,
        arity: Option<usize>,
        func: NativeMethod,
    ) {
        self.methods.register(ty, Arc::from(name), arity, func);
    }

    pub fn register_global_method(&mut self, name: &str, arity: Option<usize>, func: NativeMethod) {
        self.methods.register_global(Arc::from(name), arity, func);
    }

    pub fn call_method(
        &mut self,
        receiver: RtValue,
        name: &str,
        args: &[RtValue],
    ) -> Result<RtValue, RuntimeError> {
        let ty = receiver.type_id();
        let Some(spec) = self.methods.lookup(ty, name) else {
            let type_name = self.types.name_of(ty).unwrap_or("<unknown>").to_string();
            return Err(RuntimeError::MethodNotFound {
                type_name,
                method: name.to_string(),
            });
        };

        let arity = spec.arity;
        let func = spec.func.clone();

        if let Some(expected) = arity {
            if expected != args.len() {
                return Err(RuntimeError::ArityMismatch {
                    expected,
                    found: args.len(),
                });
            }
        }

        (func)(self, receiver, args)
    }

    pub fn install_builtin_methods(&mut self) {
        // Global: type_name() -> string
        self.register_global_method(
            "type_name",
            Some(0),
            Arc::new(|rt, receiver, _args| {
                let ty = receiver.type_id();
                let name = rt.types.name_of(ty).unwrap_or("<unknown>").to_string();
                Ok(rt.alloc_string(name))
            }),
        );

        // string.len() -> i64
        self.register_method(
            TypeId::STRING,
            "len",
            Some(0),
            Arc::new(|rt, receiver, _args| {
                let RtValue::Obj { handle, .. } = receiver else {
                    return Err(RuntimeError::InvalidReceiver {
                        expected: "string object".to_string(),
                    });
                };

                let obj = rt.heap.get(handle)?;
                let Object::String(s) = obj else {
                    return Err(RuntimeError::InvalidReceiver {
                        expected: "string object".to_string(),
                    });
                };

                Ok(RtValue::Int {
                    ty: TypeId::I64,
                    value: s.chars().count() as i128,
                })
            }),
        );

        // vec.len() -> i64 (works for any vec<T>)
        // Implemented as a global fallback that checks TypeKind::Vec.
        self.register_global_method(
            "len",
            Some(0),
            Arc::new(|rt, receiver, _args| {
                let ty = receiver.type_id();
                let kind = rt.types.get(ty).map_err(|e| RuntimeError::TypeMismatch {
                    expected: "known type".to_string(),
                    found: e.to_string(),
                })?;

                match &kind.kind {
                    TypeKind::Vec { .. } => {
                        let RtValue::Obj { handle, .. } = receiver else {
                            return Err(RuntimeError::InvalidReceiver {
                                expected: "vec object".to_string(),
                            });
                        };
                        let obj = rt.heap.get(handle)?;
                        let Object::Vec { items, .. } = obj else {
                            return Err(RuntimeError::InvalidReceiver {
                                expected: "vec object".to_string(),
                            });
                        };
                        Ok(RtValue::Int {
                            ty: TypeId::I64,
                            value: items.len() as i128,
                        })
                    }
                    _ => {
                        // If a type-specific len exists, it would have been found earlier.
                        Err(RuntimeError::MethodNotFound {
                            type_name: rt.types.name_of(ty).unwrap_or("<unknown>").to_string(),
                            method: "len".to_string(),
                        })
                    }
                }
            }),
        );

        // vec.push(value) -> unit (works for any vec<T>)
        self.register_global_method(
            "push",
            Some(1),
            Arc::new(|rt, receiver, args| {
                let vec_ty = receiver.type_id();
                let type_info = rt
                    .types
                    .get(vec_ty)
                    .map_err(|e| RuntimeError::TypeMismatch {
                        expected: "known type".to_string(),
                        found: e.to_string(),
                    })?;

                let elem_ty = match &type_info.kind {
                    TypeKind::Vec { elem } => *elem,
                    _ => {
                        return Err(RuntimeError::InvalidReceiver {
                            expected: "vec".to_string(),
                        });
                    }
                };

                let RtValue::Obj { handle, .. } = receiver else {
                    return Err(RuntimeError::InvalidReceiver {
                        expected: "vec object".to_string(),
                    });
                };

                let item = args[0].clone();
                if item.type_id() != elem_ty {
                    return Err(RuntimeError::TypeMismatch {
                        expected: rt.types.name_of(elem_ty).unwrap_or("<unknown>").to_string(),
                        found: rt
                            .types
                            .name_of(item.type_id())
                            .unwrap_or("<unknown>")
                            .to_string(),
                    });
                }

                let obj = rt.heap.get_mut(handle)?;
                let Object::Vec { items, .. } = obj else {
                    return Err(RuntimeError::InvalidReceiver {
                        expected: "vec object".to_string(),
                    });
                };

                items.push(item);
                Ok(RtValue::Unit)
            }),
        );
    }
}
