pub mod casts;
pub mod error;
pub mod ffi;
pub mod heap;
pub mod methods;
pub mod object;
pub mod registry;
pub mod value;

use crate::types::TypeRegistry;

use casts::CastRegistry;
use methods::MethodRegistry;

#[derive(Debug, Default)]
pub struct Runtime {
    pub types: TypeRegistry,
    pub heap: heap::Heap,
    pub methods: MethodRegistry,
    pub casts: CastRegistry,
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Self {
            types: TypeRegistry::new(),
            heap: heap::Heap::new(),
            methods: MethodRegistry::new(),
            casts: CastRegistry::new(),
        };
        rt.install_builtin_methods();
        rt.install_builtin_casts();
        rt
    }
}

#[cfg(test)]
mod tests;
