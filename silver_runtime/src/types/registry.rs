use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::error::TypeError;
use super::type_info::{EnumType, EnumVariant, Field, FunctionType, StructType, Type, TypeKind};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(pub(crate) u32);

impl TypeId {
    pub const UNIT: TypeId = TypeId(0);
    pub const BOOL: TypeId = TypeId(1);
    pub const I8: TypeId = TypeId(2);
    pub const I16: TypeId = TypeId(3);
    pub const I32: TypeId = TypeId(4);
    pub const I64: TypeId = TypeId(5);
    pub const I128: TypeId = TypeId(6);
    pub const U8: TypeId = TypeId(7);
    pub const U16: TypeId = TypeId(8);
    pub const U32: TypeId = TypeId(9);
    pub const U64: TypeId = TypeId(10);
    pub const U128: TypeId = TypeId(11);
    pub const F32: TypeId = TypeId(12);
    pub const F64: TypeId = TypeId(13);
    pub const STRING: TypeId = TypeId(14);
}

#[derive(Debug)]
pub struct TypeRegistry {
    types: Vec<Type>,
    by_name: HashMap<Arc<str>, TypeId>,
    vec_cache: HashMap<TypeId, TypeId>,
    optional_cache: HashMap<TypeId, TypeId>,
    fn_cache: HashMap<(Vec<TypeId>, TypeId), TypeId>,
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            types: Vec::new(),
            by_name: HashMap::new(),
            vec_cache: HashMap::new(),
            optional_cache: HashMap::new(),
            fn_cache: HashMap::new(),
        };
        registry.add_builtin(TypeId::UNIT, "unit", TypeKind::Unit);
        registry.add_builtin(TypeId::BOOL, "bool", TypeKind::Bool);
        registry.add_builtin(
            TypeId::I8,
            "i8",
            TypeKind::Int {
                bits: 8,
                signed: true,
            },
        );
        registry.add_builtin(
            TypeId::I16,
            "i16",
            TypeKind::Int {
                bits: 16,
                signed: true,
            },
        );
        registry.add_builtin(
            TypeId::I32,
            "i32",
            TypeKind::Int {
                bits: 32,
                signed: true,
            },
        );
        registry.add_builtin(
            TypeId::I64,
            "i64",
            TypeKind::Int {
                bits: 64,
                signed: true,
            },
        );
        registry.add_builtin(
            TypeId::I128,
            "i128",
            TypeKind::Int {
                bits: 128,
                signed: true,
            },
        );
        registry.add_builtin(
            TypeId::U8,
            "u8",
            TypeKind::Int {
                bits: 8,
                signed: false,
            },
        );
        registry.add_builtin(
            TypeId::U16,
            "u16",
            TypeKind::Int {
                bits: 16,
                signed: false,
            },
        );
        registry.add_builtin(
            TypeId::U32,
            "u32",
            TypeKind::Int {
                bits: 32,
                signed: false,
            },
        );
        registry.add_builtin(
            TypeId::U64,
            "u64",
            TypeKind::Int {
                bits: 64,
                signed: false,
            },
        );
        registry.add_builtin(
            TypeId::U128,
            "u128",
            TypeKind::Int {
                bits: 128,
                signed: false,
            },
        );
        registry.add_builtin(TypeId::F32, "f32", TypeKind::Float { bits: 32 });
        registry.add_builtin(TypeId::F64, "f64", TypeKind::Float { bits: 64 });
        registry.add_builtin(TypeId::STRING, "string", TypeKind::String);
        registry
    }

    fn add_builtin(&mut self, id: TypeId, name: &str, kind: TypeKind) {
        debug_assert_eq!(self.types.len() as u32, id.0);
        let name: Arc<str> = Arc::from(name);
        let ty = Type {
            id,
            name: name.clone(),
            kind,
        };
        self.types.push(ty);
        self.by_name.insert(name, id);
    }

    pub fn get(&self, id: TypeId) -> Result<&Type, TypeError> {
        self.types
            .get(id.0 as usize)
            .ok_or(TypeError::UnknownTypeId { id: id.0 })
    }

    pub fn lookup(&self, name: &str) -> Option<TypeId> {
        self.by_name.get(name).copied()
    }

    pub fn name_of(&self, id: TypeId) -> Result<&str, TypeError> {
        Ok(self.get(id)?.name.as_ref())
    }

    pub fn vec_of(&mut self, elem: TypeId) -> Result<TypeId, TypeError> {
        if let Some(existing) = self.vec_cache.get(&elem).copied() {
            return Ok(existing);
        }

        let elem_name = self.name_of(elem)?.to_string();
        let name: Arc<str> = Arc::from(format!("vec<{elem_name}>"));

        let id = self.alloc_type(name.clone(), TypeKind::Vec { elem })?;
        self.vec_cache.insert(elem, id);
        Ok(id)
    }

    pub fn optional_of(&mut self, inner: TypeId) -> Result<TypeId, TypeError> {
        if let Some(existing) = self.optional_cache.get(&inner).copied() {
            return Ok(existing);
        }

        let inner_name = self.name_of(inner)?.to_string();
        let name: Arc<str> = Arc::from(format!("optional<{inner_name}>"));

        let id = self.alloc_type(name.clone(), TypeKind::Optional { inner })?;
        self.optional_cache.insert(inner, id);
        Ok(id)
    }

    pub fn function(&mut self, params: Vec<TypeId>, ret: TypeId) -> Result<TypeId, TypeError> {
        if let Some(existing) = self.fn_cache.get(&(params.clone(), ret)).copied() {
            return Ok(existing);
        }

        let mut name = String::from("fn(");
        for (i, p) in params.iter().enumerate() {
            if i > 0 {
                name.push_str(", ");
            }
            name.push_str(self.name_of(*p)?);
        }
        name.push_str(") -> ");
        name.push_str(self.name_of(ret)?);

        let name: Arc<str> = Arc::from(name);
        let id = self.alloc_type(
            name.clone(),
            TypeKind::Function(FunctionType {
                params: params.clone(),
                ret,
            }),
        )?;

        self.fn_cache.insert((params, ret), id);
        Ok(id)
    }

    pub fn define_struct(
        &mut self,
        name: impl Into<Arc<str>>,
        fields: Vec<(impl Into<Arc<str>>, TypeId)>,
    ) -> Result<TypeId, TypeError> {
        let name = name.into();
        if self.by_name.contains_key(&name) {
            return Err(TypeError::DuplicateTypeName {
                name: name.to_string(),
            });
        }

        let mut seen = HashSet::<Arc<str>>::new();
        let mut field_defs = Vec::with_capacity(fields.len());
        for (field_name, field_ty) in fields {
            let field_name: Arc<str> = field_name.into();
            if !seen.insert(field_name.clone()) {
                return Err(TypeError::DuplicateFieldName {
                    type_name: name.to_string(),
                    field: field_name.to_string(),
                });
            }
            // Ensure referenced type exists
            self.get(field_ty)?;
            field_defs.push(Field {
                name: field_name,
                ty: field_ty,
            });
        }

        self.alloc_type(
            name.clone(),
            TypeKind::Struct(StructType { fields: field_defs }),
        )
    }

    pub fn define_enum(
        &mut self,
        name: impl Into<Arc<str>>,
        variants: Vec<(impl Into<Arc<str>>, Vec<TypeId>)>,
    ) -> Result<TypeId, TypeError> {
        self.define_enum_with_discriminants(
            name,
            variants
                .into_iter()
                .enumerate()
                .map(|(i, (n, payload))| (n, i as i64, payload))
                .collect(),
        )
    }

    pub fn define_enum_with_discriminants(
        &mut self,
        name: impl Into<Arc<str>>,
        variants: Vec<(impl Into<Arc<str>>, i64, Vec<TypeId>)>,
    ) -> Result<TypeId, TypeError> {
        let name = name.into();
        if self.by_name.contains_key(&name) {
            return Err(TypeError::DuplicateTypeName {
                name: name.to_string(),
            });
        }

        let mut seen_names = HashSet::<Arc<str>>::new();
        let mut seen_discriminants = HashSet::<i64>::new();
        let mut variant_defs = Vec::with_capacity(variants.len());
        for (variant_name, discriminant, payload) in variants {
            let variant_name: Arc<str> = variant_name.into();
            if !seen_names.insert(variant_name.clone()) {
                return Err(TypeError::DuplicateVariantName {
                    type_name: name.to_string(),
                    variant: variant_name.to_string(),
                });
            }
            if !seen_discriminants.insert(discriminant) {
                return Err(TypeError::DuplicateVariantDiscriminant {
                    type_name: name.to_string(),
                    discriminant,
                });
            }
            for ty in &payload {
                self.get(*ty)?;
            }
            variant_defs.push(EnumVariant {
                name: variant_name,
                discriminant,
                payload,
            });
        }

        self.alloc_type(
            name.clone(),
            TypeKind::Enum(EnumType {
                variants: variant_defs,
            }),
        )
    }

    /// Defines an enum where each variant can optionally specify its discriminant.
    ///
    /// Auto-fill semantics (C-style):
    /// - The first unspecified discriminant becomes 0.
    /// - After an explicit discriminant `X`, the next unspecified discriminant becomes `X + 1`.
    /// - Each subsequent unspecified discriminant increments by 1.
    pub fn define_enum_autofill_discriminants(
        &mut self,
        name: impl Into<Arc<str>>,
        variants: Vec<(impl Into<Arc<str>>, Option<i64>, Vec<TypeId>)>,
    ) -> Result<TypeId, TypeError> {
        let name_arc: Arc<str> = name.into();
        let mut next: i64 = 0;

        let mut resolved: Vec<(Arc<str>, i64, Vec<TypeId>)> = Vec::with_capacity(variants.len());
        for (variant_name, disc, payload) in variants {
            let disc = match disc {
                Some(d) => {
                    next = d.checked_add(1).ok_or(TypeError::DiscriminantOverflow {
                        type_name: name_arc.to_string(),
                        last: d,
                    })?;
                    d
                }
                None => {
                    let d = next;
                    next = next.checked_add(1).ok_or(TypeError::DiscriminantOverflow {
                        type_name: name_arc.to_string(),
                        last: d,
                    })?;
                    d
                }
            };

            resolved.push((variant_name.into(), disc, payload));
        }

        // Rebuild into the expected input type while keeping the computed Arc<str> names.
        let resolved = resolved.into_iter().map(|(n, d, p)| (n, d, p)).collect();
        self.define_enum_with_discriminants(name_arc, resolved)
    }

    pub fn enum_variant_index(
        &self,
        enum_ty: TypeId,
        name: &str,
    ) -> Result<Option<usize>, TypeError> {
        let ty = self.get(enum_ty)?;
        let TypeKind::Enum(en) = &ty.kind else {
            return Ok(None);
        };
        Ok(en.variants.iter().position(|v| v.name.as_ref() == name))
    }

    pub fn enum_variant(
        &self,
        enum_ty: TypeId,
        index: usize,
    ) -> Result<Option<&EnumVariant>, TypeError> {
        let ty = self.get(enum_ty)?;
        let TypeKind::Enum(en) = &ty.kind else {
            return Ok(None);
        };
        Ok(en.variants.get(index))
    }

    pub fn enum_variant_by_discriminant(
        &self,
        enum_ty: TypeId,
        discriminant: i64,
    ) -> Result<Option<usize>, TypeError> {
        let ty = self.get(enum_ty)?;
        let TypeKind::Enum(en) = &ty.kind else {
            return Ok(None);
        };
        Ok(en
            .variants
            .iter()
            .position(|v| v.discriminant == discriminant))
    }

    fn alloc_type(&mut self, name: Arc<str>, kind: TypeKind) -> Result<TypeId, TypeError> {
        if self.by_name.contains_key(&name) {
            return Err(TypeError::DuplicateTypeName {
                name: name.to_string(),
            });
        }

        let id = TypeId(self.types.len() as u32);
        let ty = Type {
            id,
            name: name.clone(),
            kind,
        };
        self.types.push(ty);
        self.by_name.insert(name, id);
        Ok(id)
    }
}
