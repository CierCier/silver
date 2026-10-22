//! TypeProperties — single `is_copy`/`needs_drop` source (bool/i64/f64/ptr + all-Copy struct = Copy).
//! Why: centralize heuristics from typeck/move_check/codegen; pure queries with `TypePropertiesContext` for struct recursion.

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::parser::ast;

/// Copy vs owning: `is_copy` + `needs_drop`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeProperties {
    pub is_copy: bool,
    pub needs_drop: bool,
}

/// Context for struct recursion and drop-owner lookup (pure data).
/// - `drop_bases`: bare names with `Drop` impl (e.g. "String")
/// - `struct_fields`: field types for recursion; missing = unknown
/// - `type_parameters`: declaration-order parameters substituted in concrete fields
#[derive(Debug, Clone, Default)]
pub struct TypePropertiesContext {
    pub drop_bases: HashSet<String>,
    pub struct_fields: HashMap<String, Vec<ast::Type>>,
    pub type_parameters: HashMap<String, Vec<String>>,
}

impl TypePropertiesContext {
    pub fn new() -> Self {
        Self {
            drop_bases: default_drop_bases(),
            struct_fields: HashMap::default(),
            type_parameters: HashMap::default(),
        }
    }

    pub fn with_drop_bases(bases: impl IntoIterator<Item = String>) -> Self {
        Self {
            drop_bases: bases.into_iter().collect(),
            struct_fields: HashMap::default(),
            type_parameters: HashMap::default(),
        }
    }

    pub fn insert_struct(&mut self, name: impl Into<String>, fields: Vec<ast::Type>) {
        self.insert_generic_struct(name, Vec::new(), fields);
    }

    pub fn insert_generic_struct(
        &mut self,
        name: impl Into<String>,
        parameters: Vec<String>,
        fields: Vec<ast::Type>,
    ) {
        let name = name.into();
        self.type_parameters.insert(name.clone(), parameters);
        self.struct_fields.insert(name, fields);
    }

    fn fields_for(&self, named: &ast::NamedType) -> Option<Vec<ast::Type>> {
        let name = &named.path.first()?.name;
        let fields = self.struct_fields.get(name)?;
        let parameters = self.type_parameters.get(name);
        let mapping: HashMap<_, _> = parameters
            .into_iter()
            .flatten()
            .zip(named.generics.iter().flatten())
            .map(|(parameter, argument)| {
                (parameter.clone(), crate::types::Type::from_ast(argument))
            })
            .collect();
        Some(
            fields
                .iter()
                .map(|field| {
                    crate::types::Type::from_ast(field)
                        .substitute(&mapping)
                        .to_ast()
                })
                .collect(),
        )
    }

    pub fn with_struct(mut self, name: impl Into<String>, fields: Vec<ast::Type>) -> Self {
        self.insert_struct(name, fields);
        self
    }
}

fn default_drop_bases() -> HashSet<String> {
    // Unified owned bases (String/Vec/… non-Copy) — central list for Phase 5.
    [
        "String",
        "Vec",
        "VecIter",
        "Bytes",
        "BytesIntoIter",
        "HashMap",
        "HashSet",
        "Deque",
        "BinaryHeap",
        "Queue",
        "Box",
        "Rc",
        "Arena",
        "File",
        "BufWriter",
        "Scanner",
        "ByteStream",
        "Channel",
        "RawMutex",
        "Mutex",
        "Socket",
        "TcpStream",
        "TcpListener",
        "UdpSocket",
        "HttpConnection",
        // generic collection aliases that have Drop in std
        "Set",
        "Map",
    ]
    .into_iter()
    .map(|s| s.to_string())
    .collect()
}

fn is_copy_primitive(p: &ast::PrimitiveType) -> bool {
    matches!(
        p,
        ast::PrimitiveType::Bool
            | ast::PrimitiveType::I8
            | ast::PrimitiveType::I16
            | ast::PrimitiveType::I32
            | ast::PrimitiveType::I64
            | ast::PrimitiveType::I128
            | ast::PrimitiveType::U8
            | ast::PrimitiveType::U16
            | ast::PrimitiveType::U32
            | ast::PrimitiveType::U64
            | ast::PrimitiveType::U128
            | ast::PrimitiveType::F32
            | ast::PrimitiveType::F64
            | ast::PrimitiveType::F80
            | ast::PrimitiveType::C32
            | ast::PrimitiveType::C64
            | ast::PrimitiveType::C80
            | ast::PrimitiveType::Char
            | ast::PrimitiveType::Str
    )
}

// Simple (no-context) queries — hardcoded bases; unknown named struct = Copy unless drop owner.

/// Copy per implicit rule: primitives/ptr/ref/slice Copy; tuple/array all-Copy; named non-drop-owner Copy.
pub fn is_copy(ty: &ast::Type) -> bool {
    is_copy_with(ty, &TypePropertiesContext::new())
}

/// Direct or transitive Drop (context-free: only direct owners + tuple/array recursion).
pub fn needs_drop(ty: &ast::Type) -> bool {
    needs_drop_with(ty, &TypePropertiesContext::new())
}

/// Pair query without context.
pub fn properties_for(ty: &ast::Type) -> TypeProperties {
    TypeProperties {
        is_copy: is_copy(ty),
        needs_drop: needs_drop(ty),
    }
}

// Context-aware queries — recursion enabled when name in `ctx.struct_fields`.

pub fn is_copy_with(ty: &ast::Type, ctx: &TypePropertiesContext) -> bool {
    let mut visited = HashSet::default();
    is_copy_inner(ty, ctx, &mut visited)
}

pub fn needs_drop_with(ty: &ast::Type, ctx: &TypePropertiesContext) -> bool {
    let mut visited = HashSet::default();
    needs_drop_inner(ty, ctx, &mut visited)
}

pub fn properties_for_with(ty: &ast::Type, ctx: &TypePropertiesContext) -> TypeProperties {
    TypeProperties {
        is_copy: is_copy_with(ty, ctx),
        needs_drop: needs_drop_with(ty, ctx),
    }
}

// Keep alias names matching spec alternative.
#[allow(dead_code)]
pub fn properties_for_ctx(ty: &ast::Type, ctx: &TypePropertiesContext) -> TypeProperties {
    properties_for_with(ty, ctx)
}

fn is_copy_inner(
    ty: &ast::Type,
    context: &TypePropertiesContext,
    visited: &mut HashSet<String>,
) -> bool {
    match ty.kind.as_ref() {
        ast::TypeKind::Primitive(p) => is_copy_primitive(p),
        ast::TypeKind::Pointer(_) => true,
        ast::TypeKind::Reference(_) => true,
        ast::TypeKind::Array(a) => is_copy_inner(&a.element_type, context, visited),
        ast::TypeKind::Tuple(ts) => ts.iter().all(|t| is_copy_inner(t, context, visited)),
        ast::TypeKind::Slice(_) => true, // view, no ownership
        ast::TypeKind::Optional(inner) => is_copy_inner(inner, context, visited),
        ast::TypeKind::Named(named) => {
            if named.path.is_empty() {
                return true;
            }
            let base = &named.path[0].name;
            if context.drop_bases.contains(base) {
                return false;
            }
            if let Some(fields) = context.fields_for(named) {
                // Guard against recursive structs: if we're already visiting this
                // type, consider it Copy to break the cycle (conservative for
                // is_copy; needs_drop cycle is handled separately).
                let instance = crate::types::Type::from_ast(ty).canonical_key();
                if !visited.insert(instance.clone()) {
                    return true;
                }
                let all_copy = fields.iter().all(|f| is_copy_inner(f, context, visited));
                visited.remove(&instance);
                all_copy
            } else {
                // Unknown named type without field info: if it has generics
                // containing a drop type, be conservative and mark non-Copy.
                if let Some(generics) = &named.generics {
                    // e.g. Vec<i64> — base already handled, but Vec<String>
                    // already returned false via drop_bases. For generic wrappers
                    // not in drop_bases, check payload.
                    if generics.iter().any(|g| !is_copy_inner(g, context, visited)) {
                        return false;
                    }
                }
                // For plain unknown structs, assume Copy (pure heuristic).
                // Context-aware callers should populate struct_fields for precise answers.
                true
            }
        }
        ast::TypeKind::Generic(_) => false, // type param may instantiate to owned
        ast::TypeKind::Function(_) => true, // fn pointers are Copy
    }
}

fn needs_drop_inner(
    ty: &ast::Type,
    context: &TypePropertiesContext,
    visited: &mut HashSet<String>,
) -> bool {
    match ty.kind.as_ref() {
        ast::TypeKind::Primitive(_) => false,
        ast::TypeKind::Pointer(_) => false,
        ast::TypeKind::Reference(_) => false,
        ast::TypeKind::Array(a) => needs_drop_inner(&a.element_type, context, visited),
        ast::TypeKind::Tuple(ts) => ts.iter().any(|t| needs_drop_inner(t, context, visited)),
        ast::TypeKind::Slice(s) => needs_drop_inner(&s.element_type, context, visited),
        ast::TypeKind::Optional(inner) => needs_drop_inner(inner, context, visited),
        ast::TypeKind::Named(named) => {
            if named.path.is_empty() {
                return false;
            }
            let base = &named.path[0].name;
            if context.drop_bases.contains(base) {
                return true;
            }
            if let Some(fields) = context.fields_for(named) {
                let instance = crate::types::Type::from_ast(ty).canonical_key();
                if !visited.insert(instance.clone()) {
                    return false;
                }
                let any_drop = fields.iter().any(|f| needs_drop_inner(f, context, visited));
                visited.remove(&instance);
                any_drop
            } else {
                named
                    .generics
                    .iter()
                    .flatten()
                    .any(|argument| needs_drop_inner(argument, context, visited))
            }
        }
        ast::TypeKind::Generic(_) => false, // deliberately not owned here (typeck.rs:728-737)
        ast::TypeKind::Function(_) => false,
    }
}
