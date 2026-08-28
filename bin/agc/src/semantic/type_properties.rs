// Central TypeProperties {is_copy, needs_drop} — Phase 5 query (pure, no checker integration yet).
//
// Implicit Copy retained per grilling: bool/i64/f64/ptr + all-Copy struct = Copy
// else non-Copy like String/Vec/HashMap/HashSet/Deque/File.
// Centralizes the heuristic currently duplicated in typeck.rs:type_has_drop_impl,
// move_check.rs:is_tracked, and codegen/llvm_ir/scope.rs:get_drop_function_name.
//
// Keep functions pure and testable; no TypeChecker wiring yet. A context struct
// carries the minimal data needed for struct-field recursion and drop-owner lookup
// so unit tests can inject synthetic structs without a real symbol table.

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::parser::ast;

/// Whether a type is bitwise-copyable and whether it needs a destructor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeProperties {
    pub is_copy: bool,
    pub needs_drop: bool,
}

/// Minimal context for named-type resolution. Pure data, no checker handle.
///
/// * `drop_bases` — base names that have a `Drop` impl (e.g. "String", "Vec").
///   Bare-name only, matching `typeck.rs:collect_trait_impls` which inserts
///   `named.path[0].name` for any `impl Drop<X>`.
/// * `struct_fields` — field types for structs we want recursion into. A missing
///   entry means "unknown struct"; callers decide via fallback (by default treated
///   as non-owned unless the name itself is a drop owner).
#[derive(Debug, Clone, Default)]
pub struct TypePropertiesContext {
    pub drop_bases: HashSet<String>,
    pub struct_fields: HashMap<String, Vec<ast::Type>>,
}

impl TypePropertiesContext {
    pub fn new() -> Self {
        Self {
            drop_bases: default_drop_bases(),
            struct_fields: HashMap::default(),
        }
    }

    pub fn with_drop_bases(bases: impl IntoIterator<Item = String>) -> Self {
        Self {
            drop_bases: bases.into_iter().collect(),
            struct_fields: HashMap::default(),
        }
    }

    pub fn insert_struct(&mut self, name: impl Into<String>, fields: Vec<ast::Type>) {
        self.struct_fields.insert(name.into(), fields);
    }

    pub fn with_struct(mut self, name: impl Into<String>, fields: Vec<ast::Type>) -> Self {
        self.insert_struct(name, fields);
        self
    }
}

fn default_drop_bases() -> HashSet<String> {
    // Canonical owned bases in std today. Kept deliberately broad so pure
    // queries without an explicit context still answer "String/Vec/… non-Copy"
    // correctly. Phase 5 central query will replace the ad-hoc per-subsystem
    // sets; until then this is the unified list (docs/borrow-checker/copy-semantics.md §2.2).
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
    )
}

// ---------------------------------------------------------------------------
// Simple (no-context) queries — keep existing per-subsystem heuristics working
// in parallel. Use hardcoded drop bases; named structs without field info are
// treated as Copy iff not a known drop owner.
// ---------------------------------------------------------------------------

/// Returns true for Copy types: bool, all integer/float primitives, raw pointers,
/// and (without struct-field knowledge) named types that are not known drop owners.
/// Tuples/arrays are Copy iff all elements are Copy. References/slices are views
/// (Copy). This mirrors the implicit-Copy rule per grilling.
pub fn is_copy(ty: &ast::Type) -> bool {
    let mut visited = HashSet::default();
    is_copy_inner(ty, &default_drop_bases(), &HashMap::default(), &mut visited)
}

/// Returns true when `ty` needs Drop: direct Drop impl or transitive field Drop.
/// For the context-free variant, only direct named drop owners and recursive
/// arrays/tuples are considered (matching typeck.rs:type_has_drop_impl).
pub fn needs_drop(ty: &ast::Type) -> bool {
    let mut visited = HashSet::default();
    needs_drop_inner(ty, &default_drop_bases(), &HashMap::default(), &mut visited)
}

/// Pair query without context.
pub fn properties_for(ty: &ast::Type) -> TypeProperties {
    TypeProperties {
        is_copy: is_copy(ty),
        needs_drop: needs_drop(ty),
    }
}

// ---------------------------------------------------------------------------
// Context-aware queries — for tests and later checker integration. Recursion
// into struct fields is enabled when the name appears in `ctx.struct_fields`.
// ---------------------------------------------------------------------------

pub fn is_copy_with(ty: &ast::Type, ctx: &TypePropertiesContext) -> bool {
    let mut visited = HashSet::default();
    is_copy_inner(ty, &ctx.drop_bases, &ctx.struct_fields, &mut visited)
}

pub fn needs_drop_with(ty: &ast::Type, ctx: &TypePropertiesContext) -> bool {
    let mut visited = HashSet::default();
    needs_drop_inner(ty, &ctx.drop_bases, &ctx.struct_fields, &mut visited)
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
    drop_bases: &HashSet<String>,
    struct_fields: &HashMap<String, Vec<ast::Type>>,
    visited: &mut HashSet<String>,
) -> bool {
    match ty.kind.as_ref() {
        ast::TypeKind::Primitive(p) => is_copy_primitive(p),
        ast::TypeKind::Pointer(_) => true,
        ast::TypeKind::Reference(_) => true,
        ast::TypeKind::Array(a) => {
            is_copy_inner(&a.element_type, drop_bases, struct_fields, visited)
        }
        ast::TypeKind::Tuple(ts) => ts
            .iter()
            .all(|t| is_copy_inner(t, drop_bases, struct_fields, visited)),
        ast::TypeKind::Slice(_) => true, // view, no ownership
        ast::TypeKind::Optional(inner) => {
            is_copy_inner(inner, drop_bases, struct_fields, visited)
        }
        ast::TypeKind::Named(named) => {
            if named.path.is_empty() {
                return true;
            }
            let base = &named.path[0].name;
            if drop_bases.contains(base) {
                return false;
            }
            if let Some(fields) = struct_fields.get(base) {
                // Guard against recursive structs: if we're already visiting this
                // type, consider it Copy to break the cycle (conservative for
                // is_copy; needs_drop cycle is handled separately).
                if !visited.insert(base.clone()) {
                    return true;
                }
                let all_copy = fields
                    .iter()
                    .all(|f| is_copy_inner(f, drop_bases, struct_fields, visited));
                visited.remove(base);
                all_copy
            } else {
                // Unknown named type without field info: if it has generics
                // containing a drop type, be conservative and mark non-Copy.
                if let Some(generics) = &named.generics {
                    // e.g. Vec<i64> — base already handled, but Vec<String>
                    // already returned false via drop_bases. For generic wrappers
                    // not in drop_bases, check payload.
                    if generics
                        .iter()
                        .any(|g| !is_copy_inner(g, drop_bases, struct_fields, visited))
                    {
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
    drop_bases: &HashSet<String>,
    struct_fields: &HashMap<String, Vec<ast::Type>>,
    visited: &mut HashSet<String>,
) -> bool {
    match ty.kind.as_ref() {
        ast::TypeKind::Primitive(_) => false,
        ast::TypeKind::Pointer(_) => false,
        ast::TypeKind::Reference(_) => false,
        ast::TypeKind::Array(a) => {
            needs_drop_inner(&a.element_type, drop_bases, struct_fields, visited)
        }
        ast::TypeKind::Tuple(ts) => ts
            .iter()
            .any(|t| needs_drop_inner(t, drop_bases, struct_fields, visited)),
        ast::TypeKind::Slice(s) => {
            needs_drop_inner(&s.element_type, drop_bases, struct_fields, visited)
        }
        ast::TypeKind::Optional(inner) => {
            needs_drop_inner(inner, drop_bases, struct_fields, visited)
        }
        ast::TypeKind::Named(named) => {
            if named.path.is_empty() {
                return false;
            }
            let base = &named.path[0].name;
            if drop_bases.contains(base) {
                return true;
            }
            // Generic payload drop: e.g. Wrapper<String> where Wrapper itself
            // not in drop_bases but contains a drop generic arg. Treat as needs_drop
            // if any generic arg needs_drop (mirrors typeck tuple/array recursion).
            if let Some(generics) = &named.generics {
                if generics
                    .iter()
                    .any(|g| needs_drop_inner(g, drop_bases, struct_fields, visited))
                {
                    return true;
                }
            }
            if let Some(fields) = struct_fields.get(base) {
                if !visited.insert(base.clone()) {
                    return false;
                }
                let any_drop = fields
                    .iter()
                    .any(|f| needs_drop_inner(f, drop_bases, struct_fields, visited));
                visited.remove(base);
                any_drop
            } else {
                false
            }
        }
        ast::TypeKind::Generic(_) => false, // deliberately not owned here (typeck.rs:728-737)
        ast::TypeKind::Function(_) => false,
    }
}

// ---------------------------------------------------------------------------
// Tests — 4 required + extras.
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Span;
    use crate::parser::ast;

    fn sp() -> Span {
        Span::new(0, 0)
    }

    fn ty_primitive(p: ast::PrimitiveType) -> ast::Type {
        ast::Type {
            kind: Box::new(ast::TypeKind::Primitive(p)),
            span: sp(),
        }
    }

    fn ty_named(name: &str) -> ast::Type {
        ast::Type {
            kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                path: vec![ast::Identifier {
                    name: name.to_string(),
                    span: sp(),
                }],
                generics: None,
            })),
            span: sp(),
        }
    }

    fn ty_named_generic(name: &str, args: Vec<ast::Type>) -> ast::Type {
        ast::Type {
            kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                path: vec![ast::Identifier {
                    name: name.to_string(),
                    span: sp(),
                }],
                generics: Some(args),
            })),
            span: sp(),
        }
    }

    fn ty_ptr(inner: ast::Type) -> ast::Type {
        ast::Type {
            kind: Box::new(ast::TypeKind::Pointer(ast::PointerType {
                is_mutable: false,
                is_volatile: false,
                inner: Box::new(inner),
            })),
            span: sp(),
        }
    }

    fn ty_tuple(elems: Vec<ast::Type>) -> ast::Type {
        ast::Type {
            kind: Box::new(ast::TypeKind::Tuple(elems)),
            span: sp(),
        }
    }

    fn ty_array(elem: ast::Type) -> ast::Type {
        ast::Type {
            kind: Box::new(ast::TypeKind::Array(Box::new(ast::ArrayType {
                element_type: Box::new(elem),
                size: 4,
                span: sp(),
            }))),
            span: sp(),
        }
    }

    #[test]
    fn i64_is_copy_true() {
        let ty = ty_primitive(ast::PrimitiveType::I64);
        assert!(is_copy(&ty));
        assert!(!needs_drop(&ty));
        let props = properties_for(&ty);
        assert!(props.is_copy);
        assert!(!props.needs_drop);
    }

    #[test]
    fn string_is_copy_false_needs_drop_true() {
        let ty = ty_named("String");
        assert!(!is_copy(&ty));
        assert!(needs_drop(&ty));
        let props = properties_for(&ty);
        assert!(!props.is_copy);
        assert!(props.needs_drop);
    }

    #[test]
    fn struct_i64_bool_is_copy_true() {
        // struct S { i64, bool } — all fields Copy => struct Copy
        let mut ctx = TypePropertiesContext::new();
        ctx.insert_struct(
            "MyStructAllCopy",
            vec![
                ty_primitive(ast::PrimitiveType::I64),
                ty_primitive(ast::PrimitiveType::Bool),
            ],
        );
        let ty = ty_named("MyStructAllCopy");
        assert!(is_copy_with(&ty, &ctx));
        assert!(!needs_drop_with(&ty, &ctx));
        let props = properties_for_with(&ty, &ctx);
        assert!(props.is_copy);
        assert!(!props.needs_drop);
    }

    #[test]
    fn struct_string_is_copy_false() {
        // struct S { String } — contains needs_drop field => non-Copy
        let mut ctx = TypePropertiesContext::new();
        ctx.insert_struct("MyStructWithString", vec![ty_named("String")]);
        let ty = ty_named("MyStructWithString");
        assert!(!is_copy_with(&ty, &ctx));
        assert!(needs_drop_with(&ty, &ctx));
        let props = properties_for_with(&ty, &ctx);
        assert!(!props.is_copy);
        assert!(props.needs_drop);
    }

    #[test]
    fn bool_is_copy() {
        let ty = ty_primitive(ast::PrimitiveType::Bool);
        assert!(is_copy(&ty));
        assert!(!needs_drop(&ty));
    }

    #[test]
    fn f64_is_copy() {
        let ty = ty_primitive(ast::PrimitiveType::F64);
        assert!(is_copy(&ty));
        assert!(!needs_drop(&ty));
    }

    #[test]
    fn raw_pointer_is_copy() {
        let inner = ty_primitive(ast::PrimitiveType::I64);
        let ty = ty_ptr(inner);
        assert!(is_copy(&ty));
        assert!(!needs_drop(&ty));
    }

    #[test]
    fn vec_is_not_copy_needs_drop() {
        let ty = ty_named("Vec");
        assert!(!is_copy(&ty));
        assert!(needs_drop(&ty));
    }

    #[test]
    fn tuple_all_copy_is_copy() {
        let ty = ty_tuple(vec![
            ty_primitive(ast::PrimitiveType::I64),
            ty_primitive(ast::PrimitiveType::Bool),
        ]);
        assert!(is_copy(&ty));
        assert!(!needs_drop(&ty));
    }

    #[test]
    fn tuple_with_string_not_copy() {
        let ty = ty_tuple(vec![
            ty_primitive(ast::PrimitiveType::I64),
            ty_named("String"),
        ]);
        assert!(!is_copy(&ty));
        assert!(needs_drop(&ty));
    }

    #[test]
    fn array_copy_element_is_copy() {
        let ty = ty_array(ty_primitive(ast::PrimitiveType::I64));
        assert!(is_copy(&ty));
        assert!(!needs_drop(&ty));
    }

    #[test]
    fn array_string_needs_drop() {
        let ty = ty_array(ty_named("String"));
        assert!(!is_copy(&ty));
        assert!(needs_drop(&ty));
    }

    #[test]
    fn nested_struct_transitive() {
        // Inner { String } is non-Copy; Outer { Inner } should also be non-Copy
        let mut ctx = TypePropertiesContext::new();
        ctx.insert_struct("Inner", vec![ty_named("String")]);
        ctx.insert_struct("Outer", vec![ty_named("Inner")]);
        let ty = ty_named("Outer");
        assert!(!is_copy_with(&ty, &ctx));
        assert!(needs_drop_with(&ty, &ctx));
    }

    #[test]
    fn hashmap_needs_drop() {
        let ty = ty_named("HashMap");
        assert!(!is_copy(&ty));
        assert!(needs_drop(&ty));
    }

    #[test]
    fn generic_wrapper_with_string_needs_drop() {
        // MyVec<String> where base not in drop set but generic arg is drop
        let ctx = TypePropertiesContext::new();
        // Wrapper not a drop owner, but its field is generic arg that is drop
        // For named with generics we check generic args
        let ty = ty_named_generic("Wrapper", vec![ty_named("String")]);
        assert!(!is_copy_with(&ty, &ctx));
        assert!(needs_drop_with(&ty, &ctx));
    }
}
