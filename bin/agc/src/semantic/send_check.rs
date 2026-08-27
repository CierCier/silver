//! Structural Send gate — the "can this value cross a thread boundary?"
//! field walk.
//!
//! Phase 4A of the fearless-concurrency plan: a `launch` argument is MOVED
//! into the child thread's stack, so its type must be Send.  The check is a
//! deliberately mechanical recursive walk over the type's fields — no trait
//! solving, no impl lookup, no inference.  Raw pointers and references are
//! rejected outright unless the containing type is a known *owning*
//! container (`Vec`, `Box`, `String`, `Bytes`, `HashMap`) whose pointees it
//! exclusively owns; the owned type arguments are then checked recursively.
//! Shared-ownership and GC types (`Rc`, `Handle`) are rejected explicitly.
//!
//! The walk resolves named types through the `resolve` callback supplied by
//! the type checker, which knows struct/enum definitions.  A value-field
//! struct is Send iff every field is Send; an enum is Send iff every variant
//! payload is Send.

use rustc_hash::FxHashMap as HashMap;

use crate::types::Type;

/// Owned containers: raw-pointer structs that OWN their pointees, so a value
/// of the type is Send iff the owned type arguments are Send.  The indices
/// select which generic arguments are the owned payloads.
const OWNING_CONTAINERS: &[(&str, &[usize])] = &[
    ("Vec", &[0]),
    ("Box", &[0]),
    ("String", &[]),
    ("Bytes", &[]),
    ("HashMap", &[0, 1]),
];

/// Structurally simple types that must nevertheless never cross a thread
/// boundary, with a human-readable reason.
const NOT_SEND: &[(&str, &str)] = &[
    ("Rc", "Rc<T> carries a shared non-atomic reference count"),
    (
        "Handle",
        "GC heap handles are not Send (the heap is single-threaded)",
    ),
    (
        "Guard",
        "a live Guard holds its Mutex locked; drop it (or move the unlocked \
         Mutex itself), never the guard",
    ),
];

/// Resolved definition of a named type, as provided by the type checker.
#[derive(Debug, Clone)]
pub enum DefView {
    Struct {
        type_params: Vec<String>,
        fields: Vec<(String, Type)>,
    },
    Enum {
        type_params: Vec<String>,
        /// Per-variant payload type lists (empty for unit variants).
        variants: Vec<Vec<Type>>,
    },
}

/// Returns `Ok(())` if `ty` may be moved across a thread boundary, or
/// `Err(reason)` naming the first non-Send leaf.  `resolve` looks up named
/// types; returning `None` makes the type unprovably Send (conservative).
pub(crate) fn structural_send(
    ty: &Type,
    resolve: &dyn Fn(&str) -> Option<DefView>,
) -> Result<(), String> {
    match ty {
        Type::Unit | Type::Primitive(_) => Ok(()),
        // A Task is a plain i64 handle to a thread-registry slot; the result
        // is read only after `wait` (a join barrier), so it need not be Send.
        Type::Task(_) => Ok(()),
        // A function value is a bare code address; arguments travel separately.
        Type::Function { .. } => Ok(()),
        Type::Array { element, .. } => structural_send(element, resolve),
        Type::Optional { inner } => structural_send(inner, resolve),
        Type::Tuple(items) => {
            for item in items {
                structural_send(item, resolve)?;
            }
            Ok(())
        }
        Type::Pointer { .. } => Err(
            "raw pointer types are not Send: move owned values (Box/Vec) or pass a Channel across threads"
                .to_string(),
        ),
        Type::Reference { .. } => Err(
            "references cannot cross a thread boundary in v1 (scoped threads are not implemented yet)"
                .to_string(),
        ),
        Type::Slice { .. } => Err("slice views are not Send: they borrow memory owned elsewhere".to_string()),
        Type::Unknown => Err("the type is unknown (a prior type error)".to_string()),
        Type::Named { path, generics } => {
            let Some(name) = path.last() else {
                return Err(format!("type '{}' has no name", path.join("::")));
            };
            if let Some(&(_, owned)) = OWNING_CONTAINERS.iter().find(|(n, _)| *n == name) {
                for &idx in owned {
                    if let Some(arg) = generics.get(idx) {
                        structural_send(arg, resolve)?;
                    }
                }
                return Ok(());
            }
            if let Some(&(_, reason)) = NOT_SEND.iter().find(|(n, _)| *n == name) {
                return Err(reason.to_string());
            }
            let Some(def) = resolve(name) else {
                return Err(format!(
                    "type '{}' is not provably Send: no struct/enum definition is visible \
                     (is it an unresolved generic parameter?)",
                    name
                ));
            };
            let type_params = match &def {
                DefView::Struct { type_params, .. } | DefView::Enum { type_params, .. } => {
                    type_params
                }
            };
            let mapping: HashMap<String, Type> = type_params
                .iter()
                .cloned()
                .zip(generics.iter().cloned())
                .collect();
            match def {
                DefView::Struct { fields, .. } => {
                    for (field_name, field_ty) in fields {
                        let substituted = field_ty.substitute(&mapping);
                        structural_send(&substituted, resolve).map_err(|reason| {
                            format!("field '{}' of '{}': {}", field_name, name, reason)
                        })?;
                    }
                    Ok(())
                }
                DefView::Enum { variants, .. } => {
                    for (index, payload) in variants.iter().enumerate() {
                        let tuple = Type::Tuple(
                            payload
                                .iter()
                                .map(|item| item.substitute(&mapping))
                                .collect(),
                        );
                        structural_send(&tuple, resolve).map_err(|reason| {
                            format!("variant {} of enum '{}': {}", index, name, reason)
                        })?;
                    }
                    Ok(())
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::PrimitiveType;

    fn prim(p: PrimitiveType) -> Type {
        Type::Primitive(p)
    }

    fn named(name: &str, generics: Vec<Type>) -> Type {
        Type::Named {
            path: vec![name.to_string()],
            generics,
        }
    }

    fn no_defs(_name: &str) -> Option<DefView> {
        None
    }

    #[test]
    fn primitives_are_send() {
        for p in [
            PrimitiveType::I8,
            PrimitiveType::I64,
            PrimitiveType::U8,
            PrimitiveType::U128,
            PrimitiveType::F32,
            PrimitiveType::F64,
            PrimitiveType::Bool,
            PrimitiveType::Str,
            PrimitiveType::Char,
            PrimitiveType::Void,
        ] {
            assert_eq!(structural_send(&prim(p.clone()), &no_defs), Ok(()), "{p:?}");
        }
    }

    #[test]
    fn task_and_function_are_send() {
        assert!(structural_send(&Type::Task(Box::new(prim(PrimitiveType::I64))), &no_defs).is_ok());
        assert!(
            structural_send(
                &Type::Function {
                    params: vec![prim(PrimitiveType::I64)],
                    return_type: Box::new(Type::Unit),
                },
                &no_defs
            )
            .is_ok()
        );
    }

    #[test]
    fn owning_containers_are_conditional() {
        assert!(structural_send(&named("Vec", vec![prim(PrimitiveType::I64)]), &no_defs).is_ok());
        assert!(structural_send(&named("Box", vec![prim(PrimitiveType::I64)]), &no_defs).is_ok());
        assert!(structural_send(&named("String", vec![]), &no_defs).is_ok());
        assert!(structural_send(&named("Bytes", vec![]), &no_defs).is_ok());
        assert!(
            structural_send(
                &named(
                    "HashMap",
                    vec![prim(PrimitiveType::Str), prim(PrimitiveType::I64)]
                ),
                &no_defs
            )
            .is_ok()
        );
        // Owned payloads are themselves checked.
        let err = structural_send(
            &named("Vec", vec![named("Rc", vec![prim(PrimitiveType::I64)])]),
            &no_defs,
        )
        .unwrap_err();
        assert!(err.contains("Rc"), "{err}");
    }

    #[test]
    fn shared_and_raw_views_are_rejected() {
        let err =
            structural_send(&named("Rc", vec![prim(PrimitiveType::I64)]), &no_defs).unwrap_err();
        assert!(err.contains("reference count"), "{err}");

        let ptr = Type::Pointer {
            is_mutable: true,
            is_volatile: false,
            inner: Box::new(prim(PrimitiveType::I64)),
        };
        assert!(structural_send(&ptr, &no_defs).is_err());

        let rf = Type::Reference {
            is_mutable: false,
            inner: Box::new(prim(PrimitiveType::I64)),
        };
        assert!(structural_send(&rf, &no_defs).is_err());

        let slice = Type::Slice {
            element: Box::new(prim(PrimitiveType::I64)),
        };
        assert!(structural_send(&slice, &no_defs).is_err());

        let handle = named("Handle", vec![]);
        assert!(structural_send(&handle, &no_defs).is_err());
    }

    #[test]
    fn guard_is_never_send() {
        // A Guard<T> holds its mutex locked: moving one across a thread
        // boundary is rejected with the guard-specific reason, regardless of
        // the payload type.
        let guard = named(
            "Guard",
            vec![prim(PrimitiveType::I64)],
        );
        let err = structural_send(&guard, &no_defs).unwrap_err();
        assert!(err.contains("Guard holds its Mutex locked"), "{err}");
    }

    #[test]
    fn mutex_is_conditionally_send() {
        // Mutex<T> is an owning struct (futex word + T): Send iff T is Send.
        // RawMutex resolves to a plain-field struct via `resolve`.
        let raw_mutex = DefView::Struct {
            type_params: vec![],
            fields: vec![
                ("word".to_string(), prim(PrimitiveType::I64)),
                ("waiters".to_string(), prim(PrimitiveType::I64)),
            ],
        };
        let mutex = |t: Type| {
            DefView::Struct {
                type_params: vec!["T".to_string()],
                fields: vec![
                    ("mu".to_string(), named("RawMutex", vec![])),
                    ("value".to_string(), t),
                ],
            }
        };
        let resolve = |n: &str| match n {
            "RawMutex" => Some(raw_mutex.clone()),
            "Mutex" => Some(mutex(prim(PrimitiveType::I64))),
            _ => None,
        };
        let ok_mutex = named("Mutex", vec![prim(PrimitiveType::I64)]);
        assert!(structural_send(&ok_mutex, &resolve).is_ok());

        let bad_mutex =
            named("Mutex", vec![named("Rc", vec![prim(PrimitiveType::I64)])]);
        let resolve_bad = |n: &str| match n {
            "RawMutex" => Some(raw_mutex.clone()),
            "Mutex" => Some(mutex(named("Rc", vec![prim(PrimitiveType::I64)]))),
            _ => None,
        };
        let err = structural_send(&bad_mutex, &resolve_bad).unwrap_err();
        assert!(err.contains("Mutex"), "{err}");
        assert!(err.contains("Rc"), "{err}");
    }

    #[test]
    fn user_struct_fields_are_walked() {        let pair = |t: Type| DefView::Struct {
            type_params: vec!["T".to_string()],
            fields: vec![("a".to_string(), t.clone()), ("b".to_string(), t)],
        };
        let ty = named("Pair", vec![prim(PrimitiveType::I64)]);
        let resolve = |n: &str| {
            if n == "Pair" {
                Some(pair(prim(PrimitiveType::I64)))
            } else {
                None
            }
        };
        assert!(structural_send(&ty, &resolve).is_ok());

        let bad = named("Pair", vec![named("Rc", vec![prim(PrimitiveType::I64)])]);
        let resolve_bad = |n: &str| {
            if n == "Pair" {
                Some(pair(named("Rc", vec![prim(PrimitiveType::I64)])))
            } else {
                None
            }
        };
        let err = structural_send(&bad, &resolve_bad).unwrap_err();
        assert!(err.contains("Pair"), "{err}");
        assert!(err.contains("a"), "{err}");
    }

    #[test]
    fn pointer_field_rejects_struct() {
        let node = DefView::Struct {
            type_params: vec![],
            fields: vec![
                (
                    "next".to_string(),
                    Type::Pointer {
                        is_mutable: true,
                        is_volatile: false,
                        inner: Box::new(named("Node", vec![])),
                    },
                ),
                ("v".to_string(), prim(PrimitiveType::I64)),
            ],
        };
        let ty = named("Node", vec![]);
        let resolve = |n: &str| {
            if n == "Node" {
                Some(node.clone())
            } else {
                None
            }
        };
        let err = structural_send(&ty, &resolve).unwrap_err();
        assert!(err.contains("Node"), "{err}");
    }

    #[test]
    fn unknown_type_is_rejected() {
        let err = structural_send(&named("NoSuch", vec![]), &no_defs).unwrap_err();
        assert!(err.contains("NoSuch"), "{err}");
    }

    #[test]
    fn enums_walk_payloads() {
        let shape = DefView::Enum {
            type_params: vec![],
            variants: vec![vec![prim(PrimitiveType::F64)], vec![]],
        };
        let ty = named("Shape", vec![]);
        let resolve = |n: &str| {
            if n == "Shape" {
                Some(shape.clone())
            } else {
                None
            }
        };
        assert!(structural_send(&ty, &resolve).is_ok());

        let bad = DefView::Enum {
            type_params: vec![],
            variants: vec![vec![named("Rc", vec![prim(PrimitiveType::I64)])]],
        };
        let resolve_bad = |n: &str| if n == "Bad" { Some(bad.clone()) } else { None };
        assert!(structural_send(&named("Bad", vec![]), &resolve_bad).is_err());
    }

    #[test]
    fn optional_array_tuple_recurse() {
        assert!(
            structural_send(
                &Type::Optional {
                    inner: Box::new(prim(PrimitiveType::I64))
                },
                &no_defs
            )
            .is_ok()
        );
        assert!(
            structural_send(
                &Type::Optional {
                    inner: Box::new(named("Rc", vec![prim(PrimitiveType::I64)]))
                },
                &no_defs
            )
            .is_err()
        );
        assert!(
            structural_send(
                &Type::Array {
                    element: Box::new(prim(PrimitiveType::I64)),
                    size: 4,
                },
                &no_defs
            )
            .is_ok()
        );
        assert!(
            structural_send(
                &Type::Tuple(vec![prim(PrimitiveType::I64), prim(PrimitiveType::Bool)]),
                &no_defs
            )
            .is_ok()
        );
    }
}
