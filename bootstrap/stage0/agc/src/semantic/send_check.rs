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
const MAX_SEND_TYPE_DEPTH: usize = 64;

pub(crate) fn structural_send(
    ty: &Type,
    resolve: &dyn Fn(&str) -> Option<DefView>,
) -> Result<(), String> {
    structural_send_inner(ty, resolve, &mut Vec::new(), 0)
}

fn structural_send_inner(
    ty: &Type,
    resolve: &dyn Fn(&str) -> Option<DefView>,
    visiting: &mut Vec<Type>,
    depth: usize,
) -> Result<(), String> {
    if depth >= MAX_SEND_TYPE_DEPTH {
        return Err(format!(
            "type nesting exceeds maximum depth ({MAX_SEND_TYPE_DEPTH})"
        ));
    }

    match ty {
        Type::Unit | Type::Primitive(_) | Type::Never => Ok(()),
        // A Task is a plain i64 handle to a thread-registry slot; the result
        // is read only after `wait` (a join barrier), so it need not be Send.
        Type::Task(_) => Ok(()),
        // A function value is a bare code address; arguments travel separately.
        Type::Function { .. } => Ok(()),
        Type::Array { element, .. } => {
            structural_send_inner(element, resolve, visiting, depth + 1)
        }
        Type::Optional { inner } => structural_send_inner(inner, resolve, visiting, depth + 1),
        Type::Tuple(items) => {
            for item in items {
                structural_send_inner(item, resolve, visiting, depth + 1)?;
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
        Type::Slice { .. } => {
            Err("slice views are not Send: they borrow memory owned elsewhere".to_string())
        }
        Type::Unknown => Err("the type is unknown (a prior type error)".to_string()),
        Type::Named { path, generics } => {
            let Some(name) = path.last() else {
                return Err(format!("type '{}' has no name", path.join("::")));
            };
            if let Some(&(_, owned)) = OWNING_CONTAINERS.iter().find(|(n, _)| *n == name) {
                for &idx in owned {
                    if let Some(arg) = generics.get(idx) {
                        structural_send_inner(arg, resolve, visiting, depth + 1)?;
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
            if visiting.iter().any(|active| active == ty) {
                return Ok(());
            }

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
            visiting.push(ty.clone());
            let result = match def {
                DefView::Struct { fields, .. } => {
                    for (field_name, field_ty) in fields {
                        let substituted = field_ty.substitute(&mapping);
                        structural_send_inner(&substituted, resolve, visiting, depth + 1)
                            .map_err(|reason| {
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
                        structural_send_inner(&tuple, resolve, visiting, depth + 1).map_err(
                            |reason| format!("variant {} of enum '{}': {}", index, name, reason),
                        )?;
                    }
                    Ok(())
                }
            };
            visiting.pop();
            result
        }
    }
}
