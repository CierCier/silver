//! Collision-safe symbol mangling.
//!
//! Every function-shaped symbol carries a suffix that is a pure function of
//! its COMPLETE signature: name, concrete type arguments, concrete parameter
//! types, return type, and variadic-ness. The readable prefix keeps symbols
//! debuggable; the FNV-1a-64 hash of the canonical signature makes identical
//! symbols imply identical functions (2^-64 collision odds); and explicit
//! arity counts make the common re-segmentation ambiguities (e.g. two type
//! args + one param vs one type arg + two params) impossible structurally.
//!
//! The hash inputs are the PRE-sanitization canonical type keys of concrete
//! types, so lossy sanitization (e.g. `ptr_i32` from either `i32*` or a type
//! named `ptr_i32`) cannot alias two distinct functions.

/// FNV-1a 64-bit (also used for module source hashing).
pub fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for b in bytes {
        hash ^= u64::from(*b);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

/// 16 lowercase hex digits: the collision suffix for one symbol.
pub fn signature_hash(key: &str) -> String {
    format!("{:016x}", fnv1a64(key.as_bytes()))
}

/// Sanitize a canonical type key into a mangled-name-safe token:
/// non-alphanumeric characters collapse to a single underscore, and
/// `*`/`&` become `ptr_`/`ref_`.
pub fn sanitize_type_key(value: &str) -> String {
    sanitize(&value.replace('*', "ptr_").replace('&', "ref_"))
}

fn sanitize(value: &str) -> String {
    let mut out = String::new();
    let mut last_underscore = false;
    for ch in value.chars() {
        let is_ok = ch.is_ascii_alphanumeric();
        if is_ok {
            out.push(ch);
            last_underscore = false;
        } else if !last_underscore {
            out.push('_');
            last_underscore = true;
        }
    }
    if out.is_empty() { "_".to_string() } else { out }
}

fn ret_key(ret: Option<&str>) -> &str {
    ret.unwrap_or("()")
}

fn variadic_marker(variadic: bool) -> &'static str {
    if variadic { "..." } else { "" }
}

/// Canonical hash key for a (non-generic) free function.
pub fn free_function_key(
    name: &str,
    params: &[String],
    ret: Option<&str>,
    variadic: bool,
) -> String {
    format!(
        "{name}({}{}) -> {}",
        params.join(","),
        variadic_marker(variadic),
        ret_key(ret)
    )
}

/// Canonical hash key for a generic function instance (concrete type args).
pub fn generic_function_key(
    name: &str,
    args: &[String],
    params: &[String],
    ret: Option<&str>,
    variadic: bool,
) -> String {
    format!(
        "{name}<{}>({}{}) -> {}",
        args.join(","),
        params.join(","),
        variadic_marker(variadic),
        ret_key(ret)
    )
}

/// Canonical hash key for an impl method (owner is the mangled owner name).
pub fn method_key(
    owner: &str,
    method: &str,
    params: &[String],
    ret: Option<&str>,
    variadic: bool,
) -> String {
    format!(
        "{owner}::{method}({}{}) -> {}",
        params.join(","),
        variadic_marker(variadic),
        ret_key(ret)
    )
}

/// Symbol for a generic function instance:
/// `{name}__{K}_{args}__{P}_{params}__{hash}` (K/P = arity counts).
pub fn generic_instance_symbol(
    name: &str,
    args: &[String],
    params: &[String],
    ret: Option<&str>,
    variadic: bool,
) -> String {
    let args_part = args
        .iter()
        .map(|arg| sanitize_type_key(arg))
        .collect::<Vec<_>>()
        .join("_");
    let params_part = params
        .iter()
        .map(|param| sanitize_type_key(param))
        .collect::<Vec<_>>()
        .join("_");
    let hash = signature_hash(&generic_function_key(name, args, params, ret, variadic));
    format!(
        "{name}__{}_{}__{}_{}__{}",
        args.len(),
        args_part,
        params.len(),
        params_part,
        hash
    )
}

/// Symbol for an overloaded non-generic free function: `{name}__{hash}`.
/// (Single-signature names keep the plain `{name}` symbol.)
pub fn overloaded_free_function_symbol(
    name: &str,
    params: &[String],
    ret: Option<&str>,
    variadic: bool,
) -> String {
    format!(
        "{name}__{}",
        signature_hash(&free_function_key(name, params, ret, variadic))
    )
}

/// Symbol for an impl method: `{owner}__{method}__{hash}`. Every method is
/// hashed (the previous per-(owner,method) signature index could not
/// distinguish different (owner, method) pairs that concatenate to the same
/// string, e.g. `Foo.bar__baz` vs `Foo__bar.baz`).
pub fn method_symbol(
    owner: &str,
    method: &str,
    params: &[String],
    ret: Option<&str>,
    variadic: bool,
) -> String {
    format!(
        "{owner}__{method}__{}",
        signature_hash(&method_key(owner, method, params, ret, variadic))
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_is_deterministic_and_distinct() {
        assert_eq!(signature_hash("a"), signature_hash("a"));
        assert_ne!(signature_hash("a"), signature_hash("b"));
    }

    #[test]
    fn generic_arity_counts_separate_lists() {
        // f<T,U>(T) @ [i64,f64] vs f<T>(f64,T) @ [i64] — the old scheme
        // produced f__i64_f64_i64 for both.
        let a = generic_instance_symbol(
            "f",
            &["i64".into(), "f64".into()],
            &["i64".into()],
            Some("i64"),
            false,
        );
        let b = generic_instance_symbol(
            "f",
            &["i64".into()],
            &["f64".into(), "i64".into()],
            Some("i64"),
            false,
        );
        assert_ne!(a, b);
        assert!(a.starts_with("f__2_i64_f64__1_i64__"));
        assert!(b.starts_with("f__1_i64__2_f64_i64__"));
    }

    #[test]
    fn overloaded_free_functions_distinct() {
        let g1 = overloaded_free_function_symbol("g", &["i64".into()], Some("i64"), false);
        let g2 = overloaded_free_function_symbol("g", &["f64".into()], Some("i64"), false);
        assert_ne!(g1, g2);
        assert!(g1.starts_with("g__"));
        assert_eq!(g1.len(), 1 + 2 + 16);
    }

    #[test]
    fn method_concatenation_cannot_collide() {
        // Foo.bar__baz vs Foo__bar.baz previously both -> Foo__bar__baz.
        let a = method_symbol("Foo", "bar__baz", &["i64".into()], Some("i64"), false);
        let b = method_symbol("Foo__bar", "baz", &["i64".into()], Some("i64"), false);
        assert_ne!(a, b);
        assert!(a.starts_with("Foo__bar__baz__"));
        assert!(b.starts_with("Foo__bar__baz__"));
    }

    #[test]
    fn sanitizer_lossiness_hashed_away() {
        // deref<i32*> vs deref<ptr_i32> sanitize identically; the hash over
        // the pre-sanitization keys must differ.
        let a = generic_instance_symbol(
            "deref",
            &["*i32".into()],
            &["i32".into()],
            Some("i32"),
            false,
        );
        let b = generic_instance_symbol(
            "deref",
            &["ptr_i32".into()],
            &["i32".into()],
            Some("i32"),
            false,
        );
        assert_ne!(a, b);
        assert_eq!(a[..a.len() - 16], b[..b.len() - 16]);
    }

    #[test]
    fn fnv1a64_known_vector() {
        // FNV-1a 64 reference: fnv1a64("") == 0xcbf29ce484222325.
        assert_eq!(fnv1a64(b""), 0xcbf29ce484222325);
        assert_eq!(fnv1a64(b"a"), 0xaf63dc4c8601ec8c);
    }
}
