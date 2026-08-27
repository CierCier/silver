//! Compile-time configuration (CFG) machinery.
//!
//! `--cfg "key=value,key2=value2"` flags populate a [`CfgSet`]. Items marked
//! `#[cfg(key)]` (or `#[cfg(cpu.feature)]`, `#[cfg("key")]`) are kept only
//! when the key is present; multiple cfg arguments AND-compose. The gate runs
//! after import lowering and before symbol registration, so gated-out items
//! never register or type-check.
//!
//! The `@cfg(key)` expression form folds at semantic time (see
//! `semantic::cfg_hook`): absent keys become `false` (dead branches pruned),
//! custom keys become `true`, and present `cpu.*` keys become a read of the
//! runtime probe global `g_has_<feature>` initialized by `__silver_cpu_init`
//! in `std/cpu.ag` before `main`.

use crate::lexer::Span;
use crate::parser::ast;
use rustc_hash::FxHashMap;

/// The set of cfg keys enabled for this compilation, with their values.
#[derive(Debug, Clone, Default)]
pub struct CfgSet {
    values: FxHashMap<String, String>,
}

impl CfgSet {
    /// Parse `--cfg` flag values: comma-separated `key=value` pairs; a bare
    /// `key` gets the value `"1"`. Empty fragments are ignored.
    pub fn parse(flags: &[String]) -> Self {
        let mut set = CfgSet::default();
        for flag in flags {
            for pair in flag.split(',') {
                let pair = pair.trim();
                if pair.is_empty() {
                    continue;
                }
                let (key, value) = match pair.split_once('=') {
                    Some((key, value)) => (key.trim(), value.trim()),
                    None => (pair, "1"),
                };
                if key.is_empty() {
                    continue;
                }
                set.values.insert(key.to_string(), value.to_string());
            }
        }
        set
    }

    /// True when the key is present in the cfg set (value irrelevant).
    pub fn contains(&self, key: &str) -> bool {
        self.values.contains_key(key)
    }

    /// The value bound to the key, if present.
    pub fn value(&self, key: &str) -> Option<&str> {
        self.values.get(key).map(String::as_str)
    }

    fn insert_if_absent(&mut self, key: &str) {
        if !self.values.contains_key(key) {
            self.values.insert(key.to_string(), "1".to_string());
        }
    }
}

/// Derive compiler-defined cfg keys from the build configuration:
///
///   - `debug` when the opt level is unset/0, `release` for 1/2/3/s/z/fast
///   - `arch.<arch>` and `os.<os>` from the target triple (or the compiler
///     host when no `--target` is given)
///
/// User-provided keys win (insert-if-absent), so explicit `--cfg` values are
/// never clobbered — e.g. `--cfg "debug=1"` with `-O2` keeps both.
pub fn add_derived_cfgs(set: &mut CfgSet, opt_level: Option<&str>, target: Option<&str>) {
    let release = matches!(opt_level, Some("1" | "2" | "3" | "s" | "z" | "fast"));
    set.insert_if_absent(if release { "release" } else { "debug" });
    let (arch, os) = match target {
        Some(triple) => {
            let mut parts = triple.split('-');
            let arch = parts.next().unwrap_or("unknown");
            let os = triple_os(triple);
            (arch, os)
        }
        None => (std::env::consts::ARCH, std::env::consts::OS),
    };
    set.insert_if_absent(&format!("arch.{arch}"));
    set.insert_if_absent(&format!("os.{os}"));
}

/// Best-effort OS extraction from a target triple: the token that matches a
/// known OS name, else the vendor position.
fn triple_os(triple: &str) -> &str {
    for os in [
        "linux", "darwin", "macos", "windows", "freebsd", "netbsd", "openbsd", "solaris",
        "illumos", "android", "ios", "haiku",
    ] {
        if triple.split('-').any(|part| part == os) {
            return os;
        }
    }
    triple.split('-').nth(1).unwrap_or("unknown")
}

/// A malformed `#[cfg(...)]` attribute.
pub struct CfgError {
    pub message: String,
    pub span: Span,
}

/// Normalize a cfg attribute argument to its key string.
fn cfg_arg_key(arg: &ast::AttributeArg) -> Option<String> {
    match arg {
        ast::AttributeArg::Identifier(id) => Some(id.name.clone()),
        ast::AttributeArg::Path(path) => Some(
            path.iter()
                .map(|id| id.name.as_str())
                .collect::<Vec<_>>()
                .join("."),
        ),
        ast::AttributeArg::Literal(ast::Literal::String(value)) => Some(value.clone()),
        _ => None,
    }
}

fn cfg_arg_span(arg: &ast::AttributeArg) -> Span {
    match arg {
        ast::AttributeArg::Identifier(id) => id.span,
        ast::AttributeArg::Path(path) => {
            let first = &path[0];
            let last = &path[path.len() - 1];
            first.span.with_end(last.span.end)
        }
        ast::AttributeArg::Literal(ast::Literal::String(_)) => Span::default(),
        _ => Span::default(),
    }
}

/// Evaluate the `#[cfg(...)]` attributes on one item or method against the
/// set. Returns `None` (keep) when there are no cfg attributes; otherwise the
/// keep decision plus any malformed-attribute errors. All cfg arguments must
/// match (AND); multiple `#[cfg]` attributes also AND-compose.
fn eval_cfg_attrs(
    attributes: &[ast::Attribute],
    cfg: &CfgSet,
    errors: &mut Vec<CfgError>,
) -> Option<bool> {
    let cfg_attrs: Vec<&ast::Attribute> = attributes
        .iter()
        .filter(|attr| attr.name.name == "cfg")
        .collect();
    if cfg_attrs.is_empty() {
        return None;
    }
    let mut keep = true;
    for attr in cfg_attrs {
        if attr.args.is_empty() {
            errors.push(CfgError {
                message: crate::diagnostics::messages::cfg_requires_at_least_one_argument()
                    .to_string(),
                span: attr.span,
            });
            keep = false;
            continue;
        }
        for arg in &attr.args {
            match cfg_arg_key(arg) {
                Some(key) => {
                    if !cfg.contains(&key) {
                        keep = false;
                    }
                }
                None => {
                    errors.push(CfgError {
                        message: "invalid #[cfg] argument: expected a name, string, or dotted path"
                            .to_string(),
                        span: cfg_arg_span(arg),
                    });
                    keep = false;
                }
            }
        }
    }
    Some(keep)
}

/// Drop items whose `#[cfg(...)]` attributes do not match `cfg` (and the same
/// for impl methods, which now carry attributes too). Items without cfg
/// attributes are always kept. Malformed attributes are reported as errors.
pub fn gate_items(program: &mut ast::Program, cfg: &CfgSet) -> Vec<CfgError> {
    let mut errors = Vec::new();
    // Filter impl methods first (needs &mut access per item).
    for item in &mut program.items {
        if let ast::ItemKind::Impl(impl_item) = &mut item.kind {
            impl_item.items.retain(|member| {
                let attributes = match member {
                    ast::ImplItemKind::Function(func) => &func.attributes,
                    _ => return true,
                };
                eval_cfg_attrs(attributes, cfg, &mut errors).unwrap_or(true)
            });
        }
    }
    // Then filter top-level items.
    program
        .items
        .retain(|item| eval_cfg_attrs(&item.attributes, cfg, &mut errors).unwrap_or(true));
    errors
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::Parser;

    fn parse(src: &str) -> ast::Program {
        let tokens = lex(src).expect("lexer should succeed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parser errors: {errors:?}");
        program
    }

    #[test]
    fn parse_comma_separated_pairs() {
        let set = CfgSet::parse(&["cpu.avx=1,cpu.sse41".to_string(), "release".to_string()]);
        assert!(set.contains("cpu.avx"));
        assert!(set.contains("cpu.sse41"));
        assert!(set.contains("release"));
        assert!(!set.contains("cpu.avx2"));
        assert_eq!(set.value("cpu.avx"), Some("1"));
        assert_eq!(set.value("release"), Some("1"));
    }

    #[test]
    fn gate_keeps_matching_items() {
        let mut program = parse(
            "#[cfg(flag)]\ni32 a() { return 1; }\n#[cfg(missing)]\ni32 b() { return 2; }\ni32 c() { return 3; }\n",
        );
        let set = CfgSet::parse(&["flag".to_string()]);
        let errors = gate_items(&mut program, &set);
        assert!(errors.is_empty());
        let names: Vec<&str> = program
            .items
            .iter()
            .filter_map(|item| match &item.kind {
                ast::ItemKind::Function(f) => Some(f.name.name.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(names, vec!["a", "c"]);
    }

    #[test]
    fn gate_requires_all_args_to_match() {
        let mut program = parse(
            "#[cfg(flag, missing)]\ni32 a() { return 1; }\n#[cfg(flag, other)]\ni32 b() { return 2; }\n",
        );
        let set = CfgSet::parse(&["flag".to_string()]);
        let _ = gate_items(&mut program, &set);
        assert!(program.items.is_empty());
    }

    #[test]
    fn gate_accepts_dotted_and_string_keys() {
        let mut program = parse(
            "#[cfg(cpu.sse41)]\ni32 a() { return 1; }\n#[cfg(\"plain\")]\ni32 b() { return 2; }\n",
        );
        let set = CfgSet::parse(&["cpu.sse41=1,plain".to_string()]);
        let errors = gate_items(&mut program, &set);
        assert!(errors.is_empty());
        assert_eq!(program.items.len(), 2);
    }

    #[test]
    fn gate_applies_to_impl_methods() {
        let mut program = parse(
            "struct Foo { i64 x; }\nimpl Foo {\n  #[cfg(on)]\n  i64 gated() { return 1; }\n  #[cfg(off)]\n  i64 dropped() { return 2; }\n  i64 plain() { return 3; }\n}\n",
        );
        let set = CfgSet::parse(&["on".to_string()]);
        let errors = gate_items(&mut program, &set);
        assert!(errors.is_empty());
        let ast::ItemKind::Impl(impl_item) = &program.items[1].kind else {
            panic!("expected impl");
        };
        let names: Vec<&str> = impl_item
            .items
            .iter()
            .filter_map(|m| match m {
                ast::ImplItemKind::Function(f) => Some(f.name.name.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(names, vec!["gated", "plain"]);
    }

    #[test]
    fn gate_reports_malformed_cfg() {
        let mut program = parse("#[cfg(42)]\ni32 a() { return 1; }\n");
        let set = CfgSet::default();
        let errors = gate_items(&mut program, &set);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn derives_debug_release_from_opt_level() {
        let mut set = CfgSet::default();
        add_derived_cfgs(&mut set, None, None);
        assert!(set.contains("debug"));
        assert!(!set.contains("release"));

        let mut set = CfgSet::default();
        add_derived_cfgs(&mut set, Some("2"), None);
        assert!(!set.contains("debug"));
        assert!(set.contains("release"));

        let mut set = CfgSet::default();
        add_derived_cfgs(&mut set, Some("fast"), None);
        assert!(set.contains("release"));
    }

    #[test]
    fn user_cfg_wins_over_derived() {
        let mut set = CfgSet::parse(&["debug=1".to_string()]);
        add_derived_cfgs(&mut set, Some("2"), None);
        // -O2 derives release, but the explicit debug=1 must survive.
        assert!(set.contains("debug"));
        assert!(set.contains("release"));
    }

    #[test]
    fn derives_arch_and_os_from_target() {
        let mut set = CfgSet::default();
        add_derived_cfgs(&mut set, None, Some("x86_64-unknown-linux-gnu"));
        assert!(set.contains("arch.x86_64"));
        assert!(set.contains("os.linux"));

        let mut set = CfgSet::default();
        add_derived_cfgs(&mut set, None, Some("aarch64-apple-darwin"));
        assert!(set.contains("arch.aarch64"));
        assert!(set.contains("os.darwin"));
    }
}
