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

    // Native builds (no explicit --target) advertise the host CPU features
    // as cpu.* keys. `@cfg(cpu.x)` then folds to the runtime probe global
    // (g_has_x), so dispatch stays correct on any machine; `#[cfg(cpu.x)]`
    // items compile in when the build host supports the feature. Only keys
    // whose probe globals std/cpu.ag defines are derived.
    if target.is_none() {
        #[cfg(target_arch = "x86_64")]
        {
            if std::is_x86_feature_detected!("sse4.1") {
                set.insert_if_absent("cpu.sse41");
            }
            if std::is_x86_feature_detected!("sse4.2") {
                set.insert_if_absent("cpu.sse42");
            }
            if std::is_x86_feature_detected!("popcnt") {
                set.insert_if_absent("cpu.popcnt");
            }
            if std::is_x86_feature_detected!("fma") {
                set.insert_if_absent("cpu.fma");
            }
            if std::is_x86_feature_detected!("avx") {
                set.insert_if_absent("cpu.avx");
            }
            if std::is_x86_feature_detected!("avx2") {
                set.insert_if_absent("cpu.avx2");
            }
            if std::is_x86_feature_detected!("avx512f") {
                set.insert_if_absent("cpu.avx512f");
            }
        }
    }
}

/// Best-effort OS extraction from a target triple: the token that matches a
/// known OS name, else the vendor position. WASI triples (wasm32-wasi,
/// wasm32-wasip1, wasm32-wasip2) normalize to `wasi` regardless of where the
/// token sits, so `@cfg(os.wasi)` gates the std seam consistently.
fn triple_os(triple: &str) -> &str {
    for os in [
        "linux", "darwin", "macos", "windows", "freebsd", "netbsd", "openbsd", "solaris",
        "illumos", "android", "ios", "haiku",
    ] {
        if triple.split('-').any(|part| part == os) {
            return os;
        }
    }
    let parts: Vec<&str> = triple.split('-').collect();
    for part in &parts {
        if *part == "wasi" || part.starts_with("wasip") {
            return "wasi";
        }
    }
    parts.get(1).copied().unwrap_or("unknown")
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
pub(crate) fn eval_cfg_attrs(
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

/// Evaluate a top-level `if (@cfg(...))` condition to a compile-time bool.
///
/// Supports `@cfg(key)`, `!expr`, `a && b`, `a || b` (with short-circuit
/// folding so `false && <runtime>` is still `false`). Returns `None` when
/// the condition is not a compile-time cfg (e.g. `cpu.*` probes, which fold
/// to a runtime global, or any non-cfg expression).
pub fn eval_cfg_expr_to_bool(expr: &ast::Expression, cfg: &CfgSet) -> Option<bool> {
    match expr.kind.as_ref() {
        ast::ExpressionKind::Literal(ast::Literal::Bool(v)) => Some(*v),
        ast::ExpressionKind::MacroCall { name, args } if name.name == "cfg" => {
            eval_cfg_call_to_bool(cfg, args)
        }
        ast::ExpressionKind::Unary { operator, operand }
            if *operator == ast::UnaryOperator::Not =>
        {
            eval_cfg_expr_to_bool(operand, cfg).map(|v| !v)
        }
        ast::ExpressionKind::Binary {
            left,
            right,
            operator,
        } => match operator {
            ast::BinaryOperator::LogicalAnd => match eval_cfg_expr_to_bool(left, cfg) {
                Some(false) => Some(false),
                Some(true) => eval_cfg_expr_to_bool(right, cfg),
                None => match eval_cfg_expr_to_bool(right, cfg) {
                    Some(false) => Some(false),
                    _ => None,
                },
            },
            ast::BinaryOperator::LogicalOr => match eval_cfg_expr_to_bool(left, cfg) {
                Some(true) => Some(true),
                Some(false) => eval_cfg_expr_to_bool(right, cfg),
                None => match eval_cfg_expr_to_bool(right, cfg) {
                    Some(true) => Some(true),
                    _ => None,
                },
            },
            _ => None,
        },
        _ => None,
    }
}

/// Single `@cfg(key)` call to bool. `cpu.*` keys are runtime probes, so they
/// return `None` (cannot gate items at compile time). Malformed calls also
/// return `None`.
fn eval_cfg_call_to_bool(cfg: &CfgSet, args: &[ast::MacroArg]) -> Option<bool> {
    if args.len() != 1 {
        return None;
    }
    let key = match &args[0] {
        ast::MacroArg::Expression(expr) => cfg_call_key(expr)?,
        ast::MacroArg::Identifier(id) => id.name.clone(),
        _ => return None,
    };
    if key.strip_prefix("cpu.").is_some() {
        return None;
    }
    Some(cfg.contains(&key))
}

fn cfg_call_key(expr: &ast::Expression) -> Option<String> {
    match expr.kind.as_ref() {
        ast::ExpressionKind::Identifier(id) => Some(id.name.clone()),
        ast::ExpressionKind::Literal(ast::Literal::String(v)) => Some(v.clone()),
        ast::ExpressionKind::FieldAccess { object, field } => {
            Some(format!("{}.{}", cfg_call_key(object)?, field.name))
        }
        _ => None,
    }
}

/// Expand top-level `if (@cfg(...)) { items } [else { items }]` blocks.
///
/// Must run BEFORE import lowering so dead-branch imports are never resolved,
/// and before `gate_items` so inner `#[cfg]` attributes are still honored.
/// `else if` chains appear as a single nested `CfgIf` inside `else_items`
/// and are handled recursively. Non-compile-time conditions (e.g. `cpu.*` or
/// non-`@cfg` expressions) are reported as errors and drop the whole block.
pub fn expand_cfg_blocks(program: &mut ast::Program, cfg: &CfgSet) -> Vec<CfgError> {
    let mut errors = Vec::new();
    let items = std::mem::take(&mut program.items);
    program.items = expand_item_list(items, cfg, &mut errors);
    errors
}

fn expand_item_list(
    items: Vec<ast::Item>,
    cfg: &CfgSet,
    errors: &mut Vec<CfgError>,
) -> Vec<ast::Item> {
    let mut out = Vec::with_capacity(items.len());
    for item in items {
        match item.kind {
            ast::ItemKind::CfgIf(cfg_if) => {
                // An outer #[cfg] on the `if` itself gates the whole block.
                if !eval_cfg_attrs(&item.attributes, cfg, errors).unwrap_or(true) {
                    continue;
                }
                match eval_cfg_expr_to_bool(&cfg_if.condition, cfg) {
                    Some(true) => {
                        out.extend(expand_item_list(cfg_if.then_items, cfg, errors));
                    }
                    Some(false) => {
                        out.extend(expand_item_list(cfg_if.else_items, cfg, errors));
                    }
                    None => {
                        errors.push(CfgError {
                            message: "top-level if condition must be a compile-time @cfg(...) expression (cpu.* is runtime-only and cannot gate items)".to_string(),
                            span: cfg_if.condition.span,
                        });
                    }
                }
            }
            _ => out.push(item),
        }
    }
    out
}
