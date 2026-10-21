use crate::lexer::Span;
use crate::parser::ast;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttributeError {
    pub message: String,
    pub span: Span,
}

pub fn validate_global_attributes(attributes: &[ast::Attribute]) -> Vec<AttributeError> {
    let mut errors = Vec::new();
    for attr in attributes {
        if !is_program_level_attribute(&attr.name.name) {
            continue;
        }
        if attr.name.name.as_str() == "link"
            && let Err(error) = parse_link_attribute(attr)
        {
            errors.push(error);
        }
    }
    errors
}

pub fn collect_program_link_libraries(
    program: &ast::Program,
) -> Result<Vec<String>, AttributeError> {
    let mut libs = Vec::new();
    extend_link_libraries_from_attributes(&mut libs, &program.attributes)?;
    Ok(libs)
}

pub fn extend_unique_libs<I, S>(dst: &mut Vec<String>, libs: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    for lib in libs {
        let lib = lib.as_ref();
        if !dst.iter().any(|existing| existing == lib) {
            dst.push(lib.to_string());
        }
    }
}

fn extend_link_libraries_from_attributes(
    libs: &mut Vec<String>,
    attributes: &[ast::Attribute],
) -> Result<(), AttributeError> {
    for attr in attributes {
        if let Some(lib) = parse_link_attribute(attr)? {
            extend_unique_libs(libs, [lib]);
        }
    }
    Ok(())
}

fn parse_link_attribute(attr: &ast::Attribute) -> Result<Option<String>, AttributeError> {
    if attr.name.name != "link" {
        return Ok(None);
    }

    if attr.args.len() != 1 {
        return Err(AttributeError {
            message: "link expects exactly one library name".to_string(),
            span: attr.span,
        });
    }

    let lib = match &attr.args[0] {
        ast::AttributeArg::Identifier(ident) if !ident.name.is_empty() => ident.name.clone(),
        ast::AttributeArg::Literal(ast::Literal::String(value)) if !value.is_empty() => {
            value.clone()
        }
        _ => {
            return Err(AttributeError {
                message: "link expects a library name identifier or string literal".to_string(),
                span: attr.span,
            });
        }
    };

    Ok(Some(lib))
}

/// Splits attributes into program-level and item-level attributes.
/// Known program-level attributes: `link`.
pub fn filter_program_attributes(
    attrs: Vec<ast::Attribute>,
) -> (Vec<ast::Attribute>, Vec<ast::Attribute>) {
    let mut program = Vec::new();
    let mut item = Vec::new();
    for attr in attrs {
        if is_program_level_attribute(&attr.name.name) {
            program.push(attr);
        } else {
            item.push(attr);
        }
    }
    (program, item)
}

fn is_program_level_attribute(name: &str) -> bool {
    matches!(name, "link")
}

/// Extracts a `#[link_name("...")]` value from an attribute list, if present.
/// Returns `None` if no valid `#[link_name]` attribute is found.
pub fn function_link_name(attributes: &[ast::Attribute]) -> Option<&str> {
    for attr in attributes {
        if attr.name.name == "link_name"
            && let Some(ast::AttributeArg::Literal(ast::Literal::String(s))) = attr.args.first()
            && !s.is_empty()
        {
            return Some(s);
        }
    }
    None
}

/// Extracts a `#[link_module("...")]` value from an attribute list, if present.
///
/// Used with extern declarations on WebAssembly targets: the function becomes
/// an *import* from the named wasm module (e.g. `wasi_snapshot_preview1`),
/// emitted as `wasm-import-module`/`wasm-import-name` LLVM attributes. On
/// native targets the attribute is inert (imports there come from `#[link]`
/// libraries at the symbol level).
pub fn function_link_module(attributes: &[ast::Attribute]) -> Option<&str> {
    for attr in attributes {
        if attr.name.name == "link_module"
            && let Some(ast::AttributeArg::Literal(ast::Literal::String(s))) = attr.args.first()
            && !s.is_empty()
        {
            return Some(s);
        }
    }
    None
}

/// Map a `#[target_feature("name")]` name (the same namespace as the
/// `cpu.*` cfg keys and `std/cpu.ag` probe globals) to the LLVM x86 target
/// feature string. `None` for unknown names — typeck reports those.
pub fn llvm_target_feature(name: &str) -> Option<&'static str> {
    match name {
        "sse41" => Some("sse4.1"),
        "sse42" => Some("sse4.2"),
        "popcnt" => Some("popcnt"),
        "fma" => Some("fma"),
        "avx" => Some("avx"),
        "avx2" => Some("avx2"),
        "avx512f" => Some("avx512f"),
        _ => None,
    }
}

/// True when the function carries `#[inline(always)]`, requesting the LLVM
/// alwaysinline attribute so the always-inline pass inlines it everywhere.
pub fn function_always_inline(attributes: &[ast::Attribute]) -> bool {
    attributes.iter().any(|attr| {
        attr.name.name == "inline"
            && attr.args.len() == 1
            && matches!(&attr.args[0], ast::AttributeArg::Identifier(id) if id.name == "always")
    })
}

/// True when the function carries `#[test]`.
pub fn is_test_function(attributes: &[ast::Attribute]) -> bool {
    attributes.iter().any(|attr| attr.name.name == "test")
}

/// Collect every `#[target_feature("...")]` name on a function into an LLVM
/// `target-features` attribute value ("+a,+b"), or `None` when none are
/// present. Multiple attributes AND-compose.
pub fn function_target_features(attributes: &[ast::Attribute]) -> Option<String> {
    let mut features: Vec<&str> = Vec::new();
    for attr in attributes {
        if attr.name.name != "target_feature" {
            continue;
        }
        if let Some(ast::AttributeArg::Literal(ast::Literal::String(name))) = attr.args.first()
            && let Some(feature) = llvm_target_feature(name)
        {
            features.push(feature);
        }
    }
    if features.is_empty() {
        None
    } else {
        Some(format!("+{}", features.join(",+")))
    }
}
