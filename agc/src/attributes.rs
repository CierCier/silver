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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::Parser;

    #[test]
    fn target_features_join_with_plus_prefix() {
        let program = parse(
            "#[target_feature(\"avx2\")]\n#[target_feature(\"fma\")]\ni64 f() { return 1; }\n",
        );
        let attrs = &program.items[0].attributes;
        assert_eq!(
            function_target_features(attrs).as_deref(),
            Some("+avx2,+fma")
        );
        // Probe names map to LLVM feature names.
        assert_eq!(llvm_target_feature("sse41"), Some("sse4.1"));
        assert_eq!(llvm_target_feature("avx512f"), Some("avx512f"));
        assert_eq!(llvm_target_feature("nope"), None);
    }

    #[test]
    fn no_target_features_returns_none() {
        let program = parse("#[link_name(\"strlen\")]\ni64 f() { return 1; }\n");
        assert_eq!(function_target_features(&program.items[0].attributes), None);
    }

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    #[test]
    fn collects_link_libraries_in_program_order() {
        let program = parse(
            r#"
            #[link(m)]
            extern "C" f32 sinf(f32 x);

            #[link("pthread")]
            i32 main() { return 0; }
        "#,
        );

        let libs = collect_program_link_libraries(&program).expect("collect should succeed");
        assert_eq!(libs, vec!["m".to_string(), "pthread".to_string()]);
    }

    #[test]
    fn deduplicates_link_libraries() {
        let program = parse(
            r#"
            #[link(m)]
            extern "C" f32 sinf(f32 x);
            #[link(m)]
            i32 main() { return 0; }
        "#,
        );

        let libs = collect_program_link_libraries(&program).expect("collect should succeed");
        assert_eq!(libs, vec!["m".to_string()]);
    }

    #[test]
    fn rejects_invalid_link_attribute_argument() {
        let program = parse(
            r#"
            #[link(1)]
            i32 main() { return 0; }
        "#,
        );

        let error = collect_program_link_libraries(&program).expect_err("collect should fail");
        assert_eq!(
            error.message,
            "link expects a library name identifier or string literal"
        );
    }
}
