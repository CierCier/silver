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
        if attr.name.name == "link"
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
    for item in &program.items {
        extend_link_libraries_from_attributes(&mut libs, &item.attributes)?;
    }
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
            span: attr.span.clone(),
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
                span: attr.span.clone(),
            });
        }
    };

    Ok(Some(lib))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::Parser;

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
