//! Extensible serialization system and automatic ToJson/FromJson synthesis.
//!
//! Supports `#[serialize]` (defaulting to JSON, extensible to YAML, TOML, etc.)
//! and auto-synthesizes `ToJson` and `FromJson` implementations for structs
//! referenced in `@json(...)` or `@from_json(...)` when explicit implementations
//! are not present.

use crate::lexer;
use crate::parser::ast;
use crate::parser::prt_parser::PRT_Parser;
use rustc_hash::{FxHashMap, FxHashSet};

/// Supported serialization format backends.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SerializationFormat {
    Json,
    Yaml,
    Toml,
}

impl SerializationFormat {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_ascii_lowercase().as_str() {
            "json" => Some(Self::Json),
            "yaml" | "yml" => Some(Self::Yaml),
            "toml" => Some(Self::Toml),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::Json => "json",
            Self::Yaml => "yaml",
            Self::Toml => "toml",
        }
    }

    pub fn tag_key(&self) -> &'static str {
        match self {
            Self::Json => "json",
            Self::Yaml => "yaml",
            Self::Toml => "toml",
        }
    }

    pub fn to_trait_name(&self) -> &'static str {
        match self {
            Self::Json => "ToJson",
            Self::Yaml => "ToYaml",
            Self::Toml => "ToToml",
        }
    }

    pub fn from_trait_name(&self) -> &'static str {
        match self {
            Self::Json => "FromJson",
            Self::Yaml => "FromYaml",
            Self::Toml => "FromToml",
        }
    }

    pub fn writer_type(&self) -> &'static str {
        match self {
            Self::Json => "JsonWriter",
            Self::Yaml => "YamlWriter",
            Self::Toml => "TomlWriter",
        }
    }

    pub fn reader_type(&self) -> &'static str {
        match self {
            Self::Json => "JsonReader",
            Self::Yaml => "YamlReader",
            Self::Toml => "TomlReader",
        }
    }

    pub fn error_type(&self) -> &'static str {
        match self {
            Self::Json => "JsonError",
            Self::Yaml => "YamlError",
            Self::Toml => "TomlError",
        }
    }

    pub fn to_method_name(&self) -> &'static str {
        match self {
            Self::Json => "to_json",
            Self::Yaml => "to_yaml",
            Self::Toml => "to_toml",
        }
    }

    pub fn from_method_name(&self) -> &'static str {
        match self {
            Self::Json => "from_json",
            Self::Yaml => "from_yaml",
            Self::Toml => "from_toml",
        }
    }
}

/// Parses the arguments of a `#[serialize(...)]` attribute into format backends.
pub fn parse_serialize_attribute(
    attr: &ast::Attribute,
) -> Result<Vec<SerializationFormat>, String> {
    if attr.name.name != "serialize" {
        return Ok(Vec::new());
    }
    if attr.args.is_empty() {
        return Ok(vec![SerializationFormat::Json]);
    }
    let mut formats = Vec::new();
    for arg in &attr.args {
        let format_str = match arg {
            ast::AttributeArg::Identifier(id) => id.name.as_str(),
            ast::AttributeArg::Literal(ast::Literal::String(s)) => s.as_str(),
            _ => {
                return Err(
                    "serialize expects format identifiers or strings (e.g. json, yaml, toml)"
                        .to_string(),
                );
            }
        };
        match SerializationFormat::from_str(format_str) {
            Some(fmt) => {
                if !formats.contains(&fmt) {
                    formats.push(fmt);
                }
            }
            None => {
                return Err(format!(
                    "unknown serialization format '{format_str}'; supported formats: json, yaml, toml"
                ));
            }
        }
    }
    Ok(formats)
}

/// Helper to canonicalize a type into a simple name string.
fn type_to_canonical_name(ty: &ast::Type) -> String {
    match ty.kind.as_ref() {
        ast::TypeKind::Primitive(p) => match p {
            ast::PrimitiveType::I8 => "i8".to_string(),
            ast::PrimitiveType::I16 => "i16".to_string(),
            ast::PrimitiveType::I32 => "i32".to_string(),
            ast::PrimitiveType::I64 => "i64".to_string(),
            ast::PrimitiveType::I128 => "i128".to_string(),
            ast::PrimitiveType::U8 => "u8".to_string(),
            ast::PrimitiveType::U16 => "u16".to_string(),
            ast::PrimitiveType::U32 => "u32".to_string(),
            ast::PrimitiveType::U64 => "u64".to_string(),
            ast::PrimitiveType::U128 => "u128".to_string(),
            ast::PrimitiveType::F32 => "f32".to_string(),
            ast::PrimitiveType::F64 => "f64".to_string(),
            ast::PrimitiveType::F80 => "f80".to_string(),
            ast::PrimitiveType::Bool => "bool".to_string(),
            ast::PrimitiveType::Str => "str".to_string(),
            ast::PrimitiveType::Char => "char".to_string(),
            ast::PrimitiveType::Void => "void".to_string(),
            _ => "unknown".to_string(),
        },
        ast::TypeKind::Named(named) => {
            named.path.iter().map(|id| id.name.as_str()).collect::<Vec<_>>().join("::")
        }
        _ => "other".to_string(),
    }
}

fn is_internal_stdlib_struct(name: &str) -> bool {
    name.starts_with("__Buf")
        || name.starts_with('_')
        || matches!(
            name,
            "JsonWriter"
                | "JsonReader"
                | "File"
                | "Arena"
                | "TaskRecord"
                | "Scanner"
                | "ByteStream"
                | "BufWriter"
                | "SplitIter"
                | "StringBytesIter"
                | "StringIntoIter"
                | "StringCharIter"
        )
}

/// Synthesize `ToJson` and `FromJson` implementations for structs that have
/// `#[serialize]` or are used with `@json` / `@from_json` when explicit implementations
/// do not already exist in the program.
pub fn synthesize_serialization_for_program(program: &mut ast::Program) {
    // 1. Collect all explicit trait impls for each struct
    let mut existing_to_impls: FxHashSet<(String, String)> = FxHashSet::default(); // (trait_name, struct_name)
    let mut structs: Vec<ast::StructItem> = Vec::new();
    let mut structs_with_attr: FxHashSet<String> = FxHashSet::default();

    for item in &program.items {
        match &item.kind {
            ast::ItemKind::Struct(s) => {
                structs.push(s.clone());
                for attr in &item.attributes {
                    if attr.name.name == "serialize" {
                        structs_with_attr.insert(s.name.name.clone());
                    }
                }
            }
            ast::ItemKind::Impl(impl_item) => {
                if let Some(trait_ref) = &impl_item.trait_ref
                    && let Some(trait_id) = trait_ref.path.last()
                    && let ast::TypeKind::Named(named) = impl_item.self_type.kind.as_ref()
                    && let Some(self_id) = named.path.last()
                {
                    existing_to_impls.insert((trait_id.name.clone(), self_id.name.clone()));
                }
            }
            _ => {}
        }
    }

    // 2. Scan program expressions for target struct types used with @json / @from_json
    let mut json_target_types: FxHashSet<String> = FxHashSet::default();
    scan_program_for_json_targets(program, &mut json_target_types);

    // 3. For each struct, synthesize missing ToJson / FromJson
    let mut synthesized_items = Vec::new();

    for s in &structs {
        // Only synthesize for non-generic structs
        if s.generics.is_some() {
            continue;
        }
        let struct_name = &s.name.name;
        if is_internal_stdlib_struct(struct_name) {
            continue;
        }

        let is_explicit = structs_with_attr.contains(struct_name);
        let is_target = json_target_types.contains(struct_name);

        let should_synthesize = is_explicit || is_target;

        if !should_synthesize {
            continue;
        }

        // ToJson
        if !existing_to_impls.contains(&("ToJson".to_string(), struct_name.clone())) {
            let to_json_src = generate_to_json_source(s);
            if let Some(item) = parse_impl_snippet(&to_json_src) {
                synthesized_items.push(item);
                existing_to_impls.insert(("ToJson".to_string(), struct_name.clone()));
            }
        }

        // FromJson
        if !existing_to_impls.contains(&("FromJson".to_string(), struct_name.clone())) {
            let from_json_src = generate_from_json_source(s);
            if let Some(item) = parse_impl_snippet(&from_json_src) {
                synthesized_items.push(item);
                existing_to_impls.insert(("FromJson".to_string(), struct_name.clone()));
            }
        }
    }

    program.items.extend(synthesized_items);
}

fn scan_program_for_json_targets(program: &ast::Program, targets: &mut FxHashSet<String>) {
    for item in &program.items {
        match &item.kind {
            ast::ItemKind::Function(func) => {
                let mut var_types = FxHashMap::default();
                for param in &func.parameters {
                    let ty_name = type_to_canonical_name(&param.param_type);
                    if !ty_name.is_empty() && ty_name != "other" && ty_name != "unknown" {
                        var_types.insert(param.name.name.clone(), ty_name);
                    }
                }
                scan_block_for_json_targets(&func.body, &mut var_types, targets);
            }
            ast::ItemKind::Impl(impl_item) => {
                for member in &impl_item.items {
                    if let ast::ImplItemKind::Function(func) = member {
                        let mut var_types = FxHashMap::default();
                        for param in &func.parameters {
                            let ty_name = type_to_canonical_name(&param.param_type);
                            if !ty_name.is_empty() && ty_name != "other" && ty_name != "unknown" {
                                var_types.insert(param.name.name.clone(), ty_name);
                            }
                        }
                        scan_block_for_json_targets(&func.body, &mut var_types, targets);
                    }
                }
            }
            _ => {}
        }
    }
}

fn scan_block_for_json_targets(
    block: &ast::Block,
    var_types: &mut FxHashMap<String, String>,
    targets: &mut FxHashSet<String>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            ast::StatementKind::Let(let_stmt) => {
                if let ast::PatternKind::Identifier(id) = &let_stmt.pattern.kind {
                    if let Some(ty) = &let_stmt.type_annotation {
                        let ty_name = type_to_canonical_name(ty);
                        if !ty_name.is_empty() && ty_name != "other" && ty_name != "unknown" {
                            var_types.insert(id.name.clone(), ty_name);
                        }
                    }
                }
                if let Some(init) = &let_stmt.initializer {
                    scan_expr_for_json_targets(init, var_types, targets);
                }
            }
            ast::StatementKind::Expression(expr)
            | ast::StatementKind::Return(Some(expr))
            | ast::StatementKind::Break(Some(expr)) => {
                scan_expr_for_json_targets(expr, var_types, targets);
            }
            ast::StatementKind::Block(inner) => {
                scan_block_for_json_targets(inner, var_types, targets);
            }
            _ => {}
        }
    }
}

fn scan_expr_for_json_targets(
    expr: &ast::Expression,
    var_types: &FxHashMap<String, String>,
    targets: &mut FxHashSet<String>,
) {
    match expr.kind.as_ref() {
        ast::ExpressionKind::MacroCall { name, args } => {
            if name.name == "json" {
                if let Some(ast::MacroArg::Expression(arg_expr)) = args.first() {
                    match arg_expr.kind.as_ref() {
                        ast::ExpressionKind::Identifier(id) => {
                            if let Some(ty_name) = var_types.get(&id.name) {
                                targets.insert(ty_name.clone());
                            }
                        }
                        ast::ExpressionKind::StructLiteral { path, .. } => {
                            let ty_name = path
                                .iter()
                                .map(|id| id.name.as_str())
                                .collect::<Vec<_>>()
                                .join("::");
                            if !ty_name.is_empty() {
                                targets.insert(ty_name);
                            }
                        }
                        _ => {}
                    }
                }
            } else if name.name == "from_json" {
                if let Some(ast::MacroArg::Expression(target_expr)) = args.first() {
                    match target_expr.kind.as_ref() {
                        ast::ExpressionKind::TypeName(ty) => {
                            targets.insert(type_to_canonical_name(ty));
                        }
                        ast::ExpressionKind::Identifier(id) => {
                            targets.insert(id.name.clone());
                        }
                        _ => {}
                    }
                }
            }
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            scan_expr_for_json_targets(left, var_types, targets);
            scan_expr_for_json_targets(right, var_types, targets);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. }
        | ast::ExpressionKind::Move(operand)
        | ast::ExpressionKind::Reference {
            expression: operand,
            ..
        } => {
            scan_expr_for_json_targets(operand, var_types, targets);
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            scan_expr_for_json_targets(function, var_types, targets);
            for arg in arguments {
                scan_expr_for_json_targets(arg, var_types, targets);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            scan_expr_for_json_targets(receiver, var_types, targets);
            for arg in arguments {
                scan_expr_for_json_targets(arg, var_types, targets);
            }
        }
        _ => {}
    }
}

pub fn generate_to_json_source(s: &ast::StructItem) -> String {
    let struct_name = &s.name.name;
    let mut body = String::new();
    body.push_str(&format!("impl ToJson for {struct_name} {{\n"));
    body.push_str(&format!(
        "    void to_json({struct_name}* self, JsonWriter* out) {{\n"
    ));
    body.push_str("        out.begin_object();\n");

    for field in &s.fields {
        let tag_val = field
            .tags
            .get("json")
            .map(|s| s.as_str())
            .unwrap_or(field.name.name.as_str());
        let json_key = tag_val.split(',').next().unwrap_or(tag_val).trim();
        if json_key == "-" {
            continue;
        }
        let field_name = &field.name.name;
        body.push_str(&format!("        out.field(\"{json_key}\");\n"));

        let canonical = type_to_canonical_name(&field.field_type);
        match canonical.as_str() {
            "i8" | "i16" | "i32" | "i64" | "i128" => {
                body.push_str(&format!(
                    "        out.write_i64((i64)(*self).{field_name});\n"
                ));
            }
            "u8" | "u16" | "u32" | "u64" | "u128" => {
                body.push_str(&format!(
                    "        out.write_u64((u64)(*self).{field_name});\n"
                ));
            }
            "f32" | "f64" => {
                body.push_str(&format!(
                    "        out.write_f64((f64)(*self).{field_name});\n"
                ));
            }
            "bool" => {
                body.push_str(&format!(
                    "        out.write_bool((*self).{field_name});\n"
                ));
            }
            "str" => {
                body.push_str(&format!(
                    "        out.write_string((*self).{field_name});\n"
                ));
            }
            "String" => {
                body.push_str(&format!(
                    "        out.write_string((str)(*self).{field_name}.data);\n"
                ));
            }
            _ => {
                body.push_str(&format!("        (*self).{field_name}.to_json(out);\n"));
            }
        }
    }

    body.push_str("        out.end_object();\n");
    body.push_str("    }\n");
    body.push_str("}\n");
    body
}

pub fn generate_from_json_source(s: &ast::StructItem) -> String {
    let struct_name = &s.name.name;
    let mut body = String::new();
    body.push_str(&format!("impl FromJson for {struct_name} {{\n"));
    body.push_str(&format!(
        "    Result<{struct_name}, JsonError> from_json(JsonReader* input) {{\n"
    ));
    body.push_str("        if (!input.begin_object()) {\n");
    body.push_str("            JsonError err = JsonError.invalid();\n");
    body.push_str(&format!(
        "            return Result<{struct_name}, JsonError>.Err(move err);\n"
    ));
    body.push_str("        }\n");
    body.push_str(&format!("        {struct_name} result;\n"));

    if !s.fields.is_empty() {
        body.push_str("        while (true) {\n");
        body.push_str("            String key = input.read_string();\n");
        body.push_str("            if (input.failed) { break; }\n");
        body.push_str("            if (!input.expect((u8)58)) {\n");
        body.push_str("                JsonError err = JsonError.invalid();\n");
        body.push_str(&format!(
            "                return Result<{struct_name}, JsonError>.Err(move err);\n"
        ));
        body.push_str("            }\n");

        let mut first = true;
        for field in &s.fields {
            let tag_val = field
                .tags
                .get("json")
                .map(|s| s.as_str())
                .unwrap_or(field.name.name.as_str());
            let json_key = tag_val.split(',').next().unwrap_or(tag_val).trim();
            if json_key == "-" {
                continue;
            }
            let field_name = &field.name.name;
            let else_prefix = if first { "" } else { "else " };
            first = false;

            body.push_str(&format!(
                "            {else_prefix}if (key.equals(\"{json_key}\")) {{\n"
            ));

            let canonical = type_to_canonical_name(&field.field_type);
            match canonical.as_str() {
                "i8" | "i16" | "i32" | "i64" | "i128" => {
                    body.push_str("                i64 val = 0;\n");
                    body.push_str("                if (!input.read_i64(&val)) {\n");
                    body.push_str("                    JsonError err = JsonError.invalid();\n");
                    body.push_str(&format!(
                        "                    return Result<{struct_name}, JsonError>.Err(move err);\n"
                    ));
                    body.push_str("                }\n");
                    body.push_str(&format!(
                        "                result.{field_name} = ({canonical})val;\n"
                    ));
                }
                "u8" | "u16" | "u32" | "u64" | "u128" => {
                    body.push_str("                u64 val = 0;\n");
                    body.push_str("                if (!input.read_u64(&val)) {\n");
                    body.push_str("                    JsonError err = JsonError.invalid();\n");
                    body.push_str(&format!(
                        "                    return Result<{struct_name}, JsonError>.Err(move err);\n"
                    ));
                    body.push_str("                }\n");
                    body.push_str(&format!(
                        "                result.{field_name} = ({canonical})val;\n"
                    ));
                }
                "f32" | "f64" => {
                    body.push_str("                f64 val = 0.0;\n");
                    body.push_str("                if (!input.read_f64(&val)) {\n");
                    body.push_str("                    JsonError err = JsonError.invalid();\n");
                    body.push_str(&format!(
                        "                    return Result<{struct_name}, JsonError>.Err(move err);\n"
                    ));
                    body.push_str("                }\n");
                    body.push_str(&format!(
                        "                result.{field_name} = ({canonical})val;\n"
                    ));
                }
                "bool" => {
                    body.push_str("                bool val = false;\n");
                    body.push_str("                if (!input.read_bool(&val)) {\n");
                    body.push_str("                    JsonError err = JsonError.invalid();\n");
                    body.push_str(&format!(
                        "                    return Result<{struct_name}, JsonError>.Err(move err);\n"
                    ));
                    body.push_str("                }\n");
                    body.push_str(&format!(
                        "                result.{field_name} = val;\n"
                    ));
                }
                "str" => {
                    body.push_str("                String val = input.read_string();\n");
                    body.push_str("                if (input.failed) {\n");
                    body.push_str("                    JsonError err = JsonError.invalid();\n");
                    body.push_str(&format!(
                        "                    return Result<{struct_name}, JsonError>.Err(move err);\n"
                    ));
                    body.push_str("                }\n");
                    body.push_str(&format!(
                        "                result.{field_name} = (str)val.data;\n                val.data = (u8*)0;\n"
                    ));
                }
                "String" => {
                    body.push_str("                String val = input.read_string();\n");
                    body.push_str("                if (input.failed) {\n");
                    body.push_str("                    JsonError err = JsonError.invalid();\n");
                    body.push_str(&format!(
                        "                    return Result<{struct_name}, JsonError>.Err(move err);\n"
                    ));
                    body.push_str("                }\n");
                    body.push_str(&format!(
                        "                result.{field_name} = move val;\n"
                    ));
                }
                _ => {}
            }
            body.push_str("            }\n");
        }
        body.push_str("            if (!input.comma()) { break; }\n");
        body.push_str("        }\n");
    }

    body.push_str("        if (!input.end_object()) {\n");
    body.push_str("            JsonError err = JsonError.invalid();\n");
    body.push_str(&format!(
        "            return Result<{struct_name}, JsonError>.Err(move err);\n"
    ));
    body.push_str("        }\n");
    body.push_str(&format!(
        "        return Result<{struct_name}, JsonError>.ok(move result);\n"
    ));
    body.push_str("    }\n");
    body.push_str("}\n");
    body
}

pub fn parse_impl_snippet(source: &str) -> Option<ast::Item> {
    let tokens = lexer::lex(source).ok()?;
    let code_tokens: Vec<_> = tokens
        .into_iter()
        .filter(|t| !matches!(t.kind, lexer::Token::Comment { .. } | lexer::Token::Eof))
        .collect();
    let mut parser = PRT_Parser::new(None);
    let program = parser.parse_program(&code_tokens).ok()?;
    program.items.into_iter().next()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_serialize_attr_defaults_to_json() {
        let attr = ast::Attribute {
            name: ast::Identifier {
                name: "serialize".to_string(),
                span: lexer::Span::default(),
            },
            args: Vec::new(),
            span: lexer::Span::default(),
        };
        let formats = parse_serialize_attribute(&attr).expect("should parse");
        assert_eq!(formats, vec![SerializationFormat::Json]);
    }

    #[test]
    fn parse_serialize_attr_multi_format() {
        let attr = ast::Attribute {
            name: ast::Identifier {
                name: "serialize".to_string(),
                span: lexer::Span::default(),
            },
            args: vec![
                ast::AttributeArg::Identifier(ast::Identifier {
                    name: "json".to_string(),
                    span: lexer::Span::default(),
                }),
                ast::AttributeArg::Identifier(ast::Identifier {
                    name: "yaml".to_string(),
                    span: lexer::Span::default(),
                }),
                ast::AttributeArg::Literal(ast::Literal::String("toml".to_string())),
            ],
            span: lexer::Span::default(),
        };
        let formats = parse_serialize_attribute(&attr).expect("should parse");
        assert_eq!(
            formats,
            vec![
                SerializationFormat::Json,
                SerializationFormat::Yaml,
                SerializationFormat::Toml
            ]
        );
    }
}
