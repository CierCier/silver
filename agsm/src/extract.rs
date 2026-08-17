use std::collections::BTreeSet;
use std::fmt;
use std::path::Path;

use agc::module_artifact::{
    ExportKind, ModuleAbi, ModuleArtifact, ModuleCodeArtifacts, ModuleEnumVariant, ModuleExport,
    ModuleField, ModuleTypeLayout, hash_source_text,
};
use clang::{Clang, Entity, EntityKind, Index, Type, TypeKind, Unsaved};

use crate::config::{CStandard, ResolvedConfig};

#[derive(Debug)]
pub enum ExtractError {
    Clang(String),
    Parse(String),
    Diagnostic(String),
    Unsupported(String),
    Io(String),
}

impl fmt::Display for ExtractError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Clang(message) => write!(formatter, "libclang: {message}"),
            Self::Parse(message) => write!(formatter, "failed to parse foreign headers: {message}"),
            Self::Diagnostic(message) => write!(formatter, "{message}"),
            Self::Unsupported(message) => {
                write!(formatter, "unsupported foreign declaration: {message}")
            }
            Self::Io(message) => write!(formatter, "foreign module I/O error: {message}"),
        }
    }
}

impl std::error::Error for ExtractError {}

pub fn build_artifact(
    config: &ResolvedConfig,
    base_dir: &Path,
    target: Option<&str>,
) -> Result<ModuleArtifact, ExtractError> {
    let synthetic_path = base_dir.join(format!(".agsm-{}.c", std::process::id()));
    let includes = config
        .includes
        .iter()
        .map(|header| {
            let path = config.resolve_path(base_dir, header);
            format!("#include \"{}\"\n", include_literal(&path))
        })
        .collect::<String>();
    let unsaved = Unsaved::new(&synthetic_path, &includes);

    let clang = Clang::new().map_err(ExtractError::Clang)?;
    let index = Index::new(&clang, false, false);
    let mut parser = index.parser(&synthetic_path);
    let mut arguments = vec![
        "-x".to_string(),
        "c".to_string(),
        format!("-std={}", standard_name(config.standard)),
        "-I".to_string(),
        base_dir.display().to_string(),
    ];
    arguments.extend(
        config
            .include_paths
            .iter()
            .map(|path| format!("-I{}", config.resolve_path(base_dir, path).display())),
    );
    arguments.extend(config.defines.iter().map(|define| format!("-D{define}")));
    if let Some(target) = target {
        arguments.push(format!("--target={target}"));
    }
    parser
        .arguments(&arguments)
        .unsaved(&[unsaved])
        .detailed_preprocessing_record(true)
        .skip_function_bodies(true);
    let translation_unit = parser
        .parse()
        .map_err(|error| ExtractError::Parse(error.to_string()))?;

    let errors = translation_unit
        .get_diagnostics()
        .into_iter()
        .filter(|diagnostic| diagnostic.get_severity() >= clang::diagnostic::Severity::Error)
        .map(|diagnostic| {
            diagnostic
                .formatter()
                .source_location(true)
                .column(true)
                .format()
        })
        .collect::<Vec<_>>();
    if !errors.is_empty() {
        return Err(ExtractError::Diagnostic(errors.join("\n")));
    }

    let mut exports = Vec::new();
    let mut seen = BTreeSet::new();
    for entity in translation_unit.get_entity().get_children() {
        if !is_external_declaration(&entity) || entity.is_in_system_header() {
            continue;
        }
        let Some(name) = entity.get_name() else {
            continue;
        };
        let Some(export) = export_entity(&entity, &name)? else {
            continue;
        };
        let key = format!("{:?}:{}:{}", export.kind, export.name, export.signature);
        if seen.insert(key) {
            exports.push(export);
        }
    }

    let source_hash = hash_inputs(config, base_dir);
    Ok(ModuleArtifact {
        module_name: config.name.clone(),
        module_path: config.name.clone(),
        source_path: String::new(),
        source_hash_fnv1a64: source_hash,
        compiler_version: "foreign".to_string(),
        target_triple: target.unwrap_or("unknown").to_string(),
        code_artifacts: ModuleCodeArtifacts::default(),
        module_deps: Vec::new(),
        transitive_deps: Vec::new(),
        exports,
        native_libs: config.libs.clone(),
        native_lib_paths: config
            .lib_paths
            .iter()
            .map(|path| config.resolve_path(base_dir, path).display().to_string())
            .collect(),
        artifact_path: None,
    })
}

fn export_entity(entity: &Entity<'_>, name: &str) -> Result<Option<ModuleExport>, ExtractError> {
    match entity.get_kind() {
        EntityKind::FunctionDecl => export_function(entity, name).map(Some),
        EntityKind::StructDecl => export_record(entity, name, false).map(Some),
        EntityKind::UnionDecl => Err(ExtractError::Unsupported(format!(
            "C union `{name}` is not representable in Silver metadata yet"
        ))),
        EntityKind::EnumDecl => export_enum(entity, name).map(Some),
        EntityKind::TypedefDecl => Ok(None),
        EntityKind::VarDecl | EntityKind::MacroDefinition | EntityKind::InclusionDirective => {
            Ok(None)
        }
        _ => Ok(None),
    }
}

fn export_function(entity: &Entity<'_>, name: &str) -> Result<ModuleExport, ExtractError> {
    let function_type = entity
        .get_type()
        .ok_or_else(|| ExtractError::Unsupported(format!("function `{name}` has no type")))?;
    let parameters = entity
        .get_arguments()
        .unwrap_or_default()
        .into_iter()
        .map(|parameter| {
            parameter
                .get_type()
                .ok_or_else(|| {
                    ExtractError::Unsupported(format!("parameter in `{name}` has no type"))
                })
                .and_then(canonical_type)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let return_type = function_type
        .get_result_type()
        .ok_or_else(|| ExtractError::Unsupported(format!("function `{name}` has no result type")))
        .and_then(canonical_type)?;
    Ok(ModuleExport {
        kind: ExportKind::Function,
        name: name.to_string(),
        signature: format!("fn({}) -> {return_type}", parameters.join(",")),
        type_params: Vec::new(),
        link_name: Some(name.to_string()),
        abi: Some(ModuleAbi::C),
        is_variadic: entity.is_variadic(),
        type_key: None,
        fields: Vec::new(),
        layout: None,
        enum_backing_type: None,
        enum_variants: Vec::new(),
        trait_items: Vec::new(),
    })
}

fn export_record(
    entity: &Entity<'_>,
    name: &str,
    is_union: bool,
) -> Result<ModuleExport, ExtractError> {
    let record_type = entity
        .get_type()
        .ok_or_else(|| ExtractError::Unsupported(format!("record `{name}` has no type")))?;
    let fields = record_type
        .get_fields()
        .ok_or_else(|| ExtractError::Unsupported(format!("record `{name}` has no fields")))?
        .into_iter()
        .map(|field| {
            let field_name = field.get_name().unwrap_or_else(|| "_anonymous".to_string());
            let field_type = field
                .get_type()
                .ok_or_else(|| {
                    ExtractError::Unsupported(format!("field `{field_name}` has no type"))
                })
                .and_then(canonical_type)?;
            Ok(ModuleField {
                name: field_name,
                type_key: field_type,
                tags: Default::default(),
            })
        })
        .collect::<Result<Vec<_>, ExtractError>>()?;
    let layout =
        ModuleTypeLayout {
            size: Some(record_type.get_sizeof().map_err(|error| {
                ExtractError::Unsupported(format!("record `{name}` size: {error}"))
            })? as u64),
            align: Some(record_type.get_alignof().map_err(|error| {
                ExtractError::Unsupported(format!("record `{name}` alignment: {error}"))
            })? as u64),
        };
    let mut tags = String::new();
    if is_union {
        tags.push_str("union");
    }
    Ok(ModuleExport {
        kind: ExportKind::Struct,
        name: name.to_string(),
        signature: format!(
            "struct{{{}}}",
            fields
                .iter()
                .map(|field| format!("{}:{}", field.name, field.type_key))
                .collect::<Vec<_>>()
                .join(",")
        ),
        type_params: Vec::new(),
        link_name: None,
        abi: None,
        is_variadic: false,
        type_key: Some(name.to_string()),
        fields,
        layout: Some(layout),
        enum_backing_type: None,
        enum_variants: Vec::new(),
        trait_items: if tags.is_empty() {
            Vec::new()
        } else {
            vec![agc::module_artifact::ModuleTraitItem {
                name: "__foreign_record_kind".to_string(),
                signature: tags,
            }]
        },
    })
}

fn export_enum(entity: &Entity<'_>, name: &str) -> Result<ModuleExport, ExtractError> {
    let backing_type = entity
        .get_enum_underlying_type()
        .ok_or_else(|| ExtractError::Unsupported(format!("enum `{name}` has no underlying type")))
        .and_then(canonical_type)?;
    let variants = entity
        .get_children()
        .into_iter()
        .filter(|child| child.get_kind() == EntityKind::EnumConstantDecl)
        .filter_map(|variant| {
            let variant_name = variant.get_name()?;
            let (signed, _) = variant.get_enum_constant_value()?;
            Some(ModuleEnumVariant {
                name: variant_name,
                value: signed as i128,
                payload_types: Vec::new(),
                payload_fields: Vec::new(),
            })
        })
        .collect::<Vec<_>>();
    Ok(ModuleExport {
        kind: ExportKind::Enum,
        name: name.to_string(),
        signature: format!("enum[{} variants]", variants.len()),
        type_params: Vec::new(),
        link_name: None,
        abi: None,
        is_variadic: false,
        type_key: Some(name.to_string()),
        fields: Vec::new(),
        layout: None,
        enum_backing_type: Some(backing_type),
        enum_variants: variants,
        trait_items: Vec::new(),
    })
}

fn canonical_type(type_: Type<'_>) -> Result<String, ExtractError> {
    let type_ = type_.get_canonical_type();
    let key = match type_.get_kind() {
        TypeKind::Void => "void".to_string(),
        TypeKind::Bool => "bool".to_string(),
        TypeKind::CharS | TypeKind::SChar => "i8".to_string(),
        TypeKind::CharU | TypeKind::UChar => "u8".to_string(),
        TypeKind::Short => "i16".to_string(),
        TypeKind::UShort => "u16".to_string(),
        TypeKind::Int => "i32".to_string(),
        TypeKind::UInt => "u32".to_string(),
        TypeKind::Long => integer_key(type_, true, 4),
        TypeKind::ULong => integer_key(type_, false, 4),
        TypeKind::LongLong => "i64".to_string(),
        TypeKind::ULongLong => "u64".to_string(),
        TypeKind::Int128 => "i128".to_string(),
        TypeKind::UInt128 => "u128".to_string(),
        TypeKind::Float => "f32".to_string(),
        TypeKind::Double => "f64".to_string(),
        TypeKind::LongDouble => "f80".to_string(),
        TypeKind::Pointer => {
            let pointee = type_
                .get_pointee_type()
                .ok_or_else(|| ExtractError::Unsupported("pointer without pointee".to_string()))?;
            let pointee_key = canonical_type(pointee)?;
            if type_.is_const_qualified() {
                format!("*const {pointee_key}")
            } else {
                format!("*mut {pointee_key}")
            }
        }
        TypeKind::Record | TypeKind::Enum => type_
            .get_declaration()
            .and_then(|declaration| declaration.get_name())
            .ok_or_else(|| {
                ExtractError::Unsupported(format!("unnamed type `{}`", type_.get_display_name()))
            })?,
        TypeKind::ConstantArray => {
            let element = type_
                .get_element_type()
                .ok_or_else(|| ExtractError::Unsupported("array without element".to_string()))?;
            let size = type_
                .get_size()
                .ok_or_else(|| ExtractError::Unsupported("array without size".to_string()))?;
            format!("Array<{}, {}>", canonical_type(element)?, size)
        }
        TypeKind::FunctionPrototype => {
            let args = type_
                .get_argument_types()
                .ok_or_else(|| {
                    ExtractError::Unsupported("function type without arguments".to_string())
                })?
                .into_iter()
                .map(canonical_type)
                .collect::<Result<Vec<_>, _>>()?;
            let result = type_.get_result_type().ok_or_else(|| {
                ExtractError::Unsupported("function type without result".to_string())
            })?;
            format!("fn({}) -> {}", args.join(","), canonical_type(result)?)
        }
        TypeKind::Typedef => canonical_type(
            type_
                .get_declaration()
                .and_then(|declaration| declaration.get_typedef_underlying_type())
                .ok_or_else(|| {
                    ExtractError::Unsupported(format!("typedef `{}`", type_.get_display_name()))
                })?,
        )?,
        kind => {
            return Err(ExtractError::Unsupported(format!(
                "type `{}` ({kind:?})",
                type_.get_display_name()
            )));
        }
    };
    Ok(key)
}

fn integer_key(type_: Type<'_>, signed: bool, fallback_bytes: usize) -> String {
    let bytes = type_.get_sizeof().unwrap_or(fallback_bytes);
    match (signed, bytes) {
        (true, 1) => "i8".to_string(),
        (true, 2) => "i16".to_string(),
        (true, 4) => "i32".to_string(),
        (true, 8) => "i64".to_string(),
        (false, 1) => "u8".to_string(),
        (false, 2) => "u16".to_string(),
        (false, 4) => "u32".to_string(),
        (false, 8) => "u64".to_string(),
        (true, _) => "i128".to_string(),
        (false, _) => "u128".to_string(),
    }
}

fn is_external_declaration(entity: &Entity<'_>) -> bool {
    if matches!(
        entity.get_kind(),
        EntityKind::StructDecl | EntityKind::UnionDecl | EntityKind::EnumDecl
    ) {
        return true;
    }
    entity.get_linkage().is_some_and(|linkage| {
        matches!(
            linkage,
            clang::Linkage::External | clang::Linkage::UniqueExternal
        )
    })
}
fn standard_name(standard: CStandard) -> &'static str {
    match standard {
        CStandard::C99 => "c99",
        CStandard::C11 => "c11",
    }
}

fn include_literal(path: &Path) -> String {
    path.display()
        .to_string()
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
}

fn hash_inputs(config: &ResolvedConfig, base_dir: &Path) -> u64 {
    let mut input = format!("{}\n{:?}\n", config.name, config.standard);
    for path in &config.includes {
        let path = config.resolve_path(base_dir, path);
        input.push_str(&path.display().to_string());
        input.push('\n');
        if let Ok(contents) = std::fs::read_to_string(&path) {
            input.push_str(&contents);
        }
    }
    hash_source_text(&input)
}
