use std::collections::BTreeSet;
use std::fmt;
use std::path::{Path, PathBuf};

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

static CLANG_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());

pub fn build_artifact(
    config: &ResolvedConfig,
    base_dir: &Path,
    target: Option<&str>,
) -> Result<ModuleArtifact, ExtractError> {
    let _lock = CLANG_MUTEX.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    let synthetic_path = base_dir.join(format!(".agsm-{}.c", std::process::id()));
    let includes = config
        .includes
        .iter()
        .map(|header| {
            let path = resolve_header(config, base_dir, header);
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
    if let Ok(nix_cflags) = std::env::var("NIX_CFLAGS_COMPILE") {
        let mut iter = nix_cflags.split_whitespace().peekable();
        while let Some(flag) = iter.next() {
            if flag == "-isystem" || flag == "-I" {
                if let Some(path) = iter.next() {
                    arguments.push("-I".to_string());
                    arguments.push(path.to_string());
                }
            } else if let Some(path) = flag.strip_prefix("-I") {
                arguments.push(format!("-I{path}"));
            }
        }
    }
    for env_var in ["C_INCLUDE_PATH", "CPATH"] {
        if let Ok(paths) = std::env::var(env_var) {
            for path in std::env::split_paths(&paths) {
                arguments.push("-I".to_string());
                arguments.push(path.display().to_string());
            }
        }
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
    let mut stubs = Vec::new();
    let mut seen = BTreeSet::new();
    for entity in translation_unit.get_entity().get_children() {
        if !is_external_declaration(&entity) || !is_target_entity(&entity, config) {
            continue;
        }
        let Some(name) = entity.get_name() else {
            continue;
        };
        let mut extra_exports = Vec::new();
        let Some(export) = export_entity(
            &entity,
            &name,
            config.export_types,
            config.export_opaque_types || config.opaque_types.iter().any(|ty| ty == &name),
            &mut extra_exports,
            &mut stubs,
        )?
        else {
            continue;
        };
        for extra in extra_exports {
            if let Some(filtered) = apply_filtering_and_strip(extra, config) {
                let extra_key = format!("{:?}:{}:{}", filtered.kind, filtered.name, filtered.signature);
                if seen.insert(extra_key) {
                    exports.push(filtered);
                }
            }
        }
        if let Some(filtered) = apply_filtering_and_strip(export, config) {
            let key = format!("{:?}:{}:{}", filtered.kind, filtered.name, filtered.signature);
            if seen.insert(key) {
                exports.push(filtered);
            }
        }
    }
    for name in &config.opaque_types {
        if is_name_allowed(name, config) {
            let stripped = apply_prefix_strip(name, config);
            if seen.insert(format!("{:?}:{}:struct{{}}", ExportKind::Struct, stripped)) {
                exports.push(export_opaque_record(&stripped));
            }
        }
    }

    let mut has_static_library = false;
    if !stubs.is_empty() || !config.defines.is_empty() {
        let stub_c_path = base_dir.join(format!(".agsm-{}-stubs.c", std::process::id()));
        let mut c_source = includes.clone();
        for stub in &stubs {
            let param_list = stub
                .params
                .iter()
                .map(|(ty, name)| format!("{ty} {name}"))
                .collect::<Vec<_>>()
                .join(", ");
            let arg_list = stub
                .params
                .iter()
                .map(|(_, name)| name.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            let return_prefix = if stub.return_type_c == "void" {
                ""
            } else {
                "return "
            };
            c_source.push_str(&format!(
                "\n{} {}({}) {{\n    {}{}({});\n}}\n",
                stub.return_type_c,
                stub.stub_name,
                if param_list.is_empty() { "void" } else { &param_list },
                return_prefix,
                stub.name,
                arg_list,
            ));
        }
        std::fs::write(&stub_c_path, &c_source)
            .map_err(|e| ExtractError::Io(e.to_string()))?;
        let obj_path = base_dir.join(format!("{}.o", config.name));

        let compiler = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
        let compiler_name = compiler.to_string_lossy().into_owned();
        let mut cmd = std::process::Command::new(&compiler);
        cmd.arg("-c")
            .arg("-fPIC")
            .arg("-O2")
            .arg("-fno-stack-protector")
            .arg("-U_FORTIFY_SOURCE")
            .arg(&stub_c_path)
            .arg("-o")
            .arg(&obj_path)
            .arg("-I")
            .arg(base_dir);
        for inc in &config.include_paths {
            cmd.arg(format!("-I{}", config.resolve_path(base_dir, inc).display()));
        }
        for def in &config.defines {
            cmd.arg(format!("-D{def}"));
        }
        if let Some(target) = target {
            cmd.arg(format!("--target={target}"));
        }

        let output = cmd.output().map_err(|err| {
            ExtractError::Unsupported(format!("failed to run {compiler_name} for stubs: {err}"))
        })?;
        let _ = std::fs::remove_file(&stub_c_path);
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ExtractError::Unsupported(format!("stub compilation failed: {stderr}")));
        }
        has_static_library = true;
    }

    let source_hash = hash_inputs(config, base_dir);
    Ok(ModuleArtifact {
        module_name: config.name.clone(),
        module_path: config.name.clone(),
        source_path: String::new(),
        source_hash_fnv1a64: source_hash,
        compiler_version: "foreign".to_string(),
        target_triple: target.unwrap_or("unknown").to_string(),
        code_artifacts: ModuleCodeArtifacts {
            has_static_library,
            has_shared_library: false,
        },
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
        generic_templates: Vec::new(),
    })
}

fn export_entity(
    entity: &Entity<'_>,
    name: &str,
    export_types: bool,
    export_opaque_types: bool,
    extra_exports: &mut Vec<ModuleExport>,
    stubs: &mut Vec<CStub>,
) -> Result<Option<ModuleExport>, ExtractError> {
    match entity.get_kind() {
        EntityKind::FunctionDecl => export_function(entity, name, stubs).map(Some),
        EntityKind::StructDecl => {
            let Some(record_type) = entity.get_type() else {
                return Ok(None);
            };
            if record_type.get_sizeof().is_err() {
                if export_opaque_types {
                    Ok(Some(export_opaque_record(name)))
                } else {
                    Ok(None)
                }
            } else if export_types {
                export_record(entity, name, false, extra_exports).map(Some)
            } else {
                Ok(None)
            }
        }
        EntityKind::UnionDecl => {
            let Some(record_type) = entity.get_type() else {
                return Ok(None);
            };
            if record_type.get_sizeof().is_err() {
                if export_opaque_types {
                    Ok(Some(export_opaque_record(name)))
                } else {
                    Ok(None)
                }
            } else if export_types {
                export_record(entity, name, true, extra_exports).map(Some)
            } else {
                Ok(None)
            }
        }
        EntityKind::EnumDecl => {
            if export_types {
                export_enum(entity, name).map(Some)
            } else {
                Ok(None)
            }
        }
        EntityKind::TypedefDecl => {
            if export_types {
                export_typedef(entity, name, export_opaque_types, extra_exports)
            } else if export_opaque_types {
                Ok(Some(export_opaque_record(name)))
            } else {
                Ok(None)
            }
        }
        EntityKind::VarDecl => export_var(entity, name),
        EntityKind::MacroDefinition => export_macro(entity, name),
        EntityKind::InclusionDirective => Ok(None),
        _ => Ok(None),
    }
}

fn export_typedef(
    entity: &Entity<'_>,
    name: &str,
    export_opaque_types: bool,
    extra_exports: &mut Vec<ModuleExport>,
) -> Result<Option<ModuleExport>, ExtractError> {
    let Some(underlying_type) = entity.get_typedef_underlying_type() else {
        if export_opaque_types {
            return Ok(Some(export_opaque_record(name)));
        }
        return Ok(None);
    };

    let decl_opt = underlying_type
        .get_declaration()
        .or_else(|| underlying_type.get_canonical_type().get_declaration());

    if let Some(decl) = decl_opt {
        let is_unnamed_decl = decl.get_name().as_deref().unwrap_or("").is_empty()
            || decl.get_name().as_deref().unwrap_or("").starts_with('(')
            || decl.is_anonymous()
            || decl.get_name().as_deref() == Some(name);

        if is_unnamed_decl {
            match decl.get_kind() {
                EntityKind::StructDecl => {
                    if decl.get_type().and_then(|t| t.get_sizeof().ok()).is_some() {
                        return export_record(&decl, name, false, extra_exports).map(Some);
                    }
                }
                EntityKind::UnionDecl => {
                    if decl.get_type().and_then(|t| t.get_sizeof().ok()).is_some() {
                        return export_record(&decl, name, true, extra_exports).map(Some);
                    }
                }
                EntityKind::EnumDecl => {
                    return export_enum(&decl, name).map(Some);
                }
                _ => {}
            }
        }
    }

    match canonical_type(underlying_type) {
        Ok(target_type) => {
            if target_type == name {
                return Ok(None);
            }
            Ok(Some(ModuleExport {
                kind: ExportKind::TypeAlias,
                name: name.to_string(),
                signature: format!("type {target_type}"),
                type_params: Vec::new(),
                link_name: None,
                abi: None,
                is_variadic: false,
                type_key: Some(target_type),
                fields: Vec::new(),
                layout: None,
                enum_backing_type: None,
                enum_variants: Vec::new(),
                trait_items: Vec::new(),
                const_value: None,
                is_mutable: false,
            }))
        }
        Err(_) if export_opaque_types => Ok(Some(export_opaque_record(name))),
        Err(_) => Ok(None),
    }
}

fn export_var(entity: &Entity<'_>, name: &str) -> Result<Option<ModuleExport>, ExtractError> {
    let Some(var_type) = entity.get_type() else {
        return Ok(None);
    };
    let type_key = canonical_type(var_type)?;
    let is_const = var_type.is_const_qualified();
    let kind = if is_const {
        ExportKind::Constant
    } else {
        ExportKind::Global
    };
    Ok(Some(ModuleExport {
        kind,
        name: name.to_string(),
        signature: type_key.clone(),
        type_params: Vec::new(),
        link_name: Some(name.to_string()),
        abi: Some(ModuleAbi::C),
        is_variadic: false,
        type_key: Some(type_key),
        fields: Vec::new(),
        layout: None,
        enum_backing_type: None,
        enum_variants: Vec::new(),
        trait_items: Vec::new(),
        const_value: None,
        is_mutable: !is_const,
    }))
}

fn export_macro(entity: &Entity<'_>, name: &str) -> Result<Option<ModuleExport>, ExtractError> {
    if name.starts_with("__") || entity.is_function_like_macro() {
        return Ok(None);
    }
    let Some(range) = entity.get_range() else {
        return Ok(None);
    };
    let tokens = range.tokenize();
    if tokens.len() < 2 {
        return Ok(None);
    }
    let body_tokens = &tokens[1..];
    let Some((inferred_type, const_val)) = parse_macro_constant_tokens(body_tokens) else {
        return Ok(None);
    };

    Ok(Some(ModuleExport {
        kind: ExportKind::Constant,
        name: name.to_string(),
        signature: inferred_type.clone(),
        type_params: Vec::new(),
        link_name: None,
        abi: None,
        is_variadic: false,
        type_key: Some(inferred_type),
        fields: Vec::new(),
        layout: None,
        enum_backing_type: None,
        enum_variants: Vec::new(),
        trait_items: Vec::new(),
        const_value: Some(const_val),
        is_mutable: false,
    }))
}

struct CStub {
    name: String,
    stub_name: String,
    return_type_c: String,
    params: Vec<(String, String)>,
}

fn export_function(
    entity: &Entity<'_>,
    name: &str,
    stubs: &mut Vec<CStub>,
) -> Result<ModuleExport, ExtractError> {
    let function_type = entity
        .get_type()
        .ok_or_else(|| ExtractError::Unsupported(format!("function `{name}` has no type")))?;
    let arguments = entity.get_arguments().unwrap_or_default();
    let parameters = arguments
        .iter()
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

    let is_inline_or_static = entity.is_inline_function()
        || entity
            .get_linkage()
            .is_some_and(|l| matches!(l, clang::Linkage::Internal | clang::Linkage::Automatic))
        || (entity.is_definition() && entity.get_linkage() != Some(clang::Linkage::External));

    let link_name = if is_inline_or_static {
        let stub_name = format!("__silver_stub_{name}");
        let ret_c = function_type
            .get_result_type()
            .map(|t| t.get_display_name())
            .unwrap_or_else(|| "void".to_string());
        let mut param_tuples = Vec::new();
        for (i, arg) in arguments.iter().enumerate() {
            let p_type = arg
                .get_type()
                .map(|t| t.get_display_name())
                .unwrap_or_else(|| "void*".to_string());
            let p_name = format!("__arg_{i}");
            param_tuples.push((p_type, p_name));
        }
        stubs.push(CStub {
            name: name.to_string(),
            stub_name: stub_name.clone(),
            return_type_c: ret_c,
            params: param_tuples,
        });
        stub_name
    } else {
        name.to_string()
    };

    Ok(ModuleExport {
        kind: ExportKind::Function,
        name: name.to_string(),
        signature: format!("fn({}) -> {return_type}", parameters.join(",")),
        type_params: Vec::new(),
        link_name: Some(link_name),
        abi: Some(ModuleAbi::C),
        is_variadic: entity.is_variadic(),
        type_key: None,
        fields: Vec::new(),
        layout: None,
        enum_backing_type: None,
        enum_variants: Vec::new(),
        trait_items: Vec::new(),
        const_value: None,
        is_mutable: false,
    })
}

fn export_opaque_record(name: &str) -> ModuleExport {
    ModuleExport {
        kind: ExportKind::Struct,
        name: name.to_string(),
        signature: "struct{}".to_string(),
        type_params: Vec::new(),
        link_name: None,
        abi: None,
        is_variadic: false,
        type_key: Some(name.to_string()),
        fields: Vec::new(),
        layout: None,
        enum_backing_type: None,
        enum_variants: Vec::new(),
        trait_items: Vec::new(),
        const_value: None,
        is_mutable: false,
    }
}

fn collect_record_fields(
    entity: &Entity<'_>,
    parent_name: &str,
    extra_exports: &mut Vec<ModuleExport>,
) -> Result<Vec<ModuleField>, ExtractError> {
    let mut fields = Vec::new();

    for child in entity.get_children() {
        match child.get_kind() {
            EntityKind::FieldDecl => {
                let field_name_opt = child.get_name();
                let Some(field_type) = child.get_type() else {
                    continue;
                };

                let decl_opt = field_type
                    .get_declaration()
                    .or_else(|| field_type.get_canonical_type().get_declaration());

                let is_anon = field_name_opt.as_deref().unwrap_or("").is_empty()
                    || field_name_opt.as_deref().unwrap_or("").starts_with('(')
                    || child.is_anonymous();

                if is_anon {
                    if let Some(decl) = decl_opt {
                        if matches!(decl.get_kind(), EntityKind::StructDecl | EntityKind::UnionDecl) {
                            let sub_fields = collect_record_fields(&decl, parent_name, extra_exports)?;
                            fields.extend(sub_fields);
                            continue;
                        }
                    }
                }

                let field_name = field_name_opt.unwrap_or_else(|| format!("_anon_{}", fields.len()));

                let is_unnamed_decl = decl_opt.as_ref().is_some_and(|d| {
                    d.get_name().as_deref().unwrap_or("").is_empty()
                        || d.get_name().as_deref().unwrap_or("").starts_with('(')
                        || d.is_anonymous()
                });

                let type_key = if let Some(decl) = decl_opt
                    && is_unnamed_decl
                    && matches!(decl.get_kind(), EntityKind::StructDecl | EntityKind::UnionDecl)
                {
                    let anon_record_name = format!("{parent_name}_{field_name}");
                    let is_union = decl.get_kind() == EntityKind::UnionDecl;
                    let sub_export = export_record(&decl, &anon_record_name, is_union, extra_exports)?;
                    extra_exports.push(sub_export);
                    anon_record_name
                } else {
                    canonical_type(field_type)?
                };

                fields.push(ModuleField {
                    name: field_name,
                    type_key,
                    tags: Default::default(),
                });
            }
            EntityKind::UnionDecl | EntityKind::StructDecl => {
                let name_opt = child.get_name();
                let is_anon = name_opt.as_deref().unwrap_or("").is_empty()
                    || name_opt.as_deref().unwrap_or("").starts_with('(')
                    || child.is_anonymous();
                if is_anon {
                    let sub_fields = collect_record_fields(&child, parent_name, extra_exports)?;
                    fields.extend(sub_fields);
                }
            }
            _ => {}
        }
    }

    if fields.is_empty() {
        if let Some(record_type) = entity.get_type()
            && let Some(entity_fields) = record_type.get_fields()
        {
            for field in entity_fields {
                let field_name_opt = field.get_name();
                let Some(field_type) = field.get_type() else {
                    continue;
                };
                let field_name =
                    field_name_opt.unwrap_or_else(|| format!("_anon_{}", fields.len()));
                let type_key = canonical_type(field_type)?;
                fields.push(ModuleField {
                    name: field_name,
                    type_key,
                    tags: Default::default(),
                });
            }
        }
    }

    Ok(fields)
}

fn export_record(
    entity: &Entity<'_>,
    name: &str,
    is_union: bool,
    extra_exports: &mut Vec<ModuleExport>,
) -> Result<ModuleExport, ExtractError> {
    let record_type = entity
        .get_type()
        .ok_or_else(|| ExtractError::Unsupported(format!("record `{name}` has no type")))?;
    let fields = collect_record_fields(entity, name, extra_exports)?;
    let layout = ModuleTypeLayout {
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
        const_value: None,
        is_mutable: false,
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
        const_value: None,
        is_mutable: false,
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
            let is_const = pointee.is_const_qualified();
            let pointee_key = canonical_type(pointee)?;
            if is_const {
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
        TypeKind::IncompleteArray => {
            let element = type_
                .get_element_type()
                .ok_or_else(|| ExtractError::Unsupported("incomplete array without element".to_string()))?;
            let is_const = element.is_const_qualified();
            let element_key = canonical_type(element)?;
            if is_const {
                format!("*const {element_key}")
            } else {
                format!("*mut {element_key}")
            }
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
        EntityKind::StructDecl
            | EntityKind::UnionDecl
            | EntityKind::EnumDecl
            | EntityKind::TypedefDecl
            | EntityKind::MacroDefinition
    ) {
        return true;
    }
    if entity.get_kind() == EntityKind::FunctionDecl
        && (entity.is_inline_function() || entity.is_definition())
    {
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

pub fn matches_glob(pattern: &str, text: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    if !pattern.contains('*') && !pattern.contains('?') {
        return pattern == text;
    }
    let p_chars: Vec<char> = pattern.chars().collect();
    let t_chars: Vec<char> = text.chars().collect();
    let mut p = 0;
    let mut t = 0;
    let mut star_p = None;
    let mut star_t = 0;

    while t < t_chars.len() {
        if p < p_chars.len() && (p_chars[p] == '?' || p_chars[p] == t_chars[t]) {
            p += 1;
            t += 1;
        } else if p < p_chars.len() && p_chars[p] == '*' {
            star_p = Some(p);
            p += 1;
            star_t = t;
        } else if let Some(sp) = star_p {
            p = sp + 1;
            star_t += 1;
            t = star_t;
        } else {
            return false;
        }
    }
    while p < p_chars.len() && p_chars[p] == '*' {
        p += 1;
    }
    p == p_chars.len()
}

fn apply_filtering_and_strip(
    mut export: ModuleExport,
    config: &ResolvedConfig,
) -> Option<ModuleExport> {
    let original_name = export.name.clone();
    if !is_name_allowed(&original_name, config) {
        return None;
    }
    let stripped = apply_prefix_strip(&original_name, config);
    if stripped != original_name {
        if export.link_name.is_none()
            && matches!(export.kind, ExportKind::Function | ExportKind::Global)
        {
            export.link_name = Some(original_name);
        }
        export.name = stripped;
    }
    Some(export)
}

fn is_name_allowed(name: &str, config: &ResolvedConfig) -> bool {
    if config.deny.iter().any(|pattern| matches_glob(pattern, name)) {
        return false;
    }
    if !config.allow.is_empty()
        && !config
            .allow
            .iter()
            .any(|pattern| matches_glob(pattern, name))
    {
        return false;
    }
    true
}

fn apply_prefix_strip(name: &str, config: &ResolvedConfig) -> String {
    for prefix in &config.prefix_strip {
        if let Some(stripped) = name.strip_prefix(prefix) {
            if !stripped.is_empty() {
                return stripped.to_string();
            }
        }
    }
    name.to_string()
}

fn resolve_header(config: &ResolvedConfig, base_dir: &Path, header: &Path) -> PathBuf {
    let candidate = config.resolve_path(base_dir, header);
    if candidate.is_file() {
        return candidate;
    }
    for include_path in &config.include_paths {
        let candidate = config.resolve_path(base_dir, include_path).join(header);
        if candidate.is_file() {
            return candidate;
        }
    }
    PathBuf::from(header)
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
        let path = resolve_header(config, base_dir, path);
        input.push_str(&path.display().to_string());
        input.push('\n');
        if let Ok(contents) = std::fs::read_to_string(&path) {
            input.push_str(&contents);
        }
    }
    hash_source_text(&input)
}

fn is_target_entity(entity: &Entity<'_>, config: &ResolvedConfig) -> bool {
    let Some(location) = entity.get_location() else {
        return false;
    };
    let Some(file) = location.get_file_location().file else {
        return false;
    };
    let path = file.get_path();
    let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

    for inc in &config.includes {
        let inc_name = inc.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if !inc_name.is_empty() && (file_name == inc_name || path.ends_with(inc)) {
            return true;
        }
    }

    if !entity.is_in_system_header() {
        return true;
    }

    false
}

fn parse_macro_constant_tokens(tokens: &[clang::token::Token<'_>]) -> Option<(String, String)> {
    if tokens.is_empty() {
        return None;
    }

    if tokens.len() == 1 {
        let spelling = tokens[0].get_spelling();
        return parse_single_constant_literal(&spelling);
    }

    let text = tokens
        .iter()
        .map(|t| t.get_spelling())
        .collect::<Vec<_>>()
        .join(" ");

    let trimmed = text.trim();
    if trimmed.starts_with('(') && trimmed.ends_with(')') {
        let inner = trimmed[1..trimmed.len() - 1].trim();
        if let Some(res) = parse_single_constant_literal(inner) {
            return Some(res);
        }
    }

    if let Some(val) = eval_simple_c_int_expr(trimmed) {
        let ty = if val > (i32::MAX as i128) || val < (i32::MIN as i128) {
            "i64"
        } else {
            "i32"
        };
        return Some((ty.to_string(), val.to_string()));
    }

    None
}

fn parse_single_constant_literal(spelling: &str) -> Option<(String, String)> {
    let s = spelling.trim();
    if s.is_empty() {
        return None;
    }

    if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
        return Some(("str".to_string(), s.to_string()));
    }

    if s.starts_with('\'') && s.ends_with('\'') && s.len() >= 2 {
        return Some(("char".to_string(), s.to_string()));
    }

    if s == "true" || s == "false" {
        return Some(("bool".to_string(), s.to_string()));
    }

    let is_float = (s.contains('.') || s.contains('e') || s.contains('E'))
        && !s.starts_with("0x")
        && !s.starts_with("0X");
    if is_float || (s.ends_with('f') || s.ends_with('F')) {
        let is_f32 = s.ends_with('f') || s.ends_with('F');
        let clean = s.trim_end_matches(|c| c == 'f' || c == 'F');
        if clean.parse::<f64>().is_ok() {
            return Some((
                if is_f32 {
                    "f32".to_string()
                } else {
                    "f64".to_string()
                },
                clean.to_string(),
            ));
        }
    }

    let (digits, is_unsigned, is_long, is_long_long) = parse_int_suffixes(s);
    if let Some(val) = parse_int_digits(&digits) {
        let ty = if is_long_long || is_long {
            if is_unsigned { "u64" } else { "i64" }
        } else if is_unsigned {
            "u32"
        } else if val > (i32::MAX as i128) || val < (i32::MIN as i128) {
            "i64"
        } else {
            "i32"
        };
        return Some((ty.to_string(), val.to_string()));
    }

    None
}

fn parse_int_suffixes(s: &str) -> (String, bool, bool, bool) {
    let mut lower = s.to_lowercase();
    let mut is_unsigned = false;
    let mut is_long_long = false;
    let mut is_long = false;

    while lower.ends_with('u') || lower.ends_with('l') {
        if lower.ends_with("ull") || lower.ends_with("llu") {
            is_unsigned = true;
            is_long_long = true;
            lower.truncate(lower.len() - 3);
        } else if lower.ends_with("ll") {
            is_long_long = true;
            lower.truncate(lower.len() - 2);
        } else if lower.ends_with("ul") || lower.ends_with("lu") {
            is_unsigned = true;
            is_long = true;
            lower.truncate(lower.len() - 2);
        } else if lower.ends_with('u') {
            is_unsigned = true;
            lower.pop();
        } else if lower.ends_with('l') {
            is_long = true;
            lower.pop();
        }
    }
    (lower, is_unsigned, is_long, is_long_long)
}

fn parse_int_digits(s: &str) -> Option<i128> {
    let s = s.trim();
    if s.starts_with("0x") || s.starts_with("0X") {
        i128::from_str_radix(&s[2..], 16).ok()
    } else if s.starts_with("0b") || s.starts_with("0B") {
        i128::from_str_radix(&s[2..], 2).ok()
    } else if s.starts_with('0') && s.len() > 1 && s.chars().all(|c| ('0'..='7').contains(&c)) {
        i128::from_str_radix(&s[1..], 8).ok()
    } else {
        s.parse::<i128>().ok()
    }
}

fn eval_simple_c_int_expr(expr: &str) -> Option<i128> {
    let tokens = tokenize_simple_expr(expr)?;
    let mut pos = 0;
    let val = parse_or_expr(&tokens, &mut pos)?;
    if pos == tokens.len() {
        Some(val)
    } else {
        None
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ExprTok {
    Num(i128),
    Op(String),
    LParen,
    RParen,
}

fn tokenize_simple_expr(s: &str) -> Option<Vec<ExprTok>> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '(' {
            tokens.push(ExprTok::LParen);
            i += 1;
            continue;
        }
        if c == ')' {
            tokens.push(ExprTok::RParen);
            i += 1;
            continue;
        }
        if c == '<' && i + 1 < chars.len() && chars[i + 1] == '<' {
            tokens.push(ExprTok::Op("<<".to_string()));
            i += 2;
            continue;
        }
        if c == '>' && i + 1 < chars.len() && chars[i + 1] == '>' {
            tokens.push(ExprTok::Op(">>".to_string()));
            i += 2;
            continue;
        }
        if matches!(c, '+' | '-' | '*' | '/' | '%' | '|' | '&' | '^' | '~') {
            tokens.push(ExprTok::Op(c.to_string()));
            i += 1;
            continue;
        }
        if c.is_ascii_digit()
            || (c == '0'
                && i + 1 < chars.len()
                && (chars[i + 1] == 'x' || chars[i + 1] == 'X'))
        {
            let start = i;
            while i < chars.len() && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();
            let (digits, _, _, _) = parse_int_suffixes(&word);
            let val = parse_int_digits(&digits)?;
            tokens.push(ExprTok::Num(val));
            continue;
        }
        return None;
    }
    Some(tokens)
}

fn parse_or_expr(tokens: &[ExprTok], pos: &mut usize) -> Option<i128> {
    let mut left = parse_xor_expr(tokens, pos)?;
    while *pos < tokens.len() {
        if let ExprTok::Op(ref op) = tokens[*pos] {
            if op == "|" {
                *pos += 1;
                let right = parse_xor_expr(tokens, pos)?;
                left |= right;
                continue;
            }
        }
        break;
    }
    Some(left)
}

fn parse_xor_expr(tokens: &[ExprTok], pos: &mut usize) -> Option<i128> {
    let mut left = parse_and_expr(tokens, pos)?;
    while *pos < tokens.len() {
        if let ExprTok::Op(ref op) = tokens[*pos] {
            if op == "^" {
                *pos += 1;
                let right = parse_and_expr(tokens, pos)?;
                left ^= right;
                continue;
            }
        }
        break;
    }
    Some(left)
}

fn parse_and_expr(tokens: &[ExprTok], pos: &mut usize) -> Option<i128> {
    let mut left = parse_shift_expr(tokens, pos)?;
    while *pos < tokens.len() {
        if let ExprTok::Op(ref op) = tokens[*pos] {
            if op == "&" {
                *pos += 1;
                let right = parse_shift_expr(tokens, pos)?;
                left &= right;
                continue;
            }
        }
        break;
    }
    Some(left)
}

fn parse_shift_expr(tokens: &[ExprTok], pos: &mut usize) -> Option<i128> {
    let mut left = parse_add_expr(tokens, pos)?;
    while *pos < tokens.len() {
        if let ExprTok::Op(ref op) = tokens[*pos] {
            if op == "<<" {
                *pos += 1;
                let right = parse_add_expr(tokens, pos)?;
                left = left.checked_shl(right as u32)?;
                continue;
            }
            if op == ">>" {
                *pos += 1;
                let right = parse_add_expr(tokens, pos)?;
                left = left.checked_shr(right as u32)?;
                continue;
            }
        }
        break;
    }
    Some(left)
}

fn parse_add_expr(tokens: &[ExprTok], pos: &mut usize) -> Option<i128> {
    let mut left = parse_mul_expr(tokens, pos)?;
    while *pos < tokens.len() {
        if let ExprTok::Op(ref op) = tokens[*pos] {
            if op == "+" {
                *pos += 1;
                let right = parse_mul_expr(tokens, pos)?;
                left = left.checked_add(right)?;
                continue;
            }
            if op == "-" {
                *pos += 1;
                let right = parse_mul_expr(tokens, pos)?;
                left = left.checked_sub(right)?;
                continue;
            }
        }
        break;
    }
    Some(left)
}

fn parse_mul_expr(tokens: &[ExprTok], pos: &mut usize) -> Option<i128> {
    let mut left = parse_unary_expr(tokens, pos)?;
    while *pos < tokens.len() {
        if let ExprTok::Op(ref op) = tokens[*pos] {
            if op == "*" {
                *pos += 1;
                let right = parse_unary_expr(tokens, pos)?;
                left = left.checked_mul(right)?;
                continue;
            }
            if op == "/" {
                *pos += 1;
                let right = parse_unary_expr(tokens, pos)?;
                if right == 0 {
                    return None;
                }
                left = left.checked_div(right)?;
                continue;
            }
            if op == "%" {
                *pos += 1;
                let right = parse_unary_expr(tokens, pos)?;
                if right == 0 {
                    return None;
                }
                left = left.checked_rem(right)?;
                continue;
            }
        }
        break;
    }
    Some(left)
}

fn parse_unary_expr(tokens: &[ExprTok], pos: &mut usize) -> Option<i128> {
    if *pos >= tokens.len() {
        return None;
    }
    if let ExprTok::Op(ref op) = tokens[*pos] {
        if op == "+" {
            *pos += 1;
            return parse_unary_expr(tokens, pos);
        }
        if op == "-" {
            *pos += 1;
            let val = parse_unary_expr(tokens, pos)?;
            return val.checked_neg();
        }
        if op == "~" {
            *pos += 1;
            let val = parse_unary_expr(tokens, pos)?;
            return Some(!val);
        }
    }
    if tokens[*pos] == ExprTok::LParen {
        *pos += 1;
        let val = parse_or_expr(tokens, pos)?;
        if *pos < tokens.len() && tokens[*pos] == ExprTok::RParen {
            *pos += 1;
            return Some(val);
        }
        return None;
    }
    if let ExprTok::Num(n) = tokens[*pos] {
        *pos += 1;
        return Some(n);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_literal_constants() {
        assert_eq!(
            parse_single_constant_literal("42"),
            Some(("i32".to_string(), "42".to_string()))
        );
        assert_eq!(
            parse_single_constant_literal("0xFF"),
            Some(("i32".to_string(), "255".to_string()))
        );
        assert_eq!(
            parse_single_constant_literal("1000000000000LL"),
            Some(("i64".to_string(), "1000000000000".to_string()))
        );
        assert_eq!(
            parse_single_constant_literal("3.14159f"),
            Some(("f32".to_string(), "3.14159".to_string()))
        );
        assert_eq!(
            parse_single_constant_literal("\"hello\""),
            Some(("str".to_string(), "\"hello\"".to_string()))
        );
        assert_eq!(
            parse_single_constant_literal("true"),
            Some(("bool".to_string(), "true".to_string()))
        );
    }

    #[test]
    fn evaluates_macro_expressions() {
        assert_eq!(eval_simple_c_int_expr("1 << 10"), Some(1024));
        assert_eq!(eval_simple_c_int_expr("(1 << 8) | (1 << 4)"), Some(272));
        assert_eq!(eval_simple_c_int_expr("0x10 * 4 + 2"), Some(66));
        assert_eq!(eval_simple_c_int_expr("~0 & 0xFF"), Some(255));
    }

    #[test]
    fn extracts_unions_and_anonymous_records() {
        let temp_dir = std::env::temp_dir().join("agsm_union_test");
        let _ = std::fs::create_dir_all(&temp_dir);
        let header_path = temp_dir.join("test_union.h");
        let header_content = r#"
            typedef union MyUnion {
                int i;
                float f;
                char bytes[4];
            } MyUnion;

            typedef struct MyEvent {
                int type;
                union {
                    struct { int x; int y; };
                    int raw[2];
                };
            } MyEvent;
        "#;
        std::fs::write(&header_path, header_content).unwrap();

        let config = ResolvedConfig {
            name: "test_union".to_string(),
            standard: CStandard::C11,
            target: None,
            includes: vec![header_path.clone()],
            include_paths: vec![],
            defines: vec![],
            libs: vec![],
            lib_paths: vec![],
            prefix_strip: vec![],
            allow: vec![],
            deny: vec![],
            export_types: true,
            export_opaque_types: false,
            opaque_types: vec![],
        };

        let artifact = build_artifact(&config, &temp_dir, None).expect("extraction should succeed");
        let union_export = artifact.exports.iter().find(|e| e.name == "MyUnion");
        assert!(union_export.is_some(), "MyUnion export should be present");
        let u = union_export.unwrap();
        assert_eq!(u.kind, ExportKind::Struct);
        assert_eq!(u.layout.unwrap().size, Some(4));
        assert!(u.trait_items.iter().any(|item| item.name == "__foreign_record_kind" && item.signature == "union"));

        let event_export = artifact.exports.iter().find(|e| e.name == "MyEvent");
        assert!(event_export.is_some(), "MyEvent export should be present");
        let ev = event_export.unwrap();
        assert!(ev.fields.iter().any(|f| f.name == "type"));
        assert!(ev.fields.iter().any(|f| f.name == "x"));
        assert!(ev.fields.iter().any(|f| f.name == "y"));
        assert!(ev.fields.iter().any(|f| f.name == "raw"));

        let _ = std::fs::remove_file(header_path);
        let _ = std::fs::remove_dir(temp_dir);
    }

    #[test]
    fn test_matches_glob() {
        assert!(matches_glob("*", "anything"));
        assert!(matches_glob("rl*", "rlPushMatrix"));
        assert!(!matches_glob("rl*", "DrawText"));
        assert!(matches_glob("Draw?ext", "DrawText"));
        assert!(!matches_glob("Draw?ext", "DrawTexto"));
        assert!(matches_glob("*_Internal", "MyFunc_Internal"));
    }

    #[test]
    fn prefix_strip_and_allow_deny_filtering() {
        let temp_dir = std::env::temp_dir().join("agsm_prefix_filter_test");
        let _ = std::fs::create_dir_all(&temp_dir);
        let header_path = temp_dir.join("test_filter.h");
        let header_content = r#"
            void rlPushMatrix(void);
            void rlPopMatrix(void);
            void rl_InternalInit(void);
            void UnrelatedFunc(void);
            #define RL_MAX_TEXTURES 64
            #define RL_INTERNAL_MAGIC 0xbeef
        "#;
        std::fs::write(&header_path, header_content).unwrap();

        let config = ResolvedConfig {
            name: "test_filter".to_string(),
            standard: CStandard::C11,
            target: None,
            includes: vec![header_path.clone()],
            include_paths: vec![],
            defines: vec![],
            libs: vec![],
            lib_paths: vec![],
            prefix_strip: vec!["RL_".to_string(), "rl".to_string()],
            allow: vec!["rl*".to_string(), "RL_*".to_string()],
            deny: vec!["*Internal*".to_string()],
            export_types: true,
            export_opaque_types: false,
            opaque_types: vec![],
        };

        let artifact = build_artifact(&config, &temp_dir, None).expect("extraction should succeed");

        // "rlPushMatrix" stripped to "PushMatrix" with link_name "rlPushMatrix"
        let push_export = artifact.exports.iter().find(|e| e.name == "PushMatrix");
        assert!(push_export.is_some(), "PushMatrix should be present");
        assert_eq!(push_export.unwrap().link_name.as_deref(), Some("rlPushMatrix"));

        // "rlPopMatrix" stripped to "PopMatrix" with link_name "rlPopMatrix"
        let pop_export = artifact.exports.iter().find(|e| e.name == "PopMatrix");
        assert!(pop_export.is_some(), "PopMatrix should be present");
        assert_eq!(pop_export.unwrap().link_name.as_deref(), Some("rlPopMatrix"));

        // "RL_MAX_TEXTURES" stripped to "MAX_TEXTURES"
        let max_tex = artifact.exports.iter().find(|e| e.name == "MAX_TEXTURES");
        assert!(max_tex.is_some(), "MAX_TEXTURES should be present");

        // Denied symbols containing "Internal" should NOT be present
        assert!(!artifact.exports.iter().any(|e| e.name.contains("Internal") || e.link_name.as_deref().unwrap_or("").contains("Internal")));

        // "UnrelatedFunc" not matching allow pattern "rl*" / "RL_*" should NOT be present
        assert!(!artifact.exports.iter().any(|e| e.name == "UnrelatedFunc"));

        let _ = std::fs::remove_file(header_path);
        let _ = std::fs::remove_dir(temp_dir);
    }

    #[test]
    fn test_extracts_typedef_aliases() {
        let temp_dir = std::env::temp_dir().join("agsm_typedef_test");
        let _ = std::fs::create_dir_all(&temp_dir);
        let header_path = temp_dir.join("test_typedef.h");
        let header_content = r#"
            typedef unsigned int VkFlags;
            typedef void* VkBuffer;
            typedef struct {
                unsigned char r;
                unsigned char g;
                unsigned char b;
                unsigned char a;
            } Color;
        "#;
        std::fs::write(&header_path, header_content).unwrap();

        let config = ResolvedConfig {
            name: "test_typedef".to_string(),
            standard: CStandard::C11,
            target: None,
            includes: vec![header_path.clone()],
            include_paths: vec![],
            defines: vec![],
            libs: vec![],
            lib_paths: vec![],
            prefix_strip: vec![],
            allow: vec![],
            deny: vec![],
            export_types: true,
            export_opaque_types: false,
            opaque_types: vec![],
        };

        let artifact = build_artifact(&config, &temp_dir, None).expect("extraction should succeed");

        // VkFlags -> TypeAlias to u32
        let flags = artifact.exports.iter().find(|e| e.name == "VkFlags");
        assert!(flags.is_some(), "VkFlags should be present");
        let f = flags.unwrap();
        assert_eq!(f.kind, ExportKind::TypeAlias);
        assert_eq!(f.type_key.as_deref(), Some("u32"));

        // VkBuffer -> TypeAlias to *mut void
        let buf = artifact.exports.iter().find(|e| e.name == "VkBuffer");
        assert!(buf.is_some(), "VkBuffer should be present");
        let b = buf.unwrap();
        assert_eq!(b.kind, ExportKind::TypeAlias);
        assert_eq!(b.type_key.as_deref(), Some("*mut void"));

        // Color -> Struct with r, g, b, a
        let col = artifact.exports.iter().find(|e| e.name == "Color");
        assert!(col.is_some(), "Color should be present");
        let c = col.unwrap();
        assert_eq!(c.kind, ExportKind::Struct);
        assert_eq!(c.fields.len(), 4);

        let _ = std::fs::remove_file(header_path);
        let _ = std::fs::remove_dir(temp_dir);
    }

    #[test]
    fn test_extracts_static_inline_and_generates_stubs() {
        let temp_dir = std::env::temp_dir().join("agsm_stubs_test");
        let _ = std::fs::create_dir_all(&temp_dir);
        let header_path = temp_dir.join("test_stubs.h");
        let header_content = r#"
            typedef struct Vector2 { float x; float y; } Vector2;

            static inline Vector2 Vector2Add(Vector2 v1, Vector2 v2) {
                Vector2 result = { v1.x + v2.x, v1.y + v2.y };
                return result;
            }

            static inline float Vector2Length(Vector2 v) {
                return v.x * v.x + v.y * v.y;
            }
        "#;
        std::fs::write(&header_path, header_content).unwrap();

        let config = ResolvedConfig {
            name: "test_stubs".to_string(),
            standard: CStandard::C11,
            target: None,
            includes: vec![header_path.clone()],
            include_paths: vec![],
            defines: vec![],
            libs: vec![],
            lib_paths: vec![],
            prefix_strip: vec![],
            allow: vec![],
            deny: vec![],
            export_types: true,
            export_opaque_types: false,
            opaque_types: vec![],
        };

        let artifact = build_artifact(&config, &temp_dir, None).expect("extraction should succeed");

        // Vector2Add should have link_name __silver_stub_Vector2Add
        let add = artifact.exports.iter().find(|e| e.name == "Vector2Add");
        assert!(add.is_some(), "Vector2Add should be present");
        let a = add.unwrap();
        assert_eq!(a.link_name.as_deref(), Some("__silver_stub_Vector2Add"));

        // Vector2Length should have link_name __silver_stub_Vector2Length
        let len = artifact.exports.iter().find(|e| e.name == "Vector2Length");
        assert!(len.is_some(), "Vector2Length should be present");
        let l = len.unwrap();
        assert_eq!(l.link_name.as_deref(), Some("__silver_stub_Vector2Length"));

        // Should have generated a static library object file and set has_static_library
        assert!(artifact.code_artifacts.has_static_library);
        let obj_file = temp_dir.join("test_stubs.o");
        assert!(obj_file.exists(), "stub object file should exist at {}", obj_file.display());

        let _ = std::fs::remove_file(obj_file);
        let _ = std::fs::remove_file(header_path);
        let _ = std::fs::remove_dir(temp_dir);
    }
}
