use std::path::{Path, PathBuf};

use crate::lexer::{self, Span};
use crate::parser;
use crate::parser::ast;
use crate::types::{parse_struct_attributes, struct_layout, Type, TypeContext, TypeLayout};

const MODULE_MAGIC: &[u8; 6] = b"AGM\x00\x00\x02";

#[derive(Debug, Clone)]
pub struct ModuleArtifact {
    pub module_name: String,
    pub module_path: String,
    pub source_path: String,
    pub source_hash_fnv1a64: u64,
    pub compiler_version: String,
    pub target_triple: String,
    pub code_artifacts: ModuleCodeArtifacts,
    pub module_deps: Vec<String>,
    pub exports: Vec<ModuleExport>,
    pub native_libs: Vec<String>,
    pub artifact_path: Option<PathBuf>,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ModuleCodeArtifacts {
    pub has_static_library: bool,
    pub has_shared_library: bool,
}

#[derive(Debug, Clone)]
pub struct ModuleExport {
    pub kind: ExportKind,
    pub name: String,
    pub signature: String,
    pub link_name: Option<String>,
    pub abi: Option<ModuleAbi>,
    pub is_variadic: bool,
    pub type_key: Option<String>,
    pub fields: Vec<ModuleField>,
    pub layout: Option<ModuleTypeLayout>,
    pub enum_backing_type: Option<String>,
    pub enum_variants: Vec<ModuleEnumVariant>,
    pub trait_items: Vec<ModuleTraitItem>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    Function,
    Struct,
    Enum,
    Trait,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleAbi {
    C,
    Silver,
    System,
    Rust,
    Cdecl,
    Stdcall,
    Fastcall,
}

#[derive(Debug, Clone)]
pub struct ModuleField {
    pub name: String,
    pub type_key: String,
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleTypeLayout {
    pub size: Option<u64>,
    pub align: Option<u64>,
}

#[derive(Debug, Clone)]
pub struct ModuleEnumVariant {
    pub name: String,
    pub value: i128,
}

#[derive(Debug, Clone)]
pub struct ModuleTraitItem {
    pub name: String,
    pub signature: String,
}

impl ExportKind {
    fn as_u8(self) -> u8 {
        match self {
            ExportKind::Function => 1,
            ExportKind::Struct => 2,
            ExportKind::Enum => 3,
            ExportKind::Trait => 4,
        }
    }
}

impl ModuleAbi {
    fn as_u8(self) -> u8 {
        match self {
            ModuleAbi::C => 1,
            ModuleAbi::Silver => 2,
            ModuleAbi::System => 3,
            ModuleAbi::Rust => 4,
            ModuleAbi::Cdecl => 5,
            ModuleAbi::Stdcall => 6,
            ModuleAbi::Fastcall => 7,
        }
    }

    fn from_linkage(linkage: &ast::ExternLinkage) -> Self {
        match linkage {
            ast::ExternLinkage::C => ModuleAbi::C,
            ast::ExternLinkage::Silver => ModuleAbi::Silver,
            ast::ExternLinkage::System => ModuleAbi::System,
            ast::ExternLinkage::Rust => ModuleAbi::Rust,
            ast::ExternLinkage::Cdecl => ModuleAbi::Cdecl,
            ast::ExternLinkage::Stdcall => ModuleAbi::Stdcall,
            ast::ExternLinkage::Fastcall => ModuleAbi::Fastcall,
        }
    }
}

impl ModuleExport {
    pub fn is_function(&self) -> bool {
        matches!(self.kind, ExportKind::Function)
    }

    pub fn is_struct(&self) -> bool {
        matches!(self.kind, ExportKind::Struct)
    }

    pub fn is_enum(&self) -> bool {
        matches!(self.kind, ExportKind::Enum)
    }

    pub fn is_trait(&self) -> bool {
        matches!(self.kind, ExportKind::Trait)
    }
}

impl ModuleArtifact {
    pub fn from_program(
        module_name: String,
        module_path: String,
        source_path: String,
        source_text: &str,
        program: &ast::Program,
        target_triple: String,
        code_artifacts: ModuleCodeArtifacts,
        module_deps: Vec<String>,
        native_libs: Vec<String>,
    ) -> Self {
        let exports = collect_exports(program);
        Self {
            module_name,
            module_path,
            source_path,
            source_hash_fnv1a64: fnv1a64(source_text.as_bytes()),
            compiler_version: env!("CARGO_PKG_VERSION").to_string(),
            target_triple,
            code_artifacts,
            module_deps,
            exports,
            native_libs,
            artifact_path: None,
        }
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, String> {
        let mut out = Vec::new();
        out.extend_from_slice(MODULE_MAGIC);
        write_string(&mut out, &self.module_name)?;
        write_string(&mut out, &self.module_path)?;
        write_string(&mut out, &self.source_path)?;
        out.extend_from_slice(&self.source_hash_fnv1a64.to_le_bytes());
        write_string(&mut out, &self.compiler_version)?;
        write_string(&mut out, &self.target_triple)?;
        out.push(self.code_artifacts.has_static_library as u8);
        out.push(self.code_artifacts.has_shared_library as u8);
        write_len(&mut out, self.module_deps.len())?;
        for dep in &self.module_deps {
            write_string(&mut out, dep)?;
        }
        write_len(&mut out, self.exports.len())?;
        for export in &self.exports {
            out.push(export.kind.as_u8());
            write_string(&mut out, &export.name)?;
            write_string(&mut out, &export.signature)?;
            write_optional_string(&mut out, export.link_name.as_deref())?;
            write_optional_u8(&mut out, export.abi.map(ModuleAbi::as_u8));
            out.push(export.is_variadic as u8);
            write_optional_string(&mut out, export.type_key.as_deref())?;
            write_len(&mut out, export.fields.len())?;
            for field in &export.fields {
                write_string(&mut out, &field.name)?;
                write_string(&mut out, &field.type_key)?;
            }
            write_layout(&mut out, export.layout)?;
            write_optional_string(&mut out, export.enum_backing_type.as_deref())?;
            write_len(&mut out, export.enum_variants.len())?;
            for variant in &export.enum_variants {
                write_string(&mut out, &variant.name)?;
                write_i128(&mut out, variant.value);
            }
            write_len(&mut out, export.trait_items.len())?;
            for item in &export.trait_items {
                write_string(&mut out, &item.name)?;
                write_string(&mut out, &item.signature)?;
            }
        }
        write_len(&mut out, self.native_libs.len())?;
        for lib in &self.native_libs {
            write_string(&mut out, lib)?;
        }
        Ok(out)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, String> {
        let mut cursor = 0;
        let magic = read_exact(bytes, &mut cursor, MODULE_MAGIC.len())?;
        if magic != MODULE_MAGIC {
            return Err("invalid module interface header".to_string());
        }
        let module_name = read_string(bytes, &mut cursor)?;
        let module_path = read_string(bytes, &mut cursor)?;
        let source_path = read_string(bytes, &mut cursor)?;
        let hash_bytes = read_exact(bytes, &mut cursor, 8)?;
        let source_hash_fnv1a64 = u64::from_le_bytes(
            hash_bytes
                .try_into()
                .map_err(|_| "invalid source hash bytes".to_string())?,
        );
        let compiler_version = read_string(bytes, &mut cursor)?;
        let target_triple = read_string(bytes, &mut cursor)?;
        let code_artifacts = ModuleCodeArtifacts {
            has_static_library: read_u8(bytes, &mut cursor)? != 0,
            has_shared_library: read_u8(bytes, &mut cursor)? != 0,
        };
        let deps_len = read_len(bytes, &mut cursor)? as usize;
        let mut module_deps = Vec::with_capacity(deps_len);
        for _ in 0..deps_len {
            module_deps.push(read_string(bytes, &mut cursor)?);
        }
        let exports_len = read_len(bytes, &mut cursor)? as usize;
        let mut exports = Vec::with_capacity(exports_len);
        for _ in 0..exports_len {
            let kind = match read_u8(bytes, &mut cursor)? {
                1 => ExportKind::Function,
                2 => ExportKind::Struct,
                3 => ExportKind::Enum,
                4 => ExportKind::Trait,
                other => return Err(format!("unknown export kind {other}")),
            };
            let name = read_string(bytes, &mut cursor)?;
            let signature = read_string(bytes, &mut cursor)?;
            let link_name = read_optional_string(bytes, &mut cursor)?;
            let abi = match read_optional_u8(bytes, &mut cursor)? {
                Some(1) => Some(ModuleAbi::C),
                Some(2) => Some(ModuleAbi::Silver),
                Some(3) => Some(ModuleAbi::System),
                Some(4) => Some(ModuleAbi::Rust),
                Some(5) => Some(ModuleAbi::Cdecl),
                Some(6) => Some(ModuleAbi::Stdcall),
                Some(7) => Some(ModuleAbi::Fastcall),
                Some(other) => return Err(format!("unknown module ABI {other}")),
                None => None,
            };
            let is_variadic = read_u8(bytes, &mut cursor)? != 0;
            let type_key = read_optional_string(bytes, &mut cursor)?;
            let fields_len = read_len(bytes, &mut cursor)? as usize;
            let mut fields = Vec::with_capacity(fields_len);
            for _ in 0..fields_len {
                fields.push(ModuleField {
                    name: read_string(bytes, &mut cursor)?,
                    type_key: read_string(bytes, &mut cursor)?,
                });
            }
            let layout = read_layout(bytes, &mut cursor)?;
            let enum_backing_type = read_optional_string(bytes, &mut cursor)?;
            let variants_len = read_len(bytes, &mut cursor)? as usize;
            let mut enum_variants = Vec::with_capacity(variants_len);
            for _ in 0..variants_len {
                enum_variants.push(ModuleEnumVariant {
                    name: read_string(bytes, &mut cursor)?,
                    value: read_i128(bytes, &mut cursor)?,
                });
            }
            let trait_items_len = read_len(bytes, &mut cursor)? as usize;
            let mut trait_items = Vec::with_capacity(trait_items_len);
            for _ in 0..trait_items_len {
                trait_items.push(ModuleTraitItem {
                    name: read_string(bytes, &mut cursor)?,
                    signature: read_string(bytes, &mut cursor)?,
                });
            }
            exports.push(ModuleExport {
                kind,
                name,
                signature,
                link_name,
                abi,
                is_variadic,
                type_key,
                fields,
                layout,
                enum_backing_type,
                enum_variants,
                trait_items,
            });
        }
        let libs_len = read_len(bytes, &mut cursor)? as usize;
        let mut native_libs = Vec::with_capacity(libs_len);
        for _ in 0..libs_len {
            native_libs.push(read_string(bytes, &mut cursor)?);
        }
        Ok(Self {
            module_name,
            module_path,
            source_path,
            source_hash_fnv1a64,
            compiler_version,
            target_triple,
            code_artifacts,
            module_deps,
            exports,
            native_libs,
            artifact_path: None,
        })
    }

    pub fn from_path(path: &Path) -> Result<Self, String> {
        let bytes =
            std::fs::read(path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
        let mut artifact = Self::from_bytes(&bytes)?;
        artifact.artifact_path = Some(path.to_path_buf());
        Ok(artifact)
    }

    pub fn from_source(path: &Path) -> Result<Self, String> {
        let src = std::fs::read_to_string(path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;

        let tokens = lexer::lex(&src).map_err(|e| format!("lexer errors in {path:?}: {e:?}"))?;

        let mut parser = parser::Parser::new_with_source(tokens, path.display().to_string());
        let (ast, errors) = parser.parse_program();

        if !errors.is_empty() {
            return Err(format!(
                "parser errors in {}: {:?}",
                path.display(),
                errors[0].format_with_help()
            ));
        }

        let module_path = module_path_from_path(path);
        Ok(Self::from_program(
            module_name_from_module_path(&module_path),
            module_path,
            path.display().to_string(),
            &src,
            &ast,
            "unknown".to_string(),
            ModuleCodeArtifacts::default(),
            collect_module_dependencies(&ast),
            Vec::new(),
        ))
    }

    pub fn static_library_path(&self) -> Option<PathBuf> {
        if !self.code_artifacts.has_static_library {
            return None;
        }
        let artifact_path = self.artifact_path.as_ref()?;
        Some(artifact_path.with_extension("o"))
    }

    pub fn shared_library_path(&self) -> Option<PathBuf> {
        if !self.code_artifacts.has_shared_library {
            return None;
        }
        let artifact_path = self.artifact_path.as_ref()?;
        Some(artifact_path.with_extension("so"))
    }

    pub fn source_candidate_paths(&self) -> Vec<PathBuf> {
        let mut paths = Vec::new();
        if !self.source_path.is_empty() {
            paths.push(PathBuf::from(&self.source_path));
        }
        if let Some(artifact_path) = &self.artifact_path {
            let sibling = artifact_path.with_extension("ag");
            if !paths.contains(&sibling) {
                paths.push(sibling);
            }
        }
        paths
    }
}

pub fn module_name_from_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module")
        .to_string()
}

pub fn module_path_from_path(path: &Path) -> String {
    let without_ext = path.with_extension("");
    without_ext
        .components()
        .filter_map(|component| component.as_os_str().to_str())
        .collect::<Vec<_>>()
        .join(".")
}

pub fn module_name_from_module_path(module_path: &str) -> String {
    module_path
        .rsplit('.')
        .next()
        .unwrap_or(module_path)
        .to_string()
}

pub fn ast_type_from_canonical_key(text: &str) -> Result<ast::Type, String> {
    Ok(Type::from_canonical_key(text)?.to_ast())
}

pub fn hash_source_text(text: &str) -> u64 {
    fnv1a64(text.as_bytes())
}

fn collect_module_dependencies(program: &ast::Program) -> Vec<String> {
    let mut deps = Vec::new();
    for item in &program.items {
        let ast::ItemKind::Import(import_item) = &item.kind else {
            continue;
        };
        let module_path = import_item
            .path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        if !module_path.is_empty() && !deps.contains(&module_path) {
            deps.push(module_path);
        }
    }
    deps
}

fn collect_exports(program: &ast::Program) -> Vec<ModuleExport> {
    let mut exports = Vec::new();
    let type_ctx = TypeContext::default();
    for item in &program.items {
        if matches!(item.visibility, ast::Visibility::Private) {
            continue;
        }
        match &item.kind {
            ast::ItemKind::Function(func) => {
                let params = func
                    .parameters
                    .iter()
                    .map(|p| Type::from_ast(&p.param_type).canonical_key())
                    .collect::<Vec<_>>();
                let ret = func
                    .return_type
                    .as_ref()
                    .map(Type::from_ast)
                    .unwrap_or(Type::Unit)
                    .canonical_key();
                exports.push(ModuleExport {
                    kind: ExportKind::Function,
                    name: func.name.name.clone(),
                    signature: format!("fn({})->{ret}", params.join(",")),
                    link_name: Some(func.name.name.clone()),
                    abi: Some(ModuleAbi::Silver),
                    is_variadic: false,
                    type_key: None,
                    fields: Vec::new(),
                    layout: None,
                    enum_backing_type: None,
                    enum_variants: Vec::new(),
                    trait_items: Vec::new(),
                });
            }
            ast::ItemKind::ExternFunction(func) => {
                let params = func
                    .signature
                    .parameters
                    .iter()
                    .map(|p| Type::from_ast(&p.param_type).canonical_key())
                    .collect::<Vec<_>>();
                let ret = func
                    .signature
                    .return_type
                    .as_ref()
                    .map(Type::from_ast)
                    .unwrap_or(Type::Unit)
                    .canonical_key();
                exports.push(ModuleExport {
                    kind: ExportKind::Function,
                    name: func.name.name.clone(),
                    signature: format!("fn({})->{ret}", params.join(",")),
                    link_name: Some(func.name.name.clone()),
                    abi: Some(ModuleAbi::from_linkage(&func.linkage)),
                    is_variadic: func.signature.is_variadic,
                    type_key: None,
                    fields: Vec::new(),
                    layout: None,
                    enum_backing_type: None,
                    enum_variants: Vec::new(),
                    trait_items: Vec::new(),
                });
            }
            ast::ItemKind::Struct(s) => {
                let fields = s
                    .fields
                    .iter()
                    .map(|f| ModuleField {
                        name: f.name.name.clone(),
                        type_key: Type::from_ast(&f.field_type).canonical_key(),
                    })
                    .collect::<Vec<_>>();
                let field_types = s
                    .fields
                    .iter()
                    .map(|field| Type::from_ast(&field.field_type))
                    .collect::<Vec<_>>();
                let attrs = parse_struct_attributes(&item.attributes).unwrap_or_default();
                let layout = struct_layout(&type_ctx, &field_types, &attrs);
                exports.push(ModuleExport {
                    kind: ExportKind::Struct,
                    name: s.name.name.clone(),
                    signature: format!(
                        "struct{{{}}}",
                        fields
                            .iter()
                            .map(|field| format!("{}:{}", field.name, field.type_key))
                            .collect::<Vec<_>>()
                            .join(",")
                    ),
                    link_name: None,
                    abi: None,
                    is_variadic: false,
                    type_key: Some(
                        Type::from_ast(&ast::Type {
                            kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                                path: vec![ast::Identifier {
                                    name: s.name.name.clone(),
                                    span: Span { start: 0, end: 0 },
                                }],
                                generics: None,
                            })),
                            span: Span { start: 0, end: 0 },
                        })
                        .canonical_key(),
                    ),
                    fields,
                    layout: to_module_layout(layout),
                    enum_backing_type: None,
                    enum_variants: Vec::new(),
                    trait_items: Vec::new(),
                });
            }
            ast::ItemKind::Enum(e) => {
                let variants = collect_enum_variants(e);
                let backing_type = choose_enum_backing_type_from_variants(&variants);
                exports.push(ModuleExport {
                    kind: ExportKind::Enum,
                    name: e.name.name.clone(),
                    signature: format!("enum[{} variants]", variants.len()),
                    link_name: None,
                    abi: None,
                    is_variadic: false,
                    type_key: Some(
                        Type::from_ast(&ast::Type {
                            kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                                path: vec![ast::Identifier {
                                    name: e.name.name.clone(),
                                    span: Span { start: 0, end: 0 },
                                }],
                                generics: None,
                            })),
                            span: Span { start: 0, end: 0 },
                        })
                        .canonical_key(),
                    ),
                    fields: Vec::new(),
                    layout: Some(ModuleTypeLayout {
                        size: primitive_layout_size(&backing_type).map(|size| size as u64),
                        align: primitive_layout_align(&backing_type).map(|align| align as u64),
                    }),
                    enum_backing_type: Some(Type::Primitive(backing_type.clone()).canonical_key()),
                    enum_variants: variants,
                    trait_items: Vec::new(),
                });
            }
            ast::ItemKind::Trait(t) => {
                let items = t
                    .items
                    .iter()
                    .filter_map(|item| match item {
                        ast::TraitItemKind::Function(func) => {
                            let params = func
                                .parameters
                                .iter()
                                .map(|p| Type::from_ast(&p.param_type).canonical_key())
                                .collect::<Vec<_>>()
                                .join(",");
                            let ret = func
                                .return_type
                                .as_ref()
                                .map(Type::from_ast)
                                .unwrap_or(Type::Unit)
                                .canonical_key();
                            Some(ModuleTraitItem {
                                name: func.name.name.clone(),
                                signature: format!("fn({params})->{ret}"),
                            })
                        }
                        ast::TraitItemKind::AssociatedType(assoc) => Some(ModuleTraitItem {
                            name: assoc.name.name.clone(),
                            signature: "associated_type".to_string(),
                        }),
                    })
                    .collect::<Vec<_>>();
                exports.push(ModuleExport {
                    kind: ExportKind::Trait,
                    name: t.name.name.clone(),
                    signature: format!("trait[{} items]", items.len()),
                    link_name: None,
                    abi: None,
                    is_variadic: false,
                    type_key: Some(t.name.name.clone()),
                    fields: Vec::new(),
                    layout: None,
                    enum_backing_type: None,
                    enum_variants: Vec::new(),
                    trait_items: items,
                });
            }
            _ => {}
        }
    }
    exports
}

fn collect_enum_variants(enum_item: &ast::EnumItem) -> Vec<ModuleEnumVariant> {
    let mut variants = Vec::new();
    let mut next_value = 0i128;
    for variant in &enum_item.variants {
        if !matches!(variant.data, ast::EnumVariantData::Unit) {
            continue;
        }
        let value = variant.discriminant.unwrap_or(next_value);
        variants.push(ModuleEnumVariant {
            name: variant.name.name.clone(),
            value,
        });
        next_value = value.saturating_add(1);
    }
    variants
}

fn choose_enum_backing_type_from_variants(variants: &[ModuleEnumVariant]) -> ast::PrimitiveType {
    let min_value = variants
        .iter()
        .map(|variant| variant.value)
        .min()
        .unwrap_or(0);
    let max_value = variants
        .iter()
        .map(|variant| variant.value)
        .max()
        .unwrap_or(0);
    choose_enum_backing_type(min_value, max_value)
}

fn choose_enum_backing_type(min_value: i128, max_value: i128) -> ast::PrimitiveType {
    if min_value >= 0 {
        if max_value <= u8::MAX as i128 {
            ast::PrimitiveType::U8
        } else if max_value <= u16::MAX as i128 {
            ast::PrimitiveType::U16
        } else if max_value <= u32::MAX as i128 {
            ast::PrimitiveType::U32
        } else if max_value <= u64::MAX as i128 {
            ast::PrimitiveType::U64
        } else {
            ast::PrimitiveType::U128
        }
    } else if min_value >= i8::MIN as i128 && max_value <= i8::MAX as i128 {
        ast::PrimitiveType::I8
    } else if min_value >= i16::MIN as i128 && max_value <= i16::MAX as i128 {
        ast::PrimitiveType::I16
    } else if min_value >= i32::MIN as i128 && max_value <= i32::MAX as i128 {
        ast::PrimitiveType::I32
    } else if min_value >= i64::MIN as i128 && max_value <= i64::MAX as i128 {
        ast::PrimitiveType::I64
    } else {
        ast::PrimitiveType::I128
    }
}

fn to_module_layout(layout: TypeLayout) -> Option<ModuleTypeLayout> {
    Some(ModuleTypeLayout {
        size: layout.size.map(|size| size as u64),
        align: layout.align.map(|align| align as u64),
    })
}

fn primitive_layout_size(primitive: &ast::PrimitiveType) -> Option<usize> {
    Some(match primitive {
        ast::PrimitiveType::I8 | ast::PrimitiveType::U8 => 1,
        ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => 2,
        ast::PrimitiveType::I32 | ast::PrimitiveType::U32 => 4,
        ast::PrimitiveType::I64 | ast::PrimitiveType::U64 => 8,
        ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => 16,
        ast::PrimitiveType::F32 => 4,
        ast::PrimitiveType::F64 => 8,
        ast::PrimitiveType::F80 => 16,
        ast::PrimitiveType::C32 => 8,
        ast::PrimitiveType::C64 => 16,
        ast::PrimitiveType::C80 => 32,
        ast::PrimitiveType::Bool => 1,
        ast::PrimitiveType::Char => 4,
        ast::PrimitiveType::Str => 8,
        ast::PrimitiveType::Void => 0,
    })
}

fn primitive_layout_align(primitive: &ast::PrimitiveType) -> Option<usize> {
    Some(match primitive {
        ast::PrimitiveType::I8 | ast::PrimitiveType::U8 => 1,
        ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => 2,
        ast::PrimitiveType::I32 | ast::PrimitiveType::U32 => 4,
        ast::PrimitiveType::I64 | ast::PrimitiveType::U64 => 8,
        ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => 16,
        ast::PrimitiveType::F32 => 4,
        ast::PrimitiveType::F64 => 8,
        ast::PrimitiveType::F80 => 16,
        ast::PrimitiveType::C32 => 4,
        ast::PrimitiveType::C64 => 8,
        ast::PrimitiveType::C80 => 16,
        ast::PrimitiveType::Bool => 1,
        ast::PrimitiveType::Char => 4,
        ast::PrimitiveType::Str => 8,
        ast::PrimitiveType::Void => 1,
    })
}

fn write_len(out: &mut Vec<u8>, len: usize) -> Result<(), String> {
    let len32 = u32::try_from(len).map_err(|_| format!("length does not fit u32: {len}"))?;
    out.extend_from_slice(&len32.to_le_bytes());
    Ok(())
}

fn write_string(out: &mut Vec<u8>, text: &str) -> Result<(), String> {
    write_len(out, text.len())?;
    out.extend_from_slice(text.as_bytes());
    Ok(())
}

fn write_optional_string(out: &mut Vec<u8>, text: Option<&str>) -> Result<(), String> {
    match text {
        Some(text) => {
            out.push(1);
            write_string(out, text)
        }
        None => {
            out.push(0);
            Ok(())
        }
    }
}

fn write_optional_u8(out: &mut Vec<u8>, value: Option<u8>) {
    match value {
        Some(value) => {
            out.push(1);
            out.push(value);
        }
        None => out.push(0),
    }
}

fn write_layout(out: &mut Vec<u8>, layout: Option<ModuleTypeLayout>) -> Result<(), String> {
    match layout {
        Some(layout) => {
            out.push(1);
            write_optional_u64(out, layout.size);
            write_optional_u64(out, layout.align);
        }
        None => out.push(0),
    }
    Ok(())
}

fn write_optional_u64(out: &mut Vec<u8>, value: Option<u64>) {
    match value {
        Some(value) => {
            out.push(1);
            out.extend_from_slice(&value.to_le_bytes());
        }
        None => out.push(0),
    }
}

fn write_i128(out: &mut Vec<u8>, value: i128) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn read_exact<'a>(bytes: &'a [u8], cursor: &mut usize, len: usize) -> Result<&'a [u8], String> {
    if *cursor + len > bytes.len() {
        return Err("unexpected end of module interface".to_string());
    }
    let start = *cursor;
    let end = start + len;
    *cursor = end;
    Ok(&bytes[start..end])
}

fn read_u8(bytes: &[u8], cursor: &mut usize) -> Result<u8, String> {
    Ok(*read_exact(bytes, cursor, 1)?
        .first()
        .ok_or_else(|| "missing byte".to_string())?)
}

fn read_len(bytes: &[u8], cursor: &mut usize) -> Result<u32, String> {
    let slice = read_exact(bytes, cursor, 4)?;
    Ok(u32::from_le_bytes(
        slice
            .try_into()
            .map_err(|_| "invalid length bytes".to_string())?,
    ))
}

fn read_string(bytes: &[u8], cursor: &mut usize) -> Result<String, String> {
    let len = read_len(bytes, cursor)? as usize;
    let slice = read_exact(bytes, cursor, len)?;
    let text =
        std::str::from_utf8(slice).map_err(|_| "invalid utf-8 in module interface".to_string())?;
    Ok(text.to_string())
}

fn read_optional_string(bytes: &[u8], cursor: &mut usize) -> Result<Option<String>, String> {
    match read_u8(bytes, cursor)? {
        0 => Ok(None),
        1 => Ok(Some(read_string(bytes, cursor)?)),
        other => Err(format!("invalid optional string tag {other}")),
    }
}

fn read_optional_u8(bytes: &[u8], cursor: &mut usize) -> Result<Option<u8>, String> {
    match read_u8(bytes, cursor)? {
        0 => Ok(None),
        1 => Ok(Some(read_u8(bytes, cursor)?)),
        other => Err(format!("invalid optional u8 tag {other}")),
    }
}

fn read_optional_u64(bytes: &[u8], cursor: &mut usize) -> Result<Option<u64>, String> {
    match read_u8(bytes, cursor)? {
        0 => Ok(None),
        1 => {
            let slice = read_exact(bytes, cursor, 8)?;
            Ok(Some(u64::from_le_bytes(
                slice
                    .try_into()
                    .map_err(|_| "invalid u64 bytes".to_string())?,
            )))
        }
        other => Err(format!("invalid optional u64 tag {other}")),
    }
}

fn read_layout(bytes: &[u8], cursor: &mut usize) -> Result<Option<ModuleTypeLayout>, String> {
    match read_u8(bytes, cursor)? {
        0 => Ok(None),
        1 => Ok(Some(ModuleTypeLayout {
            size: read_optional_u64(bytes, cursor)?,
            align: read_optional_u64(bytes, cursor)?,
        })),
        other => Err(format!("invalid layout tag {other}")),
    }
}

fn read_i128(bytes: &[u8], cursor: &mut usize) -> Result<i128, String> {
    let slice = read_exact(bytes, cursor, 16)?;
    Ok(i128::from_le_bytes(
        slice
            .try_into()
            .map_err(|_| "invalid i128 bytes".to_string())?,
    ))
}

fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for b in bytes {
        hash ^= u64::from(*b);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_rich_module_artifact() {
        let artifact = ModuleArtifact {
            module_name: "io".to_string(),
            module_path: "std.io".to_string(),
            source_path: "std/io.ag".to_string(),
            source_hash_fnv1a64: 42,
            compiler_version: "test".to_string(),
            target_triple: "x86_64-unknown-linux-gnu".to_string(),
            code_artifacts: ModuleCodeArtifacts {
                has_static_library: true,
                has_shared_library: true,
            },
            module_deps: vec!["std.mem".to_string()],
            exports: vec![
                ModuleExport {
                    kind: ExportKind::Function,
                    name: "print".to_string(),
                    signature: "fn(str)->unit".to_string(),
                    link_name: Some("silver_print".to_string()),
                    abi: Some(ModuleAbi::Silver),
                    is_variadic: false,
                    type_key: None,
                    fields: Vec::new(),
                    layout: None,
                    enum_backing_type: None,
                    enum_variants: Vec::new(),
                    trait_items: Vec::new(),
                },
                ModuleExport {
                    kind: ExportKind::Struct,
                    name: "File".to_string(),
                    signature: "struct{raw:str,is_open:bool}".to_string(),
                    link_name: None,
                    abi: None,
                    is_variadic: false,
                    type_key: Some("File".to_string()),
                    fields: vec![
                        ModuleField {
                            name: "raw".to_string(),
                            type_key: "str".to_string(),
                        },
                        ModuleField {
                            name: "is_open".to_string(),
                            type_key: "bool".to_string(),
                        },
                    ],
                    layout: Some(ModuleTypeLayout {
                        size: Some(16),
                        align: Some(8),
                    }),
                    enum_backing_type: None,
                    enum_variants: Vec::new(),
                    trait_items: Vec::new(),
                },
            ],
            native_libs: vec!["c".to_string()],
            artifact_path: None,
        };

        let bytes = artifact.to_bytes().expect("artifact should encode");
        let decoded = ModuleArtifact::from_bytes(&bytes).expect("artifact should decode");

        assert_eq!(decoded.module_path, "std.io");
        assert!(decoded.code_artifacts.has_static_library);
        assert!(decoded.code_artifacts.has_shared_library);
        assert_eq!(decoded.module_deps, vec!["std.mem".to_string()]);
        assert_eq!(decoded.exports.len(), 2);
        assert_eq!(
            decoded.exports[0].link_name.as_deref(),
            Some("silver_print")
        );
        assert_eq!(decoded.exports[1].fields.len(), 2);
        assert_eq!(decoded.exports[1].layout.unwrap().size, Some(16));
    }
}
