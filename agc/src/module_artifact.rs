use std::path::Path;

use crate::parser::ast;
use crate::types::Type;

const MODULE_MAGIC: &[u8; 6] = b"AGBM\x00\x01";

#[derive(Debug, Clone)]
pub struct ModuleArtifact {
    pub module_name: String,
    pub source_path: String,
    pub source_hash_fnv1a64: u64,
    pub compiler_version: String,
    pub target_triple: String,
    pub exports: Vec<ModuleExport>,
    pub native_libs: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ModuleExport {
    pub kind: ExportKind,
    pub name: String,
    pub signature: String,
}

#[derive(Debug, Clone, Copy)]
pub enum ExportKind {
    Function,
    Struct,
    Enum,
    Trait,
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
        source_path: String,
        source_text: &str,
        program: &ast::Program,
        target_triple: String,
        native_libs: Vec<String>,
    ) -> Self {
        let exports = collect_exports(program);
        Self {
            module_name,
            source_path,
            source_hash_fnv1a64: fnv1a64(source_text.as_bytes()),
            compiler_version: env!("CARGO_PKG_VERSION").to_string(),
            target_triple,
            exports,
            native_libs,
        }
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, String> {
        let mut out = Vec::new();
        out.extend_from_slice(MODULE_MAGIC);
        write_string(&mut out, &self.module_name)?;
        write_string(&mut out, &self.source_path)?;
        out.extend_from_slice(&self.source_hash_fnv1a64.to_le_bytes());
        write_string(&mut out, &self.compiler_version)?;
        write_string(&mut out, &self.target_triple)?;
        write_len(&mut out, self.exports.len())?;
        for export in &self.exports {
            out.push(export.kind.as_u8());
            write_string(&mut out, &export.name)?;
            write_string(&mut out, &export.signature)?;
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
            return Err("invalid module artifact header".to_string());
        }
        let module_name = read_string(bytes, &mut cursor)?;
        let source_path = read_string(bytes, &mut cursor)?;
        let hash_bytes = read_exact(bytes, &mut cursor, 8)?;
        let source_hash_fnv1a64 = u64::from_le_bytes(
            hash_bytes
                .try_into()
                .map_err(|_| "invalid source hash bytes".to_string())?,
        );
        let compiler_version = read_string(bytes, &mut cursor)?;
        let target_triple = read_string(bytes, &mut cursor)?;
        let exports_len = read_len(bytes, &mut cursor)? as usize;
        let mut exports = Vec::with_capacity(exports_len);
        for _ in 0..exports_len {
            let kind = read_u8(bytes, &mut cursor)?;
            let name = read_string(bytes, &mut cursor)?;
            let signature = read_string(bytes, &mut cursor)?;
            let kind = match kind {
                1 => ExportKind::Function,
                2 => ExportKind::Struct,
                3 => ExportKind::Enum,
                4 => ExportKind::Trait,
                _ => return Err(format!("unknown export kind {kind}")),
            };
            exports.push(ModuleExport {
                kind,
                name,
                signature,
            });
        }
        let libs_len = read_len(bytes, &mut cursor)? as usize;
        let mut native_libs = Vec::with_capacity(libs_len);
        for _ in 0..libs_len {
            native_libs.push(read_string(bytes, &mut cursor)?);
        }
        Ok(Self {
            module_name,
            source_path,
            source_hash_fnv1a64,
            compiler_version,
            target_triple,
            exports,
            native_libs,
        })
    }

    pub fn from_path(path: &Path) -> Result<Self, String> {
        let bytes =
            std::fs::read(path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
        Self::from_bytes(&bytes)
    }
}

pub fn module_name_from_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module")
        .to_string()
}

fn collect_exports(program: &ast::Program) -> Vec<ModuleExport> {
    let mut exports = Vec::new();
    for item in &program.items {
        if !matches!(item.visibility, ast::Visibility::Public) {
            continue;
        }
        match &item.kind {
            ast::ItemKind::Function(func) => {
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
                exports.push(ModuleExport {
                    kind: ExportKind::Function,
                    name: func.name.name.clone(),
                    signature: format!("fn({params})->{ret}"),
                });
            }
            ast::ItemKind::Struct(s) => {
                let fields = s
                    .fields
                    .iter()
                    .map(|f| {
                        format!(
                            "{}:{}",
                            f.name.name,
                            Type::from_ast(&f.field_type).canonical_key()
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                exports.push(ModuleExport {
                    kind: ExportKind::Struct,
                    name: s.name.name.clone(),
                    signature: format!("struct{{{fields}}}"),
                });
            }
            ast::ItemKind::Enum(e) => {
                exports.push(ModuleExport {
                    kind: ExportKind::Enum,
                    name: e.name.name.clone(),
                    signature: format!("enum[{} variants]", e.variants.len()),
                });
            }
            ast::ItemKind::Trait(t) => {
                exports.push(ModuleExport {
                    kind: ExportKind::Trait,
                    name: t.name.name.clone(),
                    signature: format!("trait[{} items]", t.items.len()),
                });
            }
            _ => {}
        }
    }
    exports
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

fn read_exact<'a>(bytes: &'a [u8], cursor: &mut usize, len: usize) -> Result<&'a [u8], String> {
    if *cursor + len > bytes.len() {
        return Err("unexpected end of module artifact".to_string());
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
    let len = u32::from_le_bytes(
        slice
            .try_into()
            .map_err(|_| "invalid length bytes".to_string())?,
    );
    Ok(len)
}

fn read_string(bytes: &[u8], cursor: &mut usize) -> Result<String, String> {
    let len = read_len(bytes, cursor)? as usize;
    let slice = read_exact(bytes, cursor, len)?;
    let text =
        std::str::from_utf8(slice).map_err(|_| "invalid utf-8 in module artifact".to_string())?;
    Ok(text.to_string())
}

fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for b in bytes {
        hash ^= u64::from(*b);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}
