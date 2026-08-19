use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Deserializer};

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
pub enum PkgConfigList {
    Single(String),
    Multiple(Vec<String>),
}

pub type StringList = PkgConfigList;

impl PkgConfigList {
    pub fn into_vec(self) -> Vec<String> {
        match self {
            Self::Single(s) => vec![s],
            Self::Multiple(v) => v,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourcemapConfig {
    pub name: String,
    pub standard: CStandard,
    pub includes: Vec<PathBuf>,
    pub include_paths: Vec<PathBuf>,
    pub lib_paths: Vec<PathBuf>,
    pub defines: Vec<String>,
    pub libs: Vec<String>,
    pub pkg_config: Vec<String>,
    pub prefix_strip: Vec<String>,
    pub allow: Vec<String>,
    pub deny: Vec<String>,
    pub export_types: bool,
    pub export_opaque_types: bool,
    pub opaque_types: Vec<String>,
    targets: BTreeMap<String, TargetConfig>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadedConfig {
    pub path: PathBuf,
    pub config: SourcemapConfig,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CStandard {
    C99,
    C11,
}

impl CStandard {
    fn parse(value: &str) -> Result<Self, ConfigError> {
        match value {
            "c99" => Ok(Self::C99),
            "c11" => Ok(Self::C11),
            other => Err(ConfigError::InvalidField {
                field: "standard",
                message: format!("unsupported C standard `{other}`; expected `c99` or `c11`"),
            }),
        }
    }
}
impl<'de> Deserialize<'de> for CStandard {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Self::parse(&value).map_err(|error| serde::de::Error::custom(error.to_string()))
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
struct TargetConfig {
    standard: Option<CStandard>,
    includes: Option<Vec<PathBuf>>,
    include_paths: Option<Vec<PathBuf>>,
    lib_paths: Option<Vec<PathBuf>>,
    defines: Option<Vec<String>>,
    libs: Option<Vec<String>>,
    pkg_config: Option<PkgConfigList>,
    prefix_strip: Option<PkgConfigList>,
    allow: Option<PkgConfigList>,
    deny: Option<PkgConfigList>,
    export_opaque_types: Option<bool>,
    export_types: Option<bool>,
}

#[derive(Debug, Deserialize)]
struct RawConfig {
    name: Option<String>,
    standard: Option<String>,
    includes: Option<Vec<PathBuf>>,
    #[serde(default)]
    include_paths: Vec<PathBuf>,
    #[serde(default)]
    lib_paths: Vec<PathBuf>,
    #[serde(default)]
    defines: Vec<String>,
    #[serde(default)]
    libs: Vec<String>,
    #[serde(default)]
    pkg_config: Option<PkgConfigList>,
    #[serde(default)]
    prefix_strip: Option<PkgConfigList>,
    #[serde(default)]
    allow: Option<PkgConfigList>,
    #[serde(default)]
    deny: Option<PkgConfigList>,
    #[serde(default)]
    export_opaque_types: bool,
    #[serde(default)]
    opaque_types: Vec<String>,
    #[serde(default = "default_export_types")]
    export_types: bool,
    #[serde(flatten)]
    targets: BTreeMap<String, TargetConfig>,
}

fn default_export_types() -> bool {
    true
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedConfig {
    pub name: String,
    pub standard: CStandard,
    pub includes: Vec<PathBuf>,
    pub include_paths: Vec<PathBuf>,
    pub export_opaque_types: bool,
    pub opaque_types: Vec<String>,
    pub lib_paths: Vec<PathBuf>,
    pub defines: Vec<String>,
    pub libs: Vec<String>,
    pub prefix_strip: Vec<String>,
    pub allow: Vec<String>,
    pub deny: Vec<String>,
    pub export_types: bool,
    pub target: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetResolution {
    pub config: ResolvedConfig,
    pub warnings: Vec<String>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct PkgConfigResolution {
    pub include_paths: Vec<PathBuf>,
    pub lib_paths: Vec<PathBuf>,
    pub defines: Vec<String>,
    pub libs: Vec<String>,
}

pub fn query_pkg_config(packages: &[String]) -> Result<PkgConfigResolution, ConfigError> {
    if packages.is_empty() {
        return Ok(PkgConfigResolution::default());
    }

    let mut resolution = PkgConfigResolution::default();

    let cflags_output = std::process::Command::new("pkg-config")
        .arg("--cflags")
        .args(packages)
        .output()
        .map_err(|err| ConfigError::PkgConfig {
            packages: packages.to_vec(),
            message: format!("failed to execute pkg-config: {err}"),
        })?;

    if !cflags_output.status.success() {
        let stderr = String::from_utf8_lossy(&cflags_output.stderr);
        return Err(ConfigError::PkgConfig {
            packages: packages.to_vec(),
            message: stderr.trim().to_string(),
        });
    }

    let cflags_str = String::from_utf8_lossy(&cflags_output.stdout);
    for token in cflags_str.split_whitespace() {
        if let Some(inc) = token.strip_prefix("-I") {
            if !inc.is_empty() {
                let path = PathBuf::from(inc);
                if !resolution.include_paths.contains(&path) {
                    resolution.include_paths.push(path);
                }
            }
        } else if let Some(def) = token.strip_prefix("-D") {
            if !def.is_empty() && !resolution.defines.iter().any(|d| d == def) {
                resolution.defines.push(def.to_string());
            }
        }
    }

    let libs_output = std::process::Command::new("pkg-config")
        .arg("--libs")
        .args(packages)
        .output()
        .map_err(|err| ConfigError::PkgConfig {
            packages: packages.to_vec(),
            message: format!("failed to execute pkg-config: {err}"),
        })?;

    if !libs_output.status.success() {
        let stderr = String::from_utf8_lossy(&libs_output.stderr);
        return Err(ConfigError::PkgConfig {
            packages: packages.to_vec(),
            message: stderr.trim().to_string(),
        });
    }

    let libs_str = String::from_utf8_lossy(&libs_output.stdout);
    for token in libs_str.split_whitespace() {
        if let Some(lib_path) = token.strip_prefix("-L") {
            if !lib_path.is_empty() {
                let path = PathBuf::from(lib_path);
                if !resolution.lib_paths.contains(&path) {
                    resolution.lib_paths.push(path);
                }
            }
        } else if let Some(lib) = token.strip_prefix("-l") {
            if !lib.is_empty() && !resolution.libs.iter().any(|l| l == lib) {
                resolution.libs.push(lib.to_string());
            }
        }
    }

    Ok(resolution)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConfigError {
    Io {
        path: PathBuf,
        message: String,
    },
    Parse(String),
    MissingField(&'static str),
    InvalidField {
        field: &'static str,
        message: String,
    },
    PkgConfig {
        packages: Vec<String>,
        message: String,
    },
}

impl SourcemapConfig {
    pub fn load(path: impl AsRef<Path>) -> Result<LoadedConfig, ConfigError> {
        let path = path.as_ref().to_path_buf();
        let source = fs::read_to_string(&path).map_err(|error| ConfigError::Io {
            path: path.clone(),
            message: error.to_string(),
        })?;
        let config = Self::parse(&source)?;
        Ok(LoadedConfig { path, config })
    }

    pub fn parse(source: &str) -> Result<Self, ConfigError> {
        let raw: RawConfig =
            toml::from_str(source).map_err(|error| ConfigError::Parse(error.to_string()))?;
        let name = raw.name.ok_or(ConfigError::MissingField("name"))?;
        validate_name(&name)?;
        let includes = raw.includes.ok_or(ConfigError::MissingField("includes"))?;
        if includes.is_empty() {
            return Err(ConfigError::InvalidField {
                field: "includes",
                message: "at least one header is required".to_string(),
            });
        }
        let standard = CStandard::parse(raw.standard.as_deref().unwrap_or("c11"))?;
        let pkg_config = raw
            .pkg_config
            .map(PkgConfigList::into_vec)
            .unwrap_or_default();
        let prefix_strip = raw
            .prefix_strip
            .map(PkgConfigList::into_vec)
            .unwrap_or_default();
        let allow = raw
            .allow
            .map(PkgConfigList::into_vec)
            .unwrap_or_default();
        let deny = raw
            .deny
            .map(PkgConfigList::into_vec)
            .unwrap_or_default();
        Ok(Self {
            name,
            standard,
            includes,
            include_paths: raw.include_paths,
            lib_paths: raw.lib_paths,
            defines: raw.defines,
            libs: raw.libs,
            pkg_config,
            prefix_strip,
            allow,
            deny,
            export_types: raw.export_types,
            export_opaque_types: raw.export_opaque_types,
            opaque_types: raw.opaque_types,
            targets: raw.targets,
        })
    }
    pub fn resolve_target(&self, target: Option<&str>) -> Result<TargetResolution, ConfigError> {
        let Some(target_name) = target else {
            return Ok(TargetResolution {
                config: self.base_config(None)?,
                warnings: Vec::new(),
            });
        };
        let Some(overrides) = self.targets.get(target_name) else {
            return Ok(TargetResolution {
                config: self.base_config(Some(target_name.to_string()))?,
                warnings: vec![format!(
                    "target `{target_name}` has no configuration; using top-level values"
                )],
            });
        };
        let pkg_packages = overrides
            .pkg_config
            .as_ref()
            .map(|p| p.clone().into_vec())
            .unwrap_or_else(|| self.pkg_config.clone());
        let pkg_res = query_pkg_config(&pkg_packages)?;

        let mut include_paths = overrides
            .include_paths
            .clone()
            .unwrap_or_else(|| self.include_paths.clone());
        for p in pkg_res.include_paths {
            if !include_paths.contains(&p) {
                include_paths.push(p);
            }
        }

        let mut lib_paths = overrides
            .lib_paths
            .clone()
            .unwrap_or_else(|| self.lib_paths.clone());
        for p in pkg_res.lib_paths {
            if !lib_paths.contains(&p) {
                lib_paths.push(p);
            }
        }

        let mut defines = overrides
            .defines
            .clone()
            .unwrap_or_else(|| self.defines.clone());
        for d in pkg_res.defines {
            if !defines.iter().any(|existing| existing == &d) {
                defines.push(d);
            }
        }

        let mut libs = overrides.libs.clone().unwrap_or_else(|| self.libs.clone());
        for l in pkg_res.libs {
            if !libs.iter().any(|existing| existing == &l) {
                libs.push(l);
            }
        }

        let prefix_strip = overrides
            .prefix_strip
            .as_ref()
            .map(|p| p.clone().into_vec())
            .unwrap_or_else(|| self.prefix_strip.clone());
        let allow = overrides
            .allow
            .as_ref()
            .map(|p| p.clone().into_vec())
            .unwrap_or_else(|| self.allow.clone());
        let deny = overrides
            .deny
            .as_ref()
            .map(|p| p.clone().into_vec())
            .unwrap_or_else(|| self.deny.clone());

        let config = ResolvedConfig {
            name: self.name.clone(),
            standard: overrides.standard.unwrap_or(self.standard),
            includes: overrides
                .includes
                .clone()
                .unwrap_or_else(|| self.includes.clone()),
            include_paths,
            lib_paths,
            defines,
            libs,
            prefix_strip,
            allow,
            deny,
            export_types: overrides.export_types.unwrap_or(self.export_types),
            export_opaque_types: overrides
                .export_opaque_types
                .unwrap_or(self.export_opaque_types),
            opaque_types: self.opaque_types.clone(),
            target: Some(target_name.to_string()),
        };
        Ok(TargetResolution {
            config,
            warnings: Vec::new(),
        })
    }

    fn base_config(&self, target: Option<String>) -> Result<ResolvedConfig, ConfigError> {
        let pkg_res = query_pkg_config(&self.pkg_config)?;

        let mut include_paths = self.include_paths.clone();
        for p in pkg_res.include_paths {
            if !include_paths.contains(&p) {
                include_paths.push(p);
            }
        }

        let mut lib_paths = self.lib_paths.clone();
        for p in pkg_res.lib_paths {
            if !lib_paths.contains(&p) {
                lib_paths.push(p);
            }
        }

        let mut defines = self.defines.clone();
        for d in pkg_res.defines {
            if !defines.iter().any(|existing| existing == &d) {
                defines.push(d);
            }
        }

        let mut libs = self.libs.clone();
        for l in pkg_res.libs {
            if !libs.iter().any(|existing| existing == &l) {
                libs.push(l);
            }
        }

        Ok(ResolvedConfig {
            name: self.name.clone(),
            standard: self.standard,
            includes: self.includes.clone(),
            include_paths,
            lib_paths,
            libs,
            defines,
            prefix_strip: self.prefix_strip.clone(),
            allow: self.allow.clone(),
            deny: self.deny.clone(),
            export_types: self.export_types,
            export_opaque_types: self.export_opaque_types,
            opaque_types: self.opaque_types.clone(),
            target,
        })
    }
}

impl ResolvedConfig {
    pub fn append_cli(
        &mut self,
        include_paths: impl IntoIterator<Item = PathBuf>,
        lib_paths: impl IntoIterator<Item = PathBuf>,
        defines: impl IntoIterator<Item = String>,
    ) {
        self.include_paths.extend(include_paths);
        self.lib_paths.extend(lib_paths);
        self.defines.extend(defines);
    }

    pub fn resolve_path(&self, base_dir: &Path, path: &Path) -> PathBuf {
        if path.is_absolute() {
            path.to_path_buf()
        } else {
            base_dir.join(path)
        }
    }
}

fn validate_name(name: &str) -> Result<(), ConfigError> {
    if name.is_empty() || name == "." || name == ".." || name.contains('/') || name.contains('\\') {
        return Err(ConfigError::InvalidField {
            field: "name",
            message: "must be a single module name without path separators".to_string(),
        });
    }
    Ok(())
}

impl fmt::Display for ConfigError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io { path, message } => {
                write!(formatter, "failed to read {}: {message}", path.display())
            }
            Self::Parse(message) => write!(formatter, "invalid sourcemap.toml: {message}"),
            Self::MissingField(field) => write!(formatter, "missing required field `{field}`"),
            Self::InvalidField { field, message } => {
                write!(formatter, "invalid `{field}`: {message}")
            }
            Self::PkgConfig { packages, message } => {
                write!(
                    formatter,
                    "pkg-config failed for `{}`: {message}",
                    packages.join(", ")
                )
            }
        }
    }
}

impl std::error::Error for ConfigError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_to_c11_and_empty_optional_lists() {
        let config = SourcemapConfig::parse(
            r#"
name = "raylib"
includes = ["raylib.h"]
"#,
        )
        .unwrap();

        assert_eq!(config.standard, CStandard::C11);
        assert!(config.include_paths.is_empty());
        assert!(config.libs.is_empty());
        assert!(config.pkg_config.is_empty());
    }

    #[test]
    fn target_overrides_inherit_unspecified_values() {
        let config = SourcemapConfig::parse(
            r#"
name = "raylib"
standard = "c11"
includes = ["raylib.h"]
include_paths = ["include"]
libs = ["raylib"]

[x86_64-unknown-linux-gnu]
include_paths = ["linux/include"]
"#,
        )
        .unwrap();

        let resolution = config.resolve_target(Some("x86_64-unknown-linux-gnu")).unwrap();
        assert_eq!(
            resolution.config.include_paths,
            vec![PathBuf::from("linux/include")]
        );
        assert_eq!(resolution.config.includes, vec![PathBuf::from("raylib.h")]);
        assert_eq!(resolution.config.libs, vec!["raylib"]);
        assert!(resolution.warnings.is_empty());
    }

    #[test]
    fn missing_target_warns_and_uses_top_level_values() {
        let config = SourcemapConfig::parse(
            r#"
name = "raylib"
includes = ["raylib.h"]
"#,
        )
        .unwrap();

        let resolution = config.resolve_target(Some("i386-pc-windows-msvc")).unwrap();
        assert_eq!(resolution.config.includes, vec![PathBuf::from("raylib.h")]);
        assert_eq!(resolution.warnings.len(), 1);
    }

    #[test]
    fn parses_pkg_config_field_string_or_list() {
        let config_str = SourcemapConfig::parse(
            r#"
name = "raylib"
includes = ["raylib.h"]
pkg_config = "raylib"
"#,
        )
        .unwrap();
        assert_eq!(config_str.pkg_config, vec!["raylib"]);

        let config_list = SourcemapConfig::parse(
            r#"
name = "raylib"
includes = ["raylib.h"]
pkg_config = ["raylib", "glfw3"]
"#,
        )
        .unwrap();
        assert_eq!(config_list.pkg_config, vec!["raylib", "glfw3"]);
    }

    #[test]
    fn unsupported_standard_is_rejected() {
        let error = SourcemapConfig::parse(
            r#"
name = "raylib"
standard = "c17"
includes = ["raylib.h"]
"#,
        )
        .unwrap_err();

        assert!(error.to_string().contains("c99"));
    }
}
