use std::collections::BTreeMap;
use std::fmt;
use std::path::PathBuf;

use serde::{Deserialize, Deserializer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourcemapConfig {
    pub name: String,
    pub standard: CStandard,
    pub includes: Vec<PathBuf>,
    pub include_paths: Vec<PathBuf>,
    pub lib_paths: Vec<PathBuf>,
    pub defines: Vec<String>,
    pub libs: Vec<String>,
    targets: BTreeMap<String, TargetConfig>,
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
    #[serde(flatten)]
    targets: BTreeMap<String, TargetConfig>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedConfig {
    pub name: String,
    pub standard: CStandard,
    pub includes: Vec<PathBuf>,
    pub include_paths: Vec<PathBuf>,
    pub lib_paths: Vec<PathBuf>,
    pub defines: Vec<String>,
    pub libs: Vec<String>,
    pub target: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetResolution {
    pub config: ResolvedConfig,
    pub warnings: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConfigError {
    Parse(String),
    MissingField(&'static str),
    InvalidField {
        field: &'static str,
        message: String,
    },
}

impl SourcemapConfig {
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
        Ok(Self {
            name,
            standard,
            includes,
            include_paths: raw.include_paths,
            lib_paths: raw.lib_paths,
            defines: raw.defines,
            libs: raw.libs,
            targets: raw.targets,
        })
    }

    pub fn resolve_target(&self, target: Option<&str>) -> TargetResolution {
        let Some(target_name) = target else {
            return TargetResolution {
                config: self.base_config(None),
                warnings: Vec::new(),
            };
        };

        let Some(overrides) = self.targets.get(target_name) else {
            return TargetResolution {
                config: self.base_config(Some(target_name.to_string())),
                warnings: vec![format!(
                    "target `{target_name}` has no configuration; using top-level values"
                )],
            };
        };

        let config = ResolvedConfig {
            name: self.name.clone(),
            standard: overrides.standard.unwrap_or(self.standard),
            includes: overrides
                .includes
                .clone()
                .unwrap_or_else(|| self.includes.clone()),
            include_paths: overrides
                .include_paths
                .clone()
                .unwrap_or_else(|| self.include_paths.clone()),
            lib_paths: overrides
                .lib_paths
                .clone()
                .unwrap_or_else(|| self.lib_paths.clone()),
            defines: overrides
                .defines
                .clone()
                .unwrap_or_else(|| self.defines.clone()),
            libs: overrides.libs.clone().unwrap_or_else(|| self.libs.clone()),
            target: Some(target_name.to_string()),
        };
        TargetResolution {
            config,
            warnings: Vec::new(),
        }
    }

    fn base_config(&self, target: Option<String>) -> ResolvedConfig {
        ResolvedConfig {
            name: self.name.clone(),
            standard: self.standard,
            includes: self.includes.clone(),
            include_paths: self.include_paths.clone(),
            lib_paths: self.lib_paths.clone(),
            defines: self.defines.clone(),
            libs: self.libs.clone(),
            target,
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
            Self::Parse(message) => write!(formatter, "invalid sourcemap.toml: {message}"),
            Self::MissingField(field) => write!(formatter, "missing required field `{field}`"),
            Self::InvalidField { field, message } => {
                write!(formatter, "invalid `{field}`: {message}")
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

        let resolution = config.resolve_target(Some("x86_64-unknown-linux-gnu"));
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

        let resolution = config.resolve_target(Some("i386-pc-windows-msvc"));
        assert_eq!(resolution.config.includes, vec![PathBuf::from("raylib.h")]);
        assert_eq!(resolution.warnings.len(), 1);
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
