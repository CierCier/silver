use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use crate::config::{ConfigError, LoadedConfig};
use crate::extract::{ExtractError, build_artifact};

#[derive(Debug, Default)]
pub struct BuildOptions {
    pub sourcemap: PathBuf,
    pub output: Option<PathBuf>,
    pub target: Option<String>,
    pub include_paths: Vec<PathBuf>,
    pub lib_paths: Vec<PathBuf>,
    pub defines: Vec<String>,
}

#[derive(Debug)]
pub enum BuildError {
    Config(ConfigError),
    Extract(ExtractError),
    Io { path: PathBuf, message: String },
}

impl fmt::Display for BuildError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Config(error) => error.fmt(formatter),
            Self::Extract(error) => error.fmt(formatter),
            Self::Io { path, message } => {
                write!(formatter, "failed to write {}: {message}", path.display())
            }
        }
    }
}

impl std::error::Error for BuildError {}

impl From<ConfigError> for BuildError {
    fn from(error: ConfigError) -> Self {
        Self::Config(error)
    }
}

impl From<ExtractError> for BuildError {
    fn from(error: ExtractError) -> Self {
        Self::Extract(error)
    }
}

pub fn build(options: &BuildOptions) -> Result<PathBuf, BuildError> {
    let LoadedConfig { path, config } = crate::config::SourcemapConfig::load(&options.sourcemap)?;
    let target_resolution = config.resolve_target(options.target.as_deref());
    for warning in &target_resolution.warnings {
        eprintln!("warning: {}:{}: {}", path.display(), 1, warning);
    }

    let mut resolved = target_resolution.config;
    resolved.append_cli(
        options.include_paths.clone(),
        options.lib_paths.clone(),
        options.defines.clone(),
    );
    let base_dir = path.parent().unwrap_or_else(|| Path::new("."));
    let artifact = build_artifact(&resolved, base_dir, options.target.as_deref())?;
    let output = options
        .output
        .clone()
        .unwrap_or_else(|| base_dir.join(format!("{}.agm", resolved.name)));
    let bytes = artifact.to_bytes().map_err(|message| BuildError::Io {
        path: output.clone(),
        message,
    })?;
    fs::write(&output, bytes).map_err(|error| BuildError::Io {
        path: output.clone(),
        message: error.to_string(),
    })?;
    Ok(output)
}
