//! Silver package manifests and dependency resolution.
//!
//! Package resolution deliberately stops at source paths and package roots.
//! The compiler's existing module loader and semantic pipeline remain the only
//! consumers of Silver source code.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::env;
use std::ffi::OsString;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
#[cfg(test)]
use std::time::{SystemTime, UNIX_EPOCH};

use toml::Value;

pub const MANIFEST_FILE: &str = "silver.toml";
const DEFAULT_PACKAGE_VERSION: &str = "0.1.0";
const DEFAULT_ENTRY: &str = "src/main.ag";
const DEFAULT_ENTRY_SOURCE: &str =
    "i32 main() {\n    @println(\"Her De Der, Theres silver in this vein!\");\n    return 0;\n}\n";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InitializedPackage {
    pub root: PathBuf,
    pub name: String,
    pub manifest_path: PathBuf,
    pub entry_path: PathBuf,
}

pub fn initialize_package(
    root: impl AsRef<Path>,
    name_override: Option<&str>,
) -> Result<InitializedPackage, String> {
    let requested_root = root.as_ref();
    if requested_root.exists() && !requested_root.is_dir() {
        return Err(format!(
            "package target `{}` exists and is not a directory",
            requested_root.display()
        ));
    }
    fs::create_dir_all(requested_root).map_err(|error| {
        format!(
            "failed to create package directory `{}`: {error}",
            requested_root.display()
        )
    })?;
    let root = fs::canonicalize(requested_root).map_err(|error| {
        format!(
            "failed to canonicalize package directory `{}`: {error}",
            requested_root.display()
        )
    })?;

    let manifest_path = root.join(MANIFEST_FILE);
    if manifest_path.exists() {
        return Err(format!(
            "package manifest already exists: `{}`",
            manifest_path.display()
        ));
    }

    let name = match name_override {
        Some(name) => {
            validate_init_name(name)?;
            name.to_string()
        }
        None => root
            .file_name()
            .and_then(|name| name.to_str())
            .filter(|name| !name.is_empty())
            .map(str::to_string)
            .ok_or_else(|| {
                "could not detect a package name from the target directory; pass `--name NAME`"
                    .to_string()
            })?,
    };
    validate_init_name(&name)?;

    let entry_path = root.join(DEFAULT_ENTRY);
    if entry_path.exists() && !entry_path.is_file() {
        return Err(format!(
            "package entry path `{}` exists and is not a file",
            entry_path.display()
        ));
    }
    if let Some(parent) = entry_path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create package source directory `{}`: {error}",
                parent.display()
            )
        })?;
    }

    let escaped_name = toml_basic_string(&name);
    let manifest = format!(
        "name = \"{escaped_name}\"\nversion = \"{DEFAULT_PACKAGE_VERSION}\"\n\n[bin.\"{escaped_name}\"]\nentry = \"{DEFAULT_ENTRY}\"\n"
    );
    fs::write(&manifest_path, manifest).map_err(|error| {
        format!(
            "failed to write package manifest `{}`: {error}",
            manifest_path.display()
        )
    })?;
    if !entry_path.exists() {
        if let Err(error) = fs::write(&entry_path, DEFAULT_ENTRY_SOURCE) {
            let mut rollback_errors = Vec::new();
            for path in [&entry_path, &manifest_path] {
                match fs::remove_file(path) {
                    Ok(()) => {}
                    Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                    Err(error) => rollback_errors.push(format!(
                        "failed to remove `{}` during rollback: {error}",
                        path.display()
                    )),
                }
            }

            let rollback = if rollback_errors.is_empty() {
                String::new()
            } else {
                format!("; {}", rollback_errors.join("; "))
            };
            return Err(format!(
                "failed to write package entry `{}`: {error}{rollback}",
                entry_path.display()
            ));
        }
    }

    init_git_repository(&root);

    Ok(InitializedPackage {
        root,
        name,
        manifest_path,
        entry_path,
    })
}

fn is_git_in_path() -> bool {
    Command::new("git")
        .arg("--version")
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
}

fn is_inside_git_repo(root: &Path) -> bool {
    if root.join(".git").exists() {
        return true;
    }
    Command::new("git")
        .arg("-C")
        .arg(root)
        .args(["rev-parse", "--is-inside-work-tree"])
        .output()
        .map(|output| output.status.success() && output.stdout.starts_with(b"true"))
        .unwrap_or(false)
}

fn init_git_repository(root: &Path) {
    if !is_inside_git_repo(root) && is_git_in_path() {
        let _ = Command::new("git")
            .arg("init")
            .current_dir(root)
            .output();
    }
}

fn validate_init_name(name: &str) -> Result<(), String> {
    if name.trim().is_empty() {
        return Err("package name must not be empty".to_string());
    }
    if name.chars().any(char::is_control) {
        return Err("package name must not contain control characters".to_string());
    }
    Ok(())
}

fn toml_basic_string(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for character in value.chars() {
        match character {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            character => escaped.push(character),
        }
    }
    escaped
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TargetKind {
    Bin,
    Lib,
}

impl TargetKind {
    fn label(self) -> &'static str {
        match self {
            Self::Bin => "bin",
            Self::Lib => "lib",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetSelection {
    pub kind: TargetKind,
    pub name: Option<String>,
}

impl TargetSelection {
    pub fn bin(name: Option<String>) -> Self {
        Self {
            kind: TargetKind::Bin,
            name,
        }
    }

    pub fn lib(name: Option<String>) -> Self {
        Self {
            kind: TargetKind::Lib,
            name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GitSelector {
    Default,
    Branch(String),
    Tag(String),
    Rev(String),
}

impl GitSelector {
    fn description(&self) -> String {
        match self {
            Self::Default => "default branch".to_string(),
            Self::Branch(branch) => format!("branch: {branch}"),
            Self::Tag(tag) => format!("tag: {tag}"),
            Self::Rev(rev) => format!("revision: {rev}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitSourceSpec {
    /// The underlying Git URL, without Silver's `git+` prefix.
    pub url: String,
    pub selector: GitSelector,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ManifestSourceSpec {
    Local(PathBuf),
    Git(GitSourceSpec),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetSourceSpec {
    Entry(PathBuf),
    Manifest(ManifestSourceSpec),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestTarget {
    pub name: String,
    pub kind: TargetKind,
    pub source: TargetSourceSpec,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencySpec {
    pub name: String,
    pub source: ManifestSourceSpec,
}

#[derive(Debug, Clone)]
pub struct PackageManifest {
    pub manifest_path: PathBuf,
    pub name: String,
    pub version: String,
    pub url: Option<String>,
    pub targets: Vec<ManifestTarget>,
    pub dependencies: BTreeMap<String, DependencySpec>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageSource {
    Local {
        manifest_path: PathBuf,
    },
    Git {
        repository_url: String,
        requested_selector: GitSelector,
        resolved_commit: String,
        manifest_path: PathBuf,
        manifest_path_in_repository: PathBuf,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PackageId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedTargetSource {
    Entry(PathBuf),
    Manifest(PackageId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageTarget {
    pub name: String,
    pub kind: TargetKind,
    pub source: ResolvedTargetSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageDependency {
    pub name: String,
    pub package: PackageId,
}

#[derive(Debug, Clone)]
pub struct Package {
    pub id: PackageId,
    pub name: String,
    pub version: String,
    pub url: Option<String>,
    pub manifest_path: PathBuf,
    pub source: PackageSource,
    pub targets: Vec<PackageTarget>,
    pub dependencies: Vec<PackageDependency>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedTarget {
    pub name: String,
    pub kind: TargetKind,
    pub entry: PathBuf,
    pub package: PackageId,
}

/// A package dependency exposed to the compiler as a module import.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencyImport {
    pub name: String,
    pub owner: PackageId,
    pub owner_root: PathBuf,
    pub target: ResolvedTarget,
    pub source: PathBuf,
}

#[derive(Debug, Clone)]
pub struct PackageGraph {
    root: PackageId,
    packages: Vec<Package>,
}

impl PackageGraph {
    pub fn root(&self) -> PackageId {
        self.root
    }

    pub fn package(&self, id: PackageId) -> Option<&Package> {
        self.packages.get(id.0)
    }

    pub fn packages(&self) -> &[Package] {
        &self.packages
    }

    /// Return every package directory as a module search root, preserving the
    /// graph's deterministic loading order and removing duplicate paths.
    pub fn package_roots(&self) -> Vec<PathBuf> {
        let mut roots = Vec::new();
        for package in &self.packages {
            let Some(root) = package.manifest_path.parent() else {
                continue;
            };
            let root = root.to_path_buf();
            if !roots.contains(&root) {
                roots.push(root);
            }
        }
        roots
    }

    /// Resolve each package dependency to its library target when one is
    /// declared. Binary-only dependencies remain valid graph nodes but are
    /// not importable modules.
    pub fn dependency_imports(&self) -> Result<Vec<DependencyImport>, String> {
        let mut imports = Vec::new();

        for owner in &self.packages {
            let owner_root = owner
                .manifest_path
                .parent()
                .ok_or_else(|| {
                    format!("package `{}` manifest has no parent directory", owner.name)
                })?
                .to_path_buf();

            for dependency in &owner.dependencies {
                let dependency_package = self.package(dependency.package).ok_or_else(|| {
                    format!(
                        "package `{}` dependency `{}` points to a missing package",
                        owner.name, dependency.name
                    )
                })?;

                // A package may be used only for its own binary target. Do
                // not reject that graph merely because it has no library.
                if !dependency_package
                    .targets
                    .iter()
                    .any(|target| target.kind == TargetKind::Lib)
                {
                    continue;
                }

                let target = find_target(
                    dependency_package,
                    TargetKind::Lib,
                    None,
                    Some(dependency.name.as_str()),
                )
                .map_err(|error| {
                    format!(
                        "cannot expose dependency `{}` from package `{}`: {error}",
                        dependency.name, owner.name
                    )
                })?;
                let mut seen = HashSet::new();
                let target = self.flatten_target(dependency_package.id, target, &mut seen)?;
                let source = dependency_import_source(self, &target);

                if let Some(previous) = imports.iter().find(|import: &&DependencyImport| {
                    import.name == dependency.name && import.owner_root == owner_root
                }) {
                    if previous.source != source || previous.owner_root != owner_root {
                        return Err(format!(
                            "dependency import `{}` is ambiguous between `{}` and `{}`",
                            dependency.name,
                            previous.owner_root.display(),
                            owner_root.display()
                        ));
                    }
                    continue;
                }

                imports.push(DependencyImport {
                    name: dependency.name.clone(),
                    owner: owner.id,
                    owner_root: owner_root.clone(),
                    target,
                    source,
                });
            }
        }

        Ok(imports)
    }

    pub fn select_target(&self, selection: &TargetSelection) -> Result<ResolvedTarget, String> {
        let root = self
            .package(self.root)
            .ok_or_else(|| "package graph root is missing".to_string())?;
        let target = find_target(root, selection.kind, selection.name.as_deref(), None)?;
        let mut seen = HashSet::new();
        let resolved = self.flatten_target(root.id, target, &mut seen)?;
        if !resolved.entry.is_file() {
            return Err(format!(
                "selected `{}` target `{}` has no source entry at `{}`; prebuilt `.agm` files can only satisfy library dependencies",
                resolved.kind.label(),
                resolved.name,
                resolved.entry.display()
            ));
        }
        Ok(resolved)
    }

    fn flatten_target(
        &self,
        package_id: PackageId,
        target: &PackageTarget,
        seen: &mut HashSet<PackageId>,
    ) -> Result<ResolvedTarget, String> {
        if !seen.insert(package_id) {
            return Err(format!(
                "cyclic manifest-backed target reference at package `{package_id:?}`"
            ));
        }

        let result = match &target.source {
            ResolvedTargetSource::Entry(entry) => Ok(ResolvedTarget {
                name: target.name.clone(),
                kind: target.kind,
                entry: entry.clone(),
                package: package_id,
            }),
            ResolvedTargetSource::Manifest(child_id) => {
                let child = self.package(*child_id).ok_or_else(|| {
                    format!(
                        "manifest-backed target `{}` points to a missing package",
                        target.name
                    )
                })?;
                let child_target =
                    find_target(child, target.kind, None, Some(target.name.as_str()))?;
                self.flatten_target(child.id, child_target, seen)
            }
        };

        seen.remove(&package_id);
        result
    }
}

fn dependency_import_source(graph: &PackageGraph, target: &ResolvedTarget) -> PathBuf {
    let sibling_artifact = target.entry.with_extension("agm");
    if sibling_artifact.is_file() {
        return sibling_artifact;
    }

    if let Some(package) = graph.package(target.package)
        && let Some(package_root) = package.manifest_path.parent()
    {
        let packaged_artifact = package_root
            .join("lib")
            .join("silver")
            .join(format!("{}.agm", target.name));
        if packaged_artifact.is_file() {
            return packaged_artifact;
        }
    }

    target.entry.clone()
}

#[derive(Debug, Clone)]
pub struct PackageError {
    message: String,
}

impl PackageError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for PackageError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for PackageError {}

pub fn parse_manifest(path: impl AsRef<Path>) -> Result<PackageManifest, PackageError> {
    let path = path.as_ref();
    let manifest_path = canonical_manifest_path(path)?;
    let source = fs::read_to_string(&manifest_path).map_err(|error| {
        PackageError::new(format!(
            "failed to read package manifest `{}`: {error}",
            manifest_path.display()
        ))
    })?;
    parse_manifest_text(&manifest_path, &source)
}

pub fn parse_manifest_text(
    manifest_path: impl AsRef<Path>,
    source: &str,
) -> Result<PackageManifest, PackageError> {
    let manifest_path = manifest_path.as_ref().to_path_buf();
    let value: Value = source.parse().map_err(|error| {
        PackageError::new(format!(
            "failed to parse package manifest `{}`: {error}",
            manifest_path.display()
        ))
    })?;
    let table = value.as_table().ok_or_else(|| {
        PackageError::new(format!(
            "package manifest `{}` must contain a TOML table",
            manifest_path.display()
        ))
    })?;

    validate_keys(
        table,
        &["name", "version", "url", "bin", "lib", "dependencies"],
        &manifest_path,
        "package manifest",
    )?;

    let name = required_string(table, "name", &manifest_path)?;
    let version = required_string(table, "version", &manifest_path)?;
    let url = optional_string(table, "url", &manifest_path)?;
    let mut targets = Vec::new();

    for (kind, field) in [(TargetKind::Bin, "bin"), (TargetKind::Lib, "lib")] {
        let Some(targets_table) = table.get(field) else {
            continue;
        };
        let targets_table = targets_table.as_table().ok_or_else(|| {
            PackageError::new(format!(
                "package manifest `{}`: `[{}]` must be a table",
                manifest_path.display(),
                field
            ))
        })?;
        for (target_name, target_value) in targets_table {
            if target_name.is_empty() {
                return Err(PackageError::new(format!(
                    "package manifest `{}`: [{}] contains an empty target name",
                    manifest_path.display(),
                    field
                )));
            }
            let target_table = target_value.as_table().ok_or_else(|| {
                PackageError::new(format!(
                    "package manifest `{}`: `[{}.{}]` must be a table",
                    manifest_path.display(),
                    field,
                    target_name
                ))
            })?;
            let source = parse_target_source(target_table, &manifest_path, kind, target_name)?;
            targets.push(ManifestTarget {
                name: target_name.to_string(),
                kind,
                source,
            });
        }
    }

    let mut dependencies = BTreeMap::new();
    if let Some(dependencies_value) = table.get("dependencies") {
        let dependencies_table = dependencies_value.as_table().ok_or_else(|| {
            PackageError::new(format!(
                "package manifest `{}`: `[dependencies]` must be a table",
                manifest_path.display()
            ))
        })?;
        for (dependency_name, dependency_value) in dependencies_table {
            if dependency_name.is_empty() {
                return Err(PackageError::new(format!(
                    "package manifest `{}`: dependencies cannot use an empty alias",
                    manifest_path.display()
                )));
            }
            let dependency_table = dependency_value.as_table().ok_or_else(|| {
                PackageError::new(format!(
                    "package manifest `{}`: dependency `{dependency_name}` must be a table",
                    manifest_path.display()
                ))
            })?;
            let source = parse_manifest_source(
                dependency_table,
                &manifest_path,
                &format!("dependency `{dependency_name}`"),
            )?;
            dependencies.insert(
                dependency_name.clone(),
                DependencySpec {
                    name: dependency_name.to_string(),
                    source,
                },
            );
        }
    }

    Ok(PackageManifest {
        manifest_path,
        name,
        version,
        url,
        targets,
        dependencies,
    })
}

fn required_string(
    table: &toml::Table,
    key: &str,
    manifest_path: &Path,
) -> Result<String, PackageError> {
    let Some(value) = table.get(key) else {
        return Err(PackageError::new(format!(
            "package manifest `{}` is missing required field `{key}`",
            manifest_path.display()
        )));
    };
    let Some(value) = value.as_str() else {
        return Err(PackageError::new(format!(
            "package manifest `{}` field `{key}` must be a string",
            manifest_path.display()
        )));
    };
    if value.is_empty() {
        return Err(PackageError::new(format!(
            "package manifest `{}` field `{key}` must not be empty",
            manifest_path.display()
        )));
    }
    Ok(value.to_string())
}

fn optional_string(
    table: &toml::Table,
    key: &str,
    manifest_path: &Path,
) -> Result<Option<String>, PackageError> {
    let Some(value) = table.get(key) else {
        return Ok(None);
    };
    let Some(value) = value.as_str() else {
        return Err(PackageError::new(format!(
            "package manifest `{}` field `{key}` must be a string",
            manifest_path.display()
        )));
    };
    if value.is_empty() {
        return Err(PackageError::new(format!(
            "package manifest `{}` field `{key}` must not be empty",
            manifest_path.display()
        )));
    }
    Ok(Some(value.to_string()))
}

fn parse_target_source(
    table: &toml::Table,
    manifest_path: &Path,
    kind: TargetKind,
    target_name: &str,
) -> Result<TargetSourceSpec, PackageError> {
    let label = format!("[{}.{}]", kind.label(), target_name);
    validate_keys(
        table,
        &["entry", "manifest", "branch", "tag", "rev"],
        manifest_path,
        &format!("target `{label}`"),
    )?;
    let has_entry = table.contains_key("entry");
    let has_manifest = table.contains_key("manifest");
    match (has_entry, has_manifest) {
        (true, true) | (false, false) => {
            return Err(PackageError::new(format!(
                "package manifest `{}`: target `{label}` must specify exactly one of `entry` or `manifest`",
                manifest_path.display()
            )));
        }
        _ => {}
    }

    if has_entry {
        let entry = string_field(table, "entry", manifest_path, &format!("target `{label}`"))?;
        let entry = relative_path(entry, manifest_path, &format!("target `{label}` entry"))?;
        if table
            .keys()
            .any(|key| matches!(key.as_str(), "branch" | "tag" | "rev"))
        {
            return Err(PackageError::new(format!(
                "package manifest `{}`: Git selectors are only valid with a `manifest` source for target `{label}`",
                manifest_path.display()
            )));
        }
        return Ok(TargetSourceSpec::Entry(entry));
    }

    let source = parse_manifest_source(table, manifest_path, &format!("target `{label}`"))?;
    Ok(TargetSourceSpec::Manifest(source))
}

fn parse_manifest_source(
    table: &toml::Table,
    manifest_path: &Path,
    label: &str,
) -> Result<ManifestSourceSpec, PackageError> {
    validate_keys(
        table,
        &["manifest", "branch", "tag", "rev"],
        manifest_path,
        label,
    )?;
    let manifest = string_field(table, "manifest", manifest_path, label)?;
    let selectors = parse_git_selectors(table, manifest_path, label)?;
    if let Some(url) = manifest.strip_prefix("git+") {
        if url.is_empty() {
            return Err(PackageError::new(format!(
                "package manifest `{}`: {label} has an empty Git URL",
                manifest_path.display()
            )));
        }
        return Ok(ManifestSourceSpec::Git(GitSourceSpec {
            url: canonical_git_url(url),
            selector: selectors,
        }));
    }

    if !matches!(selectors, GitSelector::Default) {
        return Err(PackageError::new(format!(
            "package manifest `{}`: {label} specifies Git selectors for a local manifest source",
            manifest_path.display()
        )));
    }
    Ok(ManifestSourceSpec::Local(relative_path(
        manifest,
        manifest_path,
        &format!("{label} manifest"),
    )?))
}

fn validate_keys(
    table: &toml::Table,
    allowed: &[&str],
    manifest_path: &Path,
    label: &str,
) -> Result<(), PackageError> {
    if let Some(key) = table.keys().find(|key| {
        !allowed
            .iter()
            .any(|allowed_key| *allowed_key == key.as_str())
    }) {
        return Err(PackageError::new(format!(
            "package manifest `{}`: {label} contains unsupported field `{key}`",
            manifest_path.display()
        )));
    }
    Ok(())
}

fn parse_git_selectors(
    table: &toml::Table,
    manifest_path: &Path,
    label: &str,
) -> Result<GitSelector, PackageError> {
    let mut values = Vec::new();
    for key in ["branch", "tag", "rev"] {
        if let Some(value) = table.get(key) {
            let Some(value) = value.as_str() else {
                return Err(PackageError::new(format!(
                    "package manifest `{}`: {label} field `{key}` must be a string",
                    manifest_path.display()
                )));
            };
            if value.is_empty() {
                return Err(PackageError::new(format!(
                    "package manifest `{}`: {label} field `{key}` must not be empty",
                    manifest_path.display()
                )));
            }
            if key == "rev"
                && (value.len() != 40 || !value.bytes().all(|byte| byte.is_ascii_hexdigit()))
            {
                return Err(PackageError::new(format!(
                    "package manifest `{}`: {label} field `rev` must be a full 40-character hexadecimal commit ID",
                    manifest_path.display()
                )));
            }
            values.push((key, value.to_string()));
        }
    }
    if values.len() > 1 {
        let names = values
            .iter()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>()
            .join(", ");
        return Err(PackageError::new(format!(
            "package manifest `{}`: {label} specifies multiple Git selectors ({names}); choose exactly one of `branch`, `tag`, or `rev`",
            manifest_path.display()
        )));
    }
    Ok(match values.pop() {
        Some(("branch", value)) => GitSelector::Branch(value),
        Some(("tag", value)) => GitSelector::Tag(value),
        Some(("rev", value)) => GitSelector::Rev(value),
        None => GitSelector::Default,
        Some((_, _)) => unreachable!("Git selector keys are exhaustive"),
    })
}

fn string_field(
    table: &toml::Table,
    key: &str,
    manifest_path: &Path,
    label: &str,
) -> Result<String, PackageError> {
    let Some(value) = table.get(key) else {
        return Err(PackageError::new(format!(
            "package manifest `{}`: {label} is missing `{key}`",
            manifest_path.display()
        )));
    };
    let Some(value) = value.as_str() else {
        return Err(PackageError::new(format!(
            "package manifest `{}`: {label} field `{key}` must be a string",
            manifest_path.display()
        )));
    };
    if value.is_empty() {
        return Err(PackageError::new(format!(
            "package manifest `{}`: {label} field `{key}` must not be empty",
            manifest_path.display()
        )));
    }
    Ok(value.to_string())
}

fn relative_path(
    value: String,
    manifest_path: &Path,
    label: &str,
) -> Result<PathBuf, PackageError> {
    let path = PathBuf::from(value);
    if path.is_absolute() {
        return Err(PackageError::new(format!(
            "package manifest `{}`: {label} must be relative to the manifest",
            manifest_path.display()
        )));
    }
    Ok(path)
}

fn canonical_manifest_path(path: &Path) -> Result<PathBuf, PackageError> {
    let candidate = if path.is_dir() {
        path.join(MANIFEST_FILE)
    } else {
        path.to_path_buf()
    };
    if !candidate.is_file() {
        return Err(PackageError::new(format!(
            "package manifest not found\n  path: {}",
            candidate.display()
        )));
    }
    fs::canonicalize(&candidate).map_err(|error| {
        PackageError::new(format!(
            "failed to canonicalize package manifest `{}`: {error}",
            candidate.display()
        ))
    })
}

fn resolve_manifest_path(base_dir: &Path, path: &Path) -> Result<PathBuf, PackageError> {
    let candidate = if path.file_name().and_then(|name| name.to_str()) == Some(MANIFEST_FILE) {
        base_dir.join(path)
    } else {
        base_dir.join(path).join(MANIFEST_FILE)
    };
    canonical_manifest_path(&candidate)
}

fn resolve_entry_path(
    base_dir: &Path,
    path: &Path,
    label: &str,
    allow_artifact: bool,
) -> Result<PathBuf, PackageError> {
    let candidate = base_dir.join(path);
    if !candidate.is_file() {
        let artifact = candidate.with_extension("agm");
        if allow_artifact && artifact.is_file() {
            return fs::canonicalize(&artifact)
                .map(|artifact| artifact.with_extension("ag"))
                .map_err(|error| {
                    PackageError::new(format!(
                        "failed to canonicalize target `{label}` artifact `{}`: {error}",
                        artifact.display()
                    ))
                });
        }
        return Err(PackageError::new(format!(
            "target `{label}` entry source not found\n  path: {}",
            candidate.display()
        )));
    }
    fs::canonicalize(&candidate).map_err(|error| {
        PackageError::new(format!(
            "failed to canonicalize target `{label}` entry `{}`: {error}",
            candidate.display()
        ))
    })
}

fn find_target<'a>(
    package: &'a Package,
    kind: TargetKind,
    requested_name: Option<&str>,
    preferred_name: Option<&str>,
) -> Result<&'a PackageTarget, String> {
    let available = package
        .targets
        .iter()
        .filter(|target| target.kind == kind)
        .map(|target| target.name.as_str())
        .collect::<Vec<_>>();

    if let Some(name) = requested_name {
        return package
            .targets
            .iter()
            .find(|target| target.kind == kind && target.name == name)
            .ok_or_else(|| {
                format!(
                    "package `{}` has no [{}.{name}] target (available: {})",
                    package.name,
                    kind.label(),
                    format_target_names(&available)
                )
            });
    }

    if let Some(name) = preferred_name
        && let Some(target) = package
            .targets
            .iter()
            .find(|target| target.kind == kind && target.name == name)
    {
        return Ok(target);
    }
    if let Some(target) = package
        .targets
        .iter()
        .find(|target| target.kind == kind && target.name == package.name)
    {
        return Ok(target);
    }

    let matches = package
        .targets
        .iter()
        .filter(|target| target.kind == kind)
        .collect::<Vec<_>>();
    match matches.as_slice() {
        [target] => Ok(target),
        [] => Err(format!(
            "package `{}` has no `{}` targets",
            package.name,
            kind.label()
        )),
        _ => Err(format!(
            "package `{}` has multiple `{}` targets; select one with `--{}` (available: {})",
            package.name,
            kind.label(),
            kind.label(),
            format_target_names(&available)
        )),
    }
}

fn format_target_names(names: &[&str]) -> String {
    if names.is_empty() {
        "none".to_string()
    } else {
        names.join(", ")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PackageKey {
    Local(PathBuf),
    Git {
        repository_url: String,
        resolved_commit: String,
        manifest_path_in_repository: PathBuf,
    },
}

#[derive(Debug, Clone, Copy)]
enum LoadState {
    Loading(PackageId),
    Loaded(PackageId),
}

#[derive(Debug, Clone)]
struct ResolvedManifest {
    manifest: PackageManifest,
    source: PackageSource,
    key: PackageKey,
}

#[derive(Debug, Clone)]
struct GitCheckout {
    repository_url: String,
    requested_selector: GitSelector,
    resolved_commit: String,
    root: PathBuf,
}

pub struct PackageResolver {
    git_cache: GitCache,
    states: HashMap<PackageKey, LoadState>,
    packages: Vec<Package>,
    stack: Vec<(PackageId, String)>,
}

impl fmt::Debug for PackageResolver {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("PackageResolver")
            .field("git_cache", &self.git_cache)
            .field("states", &self.states)
            .field("packages", &self.packages)
            .field("stack", &self.stack)
            .finish()
    }
}

impl PackageResolver {
    pub fn new() -> Result<Self, String> {
        Ok(Self::with_cache_root(default_cache_root()?))
    }

    pub fn with_cache_root(root: impl Into<PathBuf>) -> Self {
        Self {
            git_cache: GitCache::new(root.into()),
            states: HashMap::new(),
            packages: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn resolve(&mut self, root: impl AsRef<Path>) -> Result<PackageGraph, String> {
        // A resolver may be reused after a failed attempt; discard partial
        // graph nodes so a later resolution cannot expose stale packages.
        self.states.clear();
        self.packages.clear();
        self.stack.clear();
        let manifest_path =
            canonical_manifest_path(root.as_ref()).map_err(|error| error.to_string())?;
        let resolved = ResolvedManifest {
            manifest: parse_manifest(&manifest_path).map_err(|error| error.to_string())?,
            source: PackageSource::Local {
                manifest_path: manifest_path.clone(),
            },
            key: PackageKey::Local(manifest_path),
        };
        let root_id = self.load_package(resolved)?;
        Ok(PackageGraph {
            root: root_id,
            packages: self.packages.clone(),
        })
    }

    pub fn resolve_root(root: impl AsRef<Path>) -> Result<PackageGraph, String> {
        let mut resolver = Self::new()?;
        resolver.resolve(root)
    }

    fn load_package(&mut self, resolved: ResolvedManifest) -> Result<PackageId, String> {
        if let Some(state) = self.states.get(&resolved.key).copied() {
            return match state {
                LoadState::Loaded(id) => Ok(id),
                LoadState::Loading(id) => Err(self.cycle_error(id, &resolved.manifest.name)),
            };
        }

        let key = resolved.key.clone();
        let id = PackageId(self.packages.len());
        self.states.insert(key.clone(), LoadState::Loading(id));
        self.stack.push((id, resolved.manifest.name.clone()));
        self.packages.push(Package {
            id,
            name: resolved.manifest.name.clone(),
            version: resolved.manifest.version.clone(),
            url: resolved.manifest.url.clone(),
            manifest_path: resolved.manifest.manifest_path.clone(),
            source: resolved.source.clone(),
            targets: Vec::new(),
            dependencies: Vec::new(),
        });

        let result = self.load_package_contents(id, resolved);
        self.stack.pop();
        if result.is_ok() {
            self.states
                .insert(resolved_key(&self.packages[id.0]), LoadState::Loaded(id));
        } else {
            self.states.remove(&key);
        }
        result.map(|()| id)
    }

    fn load_package_contents(
        &mut self,
        id: PackageId,
        resolved: ResolvedManifest,
    ) -> Result<(), String> {
        let manifest_dir = resolved
            .manifest
            .manifest_path
            .parent()
            .ok_or_else(|| "package manifest has no parent directory".to_string())?
            .to_path_buf();

        let mut dependencies = Vec::new();
        for dependency in resolved.manifest.dependencies.values() {
            let child = self
                .resolve_source(&manifest_dir, &dependency.source)
                .and_then(|child| self.load_package(child))
                .map_err(|error| {
                    format!(
                        "failed to resolve dependency `{}` from `{}`:\n  {error}",
                        dependency.name,
                        resolved.manifest.manifest_path.display()
                    )
                })?;
            dependencies.push(PackageDependency {
                name: dependency.name.clone(),
                package: child,
            });
        }

        let mut targets = Vec::new();
        for target in &resolved.manifest.targets {
            let source = match &target.source {
                TargetSourceSpec::Entry(entry) => ResolvedTargetSource::Entry(
                    resolve_entry_path(
                        &manifest_dir,
                        entry,
                        &format!("{}.{}", target.kind.label(), target.name),
                        target.kind == TargetKind::Lib,
                    )
                    .map_err(|error| {
                        format!(
                            "{}\n  manifest: {}",
                            error,
                            resolved.manifest.manifest_path.display()
                        )
                    })?,
                ),
                TargetSourceSpec::Manifest(source) => {
                    let child = self
                        .resolve_source(&manifest_dir, source)
                        .and_then(|child| self.load_package(child))
                        .map_err(|error| {
                            format!(
                                "failed to resolve target `[{}.{}]` from `{}`:\n  {error}",
                                target.kind.label(),
                                target.name,
                                resolved.manifest.manifest_path.display()
                            )
                        })?;
                    ResolvedTargetSource::Manifest(child)
                }
            };
            targets.push(PackageTarget {
                name: target.name.clone(),
                kind: target.kind,
                source,
            });
        }

        let package = self
            .packages
            .get_mut(id.0)
            .ok_or_else(|| "package graph node disappeared during resolution".to_string())?;
        package.dependencies = dependencies;
        package.targets = targets;
        Ok(())
    }

    fn resolve_source(
        &mut self,
        base_dir: &Path,
        source: &ManifestSourceSpec,
    ) -> Result<ResolvedManifest, String> {
        match source {
            ManifestSourceSpec::Local(path) => {
                let manifest_path =
                    resolve_manifest_path(base_dir, path).map_err(|error| error.to_string())?;
                Ok(ResolvedManifest {
                    manifest: parse_manifest(&manifest_path).map_err(|error| error.to_string())?,
                    source: PackageSource::Local {
                        manifest_path: manifest_path.clone(),
                    },
                    key: PackageKey::Local(manifest_path),
                })
            }
            ManifestSourceSpec::Git(git) => {
                let checkout = self.git_cache.resolve(git)?;
                let manifest_path =
                    canonical_manifest_path(&checkout.root).map_err(|error| error.to_string())?;
                let manifest_path_in_repository = PathBuf::from(MANIFEST_FILE);
                Ok(ResolvedManifest {
                    manifest: parse_manifest(&manifest_path).map_err(|error| error.to_string())?,
                    source: PackageSource::Git {
                        repository_url: checkout.repository_url.clone(),
                        requested_selector: checkout.requested_selector.clone(),
                        resolved_commit: checkout.resolved_commit.clone(),
                        manifest_path: manifest_path.clone(),
                        manifest_path_in_repository: manifest_path_in_repository.clone(),
                    },
                    key: PackageKey::Git {
                        repository_url: checkout.repository_url,
                        resolved_commit: checkout.resolved_commit,
                        manifest_path_in_repository,
                    },
                })
            }
        }
    }

    fn cycle_error(&self, repeated: PackageId, current_name: &str) -> String {
        let start = self
            .stack
            .iter()
            .position(|(id, _)| *id == repeated)
            .unwrap_or(0);
        let mut chain = self.stack[start..]
            .iter()
            .map(|(_, name)| name.clone())
            .collect::<Vec<_>>();
        chain.push(current_name.to_string());
        format!("cyclic package dependency\n  {}", chain.join(" -> "))
    }
}

fn resolved_key(package: &Package) -> PackageKey {
    match &package.source {
        PackageSource::Local { manifest_path } => PackageKey::Local(manifest_path.clone()),
        PackageSource::Git {
            repository_url,
            resolved_commit,
            manifest_path_in_repository,
            ..
        } => PackageKey::Git {
            repository_url: repository_url.clone(),
            resolved_commit: resolved_commit.clone(),
            manifest_path_in_repository: manifest_path_in_repository.clone(),
        },
    }
}

#[derive(Debug, Clone)]
struct GitCache {
    root: PathBuf,
}

impl GitCache {
    fn new(root: PathBuf) -> Self {
        Self { root }
    }

    fn resolve(&self, source: &GitSourceSpec) -> Result<GitCheckout, String> {
        let repository_url = canonical_git_url(&source.url);
        let repository_hash = stable_hash(&repository_url);
        let repository_dir = self.root.join("git").join(repository_hash).join("repo.git");
        let parent = repository_dir
            .parent()
            .ok_or_else(|| "invalid Git cache path".to_string())?;
        fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create Silver Git cache at `{}`: {error}",
                parent.display()
            )
        })?;

        if !repository_dir.join("HEAD").is_file() {
            if repository_dir.exists() {
                fs::remove_dir_all(&repository_dir).map_err(|error| {
                    format!(
                        "failed to replace incomplete Git cache `{}`: {error}",
                        repository_dir.display()
                    )
                })?;
            }
            run_git(
                vec![
                    OsString::from("clone"),
                    OsString::from("--bare"),
                    OsString::from(&repository_url),
                    repository_dir.as_os_str().to_os_string(),
                ],
                &format!(
                    "clone Git dependency `{}`",
                    display_git_url(&repository_url)
                ),
            )?;
        } else {
            run_git(
                vec![
                    OsString::from("-C"),
                    repository_dir.as_os_str().to_os_string(),
                    OsString::from("fetch"),
                    OsString::from("--prune"),
                    OsString::from("--tags"),
                    OsString::from(&repository_url),
                    OsString::from("+refs/heads/*:refs/heads/*"),
                ],
                &format!(
                    "update Git dependency `{}`",
                    display_git_url(&repository_url)
                ),
            )?;
        }

        let resolved_commit =
            resolve_git_revision(&repository_dir, &repository_url, &source.selector)?;
        let tree_dir = repository_dir
            .parent()
            .ok_or_else(|| "invalid Git repository cache path".to_string())?
            .join("trees")
            .join(&resolved_commit);
        if !tree_dir.join(MANIFEST_FILE).is_file() {
            if tree_dir.exists() {
                fs::remove_dir_all(&tree_dir).map_err(|error| {
                    format!(
                        "failed to replace Git checkout `{}`: {error}",
                        tree_dir.display()
                    )
                })?;
            }
            if let Some(parent) = tree_dir.parent() {
                fs::create_dir_all(parent).map_err(|error| {
                    format!(
                        "failed to create Git checkout cache `{}`: {error}",
                        parent.display()
                    )
                })?;
            }
            run_git(
                vec![
                    OsString::from("--git-dir"),
                    repository_dir.as_os_str().to_os_string(),
                    OsString::from("worktree"),
                    OsString::from("add"),
                    OsString::from("--detach"),
                    tree_dir.as_os_str().to_os_string(),
                    OsString::from(&resolved_commit),
                ],
                &format!(
                    "materialize Git dependency `{}` at {}",
                    display_git_url(&repository_url),
                    resolved_commit
                ),
            )?;
        }

        if !tree_dir.join(MANIFEST_FILE).is_file() {
            return Err(format!(
                "failed to resolve Git dependency `{}`: selected revision `{resolved_commit}` does not contain `{MANIFEST_FILE}`",
                display_git_url(&repository_url)
            ));
        }

        Ok(GitCheckout {
            repository_url,
            requested_selector: source.selector.clone(),
            resolved_commit,
            root: tree_dir,
        })
    }
}

fn default_cache_root() -> Result<PathBuf, String> {
    if let Ok(value) = env::var("XDG_CACHE_HOME")
        && !value.is_empty()
    {
        return Ok(PathBuf::from(value).join("silver"));
    }
    let home = env::var_os("HOME").ok_or_else(|| {
        "cannot determine Silver cache directory: set XDG_CACHE_HOME or HOME".to_string()
    })?;
    Ok(fallback_cache_root(home))
}

fn fallback_cache_root(home: impl AsRef<Path>) -> PathBuf {
    home.as_ref().join(".local").join("cache").join("silver")
}

fn resolve_git_revision(
    repository_dir: &Path,
    repository_url: &str,
    selector: &GitSelector,
) -> Result<String, String> {
    let revision = match selector {
        GitSelector::Branch(branch) => format!("refs/heads/{branch}^{{commit}}"),
        GitSelector::Tag(tag) => format!("refs/tags/{tag}^{{commit}}"),
        GitSelector::Rev(rev) => format!("{rev}^{{commit}}"),
        GitSelector::Default => {
            if let Ok(output) = run_git_capture(
                vec![
                    OsString::from("ls-remote"),
                    OsString::from("--symref"),
                    OsString::from(repository_url),
                    OsString::from("HEAD"),
                ],
                "discover Git default branch",
            ) {
                output
                    .lines()
                    .find_map(|line| line.strip_prefix("ref: refs/heads/")?.split_once('\t'))
                    .map(|(branch, _)| format!("refs/heads/{branch}^{{commit}}"))
                    .unwrap_or_else(|| "HEAD^{commit}".to_string())
            } else {
                "HEAD^{commit}".to_string()
            }
        }
    };

    let resolved = run_git_capture(
        vec![
            OsString::from("-C"),
            repository_dir.as_os_str().to_os_string(),
            OsString::from("rev-parse"),
            OsString::from("--verify"),
            OsString::from(&revision),
        ],
        &format!("resolve Git {}", selector.description()),
    )
    .map_err(|error| {
        format!(
            "failed to resolve Git dependency `{}` ({})\n  reason: {error}",
            display_git_url(repository_url),
            selector.description()
        )
    })?;
    let resolved = resolved.trim().to_string();
    if resolved.is_empty() {
        return Err(format!(
            "failed to resolve Git dependency `{}` ({})\n  reason: Git returned an empty commit",
            display_git_url(repository_url),
            selector.description()
        ));
    }
    Ok(resolved)
}

fn run_git(args: Vec<OsString>, context: &str) -> Result<(), String> {
    run_git_capture(args, context).map(|_| ())
}

fn run_git_capture(args: Vec<OsString>, context: &str) -> Result<String, String> {
    let output = Command::new("git")
        .args(args)
        .output()
        .map_err(|error| format!("{context}: could not start Git: {error}"))?;
    if output.status.success() {
        return Ok(String::from_utf8_lossy(&output.stdout).into_owned());
    }
    let reason = String::from_utf8_lossy(&output.stderr).trim().to_string();
    let reason = if reason.is_empty() {
        format!("Git exited with status {}", output.status)
    } else {
        reason
    };
    Err(format!("{context}: {reason}"))
}

fn canonical_git_url(url: &str) -> String {
    url.trim().trim_end_matches('/').to_string()
}

fn display_git_url(url: &str) -> String {
    if let Some((prefix, rest)) = url.split_once("://")
        && let Some((_, host)) = rest.split_once('@')
    {
        return format!("{prefix}://***@{host}");
    }
    url.to_string()
}

fn stable_hash(value: &str) -> String {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in value.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{hash:016x}")
}

#[cfg(test)]
fn unique_temp_dir(label: &str) -> PathBuf {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    env::temp_dir().join(format!("agc-package-{label}-{nonce}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_manifest(root: &Path, source: &str) -> PathBuf {
        fs::create_dir_all(root).unwrap();
        let path = root.join(MANIFEST_FILE);
        fs::write(&path, source).unwrap();
        path
    }

    fn write_source(root: &Path, relative: &str) -> PathBuf {
        let path = root.join(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, "i32 main() { return 0; }\n").unwrap();
        path
    }

    #[test]
    fn default_cache_root_uses_local_cache_fallback() {
        assert_eq!(
            fallback_cache_root("/home/example"),
            PathBuf::from("/home/example/.local/cache/silver")
        );
    }

    fn git_command(repo: &Path, args: &[&str]) {
        let status = Command::new("git")
            .arg("-C")
            .arg(repo)
            .args(args)
            .status()
            .unwrap();
        assert!(status.success(), "git command failed: {args:?}");
    }

    #[test]
    fn parses_identity_targets_and_optional_url() {
        let root = unique_temp_dir("manifest");
        write_source(&root, "src/main.ag");
        write_source(&root, "src/http.ag");
        let manifest = write_manifest(
            &root,
            r#"
name = "hello"
version = "0.1.0"
url = "https://github.com/example/hello"

[bin.hello]
entry = "src/main.ag"

[lib.http]
entry = "src/http.ag"

[bin.tools]
manifest = "modules/tools"

[lib.std]
manifest = "modules/std"
"#,
        );

        let parsed = parse_manifest(&manifest).unwrap();
        assert_eq!(parsed.name, "hello");
        assert_eq!(parsed.version, "0.1.0");
        assert_eq!(
            parsed.url.as_deref(),
            Some("https://github.com/example/hello")
        );
        assert_eq!(parsed.targets.len(), 4);
        assert!(matches!(
            parsed
                .targets
                .iter()
                .find(|target| target.name == "hello")
                .unwrap()
                .source,
            TargetSourceSpec::Entry(_)
        ));
        assert!(matches!(
            parsed
                .targets
                .iter()
                .find(|target| target.name == "tools")
                .unwrap()
                .source,
            TargetSourceSpec::Manifest(ManifestSourceSpec::Local(_))
        ));

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn rejects_missing_identity_and_invalid_target_sources() {
        let root = unique_temp_dir("manifest-errors");
        let missing_name = write_manifest(&root, "version = \"0.1.0\"\n");
        assert!(
            parse_manifest(&missing_name)
                .unwrap_err()
                .to_string()
                .contains("name")
        );

        fs::write(
            &missing_name,
            "name = \"x\"\nversion = \"0.1.0\"\n[bin.app]\nentry = \"a.ag\"\nmanifest = \"b\"\n",
        )
        .unwrap();
        let error = parse_manifest(&missing_name).unwrap_err().to_string();
        assert!(error.contains("exactly one"), "{error}");

        fs::write(
            &missing_name,
            "name = \"x\"\nversion = \"0.1.0\"\n[bin.app]\n",
        )
        .unwrap();
        let error = parse_manifest(&missing_name).unwrap_err().to_string();
        assert!(error.contains("exactly one"), "{error}");

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn artifact_only_library_dependencies_do_not_mask_binary_sources() {
        let root = unique_temp_dir("artifact-boundary");
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("src/main.agm"), b"prebuilt").unwrap();
        let manifest = write_manifest(
            &root,
            "name = \"app\"\nversion = \"0.1.0\"\n[bin.app]\nentry = \"src/main.ag\"\n",
        );
        let mut resolver = PackageResolver::with_cache_root(root.join("cache"));
        let error = resolver.resolve(&manifest).unwrap_err();
        assert!(
            error.contains("target `bin.app` entry source not found"),
            "{error}"
        );

        fs::write(
            &manifest,
            "name = \"app\"\nversion = \"0.1.0\"\n[lib.app]\nentry = \"src/main.ag\"\n",
        )
        .unwrap();
        let graph = resolver.resolve(&manifest).unwrap();
        let error = graph
            .select_target(&TargetSelection::lib(None))
            .unwrap_err();
        assert!(error.contains("prebuilt `.agm` files can only satisfy library dependencies"));
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn rejects_unsupported_manifest_fields_without_panicking() {
        let root = unique_temp_dir("manifest-unknown");
        let manifest = write_manifest(&root, "name = \"x\"\nversion = \"0.1.0\"\nunknown = true\n");
        let error = parse_manifest(&manifest).unwrap_err().to_string();
        assert!(error.contains("unsupported field `unknown`"), "{error}");
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn rejects_multiple_git_selectors() {
        let root = unique_temp_dir("selectors");
        let manifest = write_manifest(
            &root,
            "name = \"x\"\nversion = \"0.1.0\"\n[dependencies.foo]\nmanifest = \"git+https://example.invalid/foo.git\"\nbranch = \"main\"\ntag = \"v1\"\n",
        );
        let error = parse_manifest(&manifest).unwrap_err().to_string();
        assert!(error.contains("multiple Git selectors"), "{error}");
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn rejects_symbolic_git_revisions() {
        let root = unique_temp_dir("symbolic-rev");
        let manifest = write_manifest(
            &root,
            "name = \"x\"\nversion = \"0.1.0\"\n[dependencies.foo]\nmanifest = \"git+https://example.invalid/foo.git\"\nrev = \"HEAD~1\"\n",
        );
        let error = parse_manifest(&manifest).unwrap_err().to_string();
        assert!(
            error.contains("full 40-character hexadecimal commit ID"),
            "{error}"
        );
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn resolves_dependency_library_imports_to_the_declared_target() {
        let root = unique_temp_dir("dependency-import");
        write_source(&root, "src/main.ag");
        let dependency = root.join("dependency");
        write_source(&dependency, "src/library.ag");
        write_manifest(
            &root,
            "name = \"app\"\nversion = \"0.1.0\"\n[bin.app]\nentry = \"src/main.ag\"\n[dependencies.widget]\nmanifest = \"dependency\"\n",
        );
        write_manifest(
            &dependency,
            "name = \"widget-package\"\nversion = \"0.1.0\"\n[lib.widget]\nentry = \"src/library.ag\"\n",
        );

        let mut resolver = PackageResolver::with_cache_root(root.join("cache"));
        let graph = resolver.resolve(&root).unwrap();
        let imports = graph.dependency_imports().unwrap();
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].name, "widget");
        assert!(imports[0].source.ends_with("src/library.ag"));
        assert_eq!(imports[0].target.kind, TargetKind::Lib);

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn resolves_recursive_local_packages_and_deduplicates_references() {
        let root = unique_temp_dir("recursive");
        let child = root.join("modules").join("child");
        let leaf = root.join("modules").join("leaf");
        write_source(&root, "src/main.ag");
        write_source(&child, "src/child.ag");
        write_source(&leaf, "src/leaf.ag");
        write_manifest(
            &root,
            "name = \"root\"\nversion = \"0.1.0\"\n[bin.app]\nentry = \"src/main.ag\"\n[dependencies.child]\nmanifest = \"modules/child\"\n[dependencies.child_again]\nmanifest = \"modules/child\"\n",
        );
        write_manifest(
            &child,
            "name = \"child\"\nversion = \"0.1.0\"\n[bin.child]\nentry = \"src/child.ag\"\n[dependencies.leaf]\nmanifest = \"../leaf\"\n",
        );
        write_manifest(
            &leaf,
            "name = \"leaf\"\nversion = \"0.1.0\"\n[lib.leaf]\nentry = \"src/leaf.ag\"\n",
        );

        let mut resolver = PackageResolver::with_cache_root(root.join("cache"));
        let graph = resolver.resolve(&root).unwrap();
        assert_eq!(graph.packages().len(), 3);
        assert_eq!(graph.package_roots().len(), 3);
        let target = graph
            .select_target(&TargetSelection::bin(Some("app".to_string())))
            .unwrap();
        assert!(target.entry.ends_with("src/main.ag"));

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn permits_same_dependency_alias_in_distinct_package_scopes() {
        let root = unique_temp_dir("scoped-alias");
        let left = root.join("left");
        let right = root.join("right");
        let left_common = left.join("common");
        let right_common = right.join("common");
        write_source(&root, "src/main.ag");
        write_source(&left, "src/left.ag");
        write_source(&right, "src/right.ag");
        write_source(&left_common, "src/common.ag");
        write_source(&right_common, "src/common.ag");
        write_manifest(
            &root,
            "name = \"root\"\nversion = \"0.1.0\"\n[bin.root]\nentry = \"src/main.ag\"\n[dependencies.left]\nmanifest = \"left\"\n[dependencies.right]\nmanifest = \"right\"\n",
        );
        write_manifest(
            &left,
            "name = \"left\"\nversion = \"0.1.0\"\n[bin.left]\nentry = \"src/left.ag\"\n[dependencies.common]\nmanifest = \"common\"\n",
        );
        write_manifest(
            &right,
            "name = \"right\"\nversion = \"0.1.0\"\n[bin.right]\nentry = \"src/right.ag\"\n[dependencies.common]\nmanifest = \"common\"\n",
        );
        write_manifest(
            &left_common,
            "name = \"left-common\"\nversion = \"0.1.0\"\n[lib.left-common]\nentry = \"src/common.ag\"\n",
        );
        write_manifest(
            &right_common,
            "name = \"right-common\"\nversion = \"0.1.0\"\n[lib.right-common]\nentry = \"src/common.ag\"\n",
        );

        let mut resolver = PackageResolver::with_cache_root(root.join("cache"));
        let graph = resolver.resolve(&root).unwrap();
        let imports = graph.dependency_imports().unwrap();
        let scoped = imports
            .iter()
            .filter(|import| import.name == "common")
            .collect::<Vec<_>>();
        assert_eq!(scoped.len(), 2);
        assert_ne!(scoped[0].owner_root, scoped[1].owner_root);

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn detects_local_dependency_cycles() {
        let root = unique_temp_dir("cycle");
        let a = root.join("a");
        let b = root.join("b");
        write_manifest(
            &a,
            "name = \"a\"\nversion = \"0.1.0\"\n[dependencies.b]\nmanifest = \"../b\"\n",
        );
        write_manifest(
            &b,
            "name = \"b\"\nversion = \"0.1.0\"\n[dependencies.a]\nmanifest = \"../a\"\n",
        );
        let mut resolver = PackageResolver::with_cache_root(root.join("cache"));
        let error = resolver.resolve(&a).unwrap_err();
        assert!(error.contains("cyclic package dependency"), "{error}");
        assert!(error.contains("a -> b -> a"), "{error}");
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn reusing_resolver_after_failure_discards_partial_nodes() {
        let root = unique_temp_dir("resolver-reuse");
        write_source(&root, "src/main.ag");
        let manifest = write_manifest(
            &root,
            "name = \"root\"\nversion = \"0.1.0\"\n[bin.root]\nentry = \"src/main.ag\"\n[dependencies.missing]\nmanifest = \"missing\"\n",
        );
        let mut resolver = PackageResolver::with_cache_root(root.join("cache"));
        assert!(resolver.resolve(&manifest).is_err());

        fs::write(
            &manifest,
            "name = \"root\"\nversion = \"0.1.0\"\n[bin.root]\nentry = \"src/main.ag\"\n",
        )
        .unwrap();
        let graph = resolver.resolve(&manifest).unwrap();
        assert_eq!(graph.packages().len(), 1);
        assert_eq!(graph.root(), PackageId(0));
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn resolves_git_revision_and_reuses_repository_cache() {
        let root = unique_temp_dir("git");
        let repository = root.join("repository");
        let project = root.join("project");
        fs::create_dir_all(&repository).unwrap();
        git_command(&repository, &["init", "-b", "main"]);
        git_command(&repository, &["config", "user.email", "test@example.com"]);
        git_command(&repository, &["config", "user.name", "Silver Tests"]);
        write_source(&repository, "src/main.ag");
        write_manifest(
            &repository,
            "name = \"git-child\"\nversion = \"0.1.0\"\n[bin.git-child]\nentry = \"src/main.ag\"\n",
        );
        git_command(&repository, &["add", "."]);
        git_command(&repository, &["commit", "-m", "initial"]);
        let revision = String::from_utf8(
            Command::new("git")
                .arg("-C")
                .arg(&repository)
                .args(["rev-parse", "HEAD"])
                .output()
                .unwrap()
                .stdout,
        )
        .unwrap()
        .trim()
        .to_string();

        write_manifest(
            &project,
            &format!(
                "name = \"root\"\nversion = \"0.1.0\"\n[dependencies.git-child]\nmanifest = \"git+file://{}\"\nrev = \"{}\"\n",
                repository.display(),
                revision
            ),
        );
        let cache = root.join("cache");
        let mut resolver = PackageResolver::with_cache_root(&cache);
        let graph = resolver.resolve(&project).unwrap();
        let git_package = graph
            .packages()
            .iter()
            .find(|package| package.name == "git-child")
            .unwrap();
        assert!(matches!(
            git_package.source,
            PackageSource::Git {
                ref resolved_commit,
                ..
            } if resolved_commit == &revision
        ));
        let cache_entries = fs::read_dir(cache.join("git")).unwrap().count();
        assert_eq!(cache_entries, 1);

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn redacts_git_url_userinfo_in_diagnostics() {
        assert_eq!(
            display_git_url("https://token@example.com/repo.git"),
            "https://***@example.com/repo.git"
        );
        assert_eq!(
            display_git_url("https://user:password@example.com/repo.git"),
            "https://***@example.com/repo.git"
        );
    }

    #[test]
    fn initializes_a_package_from_the_directory_name() {
        let root = unique_temp_dir("init-default").join("demo");
        let initialized = initialize_package(&root, None).unwrap();

        assert_eq!(initialized.name, "demo");
        assert!(initialized.manifest_path.is_file());
        assert_eq!(
            fs::read_to_string(&initialized.entry_path).unwrap(),
            DEFAULT_ENTRY_SOURCE
        );
        let manifest = fs::read_to_string(&initialized.manifest_path).unwrap();
        assert!(manifest.contains("name = \"demo\""));
        assert!(manifest.contains("[bin.\"demo\"]"));
        assert_eq!(
            parse_manifest(&initialized.manifest_path).unwrap().name,
            "demo"
        );

        if is_git_in_path() {
            assert!(root.join(".git").is_dir());
        }

        let error = initialize_package(&root, None).unwrap_err();
        assert!(error.contains("already exists"), "{error}");
        let _ = fs::remove_dir_all(root.parent().unwrap());
    }

    #[test]
    fn initializes_a_package_with_a_name_override_and_preserves_existing_entry() {
        let root = unique_temp_dir("init-override");
        let entry = root.join(DEFAULT_ENTRY);
        fs::create_dir_all(entry.parent().unwrap()).unwrap();
        fs::write(&entry, "i32 main() { return 7; }\n").unwrap();

        let initialized = initialize_package(&root, Some("custom-name")).unwrap();
        assert_eq!(initialized.name, "custom-name");
        assert_eq!(
            fs::read_to_string(entry).unwrap(),
            "i32 main() { return 7; }\n"
        );
        assert!(
            fs::read_to_string(initialized.manifest_path)
                .unwrap()
                .contains("[bin.\"custom-name\"]")
        );
        if is_git_in_path() {
            assert!(root.join(".git").is_dir());
        }
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn preserves_existing_git_repository() {
        let root = unique_temp_dir("init-preserve-git");
        let git_dir = root.join(".git");
        fs::create_dir_all(&git_dir).unwrap();
        let marker = git_dir.join("marker");
        fs::write(&marker, "keep-me").unwrap();

        let initialized = initialize_package(&root, None).unwrap();
        assert!(initialized.manifest_path.is_file());
        assert_eq!(fs::read_to_string(&marker).unwrap(), "keep-me");
        let _ = fs::remove_dir_all(root);
    }

    #[cfg(unix)]
    #[test]
    fn rolls_back_generated_files_when_entry_creation_fails() {
        use std::os::unix::fs::symlink;

        let root = unique_temp_dir("init-rollback");
        let entry = root.join(DEFAULT_ENTRY);
        fs::create_dir_all(entry.parent().unwrap()).unwrap();
        symlink(root.join("missing").join("entry-target"), &entry).unwrap();

        let error = initialize_package(&root, None).unwrap_err();
        assert!(error.contains("failed to write package entry"), "{error}");
        assert!(!root.join(MANIFEST_FILE).exists());
        assert!(!root.join(".git").exists());
        assert!(fs::symlink_metadata(&entry).is_err());

        let initialized = initialize_package(&root, None).unwrap();
        assert!(initialized.manifest_path.is_file());
        assert!(initialized.entry_path.is_file());
        if is_git_in_path() {
            assert!(root.join(".git").is_dir());
        }
        let _ = fs::remove_dir_all(root);
    }
}
