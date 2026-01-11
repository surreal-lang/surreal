//! Project configuration for Dream projects.
//!
//! Handles parsing of `dream.toml` manifest files and project discovery.

use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Error type for configuration operations.
#[derive(Debug)]
pub struct ConfigError {
    pub message: String,
}

impl ConfigError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ConfigError {}

/// Result type for configuration operations.
pub type ConfigResult<T> = Result<T, ConfigError>;

/// The parsed dream.toml configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct ProjectConfig {
    pub package: Package,
    #[serde(default)]
    pub application: Option<ApplicationConfig>,
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

/// A dependency specification.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    /// Simple version string: `jason = "1.4"`
    Version(String),
    /// Detailed specification
    Detailed(DependencySpec),
}

/// Detailed dependency specification.
#[derive(Debug, Clone, Deserialize)]
pub struct DependencySpec {
    /// Version requirement (for hex deps)
    pub version: Option<String>,
    /// Git repository URL
    pub git: Option<String>,
    /// Git branch
    pub branch: Option<String>,
    /// Git tag
    pub tag: Option<String>,
    /// Git commit ref
    #[serde(rename = "ref")]
    pub git_ref: Option<String>,
    /// Local path
    pub path: Option<String>,
    /// Whether this is a hex.pm package (default: true if version specified)
    #[serde(default)]
    pub hex: bool,
}

impl Dependency {
    /// Get the version requirement if this is a hex dependency.
    pub fn version(&self) -> Option<&str> {
        match self {
            Dependency::Version(v) => Some(v),
            Dependency::Detailed(spec) => spec.version.as_deref(),
        }
    }

    /// Check if this is a git dependency.
    pub fn is_git(&self) -> bool {
        matches!(self, Dependency::Detailed(spec) if spec.git.is_some())
    }

    /// Check if this is a path dependency.
    pub fn is_path(&self) -> bool {
        matches!(self, Dependency::Detailed(spec) if spec.path.is_some())
    }

    /// Get the git URL if this is a git dependency.
    pub fn git_url(&self) -> Option<&str> {
        match self {
            Dependency::Detailed(spec) => spec.git.as_deref(),
            _ => None,
        }
    }

    /// Get the path if this is a path dependency.
    pub fn path(&self) -> Option<&str> {
        match self {
            Dependency::Detailed(spec) => spec.path.as_deref(),
            _ => None,
        }
    }
}

/// Application configuration from dream.toml's [application] section.
#[derive(Debug, Clone, Deserialize)]
pub struct ApplicationConfig {
    /// The module containing the Application trait implementation.
    /// Defaults to package name if not specified.
    #[serde(rename = "mod")]
    pub module: Option<String>,
    /// Static environment variables for the application.
    #[serde(default)]
    pub env: HashMap<String, toml::Value>,
}

/// Package metadata from dream.toml.
#[derive(Debug, Clone, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    #[serde(default = "default_src_dir")]
    pub src: String,
}

fn default_src_dir() -> String {
    "src".to_string()
}

impl ProjectConfig {
    /// Load configuration from a dream.toml file.
    pub fn load(path: &Path) -> ConfigResult<Self> {
        let content = fs::read_to_string(path).map_err(|e| {
            ConfigError::new(format!("Failed to read {}: {}", path.display(), e))
        })?;

        toml::from_str(&content).map_err(|e| {
            ConfigError::new(format!("Failed to parse {}: {}", path.display(), e))
        })
    }

    /// Find the project root by looking for dream.toml in current and parent directories.
    pub fn find_project_root() -> Option<PathBuf> {
        let mut current = std::env::current_dir().ok()?;

        loop {
            let config_path = current.join("dream.toml");
            if config_path.exists() {
                return Some(current);
            }

            if !current.pop() {
                return None;
            }
        }
    }

    /// Load configuration from the project root.
    pub fn from_project_root() -> ConfigResult<(PathBuf, Self)> {
        let root = Self::find_project_root().ok_or_else(|| {
            ConfigError::new("Could not find dream.toml in current or parent directories")
        })?;

        let config_path = root.join("dream.toml");
        let config = Self::load(&config_path)?;

        Ok((root, config))
    }

    /// Get the source directory path relative to project root.
    pub fn src_dir(&self, project_root: &Path) -> PathBuf {
        project_root.join(&self.package.src)
    }

    /// Get the build directory path relative to project root.
    pub fn build_dir(&self, project_root: &Path) -> PathBuf {
        project_root.join("_build")
    }

    /// Get the BEAM output directory following Elixir's structure:
    /// `_build/{env}/lib/{app}/ebin/`
    pub fn beam_dir(&self, project_root: &Path) -> PathBuf {
        self.beam_dir_for_env(project_root, "dev")
    }

    /// Get the BEAM output directory for a specific environment.
    pub fn beam_dir_for_env(&self, project_root: &Path, env: &str) -> PathBuf {
        self.build_dir(project_root)
            .join(env)
            .join("lib")
            .join(&self.package.name)
            .join("ebin")
    }

    /// Check if this project is configured as an application.
    pub fn is_application(&self) -> bool {
        self.application.is_some()
    }

    /// Get the application module name.
    /// Returns the configured module or defaults to package name.
    pub fn application_module(&self) -> Option<String> {
        self.application.as_ref().map(|app| {
            app.module
                .clone()
                .unwrap_or_else(|| self.package.name.clone())
        })
    }

    /// Get the application environment configuration.
    pub fn application_env(&self) -> HashMap<String, toml::Value> {
        self.application
            .as_ref()
            .map(|app| app.env.clone())
            .unwrap_or_default()
    }
}

/// Generate a default dream.toml content for a new project.
pub fn generate_dream_toml(name: &str) -> String {
    format!(
        r#"[package]
name = "{}"
version = "0.1.0"
"#,
        name
    )
}

/// Generate a default main.dream content for a new project.
pub fn generate_main_dream(name: &str) -> String {
    format!(
        r#"// {} - A Dream project

pub fn main() {{
    :ok
}}
"#,
        name
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_dream_toml() {
        let content = r#"
[package]
name = "test_project"
version = "0.1.0"
"#;
        let config: ProjectConfig = toml::from_str(content).unwrap();
        assert_eq!(config.package.name, "test_project");
        assert_eq!(config.package.version, "0.1.0");
        assert_eq!(config.package.src, "src");
    }

    #[test]
    fn test_parse_dream_toml_custom_src() {
        let content = r#"
[package]
name = "custom"
version = "1.0.0"
src = "lib"
"#;
        let config: ProjectConfig = toml::from_str(content).unwrap();
        assert_eq!(config.package.src, "lib");
    }

    #[test]
    fn test_generate_dream_toml() {
        let content = generate_dream_toml("my_app");
        assert!(content.contains("name = \"my_app\""));
        assert!(content.contains("version = \"0.1.0\""));
    }

    #[test]
    fn test_parse_application_config() {
        let content = r#"
[package]
name = "my_app"
version = "0.1.0"

[application]
mod = "my_app_server"
env = { port = 8080, debug = true }
"#;
        let config: ProjectConfig = toml::from_str(content).unwrap();
        assert!(config.is_application());
        assert_eq!(config.application_module(), Some("my_app_server".to_string()));

        let env = config.application_env();
        assert_eq!(env.get("port"), Some(&toml::Value::Integer(8080)));
        assert_eq!(env.get("debug"), Some(&toml::Value::Boolean(true)));
    }

    #[test]
    fn test_application_module_defaults_to_package_name() {
        let content = r#"
[package]
name = "my_app"
version = "0.1.0"

[application]
env = { port = 3000 }
"#;
        let config: ProjectConfig = toml::from_str(content).unwrap();
        assert!(config.is_application());
        assert_eq!(config.application_module(), Some("my_app".to_string()));
    }

    #[test]
    fn test_no_application_section() {
        let content = r#"
[package]
name = "my_lib"
version = "0.1.0"
"#;
        let config: ProjectConfig = toml::from_str(content).unwrap();
        assert!(!config.is_application());
        assert_eq!(config.application_module(), None);
    }
}
