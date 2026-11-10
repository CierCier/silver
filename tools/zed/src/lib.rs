//! Zed extension for Silver. It starts the `aglsp` language server that ships
//! with the Silver toolchain; it never downloads or bundles a server.

use zed_extension_api::{
    self as zed, serde_json::Value, settings::LspSettings, LanguageServerId, Result,
};

const SERVER_BINARY: &str = "aglsp";

struct SilverExtension;

impl zed::Extension for SilverExtension {
    fn new() -> Self {
        Self
    }

    /// Uses `lsp.aglsp.binary` from the user's settings when set, otherwise the
    /// `aglsp` found on the worktree's `PATH`.
    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let binary = LspSettings::for_worktree(language_server_id.as_ref(), worktree)?.binary;
        let (path, arguments, env) = match binary {
            Some(binary) => (binary.path, binary.arguments, binary.env),
            None => (None, None, None),
        };

        let command = path
            .or_else(|| worktree.which(SERVER_BINARY))
            .ok_or_else(|| {
                format!(
                    "`{SERVER_BINARY}` was not found on PATH. Install the Silver toolchain \
                     (scripts/install.sh) or set `lsp.{SERVER_BINARY}.binary.path` in settings."
                )
            })?;

        Ok(zed::Command {
            command,
            args: arguments.unwrap_or_default(),
            // The shell environment carries SILVER_SYSROOT to the server.
            env: match env {
                Some(env) => env.into_iter().collect(),
                None => worktree.shell_env(),
            },
        })
    }

    fn language_server_initialization_options(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<Option<Value>> {
        Ok(LspSettings::for_worktree(language_server_id.as_ref(), worktree)?.initialization_options)
    }

    fn language_server_workspace_configuration(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<Option<Value>> {
        Ok(LspSettings::for_worktree(language_server_id.as_ref(), worktree)?.settings)
    }
}

zed::register_extension!(SilverExtension);
