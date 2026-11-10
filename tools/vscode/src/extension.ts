// VS Code client for aglsp, the Silver language server that ships with the
// Silver toolchain. The extension never downloads or bundles a server.
import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { LanguageClient, type LanguageClientOptions, type ServerOptions } from "vscode-languageclient/node";

const SERVER_BINARY = process.platform === "win32" ? "aglsp.exe" : "aglsp";
// Settings edits arrive per keystroke; restart once they settle.
const SETTINGS_RESTART_DELAY_MS = 500;

let client: LanguageClient | undefined;
let settingsRestart: ReturnType<typeof setTimeout> | undefined;
// Starts and stops run one at a time, so overlapping restarts cannot leave a
// second server running.
let lifecycle: Promise<void> = Promise.resolve();

function serialize(step: () => Promise<void>): Promise<void> {
  lifecycle = lifecycle.then(step, step);
  return lifecycle;
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const output = vscode.window.createOutputChannel("Silver Language Server", { log: true });
  context.subscriptions.push(
    output,
    vscode.commands.registerCommand("silver.restartServer", () => restart(output)),
    vscode.commands.registerCommand("silver.showServerOutput", () => output.show()),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration("silver.server.path") || event.affectsConfiguration("silver.sysroot")) {
        clearTimeout(settingsRestart);
        settingsRestart = setTimeout(() => void restart(output), SETTINGS_RESTART_DELAY_MS);
      }
    }),
    { dispose: () => clearTimeout(settingsRestart) },
    // A workspace that becomes trusted can now run the server.
    vscode.workspace.onDidGrantWorkspaceTrust(() => restart(output)),
  );
  await serialize(() => start(output));
}

export function deactivate(): Promise<void> {
  return serialize(stop);
}

function restart(output: vscode.LogOutputChannel): Promise<void> {
  return serialize(async () => {
    await stop();
    await start(output);
  });
}

async function stop(): Promise<void> {
  const running = client;
  client = undefined;
  await running?.dispose();
}

async function start(output: vscode.LogOutputChannel): Promise<void> {
  if (!vscode.workspace.isTrusted) {
    output.info("Workspace is not trusted; aglsp is not started.");
    return;
  }
  const config = vscode.workspace.getConfiguration("silver");
  const command = resolveServer(config.get<string>("server.path", ""));
  if (command === undefined) {
    // Not awaited: the prompt resolves only when dismissed, and activation and
    // later restarts must not wait on it.
    void vscode.window
      .showErrorMessage(
        "Silver: `aglsp` was not found. Install the Silver toolchain or set `silver.server.path`.",
        "Open Settings",
      )
      .then((choice) => {
        if (choice === "Open Settings") {
          void vscode.commands.executeCommand("workbench.action.openSettings", "silver.server.path");
        }
      });
    return;
  }

  const env: NodeJS.ProcessEnv = { ...process.env };
  const sysroot = config.get<string>("sysroot", "").trim();
  if (sysroot.length > 0) {
    env.SILVER_SYSROOT = expandHome(sysroot);
  }

  // No `transport`: the client then uses stdio without appending `--stdio`,
  // which aglsp does not accept.
  const serverOptions: ServerOptions = { command, options: { env } };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "silver" }],
    outputChannel: output,
  };

  const created = new LanguageClient("silver", "Silver Language Server", serverOptions, clientOptions);
  client = created;
  try {
    await created.start();
  } catch (error) {
    output.error(`Failed to start ${command}: ${String(error)}`);
    void vscode.window.showErrorMessage(`Silver: failed to start ${command}. See the output channel for details.`);
    if (client === created) client = undefined;
    await created.dispose().catch(() => undefined);
  }
}

/** The configured server path, or `aglsp` found on PATH. */
function resolveServer(configured: string): string | undefined {
  const trimmed = configured.trim();
  if (trimmed.length > 0) {
    const expanded = expandHome(trimmed);
    return isExecutable(expanded) ? expanded : undefined;
  }
  for (const directory of (process.env.PATH ?? "").split(path.delimiter)) {
    if (directory.length === 0) continue;
    const candidate = path.join(directory, SERVER_BINARY);
    if (isExecutable(candidate)) return candidate;
  }
  return undefined;
}

function isExecutable(file: string): boolean {
  try {
    fs.accessSync(file, fs.constants.X_OK);
    return fs.statSync(file).isFile();
  } catch {
    return false;
  }
}

function expandHome(file: string): string {
  if (file === "~" || file.startsWith("~/")) {
    return path.join(process.env.HOME ?? "", file.slice(1));
  }
  return file;
}
