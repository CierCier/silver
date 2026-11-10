// Runs the integration suite in a real VS Code build. Set VSCODE_EXECUTABLE to
// use an installed editor (for example VSCodium); otherwise the runner
// downloads one.
const path = require("node:path");
const { runTests } = require("@vscode/test-electron");

async function main() {
  const extensionDevelopmentPath = path.resolve(__dirname, "../..");
  const workspace = path.resolve(__dirname, "workspace");
  const editor = process.env.VSCODE_EXECUTABLE ? { vscodeExecutablePath: process.env.VSCODE_EXECUTABLE } : {};
  const launchArgs = [workspace, "--disable-extensions", "--disable-workspace-trust"];
  // With aglsp on PATH: the server answers requests.
  await runTests({
    ...editor,
    extensionDevelopmentPath,
    extensionTestsPath: path.resolve(__dirname, "suite.js"),
    launchArgs,
  });
  // Without aglsp: activation still completes.
  await runTests({
    ...editor,
    extensionDevelopmentPath,
    extensionTestsPath: path.resolve(__dirname, "missing-server.js"),
    extensionTestsEnv: { PATH: "/nonexistent" },
    launchArgs,
  });
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
