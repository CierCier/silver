// Runs the integration suite in a real VS Code build. Set VSCODE_EXECUTABLE to
// use an installed editor (for example VSCodium); otherwise the runner
// downloads one.
const path = require("node:path");
const { runTests } = require("@vscode/test-electron");

async function main() {
  const extensionDevelopmentPath = path.resolve(__dirname, "../..");
  const workspace = path.resolve(__dirname, "workspace");
  await runTests({
    extensionDevelopmentPath,
    extensionTestsPath: path.resolve(__dirname, "suite.js"),
    launchArgs: [workspace, "--disable-extensions", "--disable-workspace-trust"],
    ...(process.env.VSCODE_EXECUTABLE ? { vscodeExecutablePath: process.env.VSCODE_EXECUTABLE } : {}),
  });
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
