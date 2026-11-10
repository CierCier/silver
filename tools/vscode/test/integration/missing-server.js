// With no aglsp on PATH, the extension must still finish activating: the
// "not found" prompt is shown without waiting for the user to dismiss it.
const assert = require("node:assert/strict");
const path = require("node:path");
const vscode = require("vscode");

async function run() {
  const file = vscode.Uri.file(path.resolve(__dirname, "workspace", "main.ag"));
  await vscode.window.showTextDocument(await vscode.workspace.openTextDocument(file));
  const extension = vscode.extensions.getExtension("ciercier.silver");
  assert.ok(extension, "extension is installed");
  const deadline = Date.now() + 60000;
  while (!extension.isActive && Date.now() < deadline) {
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
  assert.ok(extension.isActive, "extension activates without aglsp");
  // Restarting must not hang on the prompt either.
  await vscode.commands.executeCommand("silver.restartServer");
  console.log("activated without aglsp; restart returned");
}

module.exports = { run };
