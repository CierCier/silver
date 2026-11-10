// Opens a Silver file and checks that aglsp answers through the extension.
const assert = require("node:assert/strict");
const path = require("node:path");
const vscode = require("vscode");

/** Retries `probe` until it returns a truthy value or the timeout passes. */
async function eventually(probe, timeoutMs, what) {
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const value = await probe();
    if (value) return value;
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
}

async function run() {
  const file = vscode.Uri.file(path.resolve(__dirname, "workspace", "main.ag"));
  const document = await vscode.workspace.openTextDocument(file);
  await vscode.window.showTextDocument(document);
  assert.equal(document.languageId, "silver", "`.ag` files use the Silver language");

  const extension = vscode.extensions.getExtension("ciercier.silver");
  assert.ok(extension, "extension is installed");
  // The first launch also initializes the test profile, so allow a slow start.
  await eventually(() => extension.isActive, 60000, "extension activation");

  const symbols = await eventually(async () => {
    const result = await vscode.commands.executeCommand("vscode.executeDocumentSymbolProvider", file);
    return Array.isArray(result) && result.length > 0 ? result : undefined;
  }, 30000, "document symbols from aglsp");
  const names = symbols.map((symbol) => symbol.name).sort();
  assert.deepEqual(names, ["Point", "main", "twice"]);

  const text = document.getText();
  const hovers = await vscode.commands.executeCommand(
    "vscode.executeHoverProvider", file, document.positionAt(text.indexOf("twice(3)") + 1));
  const hoverText = hovers.flatMap((hover) => hover.contents.map((part) => part.value ?? String(part))).join("\n");
  assert.match(hoverText, /Doubles a value\./, "hover shows the doc comment");

  console.log(`aglsp answered: symbols=${names.join(",")}`);
}

module.exports = { run };
