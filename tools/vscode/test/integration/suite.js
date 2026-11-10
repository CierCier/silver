// Opens a Silver file and checks that aglsp answers through the extension.
const assert = require("node:assert/strict");
const { execFileSync } = require("node:child_process");
const fs = require("node:fs");
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

/**
 * Arguments (after the executable) of each aglsp process started by this
 * extension host. Linux only: argv comes from /proc, so paths with spaces
 * keep their boundaries.
 */
function serverProcesses() {
  const output = execFileSync("ps", ["-o", "pid=,comm=", "--ppid", String(process.pid)], { encoding: "utf8" });
  return output
    .split("\n")
    .map((line) => line.trim().split(/\s+/))
    .filter(([, comm]) => comm === "aglsp")
    .map(([pid]) => fs.readFileSync(`/proc/${pid}/cmdline`, "utf8").split("\0").filter(Boolean).slice(1));
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

  // Overlapping restarts must leave exactly one server running.
  await Promise.all([
    vscode.commands.executeCommand("silver.restartServer"),
    vscode.commands.executeCommand("silver.restartServer"),
    vscode.commands.executeCommand("silver.restartServer"),
  ]);
  await eventually(async () => {
    const result = await vscode.commands.executeCommand("vscode.executeDocumentSymbolProvider", file);
    return Array.isArray(result) && result.length > 0;
  }, 30000, "document symbols after restarts");
  const servers = serverProcesses();
  assert.equal(servers.length, 1, `expected one aglsp child process, found ${servers.length}`);
  // aglsp takes no arguments; the command line is only the executable.
  assert.deepEqual(servers[0], [], `aglsp was started with arguments: ${servers[0].join(" ")}`);

  console.log(`aglsp answered: symbols=${names.join(",")}; servers after restarts=${servers.length}; arguments=${servers[0].length}`);
}

module.exports = { run };
