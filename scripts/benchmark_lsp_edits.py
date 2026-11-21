#!/usr/bin/env python3
"""Measure edit-time LSP latency with Zed's request pattern, in milliseconds.

Zed answers every edit with a diagnostic pull, then sends the user's format or
rename request. This replays that order against one warmed server session so a
slow pull shows up as format and rename latency, which is what the user sees.
"""

import argparse
import json
from pathlib import Path
import statistics
import time

from check_lsp import Session, semantic_capability

# The capabilities that change server behavior for Zed: pull diagnostics with
# refresh, semantic and inlay refresh, and versioned workspace edits.
ZED_CAPABILITIES = {
    "textDocument": {
        "diagnostic": {"dynamicRegistration": False, "relatedDocumentSupport": True},
        "semanticTokens": semantic_capability(),
        "rename": {"prepareSupport": True},
        "synchronization": {"didSave": True},
    },
    "workspace": {
        "diagnostics": {"refreshSupport": True},
        "semanticTokens": {"refreshSupport": True},
        "inlayHint": {"refreshSupport": True},
        "workspaceEdit": {"documentChanges": True},
    },
}
FORMAT_OPTIONS = {"tabSize": 4, "insertSpaces": True}
BROKEN_FUNCTION = "\ni32 aglsp_benchmark_broken() { return aglsp_benchmark_missing; }\n"


def summarize(samples):
    ordered = sorted(samples)
    return {"median_ms": round(statistics.median(ordered), 1),
            "p95_ms": round(ordered[min(len(ordered) - 1, (95 * len(ordered) + 99) // 100 - 1)], 1),
            "max_ms": round(ordered[-1], 1)}


class Client:
    """One warmed session that answers server requests the way Zed does."""

    def __init__(self, binary, root, document):
        self.session = Session(binary, root)
        self.text = document.read_text()
        self.uri = document.as_uri()
        self.version = 1
        self.refreshes = 0
        # Responses that arrived while waiting for a different request.
        self.responses = {}
        # Zed keeps one workspace pull open as a long-poll: an edit makes it
        # re-send as soon as the open one answers, otherwise 2 s later.
        self.workspace_poll = None
        self.poll_due = None
        self.edited_since_poll = False
        self.session.result("initialize", {"processId": None, "rootUri": root.as_uri(),
                                           "capabilities": ZED_CAPABILITIES})
        self.session.send("initialized", {})
        self.session.send("textDocument/didOpen", {"textDocument": {
            "uri": self.uri, "languageId": "silver", "version": 1, "text": self.text}})
        # Drain background indexing so every sample measures steady-state edits.
        self.session.result("workspace/symbol", {"query": "aglsp_benchmark_none"})

    def send(self, method, params):
        if self.poll_due is not None and time.perf_counter() >= self.poll_due:
            self.poll_due = None
            self.start_workspace_poll()
        self.session.next_id += 1
        self.session.send(method, params, self.session.next_id)
        return self.session.next_id

    def wait(self, request_id):
        response = self.responses.pop(request_id, None) or self.session.wait_response(request_id)
        self.answer_server_requests()
        if "error" in response:
            raise RuntimeError(f"request {request_id} failed: {response['error']}")
        return response["result"]

    def answer_server_requests(self):
        for message in self.session.notifications:
            if "id" in message and "method" not in message and message["id"] == self.workspace_poll:
                self.workspace_poll = None
                self.poll_due = time.perf_counter() + (0 if self.edited_since_poll else 2)
            elif "id" in message and "method" not in message:
                self.responses[message["id"]] = message
            elif "id" in message:
                if message["method"] == "workspace/diagnostic/refresh":
                    self.refreshes += 1
                self.session.respond(message["id"], None)
        self.session.notifications.clear()

    def start_workspace_poll(self):
        self.edited_since_poll = False
        self.workspace_poll = self.send("workspace/diagnostic", {"previousResultIds": []})

    def edit(self, text):
        self.edited_since_poll = True
        self.version += 1
        self.session.send("textDocument/didChange", {"textDocument": {"uri": self.uri, "version": self.version},
                                                     "contentChanges": [{"text": text}]})

    def document(self):
        return {"textDocument": {"uri": self.uri}}

    def pull(self):
        return self.send("textDocument/diagnostic", self.document())

    def close(self):
        self.edit(self.text)
        self.session.close()


def position_of(text, word):
    offset = text.index(word)
    line = text.count("\n", 0, offset)
    return {"line": line, "character": offset - (text.rfind("\n", 0, offset) + 1)}


def reports_error(report):
    return any(item.get("severity") == 1 and "aglsp_benchmark" in item.get("message", "")
               for item in (report or {}).get("items", []))


def fresh_diagnostics_ms(client, sample):
    """Time from an edit that adds an error until a pull reports it."""
    start = time.perf_counter()
    client.edit(client.text + BROKEN_FUNCTION + f"// {sample}\n")
    deadline = start + 30
    while time.perf_counter() < deadline:
        if reports_error(client.wait(client.pull())):
            return (time.perf_counter() - start) * 1000
        # A server that checks in the background asks for a re-pull when done.
        refresh = client.session.wait_notification("workspace/diagnostic/refresh")
        client.session.respond(refresh["id"], None)
    raise RuntimeError("the introduced error was never reported")


def benchmark(binary, root, document, rename_word, runs, workspace_poll):
    client = Client(binary, root, document)
    if workspace_poll:
        client.start_workspace_poll()
    samples = {name: [] for name in ("format_idle", "format_after_edit", "rename_idle",
                                     "rename_after_edit", "references_idle", "format_after_burst")}
    rename_position = {**client.document(), "position": position_of(client.text, rename_word)}
    try:
        for sample in range(runs):
            start = time.perf_counter()
            client.wait(client.send("textDocument/formatting", {**client.document(), "options": FORMAT_OPTIONS}))
            samples["format_idle"].append((time.perf_counter() - start) * 1000)

            client.edit(client.text + f"\n// edit {sample}\n")
            pull = client.pull()
            start = time.perf_counter()
            formatting = client.send("textDocument/formatting", {**client.document(), "options": FORMAT_OPTIONS})
            client.wait(formatting)
            samples["format_after_edit"].append((time.perf_counter() - start) * 1000)
            client.wait(pull)

            start = time.perf_counter()
            client.wait(client.send("textDocument/rename", {**rename_position, "newName": rename_word + "_renamed"}))
            samples["rename_idle"].append((time.perf_counter() - start) * 1000)

            client.edit(client.text + f"\n// rename {sample}\n")
            pull = client.pull()
            start = time.perf_counter()
            client.wait(client.send("textDocument/rename", {**rename_position, "newName": rename_word + "_renamed"}))
            samples["rename_after_edit"].append((time.perf_counter() - start) * 1000)
            client.wait(pull)

            start = time.perf_counter()
            client.wait(client.send("textDocument/references",
                                    {**rename_position, "context": {"includeDeclaration": True}}))
            samples["references_idle"].append((time.perf_counter() - start) * 1000)

            # Typing: ten keystrokes 30 ms apart, each followed by Zed's pull.
            pulls = []
            for keystroke in range(10):
                client.edit(client.text + f"\n// burst {sample} {'x' * keystroke}\n")
                pulls.append(client.pull())
                time.sleep(0.03)
            start = time.perf_counter()
            client.wait(client.send("textDocument/formatting", {**client.document(), "options": FORMAT_OPTIONS}))
            samples["format_after_burst"].append((time.perf_counter() - start) * 1000)
            for pull in pulls:
                client.wait(pull)
        fresh = [fresh_diagnostics_ms(client, sample) for sample in range(runs)]
    finally:
        client.close()
    results = {name: summarize(values) for name, values in samples.items()}
    results["fresh_diagnostics_after_edit"] = summarize(fresh)
    return results


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--server", type=Path, required=True)
    parser.add_argument("--workspace", type=Path, required=True)
    parser.add_argument("--document", type=Path, default=Path("bin/aglsp/src/main.ag"),
                        help="Workspace-relative document to edit")
    parser.add_argument("--rename", default="dispatch_message", help="Symbol in the document to rename")
    parser.add_argument("--runs", type=int, default=10)
    parser.add_argument("--workspace-poll", action="store_true",
                        help="Keep a workspace diagnostic pull open the way Zed does")
    args = parser.parse_args()
    if args.runs < 10:
        parser.error("at least 10 runs are required")
    workspace = args.workspace.resolve()
    print(json.dumps({"server": str(args.server.resolve()), "document": str(args.document), "runs": args.runs,
                      "workspace_poll": args.workspace_poll,
                      "results": benchmark(args.server.resolve(), workspace, workspace / args.document,
                                           args.rename, args.runs, args.workspace_poll)}, indent=2))
