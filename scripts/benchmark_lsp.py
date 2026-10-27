#!/usr/bin/env python3
"""Measure native LSP editor latency against a real workspace, in milliseconds."""

import argparse
import json
from pathlib import Path
import statistics
import time

from check_lsp import Session, semantic_capability


def summarize(samples):
    ordered = sorted(samples)
    return {"median_ms": statistics.median(ordered),
            "p95_ms": ordered[min(len(ordered) - 1, (95 * len(ordered) + 99) // 100 - 1)],
            "samples_ms": samples}


def benchmark(binary, root, runs, semantic_document):
    samples = {"first_hover": [], "workspace_index_and_search": [], "cached_workspace_search": [], "edited_hover": []}
    if semantic_document:
        samples["semantic_tokens"] = []
    text = "i32 main() { i32 value = 7; return value; }\n"
    uri = (root / "__aglsp_benchmark_overlay.ag").as_uri()
    params = {"textDocument": {"uri": uri}, "position": {"line": 0, "character": 35}}
    for _ in range(runs):
        session = Session(binary, root)
        try:
            session.result("initialize", {"processId": None, "rootUri": root.as_uri(), "capabilities": {
                "textDocument": {"semanticTokens": semantic_capability()}} if semantic_document else {}})
            start = time.perf_counter()
            session.send("initialized", {})
            session.send("textDocument/didOpen", {"textDocument": {
                "uri": uri, "languageId": "silver", "version": 1, "text": text}})
            assert "i32 value" in session.result("textDocument/hover", params)["contents"]["value"]
            samples["first_hover"].append((time.perf_counter() - start) * 1000)
            start = time.perf_counter()
            assert session.result("workspace/symbol", {"query": "IdeIndex"})
            samples["workspace_index_and_search"].append((time.perf_counter() - start) * 1000)
            start = time.perf_counter()
            assert session.result("workspace/symbol", {"query": "IdeIndex"})
            samples["cached_workspace_search"].append((time.perf_counter() - start) * 1000)
            start = time.perf_counter()
            session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2},
                         "contentChanges": [{"text": text.replace("7", "8")}]})
            assert "i32 value" in session.result("textDocument/hover", params)["contents"]["value"]
            samples["edited_hover"].append((time.perf_counter() - start) * 1000)
            if semantic_document:
                start = time.perf_counter()
                tokens = session.result("textDocument/semanticTokens/full", {
                    "textDocument": {"uri": (root / semantic_document).as_uri()}})
                assert len(tokens["data"]) > 1000 and len(tokens["data"]) % 5 == 0
                samples["semantic_tokens"].append((time.perf_counter() - start) * 1000)
        finally:
            session.close()
    return {name: summarize(values) for name, values in samples.items()}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--server", type=Path, required=True)
    parser.add_argument("--workspace", type=Path, required=True, help="Silver source workspace containing IdeIndex")
    parser.add_argument("--runs", type=int, default=10)
    parser.add_argument("--semantic-document", type=Path, help="Also measure tokens for a source file relative to the workspace")
    args = parser.parse_args()
    if args.runs < 10:
        parser.error("at least 10 runs are required")
    print(json.dumps({"server": str(args.server.resolve()), "workspace": str(args.workspace.resolve()),
                      "runs": args.runs, "semantic_document": str(args.semantic_document) if args.semantic_document else None,
                      "results": benchmark(args.server.resolve(), args.workspace.resolve(), args.runs, args.semantic_document)}, indent=2))
