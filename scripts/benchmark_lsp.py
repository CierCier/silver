#!/usr/bin/env python3
"""Measure native LSP editor latency against a real workspace, in milliseconds."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import statistics
import time

from check_lsp import Session, position, semantic_capability


def summarize(samples):
    ordered = sorted(samples)
    return {"median_ms": statistics.median(ordered),
            "p95_ms": ordered[min(len(ordered) - 1, (95 * len(ordered) + 99) // 100 - 1)],
            "samples_ms": samples}


def resident_kib(process):
    for line in Path(f"/proc/{process.pid}/status").read_text().splitlines():
        if line.startswith("VmRSS:"):
            return int(line.split()[1])
    raise RuntimeError("Server RSS is unavailable")


# aglsp caps one completion response at 100 items (completion.ag offer_completion).
# A reported-incomplete reply can also mean bulk indexing is still running, so the
# item cap and the server's own signal stay separate.
COMPLETION_ITEM_LIMIT = 100


def completion_snapshot(result, expected=()):
    if not isinstance(result, dict) or not isinstance(result.get("items"), list):
        raise RuntimeError("Completion response did not contain an item list")
    items = result["items"]
    for item in items:
        if not isinstance(item, dict) or not isinstance(item.get("label"), str):
            raise RuntimeError("Completion item did not contain a string label")
    labels = [item["label"] for item in items]
    data = [item.get("data") for item in items]
    auto_imports = sum(isinstance(entry, dict) and "module" in entry for entry in data)
    providers = {entry["module"] for entry in data if isinstance(entry, dict) and "module" in entry}
    present = set(labels)
    snapshot = {
        "items": len(items),
        "auto_imports": auto_imports,
        "providers": len(providers),
        "reported_incomplete": bool(result.get("isIncomplete", False)),
        "item_limit_reached": len(items) >= COMPLETION_ITEM_LIMIT,
        "sha256": hashlib.sha256(json.dumps(sorted(items, key=lambda item: json.dumps(item, sort_keys=True)),
                                             sort_keys=True).encode()).hexdigest(),
        "labels_sha256": hashlib.sha256(json.dumps(sorted(labels)).encode()).hexdigest(),
        "ordered_labels_sha256": hashlib.sha256(json.dumps(labels).encode()).hexdigest(),
    }
    if expected:
        snapshot["expected_present"] = sorted(present & set(expected))
        snapshot["expected_missing"] = sorted(set(expected) - present)
    return snapshot, present


def request_completion(session, params, expected=()):
    start = time.perf_counter()
    result = session.result("textDocument/completion", params)
    elapsed = (time.perf_counter() - start) * 1000
    snapshot, labels = completion_snapshot(result, expected)
    return elapsed, snapshot, labels


def benchmark(binary, root, runs, semantic_document, completion_prefix=None, memory=False,
              first_use_completion=False, completion_expected=None):
    samples = {"first_hover": [], "workspace_index_and_search": [], "cached_workspace_search": [], "edited_hover": []}
    if semantic_document:
        samples["semantic_tokens"] = []
    completion_results = []
    memory_samples = []
    first_reply_latencies = []
    first_reply_after_open = []
    ready_end_to_end = []
    index_plus_ready = []
    first_use_responses = []
    expected = completion_expected or []
    if completion_prefix:
        samples["completion"] = []
    text = "i32 main() { i32 value = 7; return value; }\n"
    uri = (root / "__aglsp_benchmark_overlay.ag").as_uri()
    params = {"textDocument": {"uri": uri}, "position": {"line": 0, "character": 35}}
    for run in range(runs):
        process_start = time.perf_counter()
        session = Session(binary, root)
        try:
            session.result("initialize", {"processId": None, "rootUri": root.as_uri(), "capabilities": {
                "textDocument": {"semanticTokens": semantic_capability()}} if semantic_document else {}})
            start = time.perf_counter()
            session.send("initialized", {})
            session.send("textDocument/didOpen", {"textDocument": {
                "uri": uri, "languageId": "silver", "version": 1, "text": text}})
            hover = session.result("textDocument/hover", params)
            if "i32 value" not in hover["contents"]["value"]:
                raise RuntimeError("Initial hover did not resolve the local value")
            samples["first_hover"].append((time.perf_counter() - start) * 1000)
            # First-use completion must run before any request that drains the index.
            if first_use_completion:
                overlay_text = "i32 main() { " + completion_prefix + "; }\n"
                overlay_uri = (root / "__aglsp_completion_overlay.ag").as_uri()
                overlay_params = {"textDocument": {"uri": overlay_uri},
                                  "position": position(overlay_text, len("i32 main() { ") + len(completion_prefix))}
                open_start = time.perf_counter()
                session.send("textDocument/didOpen", {"textDocument": {
                    "uri": overlay_uri, "languageId": "silver", "version": 1, "text": overlay_text}})
                latency, first_snapshot, first_labels = request_completion(session, overlay_params, expected)
                first_reply_latencies.append(latency)
                first_reply_after_open.append((time.perf_counter() - open_start) * 1000)
                start = time.perf_counter()
                symbols = session.result("workspace/symbol", {"query": "IdeIndex"})
                if not symbols:
                    raise RuntimeError("Workspace search did not find IdeIndex")
                index_ready_ms = (time.perf_counter() - start) * 1000
                samples["workspace_index_and_search"].append(index_ready_ms)
                # Measure the complete reply at the point the index becomes available.
                latency, snapshot, labels = request_completion(session, overlay_params, expected)
                samples["completion"].append(latency)
                if not snapshot["auto_imports"]:
                    raise RuntimeError("Completion did not offer any source auto-imports")
                completion_results.append(snapshot)
                ready_end_to_end.append((time.perf_counter() - process_start) * 1000)
                index_plus_ready.append(index_ready_ms + latency)
                recall = len(first_labels & labels) / len(labels) if labels else 1.0
                first_use_responses.append({
                    "run": run + 1,
                    "first_reply": first_snapshot,
                    "ready": snapshot,
                    "first_reply_labels_match_ready": first_snapshot["labels_sha256"] == snapshot["labels_sha256"],
                    "first_reply_order_matches_ready": first_snapshot["ordered_labels_sha256"] == snapshot["ordered_labels_sha256"],
                    "first_reply_payload_matches_ready": first_snapshot["sha256"] == snapshot["sha256"],
                    "first_reply_label_recall": recall,
                    "index_ready_ms_before_ready_reply": index_ready_ms,
                    "index_plus_ready_ms": index_ready_ms + latency,
                })
            else:
                start = time.perf_counter()
                symbols = session.result("workspace/symbol", {"query": "IdeIndex"})
                if not symbols:
                    raise RuntimeError("Workspace search did not find IdeIndex")
                samples["workspace_index_and_search"].append((time.perf_counter() - start) * 1000)
            start = time.perf_counter()
            symbols = session.result("workspace/symbol", {"query": "IdeIndex"})
            if not symbols:
                raise RuntimeError("Cached workspace search did not find IdeIndex")
            samples["cached_workspace_search"].append((time.perf_counter() - start) * 1000)
            if memory:
                memory_samples.append(resident_kib(session.process))
            start = time.perf_counter()
            session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2},
                         "contentChanges": [{"text": text.replace("7", "8")}]})
            hover = session.result("textDocument/hover", params)
            if "i32 value" not in hover["contents"]["value"]:
                raise RuntimeError("Edited hover did not resolve the local value")
            samples["edited_hover"].append((time.perf_counter() - start) * 1000)
            if semantic_document:
                start = time.perf_counter()
                tokens = session.result("textDocument/semanticTokens/full", {
                    "textDocument": {"uri": (root / semantic_document).as_uri()}})
                if len(tokens["data"]) <= 1000 or len(tokens["data"]) % 5 != 0:
                    raise RuntimeError("Semantic response did not contain the expected token tuples")
                samples["semantic_tokens"].append((time.perf_counter() - start) * 1000)
            if completion_prefix and not first_use_completion:
                completion_text = "i32 main() { " + completion_prefix + "; }\n"
                completion_uri = (root / "__aglsp_completion_overlay.ag").as_uri()
                session.send("textDocument/didOpen", {"textDocument": {
                    "uri": completion_uri, "languageId": "silver", "version": 1, "text": completion_text}})
                # Finish snapshot analysis before timing the completion request itself.
                session.result("textDocument/documentSymbol", {"textDocument": {"uri": completion_uri}})
                completion_params = {"textDocument": {"uri": completion_uri},
                                     "position": position(completion_text, len("i32 main() { ") + len(completion_prefix))}
                latency, snapshot, _ = request_completion(session, completion_params, expected)
                samples["completion"].append(latency)
                if not snapshot["auto_imports"]:
                    raise RuntimeError("Completion did not offer any source auto-imports")
                completion_results.append(snapshot)
        finally:
            session.close()
    results = {name: summarize(values) for name, values in samples.items()}
    if completion_results:
        results["completion"]["responses"] = completion_results
    if first_use_completion:
        first_use_results = {
            "prefix": completion_prefix,
            "item_limit": COMPLETION_ITEM_LIMIT,
            "expected_labels": list(expected),
            "first_reply": summarize(first_reply_latencies),
            "first_reply_after_open": summarize(first_reply_after_open),
            "index_ready_search_after_first_reply": summarize(samples["workspace_index_and_search"]),
            "index_plus_ready_completion_after_first_reply": summarize(index_plus_ready),
            "ready": summarize(samples["completion"]),
            "ready_end_to_end_from_process_start": summarize(ready_end_to_end),
            "first_reply_auto_import_runs": sum(response["first_reply"]["auto_imports"] > 0
                                                for response in first_use_responses),
            "ready_auto_import_runs": sum(response["ready"]["auto_imports"] > 0
                                          for response in first_use_responses),
            "first_reply_reported_incomplete_runs": sum(response["first_reply"]["reported_incomplete"]
                                                        for response in first_use_responses),
            "ready_reported_incomplete_runs": sum(response["ready"]["reported_incomplete"]
                                                  for response in first_use_responses),
            "ready_item_limit_runs": sum(response["ready"]["item_limit_reached"]
                                         for response in first_use_responses),
            "responses": first_use_responses,
        }
        if expected:
            first_use_results["first_reply_expected_available_runs"] = sum(
                not response["first_reply"]["expected_missing"] for response in first_use_responses)
            first_use_results["ready_expected_available_runs"] = sum(
                not response["ready"]["expected_missing"] for response in first_use_responses)
        results["completion_first_use"] = first_use_results
    if memory:
        results["indexed_rss"] = {"median_kib": statistics.median(memory_samples), "samples_kib": memory_samples}
    return results


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--server", type=Path, required=True)
    parser.add_argument("--workspace", type=Path, required=True, help="Silver source workspace containing IdeIndex")
    parser.add_argument("--runs", type=int, default=10)
    parser.add_argument("--semantic-document", type=Path, help="Also measure tokens for a source file relative to the workspace")
    parser.add_argument("--completion-prefix", help="Also measure warmed completion for an unimported source declaration")
    parser.add_argument("--completion-first-use", action="store_true",
                        help="Measure the first reply after the first hover but before indexing finishes, then the ready reply")
    parser.add_argument("--completion-expect", action="append", default=[], metavar="LABEL",
                        help="Completion label expected in the ready reply; repeat for several labels")
    parser.add_argument("--memory", action="store_true", help="Measure server RSS after indexing (Linux /proc required)")
    args = parser.parse_args()
    if args.runs < 10:
        parser.error("at least 10 runs are required")
    if args.completion_first_use and not args.completion_prefix:
        parser.error("--completion-first-use requires --completion-prefix")
    if args.completion_expect and not args.completion_prefix:
        parser.error("--completion-expect requires --completion-prefix")
    if args.memory and not Path(f"/proc/{os.getpid()}/status").exists():
        parser.error("--memory requires Linux /proc")
    print(json.dumps({"server": str(args.server.resolve()), "workspace": str(args.workspace.resolve()),
                      "runs": args.runs, "semantic_document": str(args.semantic_document) if args.semantic_document else None,
                      "completion_prefix": args.completion_prefix,
                      "completion_first_use": args.completion_first_use,
                      "completion_expected": args.completion_expect,
                      "results": benchmark(args.server.resolve(), args.workspace.resolve(), args.runs,
                                           args.semantic_document, args.completion_prefix, args.memory,
                                           args.completion_first_use, args.completion_expect)}, indent=2))
