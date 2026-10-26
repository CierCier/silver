#!/usr/bin/env python3
"""Exercise the native Silver LSP through its framed stdio protocol."""

import argparse
import json
import os
from pathlib import Path
import queue
import subprocess
import tempfile
import threading
import unittest


def position(text, offset):
    prefix = text[:offset].replace("\r\n", "\n").replace("\r", "\n")
    return {"line": prefix.count("\n"), "character": len(prefix.rsplit("\n", 1)[-1].encode("utf-16-le")) // 2}


def location(text, word, start=0):
    at = text.index(word, start)
    return {"start": position(text, at), "end": position(text, at + len(word))}


def apply_edits(text, edits):
    lines = text.splitlines(keepends=True)

    def offset(pos):
        if pos["line"] == len(lines) and pos["character"] == 0:
            return len(text)
        prefix = lines[pos["line"]].encode("utf-16-le")[:pos["character"] * 2].decode("utf-16-le")
        return sum(map(len, lines[:pos["line"]])) + len(prefix)

    replacements = [(offset(edit["range"]["start"]), offset(edit["range"]["end"]), edit["newText"]) for edit in edits]
    for start, end, replacement in sorted(replacements, reverse=True):
        text = text[:start] + replacement + text[end:]
    return text


class Session:
    def __init__(self, binary, root, env=None):
        self.stderr = tempfile.TemporaryFile()
        self.process = subprocess.Popen([str(binary)], cwd=root, stdin=subprocess.PIPE,
                                        stdout=subprocess.PIPE, stderr=self.stderr, env=env)
        self.messages = queue.Queue()
        self.diagnostics = {}
        self.notifications = []
        self.next_id = 0
        self.reader = threading.Thread(target=self.read, daemon=True)
        self.reader.start()

    def read(self):
        try:
            while True:
                headers = {}
                while True:
                    line = self.process.stdout.readline()
                    if not line:
                        raise EOFError("language server closed stdout")
                    if line == b"\r\n":
                        break
                    key, value = line.decode("ascii").split(":", 1)
                    headers[key.lower()] = value.strip()
                size = int(headers["content-length"])
                body = self.process.stdout.read(size)
                if len(body) != size:
                    raise EOFError("truncated JSON-RPC message")
                self.messages.put(json.loads(body))
        except Exception as error:
            self.messages.put(error)

    def send(self, method, params=None, request_id=None):
        message = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            message["params"] = params
        if request_id is not None:
            message["id"] = request_id
        body = json.dumps(message, ensure_ascii=False).encode()
        self.send_raw(body)

    def send_raw(self, body):
        self.process.stdin.write(f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
        self.process.stdin.flush()

    def respond(self, request_id, result=None, error=None):
        message = {"jsonrpc": "2.0", "id": request_id}
        message["error" if error is not None else "result"] = error if error is not None else result
        self.send_raw(json.dumps(message).encode())

    def request(self, method, params=None):
        self.next_id += 1
        self.send(method, params, self.next_id)
        return self.wait_response(self.next_id, method)

    def wait_response(self, request_id, method="request"):
        while True:
            try:
                message = self.messages.get(timeout=30)
            except queue.Empty as error:
                raise AssertionError(f"timeout waiting for {method}") from error
            if isinstance(message, Exception):
                self.stderr.seek(0)
                raise AssertionError(f"{method}: {message}\n{self.stderr.read().decode(errors='replace')}")
            if message.get("id") == request_id:
                return message
            self.notifications.append(message)
            if message.get("method") == "textDocument/publishDiagnostics":
                self.diagnostics[message["params"]["uri"]] = message["params"]

    def result(self, method, params=None):
        response = self.request(method, params)
        if "error" in response:
            raise AssertionError(f"{method}: {response['error']}")
        return response["result"]

    def close(self):
        try:
            if self.process.poll() is None:
                self.result("shutdown")
                self.send("exit")
                self.process.wait(timeout=10)
                if self.process.returncode != 0:
                    self.stderr.seek(0)
                    raise AssertionError(f"server exit {self.process.returncode}: {self.stderr.read().decode(errors='replace')}")
        finally:
            if self.process.poll() is None:
                self.process.kill()
            self.process.wait()
            self.process.stdin.close()
            self.process.stdout.close()
            self.reader.join(timeout=2)
            self.stderr.close()


class NativeLsp(unittest.TestCase):
    binary = None
    stdlib_root = None

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="silver lsp ")
        self.root = Path(self.directory.name)
        (self.root / "helper.ag").write_text("/// The answer.\ni32 answer(i32 value) { return value; }\n")
        (self.root / "other.ag").write_text("import helper;\ni32 other() { return answer(7); }\n")
        self.session = Session(self.binary, self.root)
        self.addCleanup(self.directory.cleanup)
        self.addCleanup(self.session.close)
        self.capabilities = self.session.result("initialize", {
            "processId": None, "rootUri": self.root.as_uri(), "capabilities": {"textDocument": {
                "hover": {"contentFormat": ["markdown", "plaintext"]},
                "completion": {"completionItem": {"documentationFormat": ["markdown"]}},
                "signatureHelp": {"signatureInformation": {"documentationFormat": ["markdown"]}},
                "documentSymbol": {"hierarchicalDocumentSymbolSupport": True},
                "codeAction": {"codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix", "refactor"]}}},
                "documentLink": {"tooltipSupport": True},
                "publishDiagnostics": {"versionSupport": True, "relatedInformation": True},
            }},
        })["capabilities"]
        self.session.send("initialized", {})

    def open(self, text, name="main.ag", version=1):
        uri = (self.root / name).as_uri()
        self.session.send("textDocument/didOpen", {"textDocument": {
            "uri": uri, "languageId": "silver", "version": version, "text": text,
        }})
        self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}})
        return uri

    def at(self, uri, text, word, start=0):
        return {"textDocument": {"uri": uri}, "position": location(text, word, start)["start"]}

    def folder_session(self, folders, env=None):
        session = Session(self.binary, self.root, env=env)
        self.addCleanup(session.close)
        capabilities = session.result("initialize", {"rootUri": self.root.as_uri(),
            "workspaceFolders": [{"uri": path.as_uri(), "name": path.name} for path in folders],
            "capabilities": {"workspace": {"workspaceFolders": True}, "textDocument": {"diagnostic": {}}}})["capabilities"]
        self.assertEqual(capabilities["workspace"]["workspaceFolders"], {"supported": True, "changeNotifications": True})
        session.send("initialized", {})
        return session

    def test_workspace_folder_removal_excludes_closed_files_from_all_workspace_queries(self):
        active = self.root / "active"; active.mkdir()
        external = self.root / "external"; external.mkdir()
        api = 'trait Readable { i32 read(&self); }\ni32 target() { return 1; }'
        main = 'import api;\ni32 main() { return target(); }'
        outside = ('import active.api;\nstruct Other {}\nimpl Readable for Other { i32 read(&self) { return 1; } }\n'
                   'i32 loose() { return target(); }')
        (active / "api.ag").write_text(api); (active / "main.ag").write_text(main)
        (external / "outside.ag").write_text(outside)
        session = self.folder_session([active])
        self.assertEqual(session.result("workspace/symbol", {"query": "loose"}), [])
        folder = {"uri": external.as_uri(), "name": external.name}
        session.send("workspace/didChangeWorkspaceFolders", {"event": {"added": [folder], "removed": []}})
        self.assertEqual([symbol["name"] for symbol in session.result("workspace/symbol", {"query": "loose"})], ["loose"])
        api_uri = (active / "api.ag").as_uri(); main_uri = (active / "main.ag").as_uri(); outside_uri = (external / "outside.ag").as_uri()
        self.assertEqual(len(session.result("workspace/symbol", {"query": "target"})), 1)
        links = session.result("textDocument/documentLink", {"textDocument": {"uri": outside_uri}})
        self.assertEqual(links[0]["target"], api_uri)
        target = self.at(api_uri, api, "target")
        references = session.result("textDocument/references", {**target, "context": {"includeDeclaration": True}})
        self.assertEqual({item["uri"] for item in references}, {api_uri, main_uri, outside_uri})
        readable = self.at(api_uri, api, "Readable")
        self.assertEqual(len(session.result("textDocument/implementation", readable)), 1)
        hierarchy = session.result("textDocument/prepareTypeHierarchy", readable)[0]
        self.assertEqual([item["name"] for item in session.result("typeHierarchy/subtypes", {"item": hierarchy})], ["Other"])
        reports = session.result("workspace/diagnostic", {"previousResultIds": []})["items"]
        previous = [{"uri": report["uri"], "value": report["resultId"]} for report in reports]
        session.send("workspace/didChangeWorkspaceFolders", {"event": {"added": [], "removed": [folder]}})
        self.assertEqual(session.result("workspace/symbol", {"query": "loose"}), [])
        references = session.result("textDocument/references", {**target, "context": {"includeDeclaration": True}})
        self.assertEqual({item["uri"] for item in references}, {api_uri, main_uri})
        self.assertEqual(session.result("textDocument/implementation", readable), [])
        self.assertEqual(session.result("typeHierarchy/subtypes", {"item": hierarchy}), [])
        call = session.result("textDocument/prepareCallHierarchy", target)[0]
        self.assertEqual([item["from"]["name"] for item in session.result("callHierarchy/incomingCalls", {"item": call})], ["main"])
        renamed = session.result("textDocument/rename", {**target, "newName": "renamed"})
        self.assertEqual(set(renamed["changes"]), {api_uri, main_uri})
        explicit = session.result("textDocument/rename", {**self.at(outside_uri, outside, "target"), "newName": "renamed"})
        self.assertEqual(set(explicit["changes"]), {api_uri, main_uri, outside_uri})
        self.assertEqual(session.result("workspace/symbol", {"query": "loose"}), [])
        reports = session.result("workspace/diagnostic", {"previousResultIds": previous})["items"]
        self.assertIn({"uri": outside_uri, "version": None, "kind": "full", "items": []}, reports)
        session.send("textDocument/didOpen", {"textDocument": {"uri": outside_uri, "version": 1, "text": outside}})
        self.assertEqual([item["name"] for item in session.result("workspace/symbol", {"query": "loose"})], ["loose"])
        session.send("textDocument/didClose", {"textDocument": {"uri": outside_uri}})
        self.assertEqual(session.result("workspace/symbol", {"query": "loose"}), [])

    def test_workspace_folder_batches_are_atomic_and_preserve_sysroot_imports(self):
        active = self.root / "active"; active.mkdir()
        external = self.root / "external"; external.mkdir()
        main = 'import shared;\ni32 main() { return answer(); }'
        shared = 'i32 answer() { return 7; }'
        (active / "main.ag").write_text(main); (external / "shared.ag").write_text(shared)
        (external / "unrelated.ag").write_text('i32 unrelated() { return 0; }')
        session = self.folder_session([active])
        uri = (active / "main.ag").as_uri(); target = self.at(uri, main, "answer")
        folder = {"uri": external.as_uri(), "name": external.name}
        self.assertIsNone(session.result("textDocument/definition", target))
        session.send("workspace/didChangeWorkspaceFolders", {"event": {"added": [folder], "removed": [{"uri": active.as_uri()}]}})
        self.assertIsNone(session.result("textDocument/definition", target))
        session.send("workspace/didChangeWorkspaceFolders", {"event": {"added": [folder], "removed": []}})
        self.assertEqual(session.result("textDocument/definition", target), {"uri": (external / "shared.ag").as_uri(), "range": location(shared, "answer")})
        alias = {"uri": external.as_uri() + "/./", "name": "alias"}
        session.send("workspace/didChangeWorkspaceFolders", {"event": {"added": [alias], "removed": []}})
        session.send("workspace/didChangeWorkspaceFolders", {"event": {"added": [], "removed": [folder]}})
        self.assertIsNone(session.result("textDocument/definition", target))
        installed = self.folder_session([active], env=dict(os.environ, SILVER_SYSROOT=external.name))
        installed.send("workspace/didChangeWorkspaceFolders", {"event": {"added": [folder], "removed": []}})
        self.assertTrue(installed.result("workspace/symbol", {"query": "unrelated"}))
        installed.send("workspace/didChangeWorkspaceFolders", {"event": {"added": [], "removed": [folder]}})
        self.assertEqual(installed.result("textDocument/definition", target)["uri"], (external / "shared.ag").as_uri())
        links = installed.result("textDocument/documentLink", {"textDocument": {"uri": uri}})
        self.assertEqual(links[0]["target"], (external / "shared.ag").as_uri())
        self.assertEqual(installed.result("workspace/symbol", {"query": "unrelated"}), [])
        self.assertEqual([item["name"] for item in installed.result("workspace/symbol", {"query": "answer"})], ["answer"])

    def test_scope_aware_navigation_and_rename(self):
        text = ("/// Sum values.\ni32 add(i32 left, i32 right) { return left + right; }\n"
                "i32 main() {\n i32 value = 1;\n { i32 value = 2; @println(\"{}\", value); }\n"
                " return add(value, 3);\n}\n")
        uri = self.open(text)
        params = self.at(uri, text, "value", text.index("return add"))
        definition = self.session.result("textDocument/definition", params)
        declaration = location(text, "value", text.index("i32 main"))
        self.assertEqual(definition, {"uri": uri, "range": declaration})
        references = self.session.result("textDocument/references", {**params, "context": {"includeDeclaration": True}})
        self.assertEqual([item["range"] for item in references], [declaration, location(text, "value", text.index("return add"))])
        hover = self.session.result("textDocument/hover", params)
        self.assertIn("i32 value", hover["contents"]["value"])
        prepared = self.session.result("textDocument/prepareRename", params)
        self.assertEqual(prepared["placeholder"], "value")
        renamed = self.session.result("textDocument/rename", {**params, "newName": "result"})
        self.assertEqual(renamed["changes"][uri], [{"range": item["range"], "newText": "result"} for item in references])
        for invalid in ["return", "i32", "é", "", "9name", "let"]:
            self.assertEqual(self.session.request("textDocument/rename", {**params, "newName": invalid})["error"]["code"], -32602)
        highlights = self.session.result("textDocument/documentHighlight", params)
        self.assertEqual([item["range"] for item in highlights], [item["range"] for item in references])
        function = self.session.result("textDocument/hover", self.at(uri, text, "add", text.index("return add")))
        self.assertIn("Sum values.", function["contents"]["value"])
        self.assertIn("i32 add", function["contents"]["value"])

    def test_match_bindings_are_scoped_to_their_arm_and_payload_calls_are_not_declarations(self):
        text = ('enum Choice { First(i32); Second(i32); }\n'
                'i32 evaluate(Choice choice) {\n i32 value = 5;\n return match choice {\n'
                ' First(value): value,\n Second(other): value,\n };\n}\n')
        uri = self.open(text)
        second_value = text.index("value,", text.index("Second(other)"))
        self.assertEqual(self.session.result("textDocument/definition", {"textDocument": {"uri": uri}, "position": position(text, second_value)}),
                         {"uri": uri, "range": location(text, "value", text.index("i32 value"))})
        first_binding = text.index("value", text.index("First(value)"))
        first_use = text.index("value,", first_binding)
        rename = self.session.result("textDocument/rename", {"textDocument": {"uri": uri}, "position": position(text, first_binding), "newName": "payload"})
        self.assertEqual(rename["changes"][uri], [{"range": {"start": position(text, at), "end": position(text, at + 5)}, "newText": "payload"} for at in (first_binding, first_use)])
        updated = apply_edits(text, rename["changes"][uri])
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": updated}]})
        self.assertEqual(self.session.result("textDocument/definition", {"textDocument": {"uri": uri}, "position": position(updated, updated.index("value,", updated.index("Second(other)")))}),
                         {"uri": uri, "range": location(updated, "value", updated.index("i32 value"))})
        call = ('enum Choice { First(i32); }\nChoice main() { i32 value = 3; return match value {\n'
                ' 0: Choice.First(value), _: Choice.First(value), }; }')
        uri = self.open(call, "payload-call.ag")
        for at in (call.index("value)", call.index("0:")), call.index("value)", call.index("_:"))):
            self.assertEqual(self.session.result("textDocument/definition", {"textDocument": {"uri": uri}, "position": position(call, at)}),
                             {"uri": uri, "range": location(call, "value", call.index("i32 value"))})

    def test_nested_match_and_guard_occurrences_keep_distinct_binding_identities(self):
        text = ('i32 classify(i32 input) { i32 value = 5; return match input {\n'
                ' value, value > 0: match input {\n'
                '  value, value > 1: value, _: value,\n'
                ' }, _: value,\n}; }')
        uri = self.open(text)
        outer = text.index("value,", text.index("return match"))
        inner = text.index("value,", text.index("match input", outer))
        after_inner = text.index("value,", text.index("_:"))
        after_outer = text.index("value,", text.index("}, _:"))
        for use, definition in [(after_inner, outer), (after_outer, text.index("value ="))]:
            self.assertEqual(self.session.result("textDocument/definition", {"textDocument": {"uri": uri}, "position": position(text, use)}),
                             {"uri": uri, "range": {"start": position(text, definition), "end": position(text, definition + 5)}})
        renamed = self.session.result("textDocument/rename", {"textDocument": {"uri": uri}, "position": position(text, inner), "newName": "nested"})
        expected = [inner, text.index("value > 1"), text.index("value,", text.index("value > 1"))]
        self.assertEqual(renamed["changes"][uri], [{"range": {"start": position(text, at), "end": position(text, at + 5)}, "newText": "nested"} for at in expected])

    def test_lambda_parameters_shadow_captures_only_inside_their_own_body(self):
        text = ('i32 outer(i32 value) {\n'
                ' let first = (i32 value) => value + 1;\n'
                ' let next = (i32 other) => value + other;\n'
                ' i32(i32) single = (value) => value;\n'
                ' return value;\n}\n')
        uri = self.open(text)
        outer = text.index("value")
        first = text.index("value", text.index("let first"))
        untyped = text.index("value", text.index("single ="))
        for use, definition in [(text.index("value + 1"), first), (text.index("value + other"), outer),
                                (text.index("value;", untyped), untyped), (text.index("value;", text.index("return")), outer)]:
            self.assertEqual(self.session.result("textDocument/definition", {"textDocument": {"uri": uri}, "position": position(text, use)}),
                             {"uri": uri, "range": {"start": position(text, definition), "end": position(text, definition + 5)}})
        renamed = self.session.result("textDocument/rename", {"textDocument": {"uri": uri}, "position": position(text, outer), "newName": "captured"})
        expected = [outer, text.index("value + other"), text.index("value;", text.index("return"))]
        self.assertEqual(renamed["changes"][uri], [{"range": {"start": position(text, at), "end": position(text, at + 5)}, "newText": "captured"} for at in expected])

    def test_nested_and_incomplete_lambda_parameters_remain_local(self):
        text = ('i32 outer(i32 value) {\n let first = (i32 value) => {\n'
                ' let nested = (i32 value) => value;\n return value;\n };\n return value;\n}')
        uri = self.open(text)
        definitions = [text.index("value"), text.index("value", text.index("first =")), text.index("value", text.index("nested ="))]
        uses = [text.index("value;", text.index("nested =")), text.index("value;", text.index("return")), text.rindex("value;")]
        for use, definition in zip(uses, reversed(definitions)):
            self.assertEqual(self.session.result("textDocument/definition", {"textDocument": {"uri": uri}, "position": position(text, use)}),
                             {"uri": uri, "range": {"start": position(text, definition), "end": position(text, definition + 5)}})
        items = self.session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(text, uses[0] + 5)})["items"]
        self.assertEqual(sum(item["label"] == "value" for item in items), 1)
        incomplete = 'i32 main() { let f = (i32 argument) =>'
        uri = self.open(incomplete, "incomplete-lambda.ag")
        items = self.session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(incomplete, len(incomplete))})["items"]
        self.assertIn("argument", {item["label"] for item in items})

    def test_local_inference_uses_the_lambda_scope_without_leaking_parameter_types(self):
        text = ('struct Point { i32 field; }\n'
                'i32 outer(Point value) { let f = (i32 value) => { let copy = value; return copy; };\n'
                ' let copy = value; return copy.field; }')
        uri = self.open(text)
        inner = text.index("copy =")
        outer = text.index("copy =", inner + 1)
        for at, expected in [(inner, "i32 copy"), (outer, "Point copy")]:
            hover = self.session.result("textDocument/hover", {"textDocument": {"uri": uri}, "position": position(text, at)})
            self.assertIn(expected, hover["contents"]["value"])
        items = self.session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(text, text.rindex("field;"))})["items"]
        self.assertEqual({item["label"] for item in items}, {"field"})

    def test_lambda_example_diagnostics_and_hints_use_lambda_semantics(self):
        repo = Path(__file__).resolve().parent.parent
        text = (repo / "examples" / "lambdas.ag").read_text()
        env = os.environ.copy()
        env["SILVER_SYSROOT"] = str(repo)
        session = Session(self.binary, self.root, env=env)
        self.addCleanup(session.close)
        session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {
            "textDocument": {"diagnostic": {}}}})
        session.send("initialized", {})
        uri = (self.root / "lambdas.ag").as_uri()
        document = {"textDocument": {"uri": uri}}
        session.send("textDocument/didOpen", {"textDocument": {
            "uri": uri, "languageId": "silver", "version": 1, "text": text}})
        report = session.result("textDocument/diagnostic", document)
        self.assertEqual([item for item in report["items"] if item["severity"] == 1], [])
        hints = session.result("textDocument/inlayHint", {**document, "range": {
            "start": position(text, 0), "end": position(text, len(text))}})
        self.assertFalse(any("unknown" in item["label"] for item in hints))
        at = text.index("value * 2")
        before = session.result("textDocument/definition", {**document, "position": position(text, at)})
        self.assertEqual(before, {"uri": uri, "range": location(text, "value")})
        self.assertEqual(session.result("textDocument/diagnostic", {**document,
            "previousResultId": report["resultId"]})["kind"], "unchanged")
        self.assertEqual(session.result("textDocument/definition", {**document,
            "position": position(text, at)}), before)

    def test_push_lambda_diagnostics_report_real_errors_and_clear_after_edits(self):
        original = ('i32 main() {\n i32 value = 4;\n'
                    ' let shadow = (i32 value) => value + 1;\n'
                    ' let captured = (i32 argument) => argument + value;\n'
                    ' i32(i32) identity = (argument) => argument;\n'
                    ' return shadow(captured(identity(2)));\n}\n')
        uri = (self.root / "push-lambdas.ag").as_uri()
        unknown = original.replace("=> argument;", "=> argument + missing_local;")
        capture = original.replace("=> argument;", "=> argument + value;")
        cases = [(original, None, None),
                 (unknown, "unknown identifier 'missing_local'", "missing_local"),
                 (capture, "capturing lambda cannot be stored in a function pointer; use an inferred 'let' binding", "(argument)"),
                 (original, None, None)]
        for version, (text, message, token) in enumerate(cases, 1):
            if version == 1:
                self.session.send("textDocument/didOpen", {"textDocument": {
                    "uri": uri, "languageId": "silver", "version": version, "text": text}})
            else:
                self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": version},
                    "contentChanges": [{"text": text}]})
            with self.subTest(version=version):
                for word, use, declaration in [("value", text.index("=> value"), text.index("let shadow")),
                                               ("value", text.index("argument + value"), text.index("i32 value =")),
                                               ("argument", text.index("=> argument", text.index("identity =")), text.index("identity ="))]:
                    self.assertEqual(self.session.result("textDocument/definition", self.at(uri, text, word, use)),
                                     {"uri": uri, "range": location(text, word, declaration)})
                published = self.session.diagnostics[uri]
                self.assertEqual(published["version"], version)
                errors = [item for item in published["diagnostics"] if item["severity"] == 1]
                expected = [] if message is None else [{"range": location(text, token), "severity": 1,
                                                       "source": "silver", "message": message}]
                self.assertEqual(errors, expected)

    def test_lambda_diagnostics_use_unsaved_imported_methods_and_invalidate_dependencies(self):
        disk = 'struct Gauge { i32 value; }\nimpl Gauge { i32 read(&self) { return self.value; } }\n'
        helper_path = self.root / "gauge.ag"
        helper_path.write_text(disk)
        helper_uri = helper_path.as_uri()
        text = ('import gauge;\ni32 main() {\n Gauge gauge;\n'
                ' let read = (&Gauge item) => item.read();\n return read(&gauge);\n}\n')
        uri = (self.root / "imported-lambda.ag").as_uri()
        document = {"textDocument": {"uri": uri}}
        session = Session(self.binary, self.root)
        self.addCleanup(session.close)
        session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {
            "textDocument": {"diagnostic": {}}}})
        session.send("initialized", {})
        session.send("textDocument/didOpen", {"textDocument": {
            "uri": uri, "languageId": "silver", "version": 1, "text": text}})
        overlay = "// Unsaved method signature and location.\n" + disk.replace("i32 read", "i64 read")
        broken = overlay.replace("return self.value;", (
            'i64(i32) compute = (i32 value) => value + missing_method;\n'
            ' return compute(self.value);')) + 'i32 broken() { return missing_imported; }\n'
        removed = broken.replace("i64 read", "i64 renamed")
        previous = None
        for version, helper in enumerate((disk, overlay, broken, removed, disk)):
            if version == 1:
                session.send("textDocument/didOpen", {"textDocument": {
                    "uri": helper_uri, "languageId": "silver", "version": version, "text": helper}})
            elif version in (2, 3):
                session.send("textDocument/didChange", {"textDocument": {"uri": helper_uri, "version": version},
                    "contentChanges": [{"text": helper}]})
            elif version == 4:
                session.send("textDocument/didClose", {"textDocument": {"uri": helper_uri}})
            report = session.result("textDocument/diagnostic", {**document,
                **({"previousResultId": previous} if previous is not None else {})})
            with self.subTest(version=version):
                self.assertEqual(report["kind"], "full")
                self.assertNotEqual(report["resultId"], previous)
                parameter = self.at(uri, text, "item.read")
                expected_parameter = {"uri": uri, "range": location(text, "item")}
                self.assertEqual(session.result("textDocument/definition", parameter), expected_parameter)
                method = self.at(uri, text, "read();")
                expected_method = None if version == 3 else {"uri": helper_uri, "range": location(helper, "read")}
                self.assertEqual(session.result("textDocument/definition", method), expected_method)
                if version != 3:
                    return_type = "i32" if version in (0, 4) else "i64"
                    self.assertIn(return_type + " read", session.result("textDocument/hover", method)["contents"]["value"])
                self.assertEqual(session.result("textDocument/diagnostic", {**document,
                    "previousResultId": report["resultId"]}), {"kind": "unchanged", "resultId": report["resultId"]})
                self.assertEqual(session.result("textDocument/definition", parameter), expected_parameter)
                self.assertEqual(session.result("textDocument/definition", method), expected_method)
                imported = session.result("textDocument/diagnostic", {"textDocument": {"uri": helper_uri}})
                tokens = ("missing_method", "missing_imported") if version in (2, 3) else ()
                self.assertCountEqual([item for item in imported["items"] if item["severity"] == 1], [{
                    "range": location(helper, token), "severity": 1, "source": "silver",
                    "message": f"unknown identifier '{token}'"} for token in tokens])
                expected = [] if version != 3 else [{"range": location(text, "(&Gauge item)"), "severity": 1,
                    "source": "silver", "message": "cannot infer lambda return type"}]
                self.assertEqual([item for item in report["items"] if item["severity"] == 1], expected)
            previous = report["resultId"]

    def test_unopened_files_imports_and_unsaved_overlay(self):
        text = "import helper;\ni32 main() { return answer(42); }\n"
        uri = self.open(text)
        helper = (self.root / "helper.ag").as_uri()
        params = self.at(uri, text, "answer")
        definition = self.session.result("textDocument/definition", params)
        self.assertEqual(definition["uri"], helper)
        refs = self.session.result("textDocument/references", {**params, "context": {"includeDeclaration": True}})
        self.assertEqual({item["uri"] for item in refs}, {uri, helper, (self.root / "other.ag").as_uri()})
        edit = self.session.result("textDocument/rename", {**params, "newName": "compute"})
        self.assertEqual(set(edit["changes"]), {item["uri"] for item in refs})
        symbols = self.session.result("workspace/symbol", {"query": "ANS"})
        self.assertEqual([(item["name"], item["location"]["uri"]) for item in symbols], [("answer", helper)])
        changed = "// shifted\n// shifted again\n/// Unsaved answer.\ni32 answer(i32 value) { return value + 1; }\n"
        self.open(changed, "helper.ag")
        definition = self.session.result("textDocument/definition", params)
        self.assertEqual(definition["range"]["start"]["line"], 3)
        self.assertIn("Unsaved answer.", self.session.result("textDocument/hover", params)["contents"]["value"])
        self.session.send("textDocument/didClose", {"textDocument": {"uri": helper}})
        definition = self.session.result("textDocument/definition", params)
        self.assertEqual(definition["range"]["start"]["line"], 1)
        self.assertEqual(self.session.diagnostics[helper]["diagnostics"], [])

    def test_utf16_incremental_changes_are_atomic_and_versioned(self):
        text = 'i32 main() { str label = "😀é"; i32 count = 1; return count; }\r\n'
        uri = self.open(text)
        split_surrogate = location(text, "😀")["start"]
        split_surrogate = {**split_surrogate, "character": split_surrogate["character"] + 1}
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [
            {"range": {"start": split_surrogate, "end": split_surrogate}, "text": "x"},
        ]})
        original = self.at(uri, text, "count", text.index("return"))
        self.assertEqual(self.session.result("textDocument/definition", original)["range"], location(text, "count"))
        self.assertEqual(self.session.diagnostics[uri]["version"], 1)
        changes = [{"range": location(text, "😀"), "text": "λ"}]
        updated = text.replace("😀", "λ")
        changes.append({"range": location(updated, "1"), "text": "22"})
        updated = updated.replace("= 1", "= 22")
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": changes})
        params = self.at(uri, updated, "count", updated.index("return"))
        self.assertEqual(self.session.result("textDocument/definition", params)["range"], location(updated, "count"))
        self.assertEqual(self.session.diagnostics[uri]["version"], 2)
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 1}, "contentChanges": [{"text": ""}]})
        self.assertIsNotNone(self.session.result("textDocument/hover", params))
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 3}, "contentChanges": [
            {"text": "i32 broken;"}, {"range": {"start": {"line": 9, "character": 0}, "end": {"line": 0, "character": 0}}, "text": ""},
        ]})
        self.assertIsNotNone(self.session.result("textDocument/hover", params))
        self.assertEqual(self.session.diagnostics[uri]["version"], 2)
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 3}, "contentChanges": [{"text": ""}]})
        self.assertEqual(self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}}), [])

    def test_completion_signature_and_structural_features(self):
        text = ("/* first line\n second line */\nstruct Point { i32 x; }\n"
                "impl Point { i32 plus(&self, i32 amount) { return self.x + amount; } }\n"
                "i32 add(i32 left, i32 right) { return left + right; }\n"
                "i32 main() {\n Point point;\n let inferred = 4;\n return add(add(1, 2), point.plus(3));\n}\n")
        uri = self.open(text)
        member = text.index("plus(3)")
        completion = self.session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(text, member)})
        self.assertEqual({item["label"] for item in completion["items"]}, {"x", "plus"})
        outer = text.index("point.plus(3)")
        signature = self.session.result("textDocument/signatureHelp", {"textDocument": {"uri": uri}, "position": position(text, outer)})
        self.assertIn("add", signature["signatures"][0]["label"])
        self.assertEqual(signature["activeParameter"], 1)
        method = text.index("3));")
        signature = self.session.result("textDocument/signatureHelp", {"textDocument": {"uri": uri}, "position": position(text, method)})
        self.assertEqual([item["label"] for item in signature["signatures"][0]["parameters"]], ["i32 amount"])
        self.assertEqual(signature["activeParameter"], 0)
        tokens = self.session.result("textDocument/semanticTokens/full", {"textDocument": {"uri": uri}})["data"]
        self.assertEqual(len(tokens) % 5, 0)
        self.assertEqual(tokens[:10], [0, 0, 13, 13, 0, 1, 0, 15, 13, 0])
        folds = self.session.result("textDocument/foldingRange", {"textDocument": {"uri": uri}})
        self.assertIn({"startLine": 0, "startCharacter": 0, "endLine": 1, "endCharacter": 15, "kind": "comment"}, folds)
        selected = self.session.result("textDocument/selectionRange", {"textDocument": {"uri": uri}, "positions": [position(text, member)]})
        self.assertEqual(selected[0]["range"], location(text, "plus", member))
        self.assertIn("parent", selected[0])
        hints = self.session.result("textDocument/inlayHint", {"textDocument": {"uri": uri}, "range": {"start": position(text, 0), "end": position(text, len(text))}})
        self.assertTrue(any(item["position"] == location(text, "inferred")["end"] and item["label"].startswith(": i") for item in hints), hints)
        self.assertTrue(any(item["label"] == "amount:" and item["position"] == position(text, method) for item in hints), hints)
        outline = self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}})
        point = next(item for item in outline if item["name"] == "Point")
        self.assertEqual(point["children"][0]["name"], "x")
        self.assertLess(point["range"]["start"]["character"], point["selectionRange"]["start"]["character"])

    def test_folding_honors_line_only_clients_and_range_limits(self):
        text = ('i32 main() {\n if (true) {\n  return 1;\n }\n return 0;\n}\n'
                '/* comment\n body\n */ i32 after() { return 2; }\n'
                'i32 short_block() {\n return 3; }\n')
        uri = self.open(text)
        full = self.session.result("textDocument/foldingRange", {"textDocument": {"uri": uri}})
        self.assertIn({"startLine": 0, "startCharacter": 11, "endLine": 5, "endCharacter": 1}, full)
        self.assertIn({"startLine": 6, "startCharacter": 0, "endLine": 8, "endCharacter": 3, "kind": "comment"}, full)
        for line_only, limit in ((True, None), (True, 1), (True, 0), (False, 1), (False, -1)):
            with self.subTest(line_only=line_only, limit=limit):
                session = Session(self.binary, self.root)
                self.addCleanup(session.close)
                options = {"lineFoldingOnly": line_only}
                if limit is not None:
                    options["rangeLimit"] = limit
                session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {
                    "textDocument": {"foldingRange": options}}})
                session.send("initialized", {})
                session.send("textDocument/didOpen", {"textDocument": {
                    "uri": uri, "languageId": "silver", "version": 1, "text": text}})
                folds = session.result("textDocument/foldingRange", {"textDocument": {"uri": uri}})
                if limit is not None and limit >= 0:
                    self.assertEqual(len(folds), limit)
                if line_only and (limit is None or limit > 0):
                    self.assertEqual(folds[0], {"startLine": 0, "endLine": 4})
                    self.assertTrue(all("startCharacter" not in fold and "endCharacter" not in fold for fold in folds))
                if line_only and limit is None:
                    self.assertIn({"startLine": 1, "endLine": 2}, folds)
                    self.assertIn({"startLine": 6, "endLine": 7, "kind": "comment"}, folds)
                    self.assertTrue(all(fold["startLine"] < fold["endLine"] for fold in folds))
                    self.assertFalse(any(fold["startLine"] == 9 for fold in folds))
                if not line_only:
                    self.assertEqual(folds, full if limit == -1 else full[:limit])

    def test_selection_ranges_validate_the_entire_utf16_position_batch(self):
        text = 'i32 main() {\n /* 😀 */ return 7;\n}\n'
        uri = self.open(text)
        params = {"textDocument": {"uri": uri}}
        valid = position(text, text.index("7"))
        selections = self.session.result("textDocument/selectionRange", {**params, "positions": [valid, valid]})
        self.assertEqual(len(selections), 2)
        self.assertEqual(selections[0], selections[1])
        self.assertEqual(selections[0]["range"], location(text, "7"))
        for invalid in (None, {}, [valid, {"line": 1, "character": 5}],
                        [valid, {"line": 100, "character": 0}],
                        [valid, {"line": -1, "character": 0}], [valid, {"line": 0}]):
            with self.subTest(positions=invalid):
                response = self.session.request("textDocument/selectionRange", {**params, "positions": invalid})
                self.assertEqual(response.get("error", {}).get("code"), -32602, response)
        self.assertEqual(self.session.result("textDocument/selectionRange", {**params, "positions": []}), [])
        self.assertEqual(self.session.result("textDocument/selectionRange", {**params, "positions": [valid]}), selections[:1])

    def test_fuzzy_completion_ranking_replaces_the_whole_identifier_and_honors_scope(self):
        text = ('i32 get_data() { return 1; }\ni32 GatherDetails() { return 2; }\ni32 gd() { return 0; }\n'
                'i32 unrelated() { return 3; }\ni32 main() {\n i32 get_data = 4;\n { i32 gone_data = 1; }\n return gdSuffix;\n}\n')
        uri = self.open(text)
        cursor = text.index("gdSuffix") + 2
        items = self.session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(text, cursor)})["items"]
        candidates = {item["label"]: item for item in items}
        self.assertIn("get_data", candidates)
        self.assertIn("GatherDetails", candidates)
        self.assertNotIn("gone_data", candidates)
        self.assertNotIn("unrelated", candidates)
        local = candidates["get_data"]
        self.assertEqual(local["kind"], 6)
        self.assertEqual(local["textEdit"], {"range": location(text, "gdSuffix"), "newText": "get_data"})
        self.assertEqual(local["insertTextFormat"], 1)
        self.assertLess(candidates["gd"]["sortText"], local["sortText"])
        self.assertEqual(sum(item["label"] == "get_data" for item in items), 1)
        updated = apply_edits(text, [local["textEdit"]])
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": updated}]})
        self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}})
        self.assertEqual(self.session.diagnostics[uri]["diagnostics"], [])
        workspace = self.session.result("workspace/symbol", {"query": "gd"})
        self.assertEqual({item["name"] for item in workspace}, {"gd", "get_data", "GatherDetails"})
        self.assertEqual(workspace[0]["name"], "gd")
        exact = self.session.result("workspace/symbol", {"query": "get_data"})
        self.assertEqual(exact[0]["name"], "get_data")

    def test_completion_snippets_and_deferred_details_preserve_edits_and_reject_stale_items(self):
        session = Session(self.binary, self.root)
        self.addCleanup(session.close)
        capabilities = session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {"textDocument": {
            "completion": {"completionItem": {"snippetSupport": True, "documentationFormat": ["markdown"],
                "resolveSupport": {"properties": ["detail", "documentation"]}}}}}})["capabilities"]
        self.assertTrue(capabilities["completionProvider"]["resolveProvider"])
        session.send("initialized", {})
        text = ('/// Add two values.\ni32 add_values(i32 left, i32 right) { return left + right; }\n'
                'struct Point {}\nimpl Point { i32 plus(&self, i32 amount) { return amount; } }\n'
                'i32 main() { Point p; p.pl; return av; }\n')
        uri = (self.root / "snippet.ag").as_uri()
        session.send("textDocument/didOpen", {"textDocument": {"uri": uri, "version": 1, "text": text}})
        items = session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(text, text.index("return av") + len("return av"))})["items"]
        item = next(item for item in items if item["label"] == "add_values")
        self.assertNotIn("detail", item)
        self.assertNotIn("documentation", item)
        self.assertEqual(item["insertTextFormat"], 2)
        self.assertEqual(item["textEdit"]["newText"], "add_values(${1:left}, ${2:right})$0")
        resolved = session.result("completionItem/resolve", item)
        self.assertIn("i32 left", resolved["detail"])
        self.assertEqual(resolved["documentation"], {"kind": "markdown", "value": "Add two values."})
        self.assertEqual({key: value for key, value in resolved.items() if key not in ("detail", "documentation")}, item)
        method = session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(text, text.index("p.pl") + 4)})["items"]
        self.assertEqual(next(item for item in method if item["label"] == "plus")["textEdit"]["newText"], "plus(${1:amount})$0")
        called = text.replace("return av;", "return av(1, 2);")
        session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": called}]})
        reply = session.request("completionItem/resolve", item)
        self.assertEqual(reply["error"]["code"], -32801)
        items = session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(called, called.index("av(1") + 2)})["items"]
        existing = next(item for item in items if item["label"] == "add_values")
        self.assertEqual(existing["textEdit"]["newText"], "add_values")
        self.assertEqual(existing["insertTextFormat"], 1)
        for version, call in enumerate(("av /* preserve comment */\n(1, 2)", "av<i32>(1, 2)"), start=3):
            continued = text.replace("return av;", f"return {call};")
            session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": version}, "contentChanges": [{"text": continued}]})
            items = session.result("textDocument/completion", {"textDocument": {"uri": uri}, "position": position(continued, continued.index("return av") + len("return av"))})["items"]
            self.assertEqual(next(item for item in items if item["label"] == "add_values")["textEdit"]["newText"], "add_values")
        self.assertEqual(session.request("completionItem/resolve", {"label": "bad", "data": {}})["error"]["code"], -32602)

    def test_import_quickfix_applies_to_current_source_and_resolves_diagnostics(self):
        self.assertEqual(self.capabilities["codeActionProvider"]["codeActionKinds"], ["quickfix"])
        text = '// keep header\r\ni32 main() { return answer(7); }\r\n'
        uri = self.open(text)
        params = {"textDocument": {"uri": uri}, "range": location(text, "answer"), "context": {"diagnostics": []}}
        actions = self.session.result("textDocument/codeAction", params)
        self.assertEqual([action["title"] for action in actions], ["Import answer from helper"])
        self.assertEqual(actions[0]["kind"], "quickfix")
        edits = actions[0]["edit"]["changes"][uri]
        updated = apply_edits(text, edits)
        self.assertEqual(updated, 'import helper;\r\n' + text)
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": updated}]})
        self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}})
        self.assertEqual(self.session.diagnostics[uri]["diagnostics"], [])
        self.assertEqual(self.session.result("textDocument/definition", self.at(uri, updated, "answer")),
                         {"uri": (self.root / "helper.ag").as_uri(), "range": {"start": {"line": 1, "character": 4}, "end": {"line": 1, "character": 10}}})
        params["range"] = location(updated, "answer")
        self.assertEqual(self.session.result("textDocument/codeAction", params), [])

    def test_import_quickfix_rejects_collisions_private_symbols_and_wrong_module_paths(self):
        (self.root / "hidden.ag").write_text('private i32 secret() { return 1; }')
        (self.root / "clash.ag").write_text('i32 wanted() { return 1; }\ni32 existing() { return 0; }')
        self.session.send("workspace/didChangeWatchedFiles", {"changes": [
            {"uri": (self.root / name).as_uri(), "type": 1} for name in ("hidden.ag", "clash.ag")]})
        text = 'i32 existing() { return 1; }\ni32 main() { return wanted() + secret(); }'
        uri = self.open(text)
        params = {"textDocument": {"uri": uri}, "range": {"start": position(text, 0), "end": position(text, len(text))}, "context": {"diagnostics": []}}
        self.assertEqual(self.session.result("textDocument/codeAction", params), [])
        params["context"]["only"] = ["refactor"]
        self.assertEqual(self.session.result("textDocument/codeAction", params), [])
        params["context"]["only"] = "quickfix"
        self.assertEqual(self.session.request("textDocument/codeAction", params)["error"]["code"], -32602)
        (self.root / "sub").mkdir()
        (self.root / "sub" / "helper.ag").write_text('i32 local_only() { return 0; }')
        uri = self.open('i32 main() { return answer(1); }', "sub/main.ag")
        actions = self.session.result("textDocument/codeAction", {"textDocument": {"uri": uri},
            "range": location('i32 main() { return answer(1); }', "answer"), "context": {"diagnostics": []}})
        self.assertEqual(actions, [])

    def test_import_quickfix_preserves_multiple_choices_and_finds_types_in_unsaved_modules(self):
        self.open('i32 choose() { return 1; }', "first.ag")
        self.open('i32 choose() { return 2; }', "second.ag")
        text = 'i32 main() { return choose(); }'
        uri = self.open(text)
        params = {"textDocument": {"uri": uri}, "range": location(text, "choose"), "context": {"diagnostics": []}}
        actions = self.session.result("textDocument/codeAction", params)
        self.assertEqual({action["title"] for action in actions}, {"Import choose from first", "Import choose from second"})
        self.assertTrue(all(not action.get("isPreferred", False) for action in actions))
        self.open('struct Point { i32 x; }', "point.ag")
        text = 'i32 main() { Point value; value.x = 3; return value.x; }'
        uri = self.open(text, version=2)
        params.update(textDocument={"uri": uri}, range=location(text, "Point"))
        actions = self.session.result("textDocument/codeAction", params)
        self.assertEqual([action["title"] for action in actions], ["Import Point from point"])
        updated = apply_edits(text, actions[0]["edit"]["changes"][uri])
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 3}, "contentChanges": [{"text": updated}]})
        self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}})
        self.assertEqual(self.session.diagnostics[uri]["diagnostics"], [])

    def test_import_quickfix_uses_versions_and_excludes_legacy_clients(self):
        session = Session(self.binary, self.root)
        self.addCleanup(session.close)
        caps = session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {"workspace": {"workspaceEdit": {"documentChanges": True}},
            "textDocument": {"codeAction": {"codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}}}}}})["capabilities"]
        self.assertIn("codeActionProvider", caps)
        session.send("initialized", {})
        text = 'i32 main() { return answer(1); }'
        uri = (self.root / "versioned-action.ag").as_uri()
        session.send("textDocument/didOpen", {"textDocument": {"uri": uri, "version": 7, "text": text}})
        params = {"textDocument": {"uri": uri}, "range": location(text, "answer"), "context": {"diagnostics": [], "only": ["quickfix"]}}
        action = session.result("textDocument/codeAction", params)[0]
        self.assertEqual(action["edit"]["documentChanges"][0]["textDocument"], {"uri": uri, "version": 7})
        legacy = Session(self.binary, self.root)
        self.addCleanup(legacy.close)
        self.assertNotIn("codeActionProvider", legacy.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {}})["capabilities"])
        legacy.send("initialized", {})
        self.assertEqual(legacy.request("textDocument/codeAction", params)["error"]["code"], -32601)

    def test_document_links_follow_active_imports_unsaved_modules_and_disk_precedence(self):
        (self.root / "sub").mkdir()
        local = self.root / "sub" / "helper.ag"
        local.write_text('i32 answer(i32 value) { return value; }')
        self.open('i32 value() { return 1; }', "fresh.ag")
        text = ('// 😀 module links\r\nimport helper { answer as read };\r\nimport fresh;\r\n'
                '#[cfg(windows)] import missing_windows;\r\nimport missing;\r\ni32 main() { return read(1); }\r\n')
        uri = self.open(text, "sub/main.ag")
        links = self.session.result("textDocument/documentLink", {"textDocument": {"uri": uri}})
        self.assertEqual(links, [{"range": location(text, "helper"), "target": local.as_uri(), "tooltip": "Open module helper"},
                                 {"range": location(text, "fresh"), "target": (self.root / "fresh.ag").as_uri(), "tooltip": "Open module fresh"}])
        self.session.send("textDocument/didClose", {"textDocument": {"uri": (self.root / "fresh.ag").as_uri()}})
        links = self.session.result("textDocument/documentLink", {"textDocument": {"uri": uri}})
        self.assertEqual(len(links), 1)
        self.assertEqual(links[0]["target"], local.as_uri())
        session = Session(self.binary, self.root)
        self.addCleanup(session.close)
        caps = session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {}})["capabilities"]
        self.assertFalse(caps["documentLinkProvider"]["resolveProvider"])
        session.send("initialized", {})
        session.send("textDocument/didOpen", {"textDocument": {"uri": uri, "version": 1, "text": text}})
        links = session.result("textDocument/documentLink", {"textDocument": {"uri": uri}})
        self.assertEqual(links, [{"range": location(text, "helper"), "target": local.as_uri()}])

    def test_diagnostics_stay_owned_by_document_and_close_clears(self):
        first = self.open("i32 main() { return missing_value; }", "first.ag")
        diagnostics = self.session.diagnostics[first]["diagnostics"]
        self.assertTrue(diagnostics)
        self.open("i32 second() { return 0; }", "second.ag")
        self.assertEqual(self.session.diagnostics[first]["diagnostics"], diagnostics)
        self.session.send("textDocument/didClose", {"textDocument": {"uri": first}})
        self.assertEqual(self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": first}}), [])
        self.assertEqual(self.session.diagnostics[first]["diagnostics"], [])

    def test_incomplete_code_keeps_local_completion_and_signature_help(self):
        text = "i32 add(i32 left, i32 right) { return left + right; }\ni32 main() { i32 local = 1; return add(local, "
        uri = self.open(text)
        params = {"textDocument": {"uri": uri}, "position": position(text, len(text))}
        signature = self.session.result("textDocument/signatureHelp", params)
        self.assertEqual(signature["activeParameter"], 1)
        completion = self.session.result("textDocument/completion", params)
        self.assertIn("local", {item["label"] for item in completion["items"]})
        self.assertEqual(self.session.result("textDocument/definition", self.at(uri, text, "local", text.index("return add")))["range"], location(text, "local"))

    def test_rename_rejects_capture(self):
        text = "i32 main() { i32 first = 1; i32 second = 2; return first + second; }\n"
        uri = self.open(text)
        response = self.session.request("textDocument/rename", {**self.at(uri, text, "first", text.index("return")), "newName": "second"})
        self.assertEqual(response["error"]["code"], -32602)

    def test_watched_files_refresh_closed_imports(self):
        text = "import helper;\ni32 main() { return answer(42); }\n"
        uri = self.open(text)
        params = self.at(uri, text, "answer")
        helper = self.root / "helper.ag"
        helper.write_text("// moved\n// again\ni32 answer(i32 value) { return value; }\n")
        self.session.send("workspace/didChangeWatchedFiles", {"changes": [{"uri": helper.as_uri(), "type": 2}]})
        self.assertEqual(self.session.result("textDocument/definition", params)["range"]["start"]["line"], 2)
        helper.unlink()
        self.session.send("workspace/didChangeWatchedFiles", {"changes": [{"uri": helper.as_uri(), "type": 3}]})
        self.assertIsNone(self.session.result("textDocument/definition", params))

    def test_selective_import_alias_has_independent_rename_identity(self):
        text = "import helper { answer as calculate };\ni32 main() { return calculate(42); }\n"
        uri = self.open(text)
        params = self.at(uri, text, "calculate", text.index("return"))
        definition = self.session.result("textDocument/definition", params)
        self.assertEqual(definition["uri"], (self.root / "helper.ag").as_uri())
        edit = self.session.result("textDocument/rename", {**params, "newName": "compute"})
        self.assertEqual(set(edit["changes"]), {uri})
        self.assertEqual(len(edit["changes"][uri]), 2)
        signature = self.session.result("textDocument/signatureHelp", {"textDocument": {"uri": uri}, "position": position(text, text.index("42"))})
        self.assertEqual(signature["signatures"][0]["parameters"][0]["label"], "i32 value")

    def test_generics_and_private_import_visibility(self):
        (self.root / "helper.ag").write_text("private i32 hidden() { return 1; }\ni32 answer(i32 value) { return value; }\n")
        self.session.send("workspace/didChangeWatchedFiles", {"changes": [{"uri": (self.root / "helper.ag").as_uri(), "type": 2}]})
        text = "import helper;\nT identity<T>(T value) { return value; }\ni32 main() { return hidden(); }\n"
        uri = self.open(text)
        definition = self.session.result("textDocument/definition", self.at(uri, text, "T value"))
        self.assertEqual(definition["range"], location(text, "T", text.index("<")))
        self.assertEqual(self.session.result("textDocument/definition", self.at(uri, text, "T identity"))["range"], definition["range"])
        self.assertIsNone(self.session.result("textDocument/definition", self.at(uri, text, "hidden")))
        completions = self.session.result("textDocument/completion", self.at(uri, text, "hidden"))
        self.assertNotIn("hidden", {item["label"] for item in completions["items"]})

    def test_protocol_errors_and_string_request_ids(self):
        self.assertEqual(self.session.request("unknown/request", {})["error"]["code"], -32601)
        self.assertEqual(self.session.request("initialize", {"capabilities": {}})["error"]["code"], -32600)
        text = "i32 main() { return 0; }\n"
        uri = self.open(text)
        self.session.send("textDocument/hover", self.at(uri, text, "main"), "request-string-id")
        response = self.session.messages.get(timeout=10)
        self.assertEqual(response["id"], "request-string-id")
        self.assertIn("main", response["result"]["contents"]["value"])
        self.session.send_raw(b'{"jsonrpc":')
        self.assertEqual(self.session.messages.get(timeout=10)["error"]["code"], -32700)
        self.session.send_raw(b'{"jsonrpc":"1.0","id":3,"method":"initialized"}')
        self.assertEqual(self.session.messages.get(timeout=10)["error"]["code"], -32600)
        invalid = self.at(uri, text, "main")
        invalid["position"]["character"] = 4.5
        self.assertEqual(self.session.request("textDocument/hover", invalid)["error"]["code"], -32602)

    def test_formatting_preserves_multiline_tokens_and_utf16_range(self):
        text = 'i32 main() { str text = "😀"; return 0; }  '
        uri = self.open(text)
        edits = self.session.result("textDocument/formatting", {"textDocument": {"uri": uri}, "options": {"tabSize": 4, "insertSpaces": True}})
        expected = 'i32 main() {\n    str text = "😀";\n    return 0;\n}\n'
        self.assertEqual(edits, [{"range": {"start": position(text, 0), "end": position(text, len(text))}, "newText": expected}])
        multiline = 'i32 main() { str text = "first  \n\n\nlast"; return 0; }\n'
        uri = self.open(multiline, "multiline.ag")
        params = {"textDocument": {"uri": uri}, "options": {"tabSize": 4, "insertSpaces": True}}
        formatted = apply_edits(multiline, self.session.result("textDocument/formatting", params))
        self.assertEqual(formatted, 'i32 main() {\n    str text = "first  \n\n\nlast";\n    return 0;\n}\n')
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": formatted}]})
        self.assertEqual(self.session.result("textDocument/formatting", params), [])

    def test_formatting_options_ranges_comments_and_incomplete_code(self):
        text = 'i32 first(){/* keep  \n  comment */return 1;}\r\ni32 second(){return 2;}\r\n'
        uri = self.open(text)
        params = {"textDocument": {"uri": uri}, "options": {"tabSize": 2, "insertSpaces": True}}
        selection = {"start": position(text, 0), "end": position(text, text.index("i32 second"))}
        edits = self.session.result("textDocument/rangeFormatting", dict(params, range=selection))
        self.assertTrue(edits)
        for edit in edits:
            self.assertGreaterEqual((edit["range"]["start"]["line"], edit["range"]["start"]["character"]), (0, 0))
            self.assertLessEqual((edit["range"]["end"]["line"], edit["range"]["end"]["character"]), (2, 0))
        formatted = apply_edits(text, edits)
        self.assertEqual(formatted, 'i32 first() {\r\n  /* keep  \n  comment */ return 1;\r\n}\r\ni32 second(){return 2;}\r\n')
        tabs = self.session.result("textDocument/formatting", dict(params, options={"tabSize": 4, "insertSpaces": False}))
        self.assertIn('\r\n\treturn 2;\r\n', apply_edits(text, tabs))
        self.assertEqual(self.session.request("textDocument/formatting", dict(params, options={"tabSize": 0, "insertSpaces": True}))["error"]["code"], -32602)
        uri = self.open('i32 main(){return 0;}', "no-newline.ag")
        params["textDocument"]["uri"] = uri
        params["options"]["insertFinalNewline"] = False
        self.assertEqual(apply_edits('i32 main(){return 0;}', self.session.result("textDocument/formatting", params)), 'i32 main() {\n  return 0;\n}')
        uri = self.open('i32 main() {\nreturn 0;\n', "incomplete-format.ag")
        params["textDocument"]["uri"] = uri
        self.assertEqual(self.session.result("textDocument/formatting", params), [])
        detached = '/// Detached documentation.\n\ni32 main(){return 0;}'
        uri = self.open(detached, "detached-docs.ag")
        params["textDocument"]["uri"] = uri
        self.assertNotIn("Detached documentation", self.session.result("textDocument/hover", self.at(uri, detached, "main"))["contents"]["value"])
        formatted = apply_edits(detached, self.session.result("textDocument/formatting", params))
        self.assertEqual(formatted, '/// Detached documentation.\n\ni32 main() {\n  return 0;\n}')
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": formatted}]})
        self.assertNotIn("Detached documentation", self.session.result("textDocument/hover", self.at(uri, formatted, "main"))["contents"]["value"])

    def test_multi_range_formatting_merges_selections_and_validates_every_utf16_range(self):
        self.assertTrue(self.capabilities["documentRangeFormattingProvider"]["rangesSupport"])
        text = 'i32 first(){return 1;}\r\ni32 middle(){return 2;}\r\ni32 last(){str s="😀";return 3;}\r\n'
        uri = self.open(text)
        first = {"start": position(text, 0), "end": position(text, text.index("i32 middle"))}
        last = {"start": position(text, text.index("i32 last")), "end": position(text, len(text))}
        params = {"textDocument": {"uri": uri}, "options": {"tabSize": 2, "insertSpaces": True}, "ranges": [last, first, first]}
        edits = self.session.result("textDocument/rangesFormatting", params)
        self.assertEqual(apply_edits(text, edits), 'i32 first() {\r\n  return 1;\r\n}\r\ni32 middle(){return 2;}\r\ni32 last() {\r\n  str s = "😀";\r\n  return 3;\r\n}\r\n')
        self.assertEqual(edits, self.session.result("textDocument/rangesFormatting", dict(params, ranges=[first, last])))
        self.assertEqual(self.session.result("textDocument/rangesFormatting", dict(params, ranges=[])), [])
        split = position(text, text.index("😀")); split["character"] += 1
        invalid = self.session.request("textDocument/rangesFormatting", dict(params, ranges=[first, {"start": split, "end": split}]))
        self.assertEqual(invalid["error"]["code"], -32602)
        self.assertEqual(self.session.result("textDocument/hover", self.at(uri, text, "first"))["range"], location(text, "first"))
        indented = 'i32 first() {\r\n      return 1;\r\n}\r\n'
        uri = self.open(indented, "adjacent-ranges.ag")
        middle = {"line": 1, "character": 4}
        params.update(textDocument={"uri": uri}, ranges=[{"start": position(indented, 0), "end": middle},
            {"start": middle, "end": position(indented, len(indented))}])
        self.assertEqual(apply_edits(indented, self.session.result("textDocument/rangesFormatting", params)), 'i32 first() {\r\n  return 1;\r\n}\r\n')

    def test_on_type_formatting_indents_and_dedents_without_touching_literals(self):
        text = 'i32 main() {\nreturn 0;\n    }\n'
        uri = self.open(text)
        params = {"textDocument": {"uri": uri}, "position": {"line": 1, "character": 9}, "ch": ";", "options": {"tabSize": 2, "insertSpaces": True}}
        edits = self.session.result("textDocument/onTypeFormatting", params)
        self.assertEqual(apply_edits(text, edits), 'i32 main() {\n  return 0;\n    }\n')
        params.update(position={"line": 2, "character": 5}, ch="}")
        self.assertEqual(apply_edits(text, self.session.result("textDocument/onTypeFormatting", params)), 'i32 main() {\nreturn 0;\n}\n')
        uri = self.open('i32 main() {\nstr s = "first\n  last";\n}', "literal-indent.ag")
        params.update(textDocument={"uri": uri}, position={"line": 2, "character": 3}, ch="\n")
        self.assertEqual(self.session.result("textDocument/onTypeFormatting", params), [])
        uri = self.open('i32 main() {\nstr s = "first\n  ', "incomplete-literal.ag")
        params.update(textDocument={"uri": uri}, position={"line": 2, "character": 2}, ch="\n")
        self.assertEqual(self.session.result("textDocument/onTypeFormatting", params), [])
        uri = self.open('i32 main() {\n', "typing.ag")
        params.update(textDocument={"uri": uri}, position={"line": 1, "character": 0}, ch="\n")
        self.assertEqual(apply_edits('i32 main() {\n', self.session.result("textDocument/onTypeFormatting", params)), 'i32 main() {\n  ')

    def test_disk_sources_reject_invalid_encoding_without_exposing_truncated_symbols(self):
        for name, content in [("invalid-utf8.ag", b'i32 leaked() { return 0; } //\xff'),
                              ("invalid-nul.ag", b'i32 leaked() { return 0; }\x00i32 hidden(){}')]:
            path = self.root / name
            path.write_bytes(content)
            reply = self.session.request("textDocument/documentSymbol", {"textDocument": {"uri": path.as_uri()}})
            self.assertEqual(reply["error"]["code"], -32602)
        text = 'i32 restored() { return 1; }'
        path = self.root / "restored.ag"
        path.write_text(text)
        uri = self.open(text, path.name)
        self.assertIsNotNone(self.session.result("textDocument/hover", self.at(uri, text, "restored")))
        path.write_bytes(b'i32 leaked() {} //\xf0')
        self.session.send("textDocument/didClose", {"textDocument": {"uri": uri}})
        self.assertEqual(self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}}), [])
        path.write_text(text)
        self.session.send("workspace/didChangeWatchedFiles", {"changes": [{"uri": uri, "type": 2}]})
        self.assertIsNotNone(self.session.result("textDocument/hover", self.at(uri, text, "restored")))

    def test_installed_sysroot_source_import(self):
        with tempfile.TemporaryDirectory(prefix="silver-sysroot-") as directory:
            sysroot = Path(directory)
            standard = sysroot / "include" / "silver" / "std"
            standard.mkdir(parents=True)
            source = standard / "sample.ag"
            source.write_text("i32 system_answer() { return 42; }\n")
            session = Session(self.binary, self.root, env=os.environ | {"SILVER_SYSROOT": str(sysroot)})
            try:
                session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {}})
                session.send("initialized", {})
                text = "import std.sample;\ri32 main() { return system_answer(); }\r"
                uri = (self.root / "system.ag").as_uri()
                session.send("textDocument/didOpen", {"textDocument": {"uri": uri, "languageId": "silver", "version": 1, "text": text}})
                definition = session.result("textDocument/definition", self.at(uri, text, "system_answer"))
                self.assertEqual(definition["uri"], source.as_uri())
                self.assertIn("i32 main", session.result("textDocument/hover", self.at(uri, text, "main"))["contents"]["value"])
            finally:
                session.close()
        if self.stdlib_root is not None:
            environment = os.environ.copy()
            environment.pop("SILVER_SYSROOT", None)
            session = Session(self.binary, self.root, env=environment)
            try:
                session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {}})
                session.send("initialized", {})
                text = "import std.string;\ni32 main() { String message = String.new(); return 0; }\n"
                uri = (self.root / "installed.ag").as_uri()
                session.send("textDocument/didOpen", {"textDocument": {"uri": uri, "languageId": "silver", "version": 1, "text": text}})
                definition = session.result("textDocument/definition", self.at(uri, text, "String message"))
                self.assertEqual(definition["uri"], (self.stdlib_root / "string.ag").as_uri())
            finally:
                session.close()

    def test_counter_scope_does_not_escape_loop(self):
        text = "i32 main() { i32 i = 8; for (i32 i = 0; i < 2; i += 1) { @println(\"{}\", i); } return i; }\n"
        uri = self.open(text)
        params = self.at(uri, text, "i;", text.index("return"))
        self.assertEqual(self.session.result("textDocument/definition", params)["range"], location(text, "i", text.index("i = 8")))
        refs = self.session.result("textDocument/references", {**params, "context": {"includeDeclaration": True}})
        self.assertEqual(len(refs), 2)

    def test_cancelled_index_request_and_bounded_pipeline(self):
        # The cancelled request must not consume the whole workspace before a
        # later document request can run. Reusing an ID must not inherit a cancel.
        for index in range(160):
            (self.root / f"bulk{index}.ag").write_text("\n".join(
                f"i32 symbol{index}_{item}() {{ return {item}; }}" for item in range(24)))
        session = Session(self.binary, self.root)
        try:
            session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {}})
            session.send("initialized", {})
            messages = [
                {"jsonrpc": "2.0", "id": "cancel-me", "method": "workspace/symbol", "params": {"query": "symbol"}},
                {"jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": "cancel-me"}},
            ]
            frames = []
            for message in messages:
                body = json.dumps(message).encode()
                frames.append(f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
            session.process.stdin.write(b"".join(frames))
            session.process.stdin.flush()
            self.assertEqual(session.wait_response("cancel-me")["error"]["code"], -32800)
            session.send("textDocument/hover", {
                "textDocument": {"uri": (self.root / "helper.ag").as_uri()},
                "position": {"line": 1, "character": 5},
            }, "cancel-me")
            self.assertIn("The answer.", session.wait_response("cancel-me")["result"]["contents"]["value"])
            for index in range(96):
                session.send("textDocument/documentSymbol", {"textDocument": {"uri": (self.root / "helper.ag").as_uri()}}, f"pipeline-{index}")
            for index in range(96):
                self.assertEqual(session.wait_response(f"pipeline-{index}")["result"][0]["name"], "answer")
            # A late/unknown cancel must not accumulate a cancelled-ID cache.
            session.send("$/cancelRequest", {"id": "future"})
            session.send("textDocument/documentSymbol", {"textDocument": {"uri": (self.root / "helper.ag").as_uri()}}, "future")
            self.assertEqual(session.wait_response("future")["result"][0]["name"], "answer")
        finally:
            session.close()

    def test_exit_stops_reader_with_incomplete_following_frame(self):
        session = Session(self.binary, self.root)
        try:
            session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {}})
            session.result("shutdown")
            body = json.dumps({"jsonrpc": "2.0", "method": "exit"}).encode()
            # Queue both frames before a fast server can exit.
            session.process.stdin.write(f"Content-Length: {len(body)}\r\n\r\n".encode() + body
                                        + b"Content-Length: 100\r\n\r\n{")
            session.process.stdin.flush()
            session.process.wait(timeout=5)
            self.assertEqual(session.process.returncode, 0)
        finally:
            session.close()

    def test_diagnostics_invalidate_dependents_without_republishing_unrelated_files(self):
        main = "import helper;\ni32 main() { return answer(1); }\n"
        main_uri = self.open(main)
        unrelated = self.open("i32 separate() { return 2; }\n", "separate.ag")
        helper_text = "i32 answer(i32 value) { return value; }\n"
        helper_uri = self.open(helper_text, "helper.ag")
        self.session.notifications.clear()
        self.session.send("textDocument/didChange", {"textDocument": {"uri": helper_uri, "version": 2},
                          "contentChanges": [{"text": helper_text.replace("answer", "renamed")} ]})
        self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": helper_uri}})
        published = {message["params"]["uri"] for message in self.session.notifications
                     if message.get("method") == "textDocument/publishDiagnostics"}
        self.assertEqual(published, {helper_uri, main_uri})
        self.assertNotIn(unrelated, published)
        self.assertTrue(self.session.diagnostics[main_uri]["diagnostics"])

    def test_type_declaration_and_implementation_navigation(self):
        model = ("trait Measure { i32 size(&self); }\n"
                 "struct Shape { i32 side; }\n"
                 "type Alias = Shape;\n"
                 "impl Measure for Shape { i32 size(&self) { return self.side; } }\n")
        model_uri = self.open(model, "model.ag")
        other = "import model;\nstruct Other { i32 side; }\nimpl Measure for Other { i32 size(&self) { return self.side; } }\n"
        other_uri = self.open(other, "implementation.ag")
        text = "import model;\ni32 main() { Shape shape; Alias alias; return shape.size(); }\n"
        uri = self.open(text)
        actual_type = self.session.result("textDocument/typeDefinition", self.at(uri, text, "shape.size"))
        self.assertEqual(actual_type, {"uri": model_uri, "range": location(model, "Shape")})
        alias_type = self.session.result("textDocument/typeDefinition", self.at(uri, text, "alias;"))
        self.assertEqual(alias_type, {"uri": model_uri, "range": location(model, "Alias")})
        underlying = self.session.result("textDocument/typeDefinition", self.at(model_uri, model, "Alias"))
        self.assertEqual(underlying, actual_type)
        declaration = self.session.result("textDocument/declaration", self.at(uri, text, "size"))
        self.assertEqual(declaration, {"uri": model_uri, "range": location(model, "size")})
        implemented_method = location(model, "size", model.index("impl"))
        definition = self.session.result("textDocument/definition", self.at(uri, text, "size"))
        self.assertEqual(definition, {"uri": model_uri, "range": implemented_method})
        implementations = self.session.result("textDocument/implementation", self.at(model_uri, model, "size"))
        self.assertCountEqual(implementations, [
            {"uri": model_uri, "range": implemented_method},
            {"uri": other_uri, "range": location(other, "size")},
        ])
        types = self.session.result("textDocument/implementation", self.at(model_uri, model, "Measure"))
        self.assertCountEqual(types, [
            {"uri": model_uri, "range": location(model, "Shape", model.index("impl"))},
            {"uri": other_uri, "range": location(other, "Other", other.index("impl"))},
        ])
        shape_impls = self.session.result("textDocument/implementation", self.at(model_uri, model, "Shape"))
        self.assertEqual(shape_impls, [{"uri": model_uri, "range": location(model, "Shape", model.index("impl"))}])
        unknown = self.session.result("textDocument/typeDefinition", self.at(uri, text, "main"))
        self.assertIsNone(unknown)  # Builtin i32 has no source type declaration.

    def test_ambiguous_methods_do_not_choose_an_arbitrary_implementation(self):
        text = ("trait First { i32 read(&self); }\ntrait Second { i32 read(&self); }\nstruct Value {}\n"
                "impl First for Value { i32 read(&self) { return 1; } }\n"
                "impl Second for Value { i32 read(&self) { return 2; } }\n"
                "i32 main() { Value value; return value.read(); }\n")
        uri = self.open(text)
        params = self.at(uri, text, "read", text.index("return value"))
        self.assertIsNone(self.session.result("textDocument/definition", params))
        self.assertIsNone(self.session.result("textDocument/declaration", params))
        self.assertEqual(self.session.result("textDocument/implementation", params), [])
        rename = self.session.request("textDocument/rename", {**self.at(uri, text, "read"), "newName": "fetch"})
        self.assertEqual(rename["error"]["code"], -32602)

    def test_new_unsaved_module_participates_in_import_resolution(self):
        text = "import unsaved;\ni32 main() { return fresh(1); }\n"
        uri = self.open(text)
        self.assertTrue(self.session.diagnostics[uri]["diagnostics"])
        module = "i32 fresh(i32 value) { return value; }\n"
        imported = self.open(module, "unsaved.ag")
        params = self.at(uri, text, "fresh")
        self.assertEqual(self.session.result("textDocument/definition", params),
                         {"uri": imported, "range": location(module, "fresh")})
        self.assertEqual(self.session.diagnostics[uri]["diagnostics"], [])
        self.session.send("textDocument/didClose", {"textDocument": {"uri": imported}})
        self.assertIsNone(self.session.result("textDocument/definition", params))
        self.assertTrue(self.session.diagnostics[uri]["diagnostics"])

    def test_call_hierarchy_groups_call_sites_and_rejects_stale_items(self):
        text = ("import helper { answer as compute };\n"
                "i32 leaf(i32 value) { return answer(value); }\n"
                "i32 entry() { answer; i32 total = compute(1) + answer(2); return leaf(total); }\n")
        uri = self.open(text)
        item = self.session.result("textDocument/prepareCallHierarchy", self.at(uri, text, "entry"))[0]
        outgoing = self.session.result("callHierarchy/outgoingCalls", {"item": item})
        self.assertEqual({call["to"]["name"]: call["fromRanges"] for call in outgoing}, {
            "answer": [location(text, "compute", text.index("i32 entry")), location(text, "answer", text.index("answer(2)"))],
            "leaf": [location(text, "leaf", text.index("i32 entry"))],
        })
        answer = next(call["to"] for call in outgoing if call["to"]["name"] == "answer")
        incoming = self.session.result("callHierarchy/incomingCalls", {"item": answer})
        self.assertEqual({call["from"]["name"]: len(call["fromRanges"]) for call in incoming},
                         {"leaf": 1, "entry": 2, "other": 1})
        self.assertEqual(answer["uri"], (self.root / "helper.ag").as_uri())
        self.assertEqual(self.session.request("callHierarchy/outgoingCalls", {"item": {"uri": uri}})["error"]["code"], -32602)
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2},
                          "contentChanges": [{"text": "// shifted\n" + text}]})
        self.assertEqual(self.session.request("callHierarchy/outgoingCalls", {"item": item})["error"]["code"], -32801)

    def test_type_hierarchy_follows_trait_implementations(self):
        text = ("trait Measure { i32 size(&self); }\nstruct Shape {}\nstruct Other {}\n"
                "impl Measure for Shape { i32 size(&self) { return 1; } }\n"
                "impl Measure for Other { i32 size(&self) { return 2; } }\n"
                "i32 main() { Shape shape; return shape.size(); }\n")
        uri = self.open(text)
        shape = self.session.result("textDocument/prepareTypeHierarchy", self.at(uri, text, "shape;"))[0]
        self.assertEqual(shape["name"], "Shape")
        self.assertEqual(shape["selectionRange"], location(text, "Shape"))
        supers = self.session.result("typeHierarchy/supertypes", {"item": shape})
        self.assertEqual([item["name"] for item in supers], ["Measure"])
        subtypes = self.session.result("typeHierarchy/subtypes", {"item": supers[0]})
        self.assertEqual([item["name"] for item in subtypes], ["Shape", "Other"])
        self.assertEqual(self.session.result("typeHierarchy/subtypes", {"item": shape}), [])

    def test_semantic_token_deltas_round_trip_and_ranges_use_utf16(self):
        text = '// 😀 marker\r\ni32 main() { i32 value = 1; return value; }\r\n'
        uri = self.open(text)
        document = {"textDocument": {"uri": uri}}
        original = self.session.result("textDocument/semanticTokens/full", document)
        unchanged = self.session.result("textDocument/semanticTokens/full/delta", {**document, "previousResultId": original["resultId"]})
        self.assertEqual(unchanged, {"resultId": original["resultId"], "edits": []})
        updated = "// inserted\r\n" + text.replace("value = 1", "value = 123")
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2},
                          "contentChanges": [{"text": updated}]})
        delta = self.session.result("textDocument/semanticTokens/full/delta", {**document, "previousResultId": original["resultId"]})
        reconstructed = original["data"][:]
        for edit in reversed(delta["edits"]):
            reconstructed[edit["start"]:edit["start"] + edit["deleteCount"]] = edit.get("data", [])
        complete = self.session.result("textDocument/semanticTokens/full", document)
        self.assertEqual(reconstructed, complete["data"])
        self.assertEqual(delta["resultId"], complete["resultId"])
        self.assertNotEqual(original["resultId"], complete["resultId"])
        fallback = self.session.result("textDocument/semanticTokens/full/delta", {**document, "previousResultId": "unknown"})
        self.assertEqual(fallback, complete)
        start = updated.index("return") + 2
        end = updated.index("value;", updated.index("return")) + 3
        result = self.session.result("textDocument/semanticTokens/range", {
            **document, "range": {"start": position(updated, start), "end": position(updated, end)}})
        data = result["data"]
        self.assertEqual(len(data), 10)
        first = position(updated, start)
        second = position(updated, end - 3)
        self.assertEqual(data[:5], [first["line"], first["character"], 4, 12, 0])
        self.assertEqual(data[5:], [0, second["character"] - first["character"], 3, 7, 0])
        split = position(updated, updated.index("😀"))
        split["character"] += 1
        invalid = self.session.request("textDocument/semanticTokens/range", {**document, "range": {"start": split, "end": split}})
        self.assertEqual(invalid["error"]["code"], -32602)

    def test_semantic_tokens_invalidate_when_an_import_changes(self):
        text = "import helper;\ni32 main() { return answer(1); }\n"
        uri = self.open(text)
        document = {"textDocument": {"uri": uri}}
        previous = self.session.result("textDocument/semanticTokens/full", document)
        self.open("i32 answer = 1;\n", "helper.ag")
        delta = self.session.result("textDocument/semanticTokens/full/delta", {**document, "previousResultId": previous["resultId"]})
        complete = self.session.result("textDocument/semanticTokens/full", document)
        reconstructed = previous["data"][:]
        for edit in reversed(delta["edits"]):
            reconstructed[edit["start"]:edit["start"] + edit["deleteCount"]] = edit.get("data", [])
        self.assertEqual(reconstructed, complete["data"])
        self.assertNotEqual(previous["data"], complete["data"])

    def test_trait_method_rename_updates_contract_implementations_and_calls(self):
        text = ("trait Readable { i32 read(&self); }\nstruct First {}\nstruct Second {}\n"
                "impl Readable for First { i32 read(&self) { return 1; } }\n"
                "impl Readable for Second { i32 read(&self) { return 2; } }\n"
                "i32 main() { First first; Second second; return first.read() + second.read(); }\n")
        uri = self.open(text)
        edit = self.session.result("textDocument/rename", {**self.at(uri, text, "read"), "newName": "fetch"})
        offsets = []
        start = 0
        while (start := text.find("read", start)) >= 0:
            offsets.append(start)
            start += len("read")
        self.assertEqual(edit["changes"][uri], [
            {"range": {"start": position(text, offset), "end": position(text, offset + 4)}, "newText": "fetch"}
            for offset in offsets])
        concrete = self.session.result("textDocument/rename", {
            **self.at(uri, text, "read", text.index("return first")), "newName": "fetch"})
        self.assertEqual(concrete, edit)
        updated = apply_edits(text, edit["changes"][uri])
        self.session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2},
                          "contentChanges": [{"text": updated}]})
        self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}})
        self.assertEqual(self.session.diagnostics[uri]["diagnostics"], [])
        declared = self.session.result("textDocument/declaration", self.at(uri, updated, "fetch", updated.index("return first")))
        self.assertEqual(declared, {"uri": uri, "range": location(updated, "fetch")})
        conflict = text.replace("return 1; } }", "return 1; } i32 fetch(&self) { return 3; } }")
        self.open(conflict, version=3)
        rejected = self.session.request("textDocument/rename", {**self.at(uri, conflict, "read"), "newName": "fetch"})
        self.assertEqual(rejected["error"]["code"], -32602)

    def test_client_formats_flat_symbols_and_versioned_workspace_edits(self):
        session = Session(self.binary, self.root)
        try:
            session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {
                "workspace": {"workspaceEdit": {"documentChanges": True}},
                "textDocument": {"hover": {"contentFormat": ["plaintext", "markdown"]}},
            }})
            session.send("initialized", {})
            text = "import helper;\nstruct Box { i32 member; }\ni32 main() { return answer(1); }\n"
            uri = (self.root / "client.ag").as_uri()
            session.send("textDocument/didOpen", {"textDocument": {
                "uri": uri, "languageId": "silver", "version": 7, "text": text}})
            params = self.at(uri, text, "answer")
            hover = session.result("textDocument/hover", params)
            self.assertEqual(hover["contents"]["kind"], "plaintext")
            self.assertNotIn("```", hover["contents"]["value"])
            self.assertIn("The answer.", hover["contents"]["value"])
            self.assertNotIn("version", session.diagnostics[uri])
            flat = session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}})
            member = next(symbol for symbol in flat if symbol["name"] == "member")
            self.assertEqual(member["containerName"], "Box")
            self.assertEqual(member["location"], {"uri": uri, "range": location(text, "member")})
            self.assertNotIn("children", member)
            completion = session.result("textDocument/completion", {**params, "position": position(text, text.index("answer") + 3)})
            answer = next(item for item in completion["items"] if item["label"] == "answer")
            self.assertEqual(answer["documentation"]["kind"], "plaintext")
            signature = session.result("textDocument/signatureHelp", {**params, "position": position(text, text.index("1);"))})
            self.assertEqual(signature["signatures"][0]["documentation"]["kind"], "plaintext")
            edit = session.result("textDocument/rename", {**params, "newName": "compute"})
            self.assertNotIn("changes", edit)
            versions = {change["textDocument"]["uri"]: change["textDocument"]["version"] for change in edit["documentChanges"]}
            self.assertEqual(versions, {uri: 7, (self.root / "helper.ag").as_uri(): None, (self.root / "other.ag").as_uri(): None})
        finally:
            session.close()

    def test_request_progress_cancellation_and_completion_are_balanced(self):
        for index in range(64):
            (self.root / f"progress{index}.ag").write_text("\n".join(
                f"i32 symbol{index}_{item}() {{ return {item}; }}" for item in range(256)))
        session = Session(self.binary, self.root)
        try:
            session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {}})
            session.send("initialized", {})
            token = "\x00work😀"
            session.send("workspace/symbol", {"query": "symbol", "workDoneToken": token}, "long-index")
            started = session.messages.get(timeout=30)
            self.assertEqual(started["method"], "$/progress")
            self.assertEqual(started["params"]["token"], token)
            self.assertEqual(started["params"]["value"]["kind"], "begin")
            self.assertTrue(started["params"]["value"]["cancellable"])
            session.send("window/workDoneProgress/cancel", {"token": token})
            cancelled = session.wait_response("long-index")
            self.assertEqual(cancelled["error"]["code"], -32800)
            ended = [message for message in session.notifications if message.get("method") == "$/progress"]
            self.assertEqual([message["params"]["value"]["kind"] for message in ended], ["end"])
            self.assertEqual(ended[0]["params"]["token"], token)
            session.notifications.clear()
            self.assertEqual(session.result("workspace/symbol", {"query": "no-such-symbol", "workDoneToken": 9}), [])
            progress = [message["params"] for message in session.notifications if message.get("method") == "$/progress"]
            self.assertEqual([message["value"]["kind"] for message in progress], ["begin", "end"])
            self.assertEqual([message["token"] for message in progress], [9, 9])
            session.notifications.clear()
            invalid = session.request("workspace/symbol", {"query": "answer", "workDoneToken": []})
            self.assertEqual(invalid["error"]["code"], -32602)
            self.assertFalse(any(message.get("method") == "$/progress" for message in session.notifications))
        finally:
            session.close()

    def test_pull_diagnostics_cache_dependency_changes_and_refresh_acknowledgements(self):
        session = Session(self.binary, self.root)
        try:
            initialized = session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {
                "textDocument": {"diagnostic": {}, "publishDiagnostics": {"relatedInformation": True}},
                "workspace": {"diagnostics": {"refreshSupport": True}, "semanticTokens": {"refreshSupport": True}},
            }})
            self.assertTrue(initialized["capabilities"]["diagnosticProvider"]["interFileDependencies"])
            session.send("initialized", {})
            text = "import helper;\ni32 main() { return answer(1); }\n"
            uri = (self.root / "pull.ag").as_uri()
            document = {"textDocument": {"uri": uri}, "identifier": "silver"}
            session.send("textDocument/didOpen", {"textDocument": {"uri": uri, "languageId": "silver", "version": 1, "text": text}})
            initial = session.result("textDocument/diagnostic", document)
            self.assertEqual(initial["kind"], "full")
            self.assertEqual(initial["items"], [])
            unchanged = session.result("textDocument/diagnostic", {**document, "previousResultId": initial["resultId"]})
            self.assertEqual(unchanged, {"kind": "unchanged", "resultId": initial["resultId"]})
            workspace = session.result("workspace/diagnostic", {"previousResultIds": []})
            reports = {report["uri"]: report for report in workspace["items"]}
            self.assertEqual(reports[uri]["version"], 1)
            previous = [{"uri": report["uri"], "value": report["resultId"]} for report in workspace["items"]]
            repeated = session.result("workspace/diagnostic", {"previousResultIds": previous})
            self.assertTrue(all(report["kind"] == "unchanged" for report in repeated["items"]))
            refresh = [message for message in session.notifications if message.get("method", "").endswith("/refresh")]
            self.assertEqual({message["method"] for message in refresh}, {"workspace/diagnostic/refresh", "workspace/semanticTokens/refresh"})
            for request in refresh:
                session.respond(request["id"])
            session.result("textDocument/diagnostic", document)
            session.notifications.clear()
            helper = (self.root / "helper.ag").as_uri()
            session.send("textDocument/didOpen", {"textDocument": {"uri": helper, "languageId": "silver", "version": 1,
                         "text": "i32 different(i32 value) { return value; }\n"}})
            changed = session.result("textDocument/diagnostic", {**document, "previousResultId": initial["resultId"]})
            self.assertEqual(changed["kind"], "full")
            self.assertNotEqual(changed["resultId"], initial["resultId"])
            self.assertTrue(any("answer" in item["message"] for item in changed["items"]))
            self.assertTrue(any(message.get("method") == "workspace/diagnostic/refresh" for message in session.notifications))
            self.assertFalse(any(message.get("method") == "textDocument/publishDiagnostics" for message in session.notifications))
            session.send("textDocument/didClose", {"textDocument": {"uri": helper}})
            restored = session.result("textDocument/diagnostic", document)
            self.assertEqual(restored["items"], [])
            self.assertEqual(session.request("workspace/diagnostic", {})["error"]["code"], -32602)
            self.assertEqual(session.request("textDocument/diagnostic", {**document, "identifier": "other"})["error"]["code"], -32602)
            orphan = (self.root / "deleted.ag").as_uri()
            cleared = session.result("workspace/diagnostic", {"previousResultIds": [{"uri": orphan, "value": "old"}]})
            self.assertIn({"uri": orphan, "version": None, "kind": "full", "items": []}, cleared["items"])
        finally:
            session.close()

    def test_client_refresh_requests_coalesce_without_losing_later_changes(self):
        session = Session(self.binary, self.root)
        try:
            session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {"workspace": {"semanticTokens": {"refreshSupport": True}}}})
            session.send("initialized", {})
            uri = (self.root / "refresh.ag").as_uri()
            text = "i32 main() { return 1; }\n"
            session.send("textDocument/didOpen", {"textDocument": {"uri": uri, "languageId": "silver", "version": 1, "text": text}})
            document = {"textDocument": {"uri": uri}}
            session.result("textDocument/documentSymbol", document)
            refresh = lambda: [message for message in session.notifications if message.get("method") == "workspace/semanticTokens/refresh"]
            self.assertEqual(len(refresh()), 1)
            first = refresh()[0]["id"]
            session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": text.replace("1", "2")}]})
            session.result("textDocument/documentSymbol", document)
            self.assertEqual(len(refresh()), 1)
            session.respond(first)
            session.result("textDocument/documentSymbol", document)
            self.assertEqual(len(refresh()), 2)
            self.assertNotEqual(refresh()[1]["id"], first)
            session.respond(refresh()[1]["id"], error={"code": -32601, "message": "Unsupported"})
            session.result("textDocument/documentSymbol", document)
            session.send("textDocument/didChange", {"textDocument": {"uri": uri, "version": 3}, "contentChanges": [{"text": text}]})
            session.result("textDocument/documentSymbol", document)
            self.assertEqual(len(refresh()), 2)
        finally:
            session.close()

    def test_save_republishes_overlay_diagnostics_or_requests_pull_refresh(self):
        text = 'i32 main() { return missing(); }\n'
        uri = self.open(text, version=7)
        original = self.session.diagnostics[uri]
        self.assertTrue(any("missing" in item["message"] for item in original["diagnostics"]))
        (self.root / "main.ag").write_text('i32 disk_only() { return 0; }\n')
        self.session.notifications.clear()
        self.session.send("textDocument/didSave", {"textDocument": {"uri": uri}})
        symbols = self.session.result("textDocument/documentSymbol", {"textDocument": {"uri": uri}})
        self.assertEqual([symbol["name"] for symbol in symbols], ["main"])
        published = [message["params"] for message in self.session.notifications
                     if message.get("method") == "textDocument/publishDiagnostics"]
        self.assertEqual(published, [original])
        session = Session(self.binary, self.root)
        self.addCleanup(session.close)
        session.result("initialize", {"rootUri": self.root.as_uri(), "capabilities": {
            "textDocument": {"diagnostic": {}}, "workspace": {"diagnostics": {"refreshSupport": True}}}})
        session.send("initialized", {})
        session.send("textDocument/didOpen", {"textDocument": {"uri": uri, "languageId": "silver", "version": 7, "text": text}})
        params = {"textDocument": {"uri": uri}}
        initial = session.result("textDocument/diagnostic", params)
        refreshes = [message for message in session.notifications if message.get("method") == "workspace/diagnostic/refresh"]
        self.assertEqual(len(refreshes), 1)
        session.respond(refreshes[0]["id"])
        session.result("textDocument/documentSymbol", params)
        session.notifications.clear()
        session.send("textDocument/didSave", params)
        unchanged = session.result("textDocument/diagnostic", {**params, "previousResultId": initial["resultId"]})
        self.assertEqual(unchanged, {"kind": "unchanged", "resultId": initial["resultId"]})
        self.assertEqual([message["method"] for message in session.notifications], ["workspace/diagnostic/refresh"])

    def test_transport_rejects_invalid_utf8_nul_suffixes_and_noninteger_request_ids(self):
        for body in [b'{"jsonrpc":"2.0","id":77,"method":"bad\xff"}',
                     b'{"jsonrpc":"2.0","id":78,"method":"unknown"}\x00{"extra":true}']:
            self.session.send_raw(body)
            response = self.session.messages.get(timeout=30)
            self.assertEqual(response["error"]["code"], -32700)
            self.assertIsNone(response["id"])
        for request_id in [None, 0.5, True, [], {}, 2147483648]:
            body = json.dumps({"jsonrpc": "2.0", "id": request_id, "method": "workspace/symbol", "params": {"query": "answer"}}).encode()
            self.session.send_raw(body)
            response = self.session.messages.get(timeout=30)
            self.assertEqual(response["error"]["code"], -32600)
            self.assertIsNone(response["id"])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--server", type=Path, required=True)
    parser.add_argument("--stdlib-root", type=Path, help="Also check discovery of an installed stdlib without SILVER_SYSROOT")
    args, rest = parser.parse_known_args()
    NativeLsp.binary = args.server.resolve(strict=True)
    if args.stdlib_root is not None:
        NativeLsp.stdlib_root = args.stdlib_root.resolve(strict=True)
    unittest.main(argv=[__file__, *rest])
