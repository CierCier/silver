#!/usr/bin/env python3
"""Integration test for self-hosted aglsp (Silver Language Server)."""

import json
import subprocess
import sys

def make_msg(payload: dict) -> bytes:
    body = json.dumps(payload).encode("utf-8")
    header = f"Content-Length: {len(body)}\r\n\r\n".encode("utf-8")
    return header + body

def read_msg(proc: subprocess.Popen) -> dict:
    header = b""
    while b"\r\n\r\n" not in header:
        chunk = proc.stdout.read(1)
        if not chunk:
            raise EOFError("server closed stdout")
        header += chunk
    
    # parse Content-Length
    lines = header.decode("utf-8").split("\r\n")
    length = 0
    for line in lines:
        if line.startswith("Content-Length:"):
            length = int(line.split(":")[1].strip())
            break
    
    body = proc.stdout.read(length)
    return json.loads(body.decode("utf-8"))

def main():
    server_bin = sys.argv[1] if len(sys.argv) > 1 else "/tmp/silver-selfhost/aglsp-stage1"
    proc = subprocess.Popen(
        [server_bin],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    try:
        # 1. Test initialize
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {}
        }))
        proc.stdin.flush()

        resp = read_msg(proc)
        print("1. Initialize response:", resp)
        assert resp["id"] == 1
        assert "capabilities" in resp["result"]
        caps = resp["result"]["capabilities"]
        assert caps["hoverProvider"] is True
        assert caps["documentSymbolProvider"] is True
        assert caps["definitionProvider"] is True
        assert caps["documentFormattingProvider"] is True
        assert "completionProvider" in caps
        print("   -> Capabilities verified.")

        # 2. Test didOpen with clean Silver code
        valid_code = "i32 add(i32 a, i32 b) { return a + b; }\ni32 main() { return add(1, 2); }"
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": "file:///test.ag",
                    "languageId": "silver",
                    "version": 1,
                    "text": valid_code
                }
            }
        }))
        proc.stdin.flush()

        # Server should publish diagnostics (0 errors for valid code)
        diag = read_msg(proc)
        print("2. Valid code diagnostics:", diag)
        assert diag["method"] == "textDocument/publishDiagnostics"
        assert diag["params"]["uri"] == "file:///test.ag"
        assert len(diag["params"]["diagnostics"]) == 0
        print("   -> 0 errors for valid code verified.")

        # 3. Test hover on symbol 'add' (line 0, col 5)
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "textDocument/hover",
            "params": {
                "textDocument": {"uri": "file:///test.ag"},
                "position": {"line": 0, "character": 5}
            }
        }))
        proc.stdin.flush()

        hover_resp = read_msg(proc)
        print("3. Hover response:", hover_resp)
        assert hover_resp["id"] == 2
        assert "contents" in hover_resp["result"]
        assert "add" in hover_resp["result"]["contents"]["value"]
        print("   -> Hover on symbol verified.")

        # 4. Test documentSymbol
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "id": 3,
            "method": "textDocument/documentSymbol",
            "params": {
                "textDocument": {"uri": "file:///test.ag"}
            }
        }))
        proc.stdin.flush()

        sym_resp = read_msg(proc)
        print("4. Document symbols response:", sym_resp)
        assert sym_resp["id"] == 3
        sym_names = [s["name"] for s in sym_resp["result"]]
        assert "add" in sym_names
        assert "main" in sym_names
        print("   -> Document symbols verified:", sym_names)

        # 5. Test definition (Go to definition of 'add' in main: line 1, character 20)
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "id": 4,
            "method": "textDocument/definition",
            "params": {
                "textDocument": {"uri": "file:///test.ag"},
                "position": {"line": 1, "character": 20}
            }
        }))
        proc.stdin.flush()

        def_resp = read_msg(proc)
        print("5. Definition response:", def_resp)
        assert def_resp["id"] == 4
        assert def_resp["result"] is not None
        assert def_resp["result"]["range"]["start"]["line"] == 0
        print("   -> Go to definition verified: points to line 0.")

        # 6. Test formatting
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "id": 5,
            "method": "textDocument/formatting",
            "params": {
                "textDocument": {"uri": "file:///test.ag"}
            }
        }))
        proc.stdin.flush()

        fmt_resp = read_msg(proc)
        print("6. Formatting response:", fmt_resp)
        assert fmt_resp["id"] == 5
        assert isinstance(fmt_resp["result"], list)
        print("   -> Formatting verified.")

        # 7. Test completion
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "id": 6,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": "file:///test.ag"},
                "position": {"line": 0, "character": 0}
            }
        }))
        proc.stdin.flush()

        comp_resp = read_msg(proc)
        print("7. Completion response items count:", len(comp_resp["result"]["items"]))
        assert comp_resp["id"] == 6
        labels = [item["label"] for item in comp_resp["result"]["items"]]
        assert "fn" in labels
        assert "i32" in labels
        assert "add" in labels
        print("   -> Completion items verified: found keywords and symbols.")

        # 8. Test didChange with an error (unknown type)
        bad_code = "UnknownType main() { return 0; }"
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {
                    "uri": "file:///test.ag",
                    "version": 2
                },
                "contentChanges": [
                    {"text": bad_code}
                ]
            }
        }))
        proc.stdin.flush()

        bad_diag = read_msg(proc)
        print("8. Error diagnostics:", bad_diag)
        assert bad_diag["method"] == "textDocument/publishDiagnostics"
        assert len(bad_diag["params"]["diagnostics"]) > 0
        err_msg = bad_diag["params"]["diagnostics"][0]["message"]
        print("   -> Caught error:", err_msg)
        assert "UnknownType" in err_msg

        # 9. Test shutdown
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "id": 7,
            "method": "shutdown",
            "params": {}
        }))
        proc.stdin.flush()
        shut_resp = read_msg(proc)
        print("9. Shutdown response:", shut_resp)
        assert shut_resp["id"] == 7

        # 10. Test exit
        proc.stdin.write(make_msg({
            "jsonrpc": "2.0",
            "method": "exit",
            "params": {}
        }))
        proc.stdin.flush()
        proc.wait(timeout=2)
        print("10. Server exited cleanly with code:", proc.returncode)
        assert proc.returncode == 0

        print("\nAll aglsp protocol tests PASSED successfully!")

    finally:
        if proc.poll() is None:
            proc.kill()

if __name__ == "__main__":
    main()
