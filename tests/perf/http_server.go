// tests/perf/http_server.go — fast loopback HTTP/1.1 server for the client
// performance comparison. Serves a tiny fixed body on every GET; keep-alive
// is on (Go's default). Run it, then hammer it with the Silver, Go, and Rust
// clients and compare sustained request rates.
package main

import (
	"fmt"
	"net"
	"net/http"
)

const body = "hello"

func main() {
	ln, err := net.Listen("tcp", "127.0.0.1:18099")
	if err != nil {
		panic(err)
	}
	fmt.Println("PERF_SERVER_READY")
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain")
		w.Header().Set("Content-Length", fmt.Sprint(len(body)))
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(body))
	})
	// 127.0.0.1:18099 — dedicated port, see run_perf.sh and run_tests.sh.
	if err := http.Serve(ln, nil); err != nil {
		panic(err)
	}
}
