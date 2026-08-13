// tests/perf/client_go.go — Go HTTP client performance benchmark.
//
// Hammers the loopback server with sequential keep-alive GETs for the same
// 15-second window as the Silver and Rust clients, then prints the sustained
// request rate. Mirrors tests/http_perf_test.ag's workload: one connection,
// small body, full response read per request.
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"time"
)

func main() {
	url := "http://127.0.0.1:18099/"
	client := &http.Client{}

	// Warm up (connection establishment + server accept).
	resp, err := client.Get(url)
	if err != nil {
		fmt.Fprintln(os.Stderr, "warmup failed:", err)
		os.Exit(1)
	}
	io.Copy(io.Discard, resp.Body)
	resp.Body.Close()

	start := time.Now()
	deadline := start.Add(15 * time.Second)
	count := 0
	for time.Now().Before(deadline) {
		resp, err := client.Get(url)
		if err != nil {
			fmt.Fprintln(os.Stderr, "request failed:", err)
			break
		}
		// Consume the body so the transport can reuse the connection.
		io.Copy(io.Discard, resp.Body)
		resp.Body.Close()
		count++
	}
	elapsed := time.Since(start).Seconds()
	fmt.Printf("go: %d requests in %.2fs (%.0f req/s)\n", count, elapsed, float64(count)/elapsed)
}
