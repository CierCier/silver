# Elise Performance Log

Every milestone gate requires a perf review: run the corpus benches, record
numbers here, and compare against the previous entry. Regressions beyond
noise (±5%) need documented justification or a revert.

Method: `cargo bench -p agc` (criterion, 5s measurement per bench).
Throughput is reported by criterion as MiB/s over corpus bytes.
The `pipeline` benches measure lex + parse end-to-end — that number is the
headline comparison for elise.

## 2026-08-27 — C11 Formal Grammar & SQLite Production Benchmarks

- **Commit**: `c11.elise` formal grammar + `elise-grammar` DFA state compiler + `c-parser` production release
- **Machine**: Intel Core 5 210H, Linux; Criterion & optimized release profile.

### 1. C11 Parser vs. Clang 22 (`-fsyntax-only`)

| Target / Codebase | File Count / Lines | Byte Size | Elise (`c-parser`) | Clang 22 (`-fsyntax-only`) | Speedup vs. Clang |
|---|---|---|---|---|---|
| `hello.c` | 1 file (29 LOC) | 571 B | **0.48 ms** | 24.82 ms | **51.8× faster** |
| `sqlite3ext.h` | 1 file (730 LOC) | 38.6 KB | **0.99 ms** | 24.89 ms | **25.1× faster** |
| `sqlite3.h` (SQLite Core Header) | 1 file (13,968 LOC) | 671.8 KB | **7.97 ms** | 23.90 ms | **3.0× faster** |
| `wuffs.c` (Real-World Amalgamation) | 1 file (86,663 LOC) | 3.29 MB | **17.14 ms** | 158.51 ms | **9.2× faster** |
| **Full SQLite Repository** | **357 files (443,143 LOC)** | **14.45 MB** | **230.62 ms (~0.23s)** | **13,365.10 ms (~13.37s)** | **58.0× faster** |

Throughput on entire SQLite repository: **~59.8 MiB/s (1.92 Million lines/sec)** with **100% lossless CST round-trip** verified.

---

### 2. Silver Full Corpus Pipeline (Criterion Measurement)

| Scope | Legacy Pipeline | Elise Pipeline | Criterion Mean | Throughput | Speedup |
|---|---|---|---|---|---|
| std | 37.66 ms | 9.34 ms | **9.34 ms** | 75.0 MiB/s | **4.0× faster** |
| tests | 26.14 ms | 8.44 ms | **8.44 ms** | 53.7 MiB/s | **3.1× faster** |
| examples | 2.74 ms | 0.88 ms | **0.88 ms** | 55.2 MiB/s | **3.1× faster** |
| **all (1.26 MB / 38k lines)** | **67.48 ms** | **19.52 ms** | **19.52 ms** | **61.6 MiB/s** | **3.5× faster** |

---

## 2026-08-26 — M5 gate (Elise full pipeline vs legacy pipeline)

- **Commit**: M5 ship milestone (branch `migrate/elise`)
- **Machine**: same as baseline; `cargo bench -p agc`, optimized profile.

| Scope | Legacy Pipeline | Elise Pipeline | Delta |
|---|---|---|---|
| std | 35.50 ms | ~11.2 ms | **3.2× faster** |
| tests | 25.83 ms | ~7.8 ms | **3.3× faster** |
| examples | 2.66 ms | ~0.8 ms | **3.3× faster** |
| **all (1.26 MB)** | **64.93 ms** | **~19.8 ms** | **3.3× faster (~63.7 MiB/s)** |

Functional gate: lossless round-trip + full item & expression parity across all 269 corpus files with zero AST degradation.

Verdict: M5 ship review **passed** — Elise delivers a 3.3× end-to-end throughput speedup over the legacy parser with guaranteed lossless source graph materialization.

---

## 2026-08-25 — M1 gate (elise-lex Silver spec vs legacy lexer)

- **Commit**: `52935aa`+ (branch `migrate/elise`)
- **Machine**: same as baseline; `cargo bench -p agc`, optimized profile.

| Scope | Legacy lex | elise lex | Delta |
|---|---|---|---|
| std | 7.73 ms | 2.78 ms | **2.8× faster** |
| tests | 6.76 ms | 1.97 ms | **3.4× faster** |
| examples | 0.70 ms | 0.22 ms | **3.2× faster** |
| **all (1.26 MB)** | **17.15 ms** | **4.99 ms** | **3.4× faster (~253 MiB/s)** |

Functional gate: token streams byte-identical across all 269 files
(`agc/tests/elise_lexer_parity.rs`), including adversarial comment/escape
snippets.

Verdict: M1 perf review **passed** — no regression; elise-lex is already
3.4× ahead of the reference lexer before SIMD work.

---
## Baseline — legacy hand-written parser

- **Date**: 2026-08-25
- **Commit**: `5f1f1c5` (pre-elise)
- **Machine**: Intel Core 5 210H, 8 threads, Linux
- **Corpus**: 269 files (`std/`, `tests/`, `examples/`), 1,261,570 bytes,
  38,001 lines
- **Build**: cargo bench profile (inherits release, opt-level 3) — optimized

| Stage | Scope | Mean | Throughput |
|---|---|---|---|
| lex | std | 7.73 ms | ~97 MiB/s |
| lex | tests | 6.76 ms | ~78 MiB/s |
| lex | examples | 0.70 ms | ~85 MiB/s |
| **lex** | **all** | **17.15 ms** | **70.1 MiB/s** |
| parse | std | 32.76 ms | |
| parse | tests | 21.87 ms | |
| parse | examples | 2.36 ms | |
| **parse** | **all** | **58.00 ms** | **20.7 MiB/s** |
| pipeline | std | 35.50 ms | |
| pipeline | tests | 25.83 ms | |
| pipeline | examples | 2.66 ms | |
| **pipeline** | **all** | **64.93 ms** | **18.5 MiB/s** |

**Headline**: legacy lexer+parser processes the full 38k-line corpus in
~64.9 ms ≈ **585k lines/sec** (~18.5 MiB/s). Lexing is ~26% of pipeline cost,
parsing ~74%.

> Profile note: Cargo's `bench` profile inherits `release` (opt-level 3), so
> these are optimized numbers, directly comparable against future elise
> benches built under the same profile.

### Per-line reference points

- Legacy lex:    ~2.2 µs/file-line-equivalent → 70 MiB/s
- Legacy parse:  ~1.5 µs/line incremental over lex → 20.7 MiB/s
- Legacy total:  ~1.7 µs/line → 585k lines/sec
