# Elise Performance Log

Every milestone gate requires a perf review: run the corpus benches, record
numbers here, and compare against the previous entry. Regressions beyond
noise (±5%) need documented justification or a revert.

Method: `cargo bench -p agc` (criterion, 5s measurement per bench).
Throughput is reported by criterion as MiB/s over corpus bytes.
The `pipeline` benches measure lex + parse end-to-end — that number is the
headline comparison for elise.

---

## Baseline — legacy hand-written parser

- **Date**: 2026-08-25
- **Commit**: `5f1f1c5` (pre-elise)
- **Machine**: Intel Core 5 210H, 8 threads, Linux
- **Corpus**: 269 files (`std/`, `tests/`, `examples/`), 1,261,570 bytes,
  38,001 lines
- **Build**: cargo debug-profile bench binary (unoptimized — see note)

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

> Note: criterion benches run on the dev (unoptimized) profile here because
> the workspace has no release-bench wiring yet; both elise and this baseline
> will be re-measured identically at each gate so comparisons stay valid.
> Switching to `--profile profiling`-style optimized benches is an M0 exit
> item so the absolute numbers become meaningful against the <50 ms/file
> ship-gate target.

### Per-line reference points

- Legacy lex:    ~2.2 µs/file-line-equivalent → 70 MiB/s
- Legacy parse:  ~1.5 µs/line incremental over lex → 20.7 MiB/s
- Legacy total:  ~1.7 µs/line → 585k lines/sec
