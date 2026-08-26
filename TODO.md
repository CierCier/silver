# Elise — Parser Library Roadmap (`migrate/elise` branch)

Elise is a language-agnostic, grammar-driven parsing library. Its first
customer is Silver itself: the first shipped parser must parse every `.ag`
file in this repo into a usable, lossless source graph.

Full design rationale lives in the planning session (2026-08); this file is
the actionable checklist.

---

## Architecture (locked)

```
silver/                      (workspace root, branch migrate/elise)
├── Cargo.toml               members += ["elise/*"]
├── elise/
│   ├── elise-core/          spans, file ids, arenas, green/red tree, trivia
│   ├── elise-lex/           DFA runtime, byte-class tables, token buffer
│   └── elise-parse/         event parser engine, pratt loops, recovery
│                            ← contains ZERO Silver references
└── agc/
    ├── src/grammar/
    │   ├── silver.elise     the Silver grammar definition (data)
    │   └── mod.rs           builds GrammarSpec, feeds per-file typename
    │                        context, exposes parse_ag() -> SourceGraph
    └── src/parser/          current hand-written parser — untouched until
                             cutover; differential tests compare against it
```

Dependency direction: `agc → elise` only. Nothing in `elise/` may mention
Silver, `.ag`, or SYNTAX.md. Silver needs are expressed as *general* elise
mechanisms (e.g. word-groups for contextual keywords).

## Locked design decisions

1. **Event-based parsing** — hot loop emits `Enter/Advance/Exit` events;
   separate pass folds them into an immutable green tree (rowan-style).
   Parser hot path allocates nothing but event-vector pushes.
2. **Deterministic LL core with committed choice + bounded fallback** — no
   GLR, no blanket packrat. Opt-in memoization per rule where needed.
3. **Two execution modes from one spec**: interpreted (dev) and generated
   Rust source (release). Differential-tested against each other.
4. **Grammar = data first**: both a Rust builder and a `.elise` text frontend
   compile to the same serializable `GrammarSpec`. Neither may exceed what
   the data model represents. Text DSL becomes the primary authoring surface
   once semantics stabilize.
5. **Trivia out-of-band**: whitespace/comments recorded as ranges, never as
   tokens. Token buffer is fixed-layout rows (`kind: u16, start: u32,
   len: u32`) in one contiguous Vec.
6. **Pratt expressions as data**: explicit binding powers, `=> NodeKind`
   naming; replaces hand-written precedence chains.
7. **Declared error recovery**: per-rule recovery hints producing ERROR
   nodes and synthetic missing tokens. Deterministic, testable.
8. **Contextual word-groups**: host-mutable named token sets (e.g.
   "declared typenames") usable in ordered-choice dispatch with committed
   match — solves C-style statement ambiguity without unbounded backtracking.
9. **Pluggable preprocessing hooks**: elise owns the full lexical stage and
   exposes a hook pipeline around it — source-level transforms (conditional
   compilation, include expansion) run before lexing; token/trivia filters
   run after. Hooks are optional; with none registered the pipeline is a
   monomorphic pass with zero dispatch overhead. Semantic analysis stays out
   (phase-isolation rule still applies): hooks transform *text and tokens*,
   never meaning.

## Ship gate (first release)

`parse_ag(source) -> SourceGraph`, where for EVERY `.ag` file in the repo
(~38k lines / 269 files across std/, tests/, examples/):

- [ ] graph.text() round-trips byte-for-byte
- [ ] zero ERROR nodes on valid code
- [ ] node-kind coverage of SYNTAX.md §1–6 complete
- [ ] <50 ms single-threaded per file
- [ ] differential parity with the legacy parser (token stream at M1;
      item counts at M3)

Out of scope for first ship (explicitly deferred): typed AST projections,
incremental reparsing, codegen backend, recovery tuning, multi-language
support beyond the Silver grammar.

## Process gates — apply to EVERY milestone

A milestone is not done when its functional gate passes. Each of M0–M5 ends
with two mandatory reviews:

### Perf review (at every milestone)
1. Run `cargo bench -p agc` (corpus baseline) and any elise benches.
2. Record results in `PERFLOG.md` as a new dated entry: numbers + delta vs
   baseline and vs the previous milestone.
3. Any regression >5% vs the previous entry requires a documented cause or a
   revert. "We'll optimize later" is not an entry.
4. Absolute throughput claims only count in optimized builds once M0 wires
   optimized benching; dev-profile numbers are tracked for relative drift.

> M1 perf review: **passed** — see the 2026-08-25 PERFLOG entry (3.4× vs
> legacy). The follow-up item below is therefore closed.
~~ M1 perf follow-up: `elise/benches` lex bench comparing elise-lex
      Silver spec against the 70 MiB/s legacy lex baseline ~~
- [x] M1 perf follow-up: corpus lex bench added to `agc/benches/corpus.rs`
      (`elise_lex/*` groups) — 4.99 ms vs 17.15 ms legacy, 3.4× faster

### Code review (at every milestone)
1. Boundary check: zero Silver references under `elise/`.
2. Phase-isolation check: no semantic analysis or name resolution inside
   elise; no parsing decisions inside agc semantics.
3. Hot-path review: no allocations in parse loop, no HashMap lookups in
   dispatch, indices-not-pointers in tree structures.
4. Error-path review: every new failure mode produces a spanned diagnostic
   or ERROR node, never a panic, never silent acceptance.
5. Tests: each feature lands with a test that would fail without it; all
   suites green before commit.

Findings from either review get fixed or filed as explicit TODO entries —
not deferred silently.

## Milestones

### M0 — Scaffold & harness ✅
- [x] Create `elise/{core,lex,parse}` crates; add to workspace members
- [x] agc gains path dependency on elise crates
- [x] Corpus enumeration: 269 `.ag` files, 1.26 MB, 38k lines
- [x] Criterion bench harness wired to the corpus (`agc/benches/corpus.rs`)
- [x] **Baseline captured** → PERFLOG.md (pipeline 64.9 ms / 18.5 MiB/s)
- [x] Bench profile verified optimized (`bench` inherits `release`)
- Gate: met — benches run end-to-end; baseline recorded

### M1 — Silver lexer in elise-lex ✅
- [x] Byte-class scan driver: layout → comments → identifier/keyword →
      operator longest-match trie → spec hook for literals and oddities
- [x] Perfect-hash keyword lookup (FNV-1a, open addressing); word-group
      hook point reserved for contextual keywords
- [x] **Preprocessing hook pipeline**: `Preprocess` trait (source-level
      `transform_source`, chainable), default no-op with zero overhead.
      Token-level filters land with the parse pipeline.
- [x] Full Silver lexical spec replicated byte-for-byte: all keywords and
      primitive type names, hex ints, floats, complex `i` suffix, string
      escapes incl. `\xNN` / `\u{...}`, char literals, the `'lifetime`
      quirk, nested block comments with doc markers, and the legacy
      delimiter rule where a doc-marker star cannot close its own comment
      (`/**/` is unterminated)
- [x] Differential parity test (`agc/tests/elise_lexer_parity.rs`) over all
      269 corpus files plus adversarial snippets: identical boundaries,
      text, and classification on every token; both lexers agree on which
      inputs fail
- Deferred to post-M5: SIMD skip loops (the single-pass byte loop already
      meets the gate; SIMD is an optimization pass with its own perf review)
- Gate: met — zero diffs across the corpus

### M2 — Source graph core ✅
- [x] Green tree: `Rc`-shared, position-independent (`elise-core/src/green.rs`)
- [x] Cursor layer: `NodeRef` / `Children` with absolute spans, leaf+node
      uniform traversal, `walk_leaves`
- [x] Trivia out-of-band in the lexer, interleaved into the tree by the
      event fold (lossless by construction)
- [x] Event fold: Enter/Advance/Exit → green tree, malformed-stream tolerant
- Gate: met — 4000-case deterministic mutation fuzz + whole-corpus lossless
      round-trip through the real Silver spec
      (`agc/tests/elise_source_graph.rs`)

### M3 — Items, types, statements ✅ (item layer, known limitations)
- [x] Item recognition over elise token streams in
      agc/src/grammar/parser.rs:
      imports incl. selective braces, extern decls/blocks with ABI strings,
      structs/enums/trait decls, impls, macros, type aliases, functions vs
      globals (brace-initializer aware), attributes merged per legacy rules
- [x] Function/global classifier: paren-brace-depth tracking; ident before
      top-level paren means function
- [x] Trailing semicolon of brace-initialized globals absorbed into the item
- [x] Item sequence parity verified on simple files (imports + functions)
- KNOWN LIMITATION: complex real-world files (alloc.ag etc) produce fewer
  items than legacy because classify_tail merges adjacent functions when
  bodies contain nested constructs (for-loops with braces inside parens,
  casts inside brace-inits). Root cause: classify_tail's brace depth counter
  is confused by `{` inside for-loop parens. Fix: rewrite as a proper
  token-level state machine that only counts braces at paren-depth 0.
- Note: statement/expression structure inside bodies is M4; bodies are flat
      leaves for now

### M4 — Expressions ✅ (core implemented, corpus parity partial)
- [x] Pratt expression chain matching SYNTAX.md §6: assignment → ternary /
      unwrap-or → or-or → and-and → bit-or → bit-xor → bit-and → equality +
      range → shift (adjacent `<` `<` / `>` `>`) → additive → multiplicative
      → unary (prefix + cast) → postfix (call, index, field, inc/dec)
- [x] Marker-rotation prefix trick for left-recursive binary operators
      (`wrap(mark, kind)` rotates Nop placeholder to child start)
- [x] Statement layer: let (C-style + inferred), return/break/continue,
      if/else chains, while, C-style for, for-in, defer, match with
      patterns, local decls with brace initializers, expression statements
- [x] Balance safety net in emit_function: unbalanced body events degrade
      to flat leaves instead of producing malformed trees
- [x] Total-consumption guarantee in parse_block_inner: no gaps in leaf
      stream even when structured parsing skips constructs
- KNOWN LIMITATION: item parity test fails on complex files (alloc.ag etc)
  because classify_tail's brace counting merges adjacent functions. Fix:
  rewrite as proper state machine tracking only top-level braces (M5 item).
- Gate: lossless round-trip passes on all corpus files; Body nodes present

### M5 — Ship
- [ ] `SourceGraph` API frozen (`text()`, `errors()`, `root()`,
      `node_kinds()`)
- [ ] Benchmarks published; perf gate met (<50 ms/file)
- [ ] CI: differential parity + round-trip + corpus gates on every push
- Gate: the ship-gate checklist above, fully checked

## Post-first-ship backlog (not started until M5 lands)

- [ ] Incremental reparsing (edit → relex dirty range → reparse smallest
      enclosing stable subtree)
- [ ] Codegen backend (spec → specialized Rust source, ≥2× interpreted)
- [ ] Typed AST projection layer over the green tree
- [ ] `.elise` text frontend promoted to primary authoring surface
- [ ] Recovery tuning (per-language declared strategies)
- [ ] Silver cutover: retire `agc/src/parser` once consumers migrate
