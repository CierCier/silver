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

### M0 — Scaffold & harness
- [ ] Create `elise/{core,lex,parse}` crates; add to workspace members
- [ ] agc gains path dependency on elise crates
- [ ] Corpus script enumerating all repo `.ag` files (38k lines / 269 files)
- [ ] Criterion bench harness wired to the corpus (`agc/benches/corpus.rs`)
- [ ] **Baseline captured** → PERFLOG.md
- [ ] Wire optimized bench profile so absolute numbers are meaningful
- Gate: `cargo bench -p agc` runs end-to-end; baseline recorded

### M1 — Silver lexer in elise-lex
- [ ] Byte-class DFA-style runtime + flat transition tables
- [ ] Perfect-hash keyword lookup; contextual keyword groups
- [ ] **Preprocessing hook pipeline**: `Preprocess` trait (source-level
      transform + token/trivia filter), default no-op with zero overhead;
      registered via `LexPipeline::with_hook(...)`
- [ ] Full Silver lexical spec: identifiers, decimal/hex ints, floats with
      exponents, complex suffix (`3.5i`), strings/chars with escapes, all
      keywords incl. multi-char operators, lifetimes (`'a`), attributes
      (`#[...]`), nested block comments
- [ ] SIMD skips behind feature flag (whitespace/string/comment runs)
- Gate: differential test — token streams identical to current `agc`
  lexer on every repo file, zero diffs (boundaries, text, token class)

### M2 — Source graph core (elise-core)
- [ ] Green tree: arena-backed, position-independent, structural hashing
- [ ] Red layer: parent cursors
- [ ] Trivia side-list integrated into lossless text reconstruction
- [ ] Event fold: Enter/Advance/Exit → green tree
- Gate: fuzz-clean lossless round-trip under random input mutation

### M3 — Items, types, statements (agc/src/grammar)
- [ ] silver.elise: all top-level items (imports incl. selective braces,
      structs/enums/trait decls, impls incl. trait impls + cast members,
      extern blocks/decls with ABI strings, globals, type aliases, macros,
      attributes)
- [ ] Type grammar: primitives, pointers/references/slices, postfix array
      declarators, generics, function types, lifetime generics
- [ ] Statement grammar: C-style + inferred lets, defer, control flow,
      block statements
- [ ] Word-group context fed per-file (declared typenames) for statement
      dispatch
- Gate: every repo file parses; per-file top-level item count matches the
  legacy parser exactly

### M4 — Expressions
- [ ] Pratt table matching SYNTAX.md §6 exactly (12 levels, `..` at
      relational level, ternary + unwrap-or sharing `?`)
- [ ] Contextual disambiguation: `(Type)expr` casts vs parenthesized
      expressions; `Vec<i32>` generic args vs `<` comparison in postfix
      position (bounded speculation, memoized on success)
- [ ] Match arms with patterns (`Circle(r)`, `move v`, `_`) and guards;
      brace initializers (designated/positional) restricted to initializer
      position
- Gate: zero ERROR nodes across the entire corpus; round-trip holds

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
