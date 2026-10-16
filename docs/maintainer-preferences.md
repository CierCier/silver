# Maintainer preferences for Silver

Reviewed on 2026-10-01. This is a profile of how the maintainer wants work on
Silver carried out, based on local OMP and OpenCode sessions. It is not a
personality assessment or a claim about preferences in other projects.

## Coverage and interpretation

The review extracted user-role messages from all 48 top-level OMP session files
in the two Silver workspace directories and all 6 top-level Silver OpenCode
sessions in the local database. Together they contain 877 user-role records,
743 from OMP and 134 from OpenCode, dated 2026-07-14 through 2026-10-01.
OMP session headers also confirmed that no other top-level session directory
contained a Silver workspace session. The separate pi history had no
Silver-specific workspace directory.

The old lowercase workspace path and the current `Projects/silver` path were
both included. OpenCode was queried read-only. Child-agent task prompts,
assistant summaries, tool output, greetings, model switches, and automatically
generated troubleshooting prompts were not treated as personal preferences.
Some conversations launched work in Atelier or desktop configuration from a
Silver session. Those supplied context, not authority to inspect another repo.

Recurring explicit instructions have high confidence. A single explicit choice
is recorded with its scope. Interpretations are labeled as such. Later explicit
corrections supersede earlier requests; the active user's instructions supersede
this profile. Session identifiers below allow local verification without
publishing raw transcripts, private machine details, or credentials.

## Execution and collaboration

The maintainer expects agents to finish the authorized objective. Repeated
"continue", "one by one", and "fix the issues first" messages support steady
progress through the existing backlog, with blockers resolved before adding
more features. Planning-only and stop instructions are equally explicit and
must be honored. [E01](#e01) [E02](#e02)

Think before editing, especially when language semantics or architecture change.
Then keep the design small and implement it. The maintainer has challenged both
arbitrary edits and prolonged overthinking. KISS and YAGNI recur in both tools.
This means a complete solution to the actual requirement, with few moving parts.
It does not justify a hack or a partially working substitute. [E03](#e03)

Ask about consequential API and syntax choices. Show concrete examples and
tradeoffs, and use a grilling session when requested. Routine work should not
require repeated permission to continue. Architectural second opinions and
external reviews have been requested often, but delegation preferences changed.
The latest OMP correction says one process is enough. Default to a single agent
unless the current task asks otherwise. [E04](#e04) [E05](#e05)

Use relevant installed skills and available tools. Historical choices of AGY,
Greptile, OpenCode, and specific models are evidence of interest in independent
review, not a permanent requirement that an unavailable endpoint must be used.
Report missing review capability honestly. [E05](#e05) [E06](#e06)

## Communication

Lead with what changed or what is currently true, then the remaining gap and
next action. The maintainer frequently asks for status, implementation trees,
API inventories, and comparison tables. Use a table for parallel comparisons
and a tree for module structure. Explain the actual API, not just file names.
[E07](#e07)

Keep routine progress concise. Give substantial detail when asked for a design
comparison, complete rundown, or explanation. The most recent formatting
request favors numbered steps, clear current state, and an obvious next action.
Treat this as an output preference, not an invitation to publish personal
information. [E08](#e08)

Inference from repeated corrections: concrete evidence is more useful than
reassurance. Show the output, benchmark, executable behavior, or precise missing
capability. Do not call a smaller projection "complete" or make the user discover
that a passing test used a different execution path. [E09](#e09)

## Code and architecture

Reuse stdlib functions, traits, syscall shims, and generic machinery. The
maintainer explicitly objects to repeated casts, magic numbers, duplicated code,
and unnecessary wrappers. Small cohesive modules and narrow imports are recurring
preferences; large files should be split by responsibility. [E10](#e10)

Keep reusable compiler code in `libs/agc` and executable driver code in `bin/agc`.
Use nested package manifests and real `agc` workspace commands. This allows LSP
and module tooling to consume the same compiler library. These are concrete
Silver architecture choices, not a demand for a new abstraction per feature.
[E11](#e11)

Fix compiler limitations when they cause awkward APIs or repeated stdlib
workarounds. State the missing capability and why it matters first. The
maintainer views real applications as feedback for the compiler and stdlib;
changes are welcome when within the active task. [E12](#e12)

Prefer structured errors with useful data for recoverable failures. Use enums
for meaningful variants and outcomes instead of unexplained global integer
constants. The explicit uppercase `SYSCALL.EXIT` choice applies to syscall
constants; it does not establish uppercase naming for every enum. [E13](#e13)

## Silver syntax and API ergonomics

Read `AGENTS.md` and `SYNTAX.md` before working on Silver code. The maintainer
repeatedly corrects invented Rust declarations, trait syntax, casts, visibility,
and function pointers. Confirm current syntax in source and runnable examples.
Historical requests to remove `let` or use `priv` are not current specifications;
the current syntax reference supports inferred `let` bindings and `private`.
[E14](#e14)

Silver should feel like Silver. Go's HTTP APIs, Python's string and slicing
ergonomics, and Rust's iterator/collection APIs are references for usability,
not a mandate to copy another language's syntax. Ask when a new public contract
has several plausible spellings. [E04](#e04) [E15](#e15)

Prefer ergonomic generic APIs and trait-based dispatch. Infer types where the
available context makes them unambiguous rather than forcing users to repeat
generic arguments or casts. Use existing trait checks for formatting and other
cross-type operations. Keep failure and ownership behavior explicit. [E16](#e16)

## Ownership, safety, and performance

Memory correctness is a recurring priority: return-value moves, chained
temporaries, container cleanup, partial initialization, and real leaks all
received explicit attention. Prefer borrow references for caller-owned state;
raw pointers remain appropriate for manual memory and FFI contracts. Avoid
adding verbose lifetime syntax without discussing its ergonomic cost. [E17](#e17)

Struct field cleanup should be automatic after the outer destructor. Explicit
field drops inside that destructor are not the preferred ownership contract.
The compiler reference and contributor guide were corrected against the
stage0 field cascade and `tests/cascade_drop_test.ag`. Pointer pointees and enum
payloads with a custom enum destructor have different cleanup contracts.
[E18](#e18)

"Zero cost" means avoiding needless overhead, not forbidding every allocation.
The maintainer explicitly accepts hidden string allocations when ownership and
RAII release them correctly. Buffered I/O, fewer syscalls, data locality, fast
imports, and compiler/linker speed are repeated concerns. Measure the actual
cost and preserve semantics. [E19](#e19)

Benchmark changes before and after on comparable workloads. Comparative reports
should use at least 10 runs and the median. For latency comparisons the maintainer
requested p1, p5, p10, p50, p95, and p99.
Differences in concurrency or execution models must be visible. Include measured
before/after results in performance commit descriptions. [E20](#e20)

Keep the Silver runtime and stdlib freestanding by default. This does not mean
all external libraries must be statically linked or libc interop is forbidden:
later instructions explicitly allow optional vendor libc and shared-library
integration. Keep that dependency out of the stdlib's default contract. [E21](#e21)

## Testing and completion evidence

Tests must validate behavior, not whether a symbol or source string exists.
Compile and execute programs, check output as well as exit status, cover error
paths, and verify cleanup. Expected aborts must be distinguished from crashes
that a harness accidentally accepts. Tests and examples both matter. [E22](#e22)

For compiler and stdlib changes, run compiler tests and the integration suite
before committing. Test the self-hosted compiler after meaningful migration
steps, using actual package commands. Frontend acceptance parity, native command
bridging, stage1 native execution, and stage2 self-compilation are different
claims and need different evidence. [E09](#e09) [E23](#e23)

The migration goal is functional parity with stage0, including cache correctness,
speed, usability, tests, and examples. CLI presentation may change if functionality
remains. This is a target, not a statement that the current tree has achieved it.
The request for modern `#[test]` functions occurred during TUI library work; it
does not authorize replacing Silver's compiler integration harness. [E23](#e23)

## Git, review, and repository hygiene

Atomic commits are the strongest recurring workflow preference. Complete and
verify one coherent change before starting another. Use meaningful behavior-based
subjects and descriptions, without numbered phases or model coauthor trailers.
One mixed commit was explicitly allowed as an exception; it does not replace
the repeated atomic-commit preference. [E24](#e24)

Work on branches and publish PRs when authorized. Allow requested reviews to
finish, verify findings, and address valid ones. The maintainer normally chooses
when to merge, but sometimes explicitly delegates merging. A previous session's
push or merge instruction does not authorize publishing a later task. [E06](#e06)

Keep permanent docs synchronized with real behavior. Comments explain reasons
and constraints, not a long narration of the code. Public docstrings should
describe usable contracts concisely. Educational examples should be simple,
accurate, and executable. [E25](#e25)

Temporary plans stay out of commits and PR text. Root `todo.md` and `handoff.md`
are local and ignored. Track issues with useful status and evidence; avoid
duplicate progress documents and scattered markdown clutter. The current request
explicitly authorizes this durable helper documentation and profile. [E26](#e26)

## Environment and context-specific preferences

Use the existing Nix/direnv environment and temporary Nix shells for missing
tools. Dev-only servers, Go comparison programs, and test runners belong in dev
dependencies, not runtime package dependencies. Discover executables and compiler
include paths rather than assuming a conventional Linux filesystem layout.
Use `rg` and host edit tools. [E27](#e27)

For terminal or UI work, session-local preferences include real interactive demos,
live system data, controlled update cadence, redraws limited to changed regions,
and visual verification in an actual terminal. Named themes and chart styles
were Atelier design choices; they are not Silver-wide UI rules. [E28](#e28)

## Instructions that changed

| Earlier instruction | Later correction and current interpretation |
| --- | --- |
| Frequent subagent orchestration and architectural second opinions | Latest OMP request favors one process. Follow current task-specific delegation instructions. |
| Update checked-in bootstrap binaries in a separate commit | August sessions explicitly removed that workflow. Current Rust bootstrap source under `bootstrap/` still exists and is built with Cargo. |
| No libc linking at all | Later sessions allow optional vendor libc and external dynamic libraries while keeping std freestanding. |
| Remove inferred `let`; visibility spelled `priv` | Language evolved. Consult current `SYNTAX.md`, lexer, and tests. |
| Go-like API references | Later clarification requires a natural Silver result, not wholesale imitation. |
| Separate plans and many migration progress documents | Latest correction keeps only needed durable docs and ignored root trackers. |
| User normally merges | An explicit instruction to merge can delegate that action for the active task. |

## Evidence index

OMP locators are session UUID plus JSONL line. OpenCode locators are session ID
plus user message ID. Dates below refer to the messages, not session creation.
These are short excerpts or summaries of user instructions, not agent conclusions.

- <a id="e01"></a>E01: OpenCode `ses_f1716e5deffeFBKOFV8dnBtROx`, `msg_0e8f21f66001LGOO0kxsGCvWr9` and `msg_0f2b44d2b001JrTGuhRJ019c9Z`, 2026-09-28/30. "complete each one by one" and "fix the issues first".
- <a id="e02"></a>E02: OMP `01a0ec78-4549-722f-a345-27cd51fe0305`, line 3999, 2026-09-29. "just update the todos, the handoff and stop". OMP `019fb1ad-d7b5-7000-a50f-f94a0bec8d7f`, line 6342, 2026-08-02, explicitly asks for todos without starting work.
- <a id="e03"></a>E03: OMP `019f5f82-8f7b-7000-9be2-8f504e3cecd7`, line 2502, 2026-07-14, asks for design before arbitrary changes. OMP `019fae40-5e63-7000-8dd5-06cec55b22dd`, line 1348, 2026-07-29, rejects complications and shortcuts. OpenCode `ses_f31246700ffe9lbp6x4icTVUu9`, `msg_0d2484496001boXL9SfYBEscT5`, 2026-09-24, asks for KISS/YAGNI rather than overthinking.
- <a id="e04"></a>E04: OMP `01a0441f-78a0-7325-abc4-ac276803279e`, line 1821, 2026-08-28, asks for natural Silver syntax and grilling when unsure. OMP `01a05883-e323-767f-a2b5-ea479e928566`, line 82, 2026-08-31, requests grilling for syntax/API choices.
- <a id="e05"></a>E05: OMP `019f8406-e8da-7000-909d-c6c8c063c8b8`, line 439, 2026-07-21, requests architectural second opinions. OMP `01a0ec78-4549-722f-a345-27cd51fe0305`, lines 760/766, 2026-09-29, says a single process is enough and suggests background OpenCode only if another agent is needed.
- <a id="e06"></a>E06: OMP `019f853e-d8de-7000-9a1e-8934ca61c8f7`, lines 12/310, 2026-07-21, requests branches, PRs, reviews, and user-controlled merging. OMP `019f7662-b571-7000-a3ea-22bbf272d759`, line 1649, 2026-07-19, asks to allow 5-10 minutes for review. OMP `01a0a3dc-d5a3-720c-896a-5eafed5a66be`, line 528, 2026-09-15, explicitly delegates merging.
- <a id="e07"></a>E07: OMP `019fd30c-73ed-7000-b650-9be999f34f6c`, lines 107/370/372, 2026-08-06, asks for a status table and an API tree with Unicode drawing.
- <a id="e08"></a>E08: OpenCode `ses_f1716e5deffeFBKOFV8dnBtROx`, `msg_0f2ce08b4001dHWQVLBZZ3Wzt4`, 2026-09-30, asks for detailed setup/next-step guidance and the accessible-output skill. OMP `019fb1ad-d7b5-7000-a50f-f94a0bec8d7f`, line 6330, 2026-08-02, requests detailed pros and cons.
- <a id="e09"></a>E09: OpenCode `ses_f31246700ffe9lbp6x4icTVUu9`, `msg_0d1f4390c001E9yUuzkPDCKWQm`, 2026-09-24, reports a built compiler failing at startup and challenges missing runtime testing. Messages `msg_0d75e81b5001spve5Hkeum6J4r` and `msg_0d7603e60001dgWz28zLt9XjaM`, 2026-09-25, ask how much stage1 still depends on Rust and to remove that dependence.
- <a id="e10"></a>E10: OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, lines 61/4179, 2026-08-09/12, asks for cohesive files and reuse of existing stdlib functions without casts and magic numbers. OMP `019fad32-38e5-7000-b471-4a0a8c59dd2b`, line 538, 2026-07-29, objects to a 10k-line codegen unit.
- <a id="e11"></a>E11: OpenCode `ses_f31246700ffe9lbp6x4icTVUu9`, `msg_0d21133e4001BkvzFzj80WRuXO` and `msg_0d22c4f88001NJBCtxcJg14R3I`, 2026-09-24, specifies nested manifests, `bin/agc`, `libs/agc`, and a driver-only binary.
- <a id="e12"></a>E12: OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, line 12394, 2026-08-15, asks to identify missing compiler features instead of awkward workarounds. OpenCode `ses_f4a90348dffeDsLk7PFXTzcJrb`, `msg_0b812f6ee001I9yUpEWzywfzSs`, 2026-09-19, describes applications feeding back into compiler/stdlib improvements.
- <a id="e13"></a>E13: OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, lines 143/4904/9236, 2026-08-09/12/14, requests enums for variants and Results with actual errors. OMP `019f7662-b571-7000-a3ea-22bbf272d759`, line 2365, 2026-07-20, chooses uppercase syscall constants.
- <a id="e14"></a>E14: OpenCode `ses_f4a90348dffeDsLk7PFXTzcJrb`, `msg_0b57200d1001pDLBEyQbS1DM0Y`, 2026-09-18, requires reading agent/syntax files first. OpenCode `ses_f31246700ffe9lbp6x4icTVUu9`, `msg_0d24acc91001mSDDHsCDDkz4XX`, 2026-09-24, corrects invented Silver syntax.
- <a id="e15"></a>E15: OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, line 5672, 2026-08-13, requests familiar Go-style HTTP APIs. OMP `01a05883-e323-767f-a2b5-ea479e928566`, line 1011, 2026-08-31, proposes Python-inspired slicing. OMP `01a0441f-78a0-7325-abc4-ac276803279e`, line 1821, 2026-08-28, explicitly distinguishes Silver from Rust/Go.
- <a id="e16"></a>E16: OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, line 5038, 2026-08-12, requests contextual generic inference. OpenCode `ses_f503926d5ffe1DPfA4iB11zHvs`, `msg_0afcc6e55001q385bS5PNJ71xk` and `msg_0afcca0da001U5n3Jh5fkYQkxx`, 2026-09-17, requests existing trait checks and Display.
- <a id="e17"></a>E17: OMP `019fad32-38e5-7000-b471-4a0a8c59dd2b`, lines 255/288, 2026-07-29, asks whether chains leak and to inspect generated LLVM. OMP `019fb1ad-d7b5-7000-a50f-f94a0bec8d7f`, lines 7196/7372/8272, 2026-08-02/03, questions lifetime verbosity and favors borrowed stdlib methods.
- <a id="e18"></a>E18: OMP `019f761d-2683-7000-9310-c2afbb5663aa`, line 346, 2026-07-18, asks for cascading field drops. OMP `019f94d7-272d-7000-a783-785337ca83b4`, line 735, 2026-07-24, repeats that the compiler should drop inner Drop fields automatically.
- <a id="e19"></a>E19: OMP `019f94d7-272d-7000-a783-785337ca83b4`, line 2080, 2026-07-29, explicitly accepts hidden allocations under RAII. OMP `019f82e8-98e8-7000-957f-2c2f3f7b3006`, line 2222, 2026-07-21, asks for buffered I/O to minimize syscalls.
- <a id="e20"></a>E20: OMP `01a05dd6-81d0-70c8-9047-a7801421e9de`, lines 1287/1330, 2026-09-04, requests at least 10 runs, median/percentile metrics, comparable mechanisms, and before/after results in commits.
- <a id="e21"></a>E21: OMP `019fd081-08cb-7000-9307-14a52964d96b`, line 59, 2026-08-05, asks for the default Silver runtime without libc. OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, lines 5252/5353, 2026-08-12, permits external shared libraries and optional vendor libc.
- <a id="e22"></a>E22: OpenCode `ses_f4a90348dffeDsLk7PFXTzcJrb`, `msg_0bd9f07790017A625G6NDkQcdv` and `msg_0c340d86f001k7aBE9A3ixu3y7`, 2026-09-20/21, explicitly requires behavior tests instead of presence checks. OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, lines 3319/10028/10065, 2026-08-10/14, challenges misleading exit success and distinguishes an expected abort.
- <a id="e23"></a>E23: OMP `01a05dd6-81d0-70c8-9047-a7801421e9de`, line 630, 2026-09-04, requires compiler/integration tests before commits. OpenCode `ses_f31246700ffe9lbp6x4icTVUu9`, `msg_0d2443ec6001nqUf7XLJHxhBiF` and `msg_0d740ddd60015lEGE7CtbNm1Qa`, 2026-09-24/25, requires repeated self-host testing and functional parity while allowing presentation changes.
- <a id="e24"></a>E24: OMP `019f5f67-3eaf-7000-8334-c49236b1f320`, line 256, 2026-07-14, requests atomic commits. OMP `01a05883-e323-767f-a2b5-ea479e928566`, line 80, 2026-08-31, rejects phase-number subjects. OMP `01a0441f-78a0-7325-abc4-ac276803279e`, line 2581, 2026-08-28, removes model coauthor trailers. OpenCode `ses_f4a90348dffeDsLk7PFXTzcJrb`, `msg_0b573e2f0001G2Q0oyGUZd32e6`, 2026-09-18, requests meaningful unnumbered atomic commits. OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, line 5030, 2026-08-12, explicitly allows one mixed commit.
- <a id="e25"></a>E25: OMP `01a0441f-78a0-7325-abc4-ac276803279e`, lines 2734/3366/3375, 2026-08-28, requests concise comments and docstrings, with comments explaining why. OMP `019fad32-38e5-7000-b471-4a0a8c59dd2b`, line 245, 2026-07-29, asks for an educational string example.
- <a id="e26"></a>E26: OMP `01a0a910-2c0f-7779-adef-ff2692193de5`, line 602, 2026-09-16, keeps private plans out of commits and mentions. OMP `01a0ec78-4549-722f-a345-27cd51fe0305`, lines 4028/4077, 2026-09-29, removes markdown clutter and keeps root todo/handoff out of Git. OpenCode `ses_f1716e5deffeFBKOFV8dnBtROx`, `msg_0e8e91a24001JLyZ8iaGu0L8NT`, 2026-09-28, requests a structured issue tracker.
- <a id="e27"></a>E27: OMP `019f94d7-272d-7000-a783-785337ca83b4`, lines 1269/1442, 2026-07-25, requires ripgrep and careful manual edits. OMP `019fe766-52ff-7000-885c-fec8ce95bb12`, line 9002, 2026-08-13, requires edit tools instead of Python rewrites. OMP `01a0a910-2c0f-7779-adef-ff2692193de5`, lines 1574/1612, 2026-09-16, distinguishes dev dependencies and requires direnv setup.
- <a id="e28"></a>E28: OpenCode `ses_f4a90348dffeDsLk7PFXTzcJrb`, `msg_0b8c78fa9001Lk8hKmCcRu8mMm`, `msg_0c47d4b64001iUYr5Ab7bnUvlX`, and `msg_0c48fc658001BTsazWr2ykwznP`, 2026-09-19/21, asks for changed-region redraws, separate interactive demos, and live system data.

The retired bootstrap-binary workflow is explicitly removed in OMP
`019fb1ad-d7b5-7000-a50f-f94a0bec8d7f`, lines 11216/11584, 2026-08-05.
