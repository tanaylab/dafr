# Slice 4 — mid-slice checkpoint (end of Phase F)

**Date:** 2026-04-21.
**Purpose:** Hand off to a fresh Claude Code session to execute Phase C
(chains, 9 tasks) + Phase T (contracts, 6 tasks) + Phase Z (exit gate,
2 tasks) without dragging Phase P + F context along.

## What landed (Phases P + F, 12 commits on package repo + 1 on dev repo)

**Branch:** `slice-4-perf-chains-contracts` on
`/home/aviezerl/src/dafr-native/`. NOT merged to `main`; Z2 at the end
of the slice handles the fast-forward merge and tag.

**Package-repo commits (oldest first on the branch):**

```
4e354b0 fix(kernels): wire dafr.omp_threshold through (was orphaned)            [P1]
840ed3f perf(query-eval): sparsity-preserving Log eps: 1 on dgCMatrix ...       [P2]
0e663a1 perf(query-eval): bare default reductions route to rowSums ...          [P3]
d1c087c perf(query-eval): fused kernel_log_reduce_{dense,csc} ...               [P4]
072e119 fix(query-eval): fused dense path coerces integer matrix to double ...  [P4 fix]
f81bbec chore(perf): add dafr.perf.fast_paths bench gate (default TRUE)         [P5]
7e0e465 fix(view_daf): reuse base daf cache env (remove dead per-view bucket)   [F1]
fe531a0 fix(query-eval): drop NA in mask output (Julia parity)                  [F2]
9410e83 feat(query-eval): IfNot records chain-lookup sentinel on state          [F3]
ea67587 feat(query-eval): AsAxis single-hop chained lookup + IfNot sentinel     [F4]
cad7cd1 feat(view_daf): axis rename propagates to vector/matrix reads           [F5]
c2fd0e0 feat(view_daf): axis filter propagates to vector/matrix reads           [F6]
```

Pre-branch base: `af842d7` (Slice 3 styler pass on `main`). Tag
`slice-3` is at `f3bcc24` (one commit behind main).

**Dev-repo commit:**

```
ec04a1f bench(slice-4-perf-wedge): wall-clock + peak RSS, fast on/off, 10K^2 + 30K^2
```

## Test + build status at checkpoint

- `testthat::test_dir("tests/testthat")` — **978 PASS / 0 FAIL / 0
  SKIP / 1 WARN**. Baseline at slice-3 tag was 939 PASS; +39 tests
  covers P2/P3/P4 perf fast paths, P4 CSC kernel + integer coercion,
  F2 NA-in-mask, F3/F4 chain lookup (3 tests × 2 expects each), F5
  rename propagation (2 tests × up to 3 expects), F6 filter
  propagation (2 tests × up to 3 expects), plus F1's view cache
  assertion. The 1 WARN is the pre-existing
  `scran::quickCluster` / `irlba::irlba` SVD tolerance notice in
  `test-altrep-downstream.R`, unchanged since Slice 0.
- `pkgbuild::compile_dll(debug = FALSE)` — clean. One new C++ file
  (`src/kernel_log_reduce.cpp`, P4) and two helpers added to
  `src/openmp_shim.h` (P4: `dafr_omp_get_thread_num` +
  `dafr_omp_get_max_threads_capped`).
- `devtools::check(error_on = "note")` — NOT run at this checkpoint.
  Should be run as part of Z1 at slice exit.

## P5 benchmark headline numbers

At 30K × 30K dgCMatrix, motif `:: UMIs % Log eps: 1 >| Sum`:

|                  | fast_paths=FALSE | fast_paths=TRUE | ratio       |
|------------------|------------------|-----------------|-------------|
| elapsed_s        | 46.43 s          | 0.0788 s        | **589×**    |
| peak_mb          | 24 973 MB        | 1.4 MB          | **17 838×** |

At 10K² the ratio is already **42× wall / 652× RSS**. The P5 stop
condition (≥5× wall / ≥10× RSS at 30K²) was massively exceeded. See
`~/src/dafr-native/dev/benchmarks/slice-4-perf-wedge-2026-04-21.csv`.

## Scope-deviation notes (read before the next session touches related areas)

**F4: bare `=@` in the parser.** The plan's F4 Step 3 said bare `=@`
was "not yet supported"; BUT its Tests 1 + 2 exercised bare `=@` and
expected results. The implementer extended the parser to accept bare
`=@` (emits `AsAxis(axis_name = NULL)`) and implemented Julia-parity
inference at evaluation time: bare `=@` resolves the target axis from
`state$property` (set by `.apply_lookup_vector`). This required
adding a `property` field to `.apply_lookup_vector`'s return, a new
`.parse_as_axis` parser, and a canonicaliser branch for NULL
`axis_name`. The plan's Tests 1 + 2 and Test 3 (explicit `=@ batch`)
all pass. Multi-hop chains remain out of scope.

**P3: TDD "failing test first" rule did NOT fire.** All 5 new tests
in `test-perf-bare-reductions.R` passed pre-implementation because
`apply()` + `sum` produces numerically equivalent output to
`rowSums`. User accepted Option 1 (accept as-landed, note in exit
gate). The tests function as correctness-equivalence regression
guards for a refactor whose actual win is wall-clock + memory
(measured in P5). Fast-path dispatch was verified by instrumentation
(Sum/Mean/Max/Min hit fast path; Count falls through). See
`feedback_slice4_p3_tdd.md` in auto-memory.

**P4 integer-matrix regression.** The code reviewer caught a hard bug
in the initial P4 commit (`d1c087c`): `cpp11::doubles_matrix<>`
requires `REALSXP`, but UMI matrices are typically integer, so the
fused dense kernel crashed on the canonical real-world input. Fixed
in `072e119` with `if (!is.double(m)) storage.mode(m) <- "double"`
in `.try_fused_log_reduce`'s dense branch, plus two regression tests
(integer-matrix + `eps:2` distinguishing test for the CSC path).

**F1 testthat/waldo gotcha.** `expect_identical(env1, env2)` in
testthat 3.3.1 uses `waldo::compare` for environments, which reports
"no differences" on two empty envs regardless of reference identity.
The plan's literal test assertion would have passed WITHOUT the fix.
Implementer used `expect_true(identical(...))` instead — tighter
reference-identity check.

**F3 plan test was malformed.** The plan's Step 1 test query
`'@ cell : metacell ?? "UNK" =@ : type'` is unparseable (extra `:`
before `=@`) and its regex expected `"UNK"` to stay quoted in
canonical form (the canonicaliser strips quotes for plain values).
Not load-bearing because F4 overwrote the test file. Flagged for the
plan author if the same snippet is reused elsewhere.

**F5/F6 behavioural change:** view-level axis indices (F6's
`view_axis_indices`) are the new source of truth for
`format_axis_length` / `format_axis_array` /
`format_get_vector` / `format_get_matrix` on ViewDaf. Identity views
(no filter, no rename) still produce `seq_along(base_entries)` so
behaviour is unchanged for the common case. Filtered views correctly
return filtered rows/cols for both dense matrices and
dgCMatrix/lgCMatrix (the `raw[r_idx, c_idx, drop = FALSE]` idiom
dispatches through Matrix's S4 `[` for both).

## What's still open (Phase C, T, Z)

**Phase C — Chains (9 tasks, C1–C9):**

- C1: Scaffold `R/chain_daf.R` with `ReadOnlyChainDaf` (`DafReadOnly`
  subclass) + `WriteChainDaf` (`DafWriter` subclass) + `chain_reader()`
  + `chain_writer()` + `.validate_chain_axes` helper. Matches Julia's
  two-class design (reader vs writer chain).
- C2: Chain reader scalar dispatch (reverse-order fall-through,
  last-wins).
- C3: Chain reader axis dispatch (union of axes across chain; axis
  consistency validated at construction).
- C4: Chain reader vector dispatch.
- C5: Chain reader matrix dispatch.
- C6: Write chain scalar set / delete (top-writer writes, delete
  errors if scalar exists in earlier daf).
- C7: Write chain axis + vector set / delete, with auto-add-axis on
  writer when vector is written to an axis that only exists in an
  earlier daf.
- C8: Write chain matrix set / delete / relayout.
- C9: Julia chain fixture via conda env `dafr-mcview`; chain round-trip
  test in `test-chain-julia-compat.R`. STOP if conda env unavailable.

All 22 `format_*` generics need method registrations on both chain
classes. Plan uses `list(ClassA, S7::class_character, ...)` S7 dispatch
signatures. `R/chain_daf.R` grows to ~700 lines.

**Phase T — Contracts (6 tasks, T1–T6):**

- T1: Scaffold `R/contracts.R` with 5 expectation string constants
  (RequiredInput / OptionalInput / CreatedOutput / GuaranteedOutput /
  OptionalOutput), `.assert_expectation` / `.assert_type` helpers, S7
  `Contract` class (`name` / `is_relaxed` / `axes` / `data` slots),
  `contract_scalar` / `contract_vector` / `contract_matrix`
  constructors.
- T2: `ContractDaf` S7 class (`DafWriter` subclass) +
  `.enforcement_enabled()` gate (env var `DAF_ENFORCE_CONTRACTS`
  OR R option `dafr.enforce_contracts`) + `contractor()` that
  returns daf unchanged when off, wraps when on.
- T3: All 22 `format_*` methods on `ContractDaf` with access-tracking
  helpers `.access_scalar/axis/vector/matrix`; relaxed-mode support;
  immutable-for-modify guards (RequiredInput / OptionalInput can't
  be written).
- T4: `verify_input(daf)` + `verify_output(daf)` — existence / type
  checks per direction + access-tracking check on output. No-op on
  non-ContractDaf.
- T5: `merge_contracts(left, right)` — Julia's `|>` port. Narrower
  type wins; incompatible output-output raises; left-wins for
  Required.
- T6: End-to-end tests: contract-wrap `MemoryDaf` + `FilesDaf`,
  simulated computation, verify input → run → verify output.

`R/contracts.R` grows to ~900 lines.

**Phase Z — Exit gate (2 tasks):**

- Z1: `devtools::document()` to regenerate NAMESPACE + man/*.Rd +
  NEWS.md Slice 4 entry. Run
  `devtools::check(error_on = "note")` — expect 0 / 0 / 0. Full
  test suite expected to remain 0 FAIL.
- Z2: Write `dev/notes/slice-4-exit.md` (following slice-3-exit.md
  structure). Fast-forward merge `slice-4-perf-chains-contracts` into
  `main`, tag `slice-4`, push. Re-ask user about the deferred L2
  upstream PR.

## Context for the fresh session

**Plan file:** `~/src/dafr-native/dev/plans/2026-04-21-slice-4-chains-contracts.md`
(untracked in dev repo; exists locally; do NOT commit to dev repo
without user permission). The relevant sections are:
- Phase C: "Task C1" through "Task C9".
- Phase T: "Task T1" through "Task T6".
- Phase Z: "Task Z1" and "Task Z2".

**Kickoff note:** `~/src/dafr-native/dev/notes/slice-4-kickoff.md`
(also untracked) has broader context on Slice 3 mines and repo
conventions.

**Slice 3 exit:** `~/src/dafr-native/dev/notes/slice-3-exit.md`
(committed) has the complete state at Slice 3 exit, including the
"Deferred to Slice 4" list Phase C / T / Z fulfils.

**Repo conventions (reinforced across P + F):**

- 4-space R indent (styler pass landed post-Slice 3).
- S7 multi-dispatch always uses `list(ClassA, ...)` signatures.
- `#' @include` directives are load-bearing — any new R file
  registering S7 methods against `format_*` generics needs
  `#' @include format_api.R` + concrete-class file includes.
- `format_get_*` returns plain arrays WITHOUT dimnames. `get_*`
  user-facing wrappers add names. Chain's `format_get_*` must honour
  this contract — drop dimnames if the underlying daf could leak
  them.
- `sort(..., method = "radix")` for all listing returns.
- `/bin/rm` and `/bin/cp` (shell has `-i` aliases).
- No emojis.
- Never `--no-verify` / `--amend` / force-push; always NEW commits.
- Native headers use `.h`, not `.hpp` (though F phase didn't touch
  C++).
- `.dafr_builtin` attribute on default ops is the identity hook used
  by P2/P3/P4. Tagged on all 10 default ops. Don't need to touch for
  C or T.

**Slice 3 mines still live:**

- Evaluator `state$kind` is a closed enum. F4 added `"vector_axis"`.
  Any new handler added in C or T must respect the extended set (now
  including `"vector_axis"`) and must not introduce a `kind` that
  implicit fall-through assumes.
- `.apply_axis` is load-bearing for the first→second-axis transition.
  Don't touch.
- Matrix cache version key uses `"rows:cols"` (colon-separated,
  axis-order dependent). Queries on the same data under flipped
  orientation don't share cache invalidation. C (chain) should use its
  own cache (fresh `new_cache_env`) — the plan says so; follow.
- Query cache key collisions: chain wrappers should NOT share a cache
  instance with their base dafs. Each chain gets its own
  `new_cache_env`. The kickoff warns about view / chain / cache
  collisions via canonical query strings.

**Julia DAF state:**

- `~/src/DataAxesFormats.jl` at `49fbba1` (origin/main as of
  handoff).
- `~/src/TanayLabUtilities.jl` at `48a4a57`.
- Both registered as Julia `dev` packages in conda env `dafr-mcview`.
- The 17-query Julia fixture (`tests/testthat/fixtures/julia-queries/`)
  was confirmed byte-identical to Slice 3. C9 + (eventually T's
  fixture) will extend this pattern.

## Ready-to-paste prompt for the next session

```
Execute Slice 4 of the native-R dafr package, continuing from the
mid-slice checkpoint.

Plan to execute: ~/src/dafr-native/dev/plans/2026-04-21-slice-4-chains-contracts.md

Context to read first (in order):
1. ~/src/dafr-native/dev/notes/slice-4-mid-slice-checkpoint.md — this note.
2. The plan's Phase C, Phase T, and Phase Z sections.

You are resuming mid-slice. Phases P (perf wedge) and F (Slice 3
follow-ups) are DONE and committed on branch
slice-4-perf-chains-contracts; do not re-do them. Pick up at Phase C
Task C1.

Branch: slice-4-perf-chains-contracts, HEAD c2fd0e0, 12 commits ahead
of main. Package repo clean. Dev repo has 1 Slice 4 commit (P5
benchmark ec04a1f).

Tag slice-4 only lands at Z2 after Phase C, T, Z all complete.

Use the superpowers:subagent-driven-development skill. All 15
remaining tasks are implementation tasks that benefit from
fresh-subagent-per-task + two-stage review.

Stop conditions (unchanged from slice start):
- Any commit needs to amend a previous commit.
- devtools::check produces any new NOTE not present at slice-3.
- C9 Julia fixture regeneration needs the dafr-mcview conda env but
  it's unavailable — defer the live regen if so; static fixture
  still tests against the committed JSON.
- Any TDD "test passes pre-implementation" surprise (precedent: P3
  was accepted via Option 1; decide case-by-case).
```

## Status at checkpoint

- Package repo `/home/aviezerl/src/dafr-native/`: branch
  `slice-4-perf-chains-contracts` at `c2fd0e0`, working tree clean.
  NOT pushed to origin. NOT merged to main. No tag yet.
- Dev repo `/home/aviezerl/src/dafr-native/dev/`: branch `main` at
  `ec04a1f`, with pre-existing unstaged `benchmarks/bake-off-results.csv`
  modification (not ours) and pre-existing untracked
  `plans/2026-04-21-slice-4-chains-contracts.md`,
  `notes/slice-4-kickoff.md`, `.a5c/`, and two Slice 0 baseline CSVs.
  This checkpoint note is the only new addition.
- Julia repos unchanged since slice-4 kickoff.
- No CI runs yet on the branch (user hasn't pushed).
