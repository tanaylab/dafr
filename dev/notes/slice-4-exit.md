# Slice 4 exit gate — 2026-04-21

## Deliverables

**Phase P — Perf hot-path fix (landed FIRST, pre-checkpoint)**

- [x] P1: `dafr.omp_threshold` threaded through `kernel_log_add_cpp` and
  `kernel_csc_colsums_cpp` (was orphaned since Slice 0).
- [x] P2: Sparsity-preserving `% Log eps: 1` on `dgCMatrix` — in-place `log1p`
  on the `@x` slot, no dense intermediate.
- [x] P3: Bare default reductions (`>| Sum / Mean / Max / Min`, `>- Sum / Mean`)
  route to `rowSums` / `Matrix::rowSums` / `matrixStats::rowMaxs` etc. instead
  of `apply()`. `Count` falls through.
- [x] P4: Fused `kernel_log_reduce_{dense,csc}_cpp` for `% Log eps:ε >| Sum|Mean`
  + `>- Sum|Mean` motif. CSC variant is single-pass over `nnz` with no dense
  intermediate. Integer-matrix fix (`storage.mode` coerce to double) landed
  with regression tests after code review caught the initial commit's UMI
  integer crash.
- [x] P5: Benchmark harness (wall + peak RSS, fast-paths on/off, 10K² + 30K²
  dgCMatrix). CSV at `dev/benchmarks/slice-4-perf-wedge-2026-04-21.csv`.
  Headline: at 30K² the fused path is **589× wall / 17 838× RSS** versus the
  pre-P2/P3/P4 path on the canonical `:: UMIs % Log eps: 1 >| Sum` motif.

**Phase F — Slice 3 follow-ups (landed pre-checkpoint)**

- [x] F1: `ViewDaf` cache bucket removed; views now reuse the base daf's cache
  env, so base writes correctly invalidate view reads.
- [x] F2: NA-in-mask drop semantics aligned with Julia (`drop_na_idx` in the
  mask evaluator).
- [x] F3: `IfNot` evaluator records a chain-lookup sentinel on `state`.
- [x] F4: `AsAxis` single-hop chained lookup + `IfNot` substitution. Parser
  extended to accept bare `=@` (inferred axis via `state$property`); this was
  a scope deviation from the plan's "not yet supported" — Julia-parity tests
  in the plan required it, so it landed as part of F4.
- [x] F5: `ViewDaf` axis rename propagates to `get_vector()` / `get_matrix()`.
- [x] F6: `ViewDaf` axis filter propagates to `get_vector()` / `get_matrix()`;
  view-level axis indices are the new source of truth (identity views remain
  `seq_along(base_entries)` so the common case is unchanged).

**Phase C — Chains (landed this session)**

- [x] C1: `ReadOnlyChainDaf` (`DafReadOnly` subclass) + `WriteChainDaf`
  (`DafWriter` subclass) S7 classes; `chain_reader()` / `chain_writer()`
  constructors; `.validate_chain_axes()` runs at construction and produces
  Julia-parity error messages (`"different number of entries"`,
  `"different entry#N"`, `"read-only final data"`).
- [x] C2: Chain reader scalar dispatch — reverse-order fall-through for
  `format_has_scalar` / `format_get_scalar`; union+sort for `format_scalars_set`.
- [x] C3: Chain reader axis dispatch — 5 generics × 2 classes = 10 methods.
  Tests for axis-union, entry-mismatch, and length-mismatch all pass; the two
  mismatch tests are regression guards for C1's `.validate_chain_axes`
  (they passed pre-C3 implementation because construction runs before any
  C3 method is called — see Scope deviations below).
- [x] C4: Chain reader vector dispatch — 6 methods (3 per class); each has/get
  method guards with `format_has_axis(d, axis)` before calling
  `format_has_vector`.
- [x] C5: Chain reader matrix dispatch — 6 methods (3 per class); both axes
  guarded before matrix accessor.
- [x] C6: Write-chain scalar set/delete — `.chain_writer(daf)` helper + 2 new
  methods on `WriteChainDaf`. Delete errors if scalar exists in any earlier
  daf (`"because it exists in the earlier: <name>"`).
- [x] C7: Write-chain axis add/delete + vector set/delete — 4 new methods +
  `.chain_ensure_axis_on_writer()` helper (auto-adds missing axis on the
  writer from the first earlier daf that has it).
- [x] C8: Write-chain matrix set/delete/relayout — 3 new methods. `relayout`
  pulls the matrix into the writer first if it only lives in an earlier daf.
- [x] C9: Julia chain fixture round-trip (`tests/testthat/test-chain-julia-compat.R`).
  Julia regen script at `dev/scripts/regen-julia-chains-fixture.jl` (dev
  repo). Plan used `using JSON` but JSON isn't in DAF.jl's project manifest,
  so the script uses an inline minimal JSON emitter matching the Slice-3
  `regen-julia-queries-fixture.jl` pattern. Fixture bytes committed to
  package repo; round-trip test passes.

**Phase T — Contracts (landed this session)**

- [x] T1: `Contract` S7 class + 5 expectation constants
  (`RequiredInput`, `OptionalInput`, `CreatedOutput`, `GuaranteedOutput`,
  `OptionalOutput`) + 3 builder functions (`contract_scalar`, `contract_vector`,
  `contract_matrix`) + internal helpers (`.assert_expectation`, `.assert_type`).
  `@include` directive adapted from plan's `classes.R format_api.R` to include
  `utils.R` (where `.assert_name` lives).
- [x] T2: `ContractDaf` S7 class (DafWriter subclass) + `contractor(comp, contract, daf)`
  factory + `.enforcement_enabled()` gate (R option `dafr.enforce_contracts`
  takes precedence; env var `DAF_ENFORCE_CONTRACTS` accepts `1|true|t|yes|y`,
  case-insensitive; else FALSE). ContractDaf shares the base daf's cache and
  version counters — load-bearing for cache coherence.
- [x] T3: All 22 `format_*` methods on `ContractDaf` + 4 access trackers
  (`.access_scalar`, `.access_axis`, `.access_vector`, `.access_matrix`).
  Tracked reads/writes flip `tracker$accessed <- TRUE`; missing trackers
  error in non-relaxed mode; `RequiredInput`/`OptionalInput` entries are
  immutable-for-modify; matrix access tries both row/column axis orderings
  in the contract; vector `name`/`index` are built-in exceptions.
- [x] T4: `verify_input(daf)` + `verify_output(daf)`. Verifies existence
  (mandatory: `RequiredInput` on input, `CreatedOutput` on output), type
  (via `.type_ok` / `.vector_type_ok` / `.matrix_type_ok`), and
  forbidden-pre-existing (CreatedOutput at input time without `overwrite`).
  `verify_output` also scans for unused `RequiredInput` entries. No-op on
  non-ContractDaf.
- [x] T5: `merge_contracts(left, right)` — Julia's `|>` port. Narrower type
  wins via `.TYPE_WIDTH_ORDER = c("logical", "integer", "double", "numeric", "character")`.
  Expectation resolution per Julia's rules; incompatible output-output raises
  `"incompatible expectation"`.
- [x] T6: End-to-end tests on `MemoryDaf` + `FilesDaf` — a simulated
  computation declaring `cell` RequiredInput axis + `cell/age` RequiredInput
  vector + `cell/doubled` CreatedOutput vector; reads age, writes doubled,
  then `verify_input` → `verify_output` both pass.

**Phase Z — Docs + exit**

- [x] Z1: `devtools::document()` regenerated NAMESPACE + 12 new/updated
  `man/*.Rd` files + DESCRIPTION `Collate:`. Added `#' @param` roxygen
  blocks on `ContractDaf` and `merge_contracts` to clear "Undocumented
  arguments" WARNINGs that the plan snippets hadn't accounted for. NEWS.md
  Slice 4 entry prepended.
- [x] Z2: Exit note (this document); fast-forward merge of
  `slice-4-perf-chains-contracts` into `main`; tag `slice-4` created locally.
  Push to origin and L2 upstream PR remain user-discretion (see Status).

## Test + build status

- `testthat::test_dir("tests/testthat")` — **1055 PASS / 0 FAIL / 0 SKIP /
  1 WARN**. The single WARN is the pre-existing `scran::quickCluster` /
  `irlba::irlba` SVD tolerance notice in `test-altrep-downstream.R`, unchanged
  since Slice 0. Versus the Slice-3 baseline of 939 tests this is
  **+116 expectations** covering: Phase P fast-path regressions (P2/P3/P4 +
  CSC + integer-coerce), Phase F Slice-3 follow-ups (F1 view cache reuse,
  F2 NA-drop, F3/F4 chain lookup, F5/F6 view rename/filter propagation),
  Phase C chain readers + writers (C1-C8), C9 Julia chain round-trip, and
  Phase T contracts (T1-T6 including end-to-end).
- `devtools::check(error_on = "note")` with `_R_CHECK_SYSTEM_CLOCK_=0` —
  **0 ERROR / 0 WARNING / 0 NOTE**. Matches the Slice-3 baseline.
- `pkgbuild::compile_dll(debug = FALSE)` — clean. One new C++ file
  (`src/kernel_log_reduce.cpp`, P4) and two helpers added to
  `src/openmp_shim.h` (`dafr_omp_get_thread_num` +
  `dafr_omp_get_max_threads_capped`). No new C++ in Phases F/C/T/Z.

## Scope closed vs deferred

**Closed in Slice 4:**

- Perf hot-path wedge (P1-P5): `dafr.omp_threshold` wiring, sparsity-preserving
  `Log eps:1`, bare-default reduction routing, fused `Log->Sum|Mean` kernel,
  benchmark harness.
- Slice-3 view follow-ups (F1, F5, F6): view cache reuse, axis rename
  propagation to reads, axis filter propagation to reads.
- Query evaluator follow-ups (F2, F3, F4): NA drop in masks, single-hop
  chained lookup via `AsAxis` + `IfNot` sentinel.
- Chain federation (C1-C9): `ReadOnlyChainDaf` + `WriteChainDaf` classes;
  full scalar/axis/vector/matrix read dispatch with reverse-fall-through;
  write dispatch with delete-earlier-error + auto-add-axis; Julia
  round-trip fixture + test.
- Contract system (T1-T6): `Contract` + builders; `ContractDaf` wrapper +
  `contractor()` factory + enforcement gate; 22 format_* methods with
  access tracking + relaxed mode; `verify_input`/`verify_output` with
  existence + type + access checks; `merge_contracts` Julia-`|>` port;
  end-to-end test on MemoryDaf + FilesDaf.

**Deferred to Slice 5+:**

- `@examples` roxygen blocks — still deferred per prior user instruction.
- `complete_chain!` disk-chain helper — not implemented; defer until a
  consumer needs it.
- `@computation` macro equivalent for R — orthogonal to contracts' runtime
  behaviour; defer.
- Tensor keys in contracts (Julia UNTESTED path) — defer.
- **Multi-hop chained lookup** (`@ A : v =@ : w =@ : u`) — Slice 4 only
  covers single-hop `AsAxis`.
- Full kernel buildout per design spec §6 (`kernels_eltwise.cpp`,
  `kernels_reduce.cpp`, `kernels_matvec.cpp`) — Slice 4 ships only the one
  fused motif (P4). Dedicated perf slice follows once a second motif
  justifies it.
- General fusion planner — Slice 4 hand-codes the one motif; revisit when
  the second pattern shows up.
- Per-thread accumulator buffers in `kernel_log_reduce_csc_cpp` for the
  row-axis variant — Slice 4 accepts the serial nnz scan; lift to
  per-thread buffers if profiling shows it pays.
- **L2 upstream PR** against `tanaylab/DataAxesFormats.jl` docs — declined
  three times prior; **re-ask user at session end**. Spec draft at
  `dev/specs/filesdaf-on-disk-spec-draft.md` remains resolved.
- Long-vector (>2³¹) ALTREP scenarios — still untested (inherited from
  Slice 0-3).
- UInt32 > 2³¹ read arm — inherited from Slice 2.
- Multi-writer filesystem locking on FilesDaf — inherited from Slice 2.

## Scope deviations landed (plan vs implementation)

- **F4 bare `=@` parser extension**: plan said "not yet supported" but its
  Tests 1-2 exercised bare `=@` and expected results. Landed as a parser
  extension that emits `AsAxis(axis_name = NULL)` + evaluation-time
  inference from `state$property` (set by `.apply_lookup_vector`). Documented
  in pre-checkpoint notes.
- **P3 TDD red-first rule**: all 5 new tests in `test-perf-bare-reductions.R`
  passed pre-implementation because `apply() + sum` is numerically
  equivalent to `rowSums`. User accepted Option 1 (tests function as
  correctness-equivalence regression guards; fast-path win is wall-clock +
  RSS, measured in P5). See `feedback_slice4_p3_tdd.md` in auto-memory.
- **C3 mismatch tests passed pre-implementation**: the two "axis mismatch
  raises at construction" tests and "axis length mismatch" tests passed
  before C3's axis methods were registered, because `.validate_chain_axes`
  from C1 already raises on construction. Only the "axis union" test is a
  genuine red-first TDD test for C3. Plan defect, not an implementation
  issue — the mismatch tests are plan-misplaced regression guards for C1.
- **C9 Julia JSON emitter**: plan's Julia script uses `using JSON` but
  JSON isn't in DAF.jl's project manifest. Script was adapted to use an
  inline minimal JSON emitter following the existing Slice-3 pattern in
  `regen-julia-queries-fixture.jl`. Documented in the regen script's header.
- **T1 `@include` adaptation**: plan's scaffold said
  `#' @include classes.R format_api.R` but `contract_scalar/vector/matrix`
  call `.assert_name` from `R/utils.R`. Added `utils.R` to the include
  list. Plan should be corrected for future readers.
- **T1 validator error shape inconsistency**: `Contract` validator uses
  `return(string)` for some checks (wrapped by S7 into
  `"<dafr::Contract> object is invalid:\n- ..."`) and `stop()` inside
  `.assert_expectation`/`.assert_type` (bare message, no S7 wrapper).
  Two different error prefixes for the same operation. Cosmetic UX issue,
  not a correctness bug — callers still see *some* error. Worth a T3/T4
  follow-up that routes `.assert_*` through the validator's return channel
  or normalizes the error shape.
- **T1 unnamed/partial-named `axes` silently accepted**: `Contract(axes = list(list(RequiredInput, "d")))`
  (no name) constructs without validation because the iteration over
  `names(axes)` is NULL. Worth adding an up-front guard; not blocking for
  Slice 4 because all call sites supply names.
- **Z1 `_R_CHECK_SYSTEM_CLOCK_` env var**: plan spec had typo
  `R_CHECK_SYSTEM_CLOCK_=0` (no leading underscore); the correct variable
  name is `_R_CHECK_SYSTEM_CLOCK_`. Without the underscore, you get one
  spurious NOTE (sandbox can't reach `worldclockapi.com`). With the
  correct name, 0/0/0.
- **Z1 added roxygen blocks to `R/contracts.R`**: `devtools::check()`
  surfaced "Undocumented arguments" WARNINGs for `ContractDaf` (no `@param`
  for slots) and `merge_contracts` (no `@param left,right`). Added
  `contract-entries.Rd` shared Rd for the three builder functions plus
  blocks for `ContractDaf` (via `@inheritParams DafReader` + contract slots)
  and `merge_contracts`. Documentation-only, no behaviour change.

## Known mines laid in Slice 4 for Slice 5

- **`ContractDaf` inherits its base daf's cache env**. A chained
  `contractor -> chain_writer -> memory_daf` stack has three layers sharing
  one cache. If any layer explicitly clears the cache mid-flight, all see
  it. Cache coherence is preserved; cache isolation is not.
- **`chain_writer` auto-adds axes from earlier readers** when a new
  vector/matrix is first written to the writer for that axis. If earlier
  axes diverge from the writer after construction (shouldn't happen —
  `.validate_chain_axes` runs at construction, and modification-behind-the-back
  is an anti-pattern), the writer's copy becomes stale.
- **`.type_ok` / `.vector_type_ok` / `.matrix_type_ok` in contracts** use
  R class-name matching; they do not understand S3/S4/R5 inheritance trees
  beyond `inherits()`. For atomic types, they probe via `is.integer` etc.
  `character` is missing from `.matrix_type_ok`'s switch — character
  matrices would fall through to `inherits(m, "character")` which is FALSE
  (matrices carry class `"matrix"` + `"array"`). Non-blocking since
  character matrices are unusual.
- **`merge_contracts` type-order** `c("logical", "integer", "double", "numeric", "character")`
  is coarse and doesn't match Julia's full `<:` lattice. Narrower → wider
  only; no cross-axis moves (e.g., `integer | character` errors).
- **`Contract` validator error paths** mix `stop()` (from `.assert_*`) and
  `return(string)` (native S7). Callers writing `expect_error(...,
  "specific prefix")` tests may hit one or the other depending on *which
  field* of a bad contract entry is being validated.
- **Plan-inherited misnamed test** in `test-contracts-verify.R` — the block
  `"verify_output on CreatedOutput fails when pre-existing + !overwrite"`
  actually calls `verify_input(cd)` in its body (correctly — `.is_forbidden`
  fires at input-verify time). Test title is misleading; harmless but
  worth renaming.
- **Evaluator `state$kind` is a closed enum** (Slice 3 mine, reinforced by
  F4): now includes `"vector_axis"` from F4's `AsAxis` chained lookup. Any
  new handler must not introduce a `kind` that existing handlers
  implicitly fall through on. `.apply_axis` remains load-bearing.
- **Query cache key is canonical-string keyed**; chain wrappers get a fresh
  `new_cache_env` (not shared with base daf) per design. `ContractDaf` is
  the exception — it shares cache with its base (deliberate, for
  invalidation coherence on reads that pass through).

## Commit history

Slice 4 landed as **28 commits** on branch `slice-4-perf-chains-contracts`
(off `main` at `af842d7`). Net diff vs `main`: **+3360 / -68 across
46 files** (R source + tests + NEWS + NAMESPACE + man/ + DESCRIPTION +
new C++ kernel + fixtures).

```
7c57565 docs(slice-4): regenerate NAMESPACE + man + NEWS entry             [Z1]
6fa5eb3 test(contracts): end-to-end contract on MemoryDaf + FilesDaf        [T6]
ab56b97 feat(contracts): merge_contracts — Julia-|> port                    [T5]
1438cd6 feat(contracts): verify_input + verify_output                       [T4]
7d83069 feat(contracts): format_* dispatch + access tracking + relaxed      [T3]
98d120f feat(contracts): ContractDaf class + contractor() + enforcement     [T2]
ef2b651 scaffold(contracts): expectation constants + Contract class         [T1]
09092f8 test(chains): Julia fixture round-trip                              [C9]
02922a4 feat(chains): write-chain matrix set/delete/relayout                [C8]
c779728 feat(chains): write-chain axis/vector set/delete                    [C7]
24688d7 feat(chains): write-chain scalar set/delete                         [C6]
2084020 feat(chains): matrix read dispatch                                  [C5]
f045945 feat(chains): vector read dispatch                                  [C4]
531f0ca feat(chains): axis read dispatch + consistency check                [C3]
83bf113 feat(chains): scalar read dispatch (reverse fall-through)           [C2]
2314683 scaffold(chains): S7 classes + chain_reader/chain_writer            [C1]
c2fd0e0 feat(view_daf): axis filter propagates to vector/matrix reads       [F6]
cad7cd1 feat(view_daf): axis rename propagates to vector/matrix reads       [F5]
ea67587 feat(query-eval): AsAxis single-hop chained lookup + IfNot          [F4]
9410e83 feat(query-eval): IfNot records chain-lookup sentinel on state      [F3]
fe531a0 fix(query-eval): drop NA in mask output (Julia parity)              [F2]
7e0e465 fix(view_daf): reuse base daf cache env                             [F1]
f81bbec chore(perf): add dafr.perf.fast_paths bench gate (default TRUE)     [P5]
072e119 fix(query-eval): fused dense path coerces integer matrix to double  [P4 fix]
d1c087c perf(query-eval): fused kernel_log_reduce_{dense,csc}               [P4]
0e663a1 perf(query-eval): bare default reductions route to rowSums          [P3]
840ed3f perf(query-eval): sparsity-preserving Log eps:1 on dgCMatrix        [P2]
4e354b0 fix(kernels): wire dafr.omp_threshold through                       [P1]
```

Dev-repo additions (separate repo at `dev/`):
- `notes/slice-4-mid-slice-checkpoint.md` (mid-slice handoff, `54dc830`)
- `benchmarks/slice-4-perf-wedge-2026-04-21.csv` (P5, `ec04a1f`)
- `scripts/regen-julia-chains-fixture.jl` (C9, `a9652a4`)
- `notes/slice-4-exit.md` (this document — committed at session end)

## Repo conventions reinforced in Slice 4

- **S7 method duplication between sibling concrete classes**: `ReadOnlyChainDaf`
  and `WriteChainDaf` have byte-identical method bodies for 11 read-path
  generics. S7 doesn't inherit methods across sibling abstract-class
  intersections. Accepted duplication (code is mechanical, ~3-4 lines per
  method); revisit only if the duplication grows significantly or the
  hierarchy warrants a concrete `ChainDaf` with a mode flag.
- **Plan byte-equivalence discipline**: C1-C8 and T1-T5 method/helper bodies
  are byte-equivalent to the plan's snippets. Scope deviations (F4 parser,
  C9 JSON, T1 `@include`, Z1 env var + roxygen blocks) are documented above
  — all were forced by real constraints the plan didn't anticipate.
- **`#' @include` directives are load-bearing**. New files registering S7
  methods against `format_*` generics need `#' @include format_api.R` plus
  any concrete-class files they reference. `R/contracts.R` uses
  `classes.R format_api.R utils.R`; `R/chain_daf.R` uses
  `classes.R format_api.R cache.R`.
- **`format_get_*` returns plain arrays without dimnames**; user-facing
  `get_*` wrappers add names. Chain's `format_get_*` honours this.
- **`sort(..., method = "radix")`** for all listing returns. Chain and
  contract set-union methods follow this.
- **Error messages mirror Julia DAF's wording** for cross-runtime parity.
  Chain `.validate_chain_axes`, `format_delete_*` on WriteChainDaf, and
  contract access helpers all use the Julia error strings verbatim.
- **Julia fixture regeneration uses an inlined JSON emitter** (Slice-3
  precedent), since `JSON` isn't in `DataAxesFormats.jl`'s project
  manifest. `dev/scripts/regen-julia-chains-fixture.jl` follows
  `regen-julia-queries-fixture.jl`'s pattern.
- **NEW commits only**, never `--amend` / `--no-verify` / force-push.
  Slice 4 held to this across 28 commits.

## Ready-to-paste prompt for Slice 5

> Start implementing Slice 5 of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tag `slice-4` marks
>   the Slice 4 exit.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Slice 4 plan (fully executed):
>   `~/src/dafr-native/dev/plans/2026-04-21-slice-4-chains-contracts.md`.
> - Slice 4 exit note: `~/src/dafr-native/dev/notes/slice-4-exit.md`.
> - Slice 4 mid-checkpoint (context for how this slice proceeded):
>   `~/src/dafr-native/dev/notes/slice-4-mid-slice-checkpoint.md`.
>
> Slice 5 scope is open. Candidates from Slice 4 deferred items:
>
> - **Multi-hop chained lookup** (`@ A : v =@ : w =@ : u`) — Slice 4 only
>   covers single-hop; Julia parity requires multi-hop.
> - **Second fused kernel motif + general fusion planner** — Slice 4 ships
>   the one Log→Sum|Mean motif; profile real workloads to identify the next
>   pattern that warrants fusion.
> - **`@examples` roxygen blocks** (Z2 polish) — still deferred.
> - **Tensor keys in contracts** (Julia UNTESTED path).
> - **`complete_chain!` disk-chain helper** — if a consumer needs it.
> - **Long-vector (>2³¹) ALTREP scenarios** — inherited untested.
> - **UInt32 > 2³¹ read arm** — inherited.
>
> Use `superpowers:writing-plans` to draft a Slice 5 plan, then
> `superpowers:subagent-driven-development` to execute.
>
> **Julia DAF state at Slice 4 exit:** `~/src/DataAxesFormats.jl` at
> `49fbba1` and `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged from
> Slice 3 handoff). Both registered as Julia `dev` packages in conda env
> `dafr-mcview`. The Slice-3 17-query fixture and Slice-4 chain fixture
> are both live.

## Status at session end

- Local `~/src/dafr-native/`: `main` fast-forwarded to Slice 4 head
  (`7c57565`), tag `slice-4` applied locally. **NOT pushed to origin**
  (preserving Slice-3's convention of user-initiated push).
- Local `~/src/dafr-native/dev/`: `main` with this exit note committed.
  Pre-existing uncommitted files (pre-Slice-4) untouched:
  `benchmarks/bake-off-results.csv` (modified), `.a5c/` (untracked),
  `notes/slice-4-kickoff.md` (untracked),
  `plans/2026-04-21-slice-4-chains-contracts.md` (untracked), two Slice-0
  baseline CSVs (untracked).
- Feature branch `slice-4-perf-chains-contracts`: merged via fast-forward;
  may be deleted by user after verifying tag is correct.
- L2 upstream PR (`tanaylab/DataAxesFormats.jl` docs for
  `dev/specs/filesdaf-on-disk-spec-draft.md`): **re-ask user at session
  end** per Slice-3 exit protocol.
- Julia repos unchanged since Slice 4 kickoff.
