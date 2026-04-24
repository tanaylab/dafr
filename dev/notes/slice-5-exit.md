# Slice 5 exit gate — 2026-04-21

## Deliverables

**Phase D — Multi-hop chained lookup**

- [x] D1: `feat(evaluator): multi-hop chained lookup via '=@' stacking` — `.apply_chained_lookup_vector`
  returns `$property` + uses `names(pivot_values)` when post-hop; enables `@ cell : donor =@ : lab =@ : country`.
- [x] D tests (5 commits): explicit-axis 2-hop; 3-hop; `??` state cleared between hops; wrong-type
  pivot raises; `??` at hop 1 filters and chains through hop 2.
- [x] D docs: `docs(evaluator): clarify base_entries heuristic; assert names() on 3-hop` — docstring
  polish + defensive `stopifnot(names(...))` guard.

**Phase E — ExampleData port**

- [x] E1: `chore(example-data): import Julia test/example_data verbatim` — 18 raw files under
  `inst/extdata/example_data/` committed byte-identical to Julia.
- [x] E2: `feat(example-data): port Julia ExampleData module` — `example_cells_daf()`,
  `example_metacells_daf()`, `example_chain_daf()`; `.cast_vector` / `.cast_matrix` / per-file loaders
  replicate Julia's Bool→Int→Double→Character and UInt8→UInt16→Float32 promotion lattices.
- [x] E follow-up (2 commits): restore plan-specified assertions; declare `rprojroot` +
  harden `.cast_vector` + test cleanup. Julia-parity test against Slice 3 FilesDaf dump passes.

**Phase A — Computations HOF**

- [x] A1: `feat(computations): computation() HOF — base path + typecheck guards` — wraps `fn`
  with a `Contract`; on call `contractor()` wraps the daf, `verify_input()` / `verify_output()` fire.
- [x] A1 refactor: `refactor(computations): use .assert_name + S7::prop; docstring polish` — consolidates
  validation and cleans roxygen comments.
- `function_contract(fn)` / `contract_description(contract)` shipped in same commit. Single-contract
  only; dual/triple-contract variants (Julia UNTESTED) deferred.

**Phase B — Adapter + copy_view_to_daf**

- [x] B1: `feat(adapters): internal .copy_view_to_daf — scalars/axes/vectors/matrices` — new/replace/pad
  axis modes; `empty` flat-key form (`"axis|vector"` / `"rows|cols|matrix"`) for pad fills.
- [x] B2: `fix(adapters): trim @include to only files with their own @include chain` + 2 refactor/
  cleanup commits. Also fixes latent Slice-3 `view_daf.R` bug: `startsWith(NULL, prefix)` in
  `format_vectors_set` / `format_matrices_set`.
- [x] `adapter(daf, fn, input_axes, input_data, output_axes, output_data, capture, empty, relayout,
  overwrite, name)` exported; mirrors Julia's `adapter()`.

**Phase J — Julia-parity fixture**

- [x] J1: `test(fixtures): Julia adapter+computation roundtrip fixture` — `fixture.json` +
  `README.md` under `tests/testthat/fixtures/julia-adapter/`; regen script
  `dev/scripts/regen-julia-adapter-fixture.jl`.
- [x] J2: `test(adapter): Julia-parity roundtrip` — bit-identical round-trip verified against
  `DataAxesFormats.jl` at `49fbba140437387a378217c2fa658d4231d0c8c1`.

**Phase Z — Docs + check + examples backfill**

- [x] Z1 (cluster A): `@examples` for Slice 5's 7 new exports (`computation`, `function_contract`,
  `contract_description`, `adapter`, `example_cells_daf`, `example_metacells_daf`, `example_chain_daf`).
- [x] Z2 (clusters B–E): `@examples` backfilled to ALL 60 exported functions — user expanded scope
  mid-slice from the originally planned 7. CRUD ops use inline `memory_daf`; query/view/chain/contract
  examples use `example_cells_daf()`; `mmap_*` stay in `\donttest{}`.
- [x] Z3 (polish): 7 quality-review items applied — non-trivial contract example, builtin hints,
  delete-default hints.

## Test + build status

- `testthat::test_dir("tests/testthat")` — **1175 PASS / 0 FAIL / 0 SKIP / 1 WARN**. The single
  WARN is the pre-existing `scran::quickCluster` / `irlba::irlba` SVD tolerance notice in
  `test-altrep-downstream.R`, unchanged since Slice 0. Versus Slice 4's 1055 tests this is
  **+120 expectations** covering: Phase D multi-hop evaluator (6 tests), Phase E example-data
  loaders and Julia-parity (tests restored from plan), Phase A computation HOF, Phase B adapter +
  copy_view_to_daf, Phase J Julia adapter round-trip, and Phase Z smoke-tests embedded in `@examples`.
- `devtools::check(error_on = "note")` with `_R_CHECK_SYSTEM_CLOCK_=0` — **0 ERROR / 0 WARNING /
  0 NOTE**. Matches the Slice 4 baseline.
- `pkgbuild::compile_dll(debug = FALSE)` — clean. No new C++ files in Slice 5; all changes are
  pure R.

## Scope changes during the slice

- **Phase Z expanded mid-slice**: user broadened the `@examples` backfill from "Slice 5's 7 new
  exports only" (the plan's Z2) to ALL 60 exported functions. This added ~5 extra commits of
  examples work (clusters B–E) and the polish pass.
- **Commit granularity**: the plan's target commit counts were not met on several phases.
  Actual vs planned:
  - Phase A: 3 planned → 2 landed (feat + refactor bundled). Accepted per Slice-4 P3 precedent.
  - Phase B: 5 planned → 3 landed (feat + fix/cleanup bundled). Accepted per Slice-4 P3 precedent.
  - Phase E: 3 planned → 3 landed (chore + feat + 2-commit follow-up = 4 total; closely matched).
  In all cases the bundling was flagged in review and accepted; each commit is still a coherent
  logical unit.

## Plan errata caught and fixed in-flight

- **`/ cell` vs `@ cell` in query DSL**: plan used `/` as a query axis starter in several
  multi-hop examples; the DSL uses `@` (`/` is the group-by prefix). All tests corrected.
- **Adapter tests' axis rename**: needed `"@ cell"` query form in `input_axes`, not bare axis name.
- **`output_data = list(list(c(...), "="))` does NOT restrict outputs**: it adds entries on top of
  the implicit "default all". Tests and Julia regen corrected to prefix with
  `list(ALL_VECTORS, NULL)` / `list(ALL_MATRICES, NULL)` to clear the default-all before
  adding explicit entries.
- **Axis file count under `inst/extdata/example_data/axes/`**: plan said 8; Julia actually has 6.
  File list corrected.
- **A3 enforcement tests lacked `axes = list(...)` in the Contract**: the enforcer requires axis
  declaration for any vector access. All 3 A3 tests fixed to include the axis declaration.
- **`DataAxesFormats.jl` `output_data` scalars conflict**: `copy_all!` overwrite conflict when
  scalars were included in `output_data`. Both R and Julia regen scripts now omit scalars from
  `output_data`.
- **`view_daf.R` `startsWith(NULL, prefix)` bug (Slice 3, latent)**: `format_vectors_set` and
  `format_matrices_set` crashed when the view carried no vectors/matrices. Fixed alongside Phase B's
  minimal test fixtures.

## Known mines laid in Slice 5 for Slice 6

- **`.copy_view_to_daf` pad-mode matrix copy calls `as.matrix(val)`** — dense-coerces sparse; fine
  at `example_cells` scale (856×683) but a footgun at 50k×30k.
- **`computation()` stores contract as a function attribute, not a class** — robust but not
  introspectable through S7; no `inherits(fn, "computation")` test is possible.
- **`function_contract()` errors if the attribute is missing** — no `tryCatch`-friendly sentinel;
  callers must wrap themselves.
- **`merge_contracts` type lattice remains coarse** (`logical → integer → double → numeric →
  character`), unchanged from Slice 4.
- **Character matrices still unsupported by `.matrix_type_ok`** — fall-through to `inherits`;
  Slice-4 mine, not addressed in Slice 5.
- **`@computation` macro equivalent NOT implemented** — R has no macros; `computation()` HOF is
  the terminal form. Julia's roxygen-style `$(CONTRACT)` splice is replaced by
  `contract_description(contract)`.
- **Dual-/triple-contract computations deferred** — Julia UNTESTED upstream; not implemented.
- **`empty` pad-mode only supports flat-key form** (`"axis|vector"` / `"rows|cols|matrix"`); nested
  list form not supported.
- **`@examples` for `get_reduction("Sum")` / `get_eltwise("Abs")` depend on `.register_default_ops()`
  running at package load** — if that registration is ever made lazy, those examples will break.

## Deferred to Slice 6 / later (re-confirmed)

- **Option B (Copies + Concat + Reconstruction + Complete + Groups-tail + ExampleData-deeper +
  bulk helpers)** — still the primary recommendation for Slice 6 per kickoff.
- **Option C (20 more operations: Clamp/Convert/Fraction/Significant/Type/GeoMean/Median/Quantile/
  Std/StdN/Var/VarN/All/Any/…)** — mechanical boilerplate.
- **Tensor keys in contracts** — Julia UNTESTED path.
- **`complete_chain!` disk-chain helper** — no consumer yet.
- **Per-thread accumulator buffers in `kernel_log_reduce_csc_cpp` row-axis variant** — serial nnz
  scan accepted; lift when profiling shows it pays.
- **Long-vector (>2³¹) ALTREP scenarios** — still untested since Slice 0.
- **UInt32 > 2³¹ read arm** — inherited from Slice 2.
- **Multi-writer filesystem locking on FilesDaf** — inherited from Slice 2.
- **L2 upstream PR** for `dev/specs/filesdaf-on-disk-spec-draft.md` — re-ask at next-slice user
  checkpoint (declined three times prior; not re-raised here per protocol).

## Julia DAF state at handoff

- `~/src/DataAxesFormats.jl` at `49fbba140437387a378217c2fa658d4231d0c8c1` — **unchanged since
  Slice 3**. The Julia adapter fixture was regenerated against this commit and is bit-identical.
- `~/src/TanayLabUtilities.jl` at `48a4a57` — unchanged since Slice 3.
- Both registered as Julia `dev` packages in conda env `dafr-mcview` (Julia 1.12.5).

## Next-slice recommendation

Slice 6 should follow Option B from the kickoff plan: Copies, Concat, Reconstruction, Complete
(disk-chain finisher), the Groups-tail (GroupBy/CountBy aggregators that feed bulk workflows),
deeper ExampleData integration (using the live example datasets as inputs to computations and
adapters), and bulk-pipeline helper scaffolding. This aligns with the most likely real-world usage
pattern — a user writes a computation, wraps it in `adapter()`, and immediately wants to
concatenate or reconstruct a result. Option C (the ~20 additional eltwise/reduce operations) is
the safe alternative if a smaller scope is needed: each op is mechanical boilerplate and carries
no design risk, but alone it doesn't advance the "end-to-end computation pipeline" story. The L2
upstream PR against `tanaylab/DataAxesFormats.jl` docs remains open; re-raise with the user at
the Slice 6 kickoff checkpoint rather than here.

## Commit history

Slice 5 landed as **25 commits** on branch `slice-5-computations-adapters`
(off `main` at `7c57565`). Includes the Phase Z NEWS commit just landed.

```
5aa3444 docs(news): Slice 5 entry                                              [Z / news]
0bdb3e7 docs(examples): polish — non-trivial contract, builtin hints           [Z3]
57a8b9d docs(examples): cluster E — contracts, operations registry, mmap       [Z2-E]
5a28969 docs(examples): cluster D — queries, views, chains, files              [Z2-D]
116d3ec docs(examples): cluster C — writers.R surface                          [Z2-C]
3a0aa53 docs(examples): cluster B — readers.R surface                          [Z2-B]
2f43752 docs(examples): cluster A — Slice 5 exports                            [Z1]
9ad6dbf test(adapter): Julia-parity roundtrip                                  [J2]
66f269c test(fixtures): Julia adapter+computation roundtrip fixture             [J1]
9260530 refactor(adapters): drop dead NULL fallback; exercise DafWriter guard  [B refactor]
017bf18 fix(adapters): trim @include to only files with their own @include     [B fix]
37dc42f feat(adapters): internal .copy_view_to_daf                             [B1]
3cce8c8 refactor(computations): use .assert_name + S7::prop; docstring polish  [A refactor]
cbf4120 feat(computations): computation() HOF — base path + typecheck guards   [A1]
488fa3d fix(example-data): declare rprojroot + harden .cast_vector             [E follow-up]
82250a8 test(example-data): restore plan-specified assertions                  [E follow-up]
ea5c324 feat(example-data): port Julia ExampleData module                      [E2]
1d47ac9 chore(example-data): import Julia test/example_data verbatim          [E1]
d110cfa docs(evaluator): clarify base_entries heuristic; assert names() on 3-hop [D docs]
63fd957 test(evaluator): '??' at hop 1 filters then chains through hop 2       [D test]
3f29908 test(evaluator): wrong-type pivot on chained hop raises                [D test]
952f2f0 test(evaluator): '??' state cleared between chained hops               [D test]
46ce017 test(evaluator): 3-hop chained lookup                                  [D test]
7acf41f test(evaluator): explicit-axis 2-hop chained lookup                    [D test]
b6840db feat(evaluator): multi-hop chained lookup via '=@' stacking            [D1]
```

Dev-repo additions (separate repo at `dev/`):
- `scripts/regen-julia-adapter-fixture.jl` (`6906c08`) — Julia adapter+computation fixture regen.
- `notes/slice-5-exit.md` (this document — committed at session end).

## Status at session end

- Package repo `~/src/dafr-native/`: on branch `slice-5-computations-adapters` at HEAD `5aa3444`.
  One tracked-but-unstaged change remains: `DESCRIPTION` Collate reorder from `devtools::document()`
  — it is a cosmetic ordering change consistent with the branch's `@include` topology; commit or
  discard at the controller's discretion before fast-forward merge.
- Dev repo `~/src/dafr-native/dev/`: `main` with this exit note committed. Pre-existing
  uncommitted files untouched: `benchmarks/bake-off-results.csv` (modified), `.a5c/` (untracked),
  `notes/slice-4-kickoff.md` / `notes/slice-5-kickoff.md` (untracked),
  `plans/2026-04-21-slice-4-chains-contracts.md` / `plans/2026-04-21-slice-5-computations-adapters-multihop.md`
  (untracked), two Slice-0 baseline CSVs (untracked).
- **Recommend**: fast-forward merge `slice-5-computations-adapters` into `main`, tag `slice-5`,
  after a final code-review checkpoint with the user. Do NOT push to origin until user confirms.
- Julia repos unchanged since Slice 5 kickoff (same commits as Slice 4 handoff).
