# Slice 9a — Design spec (correctness workstream)

**Date:** 2026-04-22
**Predecessor:** Slice 8 (`slice-8` tag, merge `f7978cc`)
**Kickoff:** `dev/notes/slice-9-kickoff.md`
**Scope:** Workstream A from the Slice 9 kickoff. Perf parity
(Workstream B) is deferred to a separate Slice 9b with its own
kickoff + design.

---

## 1. Goal

After Slice 9a, a query string valid in DAF.jl produces byte-equivalent
output in `dafr` for:

- Grouped vector reductions (G1) written with Julia's `>>` operator.
- Grouped matrix reductions (G2, G3) using Julia's operator convention.
- Convert ops using Julia type names (`Float32`, `Int64`, `Bool`, …).

And vice versa: a fixture record emitted by DAF.jl can be consumed
by `dafr` without a translation layer.

## 2. Out of scope

- Performance measurement, profiling, bake-off harness against DAF.jl.
- G3 kernel thread-bucket memory redesign.
- Any new backend (H5df / AnnData / Zarr).
- `bestify`, `reconstruct_axis` with existing target, multi-writer
  filesystem locking, `computation()` dual-/triple-contract forms.

All Slice 8 deferred items that are not explicitly listed below stay
deferred.

## 3. Locked decisions

| # | Decision | Choice |
|---|---|---|
| 1 | G2/G3 semantic dispatch | Swap to Julia convention (G2 = `GroupRowsBy + ReduceToRow`, G3 = `GroupColumnsBy + ReduceToColumn`) |
| 2 | G1 `>>` operator | Extend R parser to accept `>>` as an alias for the existing `>\|` token (`.qop_reduce_to_column`). Both tokens parse to the same AST node; R's existing `/ ... >\|` G1 syntax continues to work |
| 3 | Convert type vocabulary | Bidirectional aliases. Canonical form stays R-native (`double`/`integer`/`logical`). Julia names (`Float32`, `Float64`, `Int32`, `Int64`, `Bool`) accepted. `bit64` is already in `Imports` (no DESCRIPTION change); FilesDaf Int64/UInt64 round-trip already wired via `bit64::integer64` in `R/files_io.R` |
| 4 | Slice structure | Option II — Slice 9a first (correctness), then a separate Slice 9b (perf parity) |
| 5 | Axis-rename view re-apply | In-scope for 9a with 2–3h timebox; write findings and defer fix if the timebox runs out |
| 6 | Perf parity target + G3 kernel fix | Deferred to Slice 9b kickoff (needs profile data) |

## 4. Breaking changes

The G2/G3 semantic swap is a user-observable behavior change. Any
existing R-side code that pairs `GroupRowsBy + ReduceToColumn` or
`GroupColumnsBy + ReduceToRow` under the old convention will
produce different output (or dispatch errors) after this slice.

User-confirmed acceptable: the package has no external users yet.

`NEWS.md` entry for Slice 9a will announce this under a **Breaking
changes** heading with a before/after example.

## 5. Task breakdown

Ordering reflects dependencies. Total estimate ~19–21h across 3–4
working days.

### T1 — Semantic swap G2/G3 dispatch [~4h]

**Files:** `R/query_eval.R` (lines 1107–1108, 1126, 1165, 1189),
possibly `R/operations.R` cross-check.

**Steps:**

1. Flip `is_g2` condition to `by == "rows" && op == "ReduceToRow"`.
2. Flip `is_g3` condition to `by == "cols" && op == "ReduceToColumn"`.
3. Trace `.apply_reduction_grouped_matrix_fallback` axis argument
   (line 1126) — confirm the axis label still matches the new
   convention end-to-end (groups-along-rows → axis=2; groups-along-
   columns → axis=3; unchanged at the kernel boundary).
4. Re-derive G4a/G4b inner-op dispatch at lines 1165 and 1189 under
   the new convention. Work a paper example for each case before
   editing.
5. Cross-check kernel calls in `src/kernel_grouped_*.cpp` — no C++
   changes expected; swap is at the R dispatch layer only.

**Acceptance:** all Slice-8 kernel benchmarks still pass (sanity smoke
that the kernel layer is untouched). Test suite failures are
expected at this step — they are fixed in T2.

### T2 — Invert grouped-matrix test assertions [~3h]

**Files:** `tests/testthat/test-query-grouped-slice8.R` (84
assertions).

**Steps:**

1. For every assertion that currently pairs `GroupRowsBy` with
   `ReduceToColumn` or `GroupColumnsBy` with `ReduceToRow`, swap the
   reduce operator to match the new convention.
2. Verify the *expected* values still match the intended computation
   under the new dispatch — use the R-side oracle (`.op_*` applied
   to the raw matrix + group index by hand) for at least one G2 and
   one G3 case per op.
3. Run the full `test-query-grouped-slice8.R` suite. All 84
   assertions pass.

**Oracle plan:** for numeric confidence, defer final cross-check to
T6 where fixture records provide byte-parity against Julia. T2 only
needs R-side consistency.

### T3 — Audit pre-Slice-8 grouped tests [~1h]

**Files:** `tests/testthat/test-query-eval-groupby.R`.

**Steps:**

1. Read every assertion; classify as "unaffected by the swap"
   (uses G1 only, or uses shapes that don't collide) or "needs
   inversion".
2. Invert the flagged assertions to match the new dispatch.
3. Run the file, confirm 0 failures.

### T4 — Parser alias for `>>` [~2h]

**Files:** `R/query_parse.R` (line 62–63 dispatch table);
`tests/testthat/test-query-parse.R`.

**Context:** Tokenizer regex at `R/query_tokens.R:7` (`>[->\\|]`)
already matches `>>`, so no lexer change is needed. The gap is in
the parser dispatch — `R/query_parse.R:62-63` registers `>|` and
`>-` but not `>>`. R's existing G1 syntax is `/ group >| Sum`
(ReduceToColumn-based); Julia's is `/ group >> Sum`. `>>` aliases
to the same `.qop_reduce_to_column` AST node as `>|`.

**Steps:**

1. Add one dispatch line at `R/query_parse.R:63`:
   `">>" = .parse_reduction(tokens, i, src, .qop_reduce_to_column),`
2. Keep `>|` and `>-` dispatches unchanged (back-compat for
   existing R usage and matrix G2/G3).
3. Add TDD tests: `"@ cell : UMIs / donor >> Sum"` produces the
   same AST as `"@ cell : UMIs / donor >| Sum"`; end-to-end
   `get_query` result is equal.

**Acceptance:** parse-identity tests pass, existing parser tests
unchanged.

### T5 — Convert type-name aliases [~2h]

**Files:** `R/operations.R` (`.op_convert` at lines 166–197),
`tests/testthat/test-ops-convert.R` (or nearest existing `test-ops-*.R`).

**Context:** `bit64` is already in `DESCRIPTION` Imports; FilesDaf
already handles Int64/UInt64 via `bit64::integer64` in
`R/files_io.R` (lines 145, 167, 200). No FilesDaf changes needed
in 9a. This task is confined to `.op_convert`.

**Steps:**

1. Add a normalization step at the top of `.op_convert` that maps
   Julia type names to R-native ones before the existing validation:
   - `Float32`, `Float64` → `double`
   - `Int32` → `integer`
   - `Int64` → (new internal token) `integer64`
   - `Bool` → `logical`
   Keep the existing `double`/`integer`/`logical` tokens accepted.
2. Extend the type whitelist and dispatch to handle the new
   `integer64` branch: for `dgCMatrix`, integer64 preservation
   doesn't apply (no sparse integer64 class), so densify then
   convert via `bit64::as.integer64()`. For dense / vector input,
   set via `bit64::as.integer64()`. Document the densification in
   NEWS.
3. Update the error message listing valid types to include both
   R-native and Julia aliases.
4. TDD tests: each Julia alias accepted on dense + sparse input
   (where applicable); `Int64` round-trip produces a
   `bit64::integer64` class vector with expected values; unknown
   name (e.g. `Float16`) still errors.

**Acceptance:** all alias + round-trip tests pass; existing
`.op_convert` tests continue to pass unchanged.

### T6 — Extend Julia-queries fixture [~3h]

**Files:** `dev/scripts/regen-julia-queries-fixture.jl`,
`tests/testthat/fixtures/julia-queries/fixture.json`,
`tests/testthat/test-query-*-julia*.R`.

**Steps:**

1. Verify `~/src/DataAxesFormats.jl` is still at
   `49fbba140437387a378217c2fa658d4231d0c8c1`. If not, investigate
   before regenerating any fixture. Log the commit hash in the
   regen script comment header.
2. Add fixture records for:
   - G1 on every builtin op + Quantile(p=0.25), GeoMean, Mode,
     using the Julia `>>` syntax.
   - G2 on every builtin op + parametric, using Julia convention
     (`GroupRowsBy + ReduceToRow`).
   - G3 on every builtin op + parametric, using Julia convention
     (`GroupColumnsBy + ReduceToColumn`).
   - Mode-on-character (G1 scope, now that `>>` parses).
   - Convert-to-Int32 on sparse.
   - Convert-to-Int64 on sparse (in-memory path).
   - Convert-to-Bool on sparse.
3. Run the regen script; commit fixture JSON.
4. Run `test-query-*-julia*.R`; byte-parity on every new record.

**Acceptance:** every new record in fixture.json matches between
`dafr` and the Julia-emitted expected output exactly.

### T7 — Axis-rename view re-apply investigation [timebox 2–3h]

**Files:** `R/view.R`, wherever `complete_daf` re-applies views, plus
relevant tests.

**Steps:**

1. Re-read Slice 8 Task 13 notes and the current `complete_daf`
   view re-apply code path.
2. Investigate whether `viewer()` supports axis rename via a
   different arg shape (e.g., named-list `axes = list(renamed_cell
   = "cell")` or similar). Read `R/view.R` top-to-bottom for arg
   shape.
3. If yes: add a renamed-axis round-trip test (view with rename →
   `complete_daf` → reopen → query works with renamed axis).
4. If no within timebox: write `dev/notes/axis-rename-findings.md`
   summarising what we tried, what we found, and what a Slice 10+
   fix would need. Do not partially implement.

**Acceptance:** either a new passing round-trip test, or a findings
note committed in `dev/notes/`.

### T8 — NEWS, final check, exit note [~1h]

**Files:** `NEWS.md`, `dev/notes/slice-9a-exit.md`.

**Steps:**

1. NEWS entry under "Slice 9a" with a **Breaking changes** subhead
   documenting the G2/G3 swap, before/after example.
2. Sub-entries for parser `>>` alias, Convert type aliases,
   axis-rename result.
3. `devtools::test()` — 1744+ PASS, 0 FAIL. Pre-existing 1 SKIP +
   1 WARN baseline preserved.
4. `devtools::check(error_on = "note")` — same 2 structural notes as
   Slice 8 baseline. No new notes / warnings / errors.
5. Exit note `dev/notes/slice-9a-exit.md` following Slice-8 exit
   template.

## 6. Parallelisation plan

Dependencies for subagent execution:

```
T1 ─┬─ T2 ─┐
    │      │
    └─ T3 ─┤
           │
T4 ────────┼─→ T6 ─→ T8
           │
T5 ────────┘

T7 (independent; runs any time after T1 lands)
```

- T1 + T2 + T3 must run in a single session (test inversion is
  coupled to dispatch swap).
- T4 and T5 are independent; can parallelise across subagents.
- T6 depends on T1+T4+T5 all landing (fixture regen needs the new
  parser + new aliases + new dispatch).
- T7 is independent of T1–T6 and can run in parallel.
- T8 is the merge-ready gate.

## 7. TDD stance per task

| Task | TDD mode | Rationale |
|---|---|---|
| T1 | Regression-guard (bundled commit) | Refactor rewrite; inversion is mechanical mirror. Same pattern as Slice 4 P3 (see `feedback_slice4_p3_tdd.md`) |
| T2 | Regression-guard (with T1) | Test inversion is derived from dispatch swap |
| T3 | Regression-guard | Audit-and-fix on stable tests |
| T4 | Failing-first TDD | New parser behavior, clean TDD applies |
| T5 | Failing-first TDD | New alias behavior, clean TDD applies |
| T6 | Fixture expansion (byte-parity gate) | Records are the test |
| T7 | Exploratory | Timebox-bound; test added only if fix is found |
| T8 | Verification | Final gate only |

Note in Slice 9a exit that T1/T2/T3 are regression-guard, not
failing-first TDD.

## 8. Risks & mines

1. **Cascade through G4a/G4b inner-op** (`R/query_eval.R:1165, 1189`).
   Miss this and simple G2/G3 pass while two-stage reductions
   silently wrong-compute. Mitigation: explicit paper trace before
   editing.
2. **Kernel axis interpretation.** R dispatch labels `axis=2`/`axis=3`
   but C++ kernels care only about shape. Swap changes labels, not
   shapes — should be safe, but T1 acceptance includes running Slice-8
   kernel benchmarks as a smoke check.
3. **Formula authority.** `R/operations.R` `.op_*` is source of
   truth (kickoff). Don't rely on mental model from kickoff prose
   when re-deriving G4a/G4b inner ops — read the `.op_*` source.
4. **`bit64::integer64` preservation in sparse context.** No sparse
   int64 class exists in R, so `Convert(sparse, Int64)` must
   densify. Document in NEWS and make the densification explicit in
   the code (not accidental). `bit64` is already wired through
   FilesDaf per `R/files_io.R`; no new integration surface in 9a.
5. **Parser `>>` conflicts.** Confirm `>>` is not currently used by
   any existing parser path before adding the alias (T4 step 1).
6. **Axis-rename timebox discipline.** Commit the 2–3h timebox in
   plan text, not just in my head. Write findings and stop if no fix
   emerges.
7. **Fixture regen depends on DAF.jl commit.** Kickoff: unchanged
   since Slice 3 at `49fbba140437387a378217c2fa658d4231d0c8c1`. T6
   verifies this first.

## 9. Exit criteria

Gates for claiming Slice 9a done:

1. `devtools::test()` — 1744+ PASS, 0 FAIL. Pre-existing 1 SKIP +
   1 WARN baseline preserved; no new skips or warnings.
2. `devtools::check(error_on = "note")` — same 2 structural notes as
   Slice 8 baseline. No new notes, warnings, or errors.
3. Extended Julia-queries fixture covers G1 / G2 / G3 / Mode-char /
   convert-{Int32,Int64,Bool}; every new record byte-equal across
   `dafr` and Julia-emitted expected output.
4. Parser: both `>>` and `>-` parse G1 to the same AST (round-trip
   test passes).
5. Convert aliases: all five Julia names accepted; `Int64` round-trips
   through `bit64::integer64` in memory (FilesDaf scope either
   covered or explicitly documented as deferred).
6. Axis-rename: either a round-trip test lands, or
   `dev/notes/axis-rename-findings.md` documents findings and next
   steps within the 2–3h timebox.
7. `NEWS.md` Slice 9a entry with explicit **Breaking changes**
   section documenting G2/G3 swap + before/after.
8. `dev/notes/slice-9a-exit.md` following Slice-8 exit template.
9. Public exports: 110, unchanged.
10. Clean merge: single merge commit into `main`, tagged `slice-9a`.

## 10. What comes next (Slice 9b)

Deferred to Slice 9b kickoff + design:

- Decision #5: perf parity realistic target (blanket / hot-path /
  tiered).
- Decision #6: G3 kernel memory fix strategy (row-partition /
  adaptive thread cap / sequential fallback) — decide from profile
  data.
- Build DAF.jl bake-off harness in `benchmarks/` (distinct from the
  existing `dev/benchmarks/bake-off-eigen/` harness).
- Identify gaps where `julia_time / dafr_time < 0.7`, open sub-tasks.
- Fix G3 kernel memory explosion at high thread counts × high nrow
  × high ngroups.

Slice 9a ships the semantically-aligned baseline that 9b's bake-off
will measure against. Running 9b before 9a would waste measurement
effort on soon-to-change dispatch.
