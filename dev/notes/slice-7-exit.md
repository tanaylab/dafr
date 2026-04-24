# Slice 7 — Exit

**Date:** 2026-04-21.
**Tag:** `slice-7` — to be applied to the merge commit on `main` by the user.
**Predecessor:** tag `slice-6` at commit `e38c53a`.

## Delivered

12 new default query ops, all R-only, registered at package load:

**Eltwise (4):** `Clamp`, `Convert`, `Fraction`, `Significant`.
**Reductions (8):** `Var`, `Std`, `VarN`, `StdN`, `Median`, `Quantile`, `GeoMean`, `Mode`.

Highlights:
- Sparse-aware eltwise paths where applicable: Clamp (when `min ≤ 0 ≤ max`),
  Fraction (column-normalize preserves sparsity), Significant (operate on `@x`
  then `Matrix::drop0`). Dense-coerce via `as.matrix()` for the non-preserving
  cases (Clamp with non-straddling range; Convert to integer/logical on sparse).
- All reductions use **uncorrected (n-denom)** variance to match DAF.jl.
- `Mode` is numeric-only this slice; string-axis grouping deferred (would require
  refactoring `.apply_reduction_grouped_*` off the `vapply(..., numeric(1))`
  contract — cross-cutting, out of scope).
- End-to-end query tests at `tests/testthat/test-operations-query.R` cover
  every new op through `get_query()`.
- Julia-queries fixture extended by 11 records (Convert excluded: type-name
  vocabularies differ between R — `double`/`integer`/`logical` — and Julia —
  `Float64`/`Int32`/`UInt32`/`Bool`/...). 28 records total; all byte-parity
  at `tolerance = 1e-5`.

## Commits (11 on slice-7-ops-expansion)

083559d test: extend julia-queries fixture with Slice-7 ops (byte parity)
31ed36e feat(ops): register 12 Slice-7 ops + end-to-end query tests + NEWS
218739c feat(ops): add Mode reduction (numeric-only; string-support deferred)
d869a7a feat(ops): add GeoMean reduction (with eps regularisation)
a5dd2cf feat(ops): add Median and Quantile reductions
88cd1fa feat(ops): add VarN and StdN reductions (variance/stdev normalized by mean+eps)
e845226 feat(ops): add Var and Std reductions (uncorrected, n-denom)
b4ad636 feat(ops): add Significant eltwise (dual-threshold, sparse-aware)
f38db29 feat(ops): add Fraction eltwise (sparse-preserving column-normalize)
061e2b3 feat(ops): add Convert eltwise op (double/integer/logical target)
4b7f75f feat(ops): add Clamp eltwise op (sparse-preserving when 0 in range)

## Intentionally deferred

- **Fast paths for Var / Std / Median / Quantile** via `matrixStats::rowVars`,
  `rowSds`, `rowMedians`, `rowQuantiles`. The existing `.apply_reduction_slow`
  path (via `apply(m, margin, fn)`) is correct and our per-op functions are
  pure R. Profile first; add only if matrix-heavy queries are slow in practice.
- **Mode on character input.** Requires refactoring `.apply_reduction_grouped_*`
  off the `vapply(..., numeric(1))` contract. Not touched this slice.
- **`type` parameter on ops.** R's output-type policy is "always double except
  Convert, Count, Mode". Julia's per-op `type` is not ported.
- **Sparse→integer fast path for Convert.** Inherits the `.cast_matrix_type`
  dense-coercion mine (Slice 6). Not triggered by any test.
- **Convert in the Julia fixture.** Type-name vocabulary mismatch is an
  API-level cross-compat issue; no byte-parity test.

## Upstream Julia finding (not patched)

- `DataAxesFormats.jl` `src/operations.jl:988`: `significant!` computes
  `-high .< vector`. When `eltype(vector) <: Unsigned` and `high > 0`,
  `-high` **underflows** (e.g., `-UInt32(30) == 4294967266`), making the
  not-high mask uniformly false and short-circuiting the whole body. Julia's
  Significant therefore silently no-ops on UInt32 input regardless of
  threshold. Verified at Julia 1.12.5 against DataAxesFormats.jl `49fbba1`.
- **Not patched upstream per durable user feedback** (L2 upstream PR declined).
- **Workaround in our fixture:** we route the Significant fixture case
  through `% Log base 2.0 eps 1.0` first to coerce to Float32 before
  applying Significant. Float-signed `-high` works correctly, and R's
  implementation (which operates on doubles) agrees byte-for-byte.
- **Impact on dafr-native:** our R-side Significant implementation works
  correctly on any numeric input. The bug only surfaces when round-tripping
  against the buggy Julia code, which we now avoid.

## Mines laid for Slice 8

- `.apply_reduction_grouped_*` uses `vapply(..., numeric(1))`. Any future
  char-valued op (Mode-on-strings, new string reductions) must refactor this.
- `.cast_matrix_type("integer", dgCMatrix)` dense-coerces (pre-existing).
  Now reachable from the public `Convert` op for the first time. Still
  unexercised by tests.
- **`.matrix_type_ok` missing `character` case** — pre-existing Slice-4 mine;
  not fired by any Slice-7 op (no character output). Still open.
- Julia-side Significant bug on UInt32 inputs — see above. If Julia moves
  past `49fbba1` and this is fixed upstream, the fixture's Significant
  workaround can be simplified back to a direct `% Significant high N` on
  `donor : age`.

## Test status at exit

1448 PASS / 0 FAIL / 0 SKIP / 1 WARN (pre-existing scran/irlba SVD tolerance
notice in `test-altrep-downstream.R`, unchanged since Slice 0).

## Check status at exit

0 ERROR / 0 WARNING / 0 NOTE.

## Julia DAF state at exit

- `~/src/DataAxesFormats.jl` HEAD: `49fbba140437387a378217c2fa658d4231d0c8c1`
  (unchanged since Slice 3 — five slices of stability now).
- `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5 in the conda env).

## L2 upstream PR

Declined permanently per durable user feedback. No ask this slice either.
Two specific bugs identified but left upstream (Julia-side `significant!`
on unsigned integers; pre-existing dafr-native `.matrix_type_ok` character
gap).
