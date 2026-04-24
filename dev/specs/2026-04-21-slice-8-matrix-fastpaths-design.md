# Slice 8 — Matrix-kernel fast paths + `complete_daf` view re-apply

**Date:** 2026-04-21.
**Predecessor:** Slice 7 (`dev/notes/slice-7-exit.md`, tag `slice-7`).
**Kickoff:** `dev/notes/slice-8-kickoff.md`.
**Scope:** locked (user approved sections 1–4 on 2026-04-21).

---

## 1. Context and motivation

Slice 7 added 12 default query ops (`Clamp`, `Convert`, `Fraction`,
`Significant`, `Var`, `Std`, `VarN`, `StdN`, `Median`, `Quantile`,
`GeoMean`, `Mode`) as pure-R implementations layered on the Slice-3/4
dispatch infrastructure. All 12 are functionally correct and byte-parity
with DAF.jl for the covered cases, but:

1. On sparse matrices the reductions go through `.apply_reduction_slow`,
   which uses `apply(m, margin, FUN)` — this densifies `dgCMatrix` inputs
   row-by-row (or column-by-column).
2. `.apply_reduction_fast` densifies `Min`/`Max` on sparse via
   `matrixStats::rowMaxs(as.matrix(m))` (pre-existing Slice-3 mine, now
   reachable from `Convert`-adjacent paths).
3. Grouped reductions (`.apply_reduction_grouped_*`) use
   `split()` + `apply()` + `vapply(..., numeric(1))`, which both densifies
   sparse matrices and hard-codes numeric output.
4. `.op_convert` on `dgCMatrix` densifies for every `type` other than
   `double`.
5. `complete_daf()` reads back `base_daf_repository` but ignores the
   stored `base_daf_view` JSON spec, dropping the view on reopen
   (carried from Slice 6).
6. `.matrix_type_ok` has no explicit `character` case and rejects
   integer-valued `dgCMatrix` for contract-declared `integer` matrices.

Slice 8 closes all six gaps with custom C++ CSC kernels (following the
existing `kernel_log_reduce_csc_cpp` precedent), a grouped-reduction
rewrite that drops the `numeric(1)` vapply contract, and one small JSON
re-apply wire-through. No new R package dependencies.

**Guiding constraint (user directive, 2026-04-21):** "fast AND efficient,
no dense materialization" — every sparse path must work directly on
`@i`/`@x`/`@p` slots without calling `as.matrix()`. All kernels are
column-parallel via OpenMP. Benchmarks are a pass/fail exit gate.

---

## 2. Scope (locked)

### 2.1 In

1. **Six new sparse CSC kernels** (Section 3.1):
   - `kernel_var_csc_cpp` (Var/Std/VarN/StdN)
   - `kernel_minmax_csc_cpp` (Min/Max)
   - `kernel_quantile_csc_cpp` (Median/Quantile)
   - `kernel_geomean_csc_cpp` (GeoMean)
   - `kernel_mode_csc_cpp` (Mode, numeric)
   - `kernel_grouped_reduce_csc_cpp` (shared engine for grouped G2/G3
     single-reduction patterns)
2. **One dense parallel kernel** `kernel_grouped_reduce_dense_cpp` for
   G2/G3 on dense inputs (Section 3.2).
3. **Two grouped-op specialisations**:
   - `kernel_grouped_quantile_csc_cpp` (Median/Quantile per group)
   - `kernel_grouped_mode_csc_cpp` (Mode per group, numeric path)
4. **Ungrouped dense fast paths** via existing `matrixStats` entry points
   for `Var`/`Std`/`Median`/`Quantile` (Section 3.2).
5. **Grouped reduction rewrite** in `R/query_eval.R` (Section 4):
   - Drop the `vapply(..., numeric(1))` contract in favour of type-sniffed
     output storage.
   - Dispatch by builtin-label to kernel-backed fast paths.
   - Decompose double-reduction patterns (G4) into composed
     fast-path calls.
6. **Mode on character input** for grouped vectors (Section 4.3).
7. **`.op_convert` sparse→integer / sparse→logical** preserve sparsity
   (Section 5.2).
8. **`.matrix_type_ok` character case + sparse-integer recognition**
   (Section 5.3).
9. **`complete_daf` base_daf_view JSON re-apply** (Section 5.1).
10. **Benchmark harness** `benchmarks/slice-8-reduction-kernels.R` with
    pass/fail performance gates (Section 6.2).

### 2.2 Out — still deferred

- AnnData interop, Zarr backend, H5df backend.
- `bestify` heuristic for `copy_vector`/`copy_matrix`.
- `reconstruct_axis` with a pre-existing target axis.
- Long-vector (>2³¹) ALTREP.
- UInt32 > 2³¹ read arm (Slice-2 inherited).
- Multi-writer filesystem locking on FilesDaf (Slice-2 inherited).
- Sparse→character preservation for `.op_convert` — current behaviour
  (densify to a character matrix, per Section 5.2) is kept; no sparse
  character class exists in R, so there is nothing to preserve.
- `.cast_matrix_type` refactor (separate concern; used by `copy_*`, not
  by `Convert`).
- `computation()` dual-/triple-contract forms.
- `@examples` for the 25 skipped exports.

---

## 3. Ungrouped reduction fast paths

### 3.1 Sparse (`dgCMatrix`)

Current `.apply_reduction_fast` (`R/query_eval.R:553–590`) handles only
`Sum`/`Mean` (via `Matrix::rowSums/colSums`) and densifies
`Min`/`Max`. Every other reduction falls to `.apply_reduction_slow` which
`apply()`s column-by-column, densifying.

New dispatch order (keyed on `.dafr_builtin` attribute + `node$reduction`
name):

| Reduction(s) | Sparse kernel | Notes |
|---|---|---|
| Sum, Mean | existing `Matrix::rowSums/colSums` | unchanged |
| Min, Max | `kernel_minmax_csc_cpp` | replaces `as.matrix()` densify |
| Var, Std, VarN, StdN | `kernel_var_csc_cpp` | emits all four variants from one pass |
| Median, Quantile | `kernel_quantile_csc_cpp` | implicit-zero-aware rank |
| GeoMean | `kernel_geomean_csc_cpp` | fused-log pattern |
| Mode (numeric) | `kernel_mode_csc_cpp` | implicit-0-mode handled |

Anything else falls through to `.apply_reduction_slow` (unchanged).

**`kernel_var_csc_cpp` signature (cpp11):**

```cpp
[[cpp11::register]]
cpp11::writable::doubles kernel_var_csc_cpp(
    cpp11::integers i,       // @i, length nnz
    cpp11::doubles  x,       // @x, length nnz
    cpp11::integers p,       // @p, length ncol+1
    int nrow,
    int ncol,
    int axis,                // 0 = per-row (collapse cols), 1 = per-col (collapse rows)
    std::string variant,     // "Var" | "Std" | "VarN" | "StdN"
    double eps               // used for *N variants
);
```

One pass computes `sum_x`, `sum_x2`, `n_explicit` per output slot;
implicit zeros contribute only to `N`. Numerical stability: use
Welford-style online update per non-zero to avoid catastrophic
cancellation on high-mean columns.

**`kernel_minmax_csc_cpp`:** per-column min/max over `@x`, folds in
zero when `nnz_in_col < nrow`. Two output variants (`Min` / `Max`).

**`kernel_quantile_csc_cpp`:** per-column, per-axis:
1. Collect the column's `@x` slice, compute `n_zeros = col_length - nnz_col`.
2. Compute the target rank `r = q * (n_total - 1)` (type-7 quantile to
   match DAF.jl/Julia default).
3. Partition `@x` slice by sign; the number of values ≤ 0 is
   `n_neg + n_zeros` (explicit zeros conventionally not stored, but
   defensively handled via `drop0`-first normalisation).
4. Use `std::nth_element` on the negative or positive half depending
   on where `r` falls; interpolate between two neighbours for non-integer
   rank.
5. Median = q=0.5 shortcut path.

**`kernel_geomean_csc_cpp`:** `exp((Σ log(x + eps) + n_zeros · log(eps)) / n_total) - eps`. Mirrors `kernel_log_reduce_csc_cpp` with the `exp`-back + subtraction of `eps`.

**`kernel_mode_csc_cpp`:** per-column numeric mode via
`std::unordered_map<double, int>` over `@x`; if `n_zeros > max_explicit_count`,
mode is 0.

### 3.2 Dense fast paths

For `Var`/`Std`/`Median`/`Quantile`, use `matrixStats::rowVars` /
`colVars` / `rowMedians` / `colMedians` / `rowQuantiles` / `colQuantiles`
directly. Re-derive `VarN`/`StdN` from `rowVars` + `rowMeans`.

For `GeoMean` on dense: R-level `exp(rowMeans(log(x + eps))) - eps` (or
call `kernel_log_reduce_dense_cpp` + post-`exp`).

For `Mode` on dense numeric: custom R loop using `tabulate` on
floor-bucketed values, or a thin `kernel_mode_dense_cpp`. Decision
deferred to implementation: benchmark a 5k×5k profile and pick whichever
meets the performance gate.

For `Min`/`Max` on dense: keep `matrixStats::rowMaxs/rowMins` (unchanged).

### 3.3 Sparsity-preservation contract

All six sparse kernels in Section 3.1 operate on `@i`/`@x`/`@p`
directly. Call sites must **not** materialise `as.matrix(m)` before the
kernel. A unit-test helper `assert_no_densify_during(expr)` wraps
`as.matrix` via `trace()` and asserts zero invocations.

---

## 4. Grouped reduction rewrite

Touches `R/query_eval.R:616–681`.

### 4.1 Pattern inventory

Four grouped patterns observed in the current code:

| ID | Shape | Input | Output | Frequency |
|---|---|---|---|---|
| G1 | grouped vector | `vector + group[n]` | vector of length `ngroups` | common |
| G2 | matrix, rows grouped, `ReduceToColumn` | `matrix + row_group[nrow]` | matrix `ngroups × ncols` | common |
| G3 | matrix, cols grouped, `ReduceToRow` | `matrix + col_group[ncol]` | matrix `nrows × ngroups` | common |
| G4 | double reduction (RtR on row-grouped, RtC on col-grouped) | matrix + group | vector of length `ngroups` | rare |

### 4.2 New dispatch

Introduce helper `.reduction_builtin_label(fn)` which returns a string
("Sum" / "Mean" / "Var" / "Std" / "VarN" / "StdN" / "Min" / "Max" /
"Median" / "Quantile" / "GeoMean" / "Mode") when `fn` carries
`.dafr_builtin` with a recognised name, else `NA_character_`.

- **G1 (grouped vector)**:
  - If `label` is a numeric-fast-path op and input is numeric:
    - Sum/Mean/Count → `rowsum` / `tabulate` based single-pass R.
    - Min/Max → `vapply(split(...), min/max, numeric(1))` — R-level
      already C-speed, no kernel needed. (Using `numeric(1)` here is
      safe: the builtin label guarantees numeric output. The
      `numeric(1)` removal in Section 4.3 targets the non-builtin
      fallback path only.)
    - Var/Std/VarN/StdN/GeoMean → delegate to a 1D kernel
      `kernel_grouped_reduce_vec_cpp` (new) or a vectorised R equivalent
      using `rowsum` on `x` and `x^2`. Pick whichever meets the gate.
    - Median/Quantile → `kernel_grouped_quantile_vec_cpp` or
      `data.table`-free R using `sort` per group (benchmark).
    - Mode (numeric) → hash-per-group in R or
      `kernel_grouped_mode_vec_cpp`.
  - Mode on character (label="Mode", `is.character(x)`): dedicated
    helper `.grouped_mode_character(x, group)` using `tabulate` via
    `fastmatch::fmatch` if available, else base `match`.
  - Non-builtin `fn`: fallback **drops `numeric(1)`**. Sniff: call
    `fn` on the first non-empty group, inspect result storage (`numeric`
    / `integer` / `logical` / `character`), allocate `vapply` prototype
    accordingly.

- **G2 / G3 (single-reduction matrix)**: dispatch to
  `kernel_grouped_reduce_csc_cpp` (sparse) or
  `kernel_grouped_reduce_dense_cpp` (dense) with:

  ```cpp
  [[cpp11::register]]
  cpp11::writable::doubles_matrix<> kernel_grouped_reduce_csc_cpp(
      cpp11::integers i, cpp11::doubles x, cpp11::integers p,
      int nrow, int ncol,
      cpp11::integers group,   // per-row group (G2) or per-col group (G3), 1-based
      int ngroups,
      int axis,                // 2 = G2 (row-group), 3 = G3 (col-group)
      std::string op,          // "Sum"|"Mean"|"Min"|"Max"|"Var"|"Std"|"VarN"|"StdN"|"GeoMean"
      cpp11::list params       // op-specific (eps, etc.)
  );
  ```

  Median/Quantile and Mode use separate specialised kernels
  (`kernel_grouped_quantile_csc_cpp`, `kernel_grouped_mode_csc_cpp`)
  because they cannot be folded into the single-pass accumulator
  pattern.

- **G4 (double reduction)**: decompose as
  `g4(m, group, op) = g1(reduce_matrix(m, inner_axis, op), group, op)`.
  No new kernel — composed from Section 3 + G1.

### 4.3 Type-sniffing fallback

Replaces both hard-coded `vapply(..., numeric(1))` sites. Procedure:

1. Identify first non-empty group; call `fn(x[first_group], params)`;
   capture `proto <- result`.
2. Allocate result storage: `numeric`/`integer`/`logical`/`character`
   vector of length `ngroups`, based on `typeof(proto)` and `length(proto)`
   (error if `length(proto) != 1`).
3. Fill result via a typed loop (not `vapply` — to avoid re-sniffing cost
   and support NA values consistently).

Any group with zero elements gets the type-default NA (`NA_real_`,
`NA_integer_`, `NA`, `NA_character_`).

### 4.4 Output sparsity for G2/G3

Even for zero-preserving ops (Sum/Mean/GeoMean), the output matrix is
`ngroups × nother` — typically small relative to the input (since
`ngroups ≪ nrow` / `ncol`). **Always emit a dense numeric matrix.** No
CSC-output variant implemented. Document this in the NEWS entry.

### 4.5 Backwards-compat risk

Existing users with custom non-builtin reductions that relied on the
implicit `numeric(1)` coercion may see behaviour changes if their
function returns `NA_integer_` or other non-double values. Mitigation:
add a dedicated test for user-function type round-trip (integer, logical,
character, NA preservation).

---

## 5. Non-reduction fixes

### 5.1 `complete_daf` base_daf_view JSON re-apply

**File:** `R/complete.R`. Write site at `:80` (unchanged). Read site at
`:136` needs an addition.

Current `complete_daf` returns the plain chain. New behaviour:

```r
complete_daf <- function(path, ..., mode = "r") {
    chain <- <existing chain reconstruction from base_daf_repository>
    view_json <- .scalar_if_exists(chain, "base_daf_view")
    if (!is.null(view_json)) {
        spec <- jsonlite::fromJSON(view_json, simplifyVector = FALSE)
        chain <- viewer(chain, name = <path-derived>,
                        axes = spec$axes, data = spec$data)
    }
    chain
}
```

`.scalar_if_exists(daf, name)` is a new private helper that returns
`NULL` if the scalar is absent (rather than raising).

**Test** (extend `tests/testthat/test-complete.R`):

1. Build a `MemoryDaf` with axes + vectors + matrices.
2. Wrap with `viewer(m, axes = ..., data = ...)` (with a renamed axis).
3. `complete_chain(view, tempfile())`.
4. `complete_daf(...)` → assert class `ViewDaf`, assert renamed axis
   query returns the same values as the original view.

### 5.2 `.op_convert` sparse preservation

**File:** `R/operations.R:166–186`.

For `dgCMatrix` input:

| `type` | Action | Output class |
|---|---|---|
| `double` / `numeric` | identity | `dgCMatrix` |
| `integer` | assert integer-castable `@x`; canonicalise `@x <- as.double(as.integer(@x))` | `dgCMatrix` |
| `logical` | `@x <- as.double(@x != 0)` + `Matrix::drop0` | `dgCMatrix` |
| `character` | `as.matrix` then `as.character` (dense, unavoidable) | `character matrix` |

`.op_convert` must signal a clear error when integer coercion would lose
precision (match Julia's `InexactError` semantics).

### 5.3 `.matrix_type_ok` character + sparse integer

**File:** `R/contracts.R:581–590`. New body:

```r
.matrix_type_ok <- function(m, type_name) {
    switch(type_name,
        integer   = .is_integer_valued(m),
        numeric   = is.numeric(m[1L]),
        double    = is.double(m[1L]),
        logical   = is.logical(m[1L]) || .is_logical_valued_sparse(m),
        character = is.character(m[1L]),
        inherits(m, type_name)
    )
}

.is_integer_valued <- function(m) {
    if (is.integer(m[1L])) return(TRUE)
    if (inherits(m, "dgCMatrix")) {
        return(length(m@x) == 0L ||
               (all(m@x == floor(m@x)) && max(abs(m@x)) < .Machine$integer.max))
    }
    FALSE
}

.is_logical_valued_sparse <- function(m) {
    inherits(m, "dgCMatrix") &&
        (length(m@x) == 0L || all(m@x %in% c(0, 1)))
}
```

Unit tests: one test per switch branch × (dense, sparse) combinations.

---

## 6. Testing and benchmarks

### 6.1 Tests

**Per-kernel unit tests** — new file `tests/testthat/test-kernels-slice8.R`:

- Correctness: random input (seeded), assert kernel result ≈ slow-path
  R result within `1e-9`; dense + sparse; row axis + col axis; with +
  without params.
- Edge cases per op: empty column, single-element column, all-explicit
  column (no implicit zeros), all-implicit column, extreme values
  (1e15), negative values, NA handling (error vs propagate — match
  Slice-7 behaviour).
- Sparsity preservation: `assert_no_densify_during(fast_result <- ...)`
  — new test helper (placed in `tests/testthat/helper-assertions.R`)
  that wraps `as.matrix` via `trace()` and asserts zero invocations
  inside the expression.

**Grouped tests** — new file `tests/testthat/test-query-grouped-slice8.R`:

- All four patterns G1–G4 × each builtin op × dense + sparse.
- Mode-on-character: grouped vector reduction over a character
  property; assert `character(ngroups)` output.
- Type-sniffing fallback: user-defined reduction returning `integer(1)`,
  `logical(1)`, `character(1)`, `NA_real_`; verify output storage.

**Byte-parity extension** — extend `tests/testthat/fixtures/julia-queries/`
with new records covering:

- Grouped reductions (G1–G4) for each Slice-7 op that doesn't already
  have a grouped record.
- Mode-on-character (new Julia fixture entry).
- `Convert` to integer / logical on sparse input (new fixture entry).

Regeneration via `dev/julia/regen-fixture.jl` (check DAF.jl commit first
— must still be `49fbba1`; if moved, follow Slice-7 exit's regen
procedure).

**Integration test** — extend `tests/testthat/test-complete.R`:

- `complete_chain(viewer(memory_daf, ...))` → `complete_daf` → assert
  `ViewDaf`, compare queries against original.

### 6.2 Benchmark harness (pass/fail exit gate)

**File:** `benchmarks/slice-8-reduction-kernels.R` using `bench::mark`.

Fixtures: 10k × 10k `dgCMatrix` with 5% nnz (5M nonzeros), 5k × 5k
dense numeric matrix, grouped variants with 100 groups uniformly
assigned.

Gates (must all pass for slice exit):

| Case | Baseline | Target |
|---|---|---|
| Var (sparse row reduce, 10k × 10k, 5% nnz) | slow path `apply` | ≥ 10× faster |
| Median (sparse row reduce) | slow path | ≥ 10× faster |
| GeoMean (sparse row reduce) | slow path | ≥ 10× faster |
| Min/Max (sparse) | current `as.matrix+matrixStats` | ≥ 5× faster AND ≥ 10× less peak memory |
| Var (dense row reduce, 5k × 5k) | slow path | ≥ 10× faster |
| G3 grouped Sum (100 groups, sparse) | split+apply | ≥ 20× faster |
| G3 grouped Var (100 groups, sparse) | split+apply | ≥ 20× faster |
| G1 grouped Median (100 groups, 10M values) | split+apply | ≥ 10× faster |

Peak memory via `bench::bench_memory` or `Rprofmem` sampling.

Results committed to
`dev/benchmarks/slice-8-results-YYYY-MM-DD.csv`.

### 6.3 Regression tests

`devtools::test()`: 1448 PASS (Slice-7 baseline) → still 1448+ PASS.
No fails, no new skips.

`devtools::check(error_on = "note")` with `_R_CHECK_SYSTEM_CLOCK_=0`:
0 ERROR / 0 WARNING / 0 NOTE (Slice-7 baseline).

---

## 7. Dependencies, risks, and mines

### 7.1 Dependencies

No new R package dependencies. All sparse work via existing cpp11 +
OpenMP infrastructure. `matrixStats` already in Imports.

### 7.2 New C++ risk surface

Six new kernels + three grouped specialisations (9 `.cpp` files
total). Mitigations:

- cpp11 (not Rcpp) matches existing convention — no ABI drift.
- All column-parallel via `openmp_shim.h`; same pragma pattern as
  `kernel_log_reduce_csc_cpp`.
- Unit tests compare against the slow R path for correctness — the
  slow path is authoritative.

### 7.3 Mines closed by this slice

- `Min`/`Max` densifies sparse via `as.matrix()` (Slice-3 inherited).
- `.apply_reduction_slow` densifies every Slice-7 sparse reduction.
- `.apply_reduction_grouped_*` uses `vapply(..., numeric(1))` (blocks
  Mode-on-char).
- `.op_convert` densifies sparse for integer/logical.
- `.matrix_type_ok` missing `character` case and rejects integer-valued
  sparse.
- `complete_daf` drops the view on reopen.

### 7.4 Mines still open after Slice 8

- `.cast_matrix_type("integer", dgCMatrix)` densifies (in `copy_*`,
  not `Convert` — separate concern, deferred).
- `.op_convert` sparse→character still densifies (unavoidable, no
  sparse character class).
- Mode on character for **matrix** input (only grouped-vector path is
  in scope; character matrices are rare in DAF usage).
- Julia-side `significant!` UInt32 underflow (fixture routes around it).
- Long-vector ALTREP, UInt32 > 2³¹ read arm, multi-writer locking.

---

## 8. Execution approach

1. **Plan** via `superpowers:writing-plans` — break into
   ~10 independent subagent-shaped tasks:
   - One task per new kernel (6 ungrouped + 3 grouped = 9 tasks;
     Sonnet-shaped, mechanical).
   - One task for R-level grouped rewrite (Opus-shaped, design-heavy).
   - One task for `complete_daf` + `.op_convert` + `.matrix_type_ok`
     (Sonnet-shaped, independent of reductions).
   - One task for benchmark harness + fixture regen (Sonnet-shaped).
2. **Execute** via `superpowers:subagent-driven-development`.
3. **Review** at the end with a whole-branch Opus review pass.
4. **Exit gate**: all unit + grouped + byte-parity tests pass,
   `devtools::check` clean, all benchmark gates in Section 6.2 pass,
   new fixture entries committed.

### 8.1 Julia DAF state check

Before regenerating any fixture, verify
`~/src/DataAxesFormats.jl` is still at
`49fbba140437387a378217c2fa658d4231d0c8c1`. If moved, follow the
regen procedure in `dev/notes/slice-7-exit.md`.

### 8.2 Branching

Feature branch `slice-8-matrix-fastpaths` off `main@slice-7`.
Merge to `main` with `--no-ff`, tag `slice-8` on the merge commit.

---

## 9. Exit checklist

- [ ] All 9 new C++ kernels implemented, registered in cpp11, tested.
- [ ] Grouped-reduction rewrite complete; `numeric(1)` vapply contract
      removed.
- [ ] Mode-on-character working for grouped vectors (with fixture
      byte-parity).
- [ ] `.op_convert` sparse→integer / sparse→logical preserves sparsity.
- [ ] `.matrix_type_ok` character + sparse-integer cases added.
- [ ] `complete_daf` re-applies `base_daf_view` JSON on reopen.
- [ ] `tests/testthat/test-kernels-slice8.R` added and passing.
- [ ] `tests/testthat/test-query-grouped-slice8.R` added and passing.
- [ ] `tests/testthat/test-complete.R` extended and passing.
- [ ] Julia-queries fixture extended for grouped ops, Mode-on-char,
      and Convert-sparse-int.
- [ ] `benchmarks/slice-8-reduction-kernels.R` results meet all gates
      in Section 6.2.
- [ ] `devtools::test()` — 0 FAIL, 0 new SKIP.
- [ ] `devtools::check(error_on = "note")` — 0 / 0 / 0.
- [ ] NEWS entry updated.
- [ ] Dev-repo notes: `dev/notes/slice-8-exit.md` written.
