# Slice 7 — Ops Expansion

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the query-op surface gap against Julia DAF. Add the 12 remaining eltwise and reduction ops (Clamp, Convert, Fraction, Significant, Var, Std, VarN, StdN, Median, Quantile, GeoMean, Mode), register them as defaults, and extend the Julia-parity fixture where warranted. All R; no new C++.

**Architecture:**

- **Scope is per-element scalar functions**, not column-iterated matrix kernels. Each op is an R function `.op_<name>(x, ..., <params>)` registered via `register_eltwise` / `register_reduction`. Matrix iteration is already handled by `.apply_reduction_slow()` at `R/query_eval.R:592` (which calls `apply(m, margin, fn)` per row/column). We do not add new matrix kernels in this slice — if the existing `.apply_reduction_slow` is a perf bottleneck on any op, fix it in a follow-up. This matches the kickoff directive "start R-only; add C++ only where profiling warrants".
- **Sparse-aware eltwise paths.** Three eltwise ops have a sparse branch inside the registered function itself: **Fraction** (column-normalize — divide `@x` slice by column sum; zeros stay zero), **Significant** (operate on `@x`, then `Matrix::drop0`), and **Clamp when `min ≤ 0 ≤ max`** (clamp `@x` in place; zeros stay zero). Clamp with a non-straddling range, Convert to integer, and all other eltwise ops dense-coerce sparse input — same behavior pattern as the existing Abs/Exp/Sqrt ops.
- **`.dafr_builtin` attributes attach to every new op.** Required for `.apply_eltwise` / `.apply_reduction_fast` dispatch even though no Slice-7 op currently gets a fast path. Future fast-path additions (e.g., `matrixStats::rowVars` for `Var`) will check this attribute, so land it now to avoid a re-touch.
- **No `type` parameter.** Julia's eltwise/reduction ops carry an optional `type` parameter for explicit output-type control (using `float_type_for`, `int_type_for`, `sum_type_for`). R's type model is coarser — `double` / `integer` / `logical` — and the existing 5 default ops all return `double` regardless of input. We mirror that. The one exception is **Convert**, whose sole purpose is explicit type conversion; its `type` parameter accepts `"double"` / `"integer"` / `"logical"` (short R names, not Julia's `"Float64"` / `"Int32"`).
- **Mode: numeric-only this slice.** Julia's `Mode` supports strings via `supports_strings(::Mode) = true`. R's grouped-reduction path at `R/query_eval.R:620` uses `vapply(..., numeric(1))` which coerces a string return to NA. Rather than refactor `.apply_reduction_grouped_*` now (cross-cutting; invites regressions in Sum/Mean/etc.), we register `.op_mode` as numeric-only — raises informatively on character input. String-axis grouping is documented as deferred in the exit note.
- **Convert sparse→integer dense-coerces.** Julia's `Convert` has no fast sparse→integer path; neither does R (the existing `.cast_matrix_type("integer", dgCMatrix)` mine does the same). Documented; no test for sparse→integer matrices exercises this mine. If a future user hits it, it will densify but stay correct.
- **Julia fixture extension.** The existing `tests/testthat/fixtures/julia-queries/` fixture covers query evaluation for Abs/Log/Exp/Sqrt/Round/Sum/Mean/Max/Min/Count. We extend it with one additional fixture case per new op that emits a deterministic numeric value — 12 new end-to-end query cases (1 per op) with byte-parity against DAF.jl's output. Extension script: `dev/scripts/extend-julia-queries-fixture-slice7.jl`.

**Tech Stack:**

- R 4.4+, S7 0.2.1. `Matrix` for sparse ops (Imports). `matrixStats` for `Var`/`Std` on dense matrices in tests (already Imports — used for validation, not for fast paths in this slice). `stats` base package for `var`, `sd`, `median`, `quantile`. No new dependencies. No new C++.
- Julia side (fixture extension only): `DataAxesFormats.jl` at `49fbba140437387a378217c2fa658d4231d0c8c1` or newer (check for moves with `git -C ~/src/DataAxesFormats.jl pull --ff-only` before regen). Conda env `dafr-mcview`.

**Repo layout:**

- Package repo: `/home/aviezerl/src/dafr-native/` on `main` at tag `slice-6` / commit `e38c53a`. Tracks `git@github.com:tanaylab/dafr.git`. Source, tests, `inst/` commits → package repo. Execute on a feature branch `slice-7-ops-expansion` (created at Phase 0; final merge at Phase Z).
- Dev repo (nested, gitignored): `/home/aviezerl/src/dafr-native/dev/` on `main` at `a04b1d1` (post-Slice-6 exit). Plans, notes, scripts → dev repo.

**Dev loop per task:**

1. From `/home/aviezerl/src/dafr-native/`:
   ```
   Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "operations-registry|<other-filter>")'
   ```
2. Inspect; iterate to green.
3. Stage + commit with the message given in the task. Package repo vs. dev repo: infer from the file path. **Never `--amend`, `--no-verify`, or force-push.** Use `/bin/rm` / `/bin/cp` (aliased with `-i`). Wait for permission prompts.

**Known mines from Slice 6 (honor throughout):**

- **`.cast_matrix_type("integer", dgCMatrix)`** dense-coerces. Slice 7's `Convert` op accepts integer target and inherits this path. Document, do not exercise.
- **`.matrix_type_ok` missing `character` case** (pre-existing Slice 4 mine). **Mode** on numeric-only mitigates. Flagged again in exit.
- **`copy_all` axis-collision is LAZY.** Not touched by Slice 7.
- **`.concat_axis_matrix` transposes via `Matrix::t()`.** Not touched.

---

## Pre-planning decisions (settled before tasks)

### 1. Phase order

0 → A → B → C → D → E → F → G → H → I → J → K → Z, in that sequence:

- **Phase 0 (branch setup):** create `slice-7-ops-expansion`; no code changes.
- **A (Clamp):** smallest eltwise, no sparse decision needed for straddling bounds. Warm-up.
- **B (Convert):** explicit type-conversion eltwise. Establishes the `type` parameter convention for this slice.
- **C (Fraction):** sparse-preserving column-normalize. First sparse branch in a registered fn.
- **D (Significant):** dual-threshold sparse-preserving. Second sparse branch. Most complex eltwise.
- **E (Var + Std):** paired reductions — Std is `sqrt(Var)`. Uncorrected (`n`-denom, not `n-1`). Share a common `.op_var` helper.
- **F (VarN + StdN):** normalized variants; divide by `mean + eps`. Built on Phase E helper.
- **G (Median + Quantile):** paired reductions. Quantile takes a required `p` parameter.
- **H (GeoMean):** geometric mean with optional `eps` regularization.
- **I (Mode):** most-frequent-value reduction. Numeric-only this slice.
- **J (Default-op registration + NAMESPACE + NEWS):** Wire all 12 into `.register_default_ops()`; regen docs.
- **K (Julia fixture extension):** 12 new end-to-end query cases against DAF.jl.
- **Z (Polish):** `devtools::check(error_on = "note")`, exit note, tag, merge.

### 2. `na_rm` convention

All ops that could encounter NA (all except Count, Mode) expose `na_rm = FALSE` as a keyword, mirroring the existing Sum/Mean/Max/Min convention. Default is `FALSE` (NA propagates). Query users supply it as `% Op na_rm: 1`.

### 3. Parameter coercion and query-string names

The evaluator's `.coerce_params()` at `R/query_eval.R:683` casts numeric-looking strings to numeric and leaves others as strings. For Slice 7 ops:

- `eps`, `min`, `max`, `digits`, `p`, `high`, `low`, `base` — all numeric. Coercion is automatic.
- `type` (Convert only) — string. Accepted values: `"double"`, `"integer"`, `"logical"`. Validated in `.op_convert`.
- `na_rm` — user supplies `0` or `1` in query string; `.coerce_params` produces numeric; the op casts via `isTRUE(as.logical(na_rm))` or coerces at the caller boundary.

### 4. Return-type invariant

All ops return double except: `Count` (integer, unchanged), `Convert(type = "integer")` (integer), `Convert(type = "logical")` (logical), `Mode` (input type preserved). Test expectations encode this.

### 5. Test file layout

- `tests/testthat/test-operations-registry.R` — existing file, unchanged for the registration / name lookup / collision tests.
- `tests/testthat/test-operations-eltwise.R` — **new file** for Slice 7 eltwise per-op tests (Clamp, Convert, Fraction, Significant) + regression tests for existing eltwise (Abs/Log/Exp/Sqrt/Round — move them here from `test-operations-registry.R` in Phase J to keep the registry file focused on mechanics).
- `tests/testthat/test-operations-reductions.R` — **new file** for Slice 7 reduction per-op tests (Var, Std, VarN, StdN, Median, Quantile, GeoMean, Mode) + regression tests for existing reductions (Sum/Mean/Max/Min/Count — move in Phase J).
- `tests/testthat/test-operations-query.R` — **new file** for end-to-end query tests (`%`, `>|`, `|>` over the new ops).
- `tests/testthat/test-julia-queries-compat.R` — existing file, extended in Phase K with the new fixture cases.

Keep the `R/operations.R` file intact and append new helpers to it — we do not split into multiple op files. It will be ~400 LoC at the end of this slice, still readable.

---

## Phase 0: Branch setup

**Files:** none (package + dev repo state only).

- [ ] **Step 0.1: Create Slice-7 feature branch in the package repo**

```bash
cd /home/aviezerl/src/dafr-native
git status -s   # expect clean, at tag slice-6 / main commit e38c53a
git checkout -b slice-7-ops-expansion
```

- [ ] **Step 0.2: Verify dev repo is on main**

```bash
cd /home/aviezerl/src/dafr-native/dev
git status -s   # expect clean; branch main at a04b1d1 or newer
```

- [ ] **Step 0.3: Verify baseline green**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: 1315 PASS / 0 FAIL / 0 SKIP / 1 WARN (the pre-existing scran/irlba SVD tolerance notice).

- [ ] **Step 0.4: Commit the plan file to the dev repo**

```bash
cd /home/aviezerl/src/dafr-native/dev
git add plans/2026-04-21-slice-7-ops-expansion.md
git commit -m "plan: slice 7 ops expansion"
```

---

## Phase A: Clamp (eltwise)

**Files:**
- Modify: `R/operations.R` (add `.op_clamp`, attribute)
- Create: `tests/testthat/test-operations-eltwise.R` (new file; start with Clamp tests)

**Julia reference:** `~/src/DataAxesFormats.jl/src/operations.jl:527-613`. Signature `Clamp(; min = -Inf, max = Inf)` with `@assert min < max`. Sparse input: column-wise clamp — but if `min > 0` or `max < 0`, zeros become `min` or `max` respectively and sparsity is lost. We implement sparse-preserving only when `min ≤ 0 ≤ max`.

- [ ] **Step A.1: Create the new eltwise test file with Clamp tests**

File: `tests/testthat/test-operations-eltwise.R`

```r
# Per-op eltwise behaviour tests. Registration / lookup / collision are in
# test-operations-registry.R.  Slice-7 split: this file carries the new ops
# (Clamp, Convert, Fraction, Significant) plus the legacy ones moved from
# test-operations-registry.R in Phase J.

test_that("Clamp on numeric vector respects min and max", {
    fn <- get_eltwise("Clamp")
    expect_equal(fn(c(-2, -1, 0, 1, 2), min = -1, max = 1), c(-1, -1, 0, 1, 1))
})

test_that("Clamp defaults are -Inf / +Inf (pass-through)", {
    fn <- get_eltwise("Clamp")
    expect_equal(fn(c(-5, 0, 5)), c(-5, 0, 5))
})

test_that("Clamp errors on min >= max", {
    fn <- get_eltwise("Clamp")
    expect_error(fn(1:3, min = 2, max = 1), "min.*max")
    expect_error(fn(1:3, min = 1, max = 1), "min.*max")
})

test_that("Clamp preserves sparsity when 0 is in range", {
    m <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = c(-5, 5), dims = c(3, 2))
    out <- get_eltwise("Clamp")(m, min = -2, max = 2)
    expect_s4_class(out, "dgCMatrix")
    expect_equal(out@x, c(-2, 2))
})

test_that("Clamp dense-coerces sparse input when 0 not in range", {
    m <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = c(-5, 5), dims = c(3, 2))
    out <- get_eltwise("Clamp")(m, min = 1, max = 10)
    expect_false(methods::is(out, "dgCMatrix"))
    expect_equal(sum(out == 1), 5)   # 4 former-zeros + the former -5 all clamp to 1
    expect_equal(out[1, 1], 1)       # -5 -> 1
    expect_equal(out[3, 2], 5)       # 5 stays 5
})

test_that("Clamp attaches .dafr_builtin = 'Clamp'", {
    fn <- get_eltwise("Clamp")
    expect_identical(attr(fn, ".dafr_builtin"), "Clamp")
})
```

- [ ] **Step A.2: Run tests — expect FAIL (Clamp not registered)**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-eltwise.R")'
```

Expected: all 6 tests FAIL with `unknown eltwise operation: 'Clamp'`.

- [ ] **Step A.3: Implement `.op_clamp` in `R/operations.R`**

Append after the existing `.op_round` definition (around line 147):

```r
.op_clamp <- function(x, ..., min = -Inf, max = Inf) {
    if (min >= max) {
        stop(sprintf("Clamp: min (%g) must be strictly less than max (%g)", min, max),
            call. = FALSE
        )
    }
    if (methods::is(x, "dgCMatrix")) {
        if (min <= 0 && 0 <= max) {
            out <- x
            out@x <- pmin(pmax(out@x, min), max)
            return(out)
        }
        # pmin(pmax(sparse, min), max) keeps the dgCMatrix class but explicitly
        # stores every entry (nnz == full length). Force a real dense matrix.
        x <- as.matrix(x)
    }
    pmin(pmax(x, min), max)
}
attr(.op_clamp, ".dafr_builtin") <- "Clamp"
```

And add the registration line inside `.register_default_ops()` (phase J wires this; for now, add a temporary manual registration at the end of `.register_default_ops()` alongside the existing eltwise:

```r
register_eltwise("Clamp", .op_clamp, overwrite = TRUE)
```

- [ ] **Step A.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-eltwise.R")'
```

Expected: 6 PASS.

- [ ] **Step A.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-eltwise.R
git commit -m "feat(ops): add Clamp eltwise op (sparse-preserving when 0 in range)"
```

---

## Phase B: Convert (eltwise)

**Files:**
- Modify: `R/operations.R` (add `.op_convert`)
- Modify: `tests/testthat/test-operations-eltwise.R` (add Convert tests)

**Julia reference:** `operations.jl:624-676`. Julia has many numeric target types (Float32/Float64/Int8/.../UInt32/...). R has three: `double`, `integer`, `logical`. Accepted strings: `"double"`, `"integer"`, `"logical"`. Required parameter — no default. Sparse→double preserves sparsity; sparse→integer/logical dense-coerces (inherits the `.cast_matrix_type` mine from Slice 6).

- [ ] **Step B.1: Add Convert tests**

Append to `tests/testthat/test-operations-eltwise.R`:

```r
test_that("Convert changes vector storage mode", {
    fn <- get_eltwise("Convert")
    expect_type(fn(c(1.0, 2.0, 3.0), type = "integer"), "integer")
    expect_equal(fn(c(1.5, 2.9), type = "integer"), c(1L, 2L))  # truncation
    expect_type(fn(c(1L, 2L, 3L), type = "double"), "double")
    expect_type(fn(c(0, 1, 1), type = "logical"), "logical")
    expect_equal(fn(c(0, 1, 2), type = "logical"), c(FALSE, TRUE, TRUE))
})

test_that("Convert requires type parameter", {
    fn <- get_eltwise("Convert")
    expect_error(fn(c(1, 2, 3)), "type")
})

test_that("Convert rejects unknown type names", {
    fn <- get_eltwise("Convert")
    expect_error(fn(c(1, 2, 3), type = "float64"), "type.*double.*integer.*logical")
    expect_error(fn(c(1, 2, 3), type = "string"), "type")
})

test_that("Convert preserves sparsity for target 'double'", {
    m <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = c(1.5, 2.5), dims = c(3, 2))
    out <- get_eltwise("Convert")(m, type = "double")
    expect_s4_class(out, "dgCMatrix")
    expect_equal(out@x, c(1.5, 2.5))
})

test_that("Convert attaches .dafr_builtin", {
    expect_identical(attr(get_eltwise("Convert"), ".dafr_builtin"), "Convert")
})
```

- [ ] **Step B.2: Run tests — expect FAIL**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-eltwise.R")'
```

Expected: 5 new tests FAIL.

- [ ] **Step B.3: Implement `.op_convert`**

Append to `R/operations.R`:

```r
.op_convert <- function(x, ..., type) {
    if (missing(type)) {
        stop("Convert: 'type' parameter is required (one of 'double', 'integer', 'logical')",
            call. = FALSE
        )
    }
    if (!is.character(type) || length(type) != 1L || !type %in% c("double", "integer", "logical")) {
        stop(sprintf(
            "Convert: 'type' must be one of 'double', 'integer', 'logical' (got %s)",
            sQuote(as.character(type)[1L])
        ), call. = FALSE)
    }
    if (methods::is(x, "dgCMatrix") && type == "double") {
        return(x)
    }
    if (methods::is(x, "dgCMatrix")) {
        x <- as.matrix(x)  # storage.mode<- on a dgCMatrix does not convert dtype cleanly
    }
    storage.mode(x) <- type
    x
}
attr(.op_convert, ".dafr_builtin") <- "Convert"
```

Add inside `.register_default_ops()`:

```r
register_eltwise("Convert", .op_convert, overwrite = TRUE)
```

- [ ] **Step B.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-eltwise.R")'
```

Expected: all eltwise tests PASS.

- [ ] **Step B.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-eltwise.R
git commit -m "feat(ops): add Convert eltwise op (double/integer/logical target)"
```

---

## Phase C: Fraction (eltwise)

**Files:**
- Modify: `R/operations.R`
- Modify: `tests/testthat/test-operations-eltwise.R`

**Julia reference:** `operations.jl:691-741`. Vectors normalize to `x / sum(x)` (zeros if `sum == 0`). Matrices normalize each **column** to sum-1. Scalars error. Sparse matrices: divide each column's `@x` slice by that column's sum, preserving sparsity.

- [ ] **Step C.1: Add Fraction tests**

Append to `tests/testthat/test-operations-eltwise.R`:

```r
test_that("Fraction normalises a numeric vector to sum 1", {
    fn <- get_eltwise("Fraction")
    expect_equal(fn(c(1, 1, 2)), c(0.25, 0.25, 0.5))
    expect_equal(fn(c(1, 2, 3)), c(1 / 6, 2 / 6, 3 / 6))
})

test_that("Fraction returns zeros when the vector total is 0", {
    fn <- get_eltwise("Fraction")
    expect_equal(fn(c(0, 0, 0)), c(0, 0, 0))
    expect_equal(fn(c(1, -1, 0)), c(0, 0, 0))  # sum == 0 -> zeros
})

test_that("Fraction normalises each matrix column independently", {
    fn <- get_eltwise("Fraction")
    m <- matrix(c(1, 1, 2, 4), nrow = 2, ncol = 2)  # col sums: 2, 6
    expect_equal(fn(m), matrix(c(0.5, 0.5, 2 / 6, 4 / 6), nrow = 2))
})

test_that("Fraction preserves sparsity on a dgCMatrix", {
    m <- Matrix::sparseMatrix(
        i = c(1, 2, 1, 3), j = c(1, 1, 2, 2), x = c(1, 1, 2, 4),
        dims = c(3, 2)
    )
    out <- get_eltwise("Fraction")(m)
    expect_s4_class(out, "dgCMatrix")
    expect_equal(as.numeric(Matrix::colSums(out)), c(1, 1))
    expect_equal(out[1, 1], 0.5)
    expect_equal(out[3, 2], 4 / 6)
})

test_that("Fraction on a sparse column with sum 0 yields that column all-zero", {
    m <- Matrix::sparseMatrix(
        i = c(1, 2, 1), j = c(1, 1, 2), x = c(1, -1, 5),
        dims = c(3, 2)
    )  # col 1 sum == 0; col 2 sum == 5
    out <- get_eltwise("Fraction")(m)
    expect_s4_class(out, "dgCMatrix")
    expect_equal(as.numeric(Matrix::colSums(out)[1]), 0)
    expect_equal(as.numeric(Matrix::colSums(out)[2]), 1)
})

test_that("Fraction on a bare scalar errors", {
    expect_error(get_eltwise("Fraction")(5), "scalar")
})

test_that("Fraction attaches .dafr_builtin", {
    expect_identical(attr(get_eltwise("Fraction"), ".dafr_builtin"), "Fraction")
})
```

- [ ] **Step C.2: Run tests — expect FAIL**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-eltwise.R")'
```

Expected: 7 new Fraction tests FAIL.

- [ ] **Step C.3: Implement `.op_fraction`**

Append to `R/operations.R`:

```r
.op_fraction <- function(x, ...) {
    if (is.null(dim(x)) && length(x) == 1L) {
        stop("Fraction: cannot apply to a scalar", call. = FALSE)
    }
    if (methods::is(x, "dgCMatrix")) {
        out <- x
        col_sums <- Matrix::colSums(out)
        for (j in seq_along(col_sums)) {
            start <- out@p[j] + 1L
            end <- out@p[j + 1L]
            if (start <= end) {
                if (col_sums[j] != 0) {
                    out@x[start:end] <- out@x[start:end] / col_sums[j]
                } else {
                    out@x[start:end] <- 0
                }
            }
        }
        return(out)
    }
    if (is.matrix(x)) {
        col_sums <- colSums(x)
        out <- x
        storage.mode(out) <- "double"
        for (j in seq_len(ncol(out))) {
            out[, j] <- if (col_sums[j] == 0) 0 else out[, j] / col_sums[j]
        }
        return(out)
    }
    total <- sum(x)
    if (total == 0) return(rep(0, length(x)))
    x / total
}
attr(.op_fraction, ".dafr_builtin") <- "Fraction"
```

Register inside `.register_default_ops()`:

```r
register_eltwise("Fraction", .op_fraction, overwrite = TRUE)
```

- [ ] **Step C.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-eltwise.R")'
```

Expected: all eltwise tests PASS.

- [ ] **Step C.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-eltwise.R
git commit -m "feat(ops): add Fraction eltwise (sparse-preserving column-normalize)"
```

---

## Phase D: Significant (eltwise)

**Files:**
- Modify: `R/operations.R`
- Modify: `tests/testthat/test-operations-eltwise.R`

**Julia reference:** `operations.jl:890-1004`. Required `high` (positive). Optional `low` (defaults to `high`; must be `0 ≤ low ≤ high`). Per column (matrix) or per vector (vector): if **all** `|x| < high`, zero everything; else zero all `|x| < low`. Sparse: operate on `@x`, then `drop0`.

- [ ] **Step D.1: Add Significant tests**

Append to `tests/testthat/test-operations-eltwise.R`:

```r
test_that("Significant zeroes a vector whose max absolute value is below 'high'", {
    fn <- get_eltwise("Significant")
    expect_equal(fn(c(0.1, 0.2, 0.3), high = 1), c(0, 0, 0))
})

test_that("Significant with low == high keeps only entries >= high in abs", {
    fn <- get_eltwise("Significant")
    expect_equal(fn(c(0.5, 2, -3), high = 1), c(0, 2, -3))
})

test_that("Significant with low < high keeps entries >= low once any >= high exists", {
    fn <- get_eltwise("Significant")
    expect_equal(fn(c(0.1, 0.3, 2), high = 1, low = 0.2), c(0, 0.3, 2))
    # 0.1 zeroed (below low); 0.3 kept (above low); 2 kept (above high)
    # but if NO entry reaches high, whole vector is zeroed:
    expect_equal(fn(c(0.1, 0.3, 0.9), high = 1, low = 0.2), c(0, 0, 0))
})

test_that("Significant errors on invalid thresholds", {
    fn <- get_eltwise("Significant")
    expect_error(fn(1:3, high = -1), "high")
    expect_error(fn(1:3, high = 0), "high")
    expect_error(fn(1:3, high = 1, low = -1), "low")
    expect_error(fn(1:3, high = 1, low = 2), "low")
})

test_that("Significant on a matrix operates column-wise", {
    fn <- get_eltwise("Significant")
    m <- matrix(c(0.1, 0.2, 2, 3, 0.05, 0.05), nrow = 2)
    # col 1: max |x| = 0.2 < 1 -> zero out
    # col 2: max |x| = 3 >= 1 -> keep >= 1 in abs -> keep 2, 3
    # col 3: max |x| = 0.05 < 1 -> zero out
    out <- fn(m, high = 1)
    expect_equal(out[, 1], c(0, 0))
    expect_equal(out[, 2], c(2, 3))
    expect_equal(out[, 3], c(0, 0))
})

test_that("Significant on sparse preserves/drops zeros", {
    m <- Matrix::sparseMatrix(
        i = c(1, 2, 1, 3), j = c(1, 1, 2, 2), x = c(0.1, 0.2, 2, 0.5),
        dims = c(3, 2)
    )
    # col 1: max |x| = 0.2 < 1 -> zero out
    # col 2: max |x| = 2 >= 1, low default = 1 -> keep 2, drop 0.5
    out <- get_eltwise("Significant")(m, high = 1)
    expect_s4_class(out, "dgCMatrix")
    expect_equal(as.numeric(out[, 1]), c(0, 0, 0))
    expect_equal(as.numeric(out[, 2]), c(2, 0, 0))
})

test_that("Significant on scalar errors", {
    expect_error(get_eltwise("Significant")(5, high = 1), "scalar")
})

test_that("Significant attaches .dafr_builtin", {
    expect_identical(attr(get_eltwise("Significant"), ".dafr_builtin"), "Significant")
})
```

- [ ] **Step D.2: Run tests — expect FAIL**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-eltwise.R")'
```

Expected: 8 new Significant tests FAIL.

- [ ] **Step D.3: Implement `.op_significant`**

Append to `R/operations.R`:

```r
.significant_vec <- function(v, high, low) {
    if (max(abs(v)) < high) {
        return(rep(0, length(v)))
    }
    v[abs(v) < low] <- 0
    v
}

.op_significant <- function(x, ..., high, low = high) {
    if (missing(high)) {
        stop("Significant: 'high' parameter is required", call. = FALSE)
    }
    if (!is.numeric(high) || length(high) != 1L || high <= 0) {
        stop(sprintf("Significant: 'high' must be a positive number (got %s)",
            as.character(high)[1L]
        ), call. = FALSE)
    }
    if (!is.numeric(low) || length(low) != 1L || low < 0 || low > high) {
        stop(sprintf("Significant: 'low' must be in [0, high] (got %s; high = %g)",
            as.character(low)[1L], high
        ), call. = FALSE)
    }
    if (is.null(dim(x)) && length(x) == 1L) {
        stop("Significant: cannot apply to a scalar", call. = FALSE)
    }
    if (methods::is(x, "dgCMatrix")) {
        out <- x
        for (j in seq_len(ncol(out))) {
            start <- out@p[j] + 1L
            end <- out@p[j + 1L]
            if (start <= end) {
                out@x[start:end] <- .significant_vec(out@x[start:end], high, low)
            }
        }
        return(Matrix::drop0(out))
    }
    if (is.matrix(x)) {
        out <- x
        storage.mode(out) <- "double"
        for (j in seq_len(ncol(out))) {
            out[, j] <- .significant_vec(out[, j], high, low)
        }
        return(out)
    }
    .significant_vec(x, high, low)
}
attr(.op_significant, ".dafr_builtin") <- "Significant"
```

Register inside `.register_default_ops()`:

```r
register_eltwise("Significant", .op_significant, overwrite = TRUE)
```

- [ ] **Step D.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-eltwise.R")'
```

Expected: all eltwise tests PASS.

- [ ] **Step D.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-eltwise.R
git commit -m "feat(ops): add Significant eltwise (dual-threshold, sparse-aware)"
```

---

## Phase E: Var + Std (reductions)

**Files:**
- Modify: `R/operations.R`
- Create: `tests/testthat/test-operations-reductions.R` (new file)

**Julia reference:** `operations.jl:1584-1781`. `Var` uses `corrected = false` (n-denom, not n-1); `Std = sqrt(Var)`. Both return double, regardless of input type. R's `stats::var` defaults to `corrected = true` — we must explicitly compute the n-denom form.

- [ ] **Step E.1: Create the new reduction test file with Var + Std tests**

File: `tests/testthat/test-operations-reductions.R`:

```r
# Per-op reduction behaviour tests. Slice-7 split: this file carries the new
# ops (Var, Std, VarN, StdN, Median, Quantile, GeoMean, Mode) plus the legacy
# ones moved from test-operations-registry.R in Phase J.

test_that("Var returns the uncorrected variance (n-denom, not n-1)", {
    fn <- get_reduction("Var")
    # x = c(1, 2, 3); mean = 2; corrected (n-1) var = 1; uncorrected (n) = 2/3
    expect_equal(fn(c(1, 2, 3)), 2 / 3)
    expect_equal(fn(c(2, 2, 2)), 0)
})

test_that("Std is sqrt(Var) — uncorrected", {
    fn <- get_reduction("Std")
    expect_equal(fn(c(1, 2, 3)), sqrt(2 / 3))
    expect_equal(fn(c(2, 2, 2)), 0)
})

test_that("Var / Std handle NA via na_rm", {
    expect_true(is.na(get_reduction("Var")(c(1, NA, 3))))
    expect_equal(get_reduction("Var")(c(1, NA, 3), na_rm = TRUE), 1)
    # c(1, 3): mean = 2, uncorrected var = ((1-2)^2 + (3-2)^2) / 2 = 1
    expect_true(is.na(get_reduction("Std")(c(1, NA, 3))))
    expect_equal(get_reduction("Std")(c(1, NA, 3), na_rm = TRUE), 1)
})

test_that("Var on integer input returns double", {
    expect_type(get_reduction("Var")(c(1L, 2L, 3L)), "double")
})

test_that("Var / Std attach .dafr_builtin", {
    expect_identical(attr(get_reduction("Var"), ".dafr_builtin"), "Var")
    expect_identical(attr(get_reduction("Std"), ".dafr_builtin"), "Std")
})
```

- [ ] **Step E.2: Run tests — expect FAIL**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

Expected: 5 tests FAIL with `unknown reduction operation: 'Var'`.

- [ ] **Step E.3: Implement `.op_var` and `.op_std`**

Append to `R/operations.R`:

```r
.var_uncorrected <- function(x, na_rm) {
    if (na_rm) x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0L) return(NA_real_)
    if (anyNA(x)) return(NA_real_)
    mu <- sum(x) / n
    sum((x - mu)^2) / n
}

.op_var <- function(x, ..., na_rm = FALSE) {
    .var_uncorrected(as.numeric(x), isTRUE(na_rm))
}
attr(.op_var, ".dafr_builtin") <- "Var"

.op_std <- function(x, ..., na_rm = FALSE) {
    v <- .var_uncorrected(as.numeric(x), isTRUE(na_rm))
    if (is.na(v)) v else sqrt(v)
}
attr(.op_std, ".dafr_builtin") <- "Std"
```

Register inside `.register_default_ops()`:

```r
register_reduction("Var", .op_var, overwrite = TRUE)
register_reduction("Std", .op_std, overwrite = TRUE)
```

- [ ] **Step E.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

Expected: 5 PASS.

- [ ] **Step E.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-reductions.R
git commit -m "feat(ops): add Var and Std reductions (uncorrected, n-denom)"
```

---

## Phase F: VarN + StdN (reductions)

**Files:**
- Modify: `R/operations.R`
- Modify: `tests/testthat/test-operations-reductions.R`

**Julia reference:** `operations.jl:1652-1859`. `VarN = Var / (mean + eps)`, `StdN = Std / (mean + eps)`. `eps ≥ 0`, defaults to 0. These are "normalized" variants that guard against zero means.

- [ ] **Step F.1: Add VarN + StdN tests**

Append to `tests/testthat/test-operations-reductions.R`:

```r
test_that("VarN divides uncorrected variance by mean + eps", {
    fn <- get_reduction("VarN")
    # x = c(1, 2, 3); var = 2/3; mean = 2; VarN = (2/3) / 2 = 1/3
    expect_equal(fn(c(1, 2, 3)), (2 / 3) / 2)
    # with eps: (2/3) / (2 + 1)
    expect_equal(fn(c(1, 2, 3), eps = 1), (2 / 3) / 3)
})

test_that("VarN gives Inf or NaN when mean + eps == 0", {
    # c(-1, 0, 1): mean = 0, var = 2/3; VarN = (2/3) / 0 = Inf
    expect_equal(get_reduction("VarN")(c(-1, 0, 1)), Inf)
})

test_that("StdN divides uncorrected stdev by mean + eps", {
    fn <- get_reduction("StdN")
    expect_equal(fn(c(1, 2, 3)), sqrt(2 / 3) / 2)
    expect_equal(fn(c(1, 2, 3), eps = 1), sqrt(2 / 3) / 3)
})

test_that("VarN / StdN reject negative eps", {
    expect_error(get_reduction("VarN")(1:3, eps = -1), "eps")
    expect_error(get_reduction("StdN")(1:3, eps = -0.5), "eps")
})

test_that("VarN / StdN attach .dafr_builtin", {
    expect_identical(attr(get_reduction("VarN"), ".dafr_builtin"), "VarN")
    expect_identical(attr(get_reduction("StdN"), ".dafr_builtin"), "StdN")
})
```

- [ ] **Step F.2: Run tests — expect FAIL**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

Expected: 5 new tests FAIL.

- [ ] **Step F.3: Implement `.op_varn` and `.op_stdn`**

Append to `R/operations.R`:

```r
.assert_non_negative_eps <- function(eps) {
    if (!is.numeric(eps) || length(eps) != 1L || is.na(eps) || eps < 0) {
        stop(sprintf("'eps' must be a non-negative number (got %s)",
            as.character(eps)[1L]
        ), call. = FALSE)
    }
}

.op_varn <- function(x, ..., na_rm = FALSE, eps = 0) {
    .assert_non_negative_eps(eps)
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    v <- .var_uncorrected(x, na_rm = FALSE)
    mu <- if (length(x) == 0L) NA_real_ else sum(x) / length(x)
    v / (mu + eps)
}
attr(.op_varn, ".dafr_builtin") <- "VarN"

.op_stdn <- function(x, ..., na_rm = FALSE, eps = 0) {
    .assert_non_negative_eps(eps)
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    v <- .var_uncorrected(x, na_rm = FALSE)
    mu <- if (length(x) == 0L) NA_real_ else sum(x) / length(x)
    sqrt(v) / (mu + eps)
}
attr(.op_stdn, ".dafr_builtin") <- "StdN"
```

Register inside `.register_default_ops()`:

```r
register_reduction("VarN", .op_varn, overwrite = TRUE)
register_reduction("StdN", .op_stdn, overwrite = TRUE)
```

- [ ] **Step F.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

Expected: all reduction tests PASS.

- [ ] **Step F.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-reductions.R
git commit -m "feat(ops): add VarN and StdN reductions (variance/stdev normalized by mean+eps)"
```

---

## Phase G: Median + Quantile (reductions)

**Files:**
- Modify: `R/operations.R`
- Modify: `tests/testthat/test-operations-reductions.R`

**Julia reference:** `operations.jl:1285-1408`. `Median` → `stats::median`. `Quantile` requires `p ∈ [0,1]` (no default) → `stats::quantile(x, p)`. Both return double.

R's `stats::quantile` has `type = 7` default (linear interpolation). Julia's `StatsBase.quantile` also uses linear interpolation by default. Numerical results agree for common cases — check in tests.

- [ ] **Step G.1: Add Median + Quantile tests**

Append to `tests/testthat/test-operations-reductions.R`:

```r
test_that("Median returns the median value", {
    fn <- get_reduction("Median")
    expect_equal(fn(c(1, 2, 3)), 2)
    expect_equal(fn(c(1, 2, 3, 4)), 2.5)
    expect_equal(fn(c(5, 1, 3)), 3)
})

test_that("Median handles NA via na_rm", {
    fn <- get_reduction("Median")
    expect_true(is.na(fn(c(1, NA, 3))))
    expect_equal(fn(c(1, NA, 3), na_rm = TRUE), 2)
})

test_that("Quantile requires p and bounds it to [0,1]", {
    fn <- get_reduction("Quantile")
    expect_error(fn(1:3), "p")
    expect_error(fn(1:3, p = -0.1), "p")
    expect_error(fn(1:3, p = 1.1), "p")
})

test_that("Quantile returns p-th quantile (unnamed)", {
    fn <- get_reduction("Quantile")
    expect_equal(unname(fn(c(1, 2, 3), p = 0)), 1)
    expect_equal(unname(fn(c(1, 2, 3), p = 0.5)), 2)
    expect_equal(unname(fn(c(1, 2, 3), p = 1)), 3)
    # R default type=7: Q(0.25) of 1:5 = 2
    expect_equal(unname(fn(c(1, 2, 3, 4, 5), p = 0.25)), 2)
})

test_that("Median / Quantile attach .dafr_builtin", {
    expect_identical(attr(get_reduction("Median"), ".dafr_builtin"), "Median")
    expect_identical(attr(get_reduction("Quantile"), ".dafr_builtin"), "Quantile")
})
```

- [ ] **Step G.2: Run tests — expect FAIL**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

Expected: 5 new tests FAIL.

- [ ] **Step G.3: Implement `.op_median` and `.op_quantile`**

Append to `R/operations.R`:

```r
.op_median <- function(x, ..., na_rm = FALSE) {
    stats::median(as.numeric(x), na.rm = isTRUE(na_rm))
}
attr(.op_median, ".dafr_builtin") <- "Median"

.op_quantile <- function(x, ..., p, na_rm = FALSE) {
    if (missing(p)) {
        stop("Quantile: 'p' parameter is required (a value in [0, 1])",
            call. = FALSE
        )
    }
    if (!is.numeric(p) || length(p) != 1L || is.na(p) || p < 0 || p > 1) {
        stop(sprintf("Quantile: 'p' must be in [0, 1] (got %s)",
            as.character(p)[1L]
        ), call. = FALSE)
    }
    unname(stats::quantile(as.numeric(x), probs = p, na.rm = isTRUE(na_rm)))
}
attr(.op_quantile, ".dafr_builtin") <- "Quantile"
```

Register inside `.register_default_ops()`:

```r
register_reduction("Median", .op_median, overwrite = TRUE)
register_reduction("Quantile", .op_quantile, overwrite = TRUE)
```

- [ ] **Step G.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

Expected: all reduction tests PASS.

- [ ] **Step G.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-reductions.R
git commit -m "feat(ops): add Median and Quantile reductions"
```

---

## Phase H: GeoMean (reduction)

**Files:**
- Modify: `R/operations.R`
- Modify: `tests/testthat/test-operations-reductions.R`

**Julia reference:** `operations.jl:1498-1573`. Geometric mean with optional `eps` regularization: if `eps == 0`, returns `exp(mean(log(x)))`; else returns `exp(mean(log(x + eps))) - eps`. `eps ≥ 0`.

R: no base `geomean`. Compute via `exp(mean(log(x)))`. Zeros in input produce `-Inf` after log → geomean is 0 (the `exp(-Inf) = 0` path). That matches `StatsBase.geomean` for zero-containing vectors.

- [ ] **Step H.1: Add GeoMean tests**

Append to `tests/testthat/test-operations-reductions.R`:

```r
test_that("GeoMean with eps == 0 equals exp(mean(log(x)))", {
    fn <- get_reduction("GeoMean")
    expect_equal(fn(c(1, 4, 16)), exp(mean(log(c(1, 4, 16)))))
    expect_equal(fn(c(2, 8)), sqrt(16))  # geomean(2, 8) = 4
})

test_that("GeoMean with eps > 0 adds then subtracts the regulariser", {
    fn <- get_reduction("GeoMean")
    # geomean(x + eps) - eps
    x <- c(0, 1, 2)
    eps <- 1
    expect_equal(fn(x, eps = eps), exp(mean(log(x + eps))) - eps)
})

test_that("GeoMean on all-zero vector returns 0 (eps = 0)", {
    expect_equal(get_reduction("GeoMean")(c(0, 0, 0)), 0)
})

test_that("GeoMean rejects negative eps", {
    expect_error(get_reduction("GeoMean")(1:3, eps = -1), "eps")
})

test_that("GeoMean attaches .dafr_builtin", {
    expect_identical(attr(get_reduction("GeoMean"), ".dafr_builtin"), "GeoMean")
})
```

- [ ] **Step H.2: Run tests — expect FAIL**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

Expected: 5 new tests FAIL.

- [ ] **Step H.3: Implement `.op_geomean`**

Append to `R/operations.R`:

```r
.op_geomean <- function(x, ..., eps = 0, na_rm = FALSE) {
    .assert_non_negative_eps(eps)
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    if (eps == 0) {
        exp(mean(log(x)))
    } else {
        exp(mean(log(x + eps))) - eps
    }
}
attr(.op_geomean, ".dafr_builtin") <- "GeoMean"
```

Register inside `.register_default_ops()`:

```r
register_reduction("GeoMean", .op_geomean, overwrite = TRUE)
```

- [ ] **Step H.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

- [ ] **Step H.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-reductions.R
git commit -m "feat(ops): add GeoMean reduction (with eps regularisation)"
```

---

## Phase I: Mode (reduction)

**Files:**
- Modify: `R/operations.R`
- Modify: `tests/testthat/test-operations-reductions.R`

**Julia reference:** `operations.jl:1066-1115`. Returns the most-frequent value; supports strings in Julia. For R Slice 7: **numeric-only**, error on character input (documented). This avoids touching `.apply_reduction_grouped_*` which hard-codes `vapply(..., numeric(1))`. Deferred.

Tie-breaking: R's `which.max(tabulate(...))` returns the first maximum, matching Julia's `StatsBase.mode`.

- [ ] **Step I.1: Add Mode tests**

Append to `tests/testthat/test-operations-reductions.R`:

```r
test_that("Mode returns the most frequent numeric value", {
    fn <- get_reduction("Mode")
    expect_equal(fn(c(1, 2, 2, 3)), 2)
    expect_equal(fn(c(1, 1, 2, 2, 3)), 1)  # tie -> first maximum
})

test_that("Mode on a single-element vector returns that element", {
    expect_equal(get_reduction("Mode")(7), 7)
})

test_that("Mode raises on character input (numeric-only this slice)", {
    expect_error(get_reduction("Mode")(c("a", "b", "a")), "Mode.*numeric")
})

test_that("Mode attaches .dafr_builtin", {
    expect_identical(attr(get_reduction("Mode"), ".dafr_builtin"), "Mode")
})
```

- [ ] **Step I.2: Run tests — expect FAIL**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

Expected: 4 new tests FAIL.

- [ ] **Step I.3: Implement `.op_mode`**

Append to `R/operations.R`:

```r
.op_mode <- function(x, ...) {
    if (!is.numeric(x) && !is.logical(x)) {
        stop("Mode: only numeric and logical input are supported this slice; got ",
            sQuote(typeof(x)), call. = FALSE
        )
    }
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
attr(.op_mode, ".dafr_builtin") <- "Mode"
```

Register inside `.register_default_ops()`:

```r
register_reduction("Mode", .op_mode, overwrite = TRUE)
```

- [ ] **Step I.4: Run tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-reductions.R")'
```

- [ ] **Step I.5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-reductions.R
git commit -m "feat(ops): add Mode reduction (numeric-only; string-support deferred)"
```

---

## Phase J: Registration consolidation + end-to-end query tests + NAMESPACE

**Files:**
- Modify: `R/operations.R` (tidy `.register_default_ops` into the block structure of the existing file)
- Create: `tests/testthat/test-operations-query.R` (end-to-end query tests through the evaluator)
- Modify: `tests/testthat/test-operations-registry.R` (update the default-registered lists)
- Modify: `NEWS.md` (add Slice 7 entry)
- Regenerate: `NAMESPACE` and `man/*.Rd` — no new exports, just doc regen if needed

- [ ] **Step J.1: Consolidate `.register_default_ops()` into a single block**

Restructure the bottom of `R/operations.R` so all 22 registrations (10 existing + 12 new) appear together. The final `.register_default_ops()` should read:

```r
.register_default_ops <- function() {
    register_reduction("Sum", .op_sum, overwrite = TRUE)
    register_reduction("Mean", .op_mean, overwrite = TRUE)
    register_reduction("Max", .op_max, overwrite = TRUE)
    register_reduction("Min", .op_min, overwrite = TRUE)
    register_reduction("Count", .op_count, overwrite = TRUE)
    register_reduction("Var", .op_var, overwrite = TRUE)
    register_reduction("Std", .op_std, overwrite = TRUE)
    register_reduction("VarN", .op_varn, overwrite = TRUE)
    register_reduction("StdN", .op_stdn, overwrite = TRUE)
    register_reduction("Median", .op_median, overwrite = TRUE)
    register_reduction("Quantile", .op_quantile, overwrite = TRUE)
    register_reduction("GeoMean", .op_geomean, overwrite = TRUE)
    register_reduction("Mode", .op_mode, overwrite = TRUE)

    register_eltwise("Log", .op_log, overwrite = TRUE)
    register_eltwise("Abs", .op_abs, overwrite = TRUE)
    register_eltwise("Exp", .op_exp, overwrite = TRUE)
    register_eltwise("Sqrt", .op_sqrt, overwrite = TRUE)
    register_eltwise("Round", .op_round, overwrite = TRUE)
    register_eltwise("Clamp", .op_clamp, overwrite = TRUE)
    register_eltwise("Convert", .op_convert, overwrite = TRUE)
    register_eltwise("Fraction", .op_fraction, overwrite = TRUE)
    register_eltwise("Significant", .op_significant, overwrite = TRUE)

    invisible(NULL)
}
```

Remove any temporary per-phase registrations inside `.register_default_ops`. Ensure every `.op_*` definition is above `.register_default_ops`. Run `testthat::test_dir(...)` after reshaping to confirm green.

- [ ] **Step J.2: Update `test-operations-registry.R` defaults list**

Edit the existing default-registration tests at lines 70-74 and 96-99:

```r
test_that("default reductions are registered on load", {
    for (op in c("Sum", "Mean", "Max", "Min", "Count",
                 "Var", "Std", "VarN", "StdN",
                 "Median", "Quantile", "GeoMean", "Mode")) {
        expect_true(op %in% registered_reductions(), info = op)
    }
})

test_that("default eltwise ops are registered on load", {
    for (op in c("Log", "Abs", "Exp", "Sqrt", "Round",
                 "Clamp", "Convert", "Fraction", "Significant")) {
        expect_true(op %in% registered_eltwise(), info = op)
    }
})
```

- [ ] **Step J.3: Create end-to-end query test file**

File: `tests/testthat/test-operations-query.R`.

**Query syntax (confirmed against existing tests):**
- `@ cell : name` — single-axis vector
- `@ cell @ gene :: name` — matrix
- `% OpName param: value` — eltwise (params space-separated, no parens in query)
- `>| OpName` — ReduceToColumn (collapse columns, per-row result)
- `>- OpName` — ReduceToRow (collapse rows, per-column result)

The test daf has a stored vector `values = c(1,2,3,4)` on `cell` (used for eltwise testing) and a 4×2 matrix `UMIs` with the layout `matrix(c(1,2,3,4, 5,6,7,8), nrow=4)` (used for reductions). With this layout, row `i` holds values `c(i, i+4)` — mean `i+2`, uncorrected variance `4` for every row.

```r
# End-to-end: verify every new Slice-7 op is reachable from a query string.
# Uses a small in-memory daf: a stored vector for eltwise, a matrix for
# reductions. Tests round-trip through the parser, evaluator, and op dispatch.

.slice7_query_daf <- function() {
    d <- memory_daf(name = "s7")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "values", c(1, 2, 3, 4))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4, ncol = 2,
            dimnames = list(c("A", "B", "C", "D"), c("g1", "g2"))))
    d
}

test_that("% Clamp via query clamps a vector", {
    d <- .slice7_query_daf()
    out <- get_query(d, "@ cell : values % Clamp min: 2 max: 3")
    expect_equal(unname(out), c(2, 2, 3, 3))
})

test_that("% Convert via query converts to integer", {
    d <- .slice7_query_daf()
    out <- get_query(d, "@ cell : values % Convert type: integer")
    expect_type(out, "integer")
})

test_that("% Fraction via query normalises a vector to sum 1", {
    d <- .slice7_query_daf()
    out <- get_query(d, "@ cell : values % Fraction")
    expect_equal(sum(out), 1)
    expect_equal(unname(out), c(1, 2, 3, 4) / 10)
})

test_that("% Significant via query zeroes below-threshold values", {
    d <- .slice7_query_daf()
    out <- get_query(d, "@ cell : values % Significant high: 3")
    # c(1,2,3,4): max |x| = 4 >= 3 -> keep >= low=3 -> c(0, 0, 3, 4)
    expect_equal(unname(out), c(0, 0, 3, 4))
})

test_that(">| Var and >| Std reduce a matrix column-wise to per-row vector", {
    d <- .slice7_query_daf()
    # Row i has values c(i, i+4). Var = 4 for every row (uncorrected).
    v_var <- get_query(d, "@ cell @ gene :: UMIs >| Var")
    expect_equal(unname(v_var), c(4, 4, 4, 4))
    v_std <- get_query(d, "@ cell @ gene :: UMIs >| Std")
    expect_equal(unname(v_std), c(2, 2, 2, 2))
})

test_that(">| VarN / StdN divide by row mean", {
    d <- .slice7_query_daf()
    # Row i: var = 4, mean = i+2. VarN = 4/(i+2); StdN = 2/(i+2).
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| VarN")),
        4 / (seq_len(4) + 2)
    )
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| StdN")),
        2 / (seq_len(4) + 2)
    )
})

test_that(">| Median / Quantile / GeoMean / Mode reach through query", {
    d <- .slice7_query_daf()
    # Row i has c(i, i+4); median = i+2.
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| Median")),
        c(3, 4, 5, 6)
    )
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| Quantile p: 0.5")),
        c(3, 4, 5, 6)
    )
    # GeoMean of (1,5) = sqrt(5); (2,6) = sqrt(12); (3,7) = sqrt(21); (4,8) = sqrt(32)
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| GeoMean")),
        sqrt(c(5, 12, 21, 32))
    )
    # Mode of each row (all unique) -> first element: 1,2,3,4
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| Mode")),
        c(1, 2, 3, 4)
    )
})
```

- [ ] **Step J.4: Run all operations tests — expect PASS**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "operations")'
```

Expected: all operations-* tests PASS (registry + eltwise + reductions + query).

- [ ] **Step J.5: Regenerate docs if any new `@export` tags were added**

`register_eltwise` / `register_reduction` are already exported. The new `.op_*` helpers are internal — no export changes expected. Still:

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::document()'
git diff --stat NAMESPACE man/   # expect no changes or only whitespace
```

- [ ] **Step J.6: Update `NEWS.md`**

Prepend to the `## New features` section of the `dafr 0.6.0 (in development)` block:

```markdown
- **Query op surface expansion** — 12 new default ops registered at package
  load: eltwise `Clamp`, `Convert`, `Fraction`, `Significant`; reductions
  `Var`, `Std`, `VarN`, `StdN`, `Median`, `Quantile`, `GeoMean`, `Mode`.
  All available from query strings (e.g. `% Clamp min: 0 max: 10`,
  `%> Quantile p: 0.9`). Reductions use uncorrected (n-denom) variance to
  match DAF.jl. `Mode` is numeric-only this slice; string-axis grouping
  deferred. No new exports.
```

- [ ] **Step J.7: Run full test suite + check**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
# expect 1315 + (~80 new) PASS / 0 FAIL / 0 SKIP / 1 WARN
_R_CHECK_SYSTEM_CLOCK_=0 Rscript -e 'devtools::check(error_on = "note")'
# expect 0 ERROR / 0 WARNING / 0 NOTE
```

- [ ] **Step J.8: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/operations.R tests/testthat/test-operations-registry.R \
    tests/testthat/test-operations-query.R NEWS.md NAMESPACE man/
git commit -m "feat(ops): register 12 Slice-7 ops + end-to-end query tests + NEWS"
```

---

## Phase K: Julia fixture extension

**Files:**
- Create: `dev/scripts/extend-julia-queries-fixture-slice7.jl`
- Modify: `tests/testthat/fixtures/julia-queries/` (new `.rds` or `.json` cases — match the format already used)
- Modify: `tests/testthat/test-julia-queries-compat.R` (or the existing compat test file)

**Goal:** Emit one deterministic query result per new op against a small fixed DAF and capture DAF.jl's output. R-side compat test replays each case via `get_query()` and expects byte-parity.

- [ ] **Step K.1: Inspect existing fixture format**

```bash
cd /home/aviezerl/src/dafr-native
ls tests/testthat/fixtures/julia-queries/
head -20 dev/scripts/regen-julia-queries-fixture.jl  # if exists; otherwise find it
```

Identify: (a) the fixture file format (rds / json / csv), (b) the daf construction block, (c) how the R-side compat test reads the fixture. Slice-3 precedent: `dev/scripts/regen-julia-queries-fixture.jl` + fixtures under `tests/testthat/fixtures/julia-queries/`.

- [ ] **Step K.2: Check DAF.jl head hasn't moved**

```bash
cd ~/src/DataAxesFormats.jl
git fetch origin
git log --oneline -1    # expect 49fbba140437387a378217c2fa658d4231d0c8c1 or newer compatible HEAD
git status -s           # expect clean
```

If HEAD has moved: evaluate whether the move touched `src/operations.jl`. If not, proceed on the newer HEAD and record it. If yes: pin to `49fbba1` for this fixture extension via `git -C ~/src/DataAxesFormats.jl checkout 49fbba1` (then restore after).

- [ ] **Step K.3: Write the extension script**

File: `dev/scripts/extend-julia-queries-fixture-slice7.jl`

Copy the boilerplate from `dev/scripts/regen-julia-queries-fixture.jl`, but emit exactly 12 new fixture cases — one per new op. Use the same 4×2 UMIs fixture the R-side `test-operations-query.R` uses (to make the `R ↔ Julia` byte-parity check tight). For each op, record:

- `query`: the query string (matching Julia's query-string syntax)
- `expected`: the Julia-side numeric result

Example block for Clamp:

```julia
push!(cases, Dict(
    "name"     => "clamp_min2_max3",
    "query"    => "/cell/gene & = g1 : UMIs % Clamp min 2 max 3",
    "expected" => get_query(daf, "/cell/gene & = g1 : UMIs % Clamp min 2 max 3")
))
```

Handle each of the 12 ops. Query-string syntax differences between DAF.jl and dafr-native parameter passing: Julia uses bare names, R uses `name: value`. If this is not already handled in `test-julia-queries-compat.R`, check the Slice-3 fixture regen script for the precedent — it normalised the query string at fixture-read time, not at fixture-write time.

- [ ] **Step K.4: Run the Julia script inside `dafr-mcview` env**

```bash
conda activate dafr-mcview
cd /home/aviezerl/src/dafr-native/dev
julia --project=~/src/DataAxesFormats.jl scripts/extend-julia-queries-fixture-slice7.jl
# expect new fixture files or appended entries to the existing fixture
```

Verify the fixture grew by the expected 12 entries.

- [ ] **Step K.5: Add R-side compat cases**

Modify `tests/testthat/test-julia-queries-compat.R` to replay the 12 new cases against `get_query()`. Reuse the existing `for (case in cases)` loop if one exists. If not, add per-case `test_that(...)` blocks matching the style of the Slice-3 / Slice-4 compat tests.

- [ ] **Step K.6: Run the compat test**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-julia-queries-compat.R")'
```

Expected: all 12 new cases PASS with byte-parity against DAF.jl's output.

- [ ] **Step K.7: Commit fixture + script**

```bash
cd /home/aviezerl/src/dafr-native
git add tests/testthat/fixtures/julia-queries/ tests/testthat/test-julia-queries-compat.R
git commit -m "test: extend julia-queries fixture with Slice-7 ops (byte parity)"

cd /home/aviezerl/src/dafr-native/dev
git add scripts/extend-julia-queries-fixture-slice7.jl
git commit -m "dev: slice-7 julia-queries fixture extension script"
```

---

## Phase Z: Polish + exit + merge

**Files:**
- Create: `dev/notes/slice-7-exit.md`
- Create: `dev/notes/slice-8-kickoff.md` (breadcrumb for the next slice)

- [ ] **Step Z.1: Run full suite + check one more time**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
_R_CHECK_SYSTEM_CLOCK_=0 Rscript -e 'devtools::check(error_on = "note")'
```

Expected: 0 ERROR / 0 WARNING / 0 NOTE; tests PASS with the pre-existing 1 WARN (scran/irlba SVD).

- [ ] **Step Z.2: Write `slice-7-exit.md`**

Content skeleton (fill specifics after the other phases complete):

```markdown
# Slice 7 — Exit

**Date:** <actual-date>.
**Tag:** `slice-7`, applied to the final merge commit on `main`.
**Predecessor:** tag `slice-6` at commit `e38c53a`.

## Delivered
- 12 new default query ops: <list>.
- All R-only implementations; sparse-aware where the kernel admits it
  (Clamp with straddling range, Fraction, Significant).
- End-to-end query tests at `tests/testthat/test-operations-query.R`.
- Julia-queries fixture extended by 12 cases; byte-parity against DAF.jl at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (or newer, if the head moved
  mid-slice without touching `operations.jl`).

## Intentionally deferred
- **Fast paths for Var / Std / Median / Quantile** via `matrixStats::rowVars`
  etc. Profile first; add if matrix-heavy queries are slow in practice.
- **Mode on character input.** Requires refactoring `.apply_reduction_grouped_*`
  off the `vapply(..., numeric(1))` contract. Not touched this slice.
- **`type` parameter on ops.** R's output-type policy is "always double except
  Convert, Count, Mode". Julia's per-op `type` is not ported.
- **Sparse→integer fast path for Convert.** Inherits the `.cast_matrix_type`
  dense-coercion mine (Slice 6). Not triggered by any test.

## Mines laid for Slice 8
- `.apply_reduction_grouped_*` vapply-numeric-only assumption is now flagged;
  any future char-valued op (Mode-on-strings, or new string reductions) must
  refactor this.
- `.cast_matrix_type("integer", dgCMatrix)` dense-coercion is now reachable
  via the public `Convert` op for the first time. Still unexercised by tests.

## Test status at exit
- PASS / FAIL / SKIP / WARN numbers from the final run.

## Check status at exit
- `devtools::check(error_on = "note")`: 0/0/0.

## Julia DAF state at exit
- `~/src/DataAxesFormats.jl` HEAD: <sha>. Fixture still pinned at `49fbba1`
  (or updated — note which).

## L2 upstream PR
- Declined permanently per durable user feedback. No ask this slice either.
```

- [ ] **Step Z.3: Write `slice-8-kickoff.md`**

Mirror the structure of `slice-7-kickoff.md`: what changed, current state, remaining design-spec surface, Slice 8 scope candidates. From the design spec's deferred list, the natural Slice-8 options are:

- AnnData interop (bidirectional) — heaviest, user-requested.
- Zarr backend for `open_daf` — multi-day effort.
- `bestify` heuristic for `copy_vector` / `copy_matrix` — small perf slice.
- Matrix-kernel fast paths for Slice-7 ops — small perf slice.
- Long-vector (>2³¹) ALTREP support — blocks metacell-scale data at extreme sizes.

Commit to one. Keep the kickoff short.

- [ ] **Step Z.4: Commit exit + next-slice breadcrumb to dev repo**

```bash
cd /home/aviezerl/src/dafr-native/dev
git add notes/slice-7-exit.md notes/slice-8-kickoff.md
git commit -m "docs: slice-7 exit + slice-8 kickoff breadcrumb"
```

- [ ] **Step Z.5: Merge feature branch into main (fast-forward)**

```bash
cd /home/aviezerl/src/dafr-native
git checkout main
git merge --ff-only slice-7-ops-expansion
git tag slice-7
git log --oneline -1   # confirm HEAD is the Phase J.8 / Phase K.7 merged tip
```

If not fast-forward (shouldn't happen on a single-developer branch, but check): investigate before resolving — do not force.

- [ ] **Step Z.6: Push main + tag + dev main**

```bash
cd /home/aviezerl/src/dafr-native
git push origin main
git push origin slice-7
cd /home/aviezerl/src/dafr-native/dev
git push origin main
# (only if user has a remote for the dev repo; otherwise skip this line)
```

Confirm with the user before pushing the package repo — per global safety protocol, push is shared-state and requires authorization scope.

- [ ] **Step Z.7: Delete the feature branch (local + remote)**

```bash
cd /home/aviezerl/src/dafr-native
git branch -d slice-7-ops-expansion
# remote-tracking branch auto-cleaned by fetch --prune; do NOT force-delete remote
```

---

## Self-review checklist (run after writing the plan — not delegated)

- **Spec coverage:** every op in the "Slice 7 scope" section of the kickoff has a phase (A-I): Clamp, Convert, Fraction, Significant, Var, Std, VarN, StdN, Median, Quantile, GeoMean, Mode. The kickoff mentioned `Type` and `All`/`Any` — these do not exist in Julia DAF's `operations.jl` (confirmed Phase 1 of plan research). `Convert` subsumes `Type`. `All`/`Any` omitted; called out in the exit note if requested.
- **No placeholders:** every step has concrete code or a concrete command. No "add validation as appropriate" / "test the above" / "similar to Task N".
- **Type consistency:** `na_rm` spelling consistent across all reduction ops (`na_rm`, not `na.rm` — mirrors the existing Sum/Mean default). `eps` consistent (non-negative scalar). `p` for Quantile, `high`/`low` for Significant, `min`/`max` for Clamp, `type` for Convert. No cross-task renames.
- **File paths:** all absolute where required; relative paths used only inside commit commands run from the repo root.
