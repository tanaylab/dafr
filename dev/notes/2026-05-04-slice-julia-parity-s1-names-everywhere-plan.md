# S1 — Names everywhere on `format_get_*` — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Lift the format-API contract so every `format_get_vector` returns a named atomic vector (names = axis entries) and every `format_get_matrix` returns a dense matrix or `dgCMatrix`/`lgCMatrix` whose dimnames = `(rows-axis entries, cols-axis entries)`. Once enforced at the format layer, retire the redundant name-reattachment workarounds in `readers.R` and `query_eval.R`.

**Architecture:** Two tiny helpers in `R/utils.R` (`.attach_vector_axis_names`, `.attach_matrix_axis_dimnames`) wrap each backend's existing `format_get_*` payload before return. Storage stays canonical (writes still strip names via `.validate_*_value`); reads always project axis entries onto the value. Chain/Contract/View wrappers inherit the contract for free because they delegate to a leaf method (subsetting in ViewDaf preserves names through standard `[`/`drop=FALSE` rules).

**Tech Stack:** R 4.x, S7 generics, Matrix package (`dgCMatrix` / `lgCMatrix`), testthat, the dafr internal helpers under `R/`.

**Sub-slices NOT covered here:** S2 (port remaining Julia test files) and S3 (literal `data.jl` port) are independently shippable and get their own plans after S1 lands.

---

## File Structure

**Create:**
- `tests/testthat/test-format-api-named-returns.R` — backend-by-backend contract tests for the new named-return invariant.

**Modify:**
- `R/utils.R` — add `.attach_vector_axis_names()` and `.attach_matrix_axis_dimnames()` helpers.
- `R/memory_daf.R:241-253` — wire vector helper into `format_get_vector` for `MemoryDaf`.
- `R/memory_daf.R:344-359` — wire matrix helper into `format_get_matrix` for `MemoryDaf`.
- `R/files_daf_read.R:330-354` — wire vector helper into the cached path used by both `FilesDaf` and `FilesDafReadOnly`.
- `R/files_daf_read.R:548-572` — wire matrix helper into the cached matrix path.
- `R/readers.R:270-276` — drop the now-redundant `names(out) <- entries` reattachment in `get_vector`.
- `R/readers.R:368-389` — drop the now-redundant dimname reassignment in `get_matrix` (keep the transpose for the flipped-layout case; the helper applies dimnames in axis order, then transpose handles it).
- `R/query_eval.R:489-555` — simplify `.apply_chained_lookup_vector` (the "First-hop pivot_values comes from format_get_vector (unnamed)" branch becomes dead code).
- Existing tests that assert unnamed returns (`expect_equal(..., c(1, 2, 3))`-style without names): update during Task 8 sweep.

**Out of scope (per the kickoff):**
- `format_set_*` strip behaviour stays — storage stays canonical.
- `as_anndata`/`as_h5ad` round-trip is exercised in the integration test (Task 9) but no h5ad code changes if names already survive.
- Cosmetic alignment with Julia error messages.

---

## Task 1: Failing contract tests for MemoryDaf

**Files:**
- Create: `tests/testthat/test-format-api-named-returns.R`

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-format-api-named-returns.R`:

```r
# Format-API named-return contract: every backend's format_get_vector
# returns a named atomic vector (names = axis entries in axis order),
# and every format_get_matrix returns a matrix / dgCMatrix / lgCMatrix
# whose dimnames are list(rows-axis entries, cols-axis entries).

.fixture_named_memory_daf <- function() {
    d <- memory_daf(name = "names-fixture")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("gA", "gB"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
    set_matrix(d, "cell", "gene", "expr",
        matrix(c(1.0, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    )
    set_matrix(d, "cell", "gene", "expr_sparse",
        Matrix::sparseMatrix(
            i = c(1L, 3L), j = c(1L, 2L), x = c(7, 9),
            dims = c(3L, 2L), repr = "C"
        )
    )
    d
}

test_that("MemoryDaf format_get_vector returns named atomic", {
    d <- .fixture_named_memory_daf()
    v <- format_get_vector(d, "cell", "donor")
    expect_equal(names(v), c("c1", "c2", "c3"))
    expect_equal(unname(v), c("d1", "d2", "d1"))
})

test_that("MemoryDaf format_get_matrix returns dense with axis dimnames", {
    d <- .fixture_named_memory_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr")
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("MemoryDaf format_get_matrix returns sparse with @Dimnames", {
    d <- .fixture_named_memory_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")
    expect_s4_class(m, "dgCMatrix")
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run from repo root:

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-format-api-named-returns.R")'
```

Expected: 3 FAIL — `names(v)` is `NULL`, `rownames(m)` / `colnames(m)` are `NULL`, sparse `@Dimnames` is `list(NULL, NULL)`.

- [ ] **Step 3: Commit the failing tests**

```bash
git add tests/testthat/test-format-api-named-returns.R
git commit -m "test(parity): failing contract tests for named format_get_* (memory)"
```

---

## Task 2: Add the two helpers in `R/utils.R`

**Files:**
- Modify: `R/utils.R` (append at the bottom of the file, after `.validate_vector_value`)

- [ ] **Step 1: Add helper code**

Append to `R/utils.R`:

```r
# Attach axis-entry names to a vector returned by format_get_vector.
# Internal — every format_get_vector method must call this on the value
# it returns so the format-API contract ("returns are named") holds at
# every layer (memory, files, chain, view, contract, http).
#
# The helper is length-strict: a backend that returns a value of the
# wrong length is buggy regardless of names, and we'd rather surface
# that immediately than silently mismatch names to data.
.attach_vector_axis_names <- function(daf, axis, vec) {
    entries <- format_axis_array(daf, axis)
    if (length(vec) != length(entries)) {
        stop(sprintf(
            "format_get_vector contract violation: value has length %d, axis %s has %d entries",
            length(vec), sQuote(axis), length(entries)
        ), call. = FALSE)
    }
    names(vec) <- entries
    vec
}

# Attach axis-entry dimnames to a matrix returned by format_get_matrix.
# Handles both base R dense matrices and Matrix::dgCMatrix /
# Matrix::lgCMatrix (which carry dimnames on the @Dimnames slot).
.attach_matrix_axis_dimnames <- function(daf, rows_axis, columns_axis, mat) {
    rows <- format_axis_array(daf, rows_axis)
    cols <- format_axis_array(daf, columns_axis)
    d <- dim(mat)
    if (d[[1L]] != length(rows) || d[[2L]] != length(cols)) {
        stop(sprintf(
            "format_get_matrix contract violation: matrix is %dx%d, axes (%s,%s) are %dx%d",
            d[[1L]], d[[2L]], sQuote(rows_axis), sQuote(columns_axis),
            length(rows), length(cols)
        ), call. = FALSE)
    }
    if (methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")) {
        mat@Dimnames <- list(rows, cols)
    } else {
        dimnames(mat) <- list(rows, cols)
    }
    mat
}
```

- [ ] **Step 2: Reload + sanity check**

```bash
Rscript -e 'devtools::load_all(".", quiet=TRUE); cat(exists(".attach_vector_axis_names", envir=asNamespace("dafr")), "\n", exists(".attach_matrix_axis_dimnames", envir=asNamespace("dafr")), "\n")'
```

Expected: `TRUE\nTRUE`.

- [ ] **Step 3: Commit**

```bash
git add R/utils.R
git commit -m "feat(format-api): helpers to attach axis-entry names on get returns"
```

---

## Task 3: Wire MemoryDaf through the helpers

**Files:**
- Modify: `R/memory_daf.R:241-253` and `R/memory_daf.R:344-359`

- [ ] **Step 1: Update format_get_vector for MemoryDaf**

Change `R/memory_daf.R:241-253` from:

```r
S7::method(
    format_get_vector,
    list(MemoryDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    env <- .memory_axis_vectors(daf, axis, create = FALSE)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
        stop(sprintf(
            "vector %s does not exist on axis %s",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
    get(name, envir = env, inherits = FALSE)
}
```

to:

```r
S7::method(
    format_get_vector,
    list(MemoryDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    env <- .memory_axis_vectors(daf, axis, create = FALSE)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
        stop(sprintf(
            "vector %s does not exist on axis %s",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
    .attach_vector_axis_names(daf, axis, get(name, envir = env, inherits = FALSE))
}
```

- [ ] **Step 2: Update format_get_matrix for MemoryDaf**

Change `R/memory_daf.R:344-359` from:

```r
S7::method(
    format_get_matrix,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
        stop(
            sprintf(
                "matrix %s does not exist on axes (%s, %s)",
                sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
            ),
            call. = FALSE
        )
    }
    get(name, envir = env, inherits = FALSE)
}
```

to:

```r
S7::method(
    format_get_matrix,
    list(MemoryDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
        stop(
            sprintf(
                "matrix %s does not exist on axes (%s, %s)",
                sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
            ),
            call. = FALSE
        )
    }
    .attach_matrix_axis_dimnames(
        daf, rows_axis, columns_axis,
        get(name, envir = env, inherits = FALSE)
    )
}
```

- [ ] **Step 3: Run the contract test — Memory rows must pass**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-format-api-named-returns.R")'
```

Expected: the 3 MemoryDaf tests PASS. (Files / wrapper tests don't exist yet.)

- [ ] **Step 4: Commit**

```bash
git add R/memory_daf.R
git commit -m "feat(format-api): MemoryDaf format_get_* return named values"
```

---

## Task 4: Add failing tests for FilesDaf, then wire it through the helpers

**Files:**
- Modify: `tests/testthat/test-format-api-named-returns.R` (extend with files-backed cases)
- Modify: `R/files_daf_read.R:330-354` and `R/files_daf_read.R:548-572`

- [ ] **Step 1: Add failing tests for FilesDaf + FilesDafReadOnly**

Append to `tests/testthat/test-format-api-named-returns.R`:

```r
.fixture_named_files_daf <- function(envir = parent.frame()) {
    src <- .fixture_named_memory_daf()
    root <- tempfile(pattern = "dafr-names-")
    dir.create(root)
    withr::defer(unlink(root, recursive = TRUE), envir = envir)
    dst <- files_daf(root = root, name = "files-fixture", create = TRUE)
    copy_all(src, dst)
    dst
}

test_that("FilesDaf format_get_vector returns named atomic", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    v <- format_get_vector(d, "cell", "donor")
    expect_equal(names(v), c("c1", "c2", "c3"))
    expect_equal(unname(v), c("d1", "d2", "d1"))
})

test_that("FilesDaf format_get_matrix returns dense with axis dimnames", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr")
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("FilesDaf format_get_matrix returns sparse with @Dimnames", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")
    expect_s4_class(m, "dgCMatrix")
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})

test_that("FilesDafReadOnly inherits the named contract", {
    skip_if_not_installed("withr")
    d <- read_only(.fixture_named_files_daf())
    expect_equal(names(format_get_vector(d, "cell", "donor")),
                 c("c1", "c2", "c3"))
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})
```

- [ ] **Step 2: Run — confirm RED for the files cases**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-format-api-named-returns.R")'
```

Expected: the 4 new files tests FAIL (returns lack names); the 3 memory tests still PASS.

- [ ] **Step 3: Wire helpers into the cached files path**

In `R/files_daf_read.R`, change `.files_get_vector_cached` (around line 330) so the helper applies to **both** the cache hit and the freshly-loaded value:

```r
.files_get_vector_cached <- function(daf, axis, name) {
    ce <- S7::prop(daf, "cache")
    key <- cache_key_vector(axis, name)
    stamp <- vector_stamp(daf, axis, name)
    hit <- cache_lookup(ce, "mapped", key, stamp)
    if (!is.null(hit)) {
        return(.attach_vector_axis_names(daf, axis, hit))
    }
    v <- .files_get_vector_impl(daf, axis, name)
    cache_store(ce, "mapped", key, v, stamp, size_bytes = 0)
    .attach_vector_axis_names(daf, axis, v)
}
```

(Cached payloads are stored *without* names so we don't double-allocate string vectors per cache entry; the helper attaches names on each return — equivalent semantics, no extra cache cost.)

Similarly change `.files_get_matrix_cached` (around line 548):

```r
.files_get_matrix_cached <- function(daf, rows_axis, columns_axis, name) {
    ce <- S7::prop(daf, "cache")
    key <- cache_key_matrix(rows_axis, columns_axis, name)
    stamp <- matrix_stamp(daf, rows_axis, columns_axis, name)
    hit <- cache_lookup(ce, "mapped", key, stamp)
    if (!is.null(hit)) {
        return(.attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, hit))
    }
    m <- .files_get_matrix_impl(daf, rows_axis, columns_axis, name)
    cache_store(ce, "mapped", key, m, stamp, size_bytes = 0)
    .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, m)
}
```

- [ ] **Step 4: Run — confirm GREEN for files**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-format-api-named-returns.R")'
```

Expected: all 7 tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/files_daf_read.R tests/testthat/test-format-api-named-returns.R
git commit -m "feat(format-api): FilesDaf format_get_* return named values"
```

---

## Task 5: Wrapper backends inherit the contract — add tests, no code change expected

**Files:**
- Modify: `tests/testthat/test-format-api-named-returns.R` (add wrapper tests)

- [ ] **Step 1: Add wrapper tests**

Append:

```r
test_that("ReadOnlyChainDaf format_get_* delegates with names", {
    a <- .fixture_named_memory_daf()
    b <- memory_daf(name = "overlay")
    add_axis(b, "cell", c("c1", "c2", "c3"))
    add_axis(b, "gene", c("gA", "gB"))
    set_vector(b, "cell", "donor_alt", c("dA", "dB", "dA"))
    ch <- chain_reader(list(a, b))
    expect_equal(names(format_get_vector(ch, "cell", "donor")),
                 c("c1", "c2", "c3"))
    expect_equal(names(format_get_vector(ch, "cell", "donor_alt")),
                 c("c1", "c2", "c3"))
    m <- format_get_matrix(ch, "cell", "gene", "expr")
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("ContractDaf format_get_* delegates with names", {
    base <- .fixture_named_memory_daf()
    ct <- contract_reader(
        base,
        contract = contract(
            axes = list(cell = required(), gene = required()),
            data = list(
                "cell @ donor" = required(eltype = "String"),
                "cell, gene @ expr" = required(eltype = "Float64")
            )
        )
    )
    expect_equal(names(format_get_vector(ct, "cell", "donor")),
                 c("c1", "c2", "c3"))
    m <- format_get_matrix(ct, "cell", "gene", "expr")
    expect_equal(rownames(m), c("c1", "c2", "c3"))
})

test_that("ViewDaf format_get_* preserves names through subsetting", {
    base <- .fixture_named_memory_daf()
    v <- view(base,
        axes = list(cell = "cell = c1, c3", gene = "="),
        data = list("cell @ donor" = "=", "cell, gene @ expr" = "=")
    )
    out_vec <- format_get_vector(v, "cell", "donor")
    expect_equal(names(out_vec), c("c1", "c3"))
    out_mat <- format_get_matrix(v, "cell", "gene", "expr")
    expect_equal(rownames(out_mat), c("c1", "c3"))
    expect_equal(colnames(out_mat), c("gA", "gB"))
})
```

- [ ] **Step 2: Run — they should already pass; if not, fix the wrapper that drops names**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-format-api-named-returns.R")'
```

Expected: PASS — chain delegates verbatim, contract delegates verbatim, view subsets via `raw[idx]` / `raw[r_idx, c_idx, drop = FALSE]` which preserves names from the base.

If anything fails: it means a wrapper layer (likely contract's `.access_*` path or view's slicing of a sparse return) is dropping names — fix the offending site (most likely a missing `drop=FALSE` or a sparse-coerce that resets `@Dimnames`).

If the `view(...)` / `contract_reader(...)` constructor signatures differ from what's written above, adapt to the actual public-API surface — read `R/view_daf.R` and `R/contracts.R` for the shipped constructors. **Do not change the test intent**: the assertion is that names survive through the wrapper.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-format-api-named-returns.R
git commit -m "test(parity): wrapper backends inherit named format_get_* contract"
```

---

## Task 6: Drop the redundant attach in `readers.R::get_vector` / `get_matrix`

**Files:**
- Modify: `R/readers.R:270-276`, `R/readers.R:368-389`

- [ ] **Step 1: Drop the `if (is.null(names(out))) names(out) <- entries` in get_vector**

Change `R/readers.R:270-276` from:

```r
    raw <- format_get_vector(daf, axis, name)
    out <- raw
    if (is.null(names(out))) names(out) <- entries
    cache_store(cache_env, "memory", cache_key, out, stamp_now,
        size_bytes = object.size(out)
    )
    out
```

to:

```r
    out <- format_get_vector(daf, axis, name)
    cache_store(cache_env, "memory", cache_key, out, stamp_now,
        size_bytes = object.size(out)
    )
    out
```

- [ ] **Step 2: Drop the `out@Dimnames <- ...` reassignment in get_matrix**

Change the tail of `R/readers.R::get_matrix` (lines 374-389) from:

```r
    out <- if (flipped) {
        if (methods::is(stored, "dgCMatrix") || methods::is(stored, "lgCMatrix")) {
            Matrix::t(stored)
        } else {
            t(stored)
        }
    } else {
        stored
    }

    if (methods::is(out, "dgCMatrix") || methods::is(out, "lgCMatrix")) {
        out@Dimnames <- list(rows, cols)
    } else {
        dimnames(out) <- list(rows, cols)
    }
    out
}
```

to:

```r
    if (flipped) {
        if (methods::is(stored, "dgCMatrix") || methods::is(stored, "lgCMatrix")) {
            Matrix::t(stored)
        } else {
            t(stored)
        }
    } else {
        stored
    }
}
```

`Matrix::t()` and `t()` both swap dimnames automatically when they swap dims, so the post-transpose value already has `(rows, cols)` dimnames in the right slots.

- [ ] **Step 3: Run the existing readers + format-api suite**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-format-api-named-returns.R"); testthat::test_file("tests/testthat/test-memory-vectors.R"); testthat::test_file("tests/testthat/test-memory-matrices.R"); testthat::test_file("tests/testthat/test-files-vectors.R"); testthat::test_file("tests/testthat/test-files-matrices.R")'
```

Expected: PASS. If any existing test fails because it asserted `expect_equal(get_vector(...), c(1,2,3))` (unnamed), fix it inline — the new contract is named — and note the file in the Task 8 inventory.

- [ ] **Step 4: Commit**

```bash
git add R/readers.R
git commit -m "refactor(readers): drop redundant name reattachment now that format_get_* returns named"
```

---

## Task 7: Simplify `query_eval.R::.apply_chained_lookup_vector`

**Files:**
- Modify: `R/query_eval.R:489-555` (the `.apply_chained_lookup_vector` function — specifically the comment about "First-hop pivot_values comes from format_get_vector (unnamed on all current backends)")

- [ ] **Step 1: Replace the `base_entries` fallback with a strict check**

Find this block (around `R/query_eval.R:522-530`):

```r
    # Post-first-hop, pivot_values carries surviving axis names set by a prior
    # .apply_chained_lookup_vector call; use them to preserve any '??' row drop.
    # First-hop pivot_values comes from format_get_vector (unnamed on all current
    # backends), so we seed from the full axis instead.
    base_entries <- if (!is.null(names(pivot_values))) {
        names(pivot_values)
    } else {
        format_axis_array(daf, base_axis)
    }
```

Replace with:

```r
    # pivot_values now always carries names: first hop comes from
    # format_get_vector (named since the S1 names-everywhere change),
    # subsequent hops from prior .apply_chained_lookup_vector calls.
    base_entries <- names(pivot_values)
    if (is.null(base_entries) || length(base_entries) != length(pivot_values)) {
        stop("internal: pivot vector lost its names — format_get_vector contract violation",
             call. = FALSE)
    }
```

- [ ] **Step 2: Run the chain-lookup query tests**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-query-eval-lookups.R"); testthat::test_file("tests/testthat/test-query-eval-chains.R")'
```

Expected: PASS.

- [ ] **Step 3: Run the full query suite to make sure nothing else regressed**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); for (f in list.files("tests/testthat", pattern="^test-query.*\\.R$", full.names=TRUE)) testthat::test_file(f)'
```

Expected: PASS across all `test-query*.R`.

- [ ] **Step 4: Commit**

```bash
git add R/query_eval.R
git commit -m "refactor(query): assume named pivot vectors from named format_get_vector"
```

---

## Task 8: Inventory + fix tests that asserted unnamed returns

**Files:**
- Modify: zero-or-more `tests/testthat/test-*.R` files surfaced by the run

- [ ] **Step 1: Run the full suite to surface regressions**

```bash
NOT_CRAN=true Rscript tests/testthat.R 2>&1 | tee /tmp/dafr-s1-fullrun.log
```

(The lab convention; see memory `project_test_invocation`. `test-helpers.R:26` is a known pre-existing FAIL under Rscript and does not count.)

- [ ] **Step 2: Inventory failures**

```bash
grep -nE "FAIL|Error|expected|actual" /tmp/dafr-s1-fullrun.log | head -200
```

Each new failure should be one of two flavours:

1. **Named-vs-unnamed mismatch.** Update the assertion to the named form. Pattern:
   ```r
   # Before
   expect_equal(get_vector(d, "cell", "donor"), c("d1","d2","d1"))
   # After
   expect_equal(get_vector(d, "cell", "donor"),
                c(c1="d1", c2="d2", c3="d1"))
   ```

2. **A consumer that previously dropped names is now leaking them downstream where the test didn't expect them.** Example: a reduction that should return an unnamed scalar but now returns named because of a name-preserving op. In that case, the *fix* is in the consumer (e.g. `unname()` at the consumer's boundary), not the test. Don't paper over a real regression with a test edit.

- [ ] **Step 3: Fix each surfaced test/site, one commit per logical group**

```bash
git add tests/testthat/test-<file>.R   # per group
git commit -m "test: align <area> assertions with named format_get_* contract"
```

- [ ] **Step 4: Rerun full suite — confirm only the known pre-existing skip/fail remains**

```bash
NOT_CRAN=true Rscript tests/testthat.R 2>&1 | tail -40
```

Expected: 0 new failures; only the documented `test-helpers.R:26` Rscript-vs-R-CMD-check skip artifact (per memory).

---

## Task 9: Round-trip integration test

**Files:**
- Modify: `tests/testthat/test-format-api-named-returns.R` (add the round-trip)

- [ ] **Step 1: Add the round-trip test**

Append:

```r
test_that("names survive memory -> files -> read_only -> get_query roundtrip", {
    skip_if_not_installed("withr")
    src <- .fixture_named_memory_daf()
    root <- withr::local_tempfile(pattern = "dafr-rt-")
    dir.create(root)
    files <- files_daf(root = root, name = "rt-files", create = TRUE)
    copy_all(src, files)
    files_ro <- read_only(files)

    # get_vector / get_matrix / get_axis on the public API
    v <- get_vector(files_ro, "cell", "donor")
    expect_equal(names(v), c("c1", "c2", "c3"))
    m <- get_matrix(files_ro, "cell", "gene", "expr")
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
    expect_equal(get_axis(files_ro, "cell"), c("c1", "c2", "c3"))

    # get_query: matrix lookup
    qm <- get_query(files_ro, "/ cell / gene : expr")
    expect_equal(rownames(qm), c("c1", "c2", "c3"))
    expect_equal(colnames(qm), c("gA", "gB"))

    # get_query: vector lookup
    qv <- get_query(files_ro, "/ cell : donor")
    expect_equal(names(qv), c("c1", "c2", "c3"))
})

test_that("names survive memory -> as_anndata roundtrip", {
    skip_if_not_installed("anndata")
    skip_if_not_installed("withr")
    src <- .fixture_named_memory_daf()
    ad <- as_anndata(src,
        obs_axis = "cell", var_axis = "gene",
        x = "expr"
    )
    # obs row names are cell entries; var row names are gene entries.
    expect_equal(rownames(ad$obs), c("c1", "c2", "c3"))
    expect_equal(rownames(ad$var), c("gA", "gB"))
    # X matrix carries dimnames
    expect_equal(rownames(ad$X), c("c1", "c2", "c3"))
    expect_equal(colnames(ad$X), c("gA", "gB"))
})
```

If `as_anndata` / `as_h5ad` are not available in the dev environment (no `anndata` package), the second block self-skips. Don't try to install anndata as part of the slice.

If the `as_anndata` constructor signature differs, read `R/anndata_facade.R` for the shipped public API and adapt — keep the assertion intent.

- [ ] **Step 2: Run the round-trip**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-format-api-named-returns.R")'
```

Expected: all PASS (the anndata block may skip; that's fine).

- [ ] **Step 3: Final full-suite confirmation**

```bash
NOT_CRAN=true Rscript tests/testthat.R 2>&1 | tail -10
```

Expected: 0 new failures vs Task 8 baseline; only the documented Rscript-only skip artifact.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-format-api-named-returns.R
git commit -m "test(parity): names survive memory -> files -> get_query (and anndata when available)"
```

---

## Task 10: Update NEWS + memory

**Files:**
- Modify: `NEWS.md` (add a 0.4.0-dev / unreleased entry)
- Optional: refresh memory entries that talk about format-API contract drift

- [ ] **Step 1: Read current NEWS.md head**

```bash
head -30 NEWS.md
```

- [ ] **Step 2: Add S1 entry under unreleased**

Prepend an entry at the top of the unreleased section:

```markdown
## dafr 0.4.0 (in development)

### S1 — Names everywhere on `format_get_*`

- `format_get_vector(daf, axis, name)` now always returns a named atomic vector with `names = format_axis_array(daf, axis)`.
- `format_get_matrix(daf, rows_axis, columns_axis, name)` now always returns a value (dense matrix or `dgCMatrix`/`lgCMatrix`) with `dimnames = list(rows-axis entries, cols-axis entries)`.
- The contract is enforced for every backend: `MemoryDaf`, `FilesDaf`, `FilesDafReadOnly`, and propagates automatically through `ReadOnlyChainDaf` / `WriteChainDaf`, `ContractDaf`, and `ViewDaf` wrappers.
- Internal cleanup: `get_vector` / `get_matrix` no longer reattach names defensively (previously needed because backends returned unnamed); `query_eval.R::.apply_chained_lookup_vector` now asserts the named contract instead of working around it.
- Storage stays canonical: `format_set_*` continues to strip names so the on-disk / in-memory representation only carries axis entries on the axis itself, not redundantly on every value.
```

- [ ] **Step 3: Commit**

```bash
git add NEWS.md
git commit -m "news: S1 — names everywhere on format_get_* (unreleased)"
```

- [ ] **Step 4 (optional): Refresh `feedback_format_api_named.md` memory**

If the memory at `memory/feedback_format_api_named.md` still reads as a forward-looking complaint, edit it to past tense ("the contract was lifted in S1; format_get_* always returns named"). This stops future-you misreading it as an open issue.

---

## Task 11: Push branch + open PR (optional — depends on user policy)

The user typically reviews dev-branch work before push. Do NOT push or open a PR without explicit user approval. When asked:

```bash
git push -u private dev   # or whatever the working branch is
```

then `gh pr create` per the project conventions.

---

## Validation summary (read these before claiming "done")

- [ ] `tests/testthat/test-format-api-named-returns.R` exists and asserts named returns for **memory, files, files-readonly, chain, contract, view** for both vector and matrix.
- [ ] `R/utils.R` defines `.attach_vector_axis_names` and `.attach_matrix_axis_dimnames`, both length-strict.
- [ ] `R/memory_daf.R` and `R/files_daf_read.R` `format_get_*` methods route through the helpers.
- [ ] `R/readers.R::get_vector` and `get_matrix` no longer carry the defensive `names(out) <- entries` / `dimnames(out) <- ...` reattachment.
- [ ] `R/query_eval.R::.apply_chained_lookup_vector` no longer treats first-hop `pivot_values` as potentially-unnamed.
- [ ] Full `Rscript tests/testthat.R` run with `NOT_CRAN=true`: 0 new failures vs the pre-S1 baseline (the known `test-helpers.R:26` Rscript-only artifact does not count — see memory).
- [ ] Round-trip `memory_daf → files_daf → read_only → get_query → get_vector / get_matrix / get_axis` returns named values at every hop.
- [ ] NEWS.md updated.

---

## Risk register (referencing the kickoff)

- **bit64 integer64 vectors.** `bit64::as.integer64` strips names on some operations. The helper attaches names *after* dispatch, so a backend that returns an integer64 still gets names on its way out. If an internal consumer of `format_get_vector` passes the value through a bit64 op, the value may lose names downstream — that's a consumer bug, not a contract bug. Add a targeted test in Task 8 if the inventory pass surfaces a bit64-related failure.
- **Matrix transpose dimnames.** `Matrix::t()` swaps `@Dimnames`; `t()` swaps `dimnames`. Both are correct. If a backend stores under flipped layout, `get_matrix`'s transpose path (now relying on the helper having set dimnames in stored layout *before* the transpose) yields the right thing.
- **Cache-stored values without names.** Per Task 4, the helpers run on each return regardless of cache hit. We *intentionally* do not store names in the cache (saves a per-cache string-vector copy). If a future change starts storing the named value in the cache, the helper is still idempotent.
- **Existing test surface area.** Estimated 20-50 sites (per the kickoff). Task 8 absorbs the inventory pass.
