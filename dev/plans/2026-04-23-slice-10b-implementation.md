# Slice 10b — AnnData Implementation Plan

> **For agentic workers:** Use superpowers:subagent-driven-development. Steps use `- [ ]` checkbox syntax.

**Goal:** Ship `DafAnnData` R6 facade + `as_anndata` + `h5ad_as_daf` + `daf_as_h5ad` — 4 exports for AnnData interop.

**Architecture:** 2 new R files (`R/anndata_facade.R` for the R6 class + `as_anndata`; `R/anndata_format.R` for h5ad I/O). `R6` added to Imports; `hdf5r` added to Suggests. 1 fixture file. 3 new test files.

**Tech Stack:** R 4.4+, `R6` (new Imports), `hdf5r` (new Suggests), `Matrix` (existing). No C++.

**Spec:** `dev/notes/2026-04-23-slice-10b-design.md`.

**Wrapper reference:** `~/src/dafr/R/anndata_facade.R` + `~/src/dafr/R/anndata_format.R`.

**Repo layout:** Package repo at `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/`. Feature branch `slice-10b`. Merge at Phase Z.

**Dev loop per task:**
```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
```

---

## Phase 0: Branch setup

- [ ] Create branch: `git checkout main && git checkout -b slice-10b`.
- [ ] Baseline-green test run: should show 2536 PASS (slice-10a exit state).

---

## Phase A: DafAnnData facade + as_anndata

**Files:**
- Create: `R/anndata_facade.R` (R6 class + `as_anndata` + helper internals).
- Modify: `DESCRIPTION` (+`R6` to Imports).
- Create: `tests/testthat/test-anndata-facade.R`.

### Step A.1: DESCRIPTION — add R6 to Imports

Current Imports (after 10a):
```
Imports:
    S7,
    Matrix,
    cli,
    bit64,
    jsonlite,
    matrixStats,
    methods,
    rlang
```

Add `R6` alphabetically (between `methods` and `rlang`).

### Step A.2: Write failing tests

Create `tests/testthat/test-anndata-facade.R`. Use a `memory_daf` fixture with axes `cell` (obs), `gene` (var), and a `UMIs` matrix, plus per-axis vectors (`donor`, `age` on cell; `chrom` on gene) and a scalar (`organism`).

```r
# ---- Helper ----
.make_facade_daf <- function() {
    d <- memory_daf(name = "facade_test")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(1:6, 3, 2))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    set_vector(d, "cell", "age", c(1L, 2L, 3L))
    set_vector(d, "gene", "chrom", c("1", "2"))
    set_scalar(d, "organism", "human")
    d
}

# ---- Tests ----
test_that("DafAnnData constructs with explicit axes", {
    d <- .make_facade_daf()
    ann <- DafAnnData$new(d, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_s3_class(ann, "DafAnnData")
    expect_identical(ann$obs_axis, "cell")
    expect_identical(ann$var_axis, "gene")
    expect_identical(ann$x_name, "UMIs")
})

test_that("DafAnnData auto-detects obs and var axes", {
    d <- .make_facade_daf()
    ann <- DafAnnData$new(d)
    expect_identical(ann$obs_axis, "cell")
    expect_identical(ann$var_axis, "gene")
})

test_that("DafAnnData auto-detects metacell obs axis when cell absent", {
    d <- memory_daf()
    add_axis(d, "metacell", c("m1", "m2"))
    add_axis(d, "gene", c("g1", "g2"))
    set_matrix(d, "metacell", "gene", "UMIs", matrix(1:4, 2, 2))
    ann <- DafAnnData$new(d)
    expect_identical(ann$obs_axis, "metacell")
})

test_that("DafAnnData errors when no suitable obs axis exists", {
    d <- memory_daf()
    add_axis(d, "gene", c("g1"))
    expect_error(DafAnnData$new(d), "obs_axis")
})

test_that("DafAnnData rejects non-daf input", {
    expect_error(DafAnnData$new(NULL), "DafReader")
})

test_that("as_anndata returns a DafAnnData", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_s3_class(ann, "DafAnnData")
})

test_that("X active binding returns the matrix", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    m <- ann$X
    expect_identical(dim(m), c(3L, 2L))
    expect_identical(m[1, 1], 1L)
})

test_that("obs active binding returns a data.frame with axis entries as rownames", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    obs <- ann$obs
    expect_s3_class(obs, "data.frame")
    expect_identical(rownames(obs), c("c1", "c2", "c3"))
    expect_setequal(colnames(obs), c("donor", "age"))
})

test_that("var active binding returns a data.frame", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    var <- ann$var
    expect_s3_class(var, "data.frame")
    expect_identical(rownames(var), c("g1", "g2"))
})

test_that("obs_names / var_names return character vectors", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_identical(ann$obs_names, c("c1", "c2", "c3"))
    expect_identical(ann$var_names, c("g1", "g2"))
})

test_that("n_obs / n_vars / shape return correct integers", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_identical(ann$n_obs, 3L)
    expect_identical(ann$n_vars, 2L)
    expect_identical(ann$shape, c(3L, 2L))
})

test_that("uns returns named list of scalars", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    u <- ann$uns
    expect_type(u, "list")
    expect_identical(u$organism, "human")
})

test_that("layers returns empty list when no other matrices exist", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_length(ann$layers, 0L)
})

test_that("layers returns named list of extra matrices", {
    d <- .make_facade_daf()
    set_matrix(d, "cell", "gene", "normalized",
        matrix(as.numeric(1:6) / 10, 3, 2))
    ann <- as_anndata(d)
    l <- ann$layers
    expect_length(l, 1L)
    expect_identical(names(l), "normalized")
})

# ---- Read-only guards ----

test_that("X write fails with exact wrapper message", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_error(
        ann$X <- matrix(0, 3, 2),
        "DafAnnData facade is read-only. Use the underlying Daf object to modify data."
    )
})

test_that("all 10 active bindings error on assignment", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    for (nm in c("X", "obs", "var", "layers", "uns",
                 "obs_names", "var_names", "n_obs", "n_vars", "shape")) {
        expect_error(eval(parse(text = sprintf("ann$%s <- NULL", nm))),
            "read-only", info = nm)
    }
})
```

### Step A.3: Create R/anndata_facade.R

```r
#' @include classes.R format_api.R handlers.R
NULL

.read_only_error <- function() {
    stop(
        "DafAnnData facade is read-only. Use the underlying Daf object to modify data.",
        call. = FALSE
    )
}

.auto_obs_axis <- function(daf) {
    for (candidate in c("cell", "metacell")) {
        if (format_has_axis(daf, candidate)) {
            return(candidate)
        }
    }
    stop("no obs_axis could be auto-detected (looked for 'cell', 'metacell'); pass obs_axis explicitly", call. = FALSE)
}

.auto_var_axis <- function(daf) {
    if (format_has_axis(daf, "gene")) {
        return("gene")
    }
    stop("no var_axis could be auto-detected (looked for 'gene'); pass var_axis explicitly", call. = FALSE)
}

#' Read-only AnnData-shaped facade over a Daf.
#'
#' Exposes a `DafReader` through the property names familiar to
#' `anndata` / `scanpy` / `Seurat` users: `X`, `obs`, `var`, `layers`,
#' `uns`, `obs_names`, `var_names`, `n_obs`, `n_vars`, `shape`. All
#' bindings are read-only; modifying data requires writing to the
#' underlying `Daf` directly.
#'
#' @param daf A [DafReader].
#' @param obs_axis Axis name for observations. Defaults auto-detect:
#'   `"cell"` then `"metacell"`.
#' @param var_axis Axis name for variables. Defaults to `"gene"`.
#' @param x_name Matrix name for `X`. Default `"UMIs"`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' add_axis(d, "gene", c("g1", "g2", "g3"))
#' set_matrix(d, "cell", "gene", "UMIs", matrix(1:6, 2, 3))
#' ann <- as_anndata(d)
#' ann$X
#' ann$obs_names
#' @seealso [as_anndata()], [h5ad_as_daf()], [daf_as_h5ad()]
#' @export
DafAnnData <- R6::R6Class(
    classname = "DafAnnData",
    public = list(
        daf = NULL,
        obs_axis = NULL,
        var_axis = NULL,
        x_name = NULL,
        initialize = function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
            if (!is_daf(daf)) {
                stop("`daf` must be a DafReader", call. = FALSE)
            }
            self$daf <- daf
            self$obs_axis <- obs_axis %||% .auto_obs_axis(daf)
            self$var_axis <- var_axis %||% .auto_var_axis(daf)
            self$x_name <- x_name
        }
    ),
    active = list(
        X = function(value) {
            if (!missing(value)) .read_only_error()
            get_matrix(self$daf, self$obs_axis, self$var_axis, self$x_name)
        },
        obs = function(value) {
            if (!missing(value)) .read_only_error()
            get_dataframe(self$daf, self$obs_axis)
        },
        var = function(value) {
            if (!missing(value)) .read_only_error()
            get_dataframe(self$daf, self$var_axis)
        },
        obs_names = function(value) {
            if (!missing(value)) .read_only_error()
            format_axis_array(self$daf, self$obs_axis)
        },
        var_names = function(value) {
            if (!missing(value)) .read_only_error()
            format_axis_array(self$daf, self$var_axis)
        },
        n_obs = function(value) {
            if (!missing(value)) .read_only_error()
            as.integer(length(format_axis_array(self$daf, self$obs_axis)))
        },
        n_vars = function(value) {
            if (!missing(value)) .read_only_error()
            as.integer(length(format_axis_array(self$daf, self$var_axis)))
        },
        shape = function(value) {
            if (!missing(value)) .read_only_error()
            c(self$n_obs, self$n_vars)
        },
        layers = function(value) {
            if (!missing(value)) .read_only_error()
            mats <- format_matrices_set(self$daf, self$obs_axis, self$var_axis)
            mats <- setdiff(mats, self$x_name)
            stats::setNames(
                lapply(mats, function(nm) {
                    get_matrix(self$daf, self$obs_axis, self$var_axis, nm)
                }),
                mats
            )
        },
        uns = function(value) {
            if (!missing(value)) .read_only_error()
            scalars <- format_scalars_set(self$daf)
            stats::setNames(
                lapply(scalars, function(nm) get_scalar(self$daf, nm)),
                scalars
            )
        }
    )
)

#' One-shot factory for a [DafAnnData] facade.
#'
#' @inheritParams DafAnnData
#' @return A [DafAnnData] instance.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' add_axis(d, "gene", c("g1", "g2"))
#' set_matrix(d, "cell", "gene", "UMIs", matrix(1:4, 2, 2))
#' ann <- as_anndata(d)
#' @seealso [DafAnnData], [h5ad_as_daf()], [daf_as_h5ad()]
#' @export
as_anndata <- function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
    DafAnnData$new(daf, obs_axis = obs_axis, var_axis = var_axis, x_name = x_name)
}
```

### Step A.4: Regen docs, install, test, commit

```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: +~40 assertions. ~2576 PASS.

```
git add R/anndata_facade.R DESCRIPTION NAMESPACE man/ tests/testthat/test-anndata-facade.R
git commit -m "feat(10b): add DafAnnData R6 facade + as_anndata"
```

---

## Phase B: h5ad I/O

**Files:**
- Create: `R/anndata_format.R`.
- Modify: `DESCRIPTION` (+`hdf5r` to Suggests).
- Create: `inst/extdata/small_test.h5ad` (fixture).
- Create: `tests/testthat/test-anndata-format.R`, `tests/testthat/test-anndata-handlers.R`.
- Create: `dev/scripts/generate-small-test-h5ad.R` (fixture generator; in dev/).

### Step B.1: DESCRIPTION — add hdf5r to Suggests

Add `hdf5r` to Suggests alphabetically.

### Step B.2: Generate fixture

Write `dev/scripts/generate-small-test-h5ad.R` that uses Python's `anndata` via reticulate OR uses `hdf5r` directly to write a minimal h5ad. Simplest: construct via `hdf5r::H5File$new(..., mode = "w")` with these groups:
- `/X` — dense matrix, 50 × 20, float64, simple values (seed-reproducible).
- `/obs/` group with `_index` (character, 50 entries) + one `donor` (character, 50 entries) + one `age` (int64, 50 entries).
- `/var/` group with `_index` (character, 20 entries) + one `chrom` (character, 20 entries).
- `/uns/` group with `organism` (scalar string).
- `/layers/` empty group.

Root attributes: `encoding-type = "anndata"`, `encoding-version = "0.1.0"`.

Run the script once; commit the resulting `inst/extdata/small_test.h5ad` into the package repo.

### Step B.3: Write failing tests

Create `tests/testthat/test-anndata-format.R` (~45 assertions) and `tests/testthat/test-anndata-handlers.R` (~15 assertions).

Key cases:
```r
test_that("h5ad_as_daf loads the fixture into a memory_daf", {
    path <- system.file("extdata", "small_test.h5ad", package = "dafr")
    skip_if(path == "", "fixture not available")
    d <- h5ad_as_daf(path, name = "loaded")
    expect_true(is_daf(d))
    expect_identical(daf_name(d), "loaded")
    expect_true(format_has_axis(d, "obs"))
    expect_true(format_has_axis(d, "var"))
    expect_identical(length(format_axis_array(d, "obs")), 50L)
    expect_identical(length(format_axis_array(d, "var")), 20L)
    expect_true(format_has_matrix(d, "obs", "var", "UMIs") ||
                format_has_matrix(d, "var", "obs", "UMIs"))
})

test_that("daf_as_h5ad writes a Daf to h5ad and round-trips", {
    d <- memory_daf(name = "rt")
    add_axis(d, "obs", c("o1", "o2", "o3"))
    add_axis(d, "var", c("v1", "v2"))
    set_matrix(d, "obs", "var", "UMIs", matrix(as.numeric(1:6), 3, 2))
    set_vector(d, "obs", "donor", c("A", "B", "A"))
    set_scalar(d, "organism", "human")

    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p))
    daf_as_h5ad(d, p, obs_axis = "obs", var_axis = "var", x_name = "UMIs")

    expect_true(file.exists(p))
    d2 <- h5ad_as_daf(p, name = "back")
    expect_identical(sort(format_axis_array(d2, "obs")), c("o1", "o2", "o3"))
    expect_identical(sort(format_axis_array(d2, "var")), c("v1", "v2"))
    m <- get_matrix(d2, "obs", "var", "UMIs")
    expect_identical(dim(m), c(3L, 2L))
})

test_that("daf_as_h5ad errors on existing file without overwrite", {
    p <- tempfile(fileext = ".h5ad")
    file.create(p)
    on.exit(unlink(p))
    d <- memory_daf()
    expect_error(daf_as_h5ad(d, p, overwrite = FALSE), "exists")
})

test_that("h5ad_as_daf errors on missing file", {
    expect_error(h5ad_as_daf("/nonexistent/path.h5ad"))
})

test_that("h5ad functions error if hdf5r is not installed", {
    skip_if(requireNamespace("hdf5r", quietly = TRUE), "hdf5r installed")
    expect_error(h5ad_as_daf("/x"), "hdf5r")
})
```

### Step B.4: Create R/anndata_format.R

Implement `h5ad_as_daf` and `daf_as_h5ad`. The logic is complex — outline:

**`h5ad_as_daf(path, name, mode, unsupported_handler)`:**
1. `rlang::check_installed("hdf5r")`.
2. Temporarily register the `unsupported_handler` via `inefficient_action_handler`, restore on exit.
3. Open hdf5 file; `on.exit(h5$close_all())`.
4. Create `memory_daf(name = name %||% path-basename)`.
5. Read `/obs/_index` → add_axis `obs`.
6. Read `/var/_index` → add_axis `var`.
7. Read `/X` → detect sparse vs dense encoding; transpose if needed; `set_matrix(d, "obs", "var", "UMIs")`.
8. For each child in `/obs/` (except `_index`): read as vector; `set_vector(d, "obs", name, v)`.
9. Same for `/var/`.
10. For each child in `/layers/`: `set_matrix(d, "obs", "var", name, m)`.
11. For each child in `/uns/`: if scalar-shape → `set_scalar`; else → emit via `inefficient_action_handler` and skip.
12. Return daf.

**`daf_as_h5ad(daf, path, obs_axis, var_axis, x_name, overwrite, unsupported_handler)`:**
1. `rlang::check_installed("hdf5r")`.
2. `if (!overwrite && file.exists(path)) stop("file exists; pass overwrite = TRUE")`.
3. Auto-detect axes (same as facade).
4. Temporarily register handler.
5. Open hdf5 file for write; `on.exit(close_all)`.
6. Write `/obs/_index`, `/var/_index`.
7. Write `/X` (dense double).
8. Write `/obs/<name>` per vector on obs_axis (excluding `_index`).
9. Write `/var/<name>` per vector on var_axis.
10. Write `/layers/<name>` per matrix excluding `x_name`.
11. Write `/uns/<name>` per scalar.
12. Set root attrs `encoding-type`, `encoding-version`.
13. Close file. Return `invisible(path)`.

**String encoding:** h5ad uses `dtype = O`; hdf5r provides `H5T_VARLEN_STRING` for variable-length strings. Write as `H5T_STRING$new(size = Inf, variable = TRUE)`.

**Dtype preservation:** check `is.integer(v)` vs `is.double(v)` before writing; use `$create_dataset(..., dtype = h5types$H5T_NATIVE_INT)` etc.

### Step B.5: Test + commit

```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: +~60 assertions. ~2636 PASS.

```
git add R/anndata_format.R DESCRIPTION inst/extdata/small_test.h5ad tests/testthat/test-anndata-format.R tests/testthat/test-anndata-handlers.R NAMESPACE man/
git commit -m "feat(10b): add h5ad_as_daf + daf_as_h5ad h5ad I/O"
```

---

## Phase Z: Polish + merge + tag

### Step Z.1: NEWS entry

Append under `# dafr (development version)` heading, before the `## Slice 10a` section:

```markdown
## Slice 10b — AnnData + h5ad round-trip (2026-04-23)

### New exports (4)

- **`DafAnnData`** R6 class: read-only AnnData-shaped facade over a Daf.
  Active bindings for `X`, `obs`, `var`, `layers`, `uns`, `obs_names`,
  `var_names`, `n_obs`, `n_vars`, `shape`. Writes error.
- **`as_anndata(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs")`**
  — factory returning `DafAnnData`. Auto-detects obs_axis from
  `"cell"`/`"metacell"` and var_axis from `"gene"`.
- **`h5ad_as_daf(path, name = NULL, mode = "r",
  unsupported_handler = WARN_HANDLER)`** — loads a Muon-style h5ad into
  a fresh `memory_daf`.
- **`daf_as_h5ad(daf, path, obs_axis = NULL, var_axis = NULL,
  x_name = "UMIs", overwrite = FALSE,
  unsupported_handler = WARN_HANDLER)`** — writes a Daf to h5ad.

### Dependency changes

- `R6` promoted to `Imports` (facade class).
- `hdf5r` added to `Suggests` (gated via `rlang::check_installed`
  in each h5ad function).

### Fixture

- `inst/extdata/small_test.h5ad` — 50 obs × 20 var reference for
  round-trip tests.
```

### Step Z.2: devtools::check
Expected: 0E / 0W / 4N (same carry-over). Fix any new NOTE inline.

### Step Z.3: Final tests + merge + tag

```
git add NEWS.md && git commit -m "docs(10b): add NEWS entry for slice 10b — 4 new exports"
git checkout main && git merge --no-ff slice-10b -m "merge(10b): AnnData facade + h5ad round-trip"
git tag slice-10b
```

### Step Z.4: Exit note

Write `dev/notes/slice-10b-exit.md`; commit in nested `dev/`.

---

## Self-review

- Spec §4 all exports → Phase A/B. ✓
- Spec §5 error handling → tests per phase. ✓
- Spec §6 test plan → 3 new test files. ✓
- Spec §7 deps → DESCRIPTION edits in A.1 + B.1. ✓
- Spec §8 order → Phase 0 / A / B / Z. ✓
- Spec §9 exit → Phase Z. ✓
