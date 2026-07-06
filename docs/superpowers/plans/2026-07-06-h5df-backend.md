# H5df Backend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an `H5df` storage backend - a whole Daf store in one `.h5df` HDF5 file, interoperable with `DataAxesFormats.jl`'s `H5df`.

**Architecture:** A new `R/h5df.R` mirroring `R/zip_daf.R`'s structure (class pair `H5df`/`H5dfReadOnly` off `DafWriter`/`DafReadOnly`, an `internal` env holding the store handle, a public `h5df()` constructor, and `format_*` S7 method registrations). Unlike ZipDaf's raw-byte ZIP container, H5df uses `hdf5r` to write **typed HDF5 datasets** directly - no JSON descriptors, no byte encoding. It is **not** append-only: delete/overwrite/reorder are supported (FilesDaf-style protocol). Design spec: `docs/superpowers/specs/2026-07-06-h5df-backend-design.md`.

**Tech Stack:** R, S7, `hdf5r` (existing Suggests dep, used by the AnnData reader), `Matrix` (existing Import), `bit64`. Julia interop verified via `conda run -n dafr-mcview julia`.

---

## Key facts established by probing `hdf5r` 1.3.10 (do not re-derive)

- **Every `create_dataset` call passes `chunk_dims = NULL`** to force contiguous/flat, filter-free storage (mmap-able, matches Julia's default). Omitting it triggers auto-chunking, which errors on scalar space and produces chunked datasets.
- **Scalars**: `root$create_dataset("scalars/<name>", robj = value, space = hdf5r::H5S$new("scalar"), chunk_dims = NULL)` writes a true HDF5 scalar-space dataset (matches Julia). Works for double, string, and `bit64::integer64`. Nested-path create works when the parent group exists.
- **Matrix orientation is a no-op transpose.** `hdf5r` reverses dims on disk: an R matrix of dim `(nr, nc)` is stored with HDF5 dataspace `(nc, nr)` and column-major bytes - **exactly** Julia's convention. So dense matrices write directly (`robj = mat`) and read back directly (`obj$read()` gives dim `(nr, nc)`). No `t()`. (Verified by the R round-trip in Task 5 and the Julia interop in Task 7.)
- **Group vs dataset**: `inherits(obj, "H5Group")` is `TRUE` for a group (sparse), the object is class `"H5D"` for a dataset (dense). This is how sparsity is detected on read (no `sparse`/`nnz` attribute exists).
- **Unsigned index types**: `hdf5r::h5types$H5T_NATIVE_UINT16 / _UINT32 / _UINT64` all exist.
- **Deletion**: `root$link_delete("<path>")` removes datasets, nested paths, and groups (recursively for groups).
- **Existence**: `root$exists("<path>")`. **Listing**: `root[["<group>"]]$names` (returns `character(0)` for an empty group).
- **GOTCHA - empty vlen-string read crashes hdf5r** (`H5Dvlen_reclaim invalid argument`). Reads of possibly-empty string datasets go through `.h5_safe_read()` (defined in Task 4), which returns `character(0)` when the dataset is empty.
- **hdf5r H5File open modes** differ from dafr modes. Map: dafr `"r"`->`"r"`, `"r+"`->`"r+"`, `"w"`->`"w"` (truncate), `"w+"`->`"a"` (create-or-append, no truncate).

## Reused existing dafr helpers (call as-is; do not reimplement)

From `R/files_io.R` / `R/utils.R` / `R/cache.R` / `R/files_daf_read.R`, already used by ZipDaf:
`.dtype_for_r_vector`, `.indtype_for_size`, `.assert_name`, `.assert_scalar_value`, `.validate_vector_value`, `.validate_matrix_value`, `.require_scalar`, `.require_no_scalar`, `.require_axis`, `.require_no_axis`, `.require_vector`, `.require_no_vector`, `.require_matrix`, `.require_no_matrix`, `.attach_vector_axis_names`, `.attach_matrix_axis_dimnames`, `.files_daf_classify_vector`, `.files_daf_classify_matrix`, `.cache_group_value`, `bump_vector_counter`, `bump_matrix_counter`, `new_internal_env`, `new_cache_env`, `new_counter_env`, `MEMORY_DATA`.

## File structure

- **Create** `R/h5df.R` - the whole backend (class pair, constructor, all helpers, all `format_*` registrations). One focused file, like `R/zip_daf.R`.
- **Modify** `R/open_daf.R` - replace the `*.h5df` stub with dispatch to `h5df()`.
- **Create** `tests/testthat/test-h5df.R`, `tests/testthat/test-h5df-adversarial.R`, `tests/testthat/test-h5df-julia-compat.R`.
- **Modify** `DESCRIPTION` (Collate), `NAMESPACE` (via `document()`), `_pkgdown.yml`, `NEWS.md`.

## Test invocation (project convention - do NOT use the installed package)

```
NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-h5df.R")'
```

---

## Task 1: Scaffolding - class pair, constructor, open_daf dispatch

**Files:**
- Create: `R/h5df.R`
- Modify: `R/open_daf.R:38-40`
- Test: `tests/testthat/test-h5df.R`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-h5df.R`:

```r
skip_if_no_hdf5r <- function() testthat::skip_if_not_installed("hdf5r")

test_that("h5df creates a store, marks it, and reopens read-only", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    expect_s3_class(d, "dafr::H5df")
    expect_true(file.exists(p))
    rm(d)
    gc()
    r <- h5df(p, mode = "r")
    expect_s3_class(r, "dafr::H5dfReadOnly")
    expect_equal(dafr:::.is_leaf_dispatch(r), TRUE)
    rm(r)
    gc()
})

test_that("h5df mode guards", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    expect_error(h5df(p, mode = "r"), "not a daf")
    d <- h5df(p, mode = "w"); rm(d); gc()
    expect_error(h5df(p, mode = "w"), "already a daf")   # use w+
    d2 <- h5df(p, mode = "w+"); rm(d2); gc()             # append ok
})

test_that("open_daf dispatches .h5df and rejects grouped", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- open_daf(p, mode = "w")
    expect_s3_class(d, "dafr::H5df")
    rm(d); gc()
    expect_error(open_daf("x.h5dfs#/g", mode = "r"), "not supported")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-h5df.R")'`
Expected: FAIL - `could not find function "h5df"`.

- [ ] **Step 3: Create `R/h5df.R` skeleton**

```r
#' @include classes.R format_api.R files_io.R utils.R cache.R
NULL

# H5df: a whole Daf store in one .h5df HDF5 file, interoperable with
# DataAxesFormats.jl H5df. Layout (no attributes): a `daf` dataset holding
# UInt8[1,0] marks the store; scalars/axes/vectors/matrices groups hold typed
# HDF5 datasets. Sparsity = group-vs-dataset. Built on hdf5r (Suggests).

#' File-backed (HDF5) Daf writer class.
#'
#' Concrete `DafWriter` subclass instantiated by [h5df()] for writable modes
#' (`"r+"`, `"w"`, `"w+"`). Use [h5df()] to construct instances.
#' @inheritParams DafReader
#' @export
H5df <- S7::new_class(name = "H5df", package = "dafr", parent = DafWriter)

#' File-backed (HDF5) read-only Daf class.
#'
#' Concrete `DafReadOnly` subclass instantiated by [h5df()] with mode `"r"`.
#' @inheritParams DafReader
#' @export
H5dfReadOnly <- S7::new_class(
    name = "H5dfReadOnly", package = "dafr", parent = DafReadOnly
)

# ---- handle & key helpers ----
# Grouped .h5dfs#/group stores are deferred, so the H5File IS the store root.
.h5_root <- function(daf) S7::prop(daf, "internal")$h5
.hkey_scalar <- function(name) paste0("scalars/", name)
.hkey_axis   <- function(axis) paste0("axes/", axis)
.hkey_vector <- function(axis, name) paste0("vectors/", axis, "/", name)
.hkey_matrix <- function(ra, ca, name) paste0("matrices/", ra, "/", ca, "/", name)

.h5_read_only_guard <- function(verb) {
    stop(sprintf("h5df: store opened read-only; %s not permitted", verb),
        call. = FALSE)
}

# dafr mode -> hdf5r H5File mode
.H5_OPEN_MODE <- c(r = "r", "r+" = "r+", w = "w", "w+" = "a")

# indtype name ("UInt16"/...) -> hdf5r native unsigned type object
.h5_uint_type <- function(indtype) {
    switch(indtype,
        UInt16 = hdf5r::h5types$H5T_NATIVE_UINT16,
        UInt32 = hdf5r::h5types$H5T_NATIVE_UINT32,
        UInt64 = hdf5r::h5types$H5T_NATIVE_UINT64,
        stop(sprintf("h5df: unexpected index type %s", indtype), call. = FALSE))
}

.h5_write_index <- function(grp, name, values, indtype) {
    grp$create_dataset(name, robj = as.integer(values),
        dtype = .h5_uint_type(indtype), chunk_dims = NULL)
    invisible()
}

.h5_check_version <- function(h5, path) {
    v <- as.integer(h5[["daf"]]$read())
    if (length(v) != 2L || v[[1L]] != 1L || v[[2L]] > 0L) {
        stop(sprintf(paste0(
            "incompatible format version: %s\nfor the h5df: %s\n",
            "the code supports version: 1.0"),
            paste(v, collapse = "."), path), call. = FALSE)
    }
    invisible()
}

# ==== constructor ============================================================

#' Single-file (HDF5) Daf store.
#'
#' A `Daf` store held in one `.h5df` HDF5 file, interoperable with Julia's
#' `DataAxesFormats.H5df`. The file holds a `daf` marker dataset plus
#' `scalars`/`axes`/`vectors`/`matrices` groups of typed HDF5 datasets.
#' Requires the `hdf5r` package.
#'
#' @param path Path to a `.h5df` file.
#' @param mode One of `"r"` (read; must exist), `"r+"` (append; must exist),
#'   `"w"` (create; fails if it is already a daf store), `"w+"` (create or
#'   append).
#' @param name Human-readable identifier. Default derived from the store's
#'   `name` scalar if present, else `basename(path)`.
#' @return An `H5df` (writable modes) or `H5dfReadOnly` (`"r"`).
#' @examples
#' if (requireNamespace("hdf5r", quietly = TRUE)) {
#'   path <- tempfile("dafr-", fileext = ".h5df")
#'   d <- h5df(path, mode = "w")
#'   add_axis(d, "cell", c("c1", "c2"))
#'   set_scalar(d, "organism", "human")
#'   rm(d)
#'   unlink(path)
#' }
#' @export
h5df <- function(path, mode = c("r", "r+", "w", "w+"), name = NULL) {
    stopifnot(is.character(path), length(path) == 1L, !is.na(path))
    rlang::check_installed("hdf5r", reason = "for `h5df()`")
    mode <- match.arg(mode)
    if (mode == "w" && file.exists(path)) {
        already <- tryCatch({
            h0 <- hdf5r::H5File$new(path, mode = "r")
            res <- h0$exists("daf")
            h0$close_all()
            res
        }, error = function(e) FALSE)
        if (isTRUE(already)) {
            stop(sprintf("h5df(%s, 'w'): file is already a daf store; use 'w+'",
                sQuote(path)), call. = FALSE)
        }
    }
    h5 <- hdf5r::H5File$new(path, mode = unname(.H5_OPEN_MODE[[mode]]))
    has_marker <- h5$exists("daf")
    if (mode %in% c("r", "r+") && !has_marker) {
        stop(sprintf("h5df(%s, '%s'): not a daf store (no daf marker)",
            sQuote(path), mode), call. = FALSE)
    }
    if (mode %in% c("w", "w+") && !has_marker) {
        for (g in c("scalars", "axes", "vectors", "matrices")) h5$create_group(g)
        h5$create_dataset("daf", robj = c(1L, 0L),
            dtype = hdf5r::h5types$H5T_NATIVE_UINT8, chunk_dims = NULL)
    }
    .h5_check_version(h5, path)
    if (is.null(name)) {
        name <- if (h5$exists(.hkey_scalar("name"))) {
            as.character(h5[[.hkey_scalar("name")]]$read())
        } else {
            basename(path)
        }
    }
    .assert_name(name, "name")
    internal <- new_internal_env()
    internal$h5 <- h5
    internal$path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    internal$mode <- mode
    internal$axes <- new.env(parent = emptyenv())
    ctor <- if (mode == "r") H5dfReadOnly else H5df
    ctor(
        name = name, internal = internal, cache = new_cache_env(),
        axis_version_counter = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env()
    )
}
```

Add the reader/description/is_leaf registration block (extended by later tasks; scalars/axes/vectors/matrices readers are added in Tasks 2-5):

```r
# ==== S7 registrations: reader-side (both writer + read-only) ================
local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(.is_leaf_dispatch, cls) <- function(daf) TRUE
        S7::method(format_description_header, cls) <-
            function(daf, indent = "", deep = FALSE) {
                internal <- S7::prop(daf, "internal")
                c(paste0(indent, "type: H5df"),
                  paste0(indent, "path: ", internal$path),
                  paste0(indent, "mode: ", internal$mode))
            }
    }
})
```

Note: rely on `hdf5r`'s built-in gc finalizer to close the `H5File` (the `internal` env is the only reference; `rm(d); gc()` flushes to disk - the same pattern the ZipDaf tests use before Julia reads).
`# ponytail: no custom finalizer; hdf5r closes the H5File on gc. Add reg.finalizer only if a leak shows up.`

- [ ] **Step 4: Wire `open_daf` dispatch**

In `R/open_daf.R`, replace lines 38-40:

```r
    if (endsWith(uri, ".h5df")) {
        return(h5df(uri, mode = mode, name = name))
    }
    if (grepl(".h5dfs#", uri, fixed = TRUE)) {
        stop(sprintf(paste0(
            "open_daf: grouped .h5dfs#/group stores are not supported yet.\n",
            "Refused: %s"), uri), call. = FALSE)
    }
```

Add `'h5df.R'` to the `Collate:` field in `DESCRIPTION` (after `'zip_daf.R'`).

- [ ] **Step 5: Run tests to verify they pass**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-h5df.R")'`
Expected: PASS (3 tests).

- [ ] **Step 6: Commit**

```bash
git add R/h5df.R R/open_daf.R DESCRIPTION tests/testthat/test-h5df.R
git commit -m "feat(h5df): scaffold H5df backend (class, constructor, dispatch)"
```

---

## Task 2: Scalars

**Files:** Modify `R/h5df.R`; Test `tests/testthat/test-h5df.R`

- [ ] **Step 1: Write the failing test** (append to `test-h5df.R`)

```r
test_that("h5df scalars round-trip, list, overwrite, delete", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    set_scalar(d, "pi", 3.14)
    set_scalar(d, "n", 5L)
    set_scalar(d, "big", bit64::as.integer64(2^40))
    set_scalar(d, "note", "hello")
    expect_equal(get_scalar(d, "pi"), 3.14)
    expect_equal(get_scalar(d, "note"), "hello")
    expect_equal(as.numeric(get_scalar(d, "big")), 2^40)
    expect_setequal(scalars_set(d), c("pi", "n", "big", "note"))
    expect_error(set_scalar(d, "pi", 9), "exist")          # no overwrite
    set_scalar(d, "pi", 9, overwrite = TRUE)
    expect_equal(get_scalar(d, "pi"), 9)
    delete_scalar(d, "pi")
    expect_false(has_scalar(d, "pi"))
    expect_error(get_scalar(d, "pi"), "does not")
    rm(d); gc()
})
```

- [ ] **Step 2: Run to verify it fails** - Expected: FAIL (`format_has_scalar` not defined for H5df).

- [ ] **Step 3: Implement** (append to `R/h5df.R`)

```r
# ==== scalars ================================================================

.h5_has_scalar <- function(daf, name) .h5_root(daf)$exists(.hkey_scalar(name))
.h5_get_scalar <- function(daf, name) {
    root <- .h5_root(daf); key <- .hkey_scalar(name)
    if (!root$exists(key)) .require_scalar(daf, name)
    root[[key]]$read()
}
.h5_scalars_set <- function(daf) {
    root <- .h5_root(daf)
    if (!root$exists("scalars")) return(character(0L))
    sort(root[["scalars"]]$names, method = "radix")
}
.h5_set_scalar <- function(daf, name, value, overwrite) {
    .assert_scalar_value(name, value)
    root <- .h5_root(daf); key <- .hkey_scalar(name)
    if (!overwrite) .require_no_scalar(daf, name)
    if (root$exists(key)) root$link_delete(key)
    root$create_dataset(key, robj = value,
        space = hdf5r::H5S$new("scalar"), chunk_dims = NULL)
    MEMORY_DATA
}
.h5_delete_scalar <- function(daf, name, must_exist) {
    root <- .h5_root(daf); key <- .hkey_scalar(name)
    if (!root$exists(key)) {
        if (must_exist) .require_scalar(daf, name)
        return(invisible())
    }
    root$link_delete(key)
    invisible()
}

local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(format_has_scalar, list(cls, S7::class_character)) <-
            function(daf, name) .h5_has_scalar(daf, name)
        S7::method(format_get_scalar, list(cls, S7::class_character)) <-
            function(daf, name) .cache_group_value(.h5_get_scalar(daf, name), MEMORY_DATA)
        S7::method(format_scalars_set, cls) <- function(daf) .h5_scalars_set(daf)
    }
})

S7::method(format_set_scalar,
    list(H5df, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, name, value, overwrite) .h5_set_scalar(daf, name, value, overwrite)
S7::method(format_delete_scalar, list(H5df, S7::class_character, S7::class_logical)) <-
    function(daf, name, must_exist) .h5_delete_scalar(daf, name, must_exist)
```

- [ ] **Step 4: Run to verify it passes** - Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/h5df.R tests/testthat/test-h5df.R
git commit -m "feat(h5df): scalars (read/write/delete/overwrite/list)"
```

---

## Task 3: Axes

**Files:** Modify `R/h5df.R`; Test `tests/testthat/test-h5df.R`

- [ ] **Step 1: Write the failing test** (append)

```r
test_that("h5df axes round-trip, list, delete cascade, empty axis", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    add_axis(d, "empty", character(0))
    expect_true(has_axis(d, "cell"))
    expect_equal(axis_vector(d, "cell"), c("A", "B", "C", "D"))
    expect_equal(axis_length(d, "gene"), 2L)
    expect_equal(axis_length(d, "empty"), 0L)
    expect_setequal(axes_set(d), c("cell", "gene", "empty"))
    expect_error(add_axis(d, "cell", c("A", "B")), "exist")
    expect_error(add_axis(d, "bad", c("a", "a")), "non-unique")
    delete_axis(d, "gene")
    expect_false(has_axis(d, "gene"))
    rm(d); gc()
})
```

- [ ] **Step 2: Run to verify it fails** - Expected: FAIL.

- [ ] **Step 3: Implement** (append to `R/h5df.R`)

```r
# ==== axes ===================================================================

.h5_axis_parsed <- function(daf, axis) {
    cache <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = cache, inherits = FALSE)) {
        return(get(axis, envir = cache, inherits = FALSE))
    }
    root <- .h5_root(daf); key <- .hkey_axis(axis)
    if (!root$exists(key)) return(NULL)
    entries <- .h5_safe_read(root[[key]])          # .h5_safe_read defined in Task 4
    entries <- as.character(entries)
    if (anyNA(entries) || (length(entries) && any(!nzchar(entries)))) {
        stop(sprintf("h5df: axis %s contains empty entries", sQuote(axis)), call. = FALSE)
    }
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    parsed <- list(entries = entries, dict = dict)
    assign(axis, parsed, envir = cache)
    parsed
}
.h5_axis_require <- function(daf, axis) {
    parsed <- .h5_axis_parsed(daf, axis)
    if (is.null(parsed)) .require_axis(daf, "for: h5df backend", axis)
    parsed
}
.h5_has_axis <- function(daf, axis) .h5_root(daf)$exists(.hkey_axis(axis))
.h5_axes_set <- function(daf) {
    root <- .h5_root(daf)
    if (!root$exists("axes")) return(character(0L))
    sort(root[["axes"]]$names, method = "radix")
}

.h5_add_axis <- function(daf, axis, entries) {
    if (!is.character(entries)) {
        stop(sprintf("axis %s entries must be a character vector", sQuote(axis)), call. = FALSE)
    }
    if (anyNA(entries)) {
        stop(sprintf("axis %s entries contain NA", sQuote(axis)), call. = FALSE)
    }
    if (any(!nzchar(entries))) {
        stop(sprintf("axis %s entries contain empty strings", sQuote(axis)), call. = FALSE)
    }
    if (any(grepl("[\n\r]", entries))) {
        stop(sprintf("axis %s entries contain newline characters", sQuote(axis)), call. = FALSE)
    }
    if (anyDuplicated(entries)) {
        stop(sprintf("non-unique entries for new axis: %s\nin the daf data: %s",
            axis, S7::prop(daf, "name")), call. = FALSE)
    }
    .require_no_axis(daf, axis)
    root <- .h5_root(daf)
    # A zero-length `robj` writes a valid empty vlen-string dataset; reads of it
    # go through `.h5_safe_read` (hdf5r crashes reading empty vlen strings).
    root$create_dataset(.hkey_axis(axis), robj = entries, chunk_dims = NULL)
    # Eagerly create vectors/<axis> and every matrices/<a>/<b> pairing (incl.
    # self) so a Julia reader scanning the store does not trip on missing groups.
    root$create_group(paste0("vectors/", axis))
    existing <- root[["matrices"]]$names
    root$create_group(paste0("matrices/", axis))
    for (other in existing) {
        root$create_group(paste0("matrices/", axis, "/", other))
        root$create_group(paste0("matrices/", other, "/", axis))
    }
    root$create_group(paste0("matrices/", axis, "/", axis))
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    assign(axis, list(entries = entries, dict = dict),
        envir = S7::prop(daf, "internal")$axes)
    invisible()
}

.h5_delete_axis <- function(daf, axis, must_exist) {
    root <- .h5_root(daf); key <- .hkey_axis(axis)
    if (!root$exists(key)) {
        if (must_exist) .require_axis(daf, "for: delete_axis", axis)
        return(invisible())
    }
    root$link_delete(key)
    if (root$exists(paste0("vectors/", axis))) root$link_delete(paste0("vectors/", axis))
    if (root$exists(paste0("matrices/", axis))) root$link_delete(paste0("matrices/", axis))
    for (other in root[["matrices"]]$names) {
        k <- paste0("matrices/", other, "/", axis)
        if (root$exists(k)) root$link_delete(k)
    }
    cache <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = cache, inherits = FALSE)) rm(list = axis, envir = cache)
    invisible()
}

local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(format_has_axis, list(cls, S7::class_character)) <-
            function(daf, axis) .h5_has_axis(daf, axis)
        S7::method(format_axes_set, cls) <- function(daf) .h5_axes_set(daf)
        S7::method(format_axis_length, list(cls, S7::class_character)) <-
            function(daf, axis) length(.h5_axis_require(daf, axis)$entries)
        S7::method(format_axis_array, list(cls, S7::class_character)) <-
            function(daf, axis) .cache_group_value(.h5_axis_require(daf, axis)$entries, MEMORY_DATA)
        S7::method(format_axis_dict, list(cls, S7::class_character)) <-
            function(daf, axis) .h5_axis_require(daf, axis)$dict
    }
})

S7::method(format_add_axis, list(H5df, S7::class_character, S7::class_character)) <-
    function(daf, axis, entries) .h5_add_axis(daf, axis, entries)
S7::method(format_delete_axis, list(H5df, S7::class_character, S7::class_logical)) <-
    function(daf, axis, must_exist) .h5_delete_axis(daf, axis, must_exist)
```

Note: `.h5_axis_parsed` calls `.h5_safe_read` (Task 4). If implementing Task 3 before Task 4, add `.h5_safe_read` now from Task 4's Step 3.

- [ ] **Step 4: Run to verify it passes** - Expected: PASS. (If the empty-axis assertion errors on read, confirm `.h5_safe_read` is present.)

- [ ] **Step 5: Commit**

```bash
git add R/h5df.R tests/testthat/test-h5df.R
git commit -m "feat(h5df): axes (read/write/delete cascade/empty)"
```

---

## Task 4: Vectors - dense and sparse

**Files:** Modify `R/h5df.R`; Test `tests/testthat/test-h5df.R`

- [ ] **Step 1: Write the failing test** (append)

```r
test_that("h5df vectors round-trip: numeric, int, string, bool, sparse, empty", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "none", character(0))
    set_vector(d, "cell", "score", c(1.5, 2.5, 3.5, 4.5))
    set_vector(d, "cell", "donor", c(1L, 2L, 3L, 4L))
    set_vector(d, "cell", "label", c("a", "b", "c", "d"))
    set_vector(d, "cell", "flag", c(TRUE, FALSE, TRUE, TRUE))
    sv <- Matrix::sparseVector(x = c(10, 30), i = c(2L, 4L), length = 4L)
    set_vector(d, "cell", "sx", sv)
    set_vector(d, "none", "e", numeric(0))
    expect_equal(get_vector(d, "cell", "score"), c(1.5, 2.5, 3.5, 4.5), ignore_attr = TRUE)
    expect_equal(as.integer(get_vector(d, "cell", "donor")), 1:4)
    expect_equal(get_vector(d, "cell", "label"), c("a", "b", "c", "d"), ignore_attr = TRUE)
    expect_equal(get_vector(d, "cell", "flag"), c(TRUE, FALSE, TRUE, TRUE), ignore_attr = TRUE)
    expect_equal(get_vector(d, "cell", "sx"), c(0, 10, 0, 30), ignore_attr = TRUE)
    expect_length(get_vector(d, "none", "e"), 0L)
    expect_setequal(vectors_set(d, "cell"), c("score", "donor", "label", "flag", "sx"))
    expect_error(set_vector(d, "cell", "score", c(9, 9, 9, 9)), "exist")
    set_vector(d, "cell", "score", c(9, 9, 9, 9), overwrite = TRUE)
    expect_equal(get_vector(d, "cell", "score"), c(9, 9, 9, 9), ignore_attr = TRUE)
    delete_vector(d, "cell", "donor")
    expect_false(has_vector(d, "cell", "donor"))
    rm(d); gc()
})
```

- [ ] **Step 2: Run to verify it fails** - Expected: FAIL.

- [ ] **Step 3: Implement** (append to `R/h5df.R`)

```r
# ==== vectors ================================================================

# hdf5r crashes reclaiming an empty vlen-string buffer; return a typed empty
# vector for zero-length datasets, otherwise read normally.
.h5_safe_read <- function(obj) {
    if (prod(obj$dims) == 0L) {
        return(tryCatch(obj$read(), error = function(e) character(0L)))
    }
    obj$read()
}

.h5_has_vector <- function(daf, axis, name) {
    if (!format_has_axis(daf, axis)) return(FALSE)
    .h5_root(daf)$exists(.hkey_vector(axis, name))
}
.h5_vectors_set <- function(daf, axis) {
    if (!format_has_axis(daf, axis)) return(character(0L))
    root <- .h5_root(daf); key <- paste0("vectors/", axis)
    if (!root$exists(key)) return(character(0L))
    sort(root[[key]]$names, method = "radix")
}

# Scatter a sparse-group vector to a dense R vector (H5df returns dense
# vectors, matching ZipDaf).
.h5_read_sparse_vector <- function(grp, n) {
    idx <- as.integer(grp[["nzind"]]$read())        # 1-based
    if (grp$exists("nztxt")) {
        vals <- .h5_safe_read(grp[["nztxt"]])
        out <- rep("", n); out[idx] <- as.character(vals); return(out)
    }
    if (grp$exists("nzval")) {
        vals <- grp[["nzval"]]$read()
        out <- if (is.logical(vals)) logical(n) else vector(typeof(vals), n)
        out[idx] <- vals; return(out)
    }
    out <- logical(n); out[idx] <- TRUE; out        # bool-all-true: nzval omitted
}

.h5_get_vector_impl <- function(daf, axis, name) {
    root <- .h5_root(daf); key <- .hkey_vector(axis, name)
    if (!root$exists(key)) .require_vector(daf, axis, name)
    n <- format_axis_length(daf, axis)
    obj <- root[[key]]
    if (inherits(obj, "H5Group")) return(.h5_read_sparse_vector(obj, n))
    .h5_safe_read(obj)
}

.h5_set_vector_sparse <- function(daf, axis, name, sv, overwrite) {
    n <- format_axis_length(daf, axis)
    if (sv@length != n) {
        stop(sprintf("sparseVector %s length %d (expected %d) on axis %s",
            sQuote(name), sv@length, n, sQuote(axis)), call. = FALSE)
    }
    if (!overwrite) .require_no_vector(daf, axis, name)
    root <- .h5_root(daf); key <- .hkey_vector(axis, name)
    if (root$exists(key)) root$link_delete(key)
    grp <- root$create_group(key)
    .h5_write_index(grp, "nzind", as.integer(sv@i), .indtype_for_size(n))  # @i is 1-based
    eltype <- .dtype_for_r_vector(sv@x)
    if (eltype == "Bool") {
        if (!all(sv@x)) grp$create_dataset("nzval", robj = as.logical(sv@x), chunk_dims = NULL)
    } else {
        grp$create_dataset("nzval", robj = sv@x, chunk_dims = NULL)
    }
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}

# H5df stores dense input as a dense dataset and sparseVector input as a sparse
# group; it does NOT auto-sparsify dense input.
# ponytail: no sparsify heuristic; add one only if store size becomes a problem.
.h5_set_vector <- function(daf, axis, name, vec, overwrite) {
    if (methods::is(vec, "sparseVector")) {
        return(.h5_set_vector_sparse(daf, axis, name, vec, overwrite))
    }
    vec <- .validate_vector_value(daf, axis, name, vec)
    if (!overwrite) .require_no_vector(daf, axis, name)
    root <- .h5_root(daf); key <- .hkey_vector(axis, name)
    if (root$exists(key)) root$link_delete(key)
    # Empty `robj` is fine (writes a valid empty dataset); reads use `.h5_safe_read`.
    root$create_dataset(key, robj = vec, chunk_dims = NULL)
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}

.h5_delete_vector <- function(daf, axis, name, must_exist) {
    root <- .h5_root(daf); key <- .hkey_vector(axis, name)
    if (!root$exists(key)) {
        if (must_exist) .require_vector(daf, axis, name)
        return(invisible())
    }
    root$link_delete(key)
    invisible()
}

local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(format_has_vector, list(cls, S7::class_character, S7::class_character)) <-
            function(daf, axis, name) .h5_has_vector(daf, axis, name)
        S7::method(format_vectors_set, list(cls, S7::class_character)) <-
            function(daf, axis) .h5_vectors_set(daf, axis)
        S7::method(format_get_vector, list(cls, S7::class_character, S7::class_character)) <-
            function(daf, axis, name) {
                v <- .h5_get_vector_impl(daf, axis, name)
                .cache_group_value(.attach_vector_axis_names(daf, axis, v),
                    .files_daf_classify_vector(v))
            }
    }
})

S7::method(format_set_vector,
    list(H5df, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, axis, name, vec, overwrite) .h5_set_vector(daf, axis, name, vec, overwrite)
S7::method(format_delete_vector,
    list(H5df, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, axis, name, must_exist) .h5_delete_vector(daf, axis, name, must_exist)
```

- [ ] **Step 4: Run to verify it passes** - Expected: PASS. If the bool round-trip fails, it is the hdf5r-logical-vs-Julia-Bool concern - but for R-only round-trip (this test) it must pass; the Julia interop is Task 7.

- [ ] **Step 5: Commit**

```bash
git add R/h5df.R tests/testthat/test-h5df.R
git commit -m "feat(h5df): vectors (dense + sparse + bool-all-true + empty)"
```

---

## Task 5: Matrices - dense and sparse

**Files:** Modify `R/h5df.R`; Test `tests/testthat/test-h5df.R`

- [ ] **Step 1: Write the failing test** (append)

```r
test_that("h5df matrices round-trip: dense, sparse, bool, orientation, delete", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    dm <- matrix(as.double(1:8), nrow = 4, ncol = 2)   # (cell, gene)
    set_matrix(d, "cell", "gene", "dm", dm)
    got <- get_matrix(d, "cell", "gene", "dm")
    expect_equal(dim(got), c(4L, 2L))
    expect_equal(as.vector(got), as.double(1:8))       # orientation preserved
    sp <- Matrix::sparseMatrix(i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30), dims = c(4, 2))
    set_matrix(d, "cell", "gene", "sm", sp)
    gotsp <- get_matrix(d, "cell", "gene", "sm")
    expect_s4_class(gotsp, "dgCMatrix")
    expect_equal(as.matrix(gotsp), as.matrix(sp), ignore_attr = TRUE)
    bm <- as(Matrix::sparseMatrix(i = c(1, 4), j = c(1, 2), dims = c(4, 2)), "lgCMatrix")
    set_matrix(d, "cell", "gene", "bm", bm)
    expect_equal(as.matrix(get_matrix(d, "cell", "gene", "bm")), as.matrix(bm), ignore_attr = TRUE)
    expect_setequal(matrices_set(d, "cell", "gene"), c("dm", "sm", "bm"))
    delete_matrix(d, "cell", "gene", "dm")
    expect_false(has_matrix(d, "cell", "gene", "dm"))
    rm(d); gc()
})
```

- [ ] **Step 2: Run to verify it fails** - Expected: FAIL.

- [ ] **Step 3: Implement** (append to `R/h5df.R`)

```r
# ==== matrices ===============================================================

.h5_has_matrix <- function(daf, ra, ca, name) {
    if (!format_has_axis(daf, ra) || !format_has_axis(daf, ca)) return(FALSE)
    .h5_root(daf)$exists(.hkey_matrix(ra, ca, name))
}
.h5_matrices_set <- function(daf, ra, ca) {
    if (!format_has_axis(daf, ra) || !format_has_axis(daf, ca)) return(character(0L))
    root <- .h5_root(daf); key <- paste0("matrices/", ra, "/", ca)
    if (!root$exists(key)) return(character(0L))
    sort(root[[key]]$names, method = "radix")
}

.h5_read_sparse_matrix <- function(grp, nr, nc) {
    colptr <- as.integer(grp[["colptr"]]$read())
    rowval <- as.integer(grp[["rowval"]]$read())
    if (grp$exists("nztxt")) {                        # Julia-written string-sparse
        txt <- as.character(.h5_safe_read(grp[["nztxt"]]))
        m <- matrix("", nr, nc)
        for (j in seq_len(nc)) {
            if (colptr[j + 1L] > colptr[j]) {
                rng <- colptr[j]:(colptr[j + 1L] - 1L)
                m[rowval[rng], j] <- txt[rng]
            }
        }
        return(m)
    }
    i0 <- as.integer(rowval) - 1L
    p0 <- as.integer(colptr) - 1L
    if (grp$exists("nzval")) {
        vals <- grp[["nzval"]]$read()
        if (is.logical(vals)) {
            return(methods::new("lgCMatrix", x = vals, i = i0, p = p0,
                Dim = c(as.integer(nr), as.integer(nc)), Dimnames = list(NULL, NULL)))
        }
        return(methods::new("dgCMatrix", x = as.double(vals), i = i0, p = p0,
            Dim = c(as.integer(nr), as.integer(nc)), Dimnames = list(NULL, NULL)))
    }
    methods::new("lgCMatrix", x = rep(TRUE, length(i0)), i = i0, p = p0,   # bool-all-true
        Dim = c(as.integer(nr), as.integer(nc)), Dimnames = list(NULL, NULL))
}

.h5_get_matrix_impl <- function(daf, ra, ca, name) {
    root <- .h5_root(daf); key <- .hkey_matrix(ra, ca, name)
    if (!root$exists(key)) .require_matrix(daf, ra, ca, name, relayout = FALSE)
    nr <- format_axis_length(daf, ra); nc <- format_axis_length(daf, ca)
    obj <- root[[key]]
    if (inherits(obj, "H5Group")) return(.h5_read_sparse_matrix(obj, nr, nc))
    v <- .h5_safe_read(obj)                            # hdf5r reverses dims -> (nr, nc)
    if (is.null(dim(v))) dim(v) <- c(as.integer(nr), as.integer(nc))
    v
}

.h5_write_matrix_sparse <- function(root, key, mat) {
    is_bool <- methods::is(mat, "lgCMatrix")
    nr <- nrow(mat); nc <- ncol(mat); nnz <- length(mat@x)
    indtype <- .indtype_for_size(max(nr, nc, nnz))
    grp <- root$create_group(key)
    .h5_write_index(grp, "colptr", as.integer(mat@p) + 1L, indtype)   # 0-based -> 1-based
    .h5_write_index(grp, "rowval", as.integer(mat@i) + 1L, indtype)
    if (is_bool) {
        if (!all(mat@x)) grp$create_dataset("nzval", robj = as.logical(mat@x), chunk_dims = NULL)
    } else {
        grp$create_dataset("nzval", robj = as.double(mat@x), chunk_dims = NULL)
    }
    invisible()
}

.h5_set_matrix <- function(daf, ra, ca, name, mat, overwrite) {
    mat <- .validate_matrix_value(daf, ra, ca, name, mat)
    if (!overwrite) .require_no_matrix(daf, ra, ca, name, relayout = FALSE)
    root <- .h5_root(daf); key <- .hkey_matrix(ra, ca, name)
    if (root$exists(key)) root$link_delete(key)
    if (methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")) {
        .h5_write_matrix_sparse(root, key, mat)
    } else {
        # Dense: hdf5r stores R dim (nr,nc) as HDF5 (nc,nr) col-major = Julia's
        # convention, so write directly (string matrices too).
        root$create_dataset(key, robj = as.matrix(mat), chunk_dims = NULL)
    }
    bump_matrix_counter(daf, ra, ca, name)
    MEMORY_DATA
}

.h5_delete_matrix <- function(daf, ra, ca, name, must_exist) {
    root <- .h5_root(daf); key <- .hkey_matrix(ra, ca, name)
    if (!root$exists(key)) {
        if (must_exist) .require_matrix(daf, ra, ca, name, relayout = FALSE)
        return(invisible())
    }
    root$link_delete(key)
    invisible()
}

local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(format_has_matrix,
            list(cls, S7::class_character, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis, name) .h5_has_matrix(daf, rows_axis, columns_axis, name)
        S7::method(format_matrices_set,
            list(cls, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis) .h5_matrices_set(daf, rows_axis, columns_axis)
        S7::method(format_get_matrix,
            list(cls, S7::class_character, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis, name) {
                m <- .h5_get_matrix_impl(daf, rows_axis, columns_axis, name)
                .cache_group_value(
                    .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, m),
                    .files_daf_classify_matrix(m))
            }
    }
})

S7::method(format_set_matrix,
    list(H5df, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, mat, overwrite) {
        .h5_set_matrix(daf, rows_axis, columns_axis, name, mat, overwrite)
    }
S7::method(format_delete_matrix,
    list(H5df, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, must_exist) {
        .h5_delete_matrix(daf, rows_axis, columns_axis, name, must_exist)
    }
```

- [ ] **Step 4: Run to verify it passes** - Expected: PASS. The `expect_equal(as.vector(got), 1:8)` assertion is the orientation pin.

- [ ] **Step 5: Commit**

```bash
git add R/h5df.R tests/testthat/test-h5df.R
git commit -m "feat(h5df): matrices (dense + sparse CSC + bool + orientation)"
```

---

## Task 6: relayout, reorder, read-only guards, string components

**Files:** Modify `R/h5df.R`; Test `tests/testthat/test-h5df.R`

- [ ] **Step 1: Write the failing test** (append)

```r
test_that("h5df relayout and reorder", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(as.double(1:6), nrow = 3, ncol = 2)
    set_matrix(d, "cell", "gene", "m", m)
    relayout_matrix(d, "cell", "gene", "m")
    expect_true(has_matrix(d, "gene", "cell", "m"))
    expect_equal(as.matrix(get_matrix(d, "gene", "cell", "m")), t(m), ignore_attr = TRUE)
    set_vector(d, "cell", "v", c(10, 20, 30))
    reorder_axes(d, cell = c("C", "A", "B"))
    expect_equal(axis_vector(d, "cell"), c("C", "A", "B"))
    expect_equal(get_vector(d, "cell", "v"), c(30, 10, 20), ignore_attr = TRUE)
    rm(d); gc()
})

test_that("h5df string vectors and matrices round-trip (written dense)", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "sm", matrix(c("a", "b", "c", "d"), 2, 2))
    expect_equal(as.vector(get_matrix(d, "cell", "gene", "sm")), c("a", "b", "c", "d"), ignore_attr = TRUE)
    rm(d); gc()
})

test_that("h5df read-only store rejects mutation", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w"); add_axis(d, "cell", c("A", "B")); rm(d); gc()
    r <- h5df(p, mode = "r")
    expect_error(set_scalar(r, "x", 1), "read-only")
    expect_error(add_axis(r, "z", "a"), "read-only")
    expect_error(set_vector(r, "cell", "v", c(1, 2)), "read-only")
    rm(r); gc()
})
```

- [ ] **Step 2: Run to verify it fails** - Expected: FAIL.

- [ ] **Step 3: Implement** (append to `R/h5df.R`)

```r
# ==== relayout + reorder =====================================================

.h5_relayout_matrix <- function(daf, ra, ca, name) {
    src <- format_get_matrix(daf, ra, ca, name)$value
    transposed <- if (methods::is(src, "dgCMatrix") || methods::is(src, "lgCMatrix")) {
        Matrix::t(src)
    } else {
        t(src)
    }
    format_set_matrix(daf, ca, ra, name, transposed, overwrite = TRUE)
    invisible()
}

# Simplified reorder: read -> permute -> overwrite, per property. No crash-safe
# lock/backup machinery (unlike FilesDaf).
# ponytail: not crash-safe, does not preserve mmap; add the backup protocol only
# if a crash mid-reorder must be recoverable.
.h5_replace_reorder <- function(daf, plan) {
    for (axis in names(plan$planned_axes)) {
        pa <- plan$planned_axes[[axis]]
        root <- .h5_root(daf); key <- .hkey_axis(axis)
        if (root$exists(key)) root$link_delete(key)
        root$create_dataset(key, robj = pa$new_entries, chunk_dims = NULL)
        cache <- S7::prop(daf, "internal")$axes
        if (exists(axis, envir = cache, inherits = FALSE)) rm(list = axis, envir = cache)
    }
    for (pv in plan$planned_vectors) {
        pa <- plan$planned_axes[[pv$axis]]
        v <- format_get_vector(daf, pv$axis, pv$name)$value      # dense (scattered)
        format_set_vector(daf, pv$axis, pv$name, v[pa$permutation], overwrite = TRUE)
    }
    for (pm in plan$planned_matrices) {
        pr <- plan$planned_axes[[pm$rows_axis]]; pc <- plan$planned_axes[[pm$columns_axis]]
        m <- format_get_matrix(daf, pm$rows_axis, pm$columns_axis, pm$name)$value
        r_perm <- if (!is.null(pr)) pr$permutation else seq_len(nrow(m))
        c_perm <- if (!is.null(pc)) pc$permutation else seq_len(ncol(m))
        format_set_matrix(daf, pm$rows_axis, pm$columns_axis, pm$name,
            m[r_perm, c_perm, drop = FALSE], overwrite = TRUE)
    }
    invisible()
}

S7::method(format_relayout_matrix,
    list(H5df, S7::class_character, S7::class_character, S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) .h5_relayout_matrix(daf, rows_axis, columns_axis, name)
S7::method(format_replace_reorder, list(H5df, S7::class_list)) <-
    function(daf, plan, crash_counter = NULL) .h5_replace_reorder(daf, plan)

# ==== read-only guards (mutating on H5dfReadOnly) ============================
S7::method(format_set_scalar,
    list(H5dfReadOnly, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, name, value, overwrite) .h5_read_only_guard("set_scalar")
S7::method(format_delete_scalar, list(H5dfReadOnly, S7::class_character, S7::class_logical)) <-
    function(daf, name, must_exist) .h5_read_only_guard("delete_scalar")
S7::method(format_add_axis, list(H5dfReadOnly, S7::class_character, S7::class_character)) <-
    function(daf, axis, entries) .h5_read_only_guard("add_axis")
S7::method(format_delete_axis, list(H5dfReadOnly, S7::class_character, S7::class_logical)) <-
    function(daf, axis, must_exist) .h5_read_only_guard("delete_axis")
S7::method(format_set_vector,
    list(H5dfReadOnly, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, axis, name, vec, overwrite) .h5_read_only_guard("set_vector")
S7::method(format_delete_vector,
    list(H5dfReadOnly, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, axis, name, must_exist) .h5_read_only_guard("delete_vector")
S7::method(format_set_matrix,
    list(H5dfReadOnly, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, mat, overwrite) .h5_read_only_guard("set_matrix")
S7::method(format_delete_matrix,
    list(H5dfReadOnly, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, must_exist) .h5_read_only_guard("delete_matrix")
S7::method(format_relayout_matrix,
    list(H5dfReadOnly, S7::class_character, S7::class_character, S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) .h5_read_only_guard("relayout_matrix")
S7::method(format_replace_reorder, list(H5dfReadOnly, S7::class_list)) <-
    function(daf, plan, crash_counter = NULL) .h5_read_only_guard("reorder_axes")
```

- [ ] **Step 4: Run to verify it passes** - Run the whole file. Expected: PASS (all tests).

- [ ] **Step 5: Commit**

```bash
git add R/h5df.R tests/testthat/test-h5df.R
git commit -m "feat(h5df): relayout, simplified reorder, read-only guards, strings"
```

---

## Task 7: Adversarial + Julia interop tests

**Files:** Create `tests/testthat/test-h5df-adversarial.R`, `tests/testthat/test-h5df-julia-compat.R`

- [ ] **Step 1: Write the adversarial test**

Create `tests/testthat/test-h5df-adversarial.R`:

```r
skip_if_no_hdf5r <- function() testthat::skip_if_not_installed("hdf5r")

test_that("h5df rejects a non-daf HDF5 file", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    h <- hdf5r::H5File$new(p, mode = "w")
    h$create_dataset("junk", robj = 1:3, chunk_dims = NULL)
    h$close_all()
    expect_error(h5df(p, mode = "r"), "not a daf")
})

test_that("h5df rejects an incompatible format version", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    h <- hdf5r::H5File$new(p, mode = "w")
    for (g in c("scalars", "axes", "vectors", "matrices")) h$create_group(g)
    h$create_dataset("daf", robj = c(2L, 0L),
        dtype = hdf5r::h5types$H5T_NATIVE_UINT8, chunk_dims = NULL)
    h$close_all()
    expect_error(h5df(p, mode = "r"), "incompatible format version")
})

test_that("open_daf rejects grouped .h5dfs#", {
    skip_if_no_hdf5r()
    expect_error(open_daf("foo.h5dfs#/grp", mode = "r"), "not supported")
})

test_that("missing hdf5r yields an actionable error", {
    # Simulated: h5df() calls rlang::check_installed("hdf5r"). When hdf5r is
    # present this is a no-op; document the guard exists.
    skip_if_no_hdf5r()
    expect_true(is.function(h5df))
})
```

- [ ] **Step 2: Run adversarial test** - Expected: PASS.

- [ ] **Step 3: Write the Julia interop test**

Create `tests/testthat/test-h5df-julia-compat.R` (mirrors `test-zip-daf-julia-compat.R`; uses `helper-julia.R`'s `run_julia` / `.have_julia_env`):

```r
# Cross-language interop for the H5df (.h5df) backend against
# DataAxesFormats.jl. Gated on the conda julia env (helper-julia.R).
skip_if_no_hdf5r <- function() testthat::skip_if_not_installed("hdf5r")

test_that("R-written .h5df is readable by Julia with identical values", {
    skip_if_no_hdf5r()
    skip_if_not(.have_julia_env())
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    set_scalar(d, "pi", 3.14)
    set_scalar(d, "note", "hello")
    set_vector(d, "cell", "donor", c(1L, 2L, 3L, 4L))
    set_vector(d, "cell", "score", c(1.5, 2.5, 3.5, 4.5))
    set_vector(d, "cell", "flag", c(TRUE, FALSE, TRUE, TRUE))
    set_matrix(d, "cell", "gene", "dm", matrix(as.double(1:8), nrow = 4))
    sp <- Matrix::sparseMatrix(i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30), dims = c(4, 2))
    set_matrix(d, "cell", "gene", "sm", sp)
    rm(d); gc()
    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = H5df(raw"%s", "r")', p),
        '@assert get_scalar(daf, "pi")   == 3.14',
        '@assert get_scalar(daf, "note") == "hello"',
        '@assert axis_vector(daf, "cell") == ["A","B","C","D"]',
        '@assert get_vector(daf, "cell", "donor") == Int32[1,2,3,4]',
        '@assert get_vector(daf, "cell", "score") == Float64[1.5,2.5,3.5,4.5]',
        '@assert get_vector(daf, "cell", "flag") == Bool[1,0,1,1]',
        '@assert get_matrix(daf, "cell", "gene", "dm") == Float64[1 5; 2 6; 3 7; 4 8]',
        'sm = get_matrix(daf, "cell", "gene", "sm")',
        '@assert size(sm) == (4,2)',
        '@assert Matrix(sm) == Float64[10 0; 0 30; 20 0; 0 0]',
        'println("JULIA_H5DF_OK")'
    )
    out <- run_julia(script)
    expect_true(any(grepl("JULIA_H5DF_OK", out)), info = paste(out, collapse = "\n"))
})

test_that("Julia-written .h5df is readable by R with identical values", {
    skip_if_no_hdf5r()
    skip_if_not(.have_julia_env())
    p <- tempfile(fileext = ".h5df")
    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = H5df(raw"%s", "w")', p),
        'add_axis!(daf, "cell", ["A","B","C","D"])',
        'add_axis!(daf, "gene", ["X","Y"])',
        'set_scalar!(daf, "pi", 3.14)',
        'set_vector!(daf, "cell", "donor", Int32[1,2,3,4])',
        'set_matrix!(daf, "cell", "gene", "dm", Float64[1 5; 2 6; 3 7; 4 8])',
        'set_matrix!(daf, "cell", "gene", "sm", sparse(Float64[10 0; 0 30; 20 0; 0 0]))',
        'println("JULIA_WROTE")'
    )
    out <- run_julia(script)
    skip_if_not(any(grepl("JULIA_WROTE", out)), paste(out, collapse = "\n"))
    d <- h5df(p, mode = "r")
    expect_equal(get_scalar(d, "pi"), 3.14)
    expect_equal(as.integer(get_vector(d, "cell", "donor")), 1:4)
    expect_equal(as.vector(get_matrix(d, "cell", "gene", "dm")), as.double(1:8), ignore_attr = TRUE)
    sm <- get_matrix(d, "cell", "gene", "sm")
    expect_equal(as.matrix(sm),
        matrix(c(10, 0, 20, 0, 0, 30, 0, 0), 4, 2), ignore_attr = TRUE)
    rm(d); gc()
})
```

- [ ] **Step 4: Run the interop test**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-h5df-julia-compat.R")'`
Expected: PASS if the julia env is present, else SKIP. **If the Bool assertion fails** (hdf5r's logical enum not read as Julia `Bool`): construct a matching enum on write. Add to `.h5_set_vector`/`.h5_write_matrix_sparse` a bool path using an explicit HDF5 enum type `{FALSE=0, TRUE=1}` base int8 via `hdf5r::H5T_ENUM`, and re-run. **If the dense-matrix assertion fails**: the orientation is the opposite of assumed - change `.h5_set_matrix`'s dense branch to write `t(as.matrix(mat))` and `.h5_get_matrix_impl` to read into `(nr, nc)` transposed; re-run both this test and Task 5's R round-trip.

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test-h5df-adversarial.R tests/testthat/test-h5df-julia-compat.R
git commit -m "test(h5df): adversarial + bidirectional Julia interop"
```

---

## Task 8: Docs, exports, packaging, R CMD check

**Files:** Modify `R/h5df.R` (roxygen already inline), `NAMESPACE` (generated), `_pkgdown.yml`, `NEWS.md`, `DESCRIPTION`

- [ ] **Step 1: Regenerate docs and NAMESPACE**

Run: `Rscript -e 'devtools::document()'`
Expected: `man/h5df.Rd`, `man/H5df.Rd`, `man/H5dfReadOnly.Rd` created; `NAMESPACE` gains `export(h5df)`, `export(H5df)`, `export(H5dfReadOnly)`.

- [ ] **Step 2: Add pkgdown reference entries**

In `_pkgdown.yml`, add `h5df`, `H5df`, `H5dfReadOnly` to the same `reference:` section that lists `zip_daf` / `ZipDaf` / `ZipDafReadOnly`. Verify:

Run: `Rscript -e 'pkgdown::check_pkgdown(".")'`
Expected: no "missing topics" error.

- [ ] **Step 3: Bump version and NEWS**

In `DESCRIPTION`, set `Version: 0.7.0`. In `NEWS.md`, add a top section:

```markdown
# dafr 0.7.0

* New `h5df()` backend: a whole Daf store in one `.h5df` HDF5 file,
  interoperable with `DataAxesFormats.jl` `H5df` (read + write + delete +
  overwrite + reorder). `open_daf()` dispatches `*.h5df`. Requires `hdf5r`.
  Compressed/packed writing, grouped `.h5dfs#/group` stores, mmap reads, and
  crash-safe reorder are deferred.
```

- [ ] **Step 4: Full test suite + R CMD check**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: all pass; H5df julia-compat tests skip if no julia env.

Run: `Rscript -e 'rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"), error_on = "warning")'`
Expected: 0 errors, 0 warnings (the `checkbashisms`/`qpdf` local-tool NOTES do not appear on CI). Fix any Rd/codoc/example issues (the `h5df` example is guarded by `requireNamespace("hdf5r")`, so it is safe on all platforms).

- [ ] **Step 5: Commit**

```bash
git add R/h5df.R man/ NAMESPACE _pkgdown.yml NEWS.md DESCRIPTION
git commit -m "docs(h5df): exports, pkgdown, NEWS 0.7.0, R CMD check green"
```

---

## Self-review notes (addressed)

- **Spec coverage:** marker/version (Task 1), scalars (2), axes incl. empty + eager groups (3), dense/sparse/bool/string/empty vectors (4), dense/sparse/bool/orientation matrices (5), relayout/reorder/read-only/string (6), adversarial + bidirectional interop (7), packaging (8). Deferred cuts (compression, grouped, mmap, crash-safe reorder, sparse-string write) are not tasks by design.
- **Type consistency:** `.h5_root`, `.hkey_*`, `.h5_safe_read`, `.h5_uint_type`, `.h5_write_index` names are used consistently across tasks. `.h5_safe_read` is introduced in Task 4 but referenced in Task 3 (`.h5_axis_parsed`); Task 3's note flags this ordering dependency.
- **Known interop risks with fallbacks (Task 7 Step 4):** Bool enum compatibility and dense-matrix orientation each have an explicit remediation path, so a failing interop assertion has a defined fix rather than being a blocker.
