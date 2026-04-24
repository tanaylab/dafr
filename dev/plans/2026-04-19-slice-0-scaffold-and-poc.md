# Slice 0 — Scaffold, POC, Benchmarks, Bake-off — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a buildable, loadable, test-passable R package skeleton for `dafr` (native-R reimplementation of `DataAxesFormats.jl`), together with the ALTREP mmap proof-of-concept, the benchmark harness, the `cpp11+BLAS` vs `RcppEigen` bake-off, and an upstreamed FilesDaf on-disk spec — the gate that must pass before Slice 1 begins.

**Architecture:** S7 class hierarchy (DafReader / DafReadOnly / DafWriter) with ~40 unexported S7 generics forming the FormatReader/Writer API. C++ kernels via `cpp11` (no Rcpp), BLAS/LAPACK via R's built-in headers, OpenMP inside loops only, ALTREP classes for mmap-backed vectors and `dgCMatrix` slots. Dev workflow uses `devtools::load_all()` and `pkgbuild::compile_dll()` — no install step.

**Tech Stack:** R 4.4+, S7, cpp11, Matrix (`dgCMatrix` / `lgCMatrix`), cli, bit64, BLAS/LAPACK (R built-in), OpenMP (configure-gated), testthat 3, bench, hedgehog. Optional for the bake-off: RcppEigen.

**Where to commit:** Package source → `~/src/dafr-native/` (package repo, branch `main`). Design notes, benchmarks, bake-off results → `~/src/dafr-native/dev/` (separate nested git repo, already ignored by the package repo). When a task says "commit", infer the correct repo from the file paths touched.

---

## File Structure

**Package repo** (`~/src/dafr-native/`):

```
DESCRIPTION
NAMESPACE                        (auto from roxygen)
LICENSE, LICENSE.md, README.md
configure.ac
configure.win
src/
  Makevars.in
  Makevars.win
  openmp_shim.h
  mmap_region.cpp
  mmap_region.h
  altrep_mmap.cpp
  altrep_mmap.h
  init.cpp                       (R_init_dafr symbol registration)
  kernel_eltwise_log_add.cpp     (bake-off kernel)
  kernel_csc_colsums.cpp         (bake-off kernel)
  kernel_csc_to_csr.cpp          (bake-off kernel)
R/
  dafr-package.R                 (package-level roxygen)
  classes.R                      (S7 class definitions)
  format_api.R                   (S7 generics for the format hooks)
  memory_daf.R                   (stub — full in Slice 1)
  files_daf.R                    (stub — full in Slice 2)
  cache.R                        (3-tier cache skeleton, version counters)
  handlers.R                     (inefficient-action handler registry)
  options.R                      (onLoad option setup)
  altrep.R                       (R wrappers around ALTREP constructors)
  mmap.R                         (user-facing mmap_vector / mmap_dgCMatrix)
  utils.R                        (assertions, type coercion)
  zzz.R                          (.onLoad, .onUnload)
inst/
  specs/
    filesdaf-on-disk-spec.md     (extracted from files_format.jl)
  benchmarks/
    workloads.R
    bench.R
  include/                       (exported C++ headers if we ever link to other pkgs — empty for now)
tests/
  testthat.R
  testthat/
    test-classes.R
    test-format-api.R
    test-mmap-region.R
    test-altrep-mmap.R
    test-altrep-dgCMatrix.R
    test-cache.R
    test-handlers.R
    test-options.R
    helper-tempfiles.R
vignettes/                       (empty — added in later slices)
.github/workflows/
  R-CMD-check.yaml
  altrep-sanity.yaml
  bench.yaml
```

**Dev repo** (`~/src/dafr-native/dev/`):

```
specs/
  2026-04-19-native-r-dafr-design.md         (already committed)
  filesdaf-on-disk-spec-draft.md             (pre-upstream working copy)
plans/
  2026-04-19-slice-0-scaffold-and-poc.md     (this file)
benchmarks/
  bake-off-results.md                        (cpp11+BLAS vs RcppEigen numbers)
  slice-0-baseline.csv                       (initial perf baseline)
notes/
  altrep-compat-findings.md                  (what works / what breaks with Matrix/Seurat/scran)
```

---

## Dev workflow reminder (every task)

In-place development, no install step:

```r
# After editing C++ files:
pkgbuild::clean_dll()
pkgbuild::compile_dll(debug = FALSE)

# Load the package into R:
devtools::load_all()

# Style + roxygen:
alutil::sad()

# Run tests:
alutil::tst(parallel = TRUE)
```

When a task says "run the test", use `alutil::tst()` (or `testthat::test_file("tests/testthat/test-X.R")` for targeted runs). When a task says "build", use `pkgbuild::compile_dll(debug = FALSE)`.

---

## Phase A — Package scaffold

### Task A1: Create `DESCRIPTION`

**Files:**
- Create: `~/src/dafr-native/DESCRIPTION`

- [ ] **Step 1: Write DESCRIPTION**

```
Package: dafr
Title: Multi-Dimensional Data Along Arbitrary Axes
Version: 0.0.0.9000
Authors@R: c(
    person("Aviezer", "Lifshitz", , "aviezer.lifshitz@weizmann.ac.il",
           role = c("aut", "cre"),
           comment = c(ORCID = "0000-0002-8458-9507")),
    person("Oren", "Ben-Kiki", , "oren@ben-kiki.org", role = "aut"),
    person("Weizmann Institute of Science", role = "cph")
  )
Description: A native R implementation of the DataAxesFormats (DAF) data
    model for multi-dimensional data along arbitrary axes. Provides the
    MemoryDaf and FilesDaf storage backends, a query DSL, views, chains,
    contracts, computations, adapters, and operations. Designed for
    efficiency on large single-cell datasets via memory-mapped file
    access (ALTREP), hand-tuned C++ kernels, and optional OpenMP
    parallelism.
License: MIT + file LICENSE
URL: https://github.com/tanaylab/dafr
BugReports: https://github.com/tanaylab/dafr/issues
Depends:
    R (>= 4.4.0)
Imports:
    S7,
    Matrix,
    cli,
    bit64,
    methods
LinkingTo:
    cpp11
Suggests:
    bench,
    hedgehog,
    knitr,
    rmarkdown,
    scran,
    Seurat,
    SingleCellExperiment,
    testthat (>= 3.0.0),
    withr
SystemRequirements: C++17
Config/testthat/edition: 3
Encoding: UTF-8
Language: en-US
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.3
```

- [ ] **Step 2: Verify**

Run: `Rscript -e 'read.dcf("~/src/dafr-native/DESCRIPTION")'`
Expected: no error; prints a 1-row character matrix whose columns include `Package: dafr`.

- [ ] **Step 3: Commit (package repo)**

```bash
cd ~/src/dafr-native
git add DESCRIPTION
git commit -m "chore: DESCRIPTION for native dafr"
```

### Task A2: Create `LICENSE`, `LICENSE.md`, `README.md`

**Files:**
- Create: `~/src/dafr-native/LICENSE`
- Create: `~/src/dafr-native/LICENSE.md`
- Create: `~/src/dafr-native/README.md`

- [ ] **Step 1: Write `LICENSE`** (two lines, as R conventions require)

```
YEAR: 2026
COPYRIGHT HOLDER: Weizmann Institute of Science
```

- [ ] **Step 2: Write `LICENSE.md`** (MIT text)

```
# MIT License

Copyright (c) 2026 Weizmann Institute of Science

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
```

- [ ] **Step 3: Write `README.md`** (minimal, expanded in later slices)

```markdown
# dafr

Native R implementation of the DataAxesFormats (DAF) data model.

**Status:** pre-alpha, under active development. Not yet installable.

For the existing Julia-facade version, see `DafJuliaWrapper`.

## Development

```r
pkgbuild::clean_dll(); pkgbuild::compile_dll(debug = FALSE)
devtools::load_all()
alutil::tst(parallel = TRUE)
```
```

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add LICENSE LICENSE.md README.md
git commit -m "chore: LICENSE and README"
```

### Task A3: Create `src/Makevars` and `src/Makevars.win`

**Rationale:** Autotools isn't on the target dev machine and the two things a configure would buy us (mmap detection, OpenMP flag resolution) are trivially handled without it: mmap splits cleanly along POSIX vs Windows (use `#if defined(_WIN32)` in C++), and R already exposes `$(SHLIB_OPENMP_CXXFLAGS)` in its Makeconf. Ship static Makevars.

**Files:**
- Create: `~/src/dafr-native/src/Makevars`
- Create: `~/src/dafr-native/src/Makevars.win`

- [ ] **Step 1: Write `src/Makevars`** (POSIX path)

```make
PKG_CXXFLAGS = -DDAFR_HAVE_MMAP=1 -DDAFR_HAVE_OPENMP=1 $(SHLIB_OPENMP_CXXFLAGS)
PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
CXX_STD = CXX17
```

- [ ] **Step 2: Write `src/Makevars.win`** (Windows — mmap absent)

```make
PKG_CXXFLAGS = -DDAFR_HAVE_MMAP=0 -DDAFR_HAVE_OPENMP=1 $(SHLIB_OPENMP_CXXFLAGS)
PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
CXX_STD = CXX17
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add src/Makevars src/Makevars.win
git commit -m "chore: Makevars (static, no autotools) for OpenMP + mmap flags"
```

### Task A4: Create `.Rbuildignore` and `src/openmp_shim.h`

**Files:**
- Create: `~/src/dafr-native/.Rbuildignore`
- Create: `~/src/dafr-native/src/openmp_shim.h`

- [ ] **Step 1: Write `.Rbuildignore`**

```
^dev$
^\.github$
^dev/.*
^.*\.Rproj$
^\.Rproj\.user$
^README\.Rmd$
^cran-comments\.md$
^CRAN-SUBMISSION$
^_pkgdown\.yml$
^docs$
^pkgdown$
^LICENSE\.md$
^config\.log$
^config\.status$
^autom4te\.cache$
```

- [ ] **Step 2: Write `src/openmp_shim.h`**

```cpp
#ifndef DAFR_OPENMP_SHIM_HPP
#define DAFR_OPENMP_SHIM_HPP

#if defined(DAFR_HAVE_OPENMP) && DAFR_HAVE_OPENMP
  #include <omp.h>
  #define DAFR_PARALLEL_FOR(cond) _Pragma("omp parallel for if(cond) schedule(static)")
  #define DAFR_OMP_THREADS() omp_get_max_threads()
#else
  #define DAFR_PARALLEL_FOR(cond)
  #define DAFR_OMP_THREADS() 1
#endif

#endif
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add .Rbuildignore src/openmp_shim.h
git commit -m "chore: Rbuildignore and OpenMP shim"
```

### Task A5: Bootstrap package skeleton with roxygen and testthat

**Files:**
- Create: `~/src/dafr-native/R/dafr-package.R`
- Create: `~/src/dafr-native/R/zzz.R`
- Create: `~/src/dafr-native/tests/testthat.R`
- Create: `~/src/dafr-native/tests/testthat/helper-tempfiles.R`

- [ ] **Step 1: Write `R/dafr-package.R`**

```r
#' @keywords internal
#' @useDynLib dafr, .registration = TRUE
#' @importFrom methods new
#' @importFrom S7 new_class method class_character class_integer class_logical
#'   class_numeric class_list class_environment new_generic
"_PACKAGE"
```

- [ ] **Step 2: Write `R/zzz.R`**

```r
.onLoad <- function(libname, pkgname) {
  set_default_options()
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("dafr", libpath)
}
```

- [ ] **Step 3: Write `tests/testthat.R`**

```r
library(testthat)
library(dafr)

test_check("dafr")
```

- [ ] **Step 4: Write `tests/testthat/helper-tempfiles.R`**

```r
new_tempfile <- function(ext = "bin") {
  f <- tempfile(fileext = paste0(".", ext))
  withr::defer_parent(unlink(f, force = TRUE))
  f
}

new_tempdir <- function() {
  d <- tempfile()
  dir.create(d)
  withr::defer_parent(unlink(d, recursive = TRUE, force = TRUE))
  d
}
```

- [ ] **Step 5: Generate NAMESPACE via roxygen**

Run:
```r
roxygen2::roxygenise("~/src/dafr-native")
```
Expected: `NAMESPACE` file created containing `useDynLib(dafr, .registration = TRUE)`.

- [ ] **Step 6: Commit**

```bash
cd ~/src/dafr-native
git add R/dafr-package.R R/zzz.R tests/testthat.R tests/testthat/helper-tempfiles.R NAMESPACE
git commit -m "chore: package skeleton (roxygen, testthat, onLoad)"
```

### Task A6: Stub `R/options.R` and `R/handlers.R`

**Files:**
- Create: `~/src/dafr-native/R/options.R`
- Create: `~/src/dafr-native/R/handlers.R`
- Create: `~/src/dafr-native/tests/testthat/test-options.R`
- Create: `~/src/dafr-native/tests/testthat/test-handlers.R`

- [ ] **Step 1: Write `R/options.R`**

```r
.dafr_default_options <- list(
  dafr.cache.memory_mb = 1024L,
  dafr.cache.disable   = FALSE,
  dafr.cache.stats     = FALSE,
  dafr.mmap            = TRUE,
  dafr.omp_threshold   = 10000L,
  dafr.inefficient     = "warn"  # one of "ignore", "warn", "error"
)

set_default_options <- function() {
  current <- options()
  to_set  <- .dafr_default_options[setdiff(names(.dafr_default_options), names(current))]
  if (length(to_set)) options(to_set)
  invisible()
}

#' Get a dafr option with a typed default.
#' @noRd
dafr_opt <- function(name) {
  stopifnot(name %in% names(.dafr_default_options))
  getOption(name, .dafr_default_options[[name]])
}
```

- [ ] **Step 2: Write `tests/testthat/test-options.R`**

```r
test_that("default options are set on load", {
  expect_equal(dafr_opt("dafr.cache.memory_mb"), 1024L)
  expect_equal(dafr_opt("dafr.inefficient"), "warn")
  expect_true(dafr_opt("dafr.mmap"))
})

test_that("overridden options flow through dafr_opt", {
  withr::with_options(list(dafr.cache.memory_mb = 512L), {
    expect_equal(dafr_opt("dafr.cache.memory_mb"), 512L)
  })
})

test_that("dafr_opt rejects unknown names", {
  expect_error(dafr_opt("dafr.bogus"), "name %in% names")
})
```

- [ ] **Step 3: Write `R/handlers.R`**

```r
.dafr_handlers <- new.env(parent = emptyenv())

#' Register a handler for an action category.
#'
#' Built-in categories: `"inefficient"`. Action is one of `"ignore"`, `"warn"`,
#' `"error"`, or a function `function(message, ...)` invoked by emit_action().
#' @export
register_dafr_handler <- function(category, action) {
  stopifnot(is.character(category), length(category) == 1L)
  if (!(is.character(action) || is.function(action))) {
    stop("action must be a string or a function")
  }
  if (is.character(action) && !action %in% c("ignore", "warn", "error")) {
    stop('action must be one of "ignore", "warn", "error", or a function')
  }
  assign(category, action, envir = .dafr_handlers)
  invisible()
}

#' Emit an action in a category; dispatches per registered handler or option.
#' @noRd
emit_action <- function(category, message) {
  handler <- if (exists(category, envir = .dafr_handlers, inherits = FALSE)) {
    get(category, envir = .dafr_handlers)
  } else {
    dafr_opt(paste0("dafr.", category))
  }
  if (is.function(handler)) {
    handler(message)
  } else {
    switch(handler,
      ignore = invisible(NULL),
      warn   = warning(message, call. = FALSE),
      error  = stop(message, call. = FALSE),
      stop("unknown handler action: ", handler)
    )
  }
}
```

- [ ] **Step 4: Write `tests/testthat/test-handlers.R`**

```r
test_that("default inefficient handler warns", {
  withr::with_options(list(dafr.inefficient = "warn"), {
    expect_warning(emit_action("inefficient", "slow path"), "slow path")
  })
})

test_that("registered function handler receives the message", {
  register_dafr_handler("inefficient", function(msg) {
    stop("custom: ", msg)
  })
  on.exit(register_dafr_handler("inefficient", "warn"))
  expect_error(emit_action("inefficient", "x"), "custom: x")
})

test_that("ignore handler is silent", {
  register_dafr_handler("inefficient", "ignore")
  on.exit(register_dafr_handler("inefficient", "warn"))
  expect_silent(emit_action("inefficient", "quiet"))
})

test_that("error handler raises", {
  register_dafr_handler("inefficient", "error")
  on.exit(register_dafr_handler("inefficient", "warn"))
  expect_error(emit_action("inefficient", "boom"), "boom")
})
```

- [ ] **Step 5: Re-generate NAMESPACE**

```r
roxygen2::roxygenise("~/src/dafr-native")
```
Expected: NAMESPACE now exports `register_dafr_handler`.

- [ ] **Step 6: Load + test**

```r
devtools::load_all("~/src/dafr-native")
testthat::test_dir("~/src/dafr-native/tests/testthat", filter = "options|handlers")
```
Expected: all tests pass.

- [ ] **Step 7: Commit**

```bash
cd ~/src/dafr-native
git add R/options.R R/handlers.R NAMESPACE tests/testthat/test-options.R tests/testthat/test-handlers.R
git commit -m "feat: option defaults and handler registration"
```

---

## Phase B — S7 class skeleton and format API

### Task B1: Define the S7 class hierarchy

**Files:**
- Create: `~/src/dafr-native/R/classes.R`
- Create: `~/src/dafr-native/tests/testthat/test-classes.R`

- [ ] **Step 1: Write `R/classes.R`**

```r
#' Base abstract reader class.
#' @export
DafReader <- S7::new_class(
  name = "DafReader",
  package = "dafr",
  abstract = TRUE,
  properties = list(
    name                   = S7::class_character,
    internal               = S7::class_environment,
    cache                  = S7::class_environment,
    axis_version_counter   = S7::class_environment,
    vector_version_counter = S7::class_environment,
    matrix_version_counter = S7::class_environment
  )
)

#' Abstract read-only reader class.
#' @export
DafReadOnly <- S7::new_class(
  name = "DafReadOnly",
  package = "dafr",
  abstract = TRUE,
  parent = DafReader
)

#' Abstract writer class.
#' @export
DafWriter <- S7::new_class(
  name = "DafWriter",
  package = "dafr",
  abstract = TRUE,
  parent = DafReader
)

new_internal_env <- function() {
  e <- new.env(parent = emptyenv())
  e$closed <- FALSE
  e
}

new_cache_env <- function() {
  e <- new.env(parent = emptyenv())
  e$mapped <- new.env(parent = emptyenv())
  e$memory <- new.env(parent = emptyenv())
  e$query  <- new.env(parent = emptyenv())
  e
}

new_counter_env <- function() new.env(parent = emptyenv())
```

- [ ] **Step 2: Write `tests/testthat/test-classes.R`**

```r
test_that("DafReader is abstract", {
  expect_error(DafReader(name = "x"), "abstract")
})

test_that("class hierarchy uses S7 inheritance", {
  expect_true(S7::class_inherits(DafReadOnly, DafReader))
  expect_true(S7::class_inherits(DafWriter,  DafReader))
})
```

- [ ] **Step 3: Load + test**

```r
devtools::load_all("~/src/dafr-native")
testthat::test_file("~/src/dafr-native/tests/testthat/test-classes.R")
```
Expected: both tests pass.

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add R/classes.R tests/testthat/test-classes.R
git commit -m "feat: S7 abstract class hierarchy (DafReader/DafReadOnly/DafWriter)"
```

### Task B2: Declare the ~40 format API S7 generics

**Files:**
- Create: `~/src/dafr-native/R/format_api.R`
- Create: `~/src/dafr-native/tests/testthat/test-format-api.R`

- [ ] **Step 1: Write `R/format_api.R`** — every generic declared with its Julia counterpart named in a comment so porters have a one-glance reference.

```r
# Format API — unexported S7 generics mirroring Julia Formats module.
# A backend implements these; user-facing Readers/Writers call through.
#
# Naming: format_<verb>[_<object>]. Each arg order matches Julia's module.

# ---- Scalars (Julia: Formats.format_has_scalar, ..) ----
format_has_scalar    <- S7::new_generic("format_has_scalar",    c("daf", "name"))
format_get_scalar    <- S7::new_generic("format_get_scalar",    c("daf", "name"))
format_set_scalar    <- S7::new_generic("format_set_scalar",    c("daf", "name", "value", "overwrite"))
format_delete_scalar <- S7::new_generic("format_delete_scalar", c("daf", "name", "must_exist"))
format_scalars_set   <- S7::new_generic("format_scalars_set",   "daf")

# ---- Axes ----
format_has_axis      <- S7::new_generic("format_has_axis",      c("daf", "axis"))
format_add_axis      <- S7::new_generic("format_add_axis",      c("daf", "axis", "entries"))
format_delete_axis   <- S7::new_generic("format_delete_axis",   c("daf", "axis", "must_exist"))
format_axes_set      <- S7::new_generic("format_axes_set",      "daf")
format_axis_array    <- S7::new_generic("format_axis_array",    c("daf", "axis"))
format_axis_length   <- S7::new_generic("format_axis_length",   c("daf", "axis"))
format_axis_dict     <- S7::new_generic("format_axis_dict",     c("daf", "axis"))

# ---- Vectors (per-axis namespace) ----
format_has_vector    <- S7::new_generic("format_has_vector",    c("daf", "axis", "name"))
format_get_vector    <- S7::new_generic("format_get_vector",    c("daf", "axis", "name"))
format_set_vector    <- S7::new_generic("format_set_vector",    c("daf", "axis", "name", "vec", "overwrite"))
format_delete_vector <- S7::new_generic("format_delete_vector", c("daf", "axis", "name", "must_exist"))
format_vectors_set   <- S7::new_generic("format_vectors_set",   c("daf", "axis"))

# ---- Matrices (ordered-pair-of-axes namespace, CSC canonical) ----
format_has_matrix      <- S7::new_generic("format_has_matrix",
  c("daf", "rows_axis", "columns_axis", "name"))
format_get_matrix      <- S7::new_generic("format_get_matrix",
  c("daf", "rows_axis", "columns_axis", "name"))
format_set_matrix      <- S7::new_generic("format_set_matrix",
  c("daf", "rows_axis", "columns_axis", "name", "mat", "overwrite"))
format_delete_matrix   <- S7::new_generic("format_delete_matrix",
  c("daf", "rows_axis", "columns_axis", "name", "must_exist"))
format_matrices_set    <- S7::new_generic("format_matrices_set",
  c("daf", "rows_axis", "columns_axis"))
format_relayout_matrix <- S7::new_generic("format_relayout_matrix",
  c("daf", "rows_axis", "columns_axis", "name"))
```

- [ ] **Step 2: Write `tests/testthat/test-format-api.R`**

```r
test_that("all format_* generics exist with expected dispatch arity", {
  expected <- list(
    format_has_scalar    = 2L,
    format_get_scalar    = 2L,
    format_set_scalar    = 4L,
    format_delete_scalar = 3L,
    format_scalars_set   = 1L,
    format_has_axis      = 2L,
    format_add_axis      = 3L,
    format_delete_axis   = 3L,
    format_axes_set      = 1L,
    format_axis_array    = 2L,
    format_axis_length   = 2L,
    format_axis_dict     = 2L,
    format_has_vector    = 3L,
    format_get_vector    = 3L,
    format_set_vector    = 5L,
    format_delete_vector = 4L,
    format_vectors_set   = 2L,
    format_has_matrix      = 4L,
    format_get_matrix      = 4L,
    format_set_matrix      = 6L,
    format_delete_matrix   = 5L,
    format_matrices_set    = 3L,
    format_relayout_matrix = 4L
  )
  for (nm in names(expected)) {
    gen <- get(nm, envir = asNamespace("dafr"))
    expect_true(S7::S7_inherits(gen, S7::S7_generic), info = nm)
    expect_equal(length(S7::prop(gen, "dispatch_args")), expected[[nm]], info = nm)
  }
})
```

- [ ] **Step 3: Load + test**

```r
devtools::load_all("~/src/dafr-native")
testthat::test_file("~/src/dafr-native/tests/testthat/test-format-api.R")
```
Expected: all generics exist with correct arity.

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add R/format_api.R tests/testthat/test-format-api.R
git commit -m "feat: unexported S7 generics for FormatReader/Writer hooks"
```

### Task B3: Stub `R/cache.R`

**Files:**
- Create: `~/src/dafr-native/R/cache.R`
- Create: `~/src/dafr-native/tests/testthat/test-cache.R`

- [ ] **Step 1: Write `R/cache.R`** (minimal — LRU logic comes in Slice 1; this is the skeleton)

```r
cache_key_scalar <- function(name)      paste0("scalar:", name)
cache_key_axis   <- function(axis)      paste0("axis:", axis)
cache_key_vector <- function(axis, name) paste0("vector:", axis, ":", name)
cache_key_matrix <- function(rows_axis, cols_axis, name) {
  paste0("matrix:", rows_axis, ":", cols_axis, ":", name)
}
cache_key_query  <- function(canon)     paste0("query:", canon)

# Lookup and store within a specific tier.
cache_get <- function(cache_env, tier, key) {
  bucket <- cache_env[[tier]]
  if (exists(key, envir = bucket, inherits = FALSE)) {
    return(get(key, envir = bucket, inherits = FALSE))
  }
  NULL
}
cache_put <- function(cache_env, tier, key, value) {
  bucket <- cache_env[[tier]]
  assign(key, value, envir = bucket)
  invisible()
}
cache_remove <- function(cache_env, tier, key) {
  bucket <- cache_env[[tier]]
  if (exists(key, envir = bucket, inherits = FALSE)) {
    rm(list = key, envir = bucket)
  }
  invisible()
}

#' Empty caches.
#' @param daf A DafReader / DafWriter.
#' @param group One or more of `"mapped"`, `"memory"`, `"query"`; default all.
#' @export
empty_cache <- function(daf, group = c("mapped", "memory", "query")) {
  group <- match.arg(group, choices = c("mapped", "memory", "query"), several.ok = TRUE)
  cache_env <- S7::prop(daf, "cache")
  for (tier in group) {
    bucket <- cache_env[[tier]]
    rm(list = ls(bucket, all.names = TRUE), envir = bucket)
  }
  invisible(daf)
}

# ---- Version counters ----
bump_axis_counter <- function(daf, axis) {
  counters <- S7::prop(daf, "axis_version_counter")
  counters[[axis]] <- (counters[[axis]] %||% 0L) + 1L
  invisible()
}
bump_vector_counter <- function(daf, axis, name) {
  counters <- S7::prop(daf, "vector_version_counter")
  key <- paste0(axis, ":", name)
  counters[[key]] <- (counters[[key]] %||% 0L) + 1L
  invisible()
}
bump_matrix_counter <- function(daf, rows_axis, cols_axis, name) {
  counters <- S7::prop(daf, "matrix_version_counter")
  key <- paste0(rows_axis, ":", cols_axis, ":", name)
  counters[[key]] <- (counters[[key]] %||% 0L) + 1L
  invisible()
}
```

- [ ] **Step 2: Write `R/utils.R`** (carries `%||%`)

```r
`%||%` <- function(a, b) if (is.null(a)) b else a
```

- [ ] **Step 3: Write `tests/testthat/test-cache.R`**

```r
fake_daf_with_cache <- function() {
  list(cache = new_cache_env(),
       axis_version_counter   = new_counter_env(),
       vector_version_counter = new_counter_env(),
       matrix_version_counter = new_counter_env())
}

test_that("cache keys are stable", {
  expect_equal(cache_key_vector("cell", "n_counts"), "vector:cell:n_counts")
  expect_equal(cache_key_matrix("cell", "gene", "UMIs"), "matrix:cell:gene:UMIs")
})

test_that("cache_put/get/remove round-trip through env tiers", {
  d <- fake_daf_with_cache()
  cache_put(d$cache, "memory", "k", 42L)
  expect_equal(cache_get(d$cache, "memory", "k"), 42L)
  expect_null(cache_get(d$cache, "mapped", "k"))
  cache_remove(d$cache, "memory", "k")
  expect_null(cache_get(d$cache, "memory", "k"))
})
```

- [ ] **Step 4: Load + test**

```r
devtools::load_all("~/src/dafr-native")
testthat::test_file("~/src/dafr-native/tests/testthat/test-cache.R")
```
Expected: all tests pass.

- [ ] **Step 5: Regenerate NAMESPACE and commit**

```r
roxygen2::roxygenise("~/src/dafr-native")
```

```bash
cd ~/src/dafr-native
git add R/cache.R R/utils.R NAMESPACE tests/testthat/test-cache.R
git commit -m "feat: cache skeleton (3-tier env, version counters, empty_cache)"
```

---

## Phase C — C++ init and MmapRegion RAII

### Task C1: Write C++ init hook `src/init.cpp`

**Files:**
- Create: `~/src/dafr-native/src/init.cpp`

- [ ] **Step 1: Write init.cpp** (symbol registration placeholder; populated as kernels get exposed to R)

```cpp
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern "C" {

// Kernel declarations will be added as src/*.cpp files register new entry
// points. Each DAFR_REGISTER_FN macro appends a row.

static const R_CallMethodDef CallEntries[] = {
    {nullptr, nullptr, 0}
};

attribute_visible void R_init_dafr(DllInfo *dll) {
    R_registerRoutines(dll, nullptr, CallEntries, nullptr, nullptr);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}

} // extern "C"
```

- [ ] **Step 2: Build**

```r
pkgbuild::clean_dll("~/src/dafr-native")
pkgbuild::compile_dll("~/src/dafr-native", debug = FALSE)
```
Expected: compiles clean.

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add src/init.cpp
git commit -m "chore: C++ init with empty CallEntries table"
```

### Task C2: Implement `MmapRegion` RAII wrapper

**Files:**
- Create: `~/src/dafr-native/src/mmap_region.h`
- Create: `~/src/dafr-native/src/mmap_region.cpp`
- Create: `~/src/dafr-native/tests/testthat/test-mmap-region.R`

- [ ] **Step 1: Write `src/mmap_region.h`**

```cpp
#ifndef DAFR_MMAP_REGION_HPP
#define DAFR_MMAP_REGION_HPP

#include <cstddef>
#include <memory>
#include <string>

namespace dafr {

// RAII wrapper: mmap a file into memory, release via munmap on destruction.
// Read-only by default. Not copyable; shared via std::shared_ptr from callers.
class MmapRegion {
public:
    static std::shared_ptr<MmapRegion> open_readonly(const std::string &path);

    MmapRegion(void *ptr, std::size_t nbytes, int fd, std::string path);
    ~MmapRegion();

    MmapRegion(const MmapRegion&) = delete;
    MmapRegion& operator=(const MmapRegion&) = delete;

    const void* data() const { return ptr_; }
    std::size_t nbytes() const { return nbytes_; }
    const std::string& path() const { return path_; }

private:
    void *ptr_;
    std::size_t nbytes_;
    int fd_;
    std::string path_;
};

} // namespace dafr

#endif
```

- [ ] **Step 2: Write `src/mmap_region.cpp`**

```cpp
#include "mmap_region.h"
#include "config.h"

#include <R.h>
#include <Rinternals.h>

#include <cerrno>
#include <cstring>
#include <stdexcept>

#if DAFR_HAVE_MMAP
  #include <fcntl.h>
  #include <sys/mman.h>
  #include <sys/stat.h>
  #include <unistd.h>
#endif

namespace dafr {

MmapRegion::MmapRegion(void *ptr, std::size_t nbytes, int fd, std::string path)
    : ptr_(ptr), nbytes_(nbytes), fd_(fd), path_(std::move(path)) {}

MmapRegion::~MmapRegion() {
#if DAFR_HAVE_MMAP
    if (ptr_ != nullptr && ptr_ != MAP_FAILED) {
        munmap(ptr_, nbytes_);
    }
    if (fd_ >= 0) {
        close(fd_);
    }
#endif
}

std::shared_ptr<MmapRegion> MmapRegion::open_readonly(const std::string &path) {
#if !DAFR_HAVE_MMAP
    throw std::runtime_error("mmap not available on this platform");
#else
    int fd = ::open(path.c_str(), O_RDONLY);
    if (fd < 0) {
        throw std::runtime_error(
            "failed to open '" + path + "': " + std::strerror(errno));
    }
    struct stat st;
    if (fstat(fd, &st) != 0) {
        int e = errno;
        ::close(fd);
        throw std::runtime_error(
            "fstat '" + path + "': " + std::strerror(e));
    }
    std::size_t nbytes = static_cast<std::size_t>(st.st_size);
    if (nbytes == 0) {
        // Empty file: can't mmap a zero-length region. Treat as empty.
        ::close(fd);
        return std::make_shared<MmapRegion>(nullptr, 0, -1, path);
    }
    void *p = mmap(nullptr, nbytes, PROT_READ, MAP_SHARED, fd, 0);
    if (p == MAP_FAILED) {
        int e = errno;
        ::close(fd);
        throw std::runtime_error(
            "mmap '" + path + "': " + std::strerror(e));
    }
    return std::make_shared<MmapRegion>(p, nbytes, fd, path);
#endif
}

} // namespace dafr
```

- [ ] **Step 3: Verify build**

```r
pkgbuild::clean_dll("~/src/dafr-native")
pkgbuild::compile_dll("~/src/dafr-native", debug = FALSE)
```
Expected: compiles clean, no warnings.

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add src/mmap_region.h src/mmap_region.cpp
git commit -m "feat: MmapRegion RAII wrapper"
```

---

## Phase D — ALTREP mmap POC

### Task D1: ALTREP real (double) class

**Files:**
- Create: `~/src/dafr-native/src/altrep_mmap.h`
- Create: `~/src/dafr-native/src/altrep_mmap.cpp`
- Modify: `~/src/dafr-native/src/init.cpp` (register entry points)

- [ ] **Step 1: Write `src/altrep_mmap.h`**

```cpp
#ifndef DAFR_ALTREP_MMAP_HPP
#define DAFR_ALTREP_MMAP_HPP

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>

#include "mmap_region.h"
#include <memory>

namespace dafr {

void init_altrep_mmap(DllInfo *dll);

// Construct an ALTREP numeric (double) SEXP backed by the given mmap region.
// `length` is the number of doubles (nbytes / sizeof(double)).
SEXP make_mmap_real_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length);

// Int32 variant.
SEXP make_mmap_int_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length);

// Logical variant (stored as int32 per R semantics).
SEXP make_mmap_lgl_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length);

} // namespace dafr

#endif
```

- [ ] **Step 2: Write `src/altrep_mmap.cpp`** — MmapRealAltrep implementation

```cpp
#include "altrep_mmap.h"

#include <R_ext/Altrep.h>
#include <R_ext/Rallocators.h>
#include <R_ext/Rdynload.h>

#include <memory>
#include <cstring>

namespace dafr {

// The ALTREP classes; populated in init_altrep_mmap().
static R_altrep_class_t MmapRealClass;
static R_altrep_class_t MmapIntClass;
static R_altrep_class_t MmapLglClass;

// ---- Common: store shared_ptr<MmapRegion> inside the ALTREP's data1. ----
//
// We wrap the shared_ptr in an EXTPTRSXP and attach a finalizer; the
// EXTPTRSXP becomes data1 on the ALTREP. Dataptr returns the raw region
// pointer. This keeps the MmapRegion alive as long as the R vector is.

static void region_finalizer(SEXP xptr) {
    auto *holder = static_cast<std::shared_ptr<MmapRegion>*>(R_ExternalPtrAddr(xptr));
    if (holder) {
        delete holder;
        R_ClearExternalPtr(xptr);
    }
}

// The xptr carries (region holder, length) so ALTREP objects don't need
// to pre-allocate an N-element dummy vector just to remember their length.
// Length is stored in the xptr's Protected slot as a ScalarReal (R_xlen_t
// fits in double for any realistic R vector).
static SEXP wrap_region(std::shared_ptr<MmapRegion> region, R_xlen_t length) {
    auto *holder = new std::shared_ptr<MmapRegion>(std::move(region));
    SEXP len_sxp = PROTECT(Rf_ScalarReal(static_cast<double>(length)));
    SEXP xptr = PROTECT(R_MakeExternalPtr(holder, R_NilValue, len_sxp));
    R_RegisterCFinalizerEx(xptr, region_finalizer, TRUE);
    UNPROTECT(2);
    return xptr;
}

static std::shared_ptr<MmapRegion> unwrap_region(SEXP data1) {
    auto *holder = static_cast<std::shared_ptr<MmapRegion>*>(R_ExternalPtrAddr(data1));
    if (!holder) Rf_error("mmap region has been released");
    return *holder;
}

static R_xlen_t xptr_length(SEXP xptr) {
    SEXP len_sxp = R_ExternalPtrProtected(xptr);
    return static_cast<R_xlen_t>(REAL(len_sxp)[0]);
}

// ---- Representation invariants ----
//
// data1 is EXTPTRSXP (region holder + length scalar) pre-materialization,
// or R_NilValue post-materialization.
// data2 is R_NilValue pre-materialization, or the materialized R vector
// post-materialization.
// Length comes from the xptr's protected slot when not yet materialized,
// else from XLENGTH of the materialized vector.

// ---- Real (double) class methods ----

static R_xlen_t mmap_real_length(SEXP x) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return XLENGTH(R_altrep_data2(x));
    return xptr_length(d1);
}

static void *mmap_real_dataptr(SEXP x, Rboolean writeable) {
    // Already materialized? Just return its buffer (idempotent, no re-copy).
    if (R_altrep_data1(x) == R_NilValue) {
        return REAL(R_altrep_data2(x));
    }
    if (writeable) {
        // Materialize: copy mmap bytes into a fresh REALSXP, swap in.
        SEXP region_xptr = R_altrep_data1(x);
        auto region = unwrap_region(region_xptr);
        R_xlen_t n = xptr_length(region_xptr);
        SEXP materialized = PROTECT(Rf_allocVector(REALSXP, n));
        if (n > 0) std::memcpy(REAL(materialized), region->data(), n * sizeof(double));
        R_set_altrep_data1(x, R_NilValue);
        R_set_altrep_data2(x, materialized);
        UNPROTECT(1);
        return REAL(materialized);
    }
    SEXP region_xptr = R_altrep_data1(x);
    auto region = unwrap_region(region_xptr);
    return const_cast<void*>(region->data());
}

static const void *mmap_real_dataptr_or_null(SEXP x) {
    SEXP region_xptr = R_altrep_data1(x);
    if (region_xptr == R_NilValue) return DATAPTR_RO(R_altrep_data2(x));
    auto region = unwrap_region(region_xptr);
    return region->data();
}

static double mmap_real_elt(SEXP x, R_xlen_t i) {
    const double *p = static_cast<const double*>(mmap_real_dataptr_or_null(x));
    return p[i];
}

static R_xlen_t mmap_real_get_region(SEXP x, R_xlen_t start, R_xlen_t size, double *buf) {
    const double *p = static_cast<const double*>(mmap_real_dataptr_or_null(x));
    R_xlen_t avail = mmap_real_length(x) - start;
    R_xlen_t n = (size < avail) ? size : avail;
    std::memcpy(buf, p + start, n * sizeof(double));
    return n;
}

static Rboolean mmap_real_inspect(SEXP x, int pre, int deep, int pvec,
                                  void (*inspect_subtree)(SEXP, int, int, int)) {
    Rprintf("dafr::MmapRealAltrep length=%lld\n",
            static_cast<long long>(mmap_real_length(x)));
    return TRUE;
}

// Serialized state: materialize then serialize as regular numeric vector.
static SEXP mmap_real_serialized_state(SEXP x) {
    R_xlen_t n = mmap_real_length(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
    std::memcpy(REAL(out), mmap_real_dataptr_or_null(x), n * sizeof(double));
    UNPROTECT(1);
    return out;
}
static SEXP mmap_real_unserialize(SEXP, SEXP state) { return state; }

static void init_mmap_real(DllInfo *dll) {
    MmapRealClass = R_make_altreal_class("MmapRealAltrep", "dafr", dll);
    R_set_altrep_Length_method(MmapRealClass, mmap_real_length);
    R_set_altrep_Inspect_method(MmapRealClass, mmap_real_inspect);
    R_set_altrep_Serialized_state_method(MmapRealClass, mmap_real_serialized_state);
    R_set_altrep_Unserialize_method(MmapRealClass, mmap_real_unserialize);
    R_set_altvec_Dataptr_method(MmapRealClass, mmap_real_dataptr);
    R_set_altvec_Dataptr_or_null_method(MmapRealClass, mmap_real_dataptr_or_null);
    R_set_altreal_Elt_method(MmapRealClass, mmap_real_elt);
    R_set_altreal_Get_region_method(MmapRealClass, mmap_real_get_region);
}

SEXP make_mmap_real_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length) {
    SEXP region_xptr = PROTECT(wrap_region(std::move(region), length));
    SEXP out = R_new_altrep(MmapRealClass, region_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

// ---- Int (int32) class — identical structure. ----

static R_xlen_t mmap_int_length(SEXP x) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return XLENGTH(R_altrep_data2(x));
    return xptr_length(d1);
}
static int mmap_int_elt(SEXP x, R_xlen_t i) {
    SEXP rx = R_altrep_data1(x);
    if (rx == R_NilValue) return INTEGER(R_altrep_data2(x))[i];
    auto region = unwrap_region(rx);
    return static_cast<const int*>(region->data())[i];
}
static void *mmap_int_dataptr(SEXP x, Rboolean writeable) {
    if (R_altrep_data1(x) == R_NilValue) {
        return INTEGER(R_altrep_data2(x));
    }
    if (writeable) {
        SEXP rx = R_altrep_data1(x);
        auto region = unwrap_region(rx);
        R_xlen_t n = xptr_length(rx);
        SEXP m = PROTECT(Rf_allocVector(INTSXP, n));
        if (n > 0) std::memcpy(INTEGER(m), region->data(), n * sizeof(int));
        R_set_altrep_data1(x, R_NilValue);
        R_set_altrep_data2(x, m);
        UNPROTECT(1);
        return INTEGER(m);
    }
    SEXP rx = R_altrep_data1(x);
    auto region = unwrap_region(rx);
    return const_cast<void*>(region->data());
}
static const void *mmap_int_dataptr_or_null(SEXP x) {
    SEXP rx = R_altrep_data1(x);
    if (rx == R_NilValue) return DATAPTR_RO(R_altrep_data2(x));
    auto region = unwrap_region(rx);
    return region->data();
}
static R_xlen_t mmap_int_get_region(SEXP x, R_xlen_t start, R_xlen_t size, int *buf) {
    const int *p = static_cast<const int*>(mmap_int_dataptr_or_null(x));
    R_xlen_t avail = mmap_int_length(x) - start;
    R_xlen_t n = (size < avail) ? size : avail;
    std::memcpy(buf, p + start, n * sizeof(int));
    return n;
}
static SEXP mmap_int_serialized_state(SEXP x) {
    R_xlen_t n = mmap_int_length(x);
    SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
    std::memcpy(INTEGER(out), mmap_int_dataptr_or_null(x), n * sizeof(int));
    UNPROTECT(1);
    return out;
}
static SEXP mmap_int_unserialize(SEXP, SEXP state) { return state; }

static void init_mmap_int(DllInfo *dll) {
    MmapIntClass = R_make_altinteger_class("MmapIntAltrep", "dafr", dll);
    R_set_altrep_Length_method(MmapIntClass, mmap_int_length);
    R_set_altvec_Dataptr_method(MmapIntClass, mmap_int_dataptr);
    R_set_altvec_Dataptr_or_null_method(MmapIntClass, mmap_int_dataptr_or_null);
    R_set_altinteger_Elt_method(MmapIntClass, mmap_int_elt);
    R_set_altinteger_Get_region_method(MmapIntClass, mmap_int_get_region);
    R_set_altrep_Serialized_state_method(MmapIntClass, mmap_int_serialized_state);
    R_set_altrep_Unserialize_method(MmapIntClass, mmap_int_unserialize);
}

SEXP make_mmap_int_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length) {
    SEXP region_xptr = PROTECT(wrap_region(std::move(region), length));
    SEXP out = R_new_altrep(MmapIntClass, region_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

// ---- Lgl (logical, stored as int32) — re-uses int methods via R_altlogical_* registration. ----

static R_xlen_t mmap_lgl_length(SEXP x) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return XLENGTH(R_altrep_data2(x));
    return xptr_length(d1);
}
static int mmap_lgl_elt(SEXP x, R_xlen_t i) {
    SEXP rx = R_altrep_data1(x);
    if (rx == R_NilValue) return LOGICAL(R_altrep_data2(x))[i];
    auto region = unwrap_region(rx);
    return static_cast<const int*>(region->data())[i];
}
static void *mmap_lgl_dataptr(SEXP x, Rboolean writeable) {
    if (R_altrep_data1(x) == R_NilValue) {
        return LOGICAL(R_altrep_data2(x));
    }
    if (writeable) {
        SEXP rx = R_altrep_data1(x);
        auto region = unwrap_region(rx);
        R_xlen_t n = xptr_length(rx);
        SEXP m = PROTECT(Rf_allocVector(LGLSXP, n));
        if (n > 0) std::memcpy(LOGICAL(m), region->data(), n * sizeof(int));
        R_set_altrep_data1(x, R_NilValue);
        R_set_altrep_data2(x, m);
        UNPROTECT(1);
        return LOGICAL(m);
    }
    SEXP rx = R_altrep_data1(x);
    auto region = unwrap_region(rx);
    return const_cast<void*>(region->data());
}
static const void *mmap_lgl_dataptr_or_null(SEXP x) {
    SEXP rx = R_altrep_data1(x);
    if (rx == R_NilValue) return DATAPTR_RO(R_altrep_data2(x));
    auto region = unwrap_region(rx);
    return region->data();
}
static R_xlen_t mmap_lgl_get_region(SEXP x, R_xlen_t start, R_xlen_t size, int *buf) {
    const int *p = static_cast<const int*>(mmap_lgl_dataptr_or_null(x));
    R_xlen_t avail = mmap_lgl_length(x) - start;
    R_xlen_t n = (size < avail) ? size : avail;
    std::memcpy(buf, p + start, n * sizeof(int));
    return n;
}
static SEXP mmap_lgl_serialized_state(SEXP x) {
    R_xlen_t n = mmap_lgl_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
    std::memcpy(LOGICAL(out), mmap_lgl_dataptr_or_null(x), n * sizeof(int));
    UNPROTECT(1);
    return out;
}
static SEXP mmap_lgl_unserialize(SEXP, SEXP state) { return state; }

static void init_mmap_lgl(DllInfo *dll) {
    MmapLglClass = R_make_altlogical_class("MmapLglAltrep", "dafr", dll);
    R_set_altrep_Length_method(MmapLglClass, mmap_lgl_length);
    R_set_altvec_Dataptr_method(MmapLglClass, mmap_lgl_dataptr);
    R_set_altvec_Dataptr_or_null_method(MmapLglClass, mmap_lgl_dataptr_or_null);
    R_set_altlogical_Elt_method(MmapLglClass, mmap_lgl_elt);
    R_set_altlogical_Get_region_method(MmapLglClass, mmap_lgl_get_region);
    R_set_altrep_Serialized_state_method(MmapLglClass, mmap_lgl_serialized_state);
    R_set_altrep_Unserialize_method(MmapLglClass, mmap_lgl_unserialize);
}

SEXP make_mmap_lgl_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length) {
    SEXP region_xptr = PROTECT(wrap_region(std::move(region), length));
    SEXP out = R_new_altrep(MmapLglClass, region_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

void init_altrep_mmap(DllInfo *dll) {
    init_mmap_real(dll);
    init_mmap_int(dll);
    init_mmap_lgl(dll);
}

} // namespace dafr
```

- [ ] **Step 3: DELETE `src/init.cpp`** — do NOT register an `R_init_dafr` by hand.

The package's `src/init.cpp` (from Phase C) previously provided a hand-rolled `R_init_dafr`. However, once D2 adds `[[cpp11::register]]` decorations, cpp11 generates its own `R_init_dafr` inside `src/cpp11.cpp`, and having two definitions is a link error (`multiple definition of R_init_dafr`). The plan originally claimed cpp11 uses `R_init_dafr_cpp11` — that was incorrect for cpp11 ≥ 0.5.

Remove `src/init.cpp` entirely. The ALTREP class registration will be wired up in D2 via a `[[cpp11::init]]` hook inside `src/altrep_mmap_r.cpp` which cpp11 automatically invokes from its generated `R_init_dafr`.

```bash
cd ~/src/dafr-native
git rm src/init.cpp
```

- [ ] **Step 4: Build**

```r
pkgbuild::clean_dll("~/src/dafr-native")
pkgbuild::compile_dll("~/src/dafr-native", debug = FALSE)
```
Expected: compiles clean.

- [ ] **Step 5: Commit**

```bash
cd ~/src/dafr-native
git add src/altrep_mmap.h src/altrep_mmap.cpp
git rm src/init.cpp
git commit -m "feat: ALTREP classes for mmap-backed double/int/logical vectors"
```

### Task D2: Expose ALTREP constructors to R via cpp11

**Files:**
- Create: `~/src/dafr-native/src/altrep_mmap_r.cpp`
- Create: `~/src/dafr-native/R/altrep.R`
- Create: `~/src/dafr-native/tests/testthat/test-altrep-mmap.R`

- [ ] **Step 1: Write `src/altrep_mmap_r.cpp`** — cpp11-decorated entry points

```cpp
#include <cpp11.hpp>
#include "altrep_mmap.h"
#include "mmap_region.h"

// cpp11 generates its own R_init_dafr; use a [[cpp11::init]] hook
// to piggyback ALTREP class registration onto it.
[[cpp11::init]]
void dafr_init_altrep_mmap(DllInfo* dll) {
    dafr::init_altrep_mmap(dll);
}

[[cpp11::register]]
SEXP mmap_real_altrep_cpp(std::string path, double length_double) {
    R_xlen_t length = static_cast<R_xlen_t>(length_double);
    auto region = dafr::MmapRegion::open_readonly(path);
    std::size_t required = static_cast<std::size_t>(length) * sizeof(double);
    if (region->nbytes() < required) {
        cpp11::stop("mmap_real: file '%s' has %zu bytes, need at least %zu for length=%lld",
                    path.c_str(), region->nbytes(), required,
                    static_cast<long long>(length));
    }
    return dafr::make_mmap_real_altrep(region, length);
}

[[cpp11::register]]
SEXP mmap_int_altrep_cpp(std::string path, double length_double) {
    R_xlen_t length = static_cast<R_xlen_t>(length_double);
    auto region = dafr::MmapRegion::open_readonly(path);
    std::size_t required = static_cast<std::size_t>(length) * sizeof(int);
    if (region->nbytes() < required) {
        cpp11::stop("mmap_int: file '%s' has %zu bytes, need at least %zu for length=%lld",
                    path.c_str(), region->nbytes(), required,
                    static_cast<long long>(length));
    }
    return dafr::make_mmap_int_altrep(region, length);
}

[[cpp11::register]]
SEXP mmap_lgl_altrep_cpp(std::string path, double length_double) {
    R_xlen_t length = static_cast<R_xlen_t>(length_double);
    auto region = dafr::MmapRegion::open_readonly(path);
    std::size_t required = static_cast<std::size_t>(length) * sizeof(int);
    if (region->nbytes() < required) {
        cpp11::stop("mmap_lgl: file '%s' has %zu bytes, need at least %zu for length=%lld",
                    path.c_str(), region->nbytes(), required,
                    static_cast<long long>(length));
    }
    return dafr::make_mmap_lgl_altrep(region, length);
}
```

- [ ] **Step 2: Run cpp11 to generate bindings**

```r
cpp11::cpp_register("~/src/dafr-native")
```
Expected: creates `src/cpp11.cpp` and `R/cpp11.R`.

- [ ] **Step 3: Write `R/altrep.R`** (user-visible thin wrappers)

```r
#' Open a binary file as a read-only mmap-backed numeric vector.
#'
#' Length is the number of `double` elements; the file must contain at least
#' `length * 8` bytes.
#' @export
mmap_real <- function(path, length) {
  stopifnot(file.exists(path), is.numeric(length), length >= 0)
  mmap_real_altrep_cpp(path.expand(path), as.double(length))
}

#' @rdname mmap_real
#' @export
mmap_int <- function(path, length) {
  stopifnot(file.exists(path), is.numeric(length), length >= 0)
  mmap_int_altrep_cpp(path.expand(path), as.double(length))
}

#' @rdname mmap_real
#' @export
mmap_lgl <- function(path, length) {
  stopifnot(file.exists(path), is.numeric(length), length >= 0)
  mmap_lgl_altrep_cpp(path.expand(path), as.double(length))
}
```

- [ ] **Step 4: Write `tests/testthat/test-altrep-mmap.R`**

```r
test_that("mmap_real reads doubles without copying", {
  f <- new_tempfile("bin")
  vals <- seq(1.0, 1000.0, length.out = 1000)
  writeBin(vals, f, size = 8L)

  v <- mmap_real(f, 1000)
  expect_equal(length(v), 1000L)
  expect_equal(v[1], 1)
  expect_equal(v[1000], 1000)
  expect_equal(sum(v), sum(vals))
})

test_that("mmap_int reads int32 elements", {
  f <- new_tempfile("bin")
  vals <- 1:1000L
  writeBin(vals, f, size = 4L)

  v <- mmap_int(f, 1000)
  expect_equal(length(v), 1000L)
  expect_equal(v[500], 500L)
  expect_equal(sum(v), sum(vals))
})

test_that("mmap_lgl reads logical (int32-stored)", {
  f <- new_tempfile("bin")
  vals <- as.integer(rep(c(1L, 0L, NA_integer_), length.out = 300))
  writeBin(vals, f, size = 4L)

  v <- mmap_lgl(f, 300)
  expect_equal(length(v), 300L)
  expect_true(is.logical(v))
  expect_identical(v[1:3], c(TRUE, FALSE, NA))
})

test_that("writing to an mmap-backed vector triggers materialization", {
  f <- new_tempfile("bin")
  writeBin(c(1.0, 2.0, 3.0), f, size = 8L)
  v <- mmap_real(f, 3)
  expect_silent({ v[1] <- 99 })        # triggers Dataptr(writeable=TRUE)
  expect_equal(v, c(99, 2, 3))         # user-side copy now owns the mutation
})

test_that("zero-length file mmap is a length-0 vector", {
  f <- new_tempfile("bin")
  file.create(f)
  v <- mmap_real(f, 0)
  expect_equal(length(v), 0L)
})
```

- [ ] **Step 5: Build, load, test**

```r
pkgbuild::clean_dll("~/src/dafr-native")
pkgbuild::compile_dll("~/src/dafr-native", debug = FALSE)
devtools::load_all("~/src/dafr-native")
testthat::test_file("~/src/dafr-native/tests/testthat/test-altrep-mmap.R")
```
Expected: all 5 tests pass.

- [ ] **Step 6: Commit**

```bash
cd ~/src/dafr-native
Rscript -e 'roxygen2::roxygenise("~/src/dafr-native")'
git add src/altrep_mmap_r.cpp src/cpp11.cpp R/altrep.R R/cpp11.R NAMESPACE tests/testthat/test-altrep-mmap.R
git commit -m "feat: R-visible mmap_real / mmap_int / mmap_lgl ALTREP constructors"
```

### Task D3: ALTREP-backed `dgCMatrix` helper and smoke tests against `Matrix`

**Files:**
- Create: `~/src/dafr-native/R/mmap.R`
- Create: `~/src/dafr-native/tests/testthat/test-altrep-dgCMatrix.R`

- [ ] **Step 1: Write `R/mmap.R`**

```r
#' Construct a `dgCMatrix` whose `x`, `i`, `p` slots are ALTREP mmap views.
#'
#' The three files must be the CSC component files of a sparse matrix:
#' `x_path` stores `nnz` doubles, `i_path` stores `nnz` int32 row indices
#' (0-based, matching `dgCMatrix@i`), `p_path` stores `ncol+1` int32
#' column pointers (0-based, matching `dgCMatrix@p`).
#'
#' @param x_path,i_path,p_path Paths to the `.bin` files.
#' @param nrow,ncol,nnz Matrix shape + number of non-zeros.
#' @param dimnames Optional `list(rowname_character, colname_character)`.
#' @importClassesFrom Matrix dgCMatrix
#' @importFrom Matrix sparseMatrix
#' @export
mmap_dgCMatrix <- function(x_path, i_path, p_path, nrow, ncol, nnz,
                           dimnames = NULL) {
  stopifnot(file.exists(x_path), file.exists(i_path), file.exists(p_path))
  stopifnot(is.numeric(nrow), is.numeric(ncol), is.numeric(nnz))
  if (!is.null(dimnames)) {
    stopifnot(is.list(dimnames), length(dimnames) == 2L)
  }

  x_slot <- mmap_real(x_path, nnz)
  i_slot <- mmap_int(i_path,  nnz)
  p_slot <- mmap_int(p_path,  as.integer(ncol) + 1L)

  # Cheap CSC invariant check: p[ncol+1] must equal nnz.
  stopifnot(p_slot[as.integer(ncol) + 1L] == nnz)

  m <- methods::new("dgCMatrix",
    x        = x_slot,
    i        = i_slot,
    p        = p_slot,
    Dim      = c(as.integer(nrow), as.integer(ncol)),
    Dimnames = if (is.null(dimnames)) list(NULL, NULL) else dimnames)
  m
}
```

- [ ] **Step 2: Write `tests/testthat/test-altrep-dgCMatrix.R`**

```r
make_test_dgC_files <- function() {
  # Build a small dgCMatrix, write its slots as plain binary files.
  set.seed(1)
  dense <- matrix(
    ifelse(runif(100) < 0.3, rnorm(100), 0),
    nrow = 10, ncol = 10
  )
  m <- methods::as(dense, "CsparseMatrix")  # dgCMatrix
  stopifnot(inherits(m, "dgCMatrix"))

  d <- new_tempdir()
  writeBin(m@x, file.path(d, "x.bin"), size = 8L)
  writeBin(as.integer(m@i), file.path(d, "i.bin"), size = 4L)  # 0-based
  writeBin(as.integer(m@p), file.path(d, "p.bin"), size = 4L)

  list(dir = d, dense = dense, nnz = length(m@x),
       reference = m)
}

test_that("mmap_dgCMatrix reconstructs slots correctly", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow   = 10, ncol = 10, nnz = tf$nnz
  )
  expect_equal(dim(m), c(10L, 10L))
  expect_equal(as.matrix(m), tf$reference |> as.matrix())
})

test_that("colSums on mmap-backed dgCMatrix matches Matrix::colSums", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow = 10, ncol = 10, nnz = tf$nnz
  )
  expect_equal(Matrix::colSums(m), Matrix::colSums(tf$reference))
})

test_that("rowSums on mmap-backed dgCMatrix matches Matrix::rowSums", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow = 10, ncol = 10, nnz = tf$nnz
  )
  expect_equal(Matrix::rowSums(m), Matrix::rowSums(tf$reference))
})

test_that("Matrix::t() round-trip yields same values", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow = 10, ncol = 10, nnz = tf$nnz
  )
  mt <- Matrix::t(m)
  expect_equal(as.matrix(mt), t(as.matrix(tf$reference)))
})

test_that("element assignment triggers materialization, doesn't crash", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow = 10, ncol = 10, nnz = tf$nnz
  )
  expect_silent(m[1, 1] <- 99)
})
```

- [ ] **Step 3: Build, load, test**

```r
pkgbuild::clean_dll("~/src/dafr-native")
pkgbuild::compile_dll("~/src/dafr-native", debug = FALSE)
devtools::load_all("~/src/dafr-native")
testthat::test_file("~/src/dafr-native/tests/testthat/test-altrep-dgCMatrix.R")
```
Expected: all 5 tests pass. **If any fail, stop** — this is the POC that decides whether ALTREP-backed `dgCMatrix` is viable. Record failures in `~/src/dafr-native/dev/notes/altrep-compat-findings.md` and revisit the design.

- [ ] **Step 4: Commit package**

```bash
cd ~/src/dafr-native
Rscript -e 'roxygen2::roxygenise("~/src/dafr-native")'
git add R/mmap.R NAMESPACE tests/testthat/test-altrep-dgCMatrix.R
git commit -m "feat: mmap_dgCMatrix + Matrix-compat smoke tests"
```

### Task D4: Downstream-package compatibility smoke test (Seurat / scran)

**Files:**
- Create: `~/src/dafr-native/tests/testthat/test-altrep-downstream.R`
- Create: `~/src/dafr-native/dev/notes/altrep-compat-findings.md`

- [ ] **Step 1: Write `tests/testthat/test-altrep-downstream.R`**

```r
# These tests are skipped when the respective packages are not installed.
# They detect whether downstream code bypasses ALTREP (asserts DATAPTR
# is a real heap pointer) — which would force us to redesign.

make_test_dgC_files <- function() {
  set.seed(42)
  nr <- 200; nc <- 100
  dense <- matrix(ifelse(runif(nr * nc) < 0.1, round(rexp(nr * nc) * 10), 0),
                  nrow = nr, ncol = nc)
  m <- methods::as(dense, "CsparseMatrix")
  d <- new_tempdir()
  writeBin(m@x, file.path(d, "x.bin"), size = 8L)
  writeBin(as.integer(m@i), file.path(d, "i.bin"), size = 4L)
  writeBin(as.integer(m@p), file.path(d, "p.bin"), size = 4L)
  list(dir = d, reference = m, nrow = nr, ncol = nc, nnz = length(m@x))
}

test_that("Seurat::CreateSeuratObject accepts mmap-backed dgCMatrix", {
  skip_if_not_installed("Seurat")
  tf <- make_test_dgC_files()
  rownames <- paste0("gene", seq_len(tf$nrow))
  colnames <- paste0("cell", seq_len(tf$ncol))
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow   = tf$nrow, ncol = tf$ncol, nnz = tf$nnz,
    dimnames = list(rownames, colnames)
  )
  obj <- Seurat::CreateSeuratObject(counts = m)
  expect_s4_class(obj, "Seurat")
  expect_equal(ncol(obj), tf$ncol)
})

test_that("scran::quickCluster tolerates mmap-backed dgCMatrix", {
  skip_if_not_installed("scran")
  skip_if_not_installed("SingleCellExperiment")
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow   = tf$nrow, ncol = tf$ncol, nnz = tf$nnz,
    dimnames = list(paste0("g", seq_len(tf$nrow)),
                    paste0("c", seq_len(tf$ncol)))
  )
  sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = m))
  cl <- scran::quickCluster(sce, min.size = 10)
  expect_length(cl, tf$ncol)
})
```

- [ ] **Step 2: Run**

```r
devtools::load_all("~/src/dafr-native")
testthat::test_file("~/src/dafr-native/tests/testthat/test-altrep-downstream.R")
```

Expected outcomes:
- Either all present packages' tests pass, OR they fail with a crash / error indicating `DATAPTR` assumptions.

- [ ] **Step 3: Record findings**

Write `~/src/dafr-native/dev/notes/altrep-compat-findings.md` with:
- Which downstream packages were available.
- Which tests passed / failed.
- If any fails: the exact error, and a recommended mitigation (e.g., "Seurat fails when calling `Seurat:::Xxx` because it materializes unconditionally — acceptable; users would have hit that anyway.").

Example skeleton:

```markdown
# ALTREP compatibility findings — 2026-04-19

| Package | Installed | Test | Result | Notes |
|---|---|---|---|---|
| Seurat  | yes / no  | CreateSeuratObject | pass/fail | ... |
| scran   | yes / no  | quickCluster       | pass/fail | ... |

## Mitigation decisions

- ...
```

- [ ] **Step 4: Commit both repos**

```bash
cd ~/src/dafr-native
git add tests/testthat/test-altrep-downstream.R
git commit -m "test: Seurat/scran ALTREP compatibility smoke tests"

cd ~/src/dafr-native/dev
git add notes/altrep-compat-findings.md
git commit -m "notes: ALTREP downstream compat findings"
```

---

## Phase E — Benchmark harness

### Task E1: Workload generator

**Files:**
- Create: `~/src/dafr-native/inst/benchmarks/workloads.R`

- [ ] **Step 1: Write `inst/benchmarks/workloads.R`**

```r
# Workload generators for Slice 0 benchmarks.
# Produces synthetic FilesDaf-like directories; real FilesDaf format comes
# in Slice 2, so for now we emit the raw slot files only.

make_synthetic_sparse <- function(nrow = 30000L, ncol = 30000L, density = 0.1) {
  nnz_est <- as.integer(nrow * ncol * density)
  j <- sort(sample.int(ncol, nnz_est, replace = TRUE))
  i <- sample.int(nrow, nnz_est, replace = TRUE) - 1L  # 0-based
  x <- rpois(nnz_est, lambda = 3)

  # Build dgCMatrix and write its slots.
  m <- Matrix::sparseMatrix(
    i = i + 1L, j = j, x = x,
    dims = c(nrow, ncol), index1 = TRUE
  )

  list(matrix = m, nnz = length(m@x))
}

write_csc_slots <- function(m, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeBin(m@x, file.path(out_dir, "x.bin"), size = 8L)
  writeBin(as.integer(m@i), file.path(out_dir, "i.bin"), size = 4L)
  writeBin(as.integer(m@p), file.path(out_dir, "p.bin"), size = 4L)
  invisible(out_dir)
}
```

- [ ] **Step 2: Commit**

```bash
cd ~/src/dafr-native
git add inst/benchmarks/workloads.R
git commit -m "bench: workload generator for Slice 0"
```

### Task E2: Main benchmark script

**Files:**
- Create: `~/src/dafr-native/inst/benchmarks/bench.R`

- [ ] **Step 1: Write `inst/benchmarks/bench.R`**

```r
# Slice 0 benchmark script.
#
# Invocation:
#   Rscript inst/benchmarks/bench.R [--small]
#
# Small mode uses 3K x 3K matrix for CI; full mode uses 30K x 30K.

suppressPackageStartupMessages({
  library(dafr)
  library(Matrix)
  library(bench)
})
source(system.file("benchmarks", "workloads.R", package = "dafr"))

args <- commandArgs(trailingOnly = TRUE)
small <- "--small" %in% args

nrow <- if (small) 3000L else 30000L
ncol <- if (small) 3000L else 30000L
density <- 0.1

cat("Generating synthetic sparse matrix", nrow, "x", ncol, "density", density, "\n")
wl <- make_synthetic_sparse(nrow, ncol, density)
out_dir <- tempfile(); dir.create(out_dir)
write_csc_slots(wl$matrix, out_dir)

# --- Benchmark 1: open cold (mmap_dgCMatrix construction time) ---
bm_open <- bench::mark(
  mmap = mmap_dgCMatrix(
    x_path = file.path(out_dir, "x.bin"),
    i_path = file.path(out_dir, "i.bin"),
    p_path = file.path(out_dir, "p.bin"),
    nrow = nrow, ncol = ncol, nnz = wl$nnz
  ),
  iterations = 20, check = FALSE
)

# --- Benchmark 2: colSums ---
m_mmap <- mmap_dgCMatrix(
  x_path = file.path(out_dir, "x.bin"),
  i_path = file.path(out_dir, "i.bin"),
  p_path = file.path(out_dir, "p.bin"),
  nrow = nrow, ncol = ncol, nnz = wl$nnz
)
bm_cs <- bench::mark(
  native = Matrix::colSums(wl$matrix),
  mmap   = Matrix::colSums(m_mmap),
  iterations = 20, check = TRUE
)

# --- Benchmark 3: transpose ---
bm_t <- bench::mark(
  native = Matrix::t(wl$matrix),
  mmap   = Matrix::t(m_mmap),
  iterations = 5, check = FALSE
)

# Write results.
results_dir <- "~/src/dafr-native/dev/benchmarks"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
ts <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
csv_out <- file.path(results_dir, paste0("slice-0-baseline-", ts, ".csv"))

out <- dplyr::bind_rows(
  data.frame(benchmark = "open_cold",      expression = as.character(bm_open$expression),
             median_ns = as.numeric(bm_open$median)),
  data.frame(benchmark = "colSums",        expression = as.character(bm_cs$expression),
             median_ns = as.numeric(bm_cs$median)),
  data.frame(benchmark = "transpose",      expression = as.character(bm_t$expression),
             median_ns = as.numeric(bm_t$median))
)
write.csv(out, csv_out, row.names = FALSE)
cat("Wrote", csv_out, "\n")
print(out)
```

- [ ] **Step 2: Run once as a smoke check**

```bash
cd ~/src/dafr-native
Rscript inst/benchmarks/bench.R --small
```
Expected: prints a table; writes a CSV under `dev/benchmarks/`.

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add inst/benchmarks/bench.R
git commit -m "bench: slice 0 benchmark script"

cd ~/src/dafr-native/dev
git add benchmarks/
git commit -m "bench: slice-0 baseline CSV"
```

---

## Phase F — C++ stack bake-off (D vs B)

### Task F1: Eltwise `log(x) + y` kernel, cpp11+BLAS path

**Files:**
- Create: `~/src/dafr-native/src/kernel_eltwise_log_add.cpp`

- [ ] **Step 1: Write the kernel**

```cpp
// Eltwise kernel: out[k] = log(x[k]) + y[k] for k in [0, n).
// Pure cpp11 + BLAS (no BLAS used here because log isn't a BLAS primitive;
// this is the "hand-rolled C++" arm of the bake-off).

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>

[[cpp11::register]]
cpp11::writable::doubles kernel_log_add_cpp(cpp11::doubles x, cpp11::doubles y) {
    const R_xlen_t n = x.size();
    if (y.size() != n) cpp11::stop("x and y must have the same length");
    cpp11::writable::doubles out(n);
    const double *px = REAL(x.data());
    const double *py = REAL(y.data());
    double *pout = REAL(out.data());
    DAFR_PARALLEL_FOR(n >= 10000)
    for (R_xlen_t k = 0; k < n; ++k) {
        pout[k] = std::log(px[k]) + py[k];
    }
    return out;
}
```

- [ ] **Step 2: Regenerate cpp11 bindings, build**

```r
cpp11::cpp_register("~/src/dafr-native")
pkgbuild::clean_dll("~/src/dafr-native")
pkgbuild::compile_dll("~/src/dafr-native", debug = FALSE)
```
Expected: compiles clean.

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add src/kernel_eltwise_log_add.cpp src/cpp11.cpp R/cpp11.R
git commit -m "bench: cpp11+BLAS eltwise log-add kernel"
```

### Task F2: CSC column-sum kernel, cpp11+BLAS path

**Files:**
- Create: `~/src/dafr-native/src/kernel_csc_colsums.cpp`

- [ ] **Step 1: Write kernel**

```cpp
#include <cpp11.hpp>
#include "openmp_shim.h"

[[cpp11::register]]
cpp11::writable::doubles kernel_csc_colsums_cpp(
    cpp11::doubles x,   // nnz values
    cpp11::integers p,  // ncol+1 column pointers (0-based)
    int ncol
) {
    cpp11::writable::doubles out(ncol);
    const double *px = REAL(x.data());
    const int *pp = INTEGER(p.data());
    double *pout = REAL(out.data());
    DAFR_PARALLEL_FOR(ncol >= 1000)
    for (int j = 0; j < ncol; ++j) {
        double s = 0.0;
        for (int k = pp[j]; k < pp[j + 1]; ++k) {
            s += px[k];
        }
        pout[j] = s;
    }
    return out;
}
```

- [ ] **Step 2: Build**

```r
cpp11::cpp_register("~/src/dafr-native")
pkgbuild::clean_dll("~/src/dafr-native")
pkgbuild::compile_dll("~/src/dafr-native", debug = FALSE)
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add src/kernel_csc_colsums.cpp src/cpp11.cpp R/cpp11.R
git commit -m "bench: cpp11+BLAS CSC colsum kernel"
```

### Task F3: CSC→CSR transpose kernel, cpp11+BLAS path

**Files:**
- Create: `~/src/dafr-native/src/kernel_csc_to_csr.cpp`

- [ ] **Step 1: Write kernel** (two-pass counting sort; single-threaded — transpose parallelization is non-trivial; acceptable baseline)

```cpp
#include <cpp11.hpp>
#include <cstring>
#include <vector>

// Output lists: new_p (nrow+1), new_j (nnz), new_x (nnz) corresponding to
// a CSR representation.
[[cpp11::register]]
cpp11::writable::list kernel_csc_to_csr_cpp(
    cpp11::doubles x,    // nnz
    cpp11::integers i,   // nnz — 0-based row indices
    cpp11::integers p,   // ncol+1
    int nrow, int ncol
) {
    const R_xlen_t nnz = x.size();
    const double *px = REAL(x.data());
    const int *pi = INTEGER(i.data());
    const int *pp = INTEGER(p.data());

    cpp11::writable::integers new_p(nrow + 1);
    cpp11::writable::integers new_j(nnz);
    cpp11::writable::doubles new_x(nnz);

    int *pnp = INTEGER(new_p.data());
    int *pnj = INTEGER(new_j.data());
    double *pnx = REAL(new_x.data());

    std::memset(pnp, 0, (nrow + 1) * sizeof(int));

    // Count entries per row.
    for (R_xlen_t k = 0; k < nnz; ++k) ++pnp[pi[k] + 1];
    // Cumulative sum -> row pointers.
    for (int r = 0; r < nrow; ++r) pnp[r + 1] += pnp[r];

    // Scatter pass. Need a mutable cursor per row.
    std::vector<int> cur(nrow);
    std::memcpy(cur.data(), pnp, nrow * sizeof(int));

    for (int col = 0; col < ncol; ++col) {
        for (int k = pp[col]; k < pp[col + 1]; ++k) {
            int row = pi[k];
            int dest = cur[row]++;
            pnj[dest] = col;
            pnx[dest] = px[k];
        }
    }

    cpp11::writable::list out;
    out.push_back({"p"_nm = new_p});
    out.push_back({"j"_nm = new_j});
    out.push_back({"x"_nm = new_x});
    return out;
}
```

- [ ] **Step 2: Build**

```r
cpp11::cpp_register("~/src/dafr-native")
pkgbuild::clean_dll("~/src/dafr-native")
pkgbuild::compile_dll("~/src/dafr-native", debug = FALSE)
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add src/kernel_csc_to_csr.cpp src/cpp11.cpp R/cpp11.R
git commit -m "bench: cpp11+BLAS CSC->CSR transpose kernel"
```

### Task F4: RcppEigen counterparts (bake-off "B" arm)

**Files:**
- Create: `~/src/dafr-native/dev/benchmarks/bake-off-eigen/` (separate tiny scratch package, since we don't want to add Eigen to the main DESCRIPTION)
- Create: `~/src/dafr-native/dev/benchmarks/bake-off-eigen/DESCRIPTION`
- Create: `~/src/dafr-native/dev/benchmarks/bake-off-eigen/NAMESPACE`
- Create: `~/src/dafr-native/dev/benchmarks/bake-off-eigen/src/eigen_kernels.cpp`

- [ ] **Step 1: Create scratch package for Eigen**

```bash
mkdir -p ~/src/dafr-native/dev/benchmarks/bake-off-eigen/{src,R}
```

- [ ] **Step 2: Write `DESCRIPTION`**

```
Package: dafrBakeoffEigen
Version: 0.0.0
Title: Eigen-based arm of the dafr Slice 0 bake-off
License: MIT + file LICENSE
Depends: R (>= 4.4.0)
LinkingTo: Rcpp, RcppEigen
Imports: Rcpp
SystemRequirements: C++17
Encoding: UTF-8
```

- [ ] **Step 3: Write `NAMESPACE`**

```
useDynLib(dafrBakeoffEigen, .registration = TRUE)
importFrom(Rcpp, sourceCpp)
export(eigen_log_add, eigen_csc_colsums, eigen_csc_to_csr)
```

- [ ] **Step 4: Write `src/eigen_kernels.cpp`**

```cpp
// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <cmath>

// --- log(x) + y ---
// [[Rcpp::export]]
Eigen::VectorXd eigen_log_add(const Eigen::Map<Eigen::VectorXd> x,
                              const Eigen::Map<Eigen::VectorXd> y) {
    Eigen::VectorXd out = x.array().log() + y.array();
    return out;
}

// --- CSC colSums ---
typedef Eigen::SparseMatrix<double, Eigen::ColMajor, int> SpMat;

// [[Rcpp::export]]
Eigen::VectorXd eigen_csc_colsums(const Eigen::Map<SpMat> m) {
    Eigen::VectorXd out(m.cols());
    for (int j = 0; j < m.cols(); ++j) {
        double s = 0.0;
        for (SpMat::InnerIterator it(m, j); it; ++it) s += it.value();
        out[j] = s;
    }
    return out;
}

// --- CSC -> CSR transpose ---
typedef Eigen::SparseMatrix<double, Eigen::RowMajor, int> SpMatR;

// [[Rcpp::export]]
Rcpp::List eigen_csc_to_csr(const Eigen::Map<SpMat> m) {
    SpMatR csr = m;
    csr.makeCompressed();
    return Rcpp::List::create(
        Rcpp::Named("p") = Rcpp::IntegerVector(csr.outerIndexPtr(),
                                               csr.outerIndexPtr() + csr.rows() + 1),
        Rcpp::Named("j") = Rcpp::IntegerVector(csr.innerIndexPtr(),
                                               csr.innerIndexPtr() + csr.nonZeros()),
        Rcpp::Named("x") = Rcpp::NumericVector(csr.valuePtr(),
                                               csr.valuePtr() + csr.nonZeros())
    );
}
```

- [ ] **Step 5: Install the scratch package**

```r
devtools::install("~/src/dafr-native/dev/benchmarks/bake-off-eigen")
```
Expected: installs successfully.

- [ ] **Step 6: Commit (dev repo)**

```bash
cd ~/src/dafr-native/dev
git add benchmarks/bake-off-eigen/
git commit -m "bench: RcppEigen bake-off arm (scratch package)"
```

### Task F5: Bake-off runner and decision record

**Files:**
- Create: `~/src/dafr-native/dev/benchmarks/run-bakeoff.R`
- Create: `~/src/dafr-native/dev/benchmarks/bake-off-results.md`

- [ ] **Step 1: Write `dev/benchmarks/run-bakeoff.R`**

```r
# Bake-off runner: D (cpp11+BLAS, in dafr) vs B (RcppEigen, in scratch package)
# Reports median time and memory allocations.

suppressPackageStartupMessages({
  devtools::load_all("~/src/dafr-native")
  library(dafrBakeoffEigen)
  library(bench)
  library(Matrix)
})

set.seed(0)

# ---- Kernel 1: eltwise log(x) + y on 30K*30K doubles ----
n <- 30000L * 30000L  # 900M doubles -> 6.7 GiB each
if (Sys.getenv("SMALL") == "1") n <- 1e7  # 10M for CI

x <- abs(rnorm(n)) + 1e-3
y <- rnorm(n)
bm_log <- bench::mark(
  D_cpp11 = kernel_log_add_cpp(x, y),
  B_eigen = eigen_log_add(x, y),
  iterations = 5, check = FALSE, memory = FALSE
)

# ---- Kernel 2: CSC col-sum on 100K*1M, 1% density ----
nr <- 100000L; nc <- 1000000L; dens <- 0.01
if (Sys.getenv("SMALL") == "1") { nr <- 1000L; nc <- 10000L }
nnz <- as.integer(nr * nc * dens)
i_ix <- sample.int(nr, nnz, replace = TRUE)
j_ix <- sort(sample.int(nc, nnz, replace = TRUE))
m <- Matrix::sparseMatrix(i = i_ix, j = j_ix, x = rpois(nnz, 3),
                          dims = c(nr, nc))
bm_cs <- bench::mark(
  D_cpp11 = kernel_csc_colsums_cpp(m@x, m@p, ncol(m)),
  B_eigen = eigen_csc_colsums(m),
  iterations = 5, check = TRUE, memory = FALSE
)

# ---- Kernel 3: CSC -> CSR transpose on 100K*100K, 5% density ----
nr <- 100000L; nc <- 100000L; dens <- 0.05
if (Sys.getenv("SMALL") == "1") { nr <- 2000L; nc <- 2000L }
nnz <- as.integer(nr * nc * dens)
i_ix <- sample.int(nr, nnz, replace = TRUE)
j_ix <- sort(sample.int(nc, nnz, replace = TRUE))
m <- Matrix::sparseMatrix(i = i_ix, j = j_ix, x = rpois(nnz, 3),
                          dims = c(nr, nc))
bm_t <- bench::mark(
  D_cpp11 = kernel_csc_to_csr_cpp(m@x, m@i, m@p, nr, nc),
  B_eigen = eigen_csc_to_csr(m),
  iterations = 3, check = FALSE, memory = FALSE
)

summary <- list(
  log_add = bm_log[, c("expression", "median")],
  colsums = bm_cs[, c("expression", "median")],
  transpose = bm_t[, c("expression", "median")]
)

print(summary)

out_path <- "~/src/dafr-native/dev/benchmarks/bake-off-results.csv"
write.csv(do.call(rbind, lapply(names(summary), function(nm) {
  df <- as.data.frame(summary[[nm]])
  df$kernel <- nm
  df
})), out_path, row.names = FALSE)
cat("Wrote", out_path, "\n")
```

- [ ] **Step 2: Run the bake-off**

```bash
cd ~/src/dafr-native
# Full size — skip if insufficient RAM
SMALL=1 Rscript ~/src/dafr-native/dev/benchmarks/run-bakeoff.R
```
Expected: prints timing for each kernel under both stacks, writes CSV.

- [ ] **Step 3: Record decision**

Write `~/src/dafr-native/dev/benchmarks/bake-off-results.md` with:
- Machine spec (CPU, cores, RAM).
- Table of measured medians for each of the 3 kernels, both stacks.
- Decision: "D wins / B wins" per kernel and overall. Per spec: if Eigen wins by > 20% on any kernel, note that we re-open the decision for that kernel family.

Template:

```markdown
# Slice 0 bake-off results — 2026-04-19

Machine: <output of `sessionInfo()$running` + `parallel::detectCores()` + free -h>.

## Measurements

| Kernel | D (cpp11+BLAS) median | B (RcppEigen) median | Ratio D/B | Winner |
|---|---|---|---|---|
| log(x) + y | ... | ... | ... | D / B |
| CSC colSums | ... | ... | ... | D / B |
| CSC->CSR transpose | ... | ... | ... | D / B |

## Decision

<One of: "Stick with D across the board" | "Stick with D but reopen for kernel X because B wins by 20%+">

## Follow-ups

- ...
```

- [ ] **Step 4: Commit (dev repo only)**

```bash
cd ~/src/dafr-native/dev
git add benchmarks/run-bakeoff.R benchmarks/bake-off-results.md benchmarks/bake-off-results.csv
git commit -m "bench: Slice 0 bake-off results + decision"
```

---

## Phase G — FilesDaf on-disk spec extraction

### Task G1: Extract the FilesDaf spec from `files_format.jl`

**Files:**
- Create: `~/src/dafr-native/dev/specs/filesdaf-on-disk-spec-draft.md`

- [ ] **Step 1: Read Julia source**

Read `~/src/DataAxesFormats.jl/src/files_format.jl` in full. Note:
- The directory-layout conventions.
- The JSON keys used in `daf.json`.
- The filename conventions (`<name>.<T>.bin`, `_sparse_colptr.Int32.bin`, etc.) — verify exact naming.
- Version counter storage format (integer in `.txt` file, or something else?).
- Endianness and alignment assumptions.
- `fsync` + atomic rename logic.
- How scalar types map to JSON (e.g., `{"type":"Float64","value":3.14}` vs bare JSON primitive).

- [ ] **Step 2: Write `filesdaf-on-disk-spec-draft.md`**

The spec must cover, as plain prose + examples:

- **Directory layout**: which directories, which files, required vs optional.
- **`daf.json` schema**: exact field names, allowed values, versioning (include a JSON schema or example instance).
- **Scalar encoding**: how each scalar type (`Float32/64`, `Int8..64`, `UInt8..64`, `Bool`, `String`) is stored on disk — inline in JSON or in a separate file.
- **Axis encoding**: `<axis>.txt` line-delimited UTF-8; trailing newline yes/no; forbidden characters.
- **Vector binary format**: `<name>.<T>.bin` — T is the exact string used (match Julia case); little-endian; contiguous; length derived from file size.
- **Matrix dense binary format**: row-major or column-major, byte order, shape encoding.
- **Matrix sparse (CSC) format**: three files — `<name>_sparse_nzval.<T>.bin`, `<name>_sparse_rowval.<IntT>.bin`, `<name>_sparse_colptr.<IntT>.bin`; index type (Int32 default, allow Int64); 0-based or 1-based indices on disk.
- **Version counter files**: `<thing>_version.txt` — integer, trailing newline? Does it bump on every mutation or only on structural changes?
- **Atomicity model**: `.tmp` sibling directories, ordering of fsyncs, rename semantics.
- **Extensibility**: how unknown files/keys must be handled by readers (ignore vs fail).

Each section must cite the specific lines in `files_format.jl` it summarizes.

- [ ] **Step 3: Commit (dev repo)**

```bash
cd ~/src/dafr-native/dev
git add specs/filesdaf-on-disk-spec-draft.md
git commit -m "spec: FilesDaf on-disk format (draft for upstream review)"
```

### Task G2: Upstream the spec to DataAxesFormats.jl

**Files:**
- (no local file changes — PR against the Julia repo)

- [ ] **Step 1: Fork / branch DataAxesFormats.jl**

```bash
cd ~/src/DataAxesFormats.jl
git checkout -b docs/filesdaf-on-disk-spec
```

- [ ] **Step 2: Copy the draft spec into Julia repo**

```bash
mkdir -p ~/src/DataAxesFormats.jl/docs/src/file_specs
/bin/cp ~/src/dafr-native/dev/specs/filesdaf-on-disk-spec-draft.md \
        ~/src/DataAxesFormats.jl/docs/src/file_specs/filesdaf-on-disk.md
```

- [ ] **Step 3: Edit in the Julia repo**

Tweak header and cross-links so it reads as a Julia-repo doc (add it to `docs/make.jl` if the project uses Documenter).

- [ ] **Step 4: Commit and open PR**

```bash
cd ~/src/DataAxesFormats.jl
git add docs/src/file_specs/filesdaf-on-disk.md docs/make.jl
git commit -m "docs: on-disk spec for FilesDaf backend"
git push -u origin docs/filesdaf-on-disk-spec
# then open a PR via `gh pr create`
```

Do not merge; wait for Oren's review. Track the PR URL in `~/src/dafr-native/dev/specs/filesdaf-on-disk-spec-draft.md` (add a top header: "Upstream PR: <URL>").

- [ ] **Step 5: Record PR URL locally**

Once PR URL is available, edit `dev/specs/filesdaf-on-disk-spec-draft.md` to prepend:

```markdown
> Upstream PR: https://github.com/tanaylab/DataAxesFormats.jl/pull/XXX (open)
```

Commit.

---

## Phase H — CI

### Task H1: GitHub Actions `R-CMD-check`

**Files:**
- Create: `~/src/dafr-native/.github/workflows/R-CMD-check.yaml`

- [ ] **Step 1: Write workflow**

```yaml
name: R-CMD-check

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  R-CMD-check:
    runs-on: ${{ matrix.config.os }}

    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: ubuntu-latest, r: 'release'}
          - {os: macos-latest, r: 'release'}
          - {os: windows-latest, r: 'release'}

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          http-user-agent: ${{ matrix.config.http-user-agent }}
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck, any::cpp11, any::roxygen2
          needs: check

      - uses: r-lib/actions/check-r-package@v2
        with:
          upload-snapshots: true
          args: 'c("--no-manual", "--as-cran")'
          error-on: '"warning"'
```

- [ ] **Step 2: Commit**

```bash
cd ~/src/dafr-native
git add .github/workflows/R-CMD-check.yaml
git commit -m "ci: R CMD check on linux/mac/windows"
```

### Task H2: ALTREP sanity job

**Files:**
- Create: `~/src/dafr-native/.github/workflows/altrep-sanity.yaml`

- [ ] **Step 1: Write workflow**

```yaml
name: altrep-sanity

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  altrep:
    runs-on: ubuntu-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: |
            any::cpp11
            any::roxygen2
            any::devtools
            any::Matrix
            any::bench
            any::withr
            any::testthat

      - name: Build + test ALTREP subset
        run: |
          Rscript -e 'pkgbuild::clean_dll(); pkgbuild::compile_dll(debug = FALSE)'
          Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat", filter = "mmap|altrep")'
```

- [ ] **Step 2: Commit**

```bash
cd ~/src/dafr-native
git add .github/workflows/altrep-sanity.yaml
git commit -m "ci: ALTREP-focused sanity job"
```

### Task H3: Nightly benchmark job (non-gating)

**Files:**
- Create: `~/src/dafr-native/.github/workflows/bench.yaml`

- [ ] **Step 1: Write workflow**

```yaml
name: bench

on:
  schedule:
    - cron: '17 4 * * *'   # nightly 04:17 UTC
  workflow_dispatch: {}

jobs:
  bench:
    runs-on: ubuntu-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::cpp11, any::bench, any::Matrix, any::withr

      - name: Build
        run: Rscript -e 'pkgbuild::clean_dll(); pkgbuild::compile_dll(debug = FALSE)'

      - name: Run Slice 0 benchmarks
        run: Rscript -e 'devtools::load_all(); source("inst/benchmarks/bench.R")' -- --small

      - uses: actions/upload-artifact@v4
        with:
          name: bench-csv
          path: dev/benchmarks/slice-0-baseline-*.csv
```

- [ ] **Step 2: Commit**

```bash
cd ~/src/dafr-native
git add .github/workflows/bench.yaml
git commit -m "ci: nightly Slice-0 benchmarks (non-gating)"
```

---

## Phase I — Slice 0 exit gate

### Task I1: Full suite run + decision document

**Files:**
- Create: `~/src/dafr-native/dev/notes/slice-0-exit.md`

- [ ] **Step 1: Run the full local test suite**

```r
devtools::load_all("~/src/dafr-native")
alutil::tst(parallel = TRUE)
```
Expected: all tests pass (downstream-compat tests skip if Seurat/scran unavailable).

- [ ] **Step 2: Re-run the Slice-0 benchmarks and bake-off**

```bash
cd ~/src/dafr-native
SMALL=1 Rscript inst/benchmarks/bench.R --small
SMALL=1 Rscript dev/benchmarks/run-bakeoff.R
```
Expected: both scripts complete and produce CSVs.

- [ ] **Step 3: Write exit note**

`~/src/dafr-native/dev/notes/slice-0-exit.md`:

```markdown
# Slice 0 exit gate — <date>

## Deliverables

- [x] Package scaffold (DESCRIPTION, LICENSE, configure, Makevars, NAMESPACE)
- [x] S7 class hierarchy (DafReader / DafReadOnly / DafWriter)
- [x] ~40 format API S7 generics declared
- [x] Cache skeleton (3-tier env, version counters, empty_cache)
- [x] Handler registration + options framework
- [x] MmapRegion RAII
- [x] ALTREP classes for mmap-backed double/int/logical
- [x] mmap_dgCMatrix helper
- [x] Matrix-compat smoke tests (colSums, rowSums, t, index-assign)
- [x] Seurat/scran compat findings recorded
- [x] Benchmark harness (workload generator + bench.R)
- [x] Bake-off (cpp11+BLAS vs RcppEigen) — decision recorded
- [x] FilesDaf on-disk spec draft + upstream PR open
- [x] CI workflows: R CMD check, ALTREP sanity, nightly bench

## Outstanding risks / follow-ups

- <list any that arose>

## Decision to enter Slice 1

- Go / No-go: <decision>
```

- [ ] **Step 4: Commit (dev repo)**

```bash
cd ~/src/dafr-native/dev
git add notes/slice-0-exit.md
git commit -m "notes: Slice 0 exit gate"
```

- [ ] **Step 5: Tag the package repo**

```bash
cd ~/src/dafr-native
git tag -a slice-0 -m "Slice 0: scaffold, POC, benchmarks, bake-off"
```

Slice 0 complete. Next plan: **Slice 1 — MemoryDaf + axes + scalars/vectors/matrices get/set + cache infrastructure**.

---

## Self-review

### 1. Spec coverage

Spec Section → Task mapping:

- **§3 MemoryDaf / FilesDaf / Zarr hook** → Slice 1 / 2 / 8 (not in this plan). Slice 0 stubs classes only — covered by Task B1, B3.
- **§4 S7 class skeleton** → Task B1.
- **§4 Format API hooks** → Task B2.
- **§5 FilesDaf on-disk spec** → Task G1, G2.
- **§5 ALTREP mmap** → Task C2, D1, D2.
- **§5 `dgCMatrix` with ALTREP slots** → Task D3.
- **§5 Fallback path** → Option `dafr.mmap = FALSE` declared in Task A6 `R/options.R`; actual code path lands in Slice 2 when `FilesDaf` exists.
- **§6 C++ stack** → Phase C, F.
- **§6 Benchmark harness** → Phase E.
- **§6 Bake-off** → Phase F.
- **§6 CRAN compile-time budget** → Task A3 / A4 (sensible defaults; CRAN-specific tightening belongs to a pre-release slice).
- **§7 Caching (3-tier)** → Task B3 skeleton; full LRU + memory cap in Slice 1.
- **§7 Version counters** → Task B3.
- **§7 Options/handlers** → Task A6.
- **§12 Package scaffold** → Phases A, B.
- **§13 Dev workflow** → called out at top; each task uses `devtools::load_all()` / `pkgbuild::compile_dll()`.
- **§14 Testing infrastructure** → Phase H (CI) + testthat files throughout.
- **§16 Risks (R1 ALTREP breakage)** → Task D4.
- **§16 Risks (R2 sparse matvec perf)** → Phase F bake-off; transpose is an acceptable proxy.
- **§16 Risks (R4 FilesDaf spec implicit)** → Phase G.

**Gaps I deliberately did not close in Slice 0:**

- Full LRU eviction under the memory cap — needs `MemoryData` content, which requires `MemoryDaf` from Slice 1.
- Goldens regenerator script — needs `FilesDaf` writer; lands in Slice 2.
- Property-based tests — land per-subsystem from Slice 1 onward.
- Writers phase (readers.R, writers.R) — Slice 1.

### 2. Placeholder scan

Searched for "TBD", "TODO", "FIXME", "implement later", "fill in", "appropriate", "similar to task", "handle edge cases" — none found in task bodies. Spec-level TBDs are explicit ("TBD in Slice 0" for bake-off ratios), which is correct.

### 3. Type / name consistency

Spot-checked:

- `make_mmap_real_altrep` / `make_mmap_int_altrep` / `make_mmap_lgl_altrep` — declared in `altrep_mmap.h`, called from `altrep_mmap_r.cpp`. Match.
- `mmap_real` / `mmap_int` / `mmap_lgl` — R wrappers in `R/altrep.R`, call `mmap_*_altrep_cpp`. Match.
- `mmap_dgCMatrix` — defined in `R/mmap.R`, referenced from tests D3/D4. Match.
- `register_dafr_handler` — defined in `R/handlers.R`, exported via roxygen in Task A6. Match.
- `empty_cache` — defined in `R/cache.R`, exported; used in spec, tasks downstream.
- `dafr_opt` / `.dafr_default_options` — defined in `R/options.R`; `set_default_options()` called in `zzz.R` `.onLoad`. Match.
- `DAFR_PARALLEL_FOR` macro — defined in `openmp_shim.h`, used in kernel_eltwise_log_add.cpp and kernel_csc_colsums.cpp. Match.
- `MmapRegion::open_readonly` — header + impl match. `wrap_region` / `unwrap_region` internal helpers consistent.

No inconsistencies found.
