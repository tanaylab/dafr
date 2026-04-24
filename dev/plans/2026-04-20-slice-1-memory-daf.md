# Slice 1 — MemoryDaf + user-facing API + cache infrastructure

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship a working `MemoryDaf` backend with the complete scalar/axis/vector/matrix user-facing API, LRU + version-stamp cache invalidation, and a ported testthat suite validating both backend hooks and the public surface.

**Architecture:**
- `R/memory_daf.R` — concrete `MemoryDaf` S7 class under `DafWriter`; implements all 22 `format_*` generics (declared in `R/format_api.R`) over nested R environments keyed by axis/name/type.
- `R/readers.R` / `R/writers.R` — user-facing exported wrappers (`get_vector`, `set_matrix`, `add_axis`, …) layering caching, version-bumps, dimnames, layout fallback, default-NA handling, and error messages on top of the format layer.
- `R/cache.R` — extended from Slice 0 skeleton with memory-cap + LRU eviction, per-key version stamps, and stamp-check on read. Three tiers stay as env-of-envs; memory/query tiers share the LRU list; mapped tier exempt from eviction (OS owns it).

**Tech Stack:** R 4.4+, S7 0.2.1, cpp11 (for later slices), `cli` (user messages — closes unused-import NOTE), `bit64` (int64 vector type — closes unused-import NOTE), base `Matrix` (dgCMatrix/lgCMatrix), testthat 3.x.

**Repo layout:**
- Package repo: `/home/aviezerl/src/dafr-native/` (also reachable at `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/`). Branch `main`, tracking `origin/main` at `git@github.com:tanaylab/dafr.git` (private). Tag `slice-0` marks Slice 0 exit.
- Dev repo (nested, gitignored by the package repo): `/home/aviezerl/src/dafr-native/dev/`. Plans + notes + specs + benchmarks live here.
- Commit destination is **inferred from the path being changed** — source + tests → package repo; plans/notes → dev repo. Use `cd ~/src/dafr-native` or `cd ~/src/dafr-native/dev` explicitly in commit steps.

**Reference test suite:** `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr/tests/testthat/` (the renamed `dafJuliaWrapper` package — same tests, package renamed). Keep data-model tests; drop Julia-bridge-mechanics tests.

**Dev loop each task:**
1. From the package root: `Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "<tag>")'`
2. Inspect output; iterate until green.
3. Stage + commit with the provided message template.

---

## Scope decisions (made before planning)

Closed in Slice 1 (per kickoff breadcrumb "Still open from Slice 0"):

- `bit64` + `cli` first real uses (Task N1, Task N2).
- Rd undocumented-args WARNING on S7 classes (partially closed in Slice 0; remaining properties documented in Task A1 / Task I4).
- `is_altrep()` helper — relocate to `tests/testthat/helper-altrep.R` so tests stop depending on `dafr:::is_altrep` (Task N3).
- Comments beside each `const_cast<void*>` in `mmap_region.cpp` / `altrep_mmap.cpp` (Task N4).
- One OpenMP-parallel-branch test per kernel (Task N5).

Deferred to Slice 2 or later:

- Phase G2 upstream PR for FilesDaf on-disk spec (pending user consent).
- Transpose kernel re-evaluation (real-world transpose usage still not materialized).
- CSC colSums bake-off re-run at 100M+ nnz (needs dataset larger than SMALL=1).
- Julia FilesDaf "no on-disk version counters" / "no atomicity" findings — only relevant when FilesDaf backend lands.
- `writeBin(..., size=8L)` endianness pin — FilesDaf concern, Slice 2.
- Long-vector (>2^31) ALTREP scenarios and "file truncated while R vector live" — later slices.

---

## File structure

Created in this slice:

- `R/memory_daf.R` — `MemoryDaf` class + 22 `format_*` S7 methods.
- `R/readers.R` — read-side exported API: `has_scalar`, `get_scalar`, `scalars_set`, `has_axis`, `axes_set`, `axis_length`, `axis_vector`, `axis_entries`, `axis_indices`, `axis_dict`, `has_vector`, `get_vector`, `vectors_set`, `has_matrix`, `get_matrix`, `matrices_set`, `description`.
- `R/writers.R` — write-side exported API: `set_scalar`, `delete_scalar`, `add_axis`, `delete_axis`, `set_vector`, `delete_vector`, `set_matrix`, `delete_matrix`, `relayout_matrix`.
- `tests/testthat/test-memory-daf.R` — native-only unit tests for MemoryDaf class shape + constructor.
- `tests/testthat/test-memory-scalars.R`, `test-memory-axes.R`, `test-memory-vectors.R`, `test-memory-matrices.R`, `test-memory-cache.R` — ported + extended tests for the user-facing API against MemoryDaf.
- `tests/testthat/helper-altrep.R` — relocated `is_altrep()` helper.
- `dev/notes/slice-1-exit.md` — exit gate doc.

Modified:

- `R/cache.R` — memory-cap + LRU + version-stamp machinery.
- `R/classes.R` — roxygen cleanup for remaining undocumented-args warnings.
- `R/altrep.R` (or wherever `is_altrep` lives) — remove/rename helper; update callers.
- `src/mmap_region.cpp`, `src/altrep_mmap.cpp` — comments beside `const_cast<void*>` uses.
- `NAMESPACE` — regenerated by roxygen2.
- `DESCRIPTION` — no change expected (bit64, cli already listed; confirm).

---

## Phase A — MemoryDaf class + constructor

### Task A1: Declare MemoryDaf S7 class and constructor

**Files:**
- Create: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-daf.R`

- [ ] **Step 1: Write the failing test**

`tests/testthat/test-memory-daf.R`:

```r
test_that("memory_daf() returns a DafWriter with the right class ancestry", {
  d <- memory_daf()
  expect_s3_class(d, "dafr::MemoryDaf")
  expect_true(inherits(d, "dafr::DafWriter"))
  expect_true(inherits(d, "dafr::DafReader"))
})

test_that("memory_daf() default name is 'memory'", {
  expect_equal(S7::prop(memory_daf(), "name"), "memory")
})

test_that("memory_daf(name = ...) sets the name", {
  expect_equal(S7::prop(memory_daf(name = "test!"), "name"), "test!")
})

test_that("memory_daf() initialises empty scalars/axes/vectors/matrices envs", {
  d <- memory_daf()
  internal <- S7::prop(d, "internal")
  expect_true(is.environment(internal$scalars))
  expect_true(is.environment(internal$axes))
  expect_true(is.environment(internal$vectors))
  expect_true(is.environment(internal$matrices))
  expect_equal(length(ls(internal$scalars, all.names = TRUE)), 0L)
  expect_equal(length(ls(internal$axes, all.names = TRUE)), 0L)
  expect_equal(length(ls(internal$vectors, all.names = TRUE)), 0L)
  expect_equal(length(ls(internal$matrices, all.names = TRUE)), 0L)
})

test_that("memory_daf() gets fresh cache / counter envs per instance", {
  a <- memory_daf()
  b <- memory_daf()
  expect_false(identical(S7::prop(a, "cache"),                  S7::prop(b, "cache")))
  expect_false(identical(S7::prop(a, "axis_version_counter"),   S7::prop(b, "axis_version_counter")))
  expect_false(identical(S7::prop(a, "vector_version_counter"), S7::prop(b, "vector_version_counter")))
  expect_false(identical(S7::prop(a, "matrix_version_counter"), S7::prop(b, "matrix_version_counter")))
})
```

- [ ] **Step 2: Run tests — expect failure**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "memory-daf")'
```

Expected: all four fail with `could not find function "memory_daf"`.

- [ ] **Step 3: Implement `MemoryDaf` + `memory_daf()`**

`R/memory_daf.R`:

```r
#' In-memory Daf store.
#'
#' A concrete `DafWriter` backed entirely by R environments — no disk,
#' no mmap. Scalars, axes, vectors, and matrices live in nested
#' environments (hash tables) under the `internal` property:
#'
#' - `internal$scalars`     : `env(name -> value)`
#' - `internal$axes`        : `env(axis -> list(entries = character, dict = env))`
#' - `internal$vectors`     : `env(axis -> env(name -> vector))`
#' - `internal$matrices`    : `env(rows_axis -> env(cols_axis -> env(name -> matrix)))`
#'
#' @param name Human-readable identifier. Defaults to `"memory"`.
#' @return A `MemoryDaf` instance.
#' @export
#' @examples
#' d <- memory_daf(name = "scratch")
#' add_axis(d, "cell", c("A", "B", "C"))
#' set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
memory_daf <- function(name = "memory") {
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  internal <- new_internal_env()
  internal$scalars  <- new.env(parent = emptyenv())
  internal$axes     <- new.env(parent = emptyenv())
  internal$vectors  <- new.env(parent = emptyenv())
  internal$matrices <- new.env(parent = emptyenv())
  MemoryDaf(
    name                   = name,
    internal               = internal,
    cache                  = new_cache_env(),
    axis_version_counter   = new_counter_env(),
    vector_version_counter = new_counter_env(),
    matrix_version_counter = new_counter_env()
  )
}

#' Concrete `DafWriter` backed by R environments (no I/O).
#'
#' Use `memory_daf()` to construct instances — the S7 constructor is
#' exported only for `isVirtualClass`-style checks.
#'
#' @inheritParams DafReader
#' @export
MemoryDaf <- S7::new_class(
  name    = "MemoryDaf",
  package = "dafr",
  parent  = DafWriter
)
```

- [ ] **Step 4: Run tests — expect pass**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::document(); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "memory-daf")'
```

Expected: 4/4 pass.

- [ ] **Step 5: Commit (package repo)**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R NAMESPACE man/memory_daf.Rd man/MemoryDaf.Rd tests/testthat/test-memory-daf.R
git commit -m "feat(memory_daf): add MemoryDaf S7 class + constructor"
```

---

## Phase B — Axes on MemoryDaf + user-facing

### Task B1: format_has_axis / format_axes_set / format_axis_length / format_axis_array / format_axis_dict

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-axes.R`

- [ ] **Step 1: Write the failing test**

`tests/testthat/test-memory-axes.R`:

```r
test_that("format_has_axis reflects added axes", {
  d <- memory_daf()
  expect_false(format_has_axis(d, "cell"))
  d@internal$axes$cell <- list(
    entries = c("A", "B"),
    dict    = list2env(list(A = 1L, B = 2L), parent = emptyenv())
  )
  expect_true(format_has_axis(d, "cell"))
})

test_that("format_axes_set returns sorted character vector of axis names", {
  d <- memory_daf()
  d@internal$axes$gene <- list(entries = character(), dict = new.env(parent = emptyenv()))
  d@internal$axes$cell <- list(entries = character(), dict = new.env(parent = emptyenv()))
  expect_equal(format_axes_set(d), c("cell", "gene"))
})

test_that("format_axis_length + format_axis_array + format_axis_dict are consistent", {
  d <- memory_daf()
  dict <- list2env(list(A = 1L, B = 2L, C = 3L), parent = emptyenv())
  d@internal$axes$cell <- list(entries = c("A", "B", "C"), dict = dict)
  expect_equal(format_axis_length(d, "cell"), 3L)
  expect_equal(format_axis_array(d, "cell"),  c("A", "B", "C"))
  expect_identical(format_axis_dict(d, "cell"), dict)
})

test_that("format_axis_* reject unknown axis", {
  d <- memory_daf()
  expect_error(format_axis_length(d, "cell"), "does not exist")
  expect_error(format_axis_array(d, "cell"),  "does not exist")
  expect_error(format_axis_dict(d, "cell"),   "does not exist")
})
```

- [ ] **Step 2: Run — expect fail**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "memory-axes")'
```

Expected: all 4 fail — `unable to find an inherited method`.

- [ ] **Step 3: Implement the five query methods**

Append to `R/memory_daf.R`:

```r
# ---- Axes: query methods ----------------------------------------------------

S7::method(format_has_axis, MemoryDaf) <- function(daf, axis) {
  exists(axis, envir = S7::prop(daf, "internal")$axes, inherits = FALSE)
}

S7::method(format_axes_set, MemoryDaf) <- function(daf) {
  nms <- ls(S7::prop(daf, "internal")$axes, all.names = TRUE)
  sort(nms)
}

.memory_axis <- function(daf, axis) {
  axes <- S7::prop(daf, "internal")$axes
  if (!exists(axis, envir = axes, inherits = FALSE)) {
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  get(axis, envir = axes, inherits = FALSE)
}

S7::method(format_axis_length, MemoryDaf) <- function(daf, axis) {
  length(.memory_axis(daf, axis)$entries)
}

S7::method(format_axis_array, MemoryDaf) <- function(daf, axis) {
  .memory_axis(daf, axis)$entries
}

S7::method(format_axis_dict, MemoryDaf) <- function(daf, axis) {
  .memory_axis(daf, axis)$dict
}
```

- [ ] **Step 4: Run — expect pass**

Expected: 4/4 pass.

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-axes.R
git commit -m "feat(memory_daf): axis query methods (has/set/length/array/dict)"
```

### Task B2: format_add_axis / format_delete_axis

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-axes.R`

- [ ] **Step 1: Write failing tests (append)**

```r
test_that("format_add_axis stores entries + builds a 1-based index dict", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A", "B", "C"))
  expect_equal(format_axis_array(d, "cell"),  c("A", "B", "C"))
  expect_equal(format_axis_length(d, "cell"), 3L)
  dict <- format_axis_dict(d, "cell")
  expect_equal(dict$A, 1L)
  expect_equal(dict$B, 2L)
  expect_equal(dict$C, 3L)
})

test_that("format_add_axis bumps the axis version counter", {
  d <- memory_daf()
  counters <- S7::prop(d, "axis_version_counter")
  expect_null(counters$cell)
  format_add_axis(d, "cell", c("A"))
  expect_equal(counters$cell, 1L)
})

test_that("format_add_axis rejects duplicate axis", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A"))
  expect_error(format_add_axis(d, "cell", c("A")), "already exists")
})

test_that("format_add_axis rejects duplicate / NA / empty entries", {
  d <- memory_daf()
  expect_error(format_add_axis(d, "cell", c("A", "A")),    "duplicate")
  expect_error(format_add_axis(d, "cell", c("A", NA)),     "NA")
  expect_error(format_add_axis(d, "cell", c("A", "")),     "empty")
  expect_error(format_add_axis(d, "cell", integer(0)),     "character")
})

test_that("format_delete_axis removes axis + bumps counter", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A"))
  counters <- S7::prop(d, "axis_version_counter")
  stamp <- counters$cell
  format_delete_axis(d, "cell", must_exist = TRUE)
  expect_false(format_has_axis(d, "cell"))
  expect_gt(counters$cell, stamp)
})

test_that("format_delete_axis with must_exist=FALSE ignores missing", {
  d <- memory_daf()
  expect_silent(format_delete_axis(d, "cell", must_exist = FALSE))
})

test_that("format_delete_axis with must_exist=TRUE errors on missing", {
  d <- memory_daf()
  expect_error(format_delete_axis(d, "cell", must_exist = TRUE), "does not exist")
})

test_that("format_delete_axis also removes vectors/matrices on that axis", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A", "B"))
  format_add_axis(d, "gene", c("X", "Y"))
  vectors <- S7::prop(d, "internal")$vectors
  vectors$cell <- new.env(parent = emptyenv())
  vectors$cell$score <- c(1.0, 2.0)
  matrices <- S7::prop(d, "internal")$matrices
  matrices$cell <- new.env(parent = emptyenv())
  matrices$cell$gene <- new.env(parent = emptyenv())
  matrices$cell$gene$UMIs <- matrix(0, 2, 2)
  format_delete_axis(d, "cell", must_exist = TRUE)
  expect_false(exists("cell", envir = vectors, inherits = FALSE))
  expect_false(exists("cell", envir = matrices, inherits = FALSE))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/memory_daf.R`:

```r
# ---- Axes: mutation ---------------------------------------------------------

S7::method(format_add_axis, MemoryDaf) <- function(daf, axis, entries) {
  if (!is.character(entries)) {
    stop("entries must be a character vector", call. = FALSE)
  }
  if (anyNA(entries)) {
    stop(sprintf("axis %s entries contain NA", sQuote(axis)), call. = FALSE)
  }
  if (any(!nzchar(entries))) {
    stop(sprintf("axis %s entries contain empty strings", sQuote(axis)), call. = FALSE)
  }
  if (anyDuplicated(entries)) {
    dup <- entries[duplicated(entries)][1L]
    stop(sprintf("axis %s has duplicate entry %s", sQuote(axis), sQuote(dup)), call. = FALSE)
  }
  axes <- S7::prop(daf, "internal")$axes
  if (exists(axis, envir = axes, inherits = FALSE)) {
    stop(sprintf("axis %s already exists", sQuote(axis)), call. = FALSE)
  }
  dict <- new.env(parent = emptyenv(), size = length(entries))
  for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
  assign(axis, list(entries = entries, dict = dict), envir = axes)
  bump_axis_counter(daf, axis)
  invisible()
}

S7::method(format_delete_axis, MemoryDaf) <- function(daf, axis, must_exist) {
  internal <- S7::prop(daf, "internal")
  if (!exists(axis, envir = internal$axes, inherits = FALSE)) {
    if (must_exist) {
      stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
    }
    return(invisible())
  }
  # Drop axis + its dependent vectors + matrix rows + matrix cols
  rm(list = axis, envir = internal$axes)
  if (exists(axis, envir = internal$vectors, inherits = FALSE)) {
    rm(list = axis, envir = internal$vectors)
  }
  if (exists(axis, envir = internal$matrices, inherits = FALSE)) {
    rm(list = axis, envir = internal$matrices)
  }
  for (rows in ls(internal$matrices, all.names = TRUE)) {
    cols_env <- get(rows, envir = internal$matrices, inherits = FALSE)
    if (exists(axis, envir = cols_env, inherits = FALSE)) {
      rm(list = axis, envir = cols_env)
    }
  }
  bump_axis_counter(daf, axis)
  invisible()
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-axes.R
git commit -m "feat(memory_daf): axis add/delete with entry validation + cascading removal"
```

### Task B3: User-facing axis API (readers.R + writers.R)

**Files:**
- Create: `/home/aviezerl/src/dafr-native/R/readers.R`
- Create: `/home/aviezerl/src/dafr-native/R/writers.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-axes.R`

- [ ] **Step 1: Write failing tests (append)**

```r
test_that("add_axis + has_axis + axes_set compose", {
  d <- memory_daf()
  expect_false(has_axis(d, "cell"))
  add_axis(d, "cell", c("A", "B"))
  expect_true(has_axis(d, "cell"))
  expect_equal(axes_set(d), "cell")
})

test_that("axis_length / axis_vector / axis_entries mirror Julia semantics", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  expect_equal(axis_length(d, "cell"), 3L)
  expect_equal(axis_vector(d, "cell"), c("A", "B", "C"))
  expect_equal(axis_entries(d, "cell"), c("A", "B", "C"))
  expect_equal(axis_entries(d, "cell", 2L), "B")
  expect_equal(axis_entries(d, "cell", c(1L, 3L)), c("A", "C"))
  expect_error(axis_entries(d, "cell", "A"),  "integer")
  expect_error(axis_entries(d, "cell", 5L),   "out of range")
  expect_error(axis_entries(d, "cell", -1L),  "out of range")
})

test_that("axis_vector default handling", {
  d <- memory_daf()
  expect_error(axis_vector(d, "cell"), "does not exist")
  expect_null(axis_vector(d, "cell", null_if_missing = TRUE))
})

test_that("axis_indices maps entries to 1-based positions", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  expect_equal(axis_indices(d, "cell", c("A", "C")), c(1L, 3L))
  expect_equal(axis_indices(d, "cell", "B"),         2L)
  expect_error(axis_indices(d, "cell", c(1L, 2L)),   "character")
  expect_error(axis_indices(d, "cell", c("A", "Z")), "not found")
})

test_that("axis_dict is queryable by [[", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  dd <- axis_dict(d, "cell")
  expect_equal(dd[["A"]], 1L)
  expect_equal(dd[["B"]], 2L)
})

test_that("delete_axis composes with axes_set", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  delete_axis(d, "cell")
  expect_equal(length(axes_set(d)), 0L)
  expect_error(delete_axis(d, "cell"),                          "does not exist")
  expect_silent(delete_axis(d, "cell", must_exist = FALSE))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement readers.R + writers.R**

`R/readers.R`:

```r
#' Test whether an axis exists.
#' @param daf A `DafReader`.
#' @param axis Axis name (character scalar).
#' @return Logical scalar.
#' @export
has_axis <- function(daf, axis) {
  stopifnot(is.character(axis), length(axis) == 1L, !is.na(axis))
  format_has_axis(daf, axis)
}

#' Names of all axes, sorted.
#' @inheritParams has_axis
#' @return Character vector of axis names.
#' @export
axes_set <- function(daf) format_axes_set(daf)

#' Length (entry count) of an axis.
#' @inheritParams has_axis
#' @return Integer scalar.
#' @export
axis_length <- function(daf, axis) {
  stopifnot(is.character(axis), length(axis) == 1L, !is.na(axis))
  format_axis_length(daf, axis)
}

#' Entry-name vector for an axis.
#'
#' @inheritParams has_axis
#' @param null_if_missing If `TRUE`, return `NULL` when the axis is
#'   absent instead of raising.
#' @return Character vector of entry names.
#' @export
axis_vector <- function(daf, axis, null_if_missing = FALSE) {
  stopifnot(is.character(axis), length(axis) == 1L, !is.na(axis))
  if (!format_has_axis(daf, axis)) {
    if (isTRUE(null_if_missing)) return(NULL)
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  format_axis_array(daf, axis)
}

#' Entry names of an axis (full or by index).
#'
#' @inheritParams has_axis
#' @param indices Optional integer index vector (1-based).
#' @return Character vector.
#' @export
axis_entries <- function(daf, axis, indices = NULL) {
  entries <- axis_vector(daf, axis)
  if (is.null(indices)) return(entries)
  if (!is.integer(indices) && !(is.numeric(indices) && all(indices == as.integer(indices)))) {
    stop("indices must be an integer vector", call. = FALSE)
  }
  indices <- as.integer(indices)
  if (any(indices < 1L | indices > length(entries))) {
    stop(sprintf("indices out of range [1, %d]", length(entries)), call. = FALSE)
  }
  entries[indices]
}

#' Look up 1-based positions of entries in an axis.
#'
#' @inheritParams has_axis
#' @param entries Character vector of entry names to resolve.
#' @return Integer vector of 1-based positions; same length as `entries`.
#' @export
axis_indices <- function(daf, axis, entries) {
  if (!is.character(entries)) stop("entries must be a character vector", call. = FALSE)
  dict <- format_axis_dict(daf, axis)
  out <- vapply(entries, function(nm) {
    v <- dict[[nm]]
    if (is.null(v)) NA_integer_ else as.integer(v)
  }, integer(1L), USE.NAMES = FALSE)
  missing <- is.na(out)
  if (any(missing)) {
    stop(sprintf("entries not found in axis %s: %s",
                 sQuote(axis),
                 paste(sQuote(entries[missing]), collapse = ", ")),
         call. = FALSE)
  }
  out
}

#' Entry-name to 1-based-index hash for an axis.
#' @inheritParams has_axis
#' @return An environment mapping entry names to integer positions.
#' @export
axis_dict <- function(daf, axis) format_axis_dict(daf, axis)
```

`R/writers.R`:

```r
#' Add a new axis.
#'
#' @param daf A `DafWriter`.
#' @param axis Axis name.
#' @param entries Unique, non-NA, non-empty character vector of entry names.
#' @return Invisibly the input `daf`.
#' @export
add_axis <- function(daf, axis, entries) {
  stopifnot(is.character(axis),   length(axis) == 1L, !is.na(axis))
  stopifnot(is.character(entries))
  format_add_axis(daf, axis, entries)
  invisible(daf)
}

#' Delete an axis (and all vectors / matrices that depend on it).
#'
#' @inheritParams add_axis
#' @param must_exist If `TRUE` (default) raise when the axis is absent;
#'   if `FALSE` silently no-op.
#' @return Invisibly the input `daf`.
#' @export
delete_axis <- function(daf, axis, must_exist = TRUE) {
  stopifnot(is.character(axis), length(axis) == 1L, !is.na(axis))
  stopifnot(is.logical(must_exist), length(must_exist) == 1L, !is.na(must_exist))
  format_delete_axis(daf, axis, must_exist)
  invisible(daf)
}
```

- [ ] **Step 4: Run — expect pass**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::document(); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "memory-axes")'
```

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/readers.R R/writers.R NAMESPACE man/*.Rd tests/testthat/test-memory-axes.R
git commit -m "feat(api): user-facing axis API (add/delete/has/set/length/vector/entries/indices/dict)"
```

---

## Phase C — Scalars on MemoryDaf + user-facing

### Task C1: format_has_scalar / format_get_scalar / format_scalars_set

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-scalars.R` (create)

- [ ] **Step 1: Write failing test**

```r
test_that("format_has_scalar / format_get_scalar / format_scalars_set query scalars env", {
  d <- memory_daf()
  expect_false(format_has_scalar(d, "pi"))
  expect_equal(format_scalars_set(d), character(0L))
  d@internal$scalars$pi <- 3.14
  expect_true(format_has_scalar(d, "pi"))
  expect_equal(format_get_scalar(d, "pi"), 3.14)
  expect_equal(format_scalars_set(d), "pi")
})

test_that("format_get_scalar errors on unknown name", {
  d <- memory_daf()
  expect_error(format_get_scalar(d, "pi"), "does not exist")
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/memory_daf.R`:

```r
# ---- Scalars: query ---------------------------------------------------------

S7::method(format_has_scalar, MemoryDaf) <- function(daf, name) {
  exists(name, envir = S7::prop(daf, "internal")$scalars, inherits = FALSE)
}

S7::method(format_get_scalar, MemoryDaf) <- function(daf, name) {
  scalars <- S7::prop(daf, "internal")$scalars
  if (!exists(name, envir = scalars, inherits = FALSE)) {
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
  }
  get(name, envir = scalars, inherits = FALSE)
}

S7::method(format_scalars_set, MemoryDaf) <- function(daf) {
  sort(ls(S7::prop(daf, "internal")$scalars, all.names = TRUE))
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-scalars.R
git commit -m "feat(memory_daf): scalar query methods (has/get/scalars_set)"
```

### Task C2: format_set_scalar / format_delete_scalar

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-scalars.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("format_set_scalar stores new scalars and respects overwrite=FALSE", {
  d <- memory_daf()
  format_set_scalar(d, "foo", "bar", overwrite = FALSE)
  expect_equal(format_get_scalar(d, "foo"), "bar")
  expect_error(format_set_scalar(d, "foo", "baz", overwrite = FALSE), "already exists")
  expect_equal(format_get_scalar(d, "foo"), "bar")
})

test_that("format_set_scalar with overwrite=TRUE replaces value", {
  d <- memory_daf()
  format_set_scalar(d, "foo", "bar", overwrite = FALSE)
  format_set_scalar(d, "foo", "baz", overwrite = TRUE)
  expect_equal(format_get_scalar(d, "foo"), "baz")
})

test_that("format_set_scalar rejects NA, NULL, and length != 1", {
  d <- memory_daf()
  expect_error(format_set_scalar(d, "foo", NA,              overwrite = FALSE), "NA")
  expect_error(format_set_scalar(d, "foo", NULL,            overwrite = FALSE), "scalar")
  expect_error(format_set_scalar(d, "foo", c("a", "b"),     overwrite = FALSE), "length 1")
  expect_error(format_set_scalar(d, "foo", list(1),         overwrite = FALSE), "atomic")
})

test_that("format_delete_scalar removes + respects must_exist", {
  d <- memory_daf()
  format_set_scalar(d, "foo", "bar", overwrite = FALSE)
  format_delete_scalar(d, "foo", must_exist = TRUE)
  expect_false(format_has_scalar(d, "foo"))
  expect_error (format_delete_scalar(d, "foo", must_exist = TRUE),  "does not exist")
  expect_silent(format_delete_scalar(d, "foo", must_exist = FALSE))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append:

```r
# ---- Scalars: mutation ------------------------------------------------------

.assert_scalar_value <- function(name, value) {
  if (is.null(value)) {
    stop(sprintf("scalar %s value may not be NULL", sQuote(name)), call. = FALSE)
  }
  if (!is.atomic(value)) {
    stop(sprintf("scalar %s value must be an atomic scalar", sQuote(name)), call. = FALSE)
  }
  if (length(value) != 1L) {
    stop(sprintf("scalar %s value must have length 1 (got %d)", sQuote(name), length(value)), call. = FALSE)
  }
  if (is.na(value)) {
    stop(sprintf("scalar %s value may not be NA", sQuote(name)), call. = FALSE)
  }
  invisible()
}

S7::method(format_set_scalar, MemoryDaf) <- function(daf, name, value, overwrite) {
  .assert_scalar_value(name, value)
  scalars <- S7::prop(daf, "internal")$scalars
  if (exists(name, envir = scalars, inherits = FALSE) && !overwrite) {
    stop(sprintf("scalar %s already exists; use overwrite = TRUE", sQuote(name)), call. = FALSE)
  }
  assign(name, value, envir = scalars)
  invisible()
}

S7::method(format_delete_scalar, MemoryDaf) <- function(daf, name, must_exist) {
  scalars <- S7::prop(daf, "internal")$scalars
  if (!exists(name, envir = scalars, inherits = FALSE)) {
    if (must_exist) {
      stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
    }
    return(invisible())
  }
  rm(list = name, envir = scalars)
  invisible()
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-scalars.R
git commit -m "feat(memory_daf): scalar set/delete with overwrite + must_exist semantics"
```

### Task C3: User-facing scalar API

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/readers.R`
- Modify: `/home/aviezerl/src/dafr-native/R/writers.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-scalars.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("scalar user-facing round-trip with default handling", {
  d <- memory_daf()
  expect_false(has_scalar(d, "foo"))
  expect_equal(length(scalars_set(d)), 0L)
  expect_error(get_scalar(d, "foo"), "does not exist")
  expect_equal(get_scalar(d, "foo", default = 17), 17)

  set_scalar(d, "foo", "bar")
  expect_true(has_scalar(d, "foo"))
  expect_equal(get_scalar(d, "foo"), "bar")
  expect_equal(scalars_set(d), "foo")

  expect_error(set_scalar(d, "foo", "baz"),               "already exists")
  set_scalar(d, "foo", "baz", overwrite = TRUE)
  expect_equal(get_scalar(d, "foo"), "baz")

  delete_scalar(d, "foo")
  expect_false(has_scalar(d, "foo"))
  expect_error (delete_scalar(d, "foo"),                  "does not exist")
  expect_silent(delete_scalar(d, "foo", must_exist = FALSE))
})

test_that("set_scalar rejects NA (per Julia DAF rules)", {
  d <- memory_daf()
  expect_error(set_scalar(d, "foo", NA))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/readers.R`:

```r
#' Test whether a scalar exists.
#' @param daf A `DafReader`.
#' @param name Scalar name.
#' @return Logical scalar.
#' @export
has_scalar <- function(daf, name) {
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  format_has_scalar(daf, name)
}

#' Names of all scalars, sorted.
#' @inheritParams has_scalar
#' @return Character vector.
#' @export
scalars_set <- function(daf) format_scalars_set(daf)

#' Get a scalar, optionally with a default when missing.
#' @inheritParams has_scalar
#' @param default Value to return when the scalar is absent. If missing
#'   and the scalar is absent, an error is raised.
#' @return The scalar value.
#' @export
get_scalar <- function(daf, name, default) {
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  if (format_has_scalar(daf, name)) {
    return(format_get_scalar(daf, name))
  }
  if (!missing(default)) return(default)
  stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
}
```

Append to `R/writers.R`:

```r
#' Set a scalar.
#' @inheritParams has_scalar
#' @param value Atomic scalar (length 1, non-NA).
#' @param overwrite If `FALSE` (default) error when the scalar already
#'   exists; if `TRUE` replace.
#' @return Invisibly the input `daf`.
#' @export
set_scalar <- function(daf, name, value, overwrite = FALSE) {
  stopifnot(is.character(name),     length(name) == 1L, !is.na(name))
  stopifnot(is.logical(overwrite),  length(overwrite) == 1L, !is.na(overwrite))
  format_set_scalar(daf, name, value, overwrite)
  invisible(daf)
}

#' Delete a scalar.
#' @inheritParams has_scalar
#' @param must_exist See `delete_axis`.
#' @return Invisibly the input `daf`.
#' @export
delete_scalar <- function(daf, name, must_exist = TRUE) {
  stopifnot(is.character(name),      length(name) == 1L, !is.na(name))
  stopifnot(is.logical(must_exist),  length(must_exist) == 1L, !is.na(must_exist))
  format_delete_scalar(daf, name, must_exist)
  invisible(daf)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/readers.R R/writers.R NAMESPACE man/*.Rd tests/testthat/test-memory-scalars.R
git commit -m "feat(api): user-facing scalar API (has/get/set/delete/scalars_set)"
```

---

## Phase D — Vectors on MemoryDaf (format_*)

### Task D1: format_has_vector / format_get_vector / format_vectors_set

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-vectors.R` (create)

- [ ] **Step 1: Write failing test**

```r
test_that("format_has_vector / format_vectors_set reflect stored vectors", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_false(format_has_vector(d, "cell", "score"))
  expect_equal(format_vectors_set(d, "cell"), character(0L))
  vectors <- S7::prop(d, "internal")$vectors
  vectors$cell <- new.env(parent = emptyenv())
  vectors$cell$score <- c(1.0, 2.0)
  expect_true(format_has_vector(d, "cell", "score"))
  expect_equal(format_vectors_set(d, "cell"), "score")
})

test_that("format_vectors_set errors on unknown axis", {
  d <- memory_daf()
  expect_error(format_vectors_set(d, "cell"), "does not exist")
})

test_that("format_get_vector returns the stored SEXP unchanged", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  vectors <- S7::prop(d, "internal")$vectors
  vectors$cell <- new.env(parent = emptyenv())
  vectors$cell$score <- c(1.5, 2.5)
  expect_equal(format_get_vector(d, "cell", "score"), c(1.5, 2.5))
})

test_that("format_get_vector errors on unknown axis / vector", {
  d <- memory_daf()
  expect_error(format_get_vector(d, "cell", "score"), "axis .* does not exist")
  add_axis(d, "cell", c("A", "B"))
  expect_error(format_get_vector(d, "cell", "score"), "vector .* does not exist")
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/memory_daf.R`:

```r
# ---- Vectors: query ---------------------------------------------------------

.memory_axis_vectors <- function(daf, axis, must_exist = TRUE) {
  if (!format_has_axis(daf, axis)) {
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  vectors <- S7::prop(daf, "internal")$vectors
  if (!exists(axis, envir = vectors, inherits = FALSE)) {
    if (!must_exist) return(NULL)
    assign(axis, new.env(parent = emptyenv()), envir = vectors)
  }
  get(axis, envir = vectors, inherits = FALSE)
}

S7::method(format_has_vector, MemoryDaf) <- function(daf, axis, name) {
  if (!format_has_axis(daf, axis)) return(FALSE)
  env <- .memory_axis_vectors(daf, axis, must_exist = FALSE)
  if (is.null(env)) return(FALSE)
  exists(name, envir = env, inherits = FALSE)
}

S7::method(format_vectors_set, MemoryDaf) <- function(daf, axis) {
  env <- .memory_axis_vectors(daf, axis, must_exist = FALSE)
  if (is.null(env)) return(character(0L))
  sort(ls(env, all.names = TRUE))
}

S7::method(format_get_vector, MemoryDaf) <- function(daf, axis, name) {
  env <- .memory_axis_vectors(daf, axis, must_exist = TRUE)
  if (!exists(name, envir = env, inherits = FALSE)) {
    stop(sprintf("vector %s does not exist on axis %s",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  get(name, envir = env, inherits = FALSE)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-vectors.R
git commit -m "feat(memory_daf): vector query methods (has/get/vectors_set)"
```

### Task D2: format_set_vector (dense types) + format_delete_vector

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-vectors.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("format_set_vector stores dense numeric/integer/logical/character vectors", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  for (v in list(c(1.0, 2.0, 3.0), c(1L, 2L, 3L), c(TRUE, FALSE, TRUE), c("x", "y", "z"))) {
    format_set_vector(d, "cell", "v", v, overwrite = TRUE)
    expect_identical(format_get_vector(d, "cell", "v"), v)
  }
})

test_that("format_set_vector strips names to the axis entry order (named input)", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  format_set_vector(d, "cell", "v",
                    c(B = 20.0, A = 10.0, C = 30.0),
                    overwrite = FALSE)
  # Reordered to axis order, names dropped at storage layer.
  got <- format_get_vector(d, "cell", "v")
  expect_equal(got, c(10.0, 20.0, 30.0), ignore_attr = TRUE)
  expect_null(names(got))
})

test_that("format_set_vector errors on length mismatch / unknown axis / NULL", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(format_set_vector(d, "gene", "v", c(1, 2), overwrite = FALSE),
               "axis .* does not exist")
  expect_error(format_set_vector(d, "cell", "v", c(1, 2, 3), overwrite = FALSE),
               "length 3.*expected 2")
  expect_error(format_set_vector(d, "cell", "v", NULL, overwrite = FALSE),
               "atomic")
})

test_that("format_set_vector errors on named vector with unknown entries", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(
    format_set_vector(d, "cell", "v",
                      c(A = 1.0, Z = 2.0),
                      overwrite = FALSE),
    "not in axis"
  )
})

test_that("format_set_vector honours overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  expect_error(format_set_vector(d, "cell", "v", c(3.0, 4.0), overwrite = FALSE),
               "already exists")
  format_set_vector(d, "cell", "v", c(3.0, 4.0), overwrite = TRUE)
  expect_equal(format_get_vector(d, "cell", "v"), c(3.0, 4.0))
})

test_that("format_set_vector bumps the vector version counter", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  vc <- S7::prop(d, "vector_version_counter")
  expect_null(vc[["cell:v"]])
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  expect_equal(vc[["cell:v"]], 1L)
  format_set_vector(d, "cell", "v", c(3.0, 4.0), overwrite = TRUE)
  expect_equal(vc[["cell:v"]], 2L)
})

test_that("format_delete_vector removes + respects must_exist", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  format_delete_vector(d, "cell", "v", must_exist = TRUE)
  expect_false(format_has_vector(d, "cell", "v"))
  expect_error (format_delete_vector(d, "cell", "v", must_exist = TRUE),  "does not exist")
  expect_silent(format_delete_vector(d, "cell", "v", must_exist = FALSE))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append:

```r
# ---- Vectors: mutation ------------------------------------------------------

.validate_vector_value <- function(daf, axis, name, vec) {
  if (is.null(vec) || !is.atomic(vec)) {
    stop(sprintf("vector %s on axis %s must be atomic", sQuote(name), sQuote(axis)),
         call. = FALSE)
  }
  n <- format_axis_length(daf, axis)
  if (!is.null(names(vec))) {
    entries <- format_axis_array(daf, axis)
    missing <- setdiff(names(vec), entries)
    if (length(missing)) {
      stop(sprintf("vector %s has names not in axis %s: %s",
                   sQuote(name), sQuote(axis),
                   paste(sQuote(missing), collapse = ", ")),
           call. = FALSE)
    }
    if (length(vec) != n) {
      stop(sprintf("vector %s has length %d (expected %d) on axis %s",
                   sQuote(name), length(vec), n, sQuote(axis)),
           call. = FALSE)
    }
    # Reorder to axis order; drop names.
    vec <- unname(vec[entries])
  } else {
    if (length(vec) != n) {
      stop(sprintf("vector %s has length %d (expected %d) on axis %s",
                   sQuote(name), length(vec), n, sQuote(axis)),
           call. = FALSE)
    }
  }
  vec
}

S7::method(format_set_vector, MemoryDaf) <- function(daf, axis, name, vec, overwrite) {
  vec <- .validate_vector_value(daf, axis, name, vec)
  env <- .memory_axis_vectors(daf, axis, must_exist = TRUE)
  if (exists(name, envir = env, inherits = FALSE) && !overwrite) {
    stop(sprintf("vector %s already exists on axis %s; use overwrite = TRUE",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  assign(name, vec, envir = env)
  bump_vector_counter(daf, axis, name)
  invisible()
}

S7::method(format_delete_vector, MemoryDaf) <- function(daf, axis, name, must_exist) {
  env <- .memory_axis_vectors(daf, axis, must_exist = FALSE)
  if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
    if (must_exist) {
      stop(sprintf("vector %s does not exist on axis %s",
                   sQuote(name), sQuote(axis)), call. = FALSE)
    }
    return(invisible())
  }
  rm(list = name, envir = env)
  bump_vector_counter(daf, axis, name)
  invisible()
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-vectors.R
git commit -m "feat(memory_daf): vector set/delete with named reorder + overwrite + must_exist"
```

---

## Phase E — User-facing vector API (with cache + layout)

### Task E1: get_vector (names + default + cache)

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/readers.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-vectors.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("get_vector returns axis-named vector", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  set_vector(d, "cell", "v", c(10.0, 20.0))
  got <- get_vector(d, "cell", "v")
  expect_equal(names(got), c("A", "B"))
  expect_equal(unname(got), c(10.0, 20.0))
})

test_that("get_vector default recycles a scalar across the axis", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(get_vector(d, "cell", "missing"), "does not exist")
  na_vec <- get_vector(d, "cell", "missing", default = NA)
  expect_equal(names(na_vec), c("A", "B"))
  expect_true(all(is.na(na_vec)))
  str_vec <- get_vector(d, "cell", "missing", default = "savta")
  expect_equal(str_vec, c(A = "savta", B = "savta"))
})

test_that("get_vector hits the memory-tier cache on repeated reads", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  set_vector(d, "cell", "v", c(1.0, 2.0))
  first  <- get_vector(d, "cell", "v")
  second <- get_vector(d, "cell", "v")
  expect_identical(first, second)
  cache_env <- S7::prop(d, "cache")
  expect_true(exists(cache_key_vector("cell", "v"),
                     envir = cache_env$memory, inherits = FALSE))
})

test_that("get_vector cache invalidates after overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  set_vector(d, "cell", "v", c(1.0, 2.0))
  expect_equal(unname(get_vector(d, "cell", "v")), c(1.0, 2.0))
  set_vector(d, "cell", "v", c(10.0, 20.0), overwrite = TRUE)
  expect_equal(unname(get_vector(d, "cell", "v")), c(10.0, 20.0))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement** (depends on cache infra from Phase I; wire a minimal cache here and extend in I1/I2)

Append to `R/readers.R`:

```r
#' Test whether a vector exists on an axis.
#' @param daf A `DafReader`.
#' @param axis Axis name.
#' @param name Vector name.
#' @return Logical scalar.
#' @export
has_vector <- function(daf, axis, name) {
  stopifnot(is.character(axis), length(axis) == 1L, !is.na(axis))
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  format_has_vector(daf, axis, name)
}

#' Names of vectors on an axis, sorted.
#' @inheritParams has_vector
#' @return Character vector.
#' @export
vectors_set <- function(daf, axis) {
  stopifnot(is.character(axis), length(axis) == 1L, !is.na(axis))
  format_vectors_set(daf, axis)
}

#' Get a vector, returning it as an axis-named R vector.
#'
#' @inheritParams has_vector
#' @param default If supplied and the vector is absent, return a
#'   constant-valued named vector of length `axis_length(daf, axis)`
#'   with the axis entries as names.
#' @return Named atomic vector.
#' @export
get_vector <- function(daf, axis, name, default) {
  stopifnot(is.character(axis), length(axis) == 1L, !is.na(axis))
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  if (!format_has_axis(daf, axis)) {
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  entries <- format_axis_array(daf, axis)
  if (!format_has_vector(daf, axis, name)) {
    if (missing(default)) {
      stop(sprintf("vector %s does not exist on axis %s",
                   sQuote(name), sQuote(axis)), call. = FALSE)
    }
    out <- rep(default, length(entries))
    names(out) <- entries
    return(out)
  }
  cache_key <- cache_key_vector(axis, name)
  cache_env <- S7::prop(daf, "cache")
  stamp_now <- vector_stamp(daf, axis, name)
  hit <- cache_lookup(cache_env, "memory", cache_key, stamp_now)
  if (!is.null(hit)) return(hit)
  raw <- format_get_vector(daf, axis, name)
  out <- raw
  if (is.null(names(out))) names(out) <- entries
  cache_store(cache_env, "memory", cache_key, out, stamp_now,
              size_bytes = object.size(out))
  out
}
```

(`vector_stamp`, `cache_lookup`, `cache_store` are defined in Phase I. For now, stub them with placeholder implementations that will be expanded later — but to keep this task TDD-green, also add the stubs below.)

Append to `R/cache.R` (stubs; full logic in Phase I):

```r
# ---- Version stamps (computed from counters) --------------------------------

axis_stamp <- function(daf, axis) {
  S7::prop(daf, "axis_version_counter")[[axis]] %||% 0L
}

vector_stamp <- function(daf, axis, name) {
  vc <- S7::prop(daf, "vector_version_counter")
  c(axis_stamp(daf, axis),
    vc[[paste0(axis, ":", name)]] %||% 0L)
}

matrix_stamp <- function(daf, rows_axis, cols_axis, name) {
  mc <- S7::prop(daf, "matrix_version_counter")
  c(axis_stamp(daf, rows_axis),
    axis_stamp(daf, cols_axis),
    mc[[paste0(rows_axis, ":", cols_axis, ":", name)]] %||% 0L)
}

# ---- Cache entries with version stamps --------------------------------------

cache_lookup <- function(cache_env, tier, key, expected_stamp) {
  bucket <- cache_env[[tier]]
  if (!exists(key, envir = bucket, inherits = FALSE)) return(NULL)
  entry <- get(key, envir = bucket, inherits = FALSE)
  if (!identical(entry$stamp, expected_stamp)) {
    rm(list = key, envir = bucket)
    return(NULL)
  }
  entry$value
}

cache_store <- function(cache_env, tier, key, value, stamp, size_bytes = 0L) {
  bucket <- cache_env[[tier]]
  assign(key, list(value = value, stamp = stamp, size = as.numeric(size_bytes)),
         envir = bucket)
  invisible()
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/readers.R R/cache.R NAMESPACE man/*.Rd tests/testthat/test-memory-vectors.R
git commit -m "feat(api): get_vector with axis names + default + version-stamped cache"
```

### Task E2: set_vector / delete_vector (user-facing)

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/writers.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-vectors.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("set_vector with named input reorders by axis entries", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  set_vector(d, "cell", "v", c(B = 2.0, A = 1.0))
  expect_equal(get_vector(d, "cell", "v"), c(A = 1.0, B = 2.0))
})

test_that("set_vector rejects length mismatch", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  expect_error(set_vector(d, "cell", "v", c(1.0, 2.0)), "length 2")
})

test_that("set_vector respects overwrite = FALSE", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  set_vector(d, "cell", "v", 1.0)
  expect_error(set_vector(d, "cell", "v", 2.0), "already exists")
  set_vector(d, "cell", "v", 2.0, overwrite = TRUE)
  expect_equal(unname(get_vector(d, "cell", "v")), 2.0)
})

test_that("delete_vector invalidates cached read", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  set_vector(d, "cell", "v", 1.0)
  get_vector(d, "cell", "v")  # populate cache
  delete_vector(d, "cell", "v")
  expect_false(has_vector(d, "cell", "v"))
  expect_error (delete_vector(d, "cell", "v"),                 "does not exist")
  expect_silent(delete_vector(d, "cell", "v", must_exist = FALSE))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/writers.R`:

```r
#' Set a vector on an axis.
#'
#' @inheritParams has_vector
#' @param vec Atomic vector of length `axis_length(daf, axis)`, or a
#'   named vector whose names are a subset of the axis entries (reordered
#'   into axis order at storage time).
#' @param overwrite See `set_scalar`.
#' @return Invisibly the input `daf`.
#' @export
set_vector <- function(daf, axis, name, vec, overwrite = FALSE) {
  stopifnot(is.character(axis),    length(axis) == 1L, !is.na(axis))
  stopifnot(is.character(name),    length(name) == 1L, !is.na(name))
  stopifnot(is.logical(overwrite), length(overwrite) == 1L, !is.na(overwrite))
  format_set_vector(daf, axis, name, vec, overwrite)
  invisible(daf)
}

#' Delete a vector on an axis.
#'
#' @inheritParams has_vector
#' @param must_exist See `delete_axis`.
#' @return Invisibly the input `daf`.
#' @export
delete_vector <- function(daf, axis, name, must_exist = TRUE) {
  stopifnot(is.character(axis), length(axis) == 1L, !is.na(axis))
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  stopifnot(is.logical(must_exist), length(must_exist) == 1L, !is.na(must_exist))
  format_delete_vector(daf, axis, name, must_exist)
  invisible(daf)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/writers.R NAMESPACE man/*.Rd tests/testthat/test-memory-vectors.R
git commit -m "feat(api): set_vector + delete_vector with cache invalidation via version bump"
```

---

## Phase F — Matrices on MemoryDaf (format_*)

### Task F1: format_has_matrix / format_matrices_set / format_get_matrix (dense)

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-matrices.R` (create)

- [ ] **Step 1: Write failing tests**

```r
test_that("format_has_matrix / format_matrices_set empty case", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  expect_false(format_has_matrix(d, "cell", "gene", "UMIs"))
  expect_equal(format_matrices_set(d, "cell", "gene"), character(0L))
})

test_that("format_get_matrix returns stored dense matrix unchanged", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(seq_len(6), nrow = 2, ncol = 3)
  matrices <- S7::prop(d, "internal")$matrices
  matrices$cell <- new.env(parent = emptyenv())
  matrices$cell$gene <- new.env(parent = emptyenv())
  matrices$cell$gene$UMIs <- m
  expect_true(format_has_matrix(d, "cell", "gene", "UMIs"))
  expect_equal(format_matrices_set(d, "cell", "gene"), "UMIs")
  expect_identical(format_get_matrix(d, "cell", "gene", "UMIs"), m)
})

test_that("format_get_matrix errors on unknown axes / missing matrix", {
  d <- memory_daf()
  expect_error(format_get_matrix(d, "cell", "gene", "UMIs"), "axis .* does not exist")
  add_axis(d, "cell", "A")
  expect_error(format_get_matrix(d, "cell", "gene", "UMIs"), "axis .* does not exist")
  add_axis(d, "gene", "X")
  expect_error(format_get_matrix(d, "cell", "gene", "UMIs"), "matrix .* does not exist")
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/memory_daf.R`:

```r
# ---- Matrices: query --------------------------------------------------------

.memory_matrix_bucket <- function(daf, rows_axis, cols_axis, create = FALSE) {
  if (!format_has_axis(daf, rows_axis)) {
    stop(sprintf("axis %s does not exist", sQuote(rows_axis)), call. = FALSE)
  }
  if (!format_has_axis(daf, cols_axis)) {
    stop(sprintf("axis %s does not exist", sQuote(cols_axis)), call. = FALSE)
  }
  matrices <- S7::prop(daf, "internal")$matrices
  if (!exists(rows_axis, envir = matrices, inherits = FALSE)) {
    if (!create) return(NULL)
    assign(rows_axis, new.env(parent = emptyenv()), envir = matrices)
  }
  rows_env <- get(rows_axis, envir = matrices, inherits = FALSE)
  if (!exists(cols_axis, envir = rows_env, inherits = FALSE)) {
    if (!create) return(NULL)
    assign(cols_axis, new.env(parent = emptyenv()), envir = rows_env)
  }
  get(cols_axis, envir = rows_env, inherits = FALSE)
}

S7::method(format_has_matrix, MemoryDaf) <- function(daf, rows_axis, columns_axis, name) {
  if (!format_has_axis(daf, rows_axis) || !format_has_axis(daf, columns_axis)) return(FALSE)
  env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
  if (is.null(env)) return(FALSE)
  exists(name, envir = env, inherits = FALSE)
}

S7::method(format_matrices_set, MemoryDaf) <- function(daf, rows_axis, columns_axis) {
  env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
  if (is.null(env)) return(character(0L))
  sort(ls(env, all.names = TRUE))
}

S7::method(format_get_matrix, MemoryDaf) <- function(daf, rows_axis, columns_axis, name) {
  env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
  if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
    stop(sprintf("matrix %s does not exist on axes (%s, %s)",
                 sQuote(name), sQuote(rows_axis), sQuote(columns_axis)),
         call. = FALSE)
  }
  get(name, envir = env, inherits = FALSE)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-matrices.R
git commit -m "feat(memory_daf): matrix query methods (has/get/matrices_set)"
```

### Task F2: format_set_matrix / format_delete_matrix (dense + sparse)

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-matrices.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("format_set_matrix accepts dense double / int / logical with correct shape", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m_d <- matrix(seq_len(6) + 0.5, nrow = 2, ncol = 3)
  m_i <- matrix(seq_len(6),       nrow = 2, ncol = 3)
  m_l <- matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), nrow = 2, ncol = 3)
  format_set_matrix(d, "cell", "gene", "d", m_d, overwrite = FALSE)
  format_set_matrix(d, "cell", "gene", "i", m_i, overwrite = FALSE)
  format_set_matrix(d, "cell", "gene", "l", m_l, overwrite = FALSE)
  expect_identical(format_get_matrix(d, "cell", "gene", "d"), m_d)
  expect_identical(format_get_matrix(d, "cell", "gene", "i"), m_i)
  expect_identical(format_get_matrix(d, "cell", "gene", "l"), m_l)
})

test_that("format_set_matrix accepts dgCMatrix + lgCMatrix sparse", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m_d <- Matrix::Matrix(c(0, 1, 2, 0, 0, 3), nrow = 2, ncol = 3, sparse = TRUE)
  m_l <- as(m_d != 0, "lgCMatrix")
  expect_s4_class(m_d, "dgCMatrix")
  expect_s4_class(m_l, "lgCMatrix")
  format_set_matrix(d, "cell", "gene", "d", m_d, overwrite = FALSE)
  format_set_matrix(d, "cell", "gene", "l", m_l, overwrite = FALSE)
  expect_equal(as.matrix(format_get_matrix(d, "cell", "gene", "d")), as.matrix(m_d))
  expect_equal(as.matrix(format_get_matrix(d, "cell", "gene", "l")), as.matrix(m_l))
})

test_that("format_set_matrix rejects shape mismatch / non-matrix / overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  expect_error(format_set_matrix(d, "cell", "gene", "m",
                                 matrix(0, 3, 3), overwrite = FALSE),
               "dim .*expected 2 x 3")
  expect_error(format_set_matrix(d, "cell", "gene", "m",
                                 c(1, 2, 3, 4, 5, 6), overwrite = FALSE),
               "not a matrix")
  format_set_matrix(d, "cell", "gene", "m", matrix(0, 2, 3), overwrite = FALSE)
  expect_error(format_set_matrix(d, "cell", "gene", "m",
                                 matrix(1, 2, 3), overwrite = FALSE),
               "already exists")
  format_set_matrix(d, "cell", "gene", "m", matrix(1, 2, 3), overwrite = TRUE)
  expect_equal(format_get_matrix(d, "cell", "gene", "m"), matrix(1, 2, 3))
})

test_that("format_set_matrix strips dimnames at storage layer", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(seq_len(6), 2, 3, dimnames = list(c("A", "B"), c("X", "Y", "Z")))
  format_set_matrix(d, "cell", "gene", "m", m, overwrite = FALSE)
  got <- format_get_matrix(d, "cell", "gene", "m")
  expect_null(dimnames(got))
})

test_that("format_set_matrix bumps matrix version counter", {
  d <- memory_daf()
  add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
  mc <- S7::prop(d, "matrix_version_counter")
  format_set_matrix(d, "cell", "gene", "m", matrix(1, 1, 1), overwrite = FALSE)
  expect_equal(mc[["cell:gene:m"]], 1L)
  format_set_matrix(d, "cell", "gene", "m", matrix(2, 1, 1), overwrite = TRUE)
  expect_equal(mc[["cell:gene:m"]], 2L)
})

test_that("format_delete_matrix removes + respects must_exist", {
  d <- memory_daf()
  add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
  format_set_matrix(d, "cell", "gene", "m", matrix(1, 1, 1), overwrite = FALSE)
  format_delete_matrix(d, "cell", "gene", "m", must_exist = TRUE)
  expect_false(format_has_matrix(d, "cell", "gene", "m"))
  expect_error (format_delete_matrix(d, "cell", "gene", "m", must_exist = TRUE),  "does not exist")
  expect_silent(format_delete_matrix(d, "cell", "gene", "m", must_exist = FALSE))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append:

```r
# ---- Matrices: mutation -----------------------------------------------------

.validate_matrix_value <- function(daf, rows_axis, cols_axis, name, mat) {
  is_sparse <- methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")
  is_dense  <- is.matrix(mat)
  if (!is_sparse && !is_dense) {
    stop(sprintf("matrix %s is not a matrix or dgCMatrix/lgCMatrix",
                 sQuote(name)), call. = FALSE)
  }
  nr <- format_axis_length(daf, rows_axis)
  nc <- format_axis_length(daf, cols_axis)
  d  <- dim(mat)
  if (d[[1L]] != nr || d[[2L]] != nc) {
    stop(sprintf("matrix %s has dim %d x %d (expected %d x %d)",
                 sQuote(name), d[[1L]], d[[2L]], nr, nc), call. = FALSE)
  }
  if (is_dense) {
    dimnames(mat) <- NULL
  } else {
    mat@Dimnames <- list(NULL, NULL)
  }
  mat
}

S7::method(format_set_matrix, MemoryDaf) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
  mat <- .validate_matrix_value(daf, rows_axis, columns_axis, name, mat)
  env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = TRUE)
  if (exists(name, envir = env, inherits = FALSE) && !overwrite) {
    stop(sprintf("matrix %s already exists on axes (%s, %s); use overwrite = TRUE",
                 sQuote(name), sQuote(rows_axis), sQuote(columns_axis)),
         call. = FALSE)
  }
  assign(name, mat, envir = env)
  bump_matrix_counter(daf, rows_axis, columns_axis, name)
  invisible()
}

S7::method(format_delete_matrix, MemoryDaf) <- function(daf, rows_axis, columns_axis, name, must_exist) {
  env <- .memory_matrix_bucket(daf, rows_axis, columns_axis, create = FALSE)
  if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
    if (must_exist) {
      stop(sprintf("matrix %s does not exist on axes (%s, %s)",
                   sQuote(name), sQuote(rows_axis), sQuote(columns_axis)),
           call. = FALSE)
    }
    return(invisible())
  }
  rm(list = name, envir = env)
  bump_matrix_counter(daf, rows_axis, columns_axis, name)
  invisible()
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-matrices.R
git commit -m "feat(memory_daf): matrix set/delete for dense + dgCMatrix/lgCMatrix sparse"
```

### Task F3: format_relayout_matrix

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-matrices.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("format_relayout_matrix writes the transposed layout", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(seq_len(6), 2, 3)
  format_set_matrix(d, "cell", "gene", "UMIs", m, overwrite = FALSE)
  expect_false(format_has_matrix(d, "gene", "cell", "UMIs"))
  format_relayout_matrix(d, "cell", "gene", "UMIs")
  expect_true(format_has_matrix(d, "gene", "cell", "UMIs"))
  expect_equal(format_get_matrix(d, "gene", "cell", "UMIs"), t(m))
})

test_that("format_relayout_matrix works for sparse (CSC -> transposed CSC)", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- Matrix::Matrix(c(0, 1, 2, 0, 0, 3), 2, 3, sparse = TRUE)
  format_set_matrix(d, "cell", "gene", "UMIs", m, overwrite = FALSE)
  format_relayout_matrix(d, "cell", "gene", "UMIs")
  got <- format_get_matrix(d, "gene", "cell", "UMIs")
  expect_s4_class(got, "dgCMatrix")
  expect_equal(as.matrix(got), as.matrix(Matrix::t(m)))
})

test_that("format_relayout_matrix errors when source matrix missing", {
  d <- memory_daf()
  add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
  expect_error(format_relayout_matrix(d, "cell", "gene", "UMIs"),
               "does not exist")
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append:

```r
S7::method(format_relayout_matrix, MemoryDaf) <- function(daf, rows_axis, columns_axis, name) {
  src <- format_get_matrix(daf, rows_axis, columns_axis, name)
  transposed <- if (methods::is(src, "dgCMatrix") || methods::is(src, "lgCMatrix")) {
    Matrix::t(src)
  } else {
    t(src)
  }
  format_set_matrix(daf, columns_axis, rows_axis, name, transposed, overwrite = TRUE)
  invisible()
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-matrices.R
git commit -m "feat(memory_daf): format_relayout_matrix writes transposed layout"
```

---

## Phase G — User-facing matrix API

### Task G1: get_matrix (dimnames + cache + layout fallback)

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/readers.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-matrices.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("get_matrix returns matrix with axis-name dimnames", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 2, 3))
  m <- get_matrix(d, "cell", "gene", "UMIs")
  expect_equal(rownames(m), c("A", "B"))
  expect_equal(colnames(m), c("X", "Y", "Z"))
})

test_that("get_matrix falls back to transposed layout when only the other is stored", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 2, 3))
  expect_false(has_matrix(d, "gene", "cell", "UMIs"))
  m <- get_matrix(d, "gene", "cell", "UMIs")
  expect_equal(dim(m), c(3L, 2L))
  expect_equal(rownames(m), c("X", "Y", "Z"))
  expect_equal(colnames(m), c("A", "B"))
})

test_that("get_matrix default returns a constant-valued dimnamed matrix", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y"))
  expect_error(get_matrix(d, "cell", "gene", "missing"), "does not exist")
  m <- get_matrix(d, "cell", "gene", "missing", default = NA)
  expect_equal(dim(m), c(2L, 2L))
  expect_equal(rownames(m), c("A", "B"))
  expect_equal(colnames(m), c("X", "Y"))
  expect_true(all(is.na(m)))
})

test_that("get_matrix hits the cache on repeated reads and invalidates on overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A")); add_axis(d, "gene", c("X"))
  set_matrix(d, "cell", "gene", "UMIs", matrix(1, 1, 1))
  first  <- get_matrix(d, "cell", "gene", "UMIs")
  second <- get_matrix(d, "cell", "gene", "UMIs")
  expect_identical(first, second)
  set_matrix(d, "cell", "gene", "UMIs", matrix(2, 1, 1), overwrite = TRUE)
  third <- get_matrix(d, "cell", "gene", "UMIs")
  expect_equal(as.numeric(third), 2)
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/readers.R`:

```r
#' Test whether a matrix exists for an axis pair.
#' @param daf A `DafReader`.
#' @param rows_axis Row-axis name.
#' @param cols_axis Column-axis name.
#' @param name Matrix name.
#' @return Logical scalar.
#' @export
has_matrix <- function(daf, rows_axis, cols_axis, name) {
  stopifnot(is.character(rows_axis), length(rows_axis) == 1L, !is.na(rows_axis))
  stopifnot(is.character(cols_axis), length(cols_axis) == 1L, !is.na(cols_axis))
  stopifnot(is.character(name),      length(name) == 1L,      !is.na(name))
  format_has_matrix(daf, rows_axis, cols_axis, name)
}

#' Names of matrices for an axis pair, sorted.
#' @inheritParams has_matrix
#' @return Character vector.
#' @export
matrices_set <- function(daf, rows_axis, cols_axis) {
  stopifnot(is.character(rows_axis), length(rows_axis) == 1L, !is.na(rows_axis))
  stopifnot(is.character(cols_axis), length(cols_axis) == 1L, !is.na(cols_axis))
  format_matrices_set(daf, rows_axis, cols_axis)
}

#' Get a matrix, returning it with axis-entry dimnames.
#'
#' @inheritParams has_matrix
#' @param default If supplied and the matrix is absent, return a
#'   constant-valued `nrow x ncol` matrix with axis entries as dimnames.
#' @return Matrix or dgCMatrix / lgCMatrix with dimnames set.
#' @export
get_matrix <- function(daf, rows_axis, cols_axis, name, default) {
  stopifnot(is.character(rows_axis), length(rows_axis) == 1L, !is.na(rows_axis))
  stopifnot(is.character(cols_axis), length(cols_axis) == 1L, !is.na(cols_axis))
  stopifnot(is.character(name),      length(name) == 1L,      !is.na(name))

  rows <- format_axis_array(daf, rows_axis)
  cols <- format_axis_array(daf, cols_axis)

  primary <- format_has_matrix(daf, rows_axis, cols_axis, name)
  flipped <- !primary && format_has_matrix(daf, cols_axis, rows_axis, name)

  if (!primary && !flipped) {
    if (missing(default)) {
      stop(sprintf("matrix %s does not exist on axes (%s, %s)",
                   sQuote(name), sQuote(rows_axis), sQuote(cols_axis)),
           call. = FALSE)
    }
    out <- matrix(default, nrow = length(rows), ncol = length(cols),
                  dimnames = list(rows, cols))
    return(out)
  }

  if (primary) {
    ra <- rows_axis; ca <- cols_axis
  } else {
    ra <- cols_axis; ca <- rows_axis
  }

  cache_key <- cache_key_matrix(ra, ca, name)
  cache_env <- S7::prop(daf, "cache")
  stamp_now <- matrix_stamp(daf, ra, ca, name)
  stored <- cache_lookup(cache_env, "memory", cache_key, stamp_now)
  if (is.null(stored)) {
    stored <- format_get_matrix(daf, ra, ca, name)
    cache_store(cache_env, "memory", cache_key, stored, stamp_now,
                size_bytes = object.size(stored))
  }

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

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/readers.R NAMESPACE man/*.Rd tests/testthat/test-memory-matrices.R
git commit -m "feat(api): get_matrix with dimnames + layout fallback + default"
```

### Task G2: set_matrix / delete_matrix / relayout_matrix (user-facing)

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/writers.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-matrices.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("set_matrix round-trips dense + sparse + respects overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(seq_len(6), 2, 3)
  set_matrix(d, "cell", "gene", "UMIs", m)
  expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
               m, ignore_attr = TRUE)
  expect_error(set_matrix(d, "cell", "gene", "UMIs", m), "already exists")
  set_matrix(d, "cell", "gene", "UMIs", m * 10, overwrite = TRUE)
  expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
               m * 10, ignore_attr = TRUE)
})

test_that("delete_matrix removes + respects must_exist", {
  d <- memory_daf()
  add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
  set_matrix(d, "cell", "gene", "m", matrix(1, 1, 1))
  delete_matrix(d, "cell", "gene", "m")
  expect_false(has_matrix(d, "cell", "gene", "m"))
  expect_error (delete_matrix(d, "cell", "gene", "m"),                     "does not exist")
  expect_silent(delete_matrix(d, "cell", "gene", "m", must_exist = FALSE))
})

test_that("relayout_matrix makes the flipped layout physical", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 2, 3))
  expect_false(has_matrix(d, "gene", "cell", "UMIs"))
  relayout_matrix(d, "cell", "gene", "UMIs")
  expect_true(has_matrix(d, "gene", "cell", "UMIs"))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/writers.R`:

```r
#' Set a matrix indexed by a pair of axes.
#'
#' @inheritParams has_matrix
#' @param mat Dense `matrix`, or sparse `dgCMatrix` / `lgCMatrix`, of
#'   shape `axis_length(rows_axis) x axis_length(cols_axis)`.
#' @param overwrite See `set_scalar`.
#' @return Invisibly the input `daf`.
#' @export
set_matrix <- function(daf, rows_axis, cols_axis, name, mat, overwrite = FALSE) {
  stopifnot(is.character(rows_axis), length(rows_axis) == 1L, !is.na(rows_axis))
  stopifnot(is.character(cols_axis), length(cols_axis) == 1L, !is.na(cols_axis))
  stopifnot(is.character(name),      length(name) == 1L,      !is.na(name))
  stopifnot(is.logical(overwrite),   length(overwrite) == 1L, !is.na(overwrite))
  format_set_matrix(daf, rows_axis, cols_axis, name, mat, overwrite)
  invisible(daf)
}

#' Delete a matrix.
#' @inheritParams has_matrix
#' @param must_exist See `delete_axis`.
#' @return Invisibly the input `daf`.
#' @export
delete_matrix <- function(daf, rows_axis, cols_axis, name, must_exist = TRUE) {
  stopifnot(is.character(rows_axis), length(rows_axis) == 1L, !is.na(rows_axis))
  stopifnot(is.character(cols_axis), length(cols_axis) == 1L, !is.na(cols_axis))
  stopifnot(is.character(name),      length(name) == 1L,      !is.na(name))
  stopifnot(is.logical(must_exist),  length(must_exist) == 1L, !is.na(must_exist))
  format_delete_matrix(daf, rows_axis, cols_axis, name, must_exist)
  invisible(daf)
}

#' Physically store the transposed layout of a matrix.
#'
#' After this call, `get_matrix(cols_axis, rows_axis, name)` skips the
#' transpose-on-the-fly path.
#'
#' @inheritParams has_matrix
#' @return Invisibly the input `daf`.
#' @export
relayout_matrix <- function(daf, rows_axis, cols_axis, name) {
  stopifnot(is.character(rows_axis), length(rows_axis) == 1L, !is.na(rows_axis))
  stopifnot(is.character(cols_axis), length(cols_axis) == 1L, !is.na(cols_axis))
  stopifnot(is.character(name),      length(name) == 1L,      !is.na(name))
  format_relayout_matrix(daf, rows_axis, cols_axis, name)
  invisible(daf)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/writers.R NAMESPACE man/*.Rd tests/testthat/test-memory-matrices.R
git commit -m "feat(api): set_matrix + delete_matrix + relayout_matrix (user-facing)"
```

---

## Phase H — `description()`

### Task H1: Multi-backend `description()`

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/readers.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-scalars.R`

- [ ] **Step 1: Write failing tests (append to scalars tests)**

```r
test_that("description() empty-store is minimal", {
  d <- memory_daf(name = "test!")
  expect_equal(
    description(d),
    "name: test!\ntype: MemoryDaf\n"
  )
})

test_that("description() reports axes, scalars, and matrix shapes", {
  d <- memory_daf(name = "test!")
  set_scalar(d, "foo", "bar")
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  set_vector(d, "cell", "donor", c("d1", "d2"))
  set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 2, 3))
  expected <- paste(
    "name: test!",
    "type: MemoryDaf",
    "scalars:",
    '  foo: "bar"',
    "axes:",
    "  cell: 2 entries",
    "  gene: 3 entries",
    "vectors:",
    "  cell:",
    "    donor",
    "matrices:",
    "  cell,gene:",
    "    UMIs",
    "",
    sep = "\n"
  )
  expect_equal(description(d), expected)
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Append to `R/readers.R`:

```r
#' Human-readable summary of a Daf store.
#'
#' Returns a multi-line string describing axes, scalars, vectors, and
#' matrices. Matches the column-order rendering of Julia DAF's own
#' `description()`.
#'
#' @param daf A `DafReader`.
#' @return Character scalar.
#' @export
description <- function(daf) {
  lines <- c(sprintf("name: %s", S7::prop(daf, "name")),
             sprintf("type: %s", .daf_type_name(daf)))
  sc <- format_scalars_set(daf)
  if (length(sc)) {
    lines <- c(lines, "scalars:")
    for (nm in sc) {
      v <- format_get_scalar(daf, nm)
      lines <- c(lines, sprintf("  %s: %s", nm, .format_scalar_literal(v)))
    }
  }
  axes <- format_axes_set(daf)
  if (length(axes)) {
    lines <- c(lines, "axes:")
    for (ax in axes) {
      lines <- c(lines, sprintf("  %s: %d entries", ax, format_axis_length(daf, ax)))
    }
  }
  vec_axes <- Filter(function(ax) length(format_vectors_set(daf, ax)) > 0L, axes)
  if (length(vec_axes)) {
    lines <- c(lines, "vectors:")
    for (ax in vec_axes) {
      lines <- c(lines, sprintf("  %s:", ax))
      for (nm in format_vectors_set(daf, ax)) {
        lines <- c(lines, sprintf("    %s", nm))
      }
    }
  }
  mat_keys <- character(0L)
  for (ra in axes) for (ca in axes) {
    ms <- format_matrices_set(daf, ra, ca)
    if (length(ms)) mat_keys <- c(mat_keys, sprintf("%s,%s", ra, ca))
  }
  if (length(mat_keys)) {
    lines <- c(lines, "matrices:")
    for (k in mat_keys) {
      parts <- strsplit(k, ",", fixed = TRUE)[[1L]]
      lines <- c(lines, sprintf("  %s:", k))
      for (nm in format_matrices_set(daf, parts[[1L]], parts[[2L]])) {
        lines <- c(lines, sprintf("    %s", nm))
      }
    }
  }
  paste0(paste(lines, collapse = "\n"), "\n")
}

.daf_type_name <- function(daf) {
  cls <- class(daf)[[1L]]
  sub("^dafr::", "", cls)
}

.format_scalar_literal <- function(v) {
  if (is.character(v)) sprintf('"%s"', v)
  else                 format(v)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/readers.R NAMESPACE man/description.Rd tests/testthat/test-memory-scalars.R
git commit -m "feat(api): description() user-facing store summary (MemoryDaf)"
```

---

## Phase I — Cache infrastructure (LRU + memory cap + version stamps)

### Task I1: Version-stamp assertions on read (already in E1); harden lookup

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/cache.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-cache.R`

- [ ] **Step 1: Write failing tests (extend existing file)**

```r
test_that("cache_lookup returns NULL and evicts when stamps differ", {
  ce <- new_cache_env()
  cache_store(ce, "memory", "v:x", "v1", stamp = c(1L, 1L), size_bytes = 100)
  expect_equal(cache_lookup(ce, "memory", "v:x", c(1L, 1L)), "v1")
  expect_null( cache_lookup(ce, "memory", "v:x", c(1L, 2L)))
  expect_false(exists("v:x", envir = ce$memory, inherits = FALSE))
})

test_that("cache_lookup returns NULL for missing key", {
  ce <- new_cache_env()
  expect_null(cache_lookup(ce, "memory", "v:missing", c(0L, 0L)))
})

test_that("cache_store persists stamp + size alongside value", {
  ce <- new_cache_env()
  cache_store(ce, "memory", "k", 42L, c(3L, 4L), size_bytes = 8)
  entry <- get("k", envir = ce$memory, inherits = FALSE)
  expect_equal(entry$value, 42L)
  expect_equal(entry$stamp, c(3L, 4L))
  expect_equal(entry$size,  8)
})
```

- [ ] **Step 2: Run — expect (mostly) pass** (cache_store/lookup were added in E1). If any fail, inspect and adjust cache.R.

- [ ] **Step 3: No new code expected** — Phase E1 already added `cache_lookup` / `cache_store` / `vector_stamp` / `matrix_stamp` with these semantics. If tests fail, fix the skeletons in `R/cache.R` until green.

- [ ] **Step 4: Commit (only if cache.R changed)**

```bash
cd /home/aviezerl/src/dafr-native
git add R/cache.R tests/testthat/test-cache.R
git commit -m "test(cache): lock version-stamp eviction semantics"
```

### Task I2: Memory-cap + LRU eviction

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/cache.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-cache.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("cache_store evicts LRU entries when memory cap is exceeded", {
  ce <- new_cache_env()
  # Simulate 1 KB cap.
  cache_set_cap(ce, 1000)
  cache_store(ce, "memory", "a", "A", c(0L), size_bytes = 400)
  cache_store(ce, "memory", "b", "B", c(0L), size_bytes = 400)
  cache_store(ce, "memory", "c", "C", c(0L), size_bytes = 400)
  # "a" should have been evicted (it became LRU after b, c).
  expect_false(exists("a", envir = ce$memory, inherits = FALSE))
  expect_true( exists("b", envir = ce$memory, inherits = FALSE))
  expect_true( exists("c", envir = ce$memory, inherits = FALSE))
})

test_that("cache_lookup touches LRU on hit (moves entry to MRU)", {
  ce <- new_cache_env()
  cache_set_cap(ce, 1000)
  cache_store(ce, "memory", "a", "A", c(0L), size_bytes = 400)
  cache_store(ce, "memory", "b", "B", c(0L), size_bytes = 400)
  # Access "a" — now "b" is LRU.
  cache_lookup(ce, "memory", "a", c(0L))
  cache_store(ce, "memory", "c", "C", c(0L), size_bytes = 400)
  # "b" should have been evicted, not "a".
  expect_true( exists("a", envir = ce$memory, inherits = FALSE))
  expect_false(exists("b", envir = ce$memory, inherits = FALSE))
  expect_true( exists("c", envir = ce$memory, inherits = FALSE))
})

test_that("mapped tier is exempt from the memory cap", {
  ce <- new_cache_env()
  cache_set_cap(ce, 100)
  cache_store(ce, "mapped", "a", "A", c(0L), size_bytes = 1e9)
  cache_store(ce, "memory", "b", "B", c(0L), size_bytes = 50)
  expect_true(exists("a", envir = ce$mapped, inherits = FALSE))
  expect_true(exists("b", envir = ce$memory, inherits = FALSE))
})

test_that("entries larger than the cap are rejected (or stored and immediately evict others)", {
  ce <- new_cache_env()
  cache_set_cap(ce, 100)
  cache_store(ce, "memory", "small", "x", c(0L), size_bytes = 50)
  # Oversized entry: stored, others evicted; still present.
  cache_store(ce, "memory", "big", "X", c(0L), size_bytes = 500)
  expect_false(exists("small", envir = ce$memory, inherits = FALSE))
  expect_true( exists("big",   envir = ce$memory, inherits = FALSE))
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement LRU + cap in `R/cache.R`**

Replace the existing `cache_store` / `cache_lookup` with the LRU-aware versions:

```r
# ---- Cache with LRU + memory-cap (applies to memory + query tiers) -----------

new_cache_env <- function() {
  e <- new.env(parent = emptyenv())
  e$mapped   <- new.env(parent = emptyenv())
  e$memory   <- new.env(parent = emptyenv())
  e$query    <- new.env(parent = emptyenv())
  e$lru      <- character(0L)   # MRU at tail; keys encode "tier:key"
  e$bytes    <- 0               # bytes used across memory + query
  e$cap      <- .cache_default_cap()
  e
}

.cache_default_cap <- function() {
  mb <- dafr_opt("dafr.cache.memory_mb")
  as.numeric(mb) * 1024 * 1024
}

cache_set_cap <- function(cache_env, bytes) {
  cache_env$cap <- as.numeric(bytes)
  .cache_evict(cache_env)
  invisible()
}

.is_capped_tier <- function(tier) tier %in% c("memory", "query")

.lru_key <- function(tier, key) paste0(tier, ":", key)

.lru_touch <- function(cache_env, tier, key) {
  k <- .lru_key(tier, key)
  cache_env$lru <- c(setdiff(cache_env$lru, k), k)
}

.lru_drop <- function(cache_env, tier, key) {
  cache_env$lru <- setdiff(cache_env$lru, .lru_key(tier, key))
}

.cache_evict <- function(cache_env) {
  while (cache_env$bytes > cache_env$cap && length(cache_env$lru) > 0L) {
    victim <- cache_env$lru[[1L]]
    cache_env$lru <- cache_env$lru[-1L]
    parts <- regmatches(victim, regexpr(":", victim), invert = TRUE)[[1L]]
    tier  <- parts[[1L]]; key <- parts[[2L]]
    bucket <- cache_env[[tier]]
    if (exists(key, envir = bucket, inherits = FALSE)) {
      entry <- get(key, envir = bucket, inherits = FALSE)
      cache_env$bytes <- cache_env$bytes - entry$size
      rm(list = key, envir = bucket)
    }
  }
}

cache_store <- function(cache_env, tier, key, value, stamp, size_bytes = 0L) {
  size_bytes <- as.numeric(size_bytes)
  bucket <- cache_env[[tier]]
  if (exists(key, envir = bucket, inherits = FALSE) && .is_capped_tier(tier)) {
    old <- get(key, envir = bucket, inherits = FALSE)
    cache_env$bytes <- cache_env$bytes - old$size
  }
  assign(key,
         list(value = value, stamp = stamp, size = size_bytes),
         envir = bucket)
  if (.is_capped_tier(tier)) {
    cache_env$bytes <- cache_env$bytes + size_bytes
    .lru_touch(cache_env, tier, key)
    .cache_evict(cache_env)
  }
  invisible()
}

cache_lookup <- function(cache_env, tier, key, expected_stamp) {
  bucket <- cache_env[[tier]]
  if (!exists(key, envir = bucket, inherits = FALSE)) return(NULL)
  entry <- get(key, envir = bucket, inherits = FALSE)
  if (!identical(entry$stamp, expected_stamp)) {
    if (.is_capped_tier(tier)) {
      cache_env$bytes <- cache_env$bytes - entry$size
      .lru_drop(cache_env, tier, key)
    }
    rm(list = key, envir = bucket)
    return(NULL)
  }
  if (.is_capped_tier(tier)) .lru_touch(cache_env, tier, key)
  entry$value
}
```

Also update `empty_cache` to reset `bytes` and `lru`:

```r
empty_cache <- function(daf, group = c("mapped", "memory", "query")) {
  group <- match.arg(group, choices = c("mapped", "memory", "query"), several.ok = TRUE)
  cache_env <- S7::prop(daf, "cache")
  for (tier in group) {
    bucket <- cache_env[[tier]]
    rm(list = ls(bucket, all.names = TRUE), envir = bucket)
    if (.is_capped_tier(tier)) {
      cache_env$lru <- cache_env$lru[!startsWith(cache_env$lru, paste0(tier, ":"))]
    }
  }
  if (any(.is_capped_tier(group))) {
    # Recompute bytes from what's left in the capped tiers.
    total <- 0
    for (t in c("memory", "query")) {
      bucket <- cache_env[[t]]
      for (k in ls(bucket, all.names = TRUE)) {
        total <- total + get(k, envir = bucket, inherits = FALSE)$size
      }
    }
    cache_env$bytes <- total
  }
  invisible(daf)
}
```

- [ ] **Step 4: Run — expect pass** (all cache tests, plus all previously passing vector/matrix tests)

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/cache.R tests/testthat/test-cache.R
git commit -m "feat(cache): LRU eviction + memory-cap on memory/query tiers"
```

### Task I3: `empty_cache` keep/clear semantics matching Julia

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/cache.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-cache.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("empty_cache accepts 'clear' and 'keep' groups (Julia parity)", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  set_vector(d, "cell", "v", 1.0)
  get_vector(d, "cell", "v")   # populate memory tier
  ce <- S7::prop(d, "cache")
  expect_true(length(ls(ce$memory)) > 0L)

  empty_cache(d, clear = "MappedData")
  expect_true(length(ls(ce$memory)) > 0L)       # memory left alone

  empty_cache(d, keep = "MemoryData")
  expect_true(length(ls(ce$memory)) > 0L)       # memory preserved, others cleared

  empty_cache(d)
  expect_equal(length(ls(ce$memory)), 0L)
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Replace `empty_cache` with a variant that accepts either `group=`, `clear=`, or `keep=`:

```r
#' Empty caches on a Daf object.
#'
#' Exactly one of `group`, `clear`, or `keep` may be supplied. Group
#' names use the short form (`"mapped"`, `"memory"`, `"query"`) or the
#' Julia-style capitalised form (`"MappedData"`, `"MemoryData"`,
#' `"QueryData"`).
#'
#' @param daf A `DafReader`/`DafWriter` instance.
#' @param group Character vector of tiers to clear (defaults to all).
#' @param clear Character vector of tiers to clear (alternative to `group`).
#' @param keep Character vector of tiers to keep; all others are cleared.
#' @return Invisibly the input `daf`.
#' @export
empty_cache <- function(daf,
                        group = NULL,
                        clear = NULL,
                        keep  = NULL) {
  all_tiers <- c("mapped", "memory", "query")
  n_specified <- sum(!is.null(group), !is.null(clear), !is.null(keep))
  if (n_specified > 1L) {
    stop("specify at most one of `group`, `clear`, `keep`", call. = FALSE)
  }
  chosen <- if (!is.null(group)) group
            else if (!is.null(clear)) clear
            else if (!is.null(keep)) setdiff(all_tiers, .canonical_tier(keep))
            else all_tiers
  chosen <- .canonical_tier(chosen)

  cache_env <- S7::prop(daf, "cache")
  for (tier in chosen) {
    bucket <- cache_env[[tier]]
    rm(list = ls(bucket, all.names = TRUE), envir = bucket)
    if (.is_capped_tier(tier)) {
      cache_env$lru <- cache_env$lru[!startsWith(cache_env$lru, paste0(tier, ":"))]
    }
  }
  total <- 0
  for (t in c("memory", "query")) {
    bucket <- cache_env[[t]]
    for (k in ls(bucket, all.names = TRUE)) {
      total <- total + get(k, envir = bucket, inherits = FALSE)$size
    }
  }
  cache_env$bytes <- total
  invisible(daf)
}

.canonical_tier <- function(x) {
  map <- c(
    mapped     = "mapped", memory     = "memory", query      = "query",
    MappedData = "mapped", MemoryData = "memory", QueryData  = "query"
  )
  out <- map[x]
  if (any(is.na(out))) {
    stop(sprintf("unknown cache tier(s): %s",
                 paste(sQuote(x[is.na(out)]), collapse = ", ")),
         call. = FALSE)
  }
  unname(out)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/cache.R NAMESPACE man/empty_cache.Rd tests/testthat/test-cache.R
git commit -m "feat(cache): empty_cache Julia-style clear/keep tier names"
```

---

## Phase J — bit64 + cli first uses

### Task J1: bit64::integer64 round-trip through vectors

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/memory_daf.R` (adjust `.validate_vector_value` if needed)
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-vectors.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("set_vector round-trips bit64::integer64 vectors", {
  skip_if_not_installed("bit64")
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  big <- bit64::as.integer64(c(1e10, 2e10, 3e10))
  set_vector(d, "cell", "big", big)
  got <- get_vector(d, "cell", "big")
  expect_s3_class(got, "integer64")
  expect_equal(as.numeric(got), as.numeric(big), ignore_attr = TRUE)
  expect_equal(names(got), c("A", "B", "C"))
})

test_that("named bit64 vector reorders to axis order", {
  skip_if_not_installed("bit64")
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  v <- bit64::as.integer64(c(2e10, 1e10))
  names(v) <- c("B", "A")
  set_vector(d, "cell", "big", v)
  got <- get_vector(d, "cell", "big")
  expect_equal(as.numeric(unname(got)), c(1e10, 2e10))
})
```

- [ ] **Step 2: Run — expect fail** if `is.atomic` or name handling breaks on integer64 (it is atomic — a double with class attr — but the reorder code uses `vec[entries]` which bit64 overrides correctly; validate).

- [ ] **Step 3: Implement (adjust if needed)**

Most likely the existing `.validate_vector_value` already handles `integer64` because:
- `is.atomic(integer64)` returns TRUE (it's a double with S3 class).
- `x[names]` dispatches to `bit64::[.integer64`.

If `unname()` strips the `integer64` class, replace the `unname(vec[entries])` call with:

```r
  vec <- vec[entries]
  names(vec) <- NULL
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/memory_daf.R tests/testthat/test-memory-vectors.R
git commit -m "feat(memory_daf): preserve bit64::integer64 class through named-reorder path"
```

### Task J2: `cli` for user messages on `empty_cache` + `add_axis`

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/cache.R`
- Modify: `/home/aviezerl/src/dafr-native/R/writers.R`
- Modify: `/home/aviezerl/src/dafr-native/R/options.R`
- Test: `/home/aviezerl/src/dafr-native/tests/testthat/test-options.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("empty_cache emits a cli message when dafr.verbose = TRUE", {
  d <- memory_daf(name = "t")
  withr::with_options(list(dafr.verbose = TRUE), {
    expect_message(empty_cache(d), "empty_cache")
  })
  withr::with_options(list(dafr.verbose = FALSE), {
    expect_no_message(empty_cache(d))
  })
})

test_that("add_axis emits a cli message when dafr.verbose = TRUE", {
  d <- memory_daf(name = "t")
  withr::with_options(list(dafr.verbose = TRUE), {
    expect_message(add_axis(d, "cell", c("A", "B")), "add_axis")
  })
})
```

- [ ] **Step 2: Run — expect fail**

- [ ] **Step 3: Implement**

Add to `.dafr_default_options` in `R/options.R`:

```r
.dafr_default_options <- list(
  dafr.cache.memory_mb = 1024L,
  dafr.cache.disable   = FALSE,
  dafr.cache.stats     = FALSE,
  dafr.mmap            = TRUE,
  dafr.omp_threshold   = 10000L,
  dafr.inefficient     = "warn",
  dafr.verbose         = FALSE
)
```

Add a small helper:

```r
.cli_verbose <- function(msg, ...) {
  if (isTRUE(dafr_opt("dafr.verbose"))) {
    cli::cli_inform(c("i" = sprintf(msg, ...)))
  }
  invisible()
}
```

Wire it into `empty_cache` (top of body):

```r
  .cli_verbose("empty_cache on %s tier(s): %s",
               S7::prop(daf, "name"),
               paste(chosen, collapse = ", "))
```

And into `add_axis`:

```r
add_axis <- function(daf, axis, entries) {
  stopifnot(is.character(axis),   length(axis) == 1L, !is.na(axis))
  stopifnot(is.character(entries))
  .cli_verbose("add_axis %s (%d entries) on %s",
               axis, length(entries), S7::prop(daf, "name"))
  format_add_axis(daf, axis, entries)
  invisible(daf)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/options.R R/cache.R R/writers.R tests/testthat/test-options.R
git commit -m "feat(cli): optional verbose messages via dafr.verbose; closes unused-import NOTE"
```

---

## Phase K — Slice-0 loose ends

### Task K1: Move `is_altrep` to test helper

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/R/altrep.R` (or wherever it lives)
- Create: `/home/aviezerl/src/dafr-native/tests/testthat/helper-altrep.R`
- Modify: callers under `tests/testthat/` that use `dafr:::is_altrep`

- [ ] **Step 1: Find existing callers**

Run Grep:

```bash
cd /home/aviezerl/src/dafr-native
```

```r
# From R:
grep -rn "is_altrep" R tests
```

- [ ] **Step 2: Create `tests/testthat/helper-altrep.R`**

```r
# Test-only helper: returns TRUE iff `x` is ALTREP.
# Lives here (not in the package namespace) to avoid exposing an
# unexported-but-user-reachable API via `dafr:::is_altrep`.
is_altrep <- function(x) {
  .Internal(inspect(x))
  # If .Internal is not usable (R sandboxing), fall back to checking the
  # ALTREP bit via base R API:
  isTRUE(.Call("ALTREP_TEST", x, PACKAGE = "dafr"))
}
```

(If the existing body in `R/altrep.R` already uses `.Call` into a package-level C entry, preserve that and move it verbatim.)

- [ ] **Step 3: Remove the helper from `R/altrep.R`**

Delete the function; leave a short comment if it clarifies intent.

- [ ] **Step 4: Update callers**

Rewrite every `dafr:::is_altrep(x)` in tests as `is_altrep(x)` (now loaded via `helper-altrep.R`).

- [ ] **Step 5: Run the full suite — expect pass**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

- [ ] **Step 6: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add R/altrep.R tests/testthat/helper-altrep.R tests/testthat/*.R
git commit -m "refactor(altrep): move is_altrep() from package namespace to test helper"
```

### Task K2: Comments beside const_cast in mmap_region.cpp / altrep_mmap.cpp

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/src/mmap_region.cpp`
- Modify: `/home/aviezerl/src/dafr-native/src/altrep_mmap.cpp`

- [ ] **Step 1: Locate existing const_cast uses**

```bash
cd /home/aviezerl/src/dafr-native
```

```r
# Find the two call sites mentioned in slice-0-kickoff:
grep -n "const_cast" src/*.cpp
```

- [ ] **Step 2: Add a comment beside each**

At each `const_cast<void*>(...)` call site, prepend:

```cpp
// SAFETY: ALTREP contract — caller must not write to the returned
// pointer when writeable=FALSE (we mapped with PROT_READ). We const_cast
// only to satisfy the ALTREP Dataptr signature, not to grant write
// access. See altrep convention §2 and R-ints §1.14.4.
```

- [ ] **Step 3: Rebuild + run tests**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

- [ ] **Step 4: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add src/mmap_region.cpp src/altrep_mmap.cpp
git commit -m "docs(mmap): comment each const_cast<void*> on read-only mapping"
```

### Task K3: One OpenMP-parallel-branch test per kernel

**Files:**
- Modify/Create: `/home/aviezerl/src/dafr-native/tests/testthat/test-kernel-openmp.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("kernel_eltwise_log_add triggers the OMP branch (ncol >= threshold)", {
  threshold <- getOption("dafr.omp_threshold", 10000L)
  n <- as.integer(max(threshold, 20000L))
  x <- runif(n, min = 0.1, max = 10)
  y <- runif(n)
  out <- kernel_eltwise_log_add_cpp(x, y)
  expect_equal(out, log(x) + y, tolerance = 1e-12)
})

test_that("kernel_csc_colsums triggers the OMP branch (ncol >= threshold)", {
  threshold <- getOption("dafr.omp_threshold", 10000L)
  ncol <- as.integer(max(threshold, 20000L))
  nrow <- 50L
  m <- Matrix::rsparsematrix(nrow, ncol, density = 0.02)
  out <- kernel_csc_colsums_cpp(m@x, m@i, m@p, nrow, ncol)
  expect_equal(out, Matrix::colSums(m), tolerance = 1e-12)
})
```

(If the exported C++ wrapper names differ, match those — grep `cpp11::register` annotations.)

- [ ] **Step 2: Run — expect fail / pass depending on existing names**

- [ ] **Step 3: Align test symbols with actual cpp11-exported names**

```bash
grep -rn "\[\[cpp11::register\]\]" src/
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add tests/testthat/test-kernel-openmp.R
git commit -m "test(kernel): exercise OpenMP branch on eltwise + colsums kernels"
```

---

## Phase L — Test port from DafJuliaWrapper

Each ported file translates Julia-bridge-dependent tests into MemoryDaf-only tests. The reference suite is at `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr/tests/testthat/`. Strip `skip_if(!JULIA_AVAILABLE, ...)`, `setup_daf()`, `h5df` / FilesDaf arms; keep only the `MemoryDaf` paths. Any test that requires `description()`, query execution, views, chains, contracts, adapters, computations, copies, concat, complete, reconstruction, example data, operations, groups, or log — **skip for Slice 1**, defer to the slice that introduces that subsystem.

### Task L1: Port `test-formats.R` scalar + axis sections

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-scalars.R` (append)
- Modify: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-axes.R` (append)

- [ ] **Step 1: Read source**

```r
# Already reviewed earlier; focus on the MemoryDaf arms.
# Reference: /net/mraid20/.../src/dafr/tests/testthat/test-formats.R lines 32-177.
```

- [ ] **Step 2: Translate scalar loop** (wrap the `formats[[1]]` arm — MemoryDaf — into expressive test_that blocks). Copy the structure, drop `skip_if(!JULIA_AVAILABLE, ...)`, drop `create_fn` abstraction (just use `memory_daf()` directly), drop `description()` comparisons (done in Phase H).

Example:

```r
test_that("string scalar round-trip", {
  d <- memory_daf(name = "test!")
  expect_equal(length(scalars_set(d)), 0L)
  expect_false(has_scalar(d, "foo"))
  set_scalar(d, "foo", "1.0.1")
  expect_error(set_scalar(d, "foo", NA), "NA")
  expect_error(set_scalar(d, "foo", "x"), "already exists")
  expect_true(has_scalar(d, "foo"))
  expect_equal(get_scalar(d, "foo"), "1.0.1")
  expect_equal(scalars_set(d), "foo")
  expect_error(get_scalar(d, "savta"), "does not exist")
  expect_equal(get_scalar(d, "savta", 17), 17)
  delete_scalar(d, "foo")
  expect_equal(length(scalars_set(d)), 0L)
  expect_false(has_scalar(d, "foo"))
})

test_that("double + integer scalars round-trip", {
  d <- memory_daf()
  for (v in list(0.5, 1L, TRUE)) {
    set_scalar(d, "foo", v, overwrite = TRUE)
    expect_true(has_scalar(d, "foo"))
    expect_equal(get_scalar(d, "foo"), v)
  }
})
```

- [ ] **Step 3: Translate axis loop** into `test-memory-axes.R`.

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add tests/testthat/test-memory-scalars.R tests/testthat/test-memory-axes.R
git commit -m "test(port): scalars + axes from DafJuliaWrapper::test-formats.R"
```

### Task L2: Port `test-formats.R` vector + matrix sections

**Files:**
- Modify: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-vectors.R` (append)
- Modify: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-matrices.R` (append)

- [ ] **Step 1: Read source** (test-formats.R lines ~180-540).

Key behaviors to port:
- `get_vector(daf, axis, name, default = NA)` returns NA-filled axis-named vector.
- `get_vector(daf, axis, name, default = "savta")` returns constant-filled axis-named char vec.
- `get_matrix(..., default = NA)` — dimnamed NA-filled matrix.
- Overwrite errors.
- Length-mismatch errors.
- Round-trip of dense int / double / logical / character vectors.
- Round-trip of dense + sparse matrices.
- `delete_vector` / `delete_matrix` with `must_exist` semantics.

- [ ] **Step 2: Translate — one `test_that` per originally-grouped assertion**

Skip any assertion that depends on Julia types (e.g., "expect Float32 storage type"); we store native R types.

- [ ] **Step 3: Run — expect pass**

- [ ] **Step 4: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add tests/testthat/test-memory-vectors.R tests/testthat/test-memory-matrices.R
git commit -m "test(port): vectors + matrices from DafJuliaWrapper::test-formats.R"
```

### Task L3: Port `test-cache.R` subset

**Files:**
- Create: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-cache.R`

- [ ] **Step 1: Read source** (`/net/mraid20/.../src/dafr/tests/testthat/test-cache.R`).

Port these test cases only (MemoryDaf scope):
- `empty_cache works` — trivial.
- `get_vector returns cached result on second call`.
- `get_vector cache is invalidated after set_vector with overwrite`.
- `empty_cache clears the R-side cache`.

Drop: any test calling `dafJuliaWrapper:::get_daf_id` or `.daf_cache_registry` (those are bridge implementation details).

- [ ] **Step 2: Write**

```r
test_that("empty_cache on a populated memory tier leaves the store intact", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
  set_matrix(d, "cell", "gene", "UMIs", m)
  get_matrix(d, "cell", "gene", "UMIs")   # populate cache
  empty_cache(d)
  expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
               m, ignore_attr = TRUE)
  empty_cache(d, clear = "MappedData")
  empty_cache(d, keep  = "MemoryData")
  expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
               m, ignore_attr = TRUE)
})

test_that("get_vector returns identical cached result on second call", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  set_vector(d, "cell", "score", c(1.0, 2.0, 3.0))
  expect_identical(get_vector(d, "cell", "score"),
                   get_vector(d, "cell", "score"))
})

test_that("get_vector cache invalidates after overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  set_vector(d, "cell", "score", c(1.0, 2.0, 3.0))
  expect_equal(unname(get_vector(d, "cell", "score")), c(1.0, 2.0, 3.0))
  set_vector(d, "cell", "score", c(10.0, 20.0, 30.0), overwrite = TRUE)
  expect_equal(unname(get_vector(d, "cell", "score")), c(10.0, 20.0, 30.0))
})

test_that("empty_cache clears the memory tier", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  set_vector(d, "cell", "score", c(1.0, 2.0, 3.0))
  get_vector(d, "cell", "score")
  ce <- S7::prop(d, "cache")
  expect_gt(length(ls(ce$memory)), 0L)
  empty_cache(d)
  expect_equal(length(ls(ce$memory)), 0L)
})
```

- [ ] **Step 3: Run — expect pass**

- [ ] **Step 4: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add tests/testthat/test-memory-cache.R
git commit -m "test(port): cache behavior from DafJuliaWrapper::test-cache.R"
```

### Task L4: Port `test-copies.R` / `test-data-writers.R` MemoryDaf-accessible bits

**Files:**
- Create: `/home/aviezerl/src/dafr-native/tests/testthat/test-memory-writers.R`

- [ ] **Step 1: Scan `test-data-writers.R`**

```r
# Look at /net/mraid20/.../src/dafr/tests/testthat/test-data-writers.R
# Port only the tests that exercise:
#  - add_axis(daf, axis, c(...))
#  - set_vector(daf, axis, name, v)
#  - set_matrix(daf, rows_axis, cols_axis, name, m)
#  - delete_* and overwrite semantics
#  - set_vector accepts bit64 when declared Int64 on the Julia side
# Skip copy_all / copy_vector / copy_matrix / views / chains / contracts.
```

- [ ] **Step 2: Translate** — keep only MemoryDaf-reachable cases. For any test requiring types we don't yet support natively (e.g., explicit `"Int64"` annotations), lean on bit64 and expect the round-trip by class, not by string type annotation.

- [ ] **Step 3: Run — expect pass**

- [ ] **Step 4: Commit**

```bash
cd /home/aviezerl/src/dafr-native
git add tests/testthat/test-memory-writers.R
git commit -m "test(port): writer cases from DafJuliaWrapper::test-data-writers.R"
```

---

## Phase M — Slice 1 exit gate

### Task M1: Full R CMD check — 0 ERRORs, 0 WARNINGs, 0 NOTEs

**Files:** none.

- [ ] **Step 1: Regenerate roxygen + NAMESPACE**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::document()'
```

- [ ] **Step 2: Run the full test suite**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: 0 failures.

- [ ] **Step 3: Run R CMD check**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::check(error_on = "note", manual = FALSE, vignettes = FALSE)'
```

Expected: 0 ERRORs, 0 WARNINGs, 0 NOTEs. If a NOTE about `bit64`/`cli` as unused imports persists, grep the codebase for actual use — Phase J should have closed both.

- [ ] **Step 4: If any warnings / notes remain, fix and re-run before committing**

No task completion without a clean check. Don't paper over with `--no-check-*` flags.

### Task M2: Slice-1 exit note

**Files:** Create: `/home/aviezerl/src/dafr-native/dev/notes/slice-1-exit.md`

- [ ] **Step 1: Write**

```markdown
# Slice 1 exit gate — <date>

## Deliverables

- [x] MemoryDaf class + 22 format_* methods (axes / scalars / vectors / matrices / relayout)
- [x] User-facing read API (has/get/*_set; axis_*/get_vector/get_matrix with defaults + dimnames)
- [x] User-facing write API (set_*/delete_*/add_axis/relayout_matrix with validation + version bumps)
- [x] description() covering scalars / axes / vectors / matrices
- [x] Cache: version-stamp invalidation + LRU + memory cap + Julia-style empty_cache(clear=/keep=)
- [x] `bit64::integer64` round-trip through set_vector / get_vector
- [x] `cli::cli_inform` via `dafr.verbose = TRUE` (closes `cli` unused-Import NOTE)
- [x] `is_altrep()` moved to `tests/testthat/helper-altrep.R`
- [x] const_cast comments in mmap_region.cpp + altrep_mmap.cpp
- [x] OpenMP-branch tests on eltwise + colsums kernels
- [x] Test port from DafJuliaWrapper: scalars, axes, vectors, matrices, cache, writers

## Test + check status

- `testthat::test_dir("tests/testthat")` — N files, M test_that blocks, 0 failures.
- `R CMD check` — 0 ERRORs, 0 WARNINGs, 0 NOTEs.

## Still open for Slice 2

- G2 upstream PR (awaits user consent).
- CSC colSums bake-off re-run at 100M+ nnz.
- Transpose kernel B vs D decision.
- Julia FilesDaf findings (no on-disk version counters, no atomicity) — Slice 2 design question.
- writeBin endianness pin.
- Long-vector ALTREP scenarios (deferred).

## Decision to enter Slice 2

Go / No-go: <decision>. Slice 2 (FilesDaf + mmap + Julia bidirectional compat + readBin fallback) can start against the now-stable MemoryDaf + user-facing API.
```

- [ ] **Step 2: Commit (dev repo)**

```bash
cd /home/aviezerl/src/dafr-native/dev
git add notes/slice-1-exit.md
git commit -m "notes: Slice 1 exit gate"
```

### Task M3: Tag slice-1

**Files:** none.

- [ ] **Step 1: Tag (package repo)**

```bash
cd /home/aviezerl/src/dafr-native
git tag -a slice-1 -m "Slice 1: MemoryDaf + user-facing API + cache infrastructure"
```

- [ ] **Step 2: Push (only with user consent — ask first)**

```bash
# With user consent:
cd /home/aviezerl/src/dafr-native
git push origin main --follow-tags
```

- [ ] **Step 3: Trigger CI verification**

After the push, monitor the R-CMD-check + altrep-sanity workflows; re-run locally if any platform-specific failures surface.

Slice 1 complete. Next plan: **Slice 2 — FilesDaf + mmap + Julia bidirectional compat + readBin fallback**.

---

## Self-review

### 1. Spec coverage

| Slice 1 deliverable (from kickoff breadcrumb) | Task(s) |
|---|---|
| MemoryDaf class + 22 format_* methods | A1 (class), B1–B2 (axes), C1–C2 (scalars), D1–D2 (vectors), F1–F3 (matrices + relayout) |
| Axis add/delete/query user-facing | B3 |
| Scalar get/set user-facing | C3 |
| Vector get/set user-facing (dense + sparse, named, bit64, character) | E1, E2, J1 |
| Matrix get/set user-facing (dense + dgCMatrix/lgCMatrix) | F2, F3 (relayout), G1, G2 |
| Cache: LRU + memory cap + version-stamp + query-result hook | E1 (stamps on get), I1–I3 (LRU + cap + empty_cache) |
| Test port from DafJuliaWrapper | L1–L4 |
| bit64 + cli first real uses | J1, J2 |
| description() (implicit; referenced in ported tests) | H1 |

All eight breadcrumb deliverables mapped. Open risks from breadcrumb scoped into K1 (`is_altrep`), K2 (const_cast comments), K3 (OMP-branch tests), J2 (cli), J1 (bit64). Deferred items explicitly listed under "Scope decisions."

### 2. Placeholder scan

Grepped my own plan text for "TBD", "TODO", "FIXME", "fill in", "implement later", "similar to", "appropriate", "edge cases":

- No `TBD` / `TODO` / `FIXME` remain outside of the shipped exit-note template (which prompts a Go/No-go and a date — intentional and explicit).
- Task L2 uses "Skip any assertion that depends on Julia types" — this is an instruction to the implementer, not a placeholder; the surrounding bullets spell out what to port and what to drop.
- Task J1 step 3 says "adjust if needed" but includes the exact patch to apply — explicit contingency, not a placeholder.

### 3. Type / name consistency

Spot-checked:

- `memory_daf()` constructor name consistent across A1, B3 tests, C1 tests, etc.
- `MemoryDaf` S7 class name consistent.
- `format_axis_array` (generic) vs `axis_vector` / `axis_entries` (user-facing) — three distinct names, all correctly scoped (generic vs exported).
- `cache_key_vector` / `cache_key_matrix` from existing `R/cache.R` — reused in E1, G1.
- `vector_stamp(daf, axis, name)` / `matrix_stamp(daf, rows_axis, cols_axis, name)` — defined in E1, reused in G1 with same arity.
- `cache_lookup(cache_env, tier, key, expected_stamp)` / `cache_store(cache_env, tier, key, value, stamp, size_bytes)` — defined in E1 (skeleton) and extended in I2 (LRU). Signatures match at both use and definition sites.
- `.is_capped_tier`, `.lru_touch`, `.lru_drop`, `.cache_evict` — all defined inside I2 block, used inside same block.
- `empty_cache(daf, group=, clear=, keep=)` — defined in I3; called with `clear = "MappedData"` and `keep = "MemoryData"` in L3. Canonicalisation via `.canonical_tier` handles both short and capitalised forms.
- `format_relayout_matrix(daf, rows_axis, columns_axis, name)` — S7 generic dispatch args match existing `R/format_api.R` declaration.
- `.cli_verbose` — defined in J2, used inside the same file.
- `S7::prop(daf, "internal")$scalars/axes/vectors/matrices` — shape fixed in A1; referenced throughout B-F.
- `bump_axis_counter` / `bump_vector_counter` / `bump_matrix_counter` — defined in Slice 0's `R/cache.R`, reused as-is.

No inconsistencies found in my own additions.

### 4. Known oversimplifications

- `cache_env$lru` uses a plain character vector with `setdiff`/concatenation — O(n) per touch/evict. Fine for ~thousands of keys per store; will need a real doubly-linked list (C++) for >10⁶-key caches. Slice 1 users cap out well below that.
- Oversized-entry handling in Task I2 (last test) accepts a single entry even when it exceeds `cap` — it evicts everything else then stores the one. Matches Julia's "best-effort" semantics and avoids silent data loss.
- `object.size()` is approximate (it traces R-side allocation, not ALTREP-mmap bytes) — good enough for MemoryDaf; FilesDaf will measure differently when it lands in Slice 2.
