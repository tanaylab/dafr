# Slice 2 — FilesDaf backend + mmap/readBin + bidirectional Julia compat

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship a working `FilesDaf` backend that reads and writes Julia-`DataAxesFormats.FilesDaf`-compatible on-disk stores, with an ALTREP mmap read path for numeric payloads, a `readBin` fallback, non-atomic writes mirroring Julia's model, and full MemoryDaf↔FilesDaf round-trip coverage.

**Architecture:**
- `R/files_daf.R` — concrete `FilesDaf` S7 class under `DafWriter`; implements the 22 `format_*` generics over directory-backed storage.
- `R/files_io.R` — low-level JSON + binary I/O helpers (type tables, descriptor read/write, dense/sparse binary read/write, name validation).
- `R/files_daf_read.R`, `R/files_daf_write.R` — format-method implementations split by direction for reviewability.
- `R/readers.R` / `R/writers.R` / `R/memory_daf.R` — drive-by cleanups (rename `cols_axis → columns_axis`, harden `.assert_name`, fix `get_vector(default=)`, drop `empty_cache(group=)`, fix `.memory_matrix_bucket` leak).
- `R/cache.R` — delete vestigial `cache_get/put/remove`; port remaining call sites to `cache_lookup`/`cache_store`.
- `tests/testthat/test-files-*.R` — TDD suite for FilesDaf per section.
- `tests/testthat/test-julia-compat.R` — bidirectional round-trip test against a pre-generated Julia FilesDaf fixture (stored under `tests/testthat/fixtures/julia-filesdaf/`).

**Tech Stack:** R 4.4+, S7 0.2.1, `jsonlite` (new Import) for daf.json and descriptor JSON, `Matrix` (dgCMatrix), `bit64` (Int64 scalar support), cpp11 ALTREP kernels from Slice 0.

**Repo layout:**
- Package repo: `/home/aviezerl/src/dafr-native/` (`main`, tracks `origin/main` at `git@github.com:tanaylab/dafr.git`, tag `slice-1`).
- Dev repo (nested, gitignored): `/home/aviezerl/src/dafr-native/dev/`. Plans + notes + specs live here.
- Source + tests commits → package repo. Plan + notes + spec commits → dev repo. Infer from file path; use `cd ~/src/dafr-native` or `cd ~/src/dafr-native/dev` explicitly.

**Dev loop per task:**
1. From the package root:
   ```
   Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "<tag>")'
   ```
2. Inspect output; iterate until green.
3. Stage + commit with the provided message.

---

## Pre-planning decisions (settled before tasks)

### 1. Atomicity model — mirror Julia (non-atomic writes)

Julia's `DataAxesFormats.FilesDaf` writes files in place: `open(path, "w")` then write; no `.tmp`+rename, no fsync. A mid-write crash can leave a partially-written file. Multi-process concurrent writes are unsafe; concurrency is handled by in-process Julia read/write locks.

**Native-R mirrors this contract for v1.** Rationale:

- Byte-for-byte compat with Julia's writer means the two implementations agree on recovery mode (i.e., none). Users who restore from backups, not partial writes.
- Atomicity introduces complexity (temp directories, ordering of parent dir `fsync` on POSIX vs `FlushFileBuffers` on Windows, rename-over semantics) that divergence from Julia forces us to diagnose alone.
- The FilesDaf on-disk spec (pre-Slice-2 draft) already documents "no atomicity" as the intended contract (spec §11). Slice 2's write path is the reference implementation of that contract.

**Constraint to document** (in `?files_daf` and the user-facing `README`): one writer at a time per store. Multi-process or multi-thread writes to the same store yield undefined behaviour. Revisit if a user encounters this.

### 2. Resolve three `[UNCLEAR]` markers in `dev/specs/filesdaf-on-disk-spec-draft.md`

- **§4 "Float32 JSON precision round-trip"** — **N/A for the R backend.** R's native `double` is IEEE 754 binary64 (Float64); R has no Float32 scalar type. The writer always emits `"Float64"`. The reader accepts `"Float32"` on input and promotes to R `double` (precision is already limited by the JSON representation). We document this promotion in the spec Appendix.
- **§11 "External-writer atomicity contract"** — **Clarified: no ordering guarantee.** Writers must complete all file writes for a given property before any reader touches the store. Multi-process or multi-writer scenarios require external coordination (file lock, sentinel, scheduler). The spec adds an explicit note.
- **Appendix "`Int`/`int` mapping"** — **Clarified: treat as `Int64` on read.** Writers never emit `"Int"` because both Julia (on 64-bit platforms, `Int == Int64`) and the R backend (R `integer` → `"Int32"`, `bit64::integer64` → `"Int64"`) produce explicit widths. Readers accept `"Int"`/`"int"` and deserialize as `Int64` for compatibility with any hypothetical hand-written store.

All three edits land in Phase M1 before the upstream PR.

### 3. Sparse-vector write path — adaptive, matching Julia's size heuristic

Julia's `FilesDaf` picks sparse over dense on-disk layout for a vector when
the size heuristic in spec §8 / §8.4 is met. For **full bidirectional
byte-equivalence** (R writes must be indistinguishable from Julia writes
of the same data), R's `set_vector` applies the same heuristic on every
numeric / Bool / string input.

**Heuristic for numeric / Bool vectors** (spec §8):

```
indtype       = if axis_length <= .Machine$integer.max "UInt32" else "UInt64"
nnz           = number of non-zero (Bool: non-FALSE) elements
sparse_bytes  = nnz * (sizeof(eltype) + sizeof(indtype))
dense_bytes   = axis_length * sizeof(eltype)
choose sparse iff sparse_bytes <= 0.75 * dense_bytes
```

**Heuristic for string vectors** (spec §8.4, verbatim):

```
indtype            = UInt32 or UInt64 per axis length
n_nonempty         = count of non-empty entries
nonempty_bytes     = sum(nchar(nonempty entries))
sparse_size        = nonempty_bytes + n_nonempty * (1 + sizeof(indtype))
dense_size         = nonempty_bytes + axis_length
choose sparse iff sparse_size <= 0.75 * dense_size
```

**Bool all-TRUE optimisation** (spec §8.3): when sparse is chosen and every
non-zero value is `TRUE`, omit the `.nzval` file entirely. Readers
synthesise `fill(TRUE, nnz)` — matches Julia.

**Explicit sparse input**: `set_vector` also accepts `Matrix::sparseVector`
objects. When the input is already a `sparseVector`, R writes sparse
unconditionally (no heuristic re-evaluation) — matches Julia's behaviour
of respecting the input type.

**Scan cost**: O(N) — the same order as writing the payload itself. No
change in asymptotic complexity.

**Rationale**: R has no native sparse-vector atomic type, so "type in →
type out" (Julia's policy for `Vector` vs `SparseVector`) cannot map
directly. Auto-sparsification by data content preserves the byte-level
round-trip guarantee, which is the contract we actually care about for
bidirectional compat.

**Matrices remain type-driven** (spec §7 / §9): dense R `matrix` → dense on
disk; `dgCMatrix`/`lgCMatrix` → sparse on disk. Matches Julia's matrix
policy exactly; R users who want sparse matrices already have to go
through the `Matrix` package, so the type is the natural signal.

### 4. Matrix sparse index conversion — read eagerly, mmap nzval

Julia stores CSC `colptr` and `rowval` as 1-based `UInt32` or `UInt64`. R's `dgCMatrix` uses 0-based `int32`. Bidirectional conversion requires a subtract-1 traversal, which precludes direct mmap of `colptr`/`rowval`. For v1:

- `colptr` and `rowval` → read eagerly with `readBin`, subtract 1, use as dgCMatrix `@p` / `@i`. Both are small relative to `nzval` (ncols+1 and nnz int32s vs nnz doubles).
- `nzval` (always Float64 for our write path; read path accepts all real types) → mmap via ALTREP when the eltype fits `dgCMatrix@x` (i.e., Float64). For other eltypes (Int*, Bool, Float32) we eager-read and convert to `double`.

This keeps the mmap fast path on the largest slot (nzval) while keeping index handling straightforward.

### 5. Axis length / index size constraint

R's base `integer` is 32-bit. `dgCMatrix` slots are int32. We require all axes to satisfy `length <= .Machine$integer.max` (≈2.1B) in v1. This covers the practical single-cell range. Larger axes (≥2^31) become supported when Slice 3+ adds long-vector plumbing. Validated at `format_add_axis` on FilesDaf (same rule applied to MemoryDaf for uniformity).

### 6. Drive-by cleanups closed in Phase A (before FilesDaf)

From the Slice 2 kickoff "Still open from Slice 1 (tracked, non-blocking)" list:

- **Close early (FilesDaf will hit these):**
  - `cache_get` / `cache_put` / `cache_remove` vestigial helpers — delete + port `test-cache.R`.
  - `cols_axis` vs `columns_axis` drift — rename to `columns_axis` everywhere (mirrors Julia + generics).
  - `.assert_name` hardening — reject `/`, `\n`, `\0`, `:`, `,`, `\`, trailing/leading whitespace in axis / vector / matrix / scalar names (these break FilesDaf paths and cache keys).
  - `get_vector(default = <axis-length vector>)` length-N pass-through (matches Julia).
  - `.memory_matrix_bucket(create = TRUE)` bucket leak on validation failure.
  - Drop `empty_cache(group=)` (duplicates `clear=`).

- **Defer to Slice 3:**
  - `@family`/top-level package roxygen.
  - `dafr.omp_threshold` wiring into kernels.

---

## File structure

**Created in this slice:**

- `R/files_daf.R` — `FilesDaf` class, constructor `files_daf()`, mode guard helper.
- `R/files_io.R` — dtype table, name validation, JSON read/write helpers, binary read/write helpers, path helpers.
- `R/files_daf_read.R` — `format_has_*`, `format_get_*`, `format_*_set`, `format_axis_*` methods (read side) for `FilesDaf`.
- `R/files_daf_write.R` — `format_add_axis`, `format_delete_*`, `format_set_*`, `format_relayout_matrix` methods (write side) for `FilesDaf`.
- `R/dafr_utils.R` — (optional) hoist `.validate_vector_value` / `.validate_matrix_value` from `R/memory_daf.R` so FilesDaf can reuse.
- `tests/testthat/test-files-daf.R` — constructor, mode handling, `daf.json` write/read.
- `tests/testthat/test-files-scalars.R` — scalar round-trip.
- `tests/testthat/test-files-axes.R` — axis round-trip + delete cascade.
- `tests/testthat/test-files-vectors.R` — dense/sparse/string vector read; dense vector write.
- `tests/testthat/test-files-matrices.R` — dense/sparse matrix read and write; relayout.
- `tests/testthat/test-files-mmap.R` — ALTREP mmap view + readBin fallback + invalidation.
- `tests/testthat/test-files-cache.R` — mapped-tier integration + version-stamp invalidation on write.
- `tests/testthat/test-files-julia-compat.R` — read a pre-generated Julia-written FilesDaf fixture; roundtrip MemoryDaf↔FilesDaf.
- `tests/testthat/fixtures/julia-filesdaf/` — tiny Julia-written fixture directory (committed; regenerator script in `dev/scripts/regen-julia-fixture.jl`).
- `dev/scripts/regen-julia-fixture.jl` — Julia script to regenerate fixtures when the on-disk spec evolves.
- `dev/notes/slice-2-exit.md` — exit gate doc.

**Modified in this slice:**

- `R/format_api.R` — no change (generics already in place).
- `R/memory_daf.R` — rename `cols_axis → columns_axis` in helper signatures + internal doc; fix `.memory_matrix_bucket` leak; rebind to shared `.validate_*_value` if hoisted.
- `R/readers.R` / `R/writers.R` — rename `cols_axis → columns_axis` in user-facing wrapper signatures; fix `get_vector(default=)` length-N pass-through.
- `R/cache.R` — delete `cache_get` / `cache_put` / `cache_remove`; drop `empty_cache(group=)` arg.
- `R/utils.R` — `.assert_name` rejects forbidden characters; add `.assert_mode` for FilesDaf's `mode` enum.
- `R/dafr-package.R` — add `@importFrom jsonlite fromJSON toJSON`.
- `tests/testthat/test-cache.R` — rewrite `cache_get/put/remove` tests against `cache_lookup`/`cache_store`.
- `tests/testthat/test-memory-*.R` — update call sites to use `columns_axis=` where named.
- `DESCRIPTION` — add `jsonlite` to `Imports`.
- `NAMESPACE` — regenerated by roxygen2.
- `man/*.Rd` — regenerated.
- `dev/specs/filesdaf-on-disk-spec-draft.md` — resolve the three `[UNCLEAR]` markers.

---

## Phase A — Drive-by cleanups (before FilesDaf lands)

These are small, independent fixes that let Phase B+ call the user-facing API by name (`columns_axis =`) and use the production cache API consistently. Each task is <10 min.

### Task A1: Rename `cols_axis → columns_axis` in user-facing wrappers

**Files:**
- Modify: `R/readers.R:201,212,231` — `has_matrix`, `matrices_set`, `get_matrix` signatures + bodies.
- Modify: `R/writers.R:96,110,127` — `set_matrix`, `delete_matrix`, `relayout_matrix`.
- Modify: `R/memory_daf.R:306,354` — `.memory_matrix_bucket`, `.validate_matrix_value` helper arg names.
- Modify: `R/cache.R:23,131,150` — `cache_key_matrix`, `bump_matrix_counter`, `matrix_stamp`.
- Modify: `tests/testthat/test-cache.R:134` — comment only.
- Modify: `tests/testthat/test-memory-axes.R:111` — comment only.
- Man pages regenerate.

- [x] **Step 1: Write failing test** in `tests/testthat/test-memory-matrices.R` asserting the named-arg path works:

```r
test_that("user-facing matrix wrappers accept columns_axis = by name", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y"))
  m <- matrix(1:4, nrow = 2, ncol = 2)
  expect_silent(set_matrix(d, rows_axis = "cell", columns_axis = "gene",
                           name = "n", mat = m))
  expect_true(has_matrix(d, rows_axis = "cell", columns_axis = "gene", "n"))
  expect_equal(dim(get_matrix(d, rows_axis = "cell",
                              columns_axis = "gene", "n")), c(2L, 2L))
})
```

- [x] **Step 2: Run filter**

```
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "memory-matrices")'
```

Expected: fail with "unused argument (columns_axis = …)".

- [x] **Step 3: Replace `cols_axis` → `columns_axis`** across the listed files (see `Grep` scan in kickoff breadcrumb §"Still open"). Run the replacement in each file individually — mixing positional and named calls is safe because positional args don't need renaming, but every `.assert_name(cols_axis, "cols_axis")` and every docstring `@param cols_axis` must update.

Apply `grep -rnw cols_axis R/ tests/` → expect no matches after editing. Leave `rows_axis` untouched.

- [x] **Step 4: Regenerate man pages**

```
Rscript -e 'devtools::document()'
```

- [x] **Step 5: Run full test suite**

```
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: 470 pass / 0 fail (same as slice-1 baseline).

- [x] **Step 6: Commit** ✅ b58e96d — 472 pass / 0 fail, both reviews green

```bash
cd ~/src/dafr-native
git add R/ tests/testthat/ man/ NAMESPACE
git commit -m "refactor: rename cols_axis to columns_axis for Julia parity"
```

---

### Task A2: Harden `.assert_name` to reject filesystem-hostile chars

**Files:**
- Modify: `R/utils.R`
- Modify: `tests/testthat/test-memory-axes.R` (and/or new `test-utils.R`)

Allowed name character set: UTF-8 printable, no leading/trailing whitespace, no `/`, no `\`, no `:`, no `,`, no `\n`, no `\r`, no `\0`. These cover POSIX path separator, Windows path separator, our cache-key separator (`:`), our description matrix-key separator (`,`), and line terminators used by axis `.txt` files.

- [x] **Step 1: Write failing tests** in `tests/testthat/test-utils.R` (create new file):

```r
test_that(".assert_name rejects filesystem-hostile characters", {
  for (bad in c("a/b", "a\\b", "a:b", "a,b", "a\nb", "a\rb", "a\0b",
                " leading", "trailing ", "")) {
    expect_error(dafr:::.assert_name(bad, "name"),
                 "must be a non-NA character scalar|contains forbidden|may not be empty")
  }
})

test_that(".assert_name accepts ordinary names", {
  for (ok in c("cell", "UMIs", "donor_1", "gene.count", "x-y", "β")) {
    expect_silent(dafr:::.assert_name(ok, "name"))
  }
})
```

Hostile chars must raise; normal names (including underscore, dot, dash, Unicode) must pass.

- [x] **Step 2: Run**

```
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "utils")'
```

Expected: fail on several hostile inputs.

- [x] **Step 3: Implement**

Replace `R/utils.R` `.assert_name`:

```r
.FORBIDDEN_NAME_CHARS <- "[/\\\\:,\n\r\t\0]"

.assert_name <- function(value, arg) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("`%s` must be a non-NA character scalar", arg), call. = FALSE)
  }
  if (!nzchar(value)) {
    stop(sprintf("`%s` may not be empty", arg), call. = FALSE)
  }
  if (value != trimws(value)) {
    stop(sprintf("`%s` may not have leading/trailing whitespace: %s",
                 arg, sQuote(value)), call. = FALSE)
  }
  if (grepl(.FORBIDDEN_NAME_CHARS, value, perl = TRUE)) {
    stop(sprintf("`%s` contains forbidden character(s): %s", arg, sQuote(value)),
         call. = FALSE)
  }
  invisible()
}
```

- [x] **Step 4: Run full test suite** — existing tests use only legal names, so everything stays green.

- [x] **Step 5: Commit** ✅ e060b62 — 487 pass / 0 fail; `\0` test case dropped (R can't embed NUL in character scalars); regex guard retained.

```bash
cd ~/src/dafr-native
git add R/utils.R tests/testthat/test-utils.R
git commit -m "feat(utils): .assert_name rejects filesystem-hostile characters"
```

---

### Task A3: Delete vestigial `cache_get/put/remove`; port test-cache.R

**Files:**
- Modify: `R/cache.R` (delete lines 29–49).
- Modify: `tests/testthat/test-cache.R` (rewrite lines 27–69 against the stamp-aware API).

- [x] **Step 1: Rewrite failing tests** in `tests/testthat/test-cache.R`:

Replace the `cache_put/get/remove round-trip` test with stamp-aware equivalents:

```r
test_that("cache_store + cache_lookup round-trip through a tier", {
  daf <- TestDaf()
  ce <- S7::prop(daf, "cache")
  cache_store(ce, "memory", "k", 42L, stamp = c(0L), size_bytes = 8)
  expect_equal(cache_lookup(ce, "memory", "k", c(0L)), 42L)
  expect_null(cache_lookup(ce, "mapped", "k", c(0L)))
  expect_null(cache_lookup(ce, "query",  "k", c(0L)))
})

test_that("empty_cache clears all three tiers by default (stamp-aware store)", {
  daf <- TestDaf()
  ce <- S7::prop(daf, "cache")
  cache_store(ce, "mapped", "v:a:x", "mapped-value", stamp = c(0L), size_bytes = 0)
  cache_store(ce, "memory", "v:a:y", "memory-value", stamp = c(0L), size_bytes = 20)
  cache_store(ce, "query",  "q:1",   "query-value",  stamp = c(0L), size_bytes = 15)
  empty_cache(daf)
  expect_null(cache_lookup(ce, "mapped", "v:a:x", c(0L)))
  expect_null(cache_lookup(ce, "memory", "v:a:y", c(0L)))
  expect_null(cache_lookup(ce, "query",  "q:1",   c(0L)))
})

test_that("empty_cache with clear targets a subset", {
  daf <- TestDaf()
  ce <- S7::prop(daf, "cache")
  cache_store(ce, "mapped", "a", 1L, stamp = c(0L), size_bytes = 0)
  cache_store(ce, "memory", "b", 2L, stamp = c(0L), size_bytes = 8)
  cache_store(ce, "query",  "c", 3L, stamp = c(0L), size_bytes = 8)
  empty_cache(daf, clear = c("memory", "query"))
  expect_equal(cache_lookup(ce, "mapped", "a", c(0L)), 1L)
  expect_null( cache_lookup(ce, "memory", "b", c(0L)))
  expect_null( cache_lookup(ce, "query",  "c", c(0L)))
})
```

- [x] **Step 2: Run filter** — expect fail because `cache_put` etc. still exist but the new asserts hit `cache_store`. Actually: the test should pass *before* deletion since both APIs exist. That's fine; keep the deletion in Step 3 atomic with test rewrite.

- [x] **Step 3: Delete `cache_get` / `cache_put` / `cache_remove`** from `R/cache.R:29-49`.

- [x] **Step 4: Run full suite**

Expected: 100% pass; no call sites outside the deleted tests referenced these functions (we verified via grep in the kickoff).

- [x] **Step 5: Commit** ✅ caa8fa8 — 486 pass (−1 vs baseline because `cache_remove` post-remove null check had no equivalent in the stamp-aware API; not a regression).

```bash
cd ~/src/dafr-native
git add R/cache.R tests/testthat/test-cache.R
git commit -m "refactor(cache): drop vestigial cache_get/put/remove; tests use cache_store/lookup"
```

---

### Task A4: Drop `empty_cache(group=)` alias

**Files:**
- Modify: `R/cache.R:64-101` — remove `group` parameter; keep `clear` + `keep`.
- Modify: `tests/testthat/test-cache.R:54-66` — rename `group =` to `clear =`.
- Modify: `man/empty_cache.Rd` — regenerated.

- [x] **Step 1: Update test** `test-cache.R`:

```r
test_that("empty_cache with clear= targets a subset", {
  daf <- TestDaf()
  ce <- S7::prop(daf, "cache")
  cache_store(ce, "mapped", "a", 1L, c(0L), 0)
  cache_store(ce, "memory", "b", 2L, c(0L), 8)
  cache_store(ce, "query",  "c", 3L, c(0L), 8)
  empty_cache(daf, clear = c("memory", "query"))
  expect_equal(cache_lookup(ce, "mapped", "a", c(0L)), 1L)
  expect_null( cache_lookup(ce, "memory", "b", c(0L)))
  expect_null( cache_lookup(ce, "query",  "c", c(0L)))
})

test_that("empty_cache errors on unknown argument `group`", {
  expect_error(empty_cache(memory_daf(), group = "memory"),
               "unused argument")
})
```

- [x] **Step 2: Run** — expect the first test passes (already), the second fails (group is still a formal).

- [x] **Step 3: Implement** — drop `group` from `empty_cache`:

```r
empty_cache <- function(daf, clear = NULL, keep = NULL) {
  all_tiers <- c("mapped", "memory", "query")
  if (!is.null(clear) && !is.null(keep)) {
    stop("specify at most one of `clear`, `keep`", call. = FALSE)
  }
  chosen <- if (!is.null(clear)) .canonical_tier(clear)
            else if (!is.null(keep)) setdiff(all_tiers, .canonical_tier(keep))
            else all_tiers
  .cli_verbose("empty_cache on %s tier(s): %s",
               S7::prop(daf, "name"), paste(chosen, collapse = ", "))
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
```

- [x] **Step 4: Run full suite + regenerate docs**

- [x] **Step 5: Commit** ✅ 8909d5e — 487 pass / 0 fail.

```bash
cd ~/src/dafr-native
git add R/cache.R tests/testthat/test-cache.R man/empty_cache.Rd
git commit -m "refactor(cache): drop empty_cache(group=) alias; use clear= / keep="
```

---

### Task A5: Fix `get_vector(default = <length-N vector>)` pass-through

**Files:**
- Modify: `R/readers.R:177`.
- Modify: `tests/testthat/test-memory-vectors.R` — new test.

Current: `rep(default, length(entries))` — correctly expands a scalar, wrongly repeats a length-N vector into length-N·N for an N-entry axis. Julia `DataAxesFormats`: accepts a length-N default as-is.

- [x] **Step 1: Write failing test**

```r
test_that("get_vector(default = <length-N vector>) passes through", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  default <- c(10.0, 20.0, 30.0)
  out <- get_vector(d, "cell", "absent", default = default)
  expect_equal(unname(out), default)
  expect_equal(names(out), c("A", "B", "C"))
})

test_that("get_vector(default = <scalar>) recycles", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  out <- get_vector(d, "cell", "absent", default = NA)
  expect_equal(unname(out), rep(NA, 3L))
})

test_that("get_vector(default = <wrong-length vector>) errors", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(get_vector(d, "cell", "absent", default = c(1, 2, 3)),
               "default has length 3|expected 2")
})
```

- [x] **Step 2: Run** — expect first test to fail (length-mismatch via `rep`).

- [x] **Step 3: Implement** — replace the absent-branch in `get_vector`:

```r
  if (!format_has_vector(daf, axis, name)) {
    if (missing(default)) {
      stop(sprintf("vector %s does not exist on axis %s",
                   sQuote(name), sQuote(axis)), call. = FALSE)
    }
    n <- length(entries)
    if (length(default) == 1L) {
      out <- rep(default, n)
    } else if (length(default) == n) {
      out <- default
    } else {
      stop(sprintf("default has length %d (expected 1 or %d) for axis %s",
                   length(default), n, sQuote(axis)), call. = FALSE)
    }
    names(out) <- entries
    return(out)
  }
```

- [x] **Step 4: Run full suite** — confirm green.

- [x] **Step 5: Commit** ✅ d288760 — 491 pass / 0 fail. Roxygen `@param default` also updated.

```bash
cd ~/src/dafr-native
git add R/readers.R tests/testthat/test-memory-vectors.R
git commit -m "fix(readers): get_vector(default=) accepts length-N vector (Julia parity)"
```

---

### Task A6: Fix `.memory_matrix_bucket(create = TRUE)` leak

**Files:**
- Modify: `R/memory_daf.R:306-324`.
- Modify: `tests/testthat/test-memory-matrices.R` — regression test.

- [x] **Step 1: Write regression test**

```r
test_that("set_matrix validation failure leaves no phantom matrix bucket", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y"))
  # Wrong shape
  bad <- matrix(1:6, nrow = 3, ncol = 2)
  expect_error(set_matrix(d, "cell", "gene", "n", bad),
               "has dim 3 x 2")
  # matrices_set on (cell, gene) should still return empty
  expect_equal(matrices_set(d, "cell", "gene"), character(0L))
  # Internal env should not have leaked bucket.
  internal <- S7::prop(d, "internal")
  expect_false(exists("cell", envir = internal$matrices, inherits = FALSE))
})
```

- [x] **Step 2: Run** — expect fail on the last assertion (phantom bucket). **Actual:** test passed without a fix — bug was already fixed during Slice 1. `format_set_matrix` already calls `.validate_matrix_value` before `.memory_matrix_bucket(create = TRUE)`.

- [x] **Step 3: Fix** — validate-before-create in `format_set_matrix`: **SKIPPED** — already in place at `R/memory_daf.R:378-379`. Regression test retained as invariant guard.

```r
S7::method(format_set_matrix,
           list(MemoryDaf, S7::class_character, S7::class_character,
                S7::class_character, S7::class_any, S7::class_logical)) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
  mat <- .validate_matrix_value(daf, rows_axis, columns_axis, name, mat)
  # validate passes → safe to create the bucket lazily
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
```

Order-of-operations change: `.validate_matrix_value` is called before `.memory_matrix_bucket(create = TRUE)`. `.validate_matrix_value` itself does not require the bucket to exist — it only needs axis metadata.

- [x] **Step 4: Run full suite** — confirm green.

- [x] **Step 5: Commit** ✅ 4b95a72 — test-only (no production change); commit subject switched to `test(memory_daf): regression guard against phantom bucket on failed set_matrix`. Suite: 494 pass / 0 fail.

---

## Phase B — FilesDaf class + constructor + `daf.json`

### Task B1: Declare `FilesDaf` S7 class

**Files:**
- Create: `R/files_daf.R`
- Modify: `NAMESPACE` (regenerated)
- Test: `tests/testthat/test-files-daf.R`

- [ ] **Step 1: Write failing tests** in `tests/testthat/test-files-daf.R`:

```r
test_that("files_daf() w+ creates a new store with daf.json + skeleton dirs", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  expect_s3_class(d, "dafr::FilesDaf")
  expect_true(inherits(d, "dafr::DafWriter"))
  expect_true(file.exists(file.path(dir, "daf.json")))
  for (sub in c("scalars", "axes", "vectors", "matrices")) {
    expect_true(dir.exists(file.path(dir, sub)))
  }
})

test_that("files_daf() w errors on existing directory with daf.json", {
  dir <- new_tempdir()
  files_daf(dir, mode = "w+")
  expect_error(files_daf(dir, mode = "w"), "already exists")
})

test_that("files_daf() r opens an existing store read-only", {
  dir <- new_tempdir()
  files_daf(dir, mode = "w+")
  d <- files_daf(dir, mode = "r")
  expect_s3_class(d, "dafr::FilesDaf")
  expect_true(inherits(d, "dafr::DafReadOnly"))  # not DafWriter
})

test_that("files_daf() r errors on missing daf.json", {
  dir <- new_tempdir()
  expect_error(files_daf(dir, mode = "r"), "not a daf directory|does not exist")
})

test_that("files_daf() default name is the basename of path", {
  dir <- new_tempdir()
  base <- basename(dir)
  d <- files_daf(dir, mode = "w+")
  expect_equal(S7::prop(d, "name"), base)
})
```

`new_tempdir` is defined in `tests/testthat/helper-tempfiles.R`.

- [ ] **Step 2: Run filter** — expect fail (`files_daf` not defined).

- [ ] **Step 3: Implement** in `R/files_daf.R`:

```r
#' File-backed Daf store.
#'
#' A `Daf` store backed by a directory of small self-describing files,
#' bidirectionally compatible with Julia's `DataAxesFormats.FilesDaf`.
#' The numeric-payload read path uses memory-mapping via ALTREP
#' (`options(dafr.mmap = FALSE)` disables mmap and eager-reads instead).
#' Writes are non-atomic; only one writer may touch a store at a time
#' (see `vignette("files-daf")` when published).
#'
#' @param path Directory path. May not yet exist when `mode` is `"w"` or
#'   `"w+"`.
#' @param mode One of `"r"` (read-only, store must exist), `"r+"`
#'   (read-write, store must exist), `"w"` (create; store must not
#'   exist), `"w+"` (create or open an existing store).
#' @param name Human-readable identifier. Defaults to `basename(path)`.
#' @return A `FilesDaf` instance (`DafWriter` under `"r+"`/`"w"`/`"w+"`,
#'   `DafReadOnly` under `"r"`).
#' @export
files_daf <- function(path, mode = c("r", "r+", "w", "w+"), name = NULL) {
  stopifnot(is.character(path), length(path) == 1L, !is.na(path))
  mode <- match.arg(mode)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  has_daf <- file.exists(file.path(path, "daf.json"))
  if (mode == "r" && !has_daf) {
    stop(sprintf("files_daf(%s, 'r'): not a daf directory (no daf.json)",
                 sQuote(path)), call. = FALSE)
  }
  if (mode == "r+" && !has_daf) {
    stop(sprintf("files_daf(%s, 'r+'): not a daf directory (no daf.json)",
                 sQuote(path)), call. = FALSE)
  }
  if (mode == "w" && has_daf) {
    stop(sprintf("files_daf(%s, 'w'): store already exists; use 'w+' to overwrite",
                 sQuote(path)), call. = FALSE)
  }
  if (mode %in% c("w", "w+")) {
    .files_daf_init(path, truncate = (mode == "w+" && has_daf))
  }
  if (mode == "r") {
    .files_daf_check_version(path)
  } else {
    .files_daf_check_version(path)
  }
  if (is.null(name)) name <- basename(path)
  internal <- new_internal_env()
  internal$path      <- path
  internal$mode      <- mode
  internal$axes      <- new.env(parent = emptyenv())  # in-memory parsed axis cache
  ctor_class <- if (mode == "r") FilesDafReadOnly else FilesDaf
  ctor_class(
    name                   = name,
    internal               = internal,
    cache                  = new_cache_env(),
    axis_version_counter   = new_counter_env(),
    vector_version_counter = new_counter_env(),
    matrix_version_counter = new_counter_env()
  )
}

#' @export
FilesDaf <- S7::new_class(
  name    = "FilesDaf",
  package = "dafr",
  parent  = DafWriter
)

#' @export
FilesDafReadOnly <- S7::new_class(
  name    = "FilesDafReadOnly",
  package = "dafr",
  parent  = DafReadOnly
)

.files_daf_init <- function(path, truncate) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  if (truncate) {
    for (sub in c("scalars", "axes", "vectors", "matrices")) {
      sp <- file.path(path, sub)
      if (dir.exists(sp)) unlink(sp, recursive = TRUE, force = TRUE)
    }
    unlink(file.path(path, "daf.json"), force = TRUE)
  }
  for (sub in c("scalars", "axes", "vectors", "matrices")) {
    dir.create(file.path(path, sub), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(path, "daf.json"))) {
    writeLines('{"version":[1,0]}', con = file.path(path, "daf.json"), sep = "\n")
  }
  invisible()
}

.files_daf_check_version <- function(path) {
  j <- jsonlite::fromJSON(file.path(path, "daf.json"), simplifyVector = TRUE)
  v <- j$version
  if (is.null(v) || length(v) != 2L) {
    stop(sprintf("files_daf: %s daf.json version is malformed", sQuote(path)),
         call. = FALSE)
  }
  if (v[[1L]] != 1L) {
    stop(sprintf("files_daf: %s daf.json major version %d unsupported (expected 1)",
                 sQuote(path), v[[1L]]), call. = FALSE)
  }
  if (v[[2L]] > 0L) {
    stop(sprintf("files_daf: %s daf.json minor version %d exceeds supported (0)",
                 sQuote(path), v[[2L]]), call. = FALSE)
  }
  invisible()
}
```

Add `jsonlite` to DESCRIPTION Imports and `@importFrom jsonlite fromJSON toJSON` to `R/dafr-package.R`.

- [ ] **Step 4: Run filter**

```
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "files-daf")'
```

Expected: all pass.

- [ ] **Step 5: Regenerate docs + namespace**

```
Rscript -e 'devtools::document()'
```

- [ ] **Step 6: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf.R R/dafr-package.R DESCRIPTION NAMESPACE man/files_daf.Rd man/FilesDaf.Rd man/FilesDafReadOnly.Rd tests/testthat/test-files-daf.R
git commit -m "feat(files_daf): S7 class + files_daf() constructor with mode handling"
```

---

### Task B2: Name + mode assertions + forbid read-only writes

**Files:**
- Modify: `R/utils.R` — add `.assert_mode`.
- Modify: `R/files_daf.R` — guard `FilesDafReadOnly` from passing through `format_set_*`.
- Test: `tests/testthat/test-files-daf.R`.

- [ ] **Step 1: Write failing tests**

```r
test_that("files_daf() rejects unknown mode", {
  dir <- new_tempdir()
  files_daf(dir, mode = "w+")
  expect_error(files_daf(dir, mode = "rw"), "'arg' should be")
})

test_that("read-only FilesDaf rejects set_scalar / add_axis", {
  dir <- new_tempdir()
  files_daf(dir, mode = "w+")
  d <- files_daf(dir, mode = "r")
  expect_error(set_scalar(d, "pi", 3.14),
               "read-only|DafReadOnly")
  expect_error(add_axis(d, "cell", "A"),
               "read-only|DafReadOnly")
})
```

- [ ] **Step 2: Run** — expect fail on the read-only path because `format_set_scalar` / `format_add_axis` have no `FilesDafReadOnly` method (actually S7 will raise "method not found" — that's acceptable, make the message explicit).

- [ ] **Step 3: Implement guards** in `R/files_daf.R`:

```r
# Default methods for FilesDafReadOnly raise explicit errors for mutation verbs.
.read_only_guard <- function(verb) {
  stop(sprintf("files_daf: store opened read-only; %s not permitted", verb),
       call. = FALSE)
}

S7::method(format_set_scalar,
           list(FilesDafReadOnly, S7::class_character, S7::class_any, S7::class_logical)) <- function(daf, name, value, overwrite) {
  .read_only_guard("set_scalar")
}
S7::method(format_delete_scalar,
           list(FilesDafReadOnly, S7::class_character, S7::class_logical)) <- function(daf, name, must_exist) {
  .read_only_guard("delete_scalar")
}
S7::method(format_add_axis,
           list(FilesDafReadOnly, S7::class_character, S7::class_character)) <- function(daf, axis, entries) {
  .read_only_guard("add_axis")
}
S7::method(format_delete_axis,
           list(FilesDafReadOnly, S7::class_character, S7::class_logical)) <- function(daf, axis, must_exist) {
  .read_only_guard("delete_axis")
}
S7::method(format_set_vector,
           list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <- function(daf, axis, name, vec, overwrite) {
  .read_only_guard("set_vector")
}
S7::method(format_delete_vector,
           list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_logical)) <- function(daf, axis, name, must_exist) {
  .read_only_guard("delete_vector")
}
S7::method(format_set_matrix,
           list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
  .read_only_guard("set_matrix")
}
S7::method(format_delete_matrix,
           list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)) <- function(daf, rows_axis, columns_axis, name, must_exist) {
  .read_only_guard("delete_matrix")
}
S7::method(format_relayout_matrix,
           list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis, name) {
  .read_only_guard("relayout_matrix")
}
```

(`FilesDaf` inherits from `DafWriter` and gets the concrete methods in Phases D–I; these guards live below for the read-only subclass.)

- [ ] **Step 4: Run filter** → green.

- [ ] **Step 5: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf.R tests/testthat/test-files-daf.R
git commit -m "feat(files_daf): read-only mode guards on mutating format methods"
```

---

## Phase C — I/O helpers (type table, binary, JSON, name→path)

### Task C1: dtype table + path helpers

**Files:**
- Create: `R/files_io.R`
- Test: `tests/testthat/test-files-io.R`

The table maps R-side types to on-disk type strings and back. The path helpers translate `(store, axis, name)` triples into absolute file paths.

- [ ] **Step 1: Write failing tests** in `tests/testthat/test-files-io.R`:

```r
test_that("dtype_for_r_vector picks the on-disk type", {
  expect_equal(dafr:::.dtype_for_r_vector(c(1.5, 2.5)),   "Float64")
  expect_equal(dafr:::.dtype_for_r_vector(1:3),           "Int32")
  expect_equal(dafr:::.dtype_for_r_vector(c(TRUE, FALSE)), "Bool")
  expect_equal(dafr:::.dtype_for_r_vector(bit64::as.integer64(c(1, 2))), "Int64")
  expect_equal(dafr:::.dtype_for_r_vector(c("a", "b")),   "String")
})

test_that(".dtype_size returns on-disk bytes per element", {
  expect_equal(dafr:::.dtype_size("Bool"),    1L)
  expect_equal(dafr:::.dtype_size("Int32"),   4L)
  expect_equal(dafr:::.dtype_size("Int64"),   8L)
  expect_equal(dafr:::.dtype_size("Float64"), 8L)
  expect_error( dafr:::.dtype_size("String"), "no fixed byte size")
})

test_that(".dtype_canonical accepts lowercase aliases", {
  expect_equal(dafr:::.dtype_canonical("int32"),   "Int32")
  expect_equal(dafr:::.dtype_canonical("FLOAT64"), "Float64")  # case-insensitive
  expect_equal(dafr:::.dtype_canonical("Int"),     "Int64")    # per spec clarification
  expect_error(dafr:::.dtype_canonical("Banana"),  "unsupported")
})

test_that(".path_for_* builds store paths", {
  root <- "/tmp/store"
  expect_equal(dafr:::.path_scalar(root, "pi"),
               "/tmp/store/scalars/pi.json")
  expect_equal(dafr:::.path_axis(root, "cell"),
               "/tmp/store/axes/cell.txt")
  expect_equal(dafr:::.path_vector_dir(root, "cell"),
               "/tmp/store/vectors/cell")
  expect_equal(dafr:::.path_matrix_dir(root, "cell", "gene"),
               "/tmp/store/matrices/cell/gene")
})
```

- [ ] **Step 2: Run filter** — expect all fail.

- [ ] **Step 3: Implement** in `R/files_io.R`:

```r
# ---- dtype table ----
.DTYPE_SIZES <- c(
  Bool    = 1L,
  Int8    = 1L,  UInt8  = 1L,
  Int16   = 2L,  UInt16 = 2L,
  Int32   = 4L,  UInt32 = 4L,
  Int64   = 8L,  UInt64 = 8L,
  Float32 = 4L, Float64 = 8L
)

.dtype_canonical <- function(x) {
  stopifnot(is.character(x), length(x) == 1L, !is.na(x))
  # Strip casing
  lower <- tolower(x)
  mapping <- c(
    bool    = "Bool",
    int8    = "Int8",  uint8   = "UInt8",
    int16   = "Int16", uint16  = "UInt16",
    int32   = "Int32", uint32  = "UInt32",
    int64   = "Int64", uint64  = "UInt64",
    float32 = "Float32", float64 = "Float64",
    string  = "String",
    int     = "Int64"      # per spec clarification (no "Int" writer path)
  )
  out <- mapping[lower]
  if (is.na(out)) {
    stop(sprintf("files_daf: unsupported type %s", sQuote(x)), call. = FALSE)
  }
  unname(out)
}

.dtype_size <- function(dtype) {
  dtype <- .dtype_canonical(dtype)
  if (dtype == "String") {
    stop("files_daf: String has no fixed byte size", call. = FALSE)
  }
  unname(.DTYPE_SIZES[[dtype]])
}

.dtype_for_r_vector <- function(v) {
  if (is.logical(v))   return("Bool")
  if (inherits(v, "integer64")) return("Int64")
  if (is.integer(v))   return("Int32")
  if (is.double(v))    return("Float64")
  if (is.character(v)) return("String")
  stop(sprintf("files_daf: cannot map R type %s to on-disk dtype",
               sQuote(typeof(v))), call. = FALSE)
}

# ---- path helpers ----
.path_scalar      <- function(root, name) file.path(root, "scalars", paste0(name, ".json"))
.path_axis        <- function(root, axis) file.path(root, "axes", paste0(axis, ".txt"))
.path_vector_dir  <- function(root, axis) file.path(root, "vectors", axis)
.path_matrix_dir  <- function(root, rows_axis, cols_axis) {
  file.path(root, "matrices", rows_axis, cols_axis)
}
```

- [ ] **Step 4: Run filter** → green.

- [ ] **Step 5: Commit**

```bash
cd ~/src/dafr-native
git add R/files_io.R tests/testthat/test-files-io.R
git commit -m "feat(files_io): dtype table + path helpers"
```

---

### Task C2: JSON descriptor read/write

**Files:**
- Modify: `R/files_io.R`
- Test: `tests/testthat/test-files-io.R`

Descriptor JSON files are small (<200 bytes); we use `jsonlite` directly.

- [ ] **Step 1: Write failing tests**

```r
test_that(".write_descriptor_dense / .read_descriptor round-trip", {
  tmp <- tempfile(fileext = ".json")
  dafr:::.write_descriptor_dense(tmp, dtype = "Float64")
  on.exit(unlink(tmp))
  d <- dafr:::.read_descriptor(tmp)
  expect_equal(d$format, "dense")
  expect_equal(d$eltype, "Float64")
})

test_that(".write_descriptor_sparse / .read_descriptor round-trip", {
  tmp <- tempfile(fileext = ".json")
  dafr:::.write_descriptor_sparse(tmp, dtype = "Float64", indtype = "UInt32")
  on.exit(unlink(tmp))
  d <- dafr:::.read_descriptor(tmp)
  expect_equal(d$format, "sparse")
  expect_equal(d$eltype, "Float64")
  expect_equal(d$indtype, "UInt32")
})

test_that(".read_descriptor rejects malformed JSON", {
  tmp <- tempfile(fileext = ".json")
  writeLines('{"nope":true}', tmp); on.exit(unlink(tmp))
  expect_error(dafr:::.read_descriptor(tmp), "format|eltype")
})

test_that(".read_scalar_json returns typed value", {
  tmp <- tempfile(fileext = ".json")
  writeLines('{"type":"Float64","value":3.14}', tmp); on.exit(unlink(tmp))
  expect_equal(dafr:::.read_scalar_json(tmp), 3.14)
})

test_that(".write_scalar_json writes Julia-compatible format", {
  tmp <- tempfile(fileext = ".json")
  dafr:::.write_scalar_json(tmp, 42L)   # Int32
  on.exit(unlink(tmp))
  raw <- readLines(tmp)
  expect_match(raw, '"type":\\s*"Int32"')
  expect_match(raw, '"value":\\s*42')
})
```

- [ ] **Step 2: Run** → expect fail.

- [ ] **Step 3: Implement** in `R/files_io.R`:

```r
# ---- JSON descriptors ----
.write_descriptor_dense <- function(path, dtype) {
  cat(sprintf('{"format":"dense","eltype":"%s"}\n', dtype), file = path)
}

.write_descriptor_sparse <- function(path, dtype, indtype) {
  cat(sprintf('{"format":"sparse","eltype":"%s","indtype":"%s"}\n',
              dtype, indtype), file = path)
}

.read_descriptor <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  fmt <- j$format
  elt <- j$eltype
  if (is.null(fmt) || !(fmt %in% c("dense", "sparse"))) {
    stop(sprintf("files_daf: %s has malformed descriptor (no format)",
                 sQuote(path)), call. = FALSE)
  }
  if (is.null(elt)) {
    stop(sprintf("files_daf: %s has malformed descriptor (no eltype)",
                 sQuote(path)), call. = FALSE)
  }
  list(format = fmt, eltype = .dtype_canonical(elt),
       indtype = if (is.null(j$indtype)) NULL else .dtype_canonical(j$indtype))
}

# ---- scalar JSON ----
.write_scalar_json <- function(path, value) {
  dtype <- .dtype_for_r_vector(value)
  if (dtype == "String") {
    obj <- list(type = "String", value = jsonlite::unbox(value))
  } else if (dtype == "Bool") {
    obj <- list(type = "Bool", value = jsonlite::unbox(as.integer(value)))
  } else if (dtype == "Int64") {
    # bit64 prints decimal faithfully; emit as JSON number via string pass-through.
    cat(sprintf('{"type":"Int64","value":%s}\n',
                format(value, scientific = FALSE)),
        file = path)
    return(invisible())
  } else {
    obj <- list(type = dtype, value = jsonlite::unbox(value))
  }
  cat(jsonlite::toJSON(obj, auto_unbox = FALSE), "\n", file = path, sep = "")
}

.read_scalar_json <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  t <- .dtype_canonical(j$type)
  v <- j$value
  switch(t,
    Bool    = as.logical(v),
    Int8    = ,
    Int16   = ,
    Int32   = as.integer(v),
    Int64   = bit64::as.integer64(v),
    UInt8   = ,
    UInt16  = ,
    UInt32  = as.integer(v),
    UInt64  = bit64::as.integer64(v),
    Float32 = as.double(v),
    Float64 = as.double(v),
    String  = as.character(v),
    stop(sprintf("files_daf: unsupported scalar type %s", t))
  )
}
```

Note on Int64: `jsonlite::toJSON` mangles `bit64::integer64` values; we format the number by hand.

- [ ] **Step 4: Run filter** → green.

- [ ] **Step 5: Commit**

```bash
cd ~/src/dafr-native
git add R/files_io.R tests/testthat/test-files-io.R
git commit -m "feat(files_io): JSON descriptor + scalar read/write helpers"
```

---

### Task C3: Binary read/write helpers (dense + sparse slots)

**Files:**
- Modify: `R/files_io.R`
- Test: `tests/testthat/test-files-io.R`

- [ ] **Step 1: Write failing tests**

```r
test_that(".write_bin_dense round-trips doubles (little-endian)", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  x <- c(1.5, 2.5, -3.25)
  dafr:::.write_bin_dense(tmp, x, dtype = "Float64")
  expect_equal(file.size(tmp), length(x) * 8L)
  out <- dafr:::.read_bin_dense(tmp, n = length(x), dtype = "Float64")
  expect_equal(out, x)
})

test_that(".write_bin_dense round-trips int32", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  x <- c(1L, -2L, 3L)
  dafr:::.write_bin_dense(tmp, x, dtype = "Int32")
  expect_equal(file.size(tmp), length(x) * 4L)
  out <- dafr:::.read_bin_dense(tmp, n = length(x), dtype = "Int32")
  expect_equal(out, x)
})

test_that(".write_bin_dense for logicals writes one byte per element", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  x <- c(TRUE, FALSE, TRUE)
  dafr:::.write_bin_dense(tmp, x, dtype = "Bool")
  expect_equal(file.size(tmp), length(x))
  out <- dafr:::.read_bin_dense(tmp, n = length(x), dtype = "Bool")
  expect_equal(out, x)
})

test_that(".indtype_for_size picks UInt32 vs UInt64", {
  expect_equal(dafr:::.indtype_for_size(2^30L), "UInt32")
  expect_equal(dafr:::.indtype_for_size(2^32),  "UInt64")
})
```

- [ ] **Step 2: Implement**

```r
# ---- binary I/O ----
.write_bin_dense <- function(path, value, dtype) {
  dtype <- .dtype_canonical(dtype)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  switch(dtype,
    Bool    = writeBin(as.raw(as.integer(value)), con),
    Int8    = writeBin(as.integer(value), con, size = 1L, endian = "little"),
    Int16   = writeBin(as.integer(value), con, size = 2L, endian = "little"),
    Int32   = writeBin(as.integer(value), con, size = 4L, endian = "little"),
    Int64   = writeBin(unclass(bit64::as.integer64(value)), con, size = 8L, endian = "little"),
    UInt8   = writeBin(as.integer(value), con, size = 1L, endian = "little"),
    UInt16  = writeBin(as.integer(value), con, size = 2L, endian = "little"),
    UInt32  = writeBin(as.integer(value), con, size = 4L, endian = "little"),
    UInt64  = writeBin(unclass(bit64::as.integer64(value)), con, size = 8L, endian = "little"),
    Float32 = writeBin(as.double(value),  con, size = 4L, endian = "little"),
    Float64 = writeBin(as.double(value),  con, size = 8L, endian = "little"),
    stop(sprintf("files_daf: unsupported dtype %s for dense write", dtype))
  )
  invisible()
}

.read_bin_dense <- function(path, n, dtype) {
  dtype <- .dtype_canonical(dtype)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  switch(dtype,
    Bool    = as.logical(readBin(con, what = "integer", n = n, size = 1L,
                                 signed = FALSE, endian = "little")),
    Int8    = readBin(con, what = "integer", n = n, size = 1L, signed = TRUE,
                      endian = "little"),
    Int16   = readBin(con, what = "integer", n = n, size = 2L, signed = TRUE,
                      endian = "little"),
    Int32   = readBin(con, what = "integer", n = n, size = 4L, signed = TRUE,
                      endian = "little"),
    Int64   = {
      raw64 <- readBin(con, what = "integer", n = n, size = 8L, endian = "little")
      bit64::as.integer64(raw64)
    },
    UInt8   = readBin(con, what = "integer", n = n, size = 1L, signed = FALSE,
                      endian = "little"),
    UInt16  = readBin(con, what = "integer", n = n, size = 2L, signed = FALSE,
                      endian = "little"),
    UInt32  = {
      # R has no unsigned; readBin size=4 sign=FALSE errors. Read as int32
      # and document — values ≥ 2^31 would overflow here; Slice 3 long-vec.
      readBin(con, what = "integer", n = n, size = 4L, signed = TRUE,
              endian = "little")
    },
    UInt64  = {
      raw64 <- readBin(con, what = "integer", n = n, size = 8L, endian = "little")
      bit64::as.integer64(raw64)
    },
    Float32 = readBin(con, what = "double",  n = n, size = 4L, endian = "little"),
    Float64 = readBin(con, what = "double",  n = n, size = 8L, endian = "little"),
    stop(sprintf("files_daf: unsupported dtype %s for dense read", dtype))
  )
}

.indtype_for_size <- function(size) {
  if (size <= 2147483647) "UInt32" else "UInt64"   # == typemax(Int32)
  # Matches Julia: typemax(UInt32) = 2^32-1, but R int32 caps at 2^31-1.
  # We conservatively pick UInt32 only when the axis fits R's native int.
}
```

- [ ] **Step 3: Run filter** → green.

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add R/files_io.R tests/testthat/test-files-io.R
git commit -m "feat(files_io): dense binary read/write + indtype selector"
```

---

## Phase D — Scalars on FilesDaf

### Task D1: Scalar format methods

**Files:**
- Create: `R/files_daf_read.R`, `R/files_daf_write.R`
- Test: `tests/testthat/test-files-scalars.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("FilesDaf scalar round-trip", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  set_scalar(d, "pi", 3.14)
  set_scalar(d, "cells", 100L)
  set_scalar(d, "is_ok", TRUE)
  set_scalar(d, "label", "batch_A")

  # persist + reopen
  d2 <- files_daf(dir, mode = "r")
  expect_true(has_scalar(d2, "pi"))
  expect_equal(get_scalar(d2, "pi"),    3.14)
  expect_equal(get_scalar(d2, "cells"), 100L)
  expect_equal(get_scalar(d2, "is_ok"), TRUE)
  expect_equal(get_scalar(d2, "label"), "batch_A")
  expect_equal(scalars_set(d2), sort(c("pi", "cells", "is_ok", "label"), method="radix"))
})

test_that("FilesDaf set_scalar overwrite behaviour matches MemoryDaf", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  set_scalar(d, "x", 1)
  expect_error(set_scalar(d, "x", 2), "already exists")
  set_scalar(d, "x", 2, overwrite = TRUE)
  expect_equal(get_scalar(d, "x"), 2)
})

test_that("FilesDaf delete_scalar removes the file", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  set_scalar(d, "x", 1)
  expect_true(file.exists(file.path(dir, "scalars", "x.json")))
  delete_scalar(d, "x")
  expect_false(file.exists(file.path(dir, "scalars", "x.json")))
  expect_error(get_scalar(d, "x"), "does not exist")
  expect_silent(delete_scalar(d, "x", must_exist = FALSE))
  expect_error(delete_scalar(d, "x"), "does not exist")
})
```

- [ ] **Step 2: Run filter** — expect fail.

- [ ] **Step 3: Implement** in `R/files_daf_read.R`:

```r
.files_root <- function(daf) S7::prop(daf, "internal")$path

# ---- scalars: query ----

S7::method(format_has_scalar,
           list(FilesDaf, S7::class_character)) <- function(daf, name) {
  file.exists(.path_scalar(.files_root(daf), name))
}
S7::method(format_has_scalar,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, name) {
  file.exists(.path_scalar(.files_root(daf), name))
}

.files_get_scalar <- function(daf, name) {
  p <- .path_scalar(.files_root(daf), name)
  if (!file.exists(p)) {
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
  }
  .read_scalar_json(p)
}
S7::method(format_get_scalar,
           list(FilesDaf, S7::class_character)) <- function(daf, name) .files_get_scalar(daf, name)
S7::method(format_get_scalar,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, name) .files_get_scalar(daf, name)

.files_scalars_set <- function(daf) {
  dir <- file.path(.files_root(daf), "scalars")
  files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
  sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(format_scalars_set, FilesDaf)        <- function(daf) .files_scalars_set(daf)
S7::method(format_scalars_set, FilesDafReadOnly) <- function(daf) .files_scalars_set(daf)
```

And in `R/files_daf_write.R`:

```r
S7::method(format_set_scalar,
           list(FilesDaf, S7::class_character, S7::class_any, S7::class_logical)) <- function(daf, name, value, overwrite) {
  .assert_scalar_value(name, value)
  p <- .path_scalar(.files_root(daf), name)
  if (file.exists(p) && !overwrite) {
    stop(sprintf("scalar %s already exists; use overwrite = TRUE",
                 sQuote(name)), call. = FALSE)
  }
  .write_scalar_json(p, value)
  invisible()
}

S7::method(format_delete_scalar,
           list(FilesDaf, S7::class_character, S7::class_logical)) <- function(daf, name, must_exist) {
  p <- .path_scalar(.files_root(daf), name)
  if (!file.exists(p)) {
    if (must_exist) {
      stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
    }
    return(invisible())
  }
  unlink(p, force = TRUE)
  invisible()
}
```

`.assert_scalar_value` currently lives in `R/memory_daf.R:72`. Move/copy it to `R/utils.R` (cross-cutting helper) in a brief refactor within this task — or reference it via `dafr:::.assert_scalar_value` for now. Cleaner option: **move to `R/utils.R`**, update MemoryDaf reference.

- [ ] **Step 4: Run filter** → green.

- [ ] **Step 5: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R R/files_daf_write.R R/utils.R R/memory_daf.R tests/testthat/test-files-scalars.R
git commit -m "feat(files_daf): scalar format_* methods"
```

---

## Phase E — Axes on FilesDaf

### Task E1: Axis read methods

**Files:**
- Modify: `R/files_daf_read.R`
- Test: `tests/testthat/test-files-axes.R`

Axis entries are cached in `internal$axes` as a `list(entries = character, dict = env)` — same shape as MemoryDaf. On first read, parse the text file; cache. On mutation, invalidate via `format_add_axis` / `format_delete_axis`.

- [ ] **Step 1: Write failing tests**

```r
test_that("FilesDaf read axis entries", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("BRCA1", "TP53", "MYC"), file.path(dir, "axes", "gene.txt"))
  d <- files_daf(dir, mode = "r")
  expect_true(has_axis(d, "gene"))
  expect_equal(axis_length(d, "gene"), 3L)
  expect_equal(axis_vector(d, "gene"), c("BRCA1", "TP53", "MYC"))
  dict <- axis_dict(d, "gene")
  expect_equal(dict[["TP53"]], 2L)
  expect_equal(axes_set(d), "gene")
})

test_that("axis parsing rejects empty lines", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "", "B"), file.path(dir, "axes", "bad.txt"))
  d <- files_daf(dir, mode = "r")
  expect_error(axis_vector(d, "bad"), "empty")
})
```

- [ ] **Step 2: Implement**

```r
.files_axis_parsed <- function(daf, axis) {
  cache <- S7::prop(daf, "internal")$axes
  if (exists(axis, envir = cache, inherits = FALSE)) {
    return(get(axis, envir = cache, inherits = FALSE))
  }
  p <- .path_axis(.files_root(daf), axis)
  if (!file.exists(p)) return(NULL)
  entries <- readLines(p, encoding = "UTF-8", warn = FALSE)
  # readLines swallows the trailing newline and does not emit an empty last element.
  if (anyNA(entries) || any(!nzchar(entries))) {
    stop(sprintf("files_daf: axis %s contains empty entries", sQuote(axis)),
         call. = FALSE)
  }
  if (anyDuplicated(entries)) {
    dup <- entries[duplicated(entries)][1L]
    stop(sprintf("files_daf: axis %s has duplicate entry %s",
                 sQuote(axis), sQuote(dup)), call. = FALSE)
  }
  dict <- new.env(parent = emptyenv(), size = length(entries))
  for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
  parsed <- list(entries = entries, dict = dict)
  assign(axis, parsed, envir = cache)
  parsed
}

.files_has_axis <- function(daf, axis) {
  file.exists(.path_axis(.files_root(daf), axis))
}

S7::method(format_has_axis,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) .files_has_axis(daf, axis)
S7::method(format_has_axis,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) .files_has_axis(daf, axis)

.files_axes_set <- function(daf) {
  dir <- file.path(.files_root(daf), "axes")
  files <- list.files(dir, pattern = "\\.txt$", full.names = FALSE)
  sort(sub("\\.txt$", "", files), method = "radix")
}
S7::method(format_axes_set, FilesDaf)          <- function(daf) .files_axes_set(daf)
S7::method(format_axes_set, FilesDafReadOnly)  <- function(daf) .files_axes_set(daf)

.files_axis_getter <- function(slot) function(daf, axis) {
  parsed <- .files_axis_parsed(daf, axis)
  if (is.null(parsed)) stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  parsed[[slot]]
}
S7::method(format_axis_length, list(FilesDaf, S7::class_character))         <- function(daf, axis) length(.files_axis_parsed(daf, axis)$entries)
S7::method(format_axis_length, list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) length(.files_axis_parsed(daf, axis)$entries)
S7::method(format_axis_array,  list(FilesDaf, S7::class_character))         <- .files_axis_getter("entries")
S7::method(format_axis_array,  list(FilesDafReadOnly, S7::class_character)) <- .files_axis_getter("entries")
S7::method(format_axis_dict,   list(FilesDaf, S7::class_character))         <- .files_axis_getter("dict")
S7::method(format_axis_dict,   list(FilesDafReadOnly, S7::class_character)) <- .files_axis_getter("dict")
```

- [ ] **Step 3: Run filter** → green.

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R tests/testthat/test-files-axes.R
git commit -m "feat(files_daf): axis read methods with in-memory entry cache"
```

---

### Task E2: Axis write + delete-cascade methods

**Files:**
- Modify: `R/files_daf_write.R`
- Test: `tests/testthat/test-files-axes.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("add_axis writes a UTF-8 \\n-terminated file", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "gene", c("BRCA1", "TP53", "MYC"))
  p <- file.path(dir, "axes", "gene.txt")
  expect_true(file.exists(p))
  raw <- readBin(p, what = "raw", n = file.size(p))
  expect_equal(rawToChar(raw), "BRCA1\nTP53\nMYC\n")
})

test_that("add_axis rejects existing", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", "A")
  expect_error(add_axis(d, "cell", "B"), "already exists")
})

test_that("delete_axis cascades to dependent vectors + matrices", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y"))
  set_vector(d, "cell", "donor", c(1, 2))
  set_matrix(d, "cell", "gene", "m", matrix(1:4, 2, 2))
  delete_axis(d, "cell")
  expect_false(has_axis(d, "cell"))
  expect_false(dir.exists(file.path(dir, "vectors", "cell")))
  expect_false(dir.exists(file.path(dir, "matrices", "cell")))
  # matrices/gene/cell/ also gone:
  expect_false(dir.exists(file.path(dir, "matrices", "gene", "cell")))
})
```

- [ ] **Step 2: Implement**

```r
.write_axis_file <- function(path, entries) {
  # One entry per line, terminal newline, UTF-8.
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(entries, con, useBytes = FALSE)
}

S7::method(format_add_axis,
           list(FilesDaf, S7::class_character, S7::class_character)) <- function(daf, axis, entries) {
  if (!is.character(entries)) {
    stop(sprintf("axis %s entries must be a character vector", sQuote(axis)), call. = FALSE)
  }
  if (anyNA(entries)) {
    stop(sprintf("axis %s entries contain NA", sQuote(axis)), call. = FALSE)
  }
  if (any(!nzchar(entries))) {
    stop(sprintf("axis %s entries contain empty strings", sQuote(axis)), call. = FALSE)
  }
  if (any(grepl("\n|\r", entries, fixed = FALSE))) {
    stop(sprintf("axis %s entries contain newline characters", sQuote(axis)), call. = FALSE)
  }
  if (anyDuplicated(entries)) {
    dup <- entries[duplicated(entries)][1L]
    stop(sprintf("axis %s has duplicate entry %s",
                 sQuote(axis), sQuote(dup)), call. = FALSE)
  }
  if (length(entries) > .Machine$integer.max) {
    stop(sprintf("axis %s length exceeds R integer capacity", sQuote(axis)), call. = FALSE)
  }
  root <- .files_root(daf)
  p <- .path_axis(root, axis)
  if (file.exists(p)) {
    stop(sprintf("axis %s already exists", sQuote(axis)), call. = FALSE)
  }
  .write_axis_file(p, entries)
  # populate the in-memory parse cache
  dict <- new.env(parent = emptyenv(), size = length(entries))
  for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
  assign(axis, list(entries = entries, dict = dict),
         envir = S7::prop(daf, "internal")$axes)
  bump_axis_counter(daf, axis)
  invisible()
}

S7::method(format_delete_axis,
           list(FilesDaf, S7::class_character, S7::class_logical)) <- function(daf, axis, must_exist) {
  root <- .files_root(daf)
  p <- .path_axis(root, axis)
  if (!file.exists(p)) {
    if (must_exist) stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
    return(invisible())
  }
  unlink(p, force = TRUE)
  # Cascade: drop vectors/<axis> and any matrices involving <axis>.
  vdir <- .path_vector_dir(root, axis)
  if (dir.exists(vdir)) unlink(vdir, recursive = TRUE, force = TRUE)
  mroot <- file.path(root, "matrices")
  mrow  <- file.path(mroot, axis)
  if (dir.exists(mrow)) unlink(mrow, recursive = TRUE, force = TRUE)
  # Also remove axis as columns_axis under other rows.
  for (rows in list.files(mroot, full.names = FALSE)) {
    mcol <- file.path(mroot, rows, axis)
    if (dir.exists(mcol)) unlink(mcol, recursive = TRUE, force = TRUE)
  }
  # Purge in-memory parse cache.
  cache <- S7::prop(daf, "internal")$axes
  if (exists(axis, envir = cache, inherits = FALSE)) {
    rm(list = axis, envir = cache)
  }
  bump_axis_counter(daf, axis)
  invisible()
}
```

- [ ] **Step 3: Run filter** → green (the set_vector / set_matrix references need Phase F / I but expect error gracefully until then; reorder task execution so F + I are defined before this test runs, or gate the cascade test with `skip_if(...)` until those phases complete).

**Important:** Gate the `delete_axis cascades` test with `skip_if_not(exists("format_set_vector.FilesDaf"))` or reorder: do the cascade test after Phase F.1+I.2. Simplest: remove the cascade test here, put it in Phase I.3 (when set_matrix/set_vector work on FilesDaf).

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_write.R tests/testthat/test-files-axes.R
git commit -m "feat(files_daf): axis add/delete with cascade to vectors + matrices"
```

---

## Phase F — Dense vector read/write (the mmap path)

### Task F1: `format_has_vector` / `format_vectors_set`

**Files:**
- Modify: `R/files_daf_read.R`
- Test: `tests/testthat/test-files-vectors.R`

- [ ] **Step 1: Tests**

```r
test_that("FilesDaf vectors_set lists descriptor-backed vectors", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "vectors", "cell", "donor.json"))
  writeBin(c(1.0, 2.0),
           file.path(dir, "vectors", "cell", "donor.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  expect_equal(vectors_set(d, "cell"), "donor")
  expect_true(has_vector(d, "cell", "donor"))
})
```

- [ ] **Step 2: Implement**

```r
.files_vector_desc_path <- function(root, axis, name) {
  file.path(.path_vector_dir(root, axis), paste0(name, ".json"))
}

.files_has_vector <- function(daf, axis, name) {
  if (!format_has_axis(daf, axis)) return(FALSE)
  file.exists(.files_vector_desc_path(.files_root(daf), axis, name))
}
S7::method(format_has_vector,
           list(FilesDaf, S7::class_character, S7::class_character)) <- function(daf, axis, name) .files_has_vector(daf, axis, name)
S7::method(format_has_vector,
           list(FilesDafReadOnly, S7::class_character, S7::class_character)) <- function(daf, axis, name) .files_has_vector(daf, axis, name)

.files_vectors_set <- function(daf, axis) {
  if (!format_has_axis(daf, axis)) return(character(0L))
  dir <- .path_vector_dir(.files_root(daf), axis)
  if (!dir.exists(dir)) return(character(0L))
  files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
  sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(format_vectors_set,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) .files_vectors_set(daf, axis)
S7::method(format_vectors_set,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) .files_vectors_set(daf, axis)
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R tests/testthat/test-files-vectors.R
git commit -m "feat(files_daf): format_has_vector + format_vectors_set"
```

---

### Task F2: `format_get_vector` dense numeric (mmap path)

**Files:**
- Modify: `R/files_daf_read.R`

- [ ] **Step 1: Tests**

```r
test_that("format_get_vector returns an ALTREP-backed vector for Float64 dense", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "vectors", "cell", "x.json"))
  writeBin(c(1.5, 2.5, -3.25),
           file.path(dir, "vectors", "cell", "x.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  v <- format_get_vector(d, "cell", "x")
  expect_equal(v, c(1.5, 2.5, -3.25))
  expect_true(is_altrep(v))  # from tests/testthat/helper-altrep.R
})

test_that("format_get_vector eager-reads Float64 when dafr.mmap = FALSE", {
  skip_if_not(requireNamespace("withr", quietly = TRUE))
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "vectors", "cell", "x.json"))
  writeBin(c(10.0, 20.0), file.path(dir, "vectors", "cell", "x.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  v <- withr::with_options(list(dafr.mmap = FALSE),
                           format_get_vector(d, "cell", "x"))
  expect_equal(v, c(10.0, 20.0))
  expect_false(is_altrep(v))
})

test_that("format_get_vector densifies Int32 to R integer", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Int32"}',
             file.path(dir, "vectors", "cell", "i.json"))
  writeBin(c(1L, -2L, 3L), file.path(dir, "vectors", "cell", "i.data"),
           size = 4L, endian = "little")
  d <- files_daf(dir, mode = "r")
  v <- format_get_vector(d, "cell", "i")
  expect_equal(v, c(1L, -2L, 3L))
  expect_true(is_altrep(v) || is.integer(v))  # int32 mmap is also ALTREP-backed
})
```

- [ ] **Step 2: Implement**

```r
.files_get_vector_dense <- function(daf, axis, name, desc, n) {
  root <- .files_root(daf)
  dir  <- .path_vector_dir(root, axis)
  elt  <- desc$eltype
  if (elt == "String") {
    return(.files_get_vector_dense_string(daf, axis, name, n))
  }
  data_path <- file.path(dir, paste0(name, ".data"))
  if (!file.exists(data_path)) {
    stop(sprintf("files_daf: missing payload %s", sQuote(data_path)), call. = FALSE)
  }
  expected <- n * .dtype_size(elt)
  actual   <- file.size(data_path)
  if (actual < expected) {
    stop(sprintf("files_daf: vector %s payload truncated (%d < %d bytes)",
                 sQuote(name), actual, expected), call. = FALSE)
  }
  use_mmap <- isTRUE(dafr_opt("dafr.mmap"))
  if (use_mmap && elt == "Float64") {
    return(mmap_real(data_path, n))
  }
  if (use_mmap && elt == "Int32") {
    return(mmap_int(data_path, n))
  }
  if (use_mmap && elt == "Bool") {
    # .data on disk is one byte per element; ALTREP expects int32. Fall back.
  }
  # Fallback: eager read.
  .read_bin_dense(data_path, n, elt)
}

.files_get_vector_dense_string <- function(daf, axis, name, n) {
  dir  <- .path_vector_dir(.files_root(daf), axis)
  txt  <- file.path(dir, paste0(name, ".txt"))
  if (!file.exists(txt)) {
    stop(sprintf("files_daf: missing payload %s", sQuote(txt)), call. = FALSE)
  }
  out <- readLines(txt, encoding = "UTF-8", warn = FALSE)
  if (length(out) != n) {
    stop(sprintf("files_daf: string vector %s has %d entries (expected %d)",
                 sQuote(name), length(out), n), call. = FALSE)
  }
  out
}

S7::method(format_get_vector,
           list(FilesDaf, S7::class_character, S7::class_character)) <- function(daf, axis, name) {
  .files_get_vector_impl(daf, axis, name)
}
S7::method(format_get_vector,
           list(FilesDafReadOnly, S7::class_character, S7::class_character)) <- function(daf, axis, name) {
  .files_get_vector_impl(daf, axis, name)
}

.files_get_vector_impl <- function(daf, axis, name) {
  root <- .files_root(daf)
  desc_path <- .files_vector_desc_path(root, axis, name)
  if (!file.exists(desc_path)) {
    stop(sprintf("vector %s does not exist on axis %s",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  desc <- .read_descriptor(desc_path)
  n <- format_axis_length(daf, axis)
  if (desc$format == "dense") {
    return(.files_get_vector_dense(daf, axis, name, desc, n))
  }
  if (desc$format == "sparse") {
    return(.files_get_vector_sparse(daf, axis, name, desc, n))
  }
  stop(sprintf("files_daf: unsupported vector format %s", desc$format), call. = FALSE)
}
```

Sparse path stub for Phase G.

- [ ] **Step 3: Run** → green (skipping sparse).

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R tests/testthat/test-files-vectors.R
git commit -m "feat(files_daf): format_get_vector dense via mmap with readBin fallback"
```

---

### Task F3: `format_set_vector` dense (numeric + string + Bool), stamping sparsify heuristic helpers

**Files:**
- Modify: `R/files_daf_write.R`, `R/files_io.R`
- Test: `tests/testthat/test-files-vectors.R`

Sparsification is implemented in Task F5 (sparse writer) and wired into this method once the sparse helpers land. This task ships the dense-always path plus the sparsity-decision *helpers* — the `set_vector` entry point ends Task F5 calling them.

- [ ] **Step 1: Tests**

```r
test_that("set_vector + get_vector dense Float64 round-trip", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A", "B", "C"))
  set_vector(d, "cell", "x", c(1.5, 2.5, -3.25))
  d2 <- files_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "x")), c(1.5, 2.5, -3.25))
  # descriptor JSON correctness: small dense vectors stay dense
  j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "x.json"))
  expect_equal(j$format, "dense")
  expect_equal(j$eltype, "Float64")
  expect_equal(file.size(file.path(dir, "vectors", "cell", "x.data")), 24L)
})

test_that("set_vector dense Int32 and Bool round-trip", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A", "B"))
  set_vector(d, "cell", "i", c(7L, 42L))
  set_vector(d, "cell", "b", c(TRUE, FALSE))
  d2 <- files_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "i")), c(7L, 42L))
  expect_equal(unname(get_vector(d2, "cell", "b")), c(TRUE, FALSE))
})

test_that("set_vector dense String round-trip", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A", "B"))
  set_vector(d, "cell", "s", c("foo", "bar"))
  txt <- readLines(file.path(dir, "vectors", "cell", "s.txt"))
  expect_equal(txt, c("foo", "bar"))
  j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "s.json"))
  expect_equal(j$format, "dense")
  expect_equal(j$eltype, "String")
})

test_that("set_vector rejects wrong length / requires overwrite", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A", "B"))
  expect_error(set_vector(d, "cell", "x", c(1, 2, 3)), "length")
  set_vector(d, "cell", "x", c(1, 2))
  expect_error(set_vector(d, "cell", "x", c(3, 4)), "already exists")
  set_vector(d, "cell", "x", c(3, 4), overwrite = TRUE)
})

# ---- heuristic helper tests ----
test_that(".should_sparsify_numeric picks sparse when nnz is small enough", {
  # Float64 + UInt32: nnz * (8 + 4) <= 0.75 * n * 8  <=>  nnz <= 0.5 n
  expect_true (dafr:::.should_sparsify_numeric(c(0,0,0,0,5),   "Float64", "UInt32"))
  expect_false(dafr:::.should_sparsify_numeric(c(1,2,3,4,5),   "Float64", "UInt32"))
  expect_false(dafr:::.should_sparsify_numeric(c(0,1,2,3,4,5), "Float64", "UInt32"))  # 83% full
})

test_that(".should_sparsify_bool never selects sparse for all-FALSE/all-TRUE when threshold forces dense", {
  # Bool + UInt32: sparse_bytes = nnz * (1 + 4) = 5 nnz; dense_bytes = n.
  # choose sparse iff 5 nnz <= 0.75 n   <=>  nnz <= 0.15 n.
  expect_true (dafr:::.should_sparsify_numeric(c(rep(FALSE, 90), rep(TRUE, 10)),
                                               "Bool", "UInt32"))
  expect_false(dafr:::.should_sparsify_numeric(c(rep(FALSE, 80), rep(TRUE, 20)),
                                               "Bool", "UInt32"))
})

test_that(".should_sparsify_string applies Julia §8.4 formula", {
  indtype <- "UInt32"
  # Five entries, three non-empty ("hi"), two empty
  v <- c("hi", "", "hi", "", "hi")
  # n_nonempty = 3, nonempty_bytes = 6, sizeof(I) = 4
  # sparse_size = 6 + 3*(1 + 4) = 21
  # dense_size  = 6 + 5         = 11
  # 21 > 0.75 * 11 → dense
  expect_false(dafr:::.should_sparsify_string(v, indtype))
  # Many empties shift the balance:
  v2 <- c("hi", rep("", 20))
  # n_nonempty = 1, nonempty_bytes = 2, sparse_size = 2 + 1*5 = 7; dense_size = 2 + 21 = 23
  # 7 <= 0.75 * 23 = 17.25 → sparse
  expect_true(dafr:::.should_sparsify_string(v2, indtype))
})
```

- [ ] **Step 2: Implement helpers** in `R/files_io.R`

```r
.should_sparsify_numeric <- function(vec, eltype, indtype) {
  n <- length(vec)
  if (n == 0L) return(FALSE)
  nnz <- if (is.logical(vec)) sum(vec, na.rm = TRUE) else sum(vec != 0)
  sparse_bytes <- nnz * (.dtype_size(eltype) + .dtype_size(indtype))
  dense_bytes  <- n   * .dtype_size(eltype)
  sparse_bytes <= 0.75 * dense_bytes
}

.should_sparsify_string <- function(vec, indtype) {
  n <- length(vec)
  if (n == 0L) return(FALSE)
  nonempty <- nzchar(vec)
  n_nonempty     <- sum(nonempty)
  nonempty_bytes <- sum(nchar(vec[nonempty], type = "bytes"))
  sparse_size <- nonempty_bytes + n_nonempty * (1L + .dtype_size(indtype))
  dense_size  <- nonempty_bytes + n
  sparse_size <= 0.75 * dense_size
}
```

- [ ] **Step 3: Implement dense writer** in `R/files_daf_write.R`

```r
.files_write_vector_dense <- function(vdir, name, vec) {
  dtype <- .dtype_for_r_vector(vec)
  if (dtype == "String") {
    con <- file(file.path(vdir, paste0(name, ".txt")), open = "wb",
                encoding = "UTF-8")
    writeLines(vec, con, useBytes = FALSE)
    close(con)
  } else {
    .write_bin_dense(file.path(vdir, paste0(name, ".data")), vec, dtype)
  }
  .write_descriptor_dense(file.path(vdir, paste0(name, ".json")), dtype)
  invisible()
}

.files_vector_unlink_payload <- function(vdir, name) {
  for (ext in c(".data", ".txt", ".nzind", ".nzval", ".nztxt")) {
    p <- file.path(vdir, paste0(name, ext))
    if (file.exists(p)) unlink(p, force = TRUE)
  }
}

# Dense-only method for this task; Task F5 replaces with sparsity-aware dispatch.
S7::method(format_set_vector,
           list(FilesDaf, S7::class_character, S7::class_character,
                S7::class_any, S7::class_logical)) <- function(daf, axis, name, vec, overwrite) {
  vec <- .validate_vector_value(daf, axis, name, vec)
  root <- .files_root(daf)
  vdir <- .path_vector_dir(root, axis)
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  desc_path <- file.path(vdir, paste0(name, ".json"))
  if (file.exists(desc_path) && !overwrite) {
    stop(sprintf("vector %s already exists on axis %s; use overwrite = TRUE",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  .files_vector_unlink_payload(vdir, name)
  .files_write_vector_dense(vdir, name, vec)
  bump_vector_counter(daf, axis, name)
  invisible()
}
```

`.validate_vector_value` hoists to `R/utils.R` during this task. Same move as for scalar — update `R/memory_daf.R` to reference the moved helper.

- [ ] **Step 4: Run** → green.

- [ ] **Step 5: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_write.R R/files_io.R R/memory_daf.R R/utils.R tests/testthat/test-files-vectors.R tests/testthat/test-files-io.R
git commit -m "feat(files_daf): format_set_vector dense writer + sparsify heuristic helpers"
```

---

### Task F4: `format_delete_vector`

**Files:**
- Modify: `R/files_daf_write.R`
- Test: `tests/testthat/test-files-vectors.R`

- [ ] **Step 1: Tests**

```r
test_that("delete_vector removes payload + descriptor", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A", "B"))
  set_vector(d, "cell", "x", c(1, 2))
  delete_vector(d, "cell", "x")
  expect_false(file.exists(file.path(dir, "vectors", "cell", "x.json")))
  expect_false(file.exists(file.path(dir, "vectors", "cell", "x.data")))
})

test_that("delete_vector must_exist=FALSE is a no-op on missing", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", "A")
  expect_silent(delete_vector(d, "cell", "nope", must_exist = FALSE))
  expect_error(delete_vector(d, "cell", "nope"), "does not exist")
})
```

- [ ] **Step 2: Implement**

```r
S7::method(format_delete_vector,
           list(FilesDaf, S7::class_character, S7::class_character, S7::class_logical)) <- function(daf, axis, name, must_exist) {
  vdir <- .path_vector_dir(.files_root(daf), axis)
  desc_path <- file.path(vdir, paste0(name, ".json"))
  if (!file.exists(desc_path)) {
    if (must_exist) {
      stop(sprintf("vector %s does not exist on axis %s",
                   sQuote(name), sQuote(axis)), call. = FALSE)
    }
    return(invisible())
  }
  unlink(desc_path, force = TRUE)
  .files_vector_unlink_payload(vdir, name)
  bump_vector_counter(daf, axis, name)
  invisible()
}
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_write.R tests/testthat/test-files-vectors.R
git commit -m "feat(files_daf): format_delete_vector"
```

---

### Task F5: `format_set_vector` sparse (adaptive heuristic + `sparseVector` input)

**Files:**
- Modify: `R/files_daf_write.R`
- Test: `tests/testthat/test-files-vectors.R`

Wires the sparsify decision into `format_set_vector`: scan input, apply
`.should_sparsify_numeric`/`.should_sparsify_string`, write either dense
(as F3) or sparse (new). Also accept `Matrix::sparseVector` input →
always sparse.

- [ ] **Step 1: Tests**

```r
test_that("set_vector auto-sparsifies a vector dominated by zeros", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", sprintf("e%d", 1:100))
  v <- numeric(100)
  v[c(10, 50, 90)] <- c(1.5, 2.5, 3.5)
  set_vector(d, "cell", "x", v)
  # descriptor must be sparse
  j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "x.json"))
  expect_equal(j$format, "sparse")
  expect_equal(j$eltype, "Float64")
  expect_equal(j$indtype, "UInt32")
  # on-disk nzind must be 1-based Julia positions
  idx <- readBin(file.path(dir, "vectors", "cell", "x.nzind"),
                 what = "integer", n = 3L, size = 4L, endian = "little")
  expect_equal(idx, c(10L, 50L, 90L))
  # round-trip
  d2 <- files_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "x")), v)
})

test_that("set_vector keeps dense when threshold not met", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", sprintf("e%d", 1:10))
  v <- c(1, 2, 3, 4, 5, 6, 0, 0, 0, 0)  # 60% populated → dense
  set_vector(d, "cell", "x", v)
  j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "x.json"))
  expect_equal(j$format, "dense")
})

test_that("set_vector sparse Bool all-TRUE omits .nzval", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", sprintf("e%d", 1:100))
  v <- logical(100)
  v[c(5, 25, 55)] <- TRUE   # 3/100 nnz -> sparse
  set_vector(d, "cell", "b", v)
  j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "b.json"))
  expect_equal(j$format, "sparse")
  expect_equal(j$eltype, "Bool")
  expect_true( file.exists(file.path(dir, "vectors", "cell", "b.nzind")))
  expect_false(file.exists(file.path(dir, "vectors", "cell", "b.nzval")))
  d2 <- files_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "b")), v)
})

test_that("set_vector sparse string writes .nztxt with only non-empty values", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", sprintf("e%d", 1:21))
  v <- rep("", 21)
  v[5] <- "hello"
  set_vector(d, "cell", "s", v)   # 1/21 non-empty → sparse
  j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "s.json"))
  expect_equal(j$format, "sparse")
  expect_equal(j$eltype, "String")
  expect_equal(readLines(file.path(dir, "vectors", "cell", "s.nztxt")), "hello")
  d2 <- files_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "s")), v)
})

test_that("set_vector accepts Matrix::sparseVector and writes sparse unconditionally", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A", "B", "C", "D"))
  sv <- Matrix::sparseVector(x = c(10.0, 30.0), i = c(2L, 4L), length = 4L)
  set_vector(d, "cell", "sv", sv)
  j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "sv.json"))
  expect_equal(j$format, "sparse")
  d2 <- files_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "sv")), c(0, 10, 0, 30))
})
```

- [ ] **Step 2: Implement** in `R/files_daf_write.R`

```r
.files_write_vector_sparse_numeric <- function(vdir, name, nzind, nzval,
                                               eltype, indtype) {
  .write_bin_dense(file.path(vdir, paste0(name, ".nzind")),
                   as.integer(nzind), indtype)
  if (eltype == "Bool") {
    if (!all(nzval)) {
      .write_bin_dense(file.path(vdir, paste0(name, ".nzval")),
                       as.logical(nzval), "Bool")
    }
  } else {
    .write_bin_dense(file.path(vdir, paste0(name, ".nzval")), nzval, eltype)
  }
  .write_descriptor_sparse(file.path(vdir, paste0(name, ".json")),
                           dtype = eltype, indtype = indtype)
  invisible()
}

.files_write_vector_sparse_string <- function(vdir, name, nzind, nzval,
                                              indtype) {
  .write_bin_dense(file.path(vdir, paste0(name, ".nzind")),
                   as.integer(nzind), indtype)
  con <- file(file.path(vdir, paste0(name, ".nztxt")), open = "wb",
              encoding = "UTF-8")
  writeLines(nzval, con, useBytes = FALSE)
  close(con)
  .write_descriptor_sparse(file.path(vdir, paste0(name, ".json")),
                           dtype = "String", indtype = indtype)
  invisible()
}

# Replace the dense-only method from Task F3 with the adaptive dispatcher.
S7::method(format_set_vector,
           list(FilesDaf, S7::class_character, S7::class_character,
                S7::class_any, S7::class_logical)) <- function(daf, axis, name, vec, overwrite) {
  if (methods::is(vec, "sparseVector")) {
    return(.files_set_vector_sparse_input(daf, axis, name, vec, overwrite))
  }
  vec <- .validate_vector_value(daf, axis, name, vec)
  root <- .files_root(daf)
  vdir <- .path_vector_dir(root, axis)
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  desc_path <- file.path(vdir, paste0(name, ".json"))
  if (file.exists(desc_path) && !overwrite) {
    stop(sprintf("vector %s already exists on axis %s; use overwrite = TRUE",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  .files_vector_unlink_payload(vdir, name)
  eltype  <- .dtype_for_r_vector(vec)
  n       <- length(vec)
  indtype <- .indtype_for_size(n)
  go_sparse <- if (eltype == "String") {
    .should_sparsify_string(vec, indtype)
  } else {
    .should_sparsify_numeric(vec, eltype, indtype)
  }
  if (!go_sparse) {
    .files_write_vector_dense(vdir, name, vec)
  } else if (eltype == "String") {
    nz  <- which(nzchar(vec))
    .files_write_vector_sparse_string(vdir, name, nz, vec[nz], indtype)
  } else {
    nz <- if (is.logical(vec)) which(vec) else which(vec != 0)
    .files_write_vector_sparse_numeric(vdir, name, nz, vec[nz], eltype, indtype)
  }
  bump_vector_counter(daf, axis, name)
  invisible()
}

.files_set_vector_sparse_input <- function(daf, axis, name, sv, overwrite) {
  n <- format_axis_length(daf, axis)
  if (sv@length != n) {
    stop(sprintf("sparseVector %s length %d (expected %d) on axis %s",
                 sQuote(name), sv@length, n, sQuote(axis)), call. = FALSE)
  }
  root <- .files_root(daf)
  vdir <- .path_vector_dir(root, axis)
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  desc_path <- file.path(vdir, paste0(name, ".json"))
  if (file.exists(desc_path) && !overwrite) {
    stop(sprintf("vector %s already exists on axis %s; use overwrite = TRUE",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  .files_vector_unlink_payload(vdir, name)
  eltype  <- .dtype_for_r_vector(sv@x)
  indtype <- .indtype_for_size(n)
  .files_write_vector_sparse_numeric(vdir, name,
                                     as.integer(sv@i), sv@x, eltype, indtype)
  bump_vector_counter(daf, axis, name)
  invisible()
}
```

Also extend `.validate_vector_value` in `R/utils.R` to pass through a
`sparseVector` object unchanged (skip the length/name-reorder logic —
validation happens inside `.files_set_vector_sparse_input`). For MemoryDaf,
`.validate_vector_value` still rejects `sparseVector` — MemoryDaf doesn't
store sparse-vector objects. Alternative: densify at MemoryDaf's validate
step via `as.numeric(as.vector(sv))`. Pick densify-for-memory to keep
both backends symmetric on the public API.

- [ ] **Step 3: Run** → green.

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_write.R R/utils.R tests/testthat/test-files-vectors.R
git commit -m "feat(files_daf): format_set_vector adaptive sparsify (Julia byte parity)"
```

---

## Phase G — Sparse vector read (densify)

### Task G1: `format_get_vector` sparse densification

**Files:**
- Modify: `R/files_daf_read.R`
- Test: `tests/testthat/test-files-vectors.R`

- [ ] **Step 1: Tests**

```r
test_that("format_get_vector densifies a sparse Float64 vector written Julia-style", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A","B","C","D"), file.path(dir, "axes", "cell.txt"))
  # 1-based indices (Julia): entries at positions 2 and 4 → 10 and 30
  writeLines('{"format":"sparse","eltype":"Float64","indtype":"UInt32"}',
             file.path(dir, "vectors", "cell", "sv.json"))
  writeBin(c(2L, 4L), file.path(dir, "vectors", "cell", "sv.nzind"),
           size = 4L, endian = "little")
  writeBin(c(10.0, 30.0), file.path(dir, "vectors", "cell", "sv.nzval"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  v <- format_get_vector(d, "cell", "sv")
  expect_equal(v, c(0, 10, 0, 30))
})

test_that("format_get_vector sparse Bool without .nzval file synthesizes fill(TRUE, nnz)", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A","B","C"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"sparse","eltype":"Bool","indtype":"UInt32"}',
             file.path(dir, "vectors", "cell", "sb.json"))
  writeBin(c(1L, 3L), file.path(dir, "vectors", "cell", "sb.nzind"),
           size = 4L, endian = "little")
  # no .nzval
  d <- files_daf(dir, mode = "r")
  v <- format_get_vector(d, "cell", "sb")
  expect_equal(v, c(TRUE, FALSE, TRUE))
})
```

- [ ] **Step 2: Implement**

```r
.files_get_vector_sparse <- function(daf, axis, name, desc, n) {
  vdir <- .path_vector_dir(.files_root(daf), axis)
  ind_path <- file.path(vdir, paste0(name, ".nzind"))
  if (!file.exists(ind_path)) {
    stop(sprintf("files_daf: sparse vector %s missing .nzind", sQuote(name)),
         call. = FALSE)
  }
  indtype <- desc$indtype %||% "UInt32"
  nnz <- file.size(ind_path) %/% .dtype_size(indtype)
  idx <- .read_bin_dense(ind_path, nnz, indtype)  # 1-based positions
  if (desc$eltype == "Bool") {
    val_path <- file.path(vdir, paste0(name, ".nzval"))
    vals <- if (file.exists(val_path)) {
      as.logical(.read_bin_dense(val_path, nnz, "Bool"))
    } else {
      rep(TRUE, nnz)
    }
    out <- logical(n)
    out[as.integer(idx)] <- vals
    return(out)
  }
  if (desc$eltype == "String") {
    nztxt <- file.path(vdir, paste0(name, ".nztxt"))
    vals <- readLines(nztxt, encoding = "UTF-8", warn = FALSE)
    if (length(vals) != nnz) {
      stop(sprintf("files_daf: sparse string vector %s .nztxt has %d lines (expected %d)",
                   sQuote(name), length(vals), nnz), call. = FALSE)
    }
    out <- rep("", n)
    out[as.integer(idx)] <- vals
    return(out)
  }
  val_path <- file.path(vdir, paste0(name, ".nzval"))
  if (!file.exists(val_path)) {
    stop(sprintf("files_daf: sparse vector %s missing .nzval for non-Bool eltype",
                 sQuote(name)), call. = FALSE)
  }
  vals <- .read_bin_dense(val_path, nnz, desc$eltype)
  out <- if (desc$eltype %in% c("Int8","Int16","Int32","UInt8","UInt16","UInt32")) {
    integer(n)
  } else if (desc$eltype %in% c("Int64","UInt64")) {
    bit64::as.integer64(integer(n))
  } else {
    numeric(n)
  }
  out[as.integer(idx)] <- vals
  out
}
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R tests/testthat/test-files-vectors.R
git commit -m "feat(files_daf): format_get_vector sparse densification (Julia compat)"
```

---

## Phase H — Dense matrix read/write

### Task H1: `format_has_matrix` + `format_matrices_set`

**Files:**
- Modify: `R/files_daf_read.R`
- Test: `tests/testthat/test-files-matrices.R`

Mirrors Phase F1 but for matrices. Descriptor at `matrices/<rows>/<cols>/<name>.json`.

- [ ] **Step 1: Tests** (similar to F1 but for matrix dir).
- [ ] **Step 2: Implement**

```r
.files_matrix_desc_path <- function(root, rows_axis, cols_axis, name) {
  file.path(.path_matrix_dir(root, rows_axis, cols_axis), paste0(name, ".json"))
}

.files_has_matrix <- function(daf, rows_axis, cols_axis, name) {
  if (!format_has_axis(daf, rows_axis) || !format_has_axis(daf, cols_axis)) return(FALSE)
  file.exists(.files_matrix_desc_path(.files_root(daf), rows_axis, cols_axis, name))
}
S7::method(format_has_matrix,
           list(FilesDaf, S7::class_character, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis, name) .files_has_matrix(daf, rows_axis, columns_axis, name)
S7::method(format_has_matrix,
           list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis, name) .files_has_matrix(daf, rows_axis, columns_axis, name)

.files_matrices_set <- function(daf, rows_axis, cols_axis) {
  if (!format_has_axis(daf, rows_axis) || !format_has_axis(daf, cols_axis)) return(character(0L))
  dir <- .path_matrix_dir(.files_root(daf), rows_axis, cols_axis)
  if (!dir.exists(dir)) return(character(0L))
  files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
  sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(format_matrices_set,
           list(FilesDaf, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis) .files_matrices_set(daf, rows_axis, columns_axis)
S7::method(format_matrices_set,
           list(FilesDafReadOnly, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis) .files_matrices_set(daf, rows_axis, columns_axis)
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R tests/testthat/test-files-matrices.R
git commit -m "feat(files_daf): format_has_matrix + format_matrices_set"
```

---

### Task H2: `format_get_matrix` dense (mmap column-major)

**Files:**
- Modify: `R/files_daf_read.R`

On-disk dense matrix: column-major, Fortran order. R `matrix(x, nrow, ncol)` with column-major default matches. We mmap the `.data` file for Float64 and set the dim attribute via `dim<-` which preserves ALTREP when possible.

- [ ] **Step 1: Tests**

```r
test_that("format_get_matrix dense Float64 round-trips with correct shape", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A","B","C"), file.path(dir, "axes", "cell.txt"))
  writeLines(c("X","Y"), file.path(dir, "axes", "gene.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "matrices", "cell", "gene", "m.json"))
  # column-major: column 1 = c(1,2,3), column 2 = c(4,5,6)
  writeBin(c(1,2,3,4,5,6),
           file.path(dir, "matrices", "cell", "gene", "m.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  m <- format_get_matrix(d, "cell", "gene", "m")
  expect_equal(dim(m), c(3L, 2L))
  expect_equal(m[2, 2], 5)
  expect_equal(m[, 1], c(1, 2, 3))
})

test_that("format_get_matrix dense Int32", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A","B"), file.path(dir, "axes", "cell.txt"))
  writeLines(c("X","Y"), file.path(dir, "axes", "gene.txt"))
  writeLines('{"format":"dense","eltype":"Int32"}',
             file.path(dir, "matrices", "cell", "gene", "mi.json"))
  writeBin(c(1L,2L,3L,4L),
           file.path(dir, "matrices", "cell", "gene", "mi.data"),
           size = 4L, endian = "little")
  d <- files_daf(dir, mode = "r")
  m <- format_get_matrix(d, "cell", "gene", "mi")
  expect_equal(dim(m), c(2L, 2L))
  expect_true(is.integer(m))
  expect_equal(m, matrix(1:4, nrow = 2))
})
```

- [ ] **Step 2: Implement**

```r
S7::method(format_get_matrix,
           list(FilesDaf, S7::class_character, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis, name) {
  .files_get_matrix_impl(daf, rows_axis, columns_axis, name)
}
S7::method(format_get_matrix,
           list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis, name) {
  .files_get_matrix_impl(daf, rows_axis, columns_axis, name)
}

.files_get_matrix_impl <- function(daf, rows_axis, cols_axis, name) {
  root <- .files_root(daf)
  desc_path <- .files_matrix_desc_path(root, rows_axis, cols_axis, name)
  if (!file.exists(desc_path)) {
    stop(sprintf("matrix %s does not exist on axes (%s, %s)",
                 sQuote(name), sQuote(rows_axis), sQuote(cols_axis)),
         call. = FALSE)
  }
  desc <- .read_descriptor(desc_path)
  nr <- format_axis_length(daf, rows_axis)
  nc <- format_axis_length(daf, cols_axis)
  if (desc$format == "dense")  return(.files_get_matrix_dense(daf, rows_axis, cols_axis, name, desc, nr, nc))
  if (desc$format == "sparse") return(.files_get_matrix_sparse(daf, rows_axis, cols_axis, name, desc, nr, nc))
  stop(sprintf("files_daf: unsupported matrix format %s", desc$format))
}

.files_get_matrix_dense <- function(daf, rows_axis, cols_axis, name, desc, nr, nc) {
  root <- .files_root(daf)
  mdir <- .path_matrix_dir(root, rows_axis, cols_axis)
  elt  <- desc$eltype
  if (elt == "String") return(.files_get_matrix_dense_string(daf, mdir, name, nr, nc))
  data_path <- file.path(mdir, paste0(name, ".data"))
  if (!file.exists(data_path)) {
    stop(sprintf("files_daf: missing payload %s", sQuote(data_path)), call. = FALSE)
  }
  total <- nr * nc
  expected_bytes <- total * .dtype_size(elt)
  if (file.size(data_path) < expected_bytes) {
    stop(sprintf("files_daf: matrix %s payload truncated (%d < %d bytes)",
                 sQuote(name), file.size(data_path), expected_bytes), call. = FALSE)
  }
  use_mmap <- isTRUE(dafr_opt("dafr.mmap"))
  v <- if (use_mmap && elt == "Float64") {
    mmap_real(data_path, total)
  } else if (use_mmap && elt == "Int32") {
    mmap_int(data_path, total)
  } else {
    .read_bin_dense(data_path, total, elt)
  }
  dim(v) <- c(as.integer(nr), as.integer(nc))
  v
}

.files_get_matrix_dense_string <- function(daf, mdir, name, nr, nc) {
  txt <- file.path(mdir, paste0(name, ".txt"))
  if (!file.exists(txt)) stop(sprintf("files_daf: missing payload %s", sQuote(txt)))
  vals <- readLines(txt, encoding = "UTF-8", warn = FALSE)
  expected <- nr * nc
  if (length(vals) != expected) {
    stop(sprintf("files_daf: string matrix has %d lines (expected %d)",
                 length(vals), expected), call. = FALSE)
  }
  matrix(vals, nrow = nr, ncol = nc)  # column-major by default
}
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R tests/testthat/test-files-matrices.R
git commit -m "feat(files_daf): format_get_matrix dense (mmap column-major)"
```

---

### Task H3: `format_set_matrix` dense

**Files:**
- Modify: `R/files_daf_write.R`
- Test: `tests/testthat/test-files-matrices.R`

- [ ] **Step 1: Tests**

```r
test_that("set_matrix + get_matrix dense Float64 round-trip", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B","C"))
  add_axis(d, "gene", c("X","Y"))
  m <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)
  set_matrix(d, "cell", "gene", "m", m)
  d2 <- files_daf(dir, mode = "r")
  m2 <- get_matrix(d2, "cell", "gene", "m")
  expect_equal(unname(m2), m)
  expect_equal(dimnames(m2), list(c("A","B","C"), c("X","Y")))
})

test_that("set_matrix dense Int32", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B"))
  add_axis(d, "gene", c("X","Y"))
  m <- matrix(1:4, nrow = 2)
  set_matrix(d, "cell", "gene", "mi", m)
  d2 <- files_daf(dir, mode = "r")
  expect_equal(unname(get_matrix(d2, "cell", "gene", "mi")), m)
})

test_that("set_matrix dense String", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B"))
  add_axis(d, "gene", c("X","Y"))
  m <- matrix(c("a","b","c","d"), nrow = 2)
  set_matrix(d, "cell", "gene", "ms", m)
  d2 <- files_daf(dir, mode = "r")
  expect_equal(unname(get_matrix(d2, "cell", "gene", "ms")), m)
})
```

- [ ] **Step 2: Implement**

```r
S7::method(format_set_matrix,
           list(FilesDaf, S7::class_character, S7::class_character,
                S7::class_character, S7::class_any, S7::class_logical)) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
  mat <- .validate_matrix_value(daf, rows_axis, columns_axis, name, mat)
  root <- .files_root(daf)
  mdir <- .path_matrix_dir(root, rows_axis, columns_axis)
  dir.create(mdir, recursive = TRUE, showWarnings = FALSE)
  desc_path <- file.path(mdir, paste0(name, ".json"))
  if (file.exists(desc_path) && !overwrite) {
    stop(sprintf("matrix %s already exists on axes (%s, %s); use overwrite = TRUE",
                 sQuote(name), sQuote(rows_axis), sQuote(columns_axis)),
         call. = FALSE)
  }
  .files_matrix_unlink_payload(mdir, name)
  if (methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")) {
    return(.files_write_matrix_sparse(mdir, name, mat))
  }
  dtype <- .dtype_for_r_vector(as.vector(mat))
  if (dtype == "String") {
    con <- file(file.path(mdir, paste0(name, ".txt")), open = "wb",
                encoding = "UTF-8")
    writeLines(as.vector(mat), con, useBytes = FALSE)  # column-major as.vector
    close(con)
  } else {
    .write_bin_dense(file.path(mdir, paste0(name, ".data")),
                     as.vector(mat), dtype)
  }
  .write_descriptor_dense(desc_path, dtype)
  bump_matrix_counter(daf, rows_axis, columns_axis, name)
  invisible()
}

.files_matrix_unlink_payload <- function(mdir, name) {
  for (ext in c(".data", ".txt", ".colptr", ".rowval", ".nzval", ".nztxt")) {
    p <- file.path(mdir, paste0(name, ext))
    if (file.exists(p)) unlink(p, force = TRUE)
  }
}
```

`.files_write_matrix_sparse` is defined in Phase I.

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_write.R tests/testthat/test-files-matrices.R
git commit -m "feat(files_daf): format_set_matrix dense (numeric/String/Bool)"
```

---

### Task H4: `format_delete_matrix`

**Files:**
- Modify: `R/files_daf_write.R`
- Test: `tests/testthat/test-files-matrices.R`

- [ ] **Step 1: Tests**

```r
test_that("delete_matrix removes all payload + descriptor", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B"))
  add_axis(d, "gene", c("X","Y"))
  set_matrix(d, "cell", "gene", "m", matrix(1:4, 2))
  delete_matrix(d, "cell", "gene", "m")
  expect_false(file.exists(file.path(dir, "matrices","cell","gene","m.json")))
  expect_false(file.exists(file.path(dir, "matrices","cell","gene","m.data")))
})
```

- [ ] **Step 2: Implement**

```r
S7::method(format_delete_matrix,
           list(FilesDaf, S7::class_character, S7::class_character,
                S7::class_character, S7::class_logical)) <- function(daf, rows_axis, columns_axis, name, must_exist) {
  mdir <- .path_matrix_dir(.files_root(daf), rows_axis, columns_axis)
  desc_path <- file.path(mdir, paste0(name, ".json"))
  if (!file.exists(desc_path)) {
    if (must_exist) {
      stop(sprintf("matrix %s does not exist on axes (%s, %s)",
                   sQuote(name), sQuote(rows_axis), sQuote(columns_axis)),
           call. = FALSE)
    }
    return(invisible())
  }
  unlink(desc_path, force = TRUE)
  .files_matrix_unlink_payload(mdir, name)
  bump_matrix_counter(daf, rows_axis, columns_axis, name)
  invisible()
}
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_write.R tests/testthat/test-files-matrices.R
git commit -m "feat(files_daf): format_delete_matrix"
```

---

## Phase I — Sparse matrix read/write + relayout

### Task I1: `format_get_matrix` sparse (CSC, 1-based → 0-based)

**Files:**
- Modify: `R/files_daf_read.R`
- Test: `tests/testthat/test-files-matrices.R`

- [ ] **Step 1: Tests**

```r
test_that("format_get_matrix densifies sparse CSC written Julia-style", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A","B","C"), file.path(dir, "axes", "cell.txt"))
  writeLines(c("X","Y"), file.path(dir, "axes", "gene.txt"))
  # Sparse 3x2: col1 has 2 nnz at rows 1,3 (vals 10,20); col2 has 1 nnz at row 2 (val 30)
  writeLines('{"format":"sparse","eltype":"Float64","indtype":"UInt32"}',
             file.path(dir, "matrices", "cell", "gene", "sm.json"))
  writeBin(c(1L, 3L, 4L), file.path(dir, "matrices","cell","gene","sm.colptr"),
           size = 4L, endian = "little")
  writeBin(c(1L, 3L, 2L), file.path(dir, "matrices","cell","gene","sm.rowval"),
           size = 4L, endian = "little")
  writeBin(c(10.0, 20.0, 30.0), file.path(dir, "matrices","cell","gene","sm.nzval"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  m <- format_get_matrix(d, "cell", "gene", "sm")
  expect_s4_class(m, "dgCMatrix")
  expect_equal(dim(m), c(3L, 2L))
  expect_equal(as.matrix(m), matrix(c(10,0,20,0,30,0), nrow=3))
})
```

- [ ] **Step 2: Implement**

```r
.files_get_matrix_sparse <- function(daf, rows_axis, cols_axis, name, desc, nr, nc) {
  mdir <- .path_matrix_dir(.files_root(daf), rows_axis, cols_axis)
  indtype <- desc$indtype %||% "UInt32"
  colptr_path <- file.path(mdir, paste0(name, ".colptr"))
  rowval_path <- file.path(mdir, paste0(name, ".rowval"))
  nzval_path  <- file.path(mdir, paste0(name, ".nzval"))
  if (!file.exists(colptr_path) || !file.exists(rowval_path)) {
    stop(sprintf("files_daf: sparse matrix %s missing colptr/rowval",
                 sQuote(name)), call. = FALSE)
  }
  colptr <- .read_bin_dense(colptr_path, as.integer(nc) + 1L, indtype)
  nnz <- as.integer(colptr[length(colptr)]) - 1L  # 1-based last entry = nnz+1
  rowval <- .read_bin_dense(rowval_path, nnz, indtype)
  if (desc$eltype == "Bool") {
    vals <- if (file.exists(nzval_path)) {
      as.logical(.read_bin_dense(nzval_path, nnz, "Bool"))
    } else {
      rep(TRUE, nnz)
    }
    return(methods::new("lgCMatrix",
      x = vals,
      i = as.integer(rowval) - 1L,
      p = as.integer(colptr) - 1L,
      Dim = c(as.integer(nr), as.integer(nc)),
      Dimnames = list(NULL, NULL)))
  }
  if (!file.exists(nzval_path)) {
    stop(sprintf("files_daf: sparse matrix %s missing .nzval for non-Bool",
                 sQuote(name)), call. = FALSE)
  }
  vals <- .read_bin_dense(nzval_path, nnz, desc$eltype)
  methods::new("dgCMatrix",
    x = as.double(vals),
    i = as.integer(rowval) - 1L,
    p = as.integer(colptr) - 1L,
    Dim = c(as.integer(nr), as.integer(nc)),
    Dimnames = list(NULL, NULL))
}
```

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R tests/testthat/test-files-matrices.R
git commit -m "feat(files_daf): format_get_matrix sparse CSC (1→0-based conversion)"
```

---

### Task I2: `format_set_matrix` sparse (`dgCMatrix`/`lgCMatrix`)

**Files:**
- Modify: `R/files_daf_write.R`
- Test: `tests/testthat/test-files-matrices.R`

- [ ] **Step 1: Tests**

```r
test_that("set_matrix + get_matrix sparse dgCMatrix round-trip", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B","C"))
  add_axis(d, "gene", c("X","Y"))
  sp <- Matrix::sparseMatrix(i = c(1,3,2), j = c(1,1,2), x = c(10,20,30),
                             dims = c(3, 2))
  set_matrix(d, "cell", "gene", "sm", sp)
  # colptr (1-based): 1,3,4
  cp <- readBin(file.path(dir, "matrices","cell","gene","sm.colptr"),
                what = "integer", n = 3L, size = 4L, endian = "little")
  expect_equal(cp, c(1L, 3L, 4L))
  # rowval (1-based): 1,3,2
  rv <- readBin(file.path(dir, "matrices","cell","gene","sm.rowval"),
                what = "integer", n = 3L, size = 4L, endian = "little")
  expect_equal(rv, c(1L, 3L, 2L))
  d2 <- files_daf(dir, mode = "r")
  m <- get_matrix(d2, "cell", "gene", "sm")
  expect_s4_class(m, "dgCMatrix")
  expect_equal(as.matrix(unname(m)), as.matrix(sp))
})

test_that("set_matrix sparse picks UInt64 indtype for oversized shape", {
  # Stub test — only exercised when max(nr,nc,nnz) > 2^31-1, outside testable range.
  skip("UInt64 indtype only triggers at oversized axes; covered by property test in slice 3")
})
```

- [ ] **Step 2: Implement**

```r
.files_write_matrix_sparse <- function(mdir, name, mat) {
  # mat is dgCMatrix or lgCMatrix; Dim already validated by .validate_matrix_value.
  is_bool <- methods::is(mat, "lgCMatrix")
  dtype <- if (is_bool) "Bool" else "Float64"
  nr <- nrow(mat); nc <- ncol(mat)
  nnz <- length(mat@x)
  indtype <- .indtype_for_size(max(nr, nc, nnz))
  # colptr (0-based in dgCMatrix@p) → 1-based on disk
  .write_bin_dense(file.path(mdir, paste0(name, ".colptr")),
                   as.integer(mat@p) + 1L, indtype)
  # rowval similarly
  .write_bin_dense(file.path(mdir, paste0(name, ".rowval")),
                   as.integer(mat@i) + 1L, indtype)
  if (is_bool) {
    # Bool: write .nzval only when not all true (to match Julia)
    if (!all(mat@x)) {
      .write_bin_dense(file.path(mdir, paste0(name, ".nzval")),
                       as.logical(mat@x), "Bool")
    }
  } else {
    .write_bin_dense(file.path(mdir, paste0(name, ".nzval")),
                     as.double(mat@x), "Float64")
  }
  .write_descriptor_sparse(file.path(mdir, paste0(name, ".json")),
                           dtype = dtype, indtype = indtype)
  invisible()
}
```

Wire this into `format_set_matrix` (already done in Phase H3's stub call).

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_write.R tests/testthat/test-files-matrices.R
git commit -m "feat(files_daf): format_set_matrix sparse (dgCMatrix/lgCMatrix)"
```

---

### Task I3: `format_relayout_matrix` + delete-axis cascade full test

**Files:**
- Modify: `R/files_daf_write.R`
- Test: `tests/testthat/test-files-matrices.R`, `test-files-axes.R` (re-enable cascade test)

- [ ] **Step 1: Tests**

```r
test_that("relayout_matrix stores transpose at flipped axis pair", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B","C"))
  add_axis(d, "gene", c("X","Y"))
  m <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)
  set_matrix(d, "cell", "gene", "m", m)
  relayout_matrix(d, "cell", "gene", "m")
  # now matrices/gene/cell/m.* must exist
  expect_true(file.exists(file.path(dir, "matrices", "gene", "cell", "m.json")))
  d2 <- files_daf(dir, mode = "r")
  m_flipped <- get_matrix(d2, "gene", "cell", "m")
  expect_equal(unname(m_flipped), t(m))
})
```

- [ ] **Step 2: Implement**

```r
S7::method(format_relayout_matrix,
           list(FilesDaf, S7::class_character, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis, name) {
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

- [ ] **Step 3: Re-enable the delete_axis cascade test** (deferred from Task E2):

```r
test_that("delete_axis cascades to dependent vectors + matrices on FilesDaf", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B"))
  add_axis(d, "gene", c("X","Y"))
  set_vector(d, "cell", "donor", c(1,2))
  set_matrix(d, "cell", "gene", "m", matrix(1:4, 2, 2))
  set_matrix(d, "gene", "cell", "m2", matrix(1:4, 2, 2))
  delete_axis(d, "cell")
  expect_false(dir.exists(file.path(dir, "vectors", "cell")))
  expect_false(dir.exists(file.path(dir, "matrices", "cell")))
  expect_false(dir.exists(file.path(dir, "matrices", "gene", "cell")))
})
```

- [ ] **Step 4: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_write.R tests/testthat/test-files-matrices.R tests/testthat/test-files-axes.R
git commit -m "feat(files_daf): format_relayout_matrix + delete_axis cascade coverage"
```

---

## Phase J — Cache mapped-tier integration + mmap fallback

### Task J1: Mapped-tier stores ALTREP views; invalidates on write

**Files:**
- Modify: `R/files_daf_read.R` — route reads through `cache_lookup` / `cache_store` on the `mapped` tier.
- Test: `tests/testthat/test-files-cache.R`

Key idea: `format_get_vector` / `format_get_matrix` on FilesDaf cache the returned ALTREP view in the `mapped` tier, keyed by the standard cache key and stamped by `vector_stamp` / `matrix_stamp`. On next read with same stamp → hit (no `mmap_real()` re-open). On write, counter bumps → stamp changes → cache_lookup returns NULL → re-open.

Note: we cache the ALTREP view itself. Its underlying `shared_ptr<MmapRegion>` keeps the mmap alive even if the file is overwritten; the `mapped` bucket is exempt from LRU eviction so the region lives until `empty_cache(clear="mapped")` or the daf object is GC'd.

- [ ] **Step 1: Tests**

```r
test_that("FilesDaf format_get_vector caches ALTREP view in mapped tier", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B","C"))
  set_vector(d, "cell", "x", c(1.5, 2.5, 3.5))
  v1 <- format_get_vector(d, "cell", "x")
  v2 <- format_get_vector(d, "cell", "x")
  # Same object identity (cache hit)
  ce <- S7::prop(d, "cache")
  expect_true(exists(cache_key_vector("cell", "x"), envir = ce$mapped))
})

test_that("writing bumps the counter and invalidates mapped cache", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B"))
  set_vector(d, "cell", "x", c(1.0, 2.0))
  v1 <- format_get_vector(d, "cell", "x")
  set_vector(d, "cell", "x", c(10.0, 20.0), overwrite = TRUE)
  v2 <- format_get_vector(d, "cell", "x")
  expect_equal(unname(v2), c(10.0, 20.0))
})
```

- [ ] **Step 2: Implement** — wrap `.files_get_vector_impl` / `.files_get_matrix_impl`:

```r
.files_get_vector_cached <- function(daf, axis, name) {
  ce <- S7::prop(daf, "cache")
  key <- cache_key_vector(axis, name)
  stamp <- vector_stamp(daf, axis, name)
  hit <- cache_lookup(ce, "mapped", key, stamp)
  if (!is.null(hit)) return(hit)
  v <- .files_get_vector_impl(daf, axis, name)
  cache_store(ce, "mapped", key, v, stamp, size_bytes = 0)
  v
}
```

Change `S7::method(format_get_vector, ...)` on `FilesDaf`/`FilesDafReadOnly` to call `.files_get_vector_cached`. Same pattern for matrix.

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add R/files_daf_read.R tests/testthat/test-files-cache.R
git commit -m "feat(files_daf): mapped-tier caching with version-stamp invalidation"
```

---

### Task J2: `dafr.mmap = FALSE` global fallback

**Files:**
- No changes to code — `.files_get_vector_dense` and `.files_get_matrix_dense` already branch on `isTRUE(dafr_opt("dafr.mmap"))`.
- Test: `tests/testthat/test-files-cache.R`

- [ ] **Step 1: Tests**

```r
test_that("dafr.mmap = FALSE returns non-ALTREP vectors for dense reads", {
  skip_if_not(requireNamespace("withr", quietly = TRUE))
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B"))
  set_vector(d, "cell", "x", c(1.0, 2.0))
  v <- withr::with_options(list(dafr.mmap = FALSE),
                           format_get_vector(d, "cell", "x"))
  expect_equal(unname(v), c(1.0, 2.0))
  expect_false(is_altrep(v))
})

test_that("dafr.mmap = FALSE matrix reads also skip ALTREP", {
  skip_if_not(requireNamespace("withr", quietly = TRUE))
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B"))
  add_axis(d, "gene", c("X","Y"))
  set_matrix(d, "cell", "gene", "m", matrix(c(1.0,2.0,3.0,4.0), 2, 2))
  m <- withr::with_options(list(dafr.mmap = FALSE),
                           format_get_matrix(d, "cell", "gene", "m"))
  expect_false(is_altrep(m))
})
```

- [ ] **Step 2: Run** — these should pass under the existing branch logic.

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add tests/testthat/test-files-cache.R
git commit -m "test(files_daf): dafr.mmap = FALSE fallback read path"
```

---

## Phase K — Julia bidirectional compatibility fixtures

### Task K1: Generate the Julia-written FilesDaf fixture via conda env

**Files:**
- Create: `dev/scripts/regen-julia-fixture.jl`
- Create: `tests/testthat/fixtures/julia-filesdaf/` (committed binary + text files)
- Create: `tests/testthat/helper-julia.R`
- Test: `tests/testthat/test-files-julia-compat.R`

Julia runs via the existing conda env `dafr-mcview` (Julia 1.12 +
`DataAxesFormats` 0.2.0 already installed — verified 2026-04-20). The
fixture is regenerated whenever the upstream spec evolves; generated
files are committed so the test suite does not need Julia at run time,
but the script is idempotent and the test suite will refuse to pass if
the fixture is stale (see Task K3's live round-trip).

Fixture contents:

- Axes: `cell` (`A`, `B`, `C`, `D`), `gene` (`X`, `Y`).
- Scalars: `pi` (Float64), `cells` (Int64), `note` (String).
- Dense vector `cell/donor` (Int32, `1:4`).
- Sparse vector `cell/sparse_x` (Float64, nnz=2 at 1-based positions 2, 4).
- Sparse Bool vector `cell/flags` (all-TRUE at positions 1,3 — exercises
  the omitted `.nzval` path).
- Dense matrix `cell/gene/dense_m` (Float64 4×2).
- Sparse CSC matrix `cell/gene/sparse_m` (Float64 4×2, 3 non-zeros).
- Sparse Bool matrix `cell/gene/mask` (4×2, two TRUE entries).

- [ ] **Step 1: Write `dev/scripts/regen-julia-fixture.jl`**

```julia
using DataAxesFormats, SparseArrays

root = joinpath(@__DIR__, "..", "..", "tests", "testthat",
                "fixtures", "julia-filesdaf")
isdir(root) && rm(root; recursive = true)
mkpath(dirname(root))

daf = FilesDaf(root, "w")

add_axis!(daf, "cell", ["A", "B", "C", "D"])
add_axis!(daf, "gene", ["X", "Y"])

set_scalar!(daf, "pi",    3.14)
set_scalar!(daf, "cells", Int64(100))
set_scalar!(daf, "note",  "hello")

set_vector!(daf, "cell", "donor", Int32[1, 2, 3, 4])

set_vector!(daf, "cell", "sparse_x",
            SparseVector{Float64, UInt32}(4, UInt32[2, 4],
                                          Float64[10.0, 30.0]))

# All-true sparse Bool (.nzval omitted on disk)
set_vector!(daf, "cell", "flags",
            SparseVector{Bool, UInt32}(4, UInt32[1, 3], Bool[true, true]))

set_matrix!(daf, "cell", "gene", "dense_m",
            Float64[1 5;
                    2 6;
                    3 7;
                    4 8])

set_matrix!(daf, "cell", "gene", "sparse_m",
            sparse(Int32[1, 3, 2],
                   Int32[1, 1, 2],
                   Float64[10.0, 20.0, 30.0], 4, 2))

set_matrix!(daf, "cell", "gene", "mask",
            sparse(Int32[1, 2],
                   Int32[1, 2],
                   Bool[true, true], 4, 2))

println("wrote fixture to $root")
```

- [ ] **Step 2: Run via conda env**

```bash
cd ~/src/dafr-native
conda run -n dafr-mcview julia --project=@. \
  dev/scripts/regen-julia-fixture.jl
```

If the package isn't activated in a project env, use the global env:

```bash
conda run -n dafr-mcview julia dev/scripts/regen-julia-fixture.jl
```

Expected stdout: `wrote fixture to .../tests/testthat/fixtures/julia-filesdaf`.

- [ ] **Step 3: Write `tests/testthat/helper-julia.R`**

```r
fixture_path <- function() {
  testthat::test_path("fixtures", "julia-filesdaf")
}

.have_julia_env <- function() {
  out <- suppressWarnings(
    system2("conda", c("run", "-n", "dafr-mcview", "julia", "--version"),
            stdout = TRUE, stderr = TRUE))
  length(out) > 0L && any(grepl("^julia", out))
}

run_julia <- function(script_lines) {
  # Execute a list of Julia statements via the conda env, returning stdout.
  script <- tempfile(fileext = ".jl")
  on.exit(unlink(script), add = TRUE)
  writeLines(script_lines, script)
  system2("conda", c("run", "-n", "dafr-mcview", "julia", script),
          stdout = TRUE, stderr = TRUE)
}
```

- [ ] **Step 4: Write compat tests** in `tests/testthat/test-files-julia-compat.R`:

```r
test_that("can read Julia-written scalars", {
  expect_true(dir.exists(fixture_path()))
  d <- files_daf(fixture_path(), mode = "r")
  expect_equal(get_scalar(d, "pi"),    3.14)
  expect_equal(get_scalar(d, "cells"), bit64::as.integer64(100))
  expect_equal(get_scalar(d, "note"),  "hello")
})

test_that("can read Julia-written axes + dense + sparse vectors", {
  d <- files_daf(fixture_path(), mode = "r")
  expect_equal(axis_vector(d, "cell"), c("A", "B", "C", "D"))
  expect_equal(axis_vector(d, "gene"), c("X", "Y"))
  expect_equal(unname(get_vector(d, "cell", "donor")),     c(1L, 2L, 3L, 4L))
  expect_equal(unname(get_vector(d, "cell", "sparse_x")),  c(0, 10, 0, 30))
  expect_equal(unname(get_vector(d, "cell", "flags")),
               c(TRUE, FALSE, TRUE, FALSE))
})

test_that("can read Julia-written dense + sparse + Bool matrices", {
  d <- files_daf(fixture_path(), mode = "r")
  dm <- get_matrix(d, "cell", "gene", "dense_m")
  expect_equal(dim(dm), c(4L, 2L))
  expect_equal(unname(dm), matrix(c(1,2,3,4,5,6,7,8), nrow = 4))
  sm <- get_matrix(d, "cell", "gene", "sparse_m")
  expect_equal(dim(sm), c(4L, 2L))
  expect_s4_class(sm, "dgCMatrix")
  expect_equal(as.matrix(unname(sm)),
               matrix(c(10,0,20,0,  0,30,0,0), nrow = 4))
  mk <- get_matrix(d, "cell", "gene", "mask")
  expect_s4_class(mk, "lgCMatrix")
  expect_equal(as.matrix(unname(mk)),
               matrix(c(TRUE, FALSE, FALSE, FALSE,
                        FALSE, TRUE,  FALSE, FALSE), nrow = 4))
})
```

- [ ] **Step 5: Commit**

```bash
cd ~/src/dafr-native
git add tests/testthat/fixtures/ tests/testthat/test-files-julia-compat.R \
        tests/testthat/helper-julia.R dev/scripts/regen-julia-fixture.jl
git commit -m "test(files_daf): Julia fixture + bidirectional read compat"
```

---

### Task K2: Round-trip MemoryDaf ↔ FilesDaf

**Files:**
- Test: `tests/testthat/test-files-julia-compat.R`

Manual per-entity copy (a full `copy_all` awaits Slice 4+).

- [ ] **Step 1: Tests**

```r
.copy_all_memory_to_files <- function(src, dir) {
  dst <- files_daf(dir, mode = "w+")
  for (nm in scalars_set(src)) set_scalar(dst, nm, get_scalar(src, nm))
  for (ax in axes_set(src))     add_axis(dst, ax, axis_vector(src, ax))
  for (ax in axes_set(src)) {
    for (nm in vectors_set(src, ax)) {
      set_vector(dst, ax, nm, unname(get_vector(src, ax, nm)))
    }
  }
  for (ra in axes_set(src)) for (ca in axes_set(src)) {
    for (nm in matrices_set(src, ra, ca)) {
      set_matrix(dst, ra, ca, nm, unname(get_matrix(src, ra, ca, nm)))
    }
  }
  dst
}

test_that("MemoryDaf → FilesDaf → MemoryDaf round-trip preserves data", {
  m <- memory_daf(name = "src")
  set_scalar(m, "k", 5L)
  add_axis(m, "cell", c("A","B"))
  add_axis(m, "gene", c("X","Y","Z"))
  set_vector(m, "cell", "v", c(10.0, 20.0))
  set_matrix(m, "cell", "gene", "m", matrix(1:6, 2, 3))
  # dense matrix
  dir <- new_tempdir()
  f <- .copy_all_memory_to_files(m, dir)
  f2 <- files_daf(dir, mode = "r")
  expect_equal(get_scalar(f2, "k"), 5L)
  expect_equal(axis_vector(f2, "cell"), c("A","B"))
  expect_equal(unname(get_vector(f2, "cell", "v")), c(10.0, 20.0))
  expect_equal(unname(get_matrix(f2, "cell", "gene", "m")),
               matrix(1:6, 2, 3))
})

test_that("sparse dgCMatrix round-trip through FilesDaf", {
  m <- memory_daf()
  add_axis(m, "cell", c("A","B","C"))
  add_axis(m, "gene", c("X","Y"))
  sp <- Matrix::sparseMatrix(i = c(1,3,2), j = c(1,1,2), x = c(10,20,30),
                             dims = c(3, 2))
  set_matrix(m, "cell", "gene", "sm", sp)
  dir <- new_tempdir()
  f <- .copy_all_memory_to_files(m, dir)
  f2 <- files_daf(dir, mode = "r")
  sm2 <- get_matrix(f2, "cell", "gene", "sm")
  expect_s4_class(sm2, "dgCMatrix")
  expect_equal(as.matrix(unname(sm2)), as.matrix(sp))
})
```

- [ ] **Step 2: Commit**

```bash
cd ~/src/dafr-native
git add tests/testthat/test-files-julia-compat.R
git commit -m "test(files_daf): MemoryDaf ↔ FilesDaf round-trip via manual copy"
```

---

### Task K3: Live R → Julia → R bidirectional round-trip

**Files:**
- Test: `tests/testthat/test-files-julia-compat.R`

Catches byte-level drift that the static fixture can't: R writes a store
containing dense + sparse vectors, dense + sparse matrices, all scalar
types. Julia (via conda env) opens it, verifies shape + values, writes
a derived store. R re-opens the derived store and asserts equivalence.

The test is `skip_if_not(.have_julia_env())` so CI without Julia still
works; local runs (and dedicated CI matrix row) exercise it.

- [ ] **Step 1: Tests**

```r
test_that("R-written store is readable by Julia with identical values", {
  skip_if_not(.have_julia_env())
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  add_axis(d, "cell", c("A","B","C","D"))
  add_axis(d, "gene", c("X","Y"))
  set_scalar(d, "pi",    3.14)
  set_scalar(d, "cells", bit64::as.integer64(100))
  set_scalar(d, "note",  "hello")
  set_vector(d, "cell", "donor", c(1L,2L,3L,4L))
  # adaptive sparsify: 2/4 zeros -> mass ratio triggers sparse
  set_vector(d, "cell", "sx", c(0, 10, 0, 30))
  set_matrix(d, "cell", "gene", "dm", matrix(1:8, nrow=4))
  sp <- Matrix::sparseMatrix(i = c(1,3,2), j = c(1,1,2),
                             x = c(10,20,30), dims = c(4,2))
  set_matrix(d, "cell", "gene", "sm", sp)

  script <- c(
    'using DataAxesFormats, SparseArrays',
    sprintf('daf = FilesDaf(raw"%s", "r")', dir),
    '@assert get_scalar(daf, "pi")    == 3.14',
    '@assert get_scalar(daf, "cells") == Int64(100)',
    '@assert get_scalar(daf, "note")  == "hello"',
    '@assert axis_array(daf, "cell")  == ["A","B","C","D"]',
    '@assert get_vector(daf, "cell", "donor") == Int32[1,2,3,4]',
    '@assert get_vector(daf, "cell", "sx") == Float64[0,10,0,30]',
    '@assert size(get_matrix(daf, "cell", "gene", "dm")) == (4,2)',
    '@assert get_matrix(daf, "cell", "gene", "dm") == Float64[1 5;2 6;3 7;4 8]',
    'sm = get_matrix(daf, "cell", "gene", "sm")',
    '@assert size(sm) == (4,2)',
    '@assert nnz(sm) == 3',
    'println("JULIA_OK")')
  out <- run_julia(script)
  expect_true(any(grepl("JULIA_OK", out)),
              info = paste(out, collapse = "\n"))
})

test_that("R -> Julia copy -> R preserves dense + sparse bytes", {
  skip_if_not(.have_julia_env())
  dir_src <- new_tempdir()
  dir_dst <- new_tempdir()
  d <- files_daf(dir_src, mode = "w+")
  add_axis(d, "cell", sprintf("c%d", 1:50))
  add_axis(d, "gene", sprintf("g%d", 1:10))
  # Sparsity-threshold crossing vector
  v <- numeric(50); v[c(5, 25, 45)] <- c(1, 2, 3)
  set_vector(d, "cell", "x", v)
  # Dense matrix
  set_matrix(d, "cell", "gene", "dm", matrix(seq_len(500), nrow=50))
  # Sparse matrix
  sp <- Matrix::rsparsematrix(50, 10, density = 0.05)
  set_matrix(d, "cell", "gene", "sm", sp)

  script <- c(
    'using DataAxesFormats',
    sprintf('src = FilesDaf(raw"%s", "r")', dir_src),
    sprintf('dst = FilesDaf(raw"%s", "w")', dir_dst),
    'for axis in axes_set(src); add_axis!(dst, axis, axis_array(src, axis)); end',
    'for name in scalars_set(src); set_scalar!(dst, name, get_scalar(src, name)); end',
    'for axis in axes_set(src)',
    '  for name in vectors_set(src, axis)',
    '    set_vector!(dst, axis, name, get_vector(src, axis, name))',
    '  end',
    'end',
    'for rows in axes_set(src), cols in axes_set(src)',
    '  for name in matrices_set(src, rows, cols)',
    '    set_matrix!(dst, rows, cols, name, get_matrix(src, rows, cols, name))',
    '  end',
    'end',
    'println("COPY_OK")')
  out <- run_julia(script)
  expect_true(any(grepl("COPY_OK", out)),
              info = paste(out, collapse = "\n"))

  d2 <- files_daf(dir_dst, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "x")), v)
  expect_equal(unname(get_matrix(d2, "cell", "gene", "dm")),
               matrix(seq_len(500), nrow = 50))
  rt_sp <- get_matrix(d2, "cell", "gene", "sm")
  expect_s4_class(rt_sp, "dgCMatrix")
  expect_equal(as.matrix(unname(rt_sp)), as.matrix(sp))
})
```

- [ ] **Step 2: Run**

```
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "julia-compat")'
```

Expected: both tests pass (or skip cleanly if the env is missing).

- [ ] **Step 3: Commit**

```bash
cd ~/src/dafr-native
git add tests/testthat/test-files-julia-compat.R
git commit -m "test(files_daf): live R->Julia->R bidirectional round-trip"
```

---

## Phase L — Resolve spec + upstream PR

### Task L1: Resolve three `[UNCLEAR]` markers in the spec draft

**Files:**
- Modify: `dev/specs/filesdaf-on-disk-spec-draft.md`

- [ ] **Step 1: Edit the draft**

1. §4 Scalars: replace the `[UNCLEAR: Float32 JSON precision …]` block with:

   > **Float32 precision and JSON.** A writer MUST emit `"Float64"` as the canonical type for IEEE 754 binary64 scalars. `"Float32"` scalars written via `JSON.Writer.print` will be formatted with up to 7 significant decimal digits and may round-trip lossily depending on the library; this imprecision affects only scalars stored as `"Float32"`. Binary payloads (vector `.data`, matrix `.data`, sparse `.nzval`) store the exact IEEE representation and are unaffected. Readers MAY coerce Float32 to the native wider float type of the host language (e.g., `double` in R/C/Python) since IEEE widening is exact.

2. §11 Atomicity: replace `[UNCLEAR: whether the module-level docstring's claim …]` with:

   > **Single-writer contract.** The FilesDaf format provides no filesystem-level atomicity. A writer MUST complete every file write associated with a property (descriptor + payload) before any reader opens the store, and only one writer may touch a given store at a time. Multi-process or multi-threaded concurrent writers require external coordination (e.g., a filesystem lock on the root directory). Readers MAY observe a partially-written store after a crash mid-write; recovery is by restoration from backup.

3. Appendix: replace the `[UNCLEAR: should files written with "Int" …]` with:

   > **`Int` / `int` alias.** Writers SHOULD NOT emit `"Int"` or `"int"`; both Julia (where `Int == Int64` on 64-bit systems) and the R/native implementation produce explicit-width names (`Int32`, `Int64`). Readers encountering `"Int"`/`"int"` MUST deserialize as 64-bit signed integers for compatibility with hand-written stores or legacy pre-spec writers.

- [ ] **Step 2: Commit (dev repo)**

```bash
cd ~/src/dafr-native/dev
git add specs/filesdaf-on-disk-spec-draft.md
git commit -m "spec: resolve three UNCLEAR markers in FilesDaf draft"
```

---

### Task L2: Open upstream PR against `tanaylab/DataAxesFormats.jl`

**Files:**
- New branch in `~/src/DataAxesFormats.jl`.

- [ ] **Step 1: Confirm user has authorized the PR.** Ask:

  > "Ready to open the upstream FilesDaf on-disk spec PR against tanaylab/DataAxesFormats.jl with the settled draft? It will add `docs/src/file_specs/filesdaf-on-disk.md` to the Julia repo and is non-destructive."

  Wait for explicit "yes" before proceeding. If "no" or ambiguous, mark the task complete-without-PR and leave a note in `dev/notes/slice-2-exit.md`.

- [ ] **Step 2 (if authorized): Copy spec to Julia repo**

```bash
cd ~/src/DataAxesFormats.jl
git checkout -b docs/filesdaf-on-disk-spec
mkdir -p docs/src/file_specs
/bin/cp ~/src/dafr-native/dev/specs/filesdaf-on-disk-spec-draft.md \
        docs/src/file_specs/filesdaf-on-disk.md
# Strip the "Draft for upstream review" status line and line citations
# that don't make sense inside the Julia repo itself — the citations
# still make sense because the file layout is identical. Keep them.
```

- [ ] **Step 3: Register doc in `docs/make.jl`** if Documenter is used (check existing usage). Add:

```julia
"FilesDaf On-Disk Format" => "file_specs/filesdaf-on-disk.md",
```

- [ ] **Step 4: Commit + push + open PR**

```bash
cd ~/src/DataAxesFormats.jl
git add docs/src/file_specs/filesdaf-on-disk.md docs/make.jl
git commit -m "docs: FilesDaf on-disk format specification"
git push -u origin docs/filesdaf-on-disk-spec
gh pr create --title "docs: FilesDaf on-disk format specification" --body "$(cat <<'EOF'
## Summary
Adds a reference specification for the FilesDaf on-disk format, extracted
from `src/files_format.jl`. The spec documents the directory layout,
`daf.json` schema, scalar JSON encoding, axis / vector / matrix binary
layout (little-endian, column-major dense, 1-based CSC sparse), and the
atomicity/concurrency contract (single-writer, no fsync).

Companion to the native R reimplementation at
https://github.com/tanaylab/dafr which follows this spec for bidirectional
read/write compatibility.

## Test plan
- [ ] Docs build passes (Documenter)
- [ ] Spec accurately cites `files_format.jl` line ranges
- [ ] R reference implementation round-trips a Julia-written fixture (validated locally)
EOF
)"
```

- [ ] **Step 5: Record PR URL** in the draft:

Edit `dev/specs/filesdaf-on-disk-spec-draft.md` header:

```
**Upstream PR:** <pasted URL>
```

- [ ] **Step 6: Commit (dev repo)**

```bash
cd ~/src/dafr-native/dev
git add specs/filesdaf-on-disk-spec-draft.md
git commit -m "spec: add upstream PR URL to draft"
```

---

## Phase M — Slice 2 exit gate

### Task M1: Full R CMD check

**Files:**
- None (verification only).

- [ ] **Step 1: Run check**

```
cd /home/aviezerl/src/dafr-native
_R_CHECK_SYSTEM_CLOCK_=0 Rscript -e 'devtools::check(error_on = "note", manual = FALSE, vignettes = FALSE)'
```

Expected: 0 ERROR, 0 WARNING, 0 NOTE.

- [ ] **Step 2: Run full test suite**

```
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: all PASS, 0 FAIL, 0 SKIP beyond the Julia-fixture skips when fixture absent.

- [ ] **Step 3: Commit any final tweaks** (docs regeneration, DESCRIPTION Imports update, NAMESPACE).

---

### Task M2: Slice 2 exit note

**Files:**
- Create: `dev/notes/slice-2-exit.md`

- [ ] **Step 1: Write the exit note** covering:

- Deliverables checkboxes (FilesDaf class, 22 format methods, mmap read, readBin fallback, mapped-tier caching with invalidation, sparse read/write round-trip, relayout, delete cascade, Julia-fixture read, MemoryDaf↔FilesDaf round-trip, spec resolution + upstream PR).
- Test/check/build status (pass counts, platform coverage).
- Closed Slice-1 drive-by items (A1–A6).
- Deferred items:
  - `@family` roxygen + top-level `?dafr` page.
  - `dafr.omp_threshold` wiring into kernels.
  - Sparse vector write path (dense-only in R v1).
  - Long-vector (>2^31) ALTREP scenarios.
  - File-truncated-during-read ALTREP scenarios.
  - CSC colSums bake-off re-run at 100M+ nnz.
  - Transpose kernel B-vs-D decision.
  - `close_daf()` / explicit store lifecycle API (mmap regions are ref-counted; LRU-exempt mapped tier keeps them alive; user can `empty_cache(clear="mapped")`).
- Decision to enter Slice 3.

- [ ] **Step 2: Commit (dev repo)**

```bash
cd ~/src/dafr-native/dev
git add notes/slice-2-exit.md
git commit -m "docs: Slice 2 exit note"
```

---

### Task M3: Tag slice-2

- [ ] **Step 1: Merge feature branch into main** (if using a feature branch; otherwise skip). Fast-forward preferred.

- [ ] **Step 2: Tag**

```bash
cd ~/src/dafr-native
git tag -a slice-2 -m "Slice 2: FilesDaf backend with mmap + Julia bidirectional compat"
git push origin main --tags
```

- [ ] **Step 3: Confirm CI green** on GitHub Actions.

- [ ] **Step 4: Close out** — announce completion, offer the Slice 3 planning prompt.

---

## Self-review (writing-plans skill checklist)

### 1. Spec coverage

Walking the kickoff breadcrumb's "What Slice 2 should deliver" list against tasks:

| Spec item                                         | Task(s)                          |
|---------------------------------------------------|----------------------------------|
| 1. FilesDaf class + `files_daf(path, mode, name)` | B1, B2                           |
| 2. On-disk format byte-compat with Julia          | C2, C3, D1, E1–2, F1–F5, G1, H1–H4, I1–I3 |
| 3. Mmap read path via ALTREP                      | F2, H2 (+ J1 cache)              |
| 4. `readBin` fallback when `dafr.mmap = FALSE`    | F2, J2                           |
| 5. Bidirectional write path (dense + sparse)      | F3, F5, H3, I2 (+ pre-planning §3) |
| 6. Cache mapped-tier integration + invalidation   | J1                               |
| 7. Upstream the on-disk spec (G2 PR)              | L1, L2                           |
| 8. MemoryDaf ↔ FilesDaf round-trip                | K1, K2                           |
| 9. Live R ↔ Julia bidirectional round-trip        | K1 (fixture), K3 (live via conda) |
| 10. Close Slice-1 tracked-non-blocking drive-bys  | A1–A6                            |

All ten items have corresponding tasks. (Was nine in the pre-revision
draft; adaptive sparsification + live Julia integration are the two new
items, both required for true bidirectional byte-equivalence.)

### 2. Placeholder scan

- No `TBD` / `TODO` / "fill in later" markers in task bodies.
- Every step has either concrete code, a concrete command, or a concrete commit message.
- Julia-fixture tests (K1) expect the committed fixture to exist — no `skip_if_not(dir.exists)` guard; the K3 live tests gate on `.have_julia_env()` so CI without Julia still passes cleanly.

### 3. Type consistency

- `format_get_vector` / `format_set_vector` signatures use `axis, name`; helpers `.files_get_vector_*` / `.files_write_vector_*` use the same names.
- `format_get_matrix` / helpers use `rows_axis`, `cols_axis`, `name`; after Phase A1 rename, user-facing wrappers + S7 method signatures use the canonical `columns_axis`. Internal helpers keep the shorter `cols_axis` for brevity — the boundary is the S7 method signature.
- `desc` list carries `format`, `eltype`, `indtype` fields — used identically in G1, I1, F5.
- `.dtype_for_r_vector`, `.dtype_size`, `.dtype_canonical`, `.indtype_for_size`, `.should_sparsify_numeric`, `.should_sparsify_string` are referenced with the same names across phases.
- `FilesDafReadOnly` parallel methods shadow `FilesDaf` for all read generics — consistent.
- `sparseVector` input handled only in F5; MemoryDaf densifies at validation (symmetric public API).

### 4. Known risks / callouts for the executor

- **Phase E2 cascade test** references `set_vector` + `set_matrix` on FilesDaf, which aren't implemented until F3 + H3. The plan explicitly defers the cascade test to Task I3, which is the first point where both writers exist.
- **`.validate_vector_value` and `.validate_matrix_value`** currently live in `R/memory_daf.R`. FilesDaf reuses them. Hoist to `R/utils.R` during D1 / F3 / H3 as those tasks need them. Update `R/memory_daf.R` call sites to use the moved location.
- **`.validate_vector_value` change for sparseVector**: the MemoryDaf path densifies `sparseVector` inputs at validation time (`as.numeric(as.vector(sv))`) so MemoryDaf stores atomic vectors; FilesDaf takes the sparseVector branch before validation. Keep this asymmetry behind the helper so public `set_vector` semantics stay uniform.
- **`jsonlite::toJSON(bit64::integer64)` mangling** — handled in C2 by hand-formatting Int64 scalars. Do not revert to `jsonlite::toJSON(auto_unbox=TRUE)` for Int64.
- **`readBin(size=8L, signed=FALSE)` is not portable** — R's readBin rejects 8-byte unsigned. We always read Int64/UInt64 as signed int64 via `bit64::as.integer64`. UInt64 values ≥ 2^63 overflow to negative; documented as v1 limitation.
- **Windows path separator** — `normalizePath(path, winslash = "/")` returns forward slashes on Windows. All `file.path` usage is cross-platform; no hardcoded `/` in path construction.
- **`dafr.mmap = FALSE` + oversized Int32 matrix slot** — `.read_bin_dense` UInt32 branch reads as signed int32; values ≥ 2^31 overflow. Documented as v1 limitation; Slice 3 long-vector work resolves.
- **Adaptive sparsification cost** (F5): every numeric / Bool `set_vector` performs an O(N) zero-scan. Same order as writing the payload. Documented in `?set_vector`.
- **Julia conda env name is `dafr-mcview`** (not `mcview-dafr`). Tasks K1 and K3 use `conda run -n dafr-mcview julia …`. Verified 2026-04-20: Julia 1.12.5 + DataAxesFormats 0.2.0.
- **K3 test flakiness risk** — the live Julia round-trip spawns a Julia process per assertion; startup latency ~5s. Tests skip when `.have_julia_env()` is false so CI images without Julia aren't blocked. Local runs hit both tests.

---

## Execution handoff

Plan complete and saved to `dev/plans/2026-04-20-slice-2-files-daf.md`. Two execution options:

1. **Subagent-Driven (recommended)** — dispatch a fresh subagent per task, two-stage review (spec + quality) between tasks. Fast iteration.
2. **Inline Execution** — execute tasks in this session with batched checkpoints per phase.

The user's kickoff message specifies subagent-driven-development. Proceed with option 1.
