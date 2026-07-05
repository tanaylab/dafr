# ZipDaf backend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `ZipDaf` backend - a whole Daf store in a single append-only `.daf.zip` archive, read + write, byte-compatible with `DataAxesFormats.jl` 0.3.0.

**Architecture:** ZipDaf reuses two things dafr already has: the append-only mmap-backed `MmapZipStore` (C++, used for `.daf.zarr.zip`) as the container, and FilesDaf's byte-identical serialization. We introduce a small **byte-store backend seam** so FilesDaf's existing `.files_*` orchestration helpers become container-agnostic (`DirBackend` = current filesystem behavior, `ZipBackend` = MmapZipStore). ZipDaf registers the *same* helpers as its format methods, differing only in the backend it carries. This mirrors how Julia factors shared logic into `PackedFormat`.

**Tech Stack:** R, S7 classes, cpp11 (existing `dafr_mmap_zip_*` entry points), `Matrix`, `bit64`, `jsonlite`, testthat.

**Test invocation (per project convention):** `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/<file>.R")'`. Do NOT use the installed package (it is stale). Count both `failed` and `error`.

---

## REVISION (2026-07-05, during execution): hybrid, not full backend-refactor

After reading `files_daf_write.R` in full, the original "route ALL FilesDaf I/O
through a backend" plan (Tasks A3/A4) is the wrong trade. FilesDaf's **write**
orchestration is deeply entangled with filesystem-only concerns ZipDaf does not
have: `metadata.json` rebuild/append on every setter, eager directory creation
in `format_add_axis`, unlink-before-write, and a hardlink-based reorder engine.
Disentangling those from mature, byte-parity-critical code is high-risk for
little gain.

**Revised approach (hybrid):**
- **Keep A1/A2** - extract the *pure serialization cores* (dense/scalar/
  descriptor/lines encode+decode) into `files_io.R`; FilesDaf's leaf helpers
  delegate to them (trivial, single-source, guarded by the existing suite).
- **Drop A3/A4** (the backend object + full FilesDaf routing).
- **ZipDaf gets its own thin orchestration** (`R/zip_daf.R`) that calls the
  shared cores + `store_get_bytes/set_bytes/exists/list`. It parallels the
  `.files_get_*`/`.files_write_*` *structure* but is simpler: no `metadata.json`,
  no eager dirs, no mmap, no unlink, no reorder. Append-only: delete / overwrite
  / relayout / reorder raise a clean error.

What's shared (byte-parity lives here, so parity fixes propagate): `.encode_dense`
/`.decode_dense`, `.encode_scalar_json`/`.decode_scalar_json`,
`.decode_descriptor_bytes` + descriptor byte-builders, `.encode_lines`/
`.decode_lines`, `.files_parse_sparse_descriptor`, `.files_packed_decode_vector`
/`_matrix`, `.shard_assemble`, `.should_sparsify_*`, `.indtype_for_size`, the
dtype helpers.

What's duplicated (structure only, simpler for zip): the read branch
(dense/sparse/string -> assemble vector/`dgCMatrix`/`lgCMatrix`) and the write
branch (sparsify decision -> descriptor + components). FilesDaf's orchestration
structure is stable; the frequent parity fixes are in serialization, which is
shared.

Revised task list: **A1, A2** (cores, unchanged below) -> **B1** ZipDaf classes +
store accessor -> **B2** read methods -> **B3** write methods (append-only) ->
**B4** `zip_daf()` + `open_daf` dispatch -> **B5** round-trip tests -> **C1**
Julia interop -> **C2** docs/NEWS/version. The B-task code is written TDD-first
during execution (paralleling the FilesDaf helpers just read), so the detailed
listings in the original Phase B below are superseded by this structure.

---

## Phase A - Backend seam (behavior-preserving refactor)

The existing FilesDaf test suite (`test-files-*.R`, `test-files-julia-compat.R`, `test-files-packed-*.R`) is the regression guard for all of Phase A. No test should change in Phase A; if one does, the refactor changed behavior and is wrong.

### Task A1: Split dense encode/decode from file I/O

**Files:**
- Modify: `R/files_io.R` (`.write_bin_dense`, `.read_bin_dense`)
- Test: `tests/testthat/test-files-io.R` (existing suite is the guard)

- [ ] **Step 1: Add pure raw-bytes cores, keep path helpers as wrappers**

In `R/files_io.R`, replace the bodies of `.write_bin_dense` / `.read_bin_dense` so the dtype `switch` operates on a connection that can be either a file or a raw vector. Add two pure cores and make the path helpers delegate:

```r
# Pure: encode a dense R vector to little-endian raw bytes (no file I/O).
.encode_dense <- function(value, dtype) {
    con <- rawConnection(raw(0L), "wb")
    on.exit(close(con), add = TRUE)
    .write_dense_con(con, value, dtype)
    rawConnectionValue(con)
}
# Pure: decode little-endian raw bytes to a dense R vector (no file I/O).
.decode_dense <- function(bytes, n, dtype) {
    con <- rawConnection(bytes, "rb")
    on.exit(close(con), add = TRUE)
    .read_dense_con(con, n, dtype)
}
```

Move the existing `switch(dtype, ...)` write body verbatim into a new `.write_dense_con(con, value, dtype)` (the current `.write_bin_dense` body minus the `file()`/`on.exit` lines), and the existing read `switch` into `.read_dense_con(con, n, dtype)`. Then:

```r
.write_bin_dense <- function(path, value, dtype) {
    con <- file(path, open = "wb"); on.exit(close(con), add = TRUE)
    .write_dense_con(con, value, dtype)
    invisible()
}
.read_bin_dense <- function(path, n, dtype) {
    con <- file(path, open = "rb"); on.exit(close(con), add = TRUE)
    .read_dense_con(con, n, dtype)
}
```

- [ ] **Step 2: Run the FilesDaf suite to verify no behavior change**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-files-io.R"); testthat::test_file("tests/testthat/test-files-vectors.R"); testthat::test_file("tests/testthat/test-files-matrices.R")'`
Expected: all PASS, 0 failed / 0 error (identical to pre-change).

- [ ] **Step 3: Commit**

```bash
git add R/files_io.R
git commit -m "refactor(files-io): split dense encode/decode from file I/O"
```

### Task A2: Byte cores for scalar-json, descriptor, and lines

**Files:**
- Modify: `R/files_io.R` (`.write_scalar_json`, `.read_scalar_json`, `.read_descriptor`), `R/files_daf_read.R` (`.files_axis_parsed` lines read)
- Test: existing suite is the guard

- [ ] **Step 1: Add raw-bytes cores for the remaining serializers**

`.read_scalar_json(path)` currently does `readChar(path, file.size(path))`. Add `.decode_scalar_json(bytes)` that takes a raw vector: `raw_txt <- rawToChar(bytes)` then the existing regex/JSON body. Make `.read_scalar_json(path)` call `.decode_scalar_json(readBin(path, "raw", file.size(path)))`. Add `.encode_scalar_json(value)` returning `charToRaw(<the JSON string .write_scalar_json builds>)`; refactor `.write_scalar_json(path, value)` to `writeBin(.encode_scalar_json(value), path)` (extract the JSON-string construction into a shared `.scalar_json_string(value)` used by both).

Do the same for the descriptor: add `.decode_descriptor_bytes(bytes)` (`jsonlite::fromJSON(rawToChar(bytes), ...)` matching the current `.read_descriptor` parse) and have `.read_descriptor(path)` delegate. For axis lines, add `.decode_lines(bytes)` = `strsplit(rawToChar(bytes), "\n", fixed = TRUE)[[1]]` filtered of a trailing empty (match the current `readLines`/axis parse semantics exactly - check `.files_axis_parsed`) and `.encode_lines(entries)` = `charToRaw(paste0(paste(entries, collapse = "\n"), "\n"))` (match how `.files_add_axis` writes axes; verify against a written fixture that the trailing newline matches Julia).

- [ ] **Step 2: Run scalar/axis suites**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); for (f in c("test-files-io","test-query-scalar-default","test-files-format-1_1")) testthat::test_file(sprintf("tests/testthat/%s.R", f))'`
Expected: all PASS, unchanged.

- [ ] **Step 3: Commit**

```bash
git add R/files_io.R R/files_daf_read.R
git commit -m "refactor(files-io): add raw-bytes cores for scalar/descriptor/lines"
```

### Task A3: Define the backend interface + DirBackend

**Files:**
- Create: `R/files_backend.R`
- Modify: `R/dafr-package.R` or the `@include` roxygen chain if collation matters (add `#' @include files_backend.R` where `files_io.R` is included)
- Test: `tests/testthat/test-files-backend.R` (new)

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-files-backend.R
test_that("DirBackend round-trips bytes, exists, and lists by prefix", {
    dir <- tempfile("dirbe-"); dir.create(dir)
    be <- .dir_backend(dir)
    be$write("scalars/pi.json", charToRaw("3.14"))
    be$write("vectors/cell/x.data", writeBin(as.double(1:3), raw()))
    expect_true(be$exists("scalars/pi.json"))
    expect_false(be$exists("scalars/nope.json"))
    expect_equal(rawToChar(be$read("scalars/pi.json")), "3.14")
    expect_equal(readBin(be$read("vectors/cell/x.data"), "double", 3), c(1, 2, 3))
    expect_setequal(be$list("scalars"), "scalars/pi.json")
    expect_setequal(be$list("vectors/cell"), "vectors/cell/x.data")
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-files-backend.R")'`
Expected: FAIL - `.dir_backend` not found.

- [ ] **Step 3: Implement DirBackend**

```r
# R/files_backend.R
#' @include files_io.R
NULL

# A byte-store backend is a plain list of closures over a container:
#   read(key)  -> raw     (error if absent)
#   write(key, raw_bytes) -> invisible; creates parent "dirs"/entries as needed
#   exists(key) -> logical
#   list(prefix) -> character keys under prefix (recursive), full-key form
#   delete(key) -> invisible (DirBackend only; ZipBackend raises append-only)
# Keys are always forward-slash relative paths (e.g. "vectors/cell/x.data").

.dir_backend <- function(root) {
    key_path <- function(key) file.path(root, key)
    list(
        kind   = "dir",
        root   = root,
        read   = function(key) readBin(key_path(key), "raw", file.size(key_path(key))),
        write  = function(key, bytes) {
            p <- key_path(key)
            d <- dirname(p)
            if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
            writeBin(bytes, p)
            invisible()
        },
        exists = function(key) file.exists(key_path(key)),
        list   = function(prefix) {
            base <- key_path(prefix)
            if (!dir.exists(base)) return(character(0L))
            fs <- list.files(base, recursive = TRUE, full.names = FALSE)
            if (!length(fs)) return(character(0L))
            paste0(prefix, "/", fs)
        },
        delete = function(key) { unlink(key_path(key), force = TRUE); invisible() }
    )
}
```

- [ ] **Step 4: Run to verify pass**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-files-backend.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/files_backend.R tests/testthat/test-files-backend.R
git commit -m "feat(files-backend): byte-store backend interface + DirBackend"
```

### Task A4: Route FilesDaf helpers through the backend

**Files:**
- Modify: `R/files_daf_read.R`, `R/files_daf_write.R`, `R/files_io.R` (path builders → key builders)
- Modify: `R/files_daf.R` (add `.files_backend` dispatch)
- Test: full FilesDaf suite is the guard

- [ ] **Step 1: Add `.files_backend` accessor**

In `R/files_daf.R`, add (FilesDaf carries a path; the backend is derived, cached on `internal`):

```r
# Backend for a files-family daf. FilesDaf -> DirBackend over its path.
# ZipDaf overrides this (Phase B) to return a ZipBackend over its store.
.files_backend <- function(daf) {
    internal <- S7::prop(daf, "internal")
    be <- internal$backend
    if (is.null(be)) { be <- .dir_backend(internal$path); internal$backend <- be }
    be
}
```

- [ ] **Step 2: Convert path builders to key builders and route leaf I/O**

Apply this exact mechanical transform to every `.files_*` orchestration helper in `files_daf_read.R` and `files_daf_write.R`:

1. Replace `root <- .files_root(daf)` / `.path_vector_dir(root, axis)` / `file.path(dir, paste0(name, ext))` with **relative key strings**: `sprintf("scalars/%s.json", name)`, `sprintf("axes/%s.txt", axis)`, `sprintf("vectors/%s/%s%s", axis, name, ext)`, `sprintf("matrices/%s/%s/%s%s", rows_axis, columns_axis, name, ext)`. Add key builders next to the existing path builders in `files_io.R`:

```r
.key_scalar     <- function(name)        sprintf("scalars/%s.json", name)
.key_axis       <- function(axis)        sprintf("axes/%s.txt", axis)
.key_vector     <- function(axis, name, ext = ".json") sprintf("vectors/%s/%s%s", axis, name, ext)
.key_matrix     <- function(ra, ca, name, ext = ".json") sprintf("matrices/%s/%s/%s%s", ra, ca, name, ext)
.key_vectors_dir<- function(axis)        sprintf("vectors/%s", axis)
.key_matrix_dir <- function(ra, ca)      sprintf("matrices/%s/%s", ra, ca)
```

2. Replace leaf I/O with backend calls (`be <- .files_backend(daf)`):
   - `file.exists(path)` -> `be$exists(key)`
   - `.read_bin_dense(path, n, dtype)` -> `.decode_dense(be$read(key), n, dtype)`
   - `.write_bin_dense(path, value, dtype)` -> `be$write(key, .encode_dense(value, dtype))`
   - `.read_scalar_json(path)` -> `.decode_scalar_json(be$read(key))`
   - `.write_scalar_json(path, value)` -> `be$write(key, .encode_scalar_json(value))`
   - `.read_descriptor(path)` -> `.decode_descriptor_bytes(be$read(key))`
   - `.write_descriptor_*(path, ...)` -> build the raw as those helpers do, then `be$write(key, raw)` (extract each `.write_descriptor_*` into a `.descriptor_*_bytes(...)` pure core the same way as A1/A2)
   - axis lines read -> `.decode_lines(be$read(.key_axis(axis)))`; write -> `be$write(.key_axis(axis), .encode_lines(entries))`
   - `list.files(dir, ...)` / `get_names_set(dir, ".json")` for `*_set` -> `be$list(prefix)` filtered to `endsWith(k, ".json")`, then strip prefix + `.json` to names.
   - sparse component read (`.files_read_sparse_component(dir, name, comp, comp_desc, count, type)`): keep the packed branch (`.files_read_packed_vector` reads the `.zip` shard bytes - for DirBackend it is `be$read(.key_vector(axis, name, paste0(".", comp, ".zip")))`); flat branch -> `.decode_dense(be$read(.key_vector(axis, name, paste0(".", comp))), count, type)`.

**Worked example** - `.files_get_scalar` (currently reads `file.path(root, "scalars", name.json)`):

```r
.files_get_scalar <- function(daf, name) {
    be <- .files_backend(daf)
    key <- .key_scalar(name)
    if (!be$exists(key)) .require_scalar(daf, name)   # keep existing error path
    .decode_scalar_json(be$read(key))
}
```

Apply the same shape to: `.files_scalars_set`, `.files_axis_parsed`, `.files_axes_set`, `.files_vectors_set`, `.files_get_vector_dense`, `.files_get_vector_dense_string`, `.files_get_vector_sparse`, `.files_matrices_set`, `.files_get_matrix_dense`, `.files_get_matrix_dense_string`, `.files_get_matrix_sparse`, and the write-side `.files_set_scalar`/`.files_add_axis`/`.files_set_vector`/`.files_set_matrix` (and the packed writers - they call `.files_write_component(base, ext, ...)`; give it a backend-aware `base = key-stem` variant that ends in `be$write(key, bytes)`).

Note: the reorder/recovery machinery (`.files_daf_recover_reorder`, `.metadata_json_*`) stays **filesystem-only** and is NOT called for ZipDaf (Phase B skips it). Leave those helpers as direct fs code.

- [ ] **Step 3: Run the FULL FilesDaf suite**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); for (f in list.files("tests/testthat", pattern="^test-files", full.names=TRUE)) testthat::test_file(f)'`
Expected: all PASS, 0 failed / 0 error. This proves the DirBackend routing is byte-identical.

- [ ] **Step 4: Commit**

```bash
git add R/files_daf_read.R R/files_daf_write.R R/files_io.R R/files_daf.R
git commit -m "refactor(files): route FilesDaf I/O through the byte-store backend"
```

---

## Phase B - ZipDaf backend

### Task B1: ZipBackend over MmapZipStore

**Files:**
- Modify: `R/files_backend.R`
- Test: `tests/testthat/test-files-backend.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("ZipBackend round-trips bytes over MmapZipStore and raises append-only on overwrite", {
    p <- tempfile(fileext = ".daf.zip")
    store <- new_mmap_zip_store(p, mode = "w")
    be <- .zip_backend(store)
    be$write("scalars/pi.json", charToRaw("3.14"))
    be$write("vectors/cell/x.data", writeBin(as.double(1:3), raw()))
    expect_true(be$exists("scalars/pi.json"))
    expect_equal(readBin(be$read("vectors/cell/x.data"), "double", 3), c(1, 2, 3))
    expect_setequal(be$list("vectors/cell"), "vectors/cell/x.data")
    expect_error(be$write("scalars/pi.json", charToRaw("2.72")), "append-only")
    expect_error(be$delete("scalars/pi.json"), "append-only")
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-files-backend.R")'`
Expected: FAIL - `.zip_backend` not found.

- [ ] **Step 3: Implement ZipBackend**

```r
# in R/files_backend.R
.zip_backend <- function(store) {
    list(
        kind   = "zip",
        store  = store,
        read   = function(key) store_get_bytes(store, key),
        write  = function(key, bytes) store_set_bytes(store, key, bytes),   # store raises on overwrite
        exists = function(key) store_exists(store, key),
        list   = function(prefix) store_list(store, prefix),
        delete = function(key) store_delete(store, key)                     # store raises: append-only
    )
}
```

(`store_list(store, prefix)` already returns full keys under the prefix - confirmed by smoke test. If it returns keys WITHOUT the prefix-boundary you expect, normalize here.)

- [ ] **Step 4: Run to verify pass**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-files-backend.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/files_backend.R tests/testthat/test-files-backend.R
git commit -m "feat(files-backend): ZipBackend over MmapZipStore"
```

### Task B2: ZipDaf classes + format methods (reuse FilesDaf helpers)

**Files:**
- Create: `R/zip_daf.R`
- Test: `tests/testthat/test-zip-daf.R` (new; written in B4)

- [ ] **Step 1: Define the classes, backend override, and method registrations**

```r
# R/zip_daf.R
#' @include files_daf.R files_daf_read.R files_daf_write.R files_backend.R format_api.R
NULL

#' @export
ZipDaf <- S7::new_class(name = "ZipDaf", package = "dafr", parent = DafWriter)
#' @export
ZipDafReadOnly <- S7::new_class(name = "ZipDafReadOnly", package = "dafr", parent = DafReadOnly)

# ZipDaf carries a MmapZipStore; its backend is a ZipBackend (cached on internal).
S7::method(.files_backend_dispatch, ZipDaf) <- function(daf) .zip_backend_cached(daf)
S7::method(.files_backend_dispatch, ZipDafReadOnly) <- function(daf) .zip_backend_cached(daf)
.zip_backend_cached <- function(daf) {
    internal <- S7::prop(daf, "internal")
    be <- internal$backend
    if (is.null(be)) { be <- .zip_backend(internal$store); internal$backend <- be }
    be
}
```

Change `.files_backend` (Task A4) to dispatch: make it a small generic so ZipDaf can override without editing FilesDaf's branch. Simplest: define `.files_backend_dispatch <- S7::new_generic(...)` with a FilesDaf/FilesDafReadOnly method returning `.dir_backend(internal$path)` (cached) and ZipDaf methods as above; `.files_backend(daf)` calls the generic. Update Task A4's `.files_backend` accordingly if not already generic.

Register every read + set-list format method for `ZipDaf`/`ZipDafReadOnly` to the SAME `.files_*` helper FilesDaf uses. Example (repeat the pattern for each generic):

```r
S7::method(format_has_scalar, list(ZipDaf, S7::class_character)) <-
    function(daf, name) .files_backend(daf)$exists(.key_scalar(name))
S7::method(format_has_scalar, list(ZipDafReadOnly, S7::class_character)) <-
    function(daf, name) .files_backend(daf)$exists(.key_scalar(name))
S7::method(format_get_scalar, list(ZipDaf, S7::class_character)) <-
    function(daf, name) .files_get_scalar(daf, name)
S7::method(format_get_scalar, list(ZipDafReadOnly, S7::class_character)) <-
    function(daf, name) .files_get_scalar(daf, name)
# ... scalars_set, has/axes_set/axis_array/axis_length/axis_dict,
#     has/get_vector, vectors_set, has/get_matrix, matrices_set, relayout_matrix,
#     description_header, .is_leaf_dispatch (TRUE) - all delegate to the
#     .files_* helper or backend call, for BOTH ZipDaf and ZipDafReadOnly.
```

The fastest correct way: enumerate the FilesDaf `S7::method(<generic>, FilesDaf*)` registrations and duplicate each for `ZipDaf*`, calling the identical helper body. (They already delegate to `.files_*`, so the bodies are one-liners.)

- [ ] **Step 2: Write mutating + append-only methods**

Writable `format_set_scalar/_set_vector/_set_matrix/_add_axis` for `ZipDaf` delegate to the same `.files_set_*` helpers (now backend-routed). `format_delete_*` and `format_relayout_matrix` for `ZipDaf` raise the append-only error; all mutating methods for `ZipDafReadOnly` raise the read-only guard:

```r
.zip_append_only_guard <- function(verb)
    stop(sprintf("zip_daf: archive is append-only; %s not permitted", verb), call. = FALSE)

S7::method(format_delete_scalar, list(ZipDaf, S7::class_character, S7::class_logical)) <-
    function(daf, name, must_exist) .zip_append_only_guard(sprintf("delete_scalar(%s)", name))
# ... delete_axis / delete_vector / delete_matrix / relayout_matrix likewise.
# ZipDafReadOnly mutating methods: reuse a read-only guard identical to FilesDafReadOnly's.
```

Note: confirm whether dafr's high-level `set_scalar(..., overwrite=TRUE)` routes through `format_delete_scalar` first. If it does, overwrite on a ZipDaf raises the append-only error via the delete method - the intended behavior (matches Julia). Add a test in B4.

- [ ] **Step 3: Description header**

```r
.zip_daf_description_header <- function(daf, indent) {
    internal <- S7::prop(daf, "internal")
    c(paste0(indent, "type: ZipDaf"),
      paste0(indent, "path: ", internal$path),
      paste0(indent, "mode: ", internal$mode))
}
S7::method(format_description_header, ZipDaf) <- function(daf, indent = "", deep = FALSE)
    .zip_daf_description_header(daf, indent)
S7::method(format_description_header, ZipDafReadOnly) <- function(daf, indent = "", deep = FALSE)
    .zip_daf_description_header(daf, indent)
S7::method(.is_leaf_dispatch, ZipDaf) <- function(daf) TRUE
S7::method(.is_leaf_dispatch, ZipDafReadOnly) <- function(daf) TRUE
```

- [ ] **Step 4: Commit (compiles; behavior tested in B4)**

```bash
git add R/zip_daf.R R/files_daf.R
git commit -m "feat(zip-daf): ZipDaf classes + format methods over ZipBackend"
```

### Task B3: `zip_daf()` constructor + `open_daf` dispatch

**Files:**
- Modify: `R/zip_daf.R`
- Modify: `R/open_daf.R`
- Test: `tests/testthat/test-zip-daf.R` (B4), `tests/testthat/test-open-daf.R`

- [ ] **Step 1: Constructor**

```r
#' Single-file (zip) Daf store.
#'
#' A `Daf` store held in one append-only `.daf.zip` archive, byte-compatible
#' with Julia's `DataAxesFormats.ZipDaf`. Same on-disk layout as [files_daf()]
#' but inside a ZIP (the ZIP central directory replaces `metadata.json`). The
#' archive is append-only: overwriting or deleting a property raises an error.
#'
#' @param path Path to a `.daf.zip` archive.
#' @param mode One of `"r"`, `"r+"`, `"w"`, `"w+"`.
#' @param name Human-readable identifier. Default derived from the archive.
#' @param packed When `TRUE` (writeable modes), large numeric components are
#'   written as packed `.zip` shards (as in [files_daf()]).
#' @return `ZipDaf` (writable) or `ZipDafReadOnly` (`"r"`).
#' @export
zip_daf <- function(path, mode = c("r", "r+", "w", "w+"), name = NULL,
                    packed = FALSE) {
    stopifnot(is.character(path), length(path) == 1L, !is.na(path))
    mode <- match.arg(mode)
    store_mode <- mode  # MmapZipStore understands r/r+/w/w+
    store <- new_mmap_zip_store(path, mode = store_mode)
    marker <- "daf.json"
    has_marker <- store_exists(store, marker)
    if (mode %in% c("r", "r+") && !has_marker)
        stop(sprintf("zip_daf(%s, '%s'): not a daf archive (no daf.json)", sQuote(path), mode), call. = FALSE)
    if (mode == "w" && has_marker)
        stop(sprintf("zip_daf(%s, 'w'): archive already a daf store; use 'w+'", sQuote(path)), call. = FALSE)
    if (mode %in% c("w", "w+") && !has_marker)
        store_set_bytes(store, marker, charToRaw('{"version":[1,1]}\n'))
    # Strip a stray metadata.json sidecar on writable open (matches Julia).
    if (mode %in% c("r+", "w+") && store_exists(store, "metadata.json")) {
        # store is append-only; a real strip needs a rewrite. For now: ignore
        # it on read (enumeration uses the central directory, not metadata.json),
        # and never write one. ponytail: leave sidecar in place, it's inert.
    }
    .zip_check_version(store, path)
    if (is.null(name)) {
        nm_key <- "scalars/name.json"
        name <- if (store_exists(store, nm_key)) .decode_scalar_json(store_get_bytes(store, nm_key)) else basename(path)
    }
    .assert_name(name, "name")
    internal <- new_internal_env()
    internal$store <- store
    internal$path  <- normalizePath(path, winslash = "/", mustWork = FALSE)
    internal$mode  <- mode
    internal$packed <- isTRUE(packed)
    internal$axes  <- new.env(parent = emptyenv())
    ctor <- if (mode == "r") ZipDafReadOnly else ZipDaf
    ctor(name = name, internal = internal, cache = new_cache_env(),
         axis_version_counter = new_counter_env(),
         vector_version_counter = new_counter_env(),
         matrix_version_counter = new_counter_env())
}

.zip_check_version <- function(store, path) {
    raw <- rawToChar(store_get_bytes(store, "daf.json"))
    m <- regmatches(raw, regexec(.DAF_JSON_RE, raw, perl = TRUE))[[1L]]
    if (length(m) != 3L) {
        j <- jsonlite::fromJSON(raw); v <- j$version
        if (is.null(v) || length(v) != 2L) stop(sprintf("zip_daf: %s daf.json malformed", sQuote(path)), call. = FALSE)
        v1 <- v[[1L]]; v2 <- v[[2L]]
    } else { v1 <- as.integer(m[[2L]]); v2 <- as.integer(m[[3L]]) }
    if (v1 != 1L || v2 > 1L)
        stop(sprintf("incompatible format version: %d.%d\nfor the zip daf: %s\nthe code supports version: 1.1", v1, v2, path), call. = FALSE)
    invisible()
}
```

Note: `internal$packed` is read by `.files_is_packed_writer` (Task A4 leaves it reading `internal$packed`) - no change needed. Confirm `new_internal_env`/`new_cache_env`/`new_counter_env`/`new_mmap_zip_store` are exported to the package namespace (they are used by `files_daf`).

- [ ] **Step 2: `open_daf` dispatch**

In `R/open_daf.R`, before the final `files_daf` fallback and after the zarr checks, add:

```r
    if (grepl("\\.daf\\.zip(#.*)?$", uri)) {
        if (grepl("#", uri, fixed = TRUE))
            stop("open_daf: grouped .dafs.zip#/group archives are not supported yet", call. = FALSE)
        return(zip_daf(uri, mode = mode, name = name))
    }
```

Also remove/repoint the `.h5df` stub? No - leave the H5df stub as is (separate slice).

- [ ] **Step 3: Commit**

```bash
git add R/zip_daf.R R/open_daf.R
git commit -m "feat(zip-daf): zip_daf() constructor + open_daf .daf.zip dispatch"
```

### Task B4: ZipDaf round-trip tests (R-internal)

**Files:**
- Create: `tests/testthat/test-zip-daf.R`

- [ ] **Step 1: Write the tests**

```r
test_that("zip_daf round-trips scalars, axes, vectors, matrices", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    set_scalar(d, "pi", 3.14)
    set_scalar(d, "cells", bit64::as.integer64(100))
    set_scalar(d, "note", "hello")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "donor", c(1L, 2L, 3L, 4L))
    set_vector(d, "cell", "sx", c(0, 10, 0, 30))          # sparsifies
    set_matrix(d, "cell", "gene", "dm", matrix(1:8, nrow = 4))
    sp <- Matrix::sparseMatrix(i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30), dims = c(4, 2))
    set_matrix(d, "cell", "gene", "sm", sp)
    rm(d); gc()

    d2 <- zip_daf(p, mode = "r")
    expect_equal(get_scalar(d2, "pi"), 3.14)
    expect_equal(get_scalar(d2, "cells"), bit64::as.integer64(100))
    expect_equal(get_scalar(d2, "note"), "hello")
    expect_equal(axis_vector(d2, "cell"), c("A", "B", "C", "D"))
    expect_equal(unname(get_vector(d2, "cell", "donor")), c(1L, 2L, 3L, 4L))
    expect_equal(unname(get_vector(d2, "cell", "sx")), c(0, 10, 0, 30))
    expect_equal(unname(get_matrix(d2, "cell", "gene", "dm")), matrix(c(1,2,3,4,5,6,7,8), nrow = 4))
    sm2 <- get_matrix(d2, "cell", "gene", "sm")
    expect_s4_class(sm2, "dgCMatrix")
    expect_equal(as.matrix(unname(sm2)), as.matrix(sp))
})

test_that("zip_daf is append-only: overwrite and delete raise", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    set_scalar(d, "k", 1L)
    expect_error(set_scalar(d, "k", 2L), "append-only")
    expect_error(delete_scalar(d, "k"), "append-only")
})

test_that("open_daf dispatches .daf.zip to zip_daf", {
    p <- tempfile(fileext = ".daf.zip")
    d <- open_daf(p, mode = "w")
    expect_true(inherits(d, "dafr::ZipDaf"))
    add_axis(d, "cell", c("A", "B")); rm(d); gc()
    d2 <- open_daf(p, mode = "r")
    expect_true(inherits(d2, "dafr::ZipDafReadOnly"))
    expect_equal(axis_vector(d2, "cell"), c("A", "B"))
})

test_that("zip_daf 'r' on a missing archive errors; 'w' on existing daf errors", {
    p <- tempfile(fileext = ".daf.zip")
    expect_error(zip_daf(p, mode = "r"))
    d <- zip_daf(p, mode = "w"); rm(d); gc()
    expect_error(zip_daf(p, mode = "w"), "w\\+")
})
```

- [ ] **Step 2: Run**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-zip-daf.R")'`
Expected: PASS. Fix any ZipDaf method wiring gaps surfaced here (missing method registration for a generic → "no applicable method" error → add it in `R/zip_daf.R`).

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-zip-daf.R
git commit -m "test(zip-daf): R-internal round-trip + append-only + dispatch"
```

---

## Phase C - Cross-language interop + release

### Task C1: Bidirectional Julia interop test

**Files:**
- Create: `tests/testthat/test-zip-daf-julia-compat.R`

- [ ] **Step 1: Write the interop tests (gated on julia env)**

Model on `test-files-julia-compat.R` (reuse `.have_julia_env()` / `run_julia()` from `helper-julia.R`).

```r
test_that("R-written .daf.zip is readable by Julia with identical values", {
    skip_if_not(.have_julia_env())
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", c("A","B","C","D")); add_axis(d, "gene", c("X","Y"))
    set_scalar(d, "pi", 3.14); set_scalar(d, "note", "hello")
    set_vector(d, "cell", "donor", c(1L,2L,3L,4L))
    set_matrix(d, "cell", "gene", "dm", matrix(1:8, nrow = 4))
    sp <- Matrix::sparseMatrix(i = c(1,3,2), j = c(1,1,2), x = c(10,20,30), dims = c(4,2))
    set_matrix(d, "cell", "gene", "sm", sp)
    rm(d); gc()
    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = ZipDaf(raw"%s", "r")', p),
        '@assert get_scalar(daf, "pi") == 3.14',
        '@assert get_scalar(daf, "note") == "hello"',
        '@assert axis_vector(daf, "cell") == ["A","B","C","D"]',
        '@assert get_vector(daf, "cell", "donor") == Int32[1,2,3,4]',
        '@assert get_matrix(daf, "cell", "gene", "dm") == Float64[1 5;2 6;3 7;4 8]',
        'sm = get_matrix(daf, "cell", "gene", "sm")',
        "@assert nnz(sm) == 3",
        'println("JULIA_OK")')
    out <- run_julia(script)
    expect_true(any(grepl("JULIA_OK", out)), info = paste(out, collapse = "\n"))
})

test_that("Julia-written .daf.zip is readable by R", {
    skip_if_not(.have_julia_env())
    p <- tempfile(fileext = ".daf.zip")
    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = ZipDaf(raw"%s", "w")', p),
        'add_axis!(daf, "cell", ["A","B","C","D"]); add_axis!(daf, "gene", ["X","Y"])',
        'set_scalar!(daf, "pi", 3.14)',
        'set_vector!(daf, "cell", "sx", Float64[0,10,0,30])',
        'set_matrix!(daf, "cell", "gene", "dm", Float64[1 5;2 6;3 7;4 8])',
        'println("JULIA_WROTE")')
    out <- run_julia(script)
    skip_if_not(any(grepl("JULIA_WROTE", out)), message = paste(out, collapse = "\n"))
    d <- zip_daf(p, mode = "r")
    expect_equal(axis_vector(d, "cell"), c("A","B","C","D"))
    expect_equal(unname(get_vector(d, "cell", "sx")), c(0,10,0,30))
    expect_equal(unname(get_matrix(d, "cell", "gene", "dm")), matrix(c(1,2,3,4,5,6,7,8), nrow = 4))
})
```

- [ ] **Step 2: Run (skips cleanly if no julia env)**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-zip-daf-julia-compat.R")'`
Expected: PASS or SKIP. If julia is available and a read mismatches, diff the archive layout vs a FilesDaf-written store (`unzip -l`).

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-zip-daf-julia-compat.R
git commit -m "test(zip-daf): bidirectional Julia interop"
```

### Task C2: Full suite, docs, NEWS, version bump

**Files:**
- Modify: `NEWS.md`, `DESCRIPTION`, `R/open_daf.R` (roxygen mentions `.daf.zip`), regenerate `NAMESPACE`/`man/` via `devtools::document()`

- [ ] **Step 1: Run the FULL test suite**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: 0 failed / 0 error (julia-gated tests may skip).

- [ ] **Step 2: Document + NEWS + version**

- `devtools::document()` to pick up the new `@export`s (`zip_daf`, `ZipDaf`, `ZipDafReadOnly`).
- Add a `NEWS.md` entry under a new version (bump `DESCRIPTION` `Version:` to `0.6.0`) describing the ZipDaf backend, byte-compatibility with DataAxesFormats.jl 0.3.0, append-only semantics, and that grouped `.dafs.zip#/group` and H5df remain unimplemented.
- Update `open_daf` roxygen to list `*.daf.zip` as a supported URI.

- [ ] **Step 3: R CMD check gate (per project convention)**

Run: `NOT_CRAN=true Rscript -e 'rcmdcheck::rcmdcheck(args = "--as-cran", error_on = "warning")'`
Expected: 0 errors / 0 warnings. Fix codoc/example/Rd issues before shipping (CI is error_on=warning on main).

- [ ] **Step 4: Commit**

```bash
git add NEWS.md DESCRIPTION NAMESPACE man/ R/open_daf.R
git commit -m "docs(zip-daf): NEWS, exports, 0.6.0 bump; ZipDaf backend complete"
```

---

## Self-review notes (coverage against the spec)

- Container reuse (MmapZipStore) → Tasks B1/B3. Serialization reuse → Phase A seam.
- Layout / keys / no-metadata.json / daf.json marker → B3 constructor + `.key_*` builders (A4).
- Append-only (delete/overwrite raise, matches Julia) → B2 guards + B4 test.
- open_daf dispatch → B3; grouped `.dafs.zip#` explicitly rejected (deferred).
- Bidirectional Julia interop → C1. R round-trips → B4.
- Cache-group classification → reuses FilesDaf classifier via shared helpers (A4).
- Deferred: grouped archives (rejected in open_daf + constructor path parsing minimal), H5df (untouched stub).

**Open risk to verify during B2:** whether `set_scalar(overwrite=TRUE)` routes through `format_delete_scalar`. If dafr's high-level writer instead calls `format_set_scalar` with an `overwrite` flag and only the format method decides, then the append-only error must also be raised inside `format_set_*` when the key already exists (add an `exists`-check + guard there). Resolve by reading the high-level `set_scalar`/`set_vector` wrappers before finalizing B2.
