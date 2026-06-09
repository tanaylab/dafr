# ZarrDaf v3 Flat Core Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port dafr-native's ZarrDaf from Zarr v2 to Zarr v3 (flat, uncompressed) so it interoperates with DataAxesFormats.jl 0.3.0, which uses Zarr v3 and rejects v2.

**Architecture:** Replace the `R/zarr_v2.R` codec layer with a new `R/zarr_v3.R` (per-node `zarr.json` metadata, v3 lowercase dtype names, `c/`-prefixed chunk keys, daf marker as a root-group attribute, inline consolidated metadata). The format-agnostic store layer (`R/zarr_store.R`) and the sparse/dense structural logic in `R/zarr_format.R` are reused; only leaf metadata I/O, chunk-key construction, dtype strings, and the open/marker logic change. Clean break: v2 stores are rejected on open. Scope is flat (single uncompressed chunk) read + write; packed/sharded read is a separate follow-up plan.

**Tech Stack:** R, S7 classes, jsonlite, Matrix, bit64, testthat, cpp11 (existing mmap ALTREP). No new dependencies (flat path is uncompressed `bytes` codec + vlen-utf8 + base-R gzip).

---

## Ground truth (verified against a real DAF 0.3.0-written store)

These are the exact on-disk shapes the reader parses and the writer produces. Verified by writing a store with DAF 0.3.0 (`~/src/DataAxesFormats.jl` @ 0.3.0) and inspecting the bytes.

**Root group** (`<root>/zarr.json`):
```json
{ "zarr_format": 3, "node_type": "group",
  "attributes": { "daf": [1, 0] },
  "consolidated_metadata": { "kind": "inline", "must_understand": false,
    "metadata": { "scalars": {"zarr_format":3,"node_type":"group"},
                  "axes/cell": { <full array metadata> }, ... } } }
```
- The daf version marker is the **root group attribute** `daf: [1, 0]` (integers). NOT a `daf` array (that was v2).
- `consolidated_metadata.metadata` maps each non-root node's relative path (no leading slash, no `/zarr.json` suffix) to its full metadata blob. Present for directory stores; omitted for `.zip` stores.

**Container / intermediate groups** (`scalars/zarr.json`, `vectors/cell/zarr.json`, `matrices/cell/gene/zarr.json`, sparse-property group):
```json
{ "zarr_format": 3, "node_type": "group" }
```

**Numeric array** (e.g. `scalars/n` Int32, `vectors/cell/score` Float64):
```json
{ "zarr_format": 3, "node_type": "array",
  "shape": [3],
  "data_type": "float64",
  "chunk_grid": { "name": "regular", "configuration": { "chunk_shape": [3] } },
  "chunk_key_encoding": { "name": "default", "configuration": { "separator": "/" } },
  "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } } ],
  "fill_value": 0.0,
  "attributes": {} }
```
- Scalars are shape-`[1]` arrays under `scalars/<name>`. JSON key order is irrelevant (parse by key).

**String array** (e.g. `scalars/title`, `axes/cell`, string vectors): same as numeric but
```json
  "data_type": "string",
  "codecs": [ { "name": "vlen-utf8", "configuration": {} } ],
  "fill_value": ""
```

**Chunk keys**: prefix `c`, separator `/`. 1-D single chunk → `c/0`; 2-D single chunk → `c/0/0`.

**Dense matrix** `Float32[1 2; 3 4; 5 6]` (Daf dims 3 rows × 2 cols):
- `shape: [2, 3]` — **reversed to `[n_cols, n_rows]`** (same convention as v2; there is no `order` field in v3).
- chunk `c/0/0` bytes (float32 LE): `[1, 3, 5, 2, 4, 6]` — **column-major** of the 3×2 matrix.
- Reader: `nr = shape[[2]]`, `nc = shape[[1]]`, fill an R matrix `dim = c(nr, nc)` directly from the column-major chunk bytes (R's native fill). Identical to the existing v2 reader logic.

**Sparse matrix** `sparse([0 2; 0 0; 5 0])` (3×2), group `matrices/cell/gene/sp/` with `nzind`-style children:
- `colptr`: **`int64`**, shape `[3]`, values `(1, 2, 3)` — 1-based CSC column pointers.
- `rowval`: **`int64`**, shape `[2]`, values `(3, 1)` — 1-based row indices.
- `nzval`: `float64`, shape `[2]`, values `(5.0, 2.0)`.
- All-true Bool sparse omits the `nzval` child (compaction, unchanged from v2).
- **DAF writes `int64` for colptr/rowval** (v2 dafr wrote `int32`); the v3 writer must emit `int64` to round-trip.

**dtype name map** (v3 lowercase, from `Zarr.typestr3`):

| R type | v3 `data_type` (write) | codec |
|---|---|---|
| `character` | `string` | `vlen-utf8` |
| `logical` | `bool` | `bytes` |
| `integer64` | `int64` | `bytes` |
| `integer` | `int32` | `bytes` |
| `double`/numeric | `float64` | `bytes` |

Read accepts additionally: `float32`→double, `int8/16`,`uint8/16/32`→integer, `uint64`→integer64.

---

## File structure

- **Create** `R/zarr_v3.R` — v3 metadata + chunk codec layer (replaces `zarr_v2.R`). dtype tables, chunk-key construction, `zarr.json` read/write for arrays and groups, root marker + consolidated metadata, chunk encode/decode, vlen-utf8.
- **Modify** `R/zarr_format.R` — route all leaf metadata/chunk I/O through `zarr_v3.R`; rewrite open/marker/init logic; reject v2; keep sparse/dense structural logic.
- **Modify** `R/zarr_store.R:1-9` — comment says "Zarr v2"; update wording (no behavior change).
- **Delete** `R/zarr_v2.R` and its tests (phase 4) once nothing references them.
- **Create** `tests/testthat/test-zarr-v3.R` — codec-layer unit tests (DictStore, no Julia).
- **Create** `tests/testthat/fixtures/daf030-flat/` — a committed DAF-0.3.0-written flat store for read tests.
- **Modify** `tests/testthat/test-zarr-julia-interop.R`, `tests/testthat/helper-julia.R` — flip the skip guard to "skip when env DAF < 0.3.0".
- **Modify** `NEWS.md`, `DESCRIPTION` (version), `environment.yml` (comment) — phase 4.

---

# Phase 1: `zarr_v3.R` codec layer (pure R, DictStore tests, no Julia)

### Task 1.1: dtype mapping

**Files:**
- Create: `R/zarr_v3.R`
- Test: `tests/testthat/test-zarr-v3.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-zarr-v3.R
test_that("zarr_v3 dtype mapping covers R types and v3 names", {
  expect_equal(zarr_v3_dtype_for_r("x"), "string")
  expect_equal(zarr_v3_dtype_for_r(TRUE), "bool")
  expect_equal(zarr_v3_dtype_for_r(bit64::as.integer64(1)), "int64")
  expect_equal(zarr_v3_dtype_for_r(1L), "int32")
  expect_equal(zarr_v3_dtype_for_r(1.5), "float64")

  expect_equal(zarr_v3_r_kind_for_dtype("float64"), "double")
  expect_equal(zarr_v3_r_kind_for_dtype("float32"), "double")
  expect_equal(zarr_v3_r_kind_for_dtype("int32"), "integer")
  expect_equal(zarr_v3_r_kind_for_dtype("uint8"), "integer")
  expect_equal(zarr_v3_r_kind_for_dtype("int64"), "integer64")
  expect_equal(zarr_v3_r_kind_for_dtype("uint64"), "integer64")
  expect_equal(zarr_v3_r_kind_for_dtype("bool"), "logical")
  expect_equal(zarr_v3_r_kind_for_dtype("string"), "character")

  expect_equal(zarr_v3_size_for_dtype("float64"), 8L)
  expect_equal(zarr_v3_size_for_dtype("int32"), 4L)
  expect_equal(zarr_v3_size_for_dtype("int64"), 8L)
  expect_equal(zarr_v3_size_for_dtype("uint8"), 1L)
  expect_true(is.na(zarr_v3_size_for_dtype("string")))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: FAIL — `could not find function "zarr_v3_dtype_for_r"`.

- [ ] **Step 3: Write minimal implementation**

```r
# R/zarr_v3.R
# Zarr v3 metadata + chunk I/O layer. Operates against any ZarrStore from
# R/zarr_store.R; does no I/O of its own beyond the Store interface.
# Replaces the deleted R/zarr_v2.R. On-disk format matches DataAxesFormats.jl
# 0.3.0 (Zarr v3): single zarr.json per node, lowercase dtype names, c/-prefixed
# chunk keys, daf marker as a root-group attribute, inline consolidated metadata.

#' @importFrom methods as
NULL

# ---- dtype mapping (R type <-> Zarr v3 data_type name) -------------------

# Zarr v3 data_type string for an R value. Always little-endian.
zarr_v3_dtype_for_r <- function(value) {
    if (is.character(value)) return("string")
    if (is.logical(value)) return("bool")
    if (inherits(value, "integer64")) return("int64")
    if (is.integer(value)) return("int32")
    if (is.numeric(value)) return("float64")
    stop(sprintf("zarr_v3: cannot map R type %s to a Zarr v3 data_type",
                 class(value)[1L]), call. = FALSE)
}

# R "kind" (readBin target) for a Zarr v3 data_type name.
zarr_v3_r_kind_for_dtype <- function(dtype) {
    switch(dtype,
        "float64" = "double", "float32" = "double",
        "int8" = "integer", "int16" = "integer", "int32" = "integer",
        "uint8" = "integer", "uint16" = "integer", "uint32" = "integer",
        "int64" = "integer64", "uint64" = "integer64",
        "bool" = "logical",
        "string" = "character",
        stop(sprintf("zarr_v3: unsupported data_type %s", sQuote(dtype)),
             call. = FALSE))
}

# Element byte size for a fixed-width v3 dtype; NA for variable-length (string).
zarr_v3_size_for_dtype <- function(dtype) {
    switch(dtype,
        "float64" = 8L, "float32" = 4L,
        "int64" = 8L, "uint64" = 8L,
        "int32" = 4L, "uint32" = 4L,
        "int16" = 2L, "uint16" = 2L,
        "int8" = 1L, "uint8" = 1L, "bool" = 1L,
        "string" = NA_integer_,
        stop(sprintf("zarr_v3: unsupported data_type %s", sQuote(dtype)),
             call. = FALSE))
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_v3.R tests/testthat/test-zarr-v3.R
git commit -m "feat(zarr-v3): dtype name mapping (R <-> Zarr v3 data_type)"
```

### Task 1.2: chunk-key construction

**Files:**
- Modify: `R/zarr_v3.R`
- Test: `tests/testthat/test-zarr-v3.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("zarr_v3 chunk keys use c/ prefix and / separator", {
  expect_equal(zarr_v3_chunk_key(1L), "c/0")
  expect_equal(zarr_v3_chunk_key(2L), "c/0/0")
  # path-qualified
  expect_equal(zarr_v3_chunk_path("vectors/cell/score", 1L), "vectors/cell/score/c/0")
  expect_equal(zarr_v3_chunk_path("matrices/cell/gene/expr", 2L),
               "matrices/cell/gene/expr/c/0/0")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: FAIL — `could not find function "zarr_v3_chunk_key"`.

- [ ] **Step 3: Write minimal implementation**

```r
# Append to R/zarr_v3.R

# ---- chunk keys (v3 default encoding: prefix "c", separator "/") ---------

# Single-chunk key for an ndim-dimensional array: "c/0" (1-D), "c/0/0" (2-D).
zarr_v3_chunk_key <- function(ndim) {
    paste(c("c", rep("0", ndim)), collapse = "/")
}

# Full store key for the single chunk of the array at `base`.
zarr_v3_chunk_path <- function(base, ndim) {
    paste0(base, "/", zarr_v3_chunk_key(ndim))
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_v3.R tests/testthat/test-zarr-v3.R
git commit -m "feat(zarr-v3): c/-prefixed chunk-key construction"
```

### Task 1.3: array `zarr.json` build / write / read

**Files:**
- Modify: `R/zarr_v3.R`
- Test: `tests/testthat/test-zarr-v3.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("zarr_v3 array metadata round-trips through a DictStore", {
  store <- new_dict_store()
  meta <- zarr_v3_array_meta(shape = c(3L), dtype = "float64")
  zarr_v3_write_array(store, "vectors/cell/score", meta)

  # the per-node file is exactly zarr.json
  expect_true(store_exists(store, "vectors/cell/score/zarr.json"))
  # ancestor groups got group markers
  expect_true(store_exists(store, "vectors/zarr.json"))
  expect_true(store_exists(store, "vectors/cell/zarr.json"))

  rt <- zarr_v3_read_array(store, "vectors/cell/score")
  expect_equal(rt$node_type, "array")
  expect_equal(rt$zarr_format, 3L)
  expect_equal(as.integer(rt$shape[[1L]]), 3L)
  expect_equal(rt$data_type, "float64")
  expect_equal(as.integer(rt$chunk_grid$configuration$chunk_shape[[1L]]), 3L)
  expect_equal(rt$codecs[[1L]]$name, "bytes")
  expect_equal(rt$codecs[[1L]]$configuration$endian, "little")

  smeta <- zarr_v3_array_meta(shape = c(2L), dtype = "string")
  expect_equal(smeta$codecs[[1L]]$name, "vlen-utf8")
  expect_equal(smeta$data_type, "string")
  expect_equal(smeta$fill_value, "")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: FAIL — `could not find function "zarr_v3_array_meta"`.

- [ ] **Step 3: Write minimal implementation**

```r
# Append to R/zarr_v3.R

# ---- array zarr.json -----------------------------------------------------

# Build a Zarr v3 array metadata list (one chunk == shape). `bytes` codec for
# numeric dtypes (little-endian); `vlen-utf8` for strings. fill_value defaults
# to the dtype zero ("" for strings).
zarr_v3_array_meta <- function(shape, dtype, fill_value = NULL) {
    shape <- as.integer(shape)
    if (is.null(fill_value)) {
        fill_value <- if (dtype == "string") "" else
            if (dtype == "bool") FALSE else
            if (zarr_v3_r_kind_for_dtype(dtype) == "double") 0.0 else 0L
    }
    codecs <- if (dtype == "string") {
        list(list(name = "vlen-utf8", configuration = structure(list(),
                                                  names = character(0L))))
    } else {
        list(list(name = "bytes", configuration = list(endian = "little")))
    }
    list(
        zarr_format = 3L,
        node_type = "array",
        shape = as.list(shape),
        data_type = dtype,
        chunk_grid = list(name = "regular",
                          configuration = list(chunk_shape = as.list(shape))),
        chunk_key_encoding = list(name = "default",
                                  configuration = list(separator = "/")),
        codecs = codecs,
        fill_value = fill_value,
        attributes = structure(list(), names = character(0L))
    )
}

# Write path/zarr.json for an array, ensuring every ancestor group marker.
zarr_v3_write_array <- function(store, path, meta) {
    .zarr_v3_ensure_ancestor_groups(store, path)
    json <- jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null",
                             pretty = FALSE)
    store_set_bytes(store, paste0(path, "/zarr.json"),
                    charToRaw(as.character(json)))
    invisible()
}

# Read + parse a node's zarr.json (array or group), or NULL if absent.
zarr_v3_read_node <- function(store, path) {
    key <- if (nzchar(path)) paste0(path, "/zarr.json") else "zarr.json"
    raw <- store_get_bytes(store, key)
    if (is.null(raw)) return(NULL)
    jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
}

# Read an array node; NULL if absent or not an array.
zarr_v3_read_array <- function(store, path) {
    node <- zarr_v3_read_node(store, path)
    if (is.null(node) || !identical(node$node_type, "array")) return(NULL)
    node
}
```

Also append the ancestor-group helper and group-marker bytes (used by the test and Task 1.4):

```r
# Append to R/zarr_v3.R

# ---- group zarr.json -----------------------------------------------------

# Group metadata for a plain (non-root) group.
.ZARR_V3_GROUP <- list(zarr_format = 3L, node_type = "group")

# Write a plain group marker at `path` (idempotent on append-only stores).
zarr_v3_write_group <- function(store, path) {
    key <- if (nzchar(path)) paste0(path, "/zarr.json") else "zarr.json"
    if (store_exists(store, key)) return(invisible())
    json <- jsonlite::toJSON(.ZARR_V3_GROUP, auto_unbox = TRUE, pretty = FALSE)
    store_set_bytes(store, key, charToRaw(as.character(json)))
    invisible()
}

# Ensure every ancestor group of an array `path` carries a group zarr.json.
# Mirrors DAF's directory layout (every intermediate dir is a v3 group).
.zarr_v3_ensure_ancestor_groups <- function(store, path) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
    if (length(parts) <= 1L) return(invisible())
    for (i in seq_len(length(parts) - 1L)) {
        zarr_v3_write_group(store, paste(parts[seq_len(i)], collapse = "/"))
    }
    invisible()
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_v3.R tests/testthat/test-zarr-v3.R
git commit -m "feat(zarr-v3): array + group zarr.json read/write"
```

### Task 1.4: root marker + consolidated metadata

**Files:**
- Modify: `R/zarr_v3.R`
- Test: `tests/testthat/test-zarr-v3.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("zarr_v3 root marker carries daf attribute and reads back", {
  store <- new_dict_store()
  zarr_v3_write_root(store)
  expect_true(zarr_v3_daf_marker_exists(store))
  expect_equal(zarr_v3_daf_version(store), c(1L, 0L))

  # an empty dict store with no root is not a marker
  expect_false(zarr_v3_daf_marker_exists(new_dict_store()))
})

test_that("zarr_v3 consolidated metadata indexes non-root nodes", {
  store <- new_dict_store()
  zarr_v3_write_root(store)
  zarr_v3_write_group(store, "scalars")
  zarr_v3_write_array(store, "vectors/cell/score",
                      zarr_v3_array_meta(c(3L), "float64"))
  zarr_v3_write_consolidated(store)

  root <- zarr_v3_read_node(store, "")
  md <- root$consolidated_metadata$metadata
  expect_true("scalars" %in% names(md))
  expect_true("vectors/cell/score" %in% names(md))
  expect_equal(md[["vectors/cell/score"]]$node_type, "array")
  expect_false("" %in% names(md))   # root is not listed in its own index
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: FAIL — `could not find function "zarr_v3_write_root"`.

- [ ] **Step 3: Write minimal implementation**

```r
# Append to R/zarr_v3.R

# ---- root marker + consolidated metadata ---------------------------------

# Daf-zarr format version (root-group attribute `daf: [MAJOR, MINOR]`).
.ZARR_V3_DAF_MAJOR <- 1L
.ZARR_V3_DAF_MINOR <- 0L

# Write the root group zarr.json with the daf version attribute. Consolidated
# metadata is (re)written separately via zarr_v3_write_consolidated().
zarr_v3_write_root <- function(store) {
    root <- list(
        zarr_format = 3L,
        node_type = "group",
        attributes = list(daf = list(.ZARR_V3_DAF_MAJOR, .ZARR_V3_DAF_MINOR))
    )
    json <- jsonlite::toJSON(root, auto_unbox = TRUE, pretty = FALSE)
    store_set_bytes(store, "zarr.json", charToRaw(as.character(json)))
    invisible()
}

# TRUE if the store's root zarr.json is a group carrying a `daf` attribute.
zarr_v3_daf_marker_exists <- function(store) {
    node <- zarr_v3_read_node(store, "")
    !is.null(node) && identical(node$node_type, "group") &&
        !is.null(node$attributes) && !is.null(node$attributes$daf)
}

# The [MAJOR, MINOR] version from the root daf attribute (integer vector).
zarr_v3_daf_version <- function(store) {
    node <- zarr_v3_read_node(store, "")
    as.integer(unlist(node$attributes$daf))
}

# Rebuild root consolidated_metadata: index every non-root node by relative
# path -> its full metadata (the root's own consolidated_metadata field is
# stripped from any node before inlining). No-op on append-only zip stores
# (DAF omits consolidated metadata there and uses the zip central directory).
zarr_v3_write_consolidated <- function(store) {
    if (S7::S7_inherits(store, MmapZipStore)) return(invisible())
    keys <- store_list(store, "")
    node_keys <- keys[keys == "zarr.json" | endsWith(keys, "/zarr.json")]
    md <- list()
    for (k in node_keys) {
        path <- if (k == "zarr.json") "" else sub("/zarr.json$", "", k)
        if (path == "") next                      # root not listed in its index
        node <- jsonlite::fromJSON(rawToChar(store_get_bytes(store, k)),
                                   simplifyVector = FALSE)
        node$consolidated_metadata <- NULL
        md[[path]] <- node
    }
    if (length(md) == 0L) md <- structure(list(), names = character(0L))
    root <- list(
        zarr_format = 3L,
        node_type = "group",
        attributes = list(daf = list(.ZARR_V3_DAF_MAJOR, .ZARR_V3_DAF_MINOR)),
        consolidated_metadata = list(kind = "inline", must_understand = FALSE,
                                     metadata = md)
    )
    json <- jsonlite::toJSON(root, auto_unbox = TRUE, null = "null",
                             pretty = FALSE)
    store_set_bytes(store, "zarr.json", charToRaw(as.character(json)))
    invisible()
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_v3.R tests/testthat/test-zarr-v3.R
git commit -m "feat(zarr-v3): root daf marker + inline consolidated metadata"
```

### Task 1.5: chunk encode/decode + vlen-utf8 (v3 dtype names)

**Files:**
- Modify: `R/zarr_v3.R`
- Test: `tests/testthat/test-zarr-v3.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("zarr_v3 numeric chunk encode/decode round-trips", {
  for (case in list(
      list(v = c(1.5, 2.5, 3.5), d = "float64"),
      list(v = c(1L, 2L, 3L), d = "int32"),
      list(v = bit64::as.integer64(c(1, 2, 3)), d = "int64"),
      list(v = c(TRUE, FALSE, TRUE), d = "bool"))) {
    enc <- zarr_v3_encode_chunk(case$v, case$d)
    dec <- zarr_v3_decode_chunk(enc, case$d, n = length(case$v))
    expect_equal(dec, case$v, info = case$d)
  }
})

test_that("zarr_v3 decodes float32 by widening to double", {
  raw <- writeBin(c(1.0, 3.0, 5.0), raw(), size = 4L, endian = "little")
  expect_equal(zarr_v3_decode_chunk(raw, "float32", n = 3L), c(1, 3, 5))
})

test_that("zarr_v3 vlen-utf8 strings round-trip", {
  enc <- zarr_v3_encode_strings(c("alpha", "", "βeta"))
  expect_equal(zarr_v3_decode_strings(enc, n = 3L), c("alpha", "", "βeta"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: FAIL — `could not find function "zarr_v3_encode_chunk"`.

- [ ] **Step 3: Write minimal implementation**

The byte-level encode/decode and vlen-utf8 are identical to the deleted v2 layer except they dispatch on v3 dtype names. Copy the proven logic from the old `R/zarr_v2.R` and re-key it:

```r
# Append to R/zarr_v3.R

# ---- chunk encode (write) ------------------------------------------------

# Encode an R vector to raw little-endian bytes for a v3 numeric dtype.
# Strings use zarr_v3_encode_strings (vlen-utf8).
zarr_v3_encode_chunk <- function(value, dtype) {
    if (dtype == "string") {
        stop("zarr_v3_encode_chunk: use zarr_v3_encode_strings for strings",
             call. = FALSE)
    }
    if (dtype == "float64") return(.zarr_v3_writeBin(as.double(value), 8L))
    if (dtype == "int32")   return(.zarr_v3_writeBin(as.integer(value), 4L))
    if (dtype == "int64") {
        if (!inherits(value, "integer64")) value <- bit64::as.integer64(value)
        return(.zarr_v3_writeBin(unclass(value), 8L, what = "double"))
    }
    if (dtype == "bool") return(as.raw(as.integer(as.logical(value))))
    stop(sprintf("zarr_v3_encode_chunk: unsupported write dtype %s",
                 sQuote(dtype)), call. = FALSE)
}

.zarr_v3_writeBin <- function(value, size, what = NULL) {
    con <- rawConnection(raw(0L), "wb"); on.exit(close(con))
    if (is.null(what)) {
        writeBin(value, con, size = size, endian = "little")
    } else {
        writeBin(as(value, what), con, size = size, endian = "little")
    }
    rawConnectionValue(con)
}

# ---- chunk decode (read) -------------------------------------------------

# Decode raw bytes to an R vector for a v3 numeric dtype. `compressor` carries
# over the v2 gzip path (DAF flat arrays are uncompressed; sharded/blosc is a
# separate plan). Strings use zarr_v3_decode_strings.
zarr_v3_decode_chunk <- function(bytes, dtype, n, compressor = NULL) {
    if (!is.null(compressor)) {
        codec_id <- compressor$id %||% compressor[["id"]]
        if (identical(codec_id, "gzip")) {
            bytes <- memDecompress(bytes, type = "gzip")
        } else {
            stop(sprintf("zarr_v3: codec %s not supported on the flat path",
                         sQuote(codec_id)), call. = FALSE)
        }
    }
    if (dtype == "string") {
        stop("zarr_v3_decode_chunk: use zarr_v3_decode_strings for strings",
             call. = FALSE)
    }
    con <- rawConnection(bytes, "rb"); on.exit(close(con))
    if (dtype == "float64") return(readBin(con, "double", n, 8L, endian = "little"))
    if (dtype == "float32") return(readBin(con, "double", n, 4L, endian = "little"))
    if (dtype == "int32")   return(readBin(con, "integer", n, 4L, endian = "little"))
    if (dtype %in% c("uint8", "int8")) {
        return(readBin(con, "integer", n, 1L, signed = (dtype == "int8")))
    }
    if (dtype %in% c("uint16", "int16")) {
        return(readBin(con, "integer", n, 2L, signed = (dtype == "int16"),
                       endian = "little"))
    }
    if (dtype == "uint32") {
        x <- readBin(con, "integer", n, 4L, endian = "little")
        neg <- !is.na(x) & x < 0L
        if (any(neg)) { x <- as.double(x); x[neg] <- x[neg] + 2^32 }
        return(x)
    }
    if (dtype %in% c("int64", "uint64")) {
        d <- readBin(con, "double", n, 8L, endian = "little")
        class(d) <- "integer64"
        return(d)
    }
    if (dtype == "bool") return(as.logical(readBin(con, "raw", n)))
    stop(sprintf("zarr_v3_decode_chunk: unsupported data_type %s",
                 sQuote(dtype)), call. = FALSE)
}

# ---- vlen-utf8 strings (numcodecs wire format; identical to v2) ----------
# [N: uint32 LE] then per string: [len_i: uint32 LE][utf8 bytes].

zarr_v3_encode_strings <- function(strings) {
    if (!is.character(strings)) {
        stop("zarr_v3_encode_strings: input must be a character vector",
             call. = FALSE)
    }
    con <- rawConnection(raw(0L), "wb"); on.exit(close(con))
    writeBin(length(strings), con, size = 4L, endian = "little")
    for (s in strings) {
        if (is.na(s)) stop("zarr_v3_encode_strings: NA not supported",
                           call. = FALSE)
        b <- charToRaw(enc2utf8(s))
        writeBin(length(b), con, size = 4L, endian = "little")
        if (length(b) > 0L) writeBin(b, con)
    }
    rawConnectionValue(con)
}

zarr_v3_decode_strings <- function(bytes, n = NA_integer_) {
    if (length(bytes) < 4L) {
        stop("zarr_v3_decode_strings: truncated (no count header)",
             call. = FALSE)
    }
    con <- rawConnection(bytes, "rb"); on.exit(close(con))
    n_in <- readBin(con, "integer", 1L, 4L, endian = "little")
    if (!is.na(n) && n_in != n) {
        stop(sprintf("zarr_v3_decode_strings: count %d != expected %d",
                     n_in, n), call. = FALSE)
    }
    out <- character(n_in)
    for (i in seq_len(n_in)) {
        len_i <- readBin(con, "integer", 1L, 4L, endian = "little")
        if (length(len_i) != 1L) {
            stop(sprintf("zarr_v3_decode_strings: truncated length at %d", i),
                 call. = FALSE)
        }
        if (len_i == 0L) { out[[i]] <- ""; next }
        payload <- readBin(con, "raw", len_i)
        if (length(payload) != len_i) {
            stop(sprintf("zarr_v3_decode_strings: truncated string at %d", i),
                 call. = FALSE)
        }
        out[[i]] <- rawToChar(payload); Encoding(out[[i]]) <- "UTF-8"
    }
    out
}
```

Note: `%||%` is already used in `zarr_format.R`; it is provided by `rlang` (imported). If `load_all` reports it missing here, add `#' @importFrom rlang %||%` near the top of `R/zarr_v3.R`.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_v3.R tests/testthat/test-zarr-v3.R
git commit -m "feat(zarr-v3): chunk encode/decode + vlen-utf8 (v3 dtype names)"
```

---

# Phase 2: Flat read orchestration

Goal: open a DAF-0.3.0-written flat store and read every property correctly. After this phase, `zarr_daf(<v3 dir>, "r")` works; v2 stores are rejected.

> **Before writing the Phase 2/3 tests:** the public accessor names used below (`get_scalar`, `set_scalar`, `get_vector`, `set_vector`, `get_matrix`, `set_matrix`, `add_axis`, `axis_vector`, `has_vector`, `delete_vector`, `scalars_set`, `axes_set`) are the expected dafr R API but must be confirmed against the actual exports. Run `grep -rn "<- function" R/format_api.R` and check `NAMESPACE` once, then use the exact names. The internal `.zarr_*` helper changes are unaffected by this.

First, wire `zarr_v3.R` into the package so `zarr_format.R` can call it.

### Task 2.0: include `zarr_v3.R` and commit a DAF flat fixture

**Files:**
- Modify: `R/zarr_format.R:1` (the `@include` line)
- Create: `tests/testthat/fixtures/daf030-flat/` (a real DAF-0.3.0 store)
- Create: `tests/testthat/helper-v3-fixtures.R`

- [ ] **Step 1: Update the `@include` roxygen line** so collation orders `zarr_v3.R` before `zarr_format.R`.

`R/zarr_format.R:1` currently reads:
```r
#' @include classes.R zarr_store.R zarr_v2.R cache.R cache_group.R format_api.R utils.R
```
Change `zarr_v2.R` → `zarr_v3.R`:
```r
#' @include classes.R zarr_store.R zarr_v3.R cache.R cache_group.R format_api.R utils.R
```
Then run `Rscript -e 'devtools::document(".")'` to refresh collation in `DESCRIPTION`.

- [ ] **Step 2: Generate the fixture with Julia.** Use the env's DAF 0.3.0 to write a small flat store covering every type, then copy it under `tests/testthat/fixtures/`.

Run:
```bash
JL=/home/aviezerl/miniconda3/envs/dafr-mcview/bin/julia
ENV=/home/aviezerl/miniconda3/envs/dafr-mcview/share/julia/environments/dafr-mcview
cat > /tmp/mk_fixture.jl <<'EOF'
using DataAxesFormats, SparseArrays
dir = "tests/testthat/fixtures/daf030-flat.daf.zarr"
rm(dir; force=true, recursive=true)
daf = ZarrDaf(dir, "w"; name="fix")
add_axis!(daf, "cell", ["c1","c2","c3"]); add_axis!(daf, "gene", ["g1","g2"])
set_scalar!(daf, "title", "hello"); set_scalar!(daf, "n", Int32(7))
set_vector!(daf, "cell", "score", Float64[1.5,2.5,3.5])
set_matrix!(daf, "cell", "gene", "expr", Float32[1 2; 3 4; 5 6])
set_matrix!(daf, "cell", "gene", "sp", sparse(Float64[0 2; 0 0; 5 0]))
EOF
$JL --project=$ENV /tmp/mk_fixture.jl
```
If `close`/finalizer noise appears, the on-disk store is still complete (every `zarr.json` + chunk is written eagerly). Verify with `find tests/testthat/fixtures/daf030-flat.daf.zarr -name zarr.json | wc -l` (expect > 8).

- [ ] **Step 3: Add a fixture-path helper.**

```r
# tests/testthat/helper-v3-fixtures.R
daf030_flat_fixture <- function() {
  testthat::test_path("fixtures", "daf030-flat.daf.zarr")
}
```

- [ ] **Step 4: Commit** (the fixture is small binary; track it).

```bash
git add R/zarr_format.R DESCRIPTION NAMESPACE \
        tests/testthat/fixtures/daf030-flat.daf.zarr \
        tests/testthat/helper-v3-fixtures.R
git commit -m "test(zarr-v3): include zarr_v3.R; commit DAF 0.3.0 flat fixture"
```

### Task 2.1: open + version detection + v2 rejection

**Files:**
- Modify: `R/zarr_format.R:166-221` (marker/verify/init helpers), `R/zarr_format.R:136-148` (open branch)
- Test: `tests/testthat/test-zarr-v3-read.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-zarr-v3-read.R
test_that("zarr_daf opens a DAF 0.3.0 flat store read-only", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expect_s3_class(d, "dafr::ZarrDafReadOnly")
})

test_that("zarr_daf rejects a Zarr v2 store with a clear, actionable error", {
  v2 <- tempfile(fileext = ".daf.zarr"); dir.create(v2)
  writeBin(charToRaw('{"zarr_format":2}'), file.path(v2, ".zgroup"))
  expect_error(zarr_daf(v2, mode = "r"),
               "Zarr v2 store .* requires a Zarr v3|v2_to_v3")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: FAIL — opening reaches the old `daf/.zarray` marker check and errors with the wrong message (or the v2 store is not rejected).

- [ ] **Step 3: Rewrite the marker/verify/init helpers and the open branch.**

Replace `R/zarr_format.R:166-221` (the `.ZARR_DAF_MAJOR`/`.ZARR_DAF_MINOR` constants and the `.zarr_daf_marker_exists` / `.zarr_verify_daf` / `.zarr_daf_init_store` functions) with v3 versions that delegate to `zarr_v3.R`:

```r
# Daf-zarr format version is carried as the root group attribute `daf:[MAJOR,
# MINOR]` (DataAxesFormats.jl 0.3.0, Zarr v3). See R/zarr_v3.R for the marker.
.ZARR_DAF_MAJOR <- .ZARR_V3_DAF_MAJOR
.ZARR_DAF_MINOR <- .ZARR_V3_DAF_MINOR

# Does the store carry a v3 daf marker (root group with a `daf` attribute)?
.zarr_daf_marker_exists <- function(store) {
    zarr_v3_daf_marker_exists(store)
}

# Reject a legacy Zarr v2 store (DAF 0.3.0 does the same). Detected by a root
# `.zgroup`/`.zarray` with no `zarr.json`.
.zarr_reject_if_v2 <- function(store, store_path) {
    has_v3 <- store_exists(store, "zarr.json")
    has_v2 <- store_exists(store, ".zgroup") || store_exists(store, ".zarray") ||
              store_exists(store, "daf/.zarray")
    if (!has_v3 && has_v2) {
        stop(sprintf(paste0(
            "zarr_daf: Zarr v2 store at %s; dafr requires a Zarr v3 store ",
            "(DAF 0.3.0). Convert via `python -m zarr v2_to_v3 <path>` ",
            "(zarr-python 3.1.2+), then reopen."), sQuote(store_path)),
            call. = FALSE)
    }
    invisible()
}

# Read + validate the root daf version attribute. A newer major, or a newer
# minor than this code supports, is rejected.
.zarr_verify_daf <- function(store, store_path) {
    version <- zarr_v3_daf_version(store)
    if (length(version) != 2L) {
        stop(sprintf("zarr_daf: store at %s has a malformed `daf` marker",
                     sQuote(store_path)), call. = FALSE)
    }
    if (version[1L] != .ZARR_DAF_MAJOR || version[2L] > .ZARR_DAF_MINOR) {
        stop(sprintf(paste0(
            "zarr_daf: incompatible format version %d.%d for the daf zarr ",
            "store at %s; this code supports version %d.%d"),
            version[1L], version[2L], sQuote(store_path),
            .ZARR_DAF_MAJOR, .ZARR_DAF_MINOR), call. = FALSE)
    }
    invisible()
}

# Initialize an empty v3 store: root group (with daf attribute) + the four
# container groups, then refresh consolidated metadata.
.zarr_daf_init_store <- function(store) {
    zarr_v3_write_root(store)
    for (grp in c("scalars", "axes", "vectors", "matrices")) {
        zarr_v3_write_group(store, grp)
    }
    zarr_v3_write_consolidated(store)
    invisible()
}
```

Then in the open branch at `R/zarr_format.R:140-148`, add the v2 rejection before the marker check:
```r
    if (mode %in% c("r", "r+")) {
        .zarr_reject_if_v2(store, store_path)
        if (!.zarr_daf_marker_exists(store)) {
            stop(sprintf(
                "zarr_daf: store at %s is not a valid ZarrDaf store (missing `daf` marker)",
                sQuote(store_path)
            ), call. = FALSE)
        }
        .zarr_verify_daf(store, store_path)
    }
```

Leave `.ZARR_ZGROUP_BYTES` and the v2 write-property helpers (`.zarr_write_*`) in place for now — they are still referenced by write paths that get ported in Phase 3. R resolves those names at call time, and Phase 2 tests only read, so the unported write helpers do not run. The `.ZARR_ZGROUP_BYTES` constant and the last v2 write references are removed in the Phase 3 sweep (Task 3.4). This task only needs read-open + v2 rejection working.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-read.R
git commit -m "feat(zarr-v3): v3 open/marker/verify; reject v2 stores"
```

### Task 2.2: scalar + axis read

**Files:**
- Modify: `R/zarr_format.R:249-270` (`.zarr_get_scalar`), `R/zarr_format.R:381-395` (`.zarr_axis_entries`)
- Test: `tests/testthat/test-zarr-v3-read.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("v3 scalars and axis entries read correctly", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expect_equal(get_scalar(d, "title"), "hello")
  expect_equal(as.integer(get_scalar(d, "n")), 7L)
  expect_equal(axis_vector(d, "cell"), c("c1", "c2", "c3"))
  expect_equal(axis_vector(d, "gene"), c("g1", "g2"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: FAIL — `.zarr_get_scalar` calls `zarr_v2_read_zarray` (now deleted/renamed) or reads the wrong chunk key.

- [ ] **Step 3: Read the current `.zarr_get_scalar` and `.zarr_axis_entries`** (Read `R/zarr_format.R:249-270` and `R/zarr_format.R:381-395`), then rewrite them to use the v3 helpers. A scalar/axis is a shape-`[N]` array under `scalars/<name>` / `axes/<axis>`; the chunk is `c/0`:

```r
.zarr_get_scalar <- function(daf, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("scalars/", name)
    meta <- zarr_v3_read_array(store, base)
    if (is.null(meta)) .require_scalar(daf, name)
    n <- as.integer(meta$shape[[1L]])
    chunk <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    val <- if (identical(meta$data_type, "string")) {
        zarr_v3_decode_strings(chunk, n = n)
    } else {
        zarr_v3_decode_chunk(chunk, meta$data_type, n = n)
    }
    val[[1L]]   # scalars are shape [1]
}

.zarr_axis_entries <- function(daf, axis) {
    store <- S7::prop(daf, "store")
    base <- paste0("axes/", axis)
    meta <- zarr_v3_read_array(store, base)
    if (is.null(meta)) .require_axis(daf, "", axis)
    n <- as.integer(meta$shape[[1L]])
    chunk <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    zarr_v3_decode_strings(chunk, n = n)
}
```
(Match the exact return convention of the originals — confirm whether `.zarr_get_scalar` returns a length-1 vector or scalar by reading the original first; preserve it.)

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-read.R
git commit -m "feat(zarr-v3): scalar + axis read"
```

### Task 2.3: dense + sparse vector read

**Files:**
- Modify: `R/zarr_format.R:509-515` (`.zarr_has_vector`), `R/zarr_format.R:523-540` (`.zarr_vectors_set`), `R/zarr_format.R:577-654` (`.zarr_get_vector`, `.zarr_get_sparse_vector`)
- Test: `tests/testthat/test-zarr-v3-read.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("v3 dense vector reads with correct values", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expect_equal(unname(get_vector(d, "cell", "score")), c(1.5, 2.5, 3.5))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: FAIL — `.zarr_get_vector` references `.zarray`/`zarr_v2_*` and the `/0` chunk key.

- [ ] **Step 3: Rewrite the vector read helpers** to use v3 node detection (group vs array via `zarr.json`'s `node_type`), the `c/0` chunk key, and v3 codecs. Read the current `.zarr_get_vector`/`.zarr_get_sparse_vector` (`R/zarr_format.R:577-654`) first to preserve the cache-group/mmap wiring, then:

```r
.zarr_has_vector <- function(daf, axis, name) {
    store <- S7::prop(daf, "store")
    node <- zarr_v3_read_node(store, paste0("vectors/", axis, "/", name))
    !is.null(node)   # array (dense) or group (sparse)
}

.zarr_vectors_set <- function(daf, axis) {
    store <- S7::prop(daf, "store")
    prefix <- paste0("vectors/", axis)
    keys <- store_list(store, prefix)
    if (length(keys) == 0L) return(character(0L))
    rel <- sub(paste0("^", prefix, "/"), "", keys)
    # A property is "<name>/zarr.json" (one slash). Sparse children are deeper.
    names <- sub("/zarr.json$", "", rel[grepl("^[^/]+/zarr.json$", rel)])
    sort(unique(names), method = "radix")
}

.zarr_get_vector <- function(daf, axis, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("vectors/", axis, "/", name)
    node <- zarr_v3_read_node(store, base)
    if (is.null(node)) .require_vector(daf, axis, name)
    if (identical(node$node_type, "group")) {
        return(.zarr_get_sparse_vector(daf, axis, name))
    }
    n <- as.integer(node$shape[[1L]])
    is_string <- identical(node$data_type, "string")
    if (!is_string) {
        mm <- .zarr_try_mmap_dense(store, zarr_v3_chunk_path(base, 1L),
                                   node$data_type, n, NULL)
        if (!is.null(mm)) return(mm)
    }
    chunk <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    if (is.null(chunk)) stop(sprintf("vector %s missing chunk", sQuote(name)),
                             call. = FALSE)
    if (is_string) return(zarr_v3_decode_strings(chunk, n = n))
    zarr_v3_decode_chunk(chunk, node$data_type, n = n)
}
```

For `.zarr_get_sparse_vector`, mirror the existing structure (read `nzind` + `nzval` arrays, 1-based → 0-based) using `zarr_v3_read_array` + `zarr_v3_chunk_path(.., 1L)` + `zarr_v3_decode_chunk`. Read the current body first and translate each `zarr_v2_read_zarray(store, p)`→`zarr_v3_read_array(store, p)`, `paste0(p,"/0")`→`zarr_v3_chunk_path(p,1L)`, `$dtype`→`$data_type`, `zarr_v2_decode_chunk`→`zarr_v3_decode_chunk`.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-read.R
git commit -m "feat(zarr-v3): dense + sparse vector read"
```

### Task 2.4: dense + sparse matrix read

**Files:**
- Modify: `R/zarr_format.R:830-835` (`.zarr_has_matrix`), `R/zarr_format.R:847-865` (`.zarr_matrices_set`), `R/zarr_format.R:867-972` (matrix read helpers)
- Test: `tests/testthat/test-zarr-v3-read.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("v3 dense + sparse matrix read with correct orientation", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expr <- get_matrix(d, "cell", "gene", "expr")
  expect_equal(dim(expr), c(3L, 2L))
  expect_equal(unname(as.matrix(expr)),
               matrix(c(1, 3, 5, 2, 4, 6), nrow = 3))   # cols [1,3,5],[2,4,6]
  sp <- get_matrix(d, "cell", "gene", "sp")
  expect_equal(dim(sp), c(3L, 2L))
  expect_equal(unname(as.matrix(sp)),
               matrix(c(0, 0, 5, 2, 0, 0), nrow = 3))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: FAIL — matrix helpers reference `.zarray`/`0.0` and `zarr_v2_*`.

- [ ] **Step 3: Rewrite the matrix read helpers.** The dense-matrix shape convention is unchanged from v2 (on-disk `shape = [n_cols, n_rows]`, column-major chunk bytes), so the only changes are node detection, chunk key `c/0/0`, and v3 codecs. The sparse `colptr`/`rowval` are now `int64` — `zarr_v3_decode_chunk` returns `integer64`; convert with `as.integer`.

```r
.zarr_has_matrix <- function(daf, rows_axis, columns_axis, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("matrices/", rows_axis, "/", columns_axis, "/", name)
    !is.null(zarr_v3_read_node(store, base))
}

.zarr_get_matrix <- function(daf, rows_axis, columns_axis, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("matrices/", rows_axis, "/", columns_axis, "/", name)
    node <- zarr_v3_read_node(store, base)
    if (is.null(node)) {
        .require_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
    }
    if (identical(node$node_type, "array")) {
        return(.zarr_get_dense_matrix(store, base, node))
    }
    nr <- as.integer(format_axis_length(daf, rows_axis))
    nc <- as.integer(format_axis_length(daf, columns_axis))
    .zarr_get_sparse_matrix(store, base, nr, nc)
}

.zarr_get_dense_matrix <- function(store, base, node) {
    # On-disk shape is reversed: [n_cols, n_rows]; chunk bytes are column-major.
    on_disk_d0 <- as.integer(node$shape[[1L]])   # n_cols
    on_disk_d1 <- as.integer(node$shape[[2L]])   # n_rows
    nr <- on_disk_d1; nc <- on_disk_d0
    total <- nr * nc
    chunk_path <- zarr_v3_chunk_path(base, 2L)
    is_string <- identical(node$data_type, "string")
    if (!is_string) {
        mm <- .zarr_try_mmap_dense(store, chunk_path, node$data_type, total, NULL)
        if (!is.null(mm)) { dim(mm) <- c(nr, nc); return(mm) }
    }
    bytes <- store_get_bytes(store, chunk_path)
    if (is.null(bytes)) stop(sprintf("matrix at %s missing chunk", sQuote(base)),
                             call. = FALSE)
    flat <- if (is_string) zarr_v3_decode_strings(bytes, n = total) else
        zarr_v3_decode_chunk(bytes, node$data_type, n = total)
    dim(flat) <- c(nr, nc)
    flat
}
```

For `.zarr_get_sparse_matrix`, take the current body (`R/zarr_format.R:916-972`) and translate it: `zarr_v2_read_zarray`→`zarr_v3_read_array`, `paste0(p,"/0")`→`zarr_v3_chunk_path(p,1L)`, `$dtype`→`$data_type`, `zarr_v2_decode_chunk`→`zarr_v3_decode_chunk`, the `.zarray` existence check for `nzval`→`!is.null(zarr_v3_read_array(store, paste0(base,"/nzval")))`, and wrap the colptr/rowval results in `as.integer(...)` (they decode as `integer64`). The 1-based→0-based `- 1L` math and the `dgCMatrix`/`lgCMatrix` construction are unchanged.

Also update `.zarr_matrices_set` (`R/zarr_format.R:847-865`): read the current body and replace the `.zarray`/`.zgroup` rel-path patterns with the `zarr.json` equivalent used in `.zarr_vectors_set` above (a matrix property is `<name>/zarr.json` under `matrices/<rows>/<cols>/`).

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-read.R
git commit -m "feat(zarr-v3): dense + sparse matrix read"
```

### Task 2.5: mmap fast-path port

**Files:**
- Modify: `R/zarr_format.R:556-566` (`.zarr_try_mmap_dense`)
- Test: `tests/testthat/test-zarr-v3-read.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("v3 flat dense float64 vector reads via the mmap ALTREP fast path", {
  withr::local_options(list(dafr.mmap = TRUE))
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  v <- get_vector(d, "cell", "score")
  expect_true(is_altrep_cpp(v) || isTRUE(all.equal(unname(v), c(1.5,2.5,3.5))))
})
```
(If `dafr.mmap` is off by default or `is_altrep_cpp` is internal, assert correctness only; the ALTREP assertion is a bonus.)

- [ ] **Step 2: Run test to verify it fails / passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: the read may already work via the decode fallback; the point of this task is to make the fast path fire for v3 dtype names.

- [ ] **Step 3: Update the dtype gate** at `R/zarr_format.R:560` from v2 numpy names to v3 names:

```r
.zarr_try_mmap_dense <- function(store, chunk_key, dtype, n, compressor) {
    if (!isTRUE(dafr_opt("dafr.mmap"))) return(NULL)
    if (!is.null(compressor)) return(NULL)               # uncompressed only
    if (n <= 0L) return(NULL)
    if (!dtype %in% c("float64", "int32")) return(NULL)  # mmap-able fixed-width
    file <- .zarr_chunk_file(store, chunk_key)
    if (is.null(file)) return(NULL)
    elt <- if (dtype == "float64") 8L else 4L
    if (file.size(file) < as.numeric(n) * elt) return(NULL)
    if (dtype == "float64") mmap_real(file, n) else mmap_int(file, n)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-read.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-read.R
git commit -m "feat(zarr-v3): port mmap zero-copy fast path to v3 dtype names + c/0 keys"
```

### Task 2.6: full-store read integration test

**Files:**
- Test: `tests/testthat/test-zarr-v3-read.R`

- [ ] **Step 1: Write the test** (all-in-one read of the fixture):

```r
test_that("v3 flat fixture reads end-to-end", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expect_setequal(scalars_set(d), c("title", "n"))
  expect_setequal(axes_set(d), c("cell", "gene"))
  expect_equal(get_scalar(d, "title"), "hello")
  expect_equal(unname(get_vector(d, "cell", "score")), c(1.5, 2.5, 3.5))
  expect_equal(dim(get_matrix(d, "cell", "gene", "expr")), c(3L, 2L))
  expect_equal(dim(get_matrix(d, "cell", "gene", "sp")), c(3L, 2L))
})
```
(Use the actual public introspection names from `R/format_api.R` — `scalars_set`/`axes_set` may be `scalar_names`/`axis_names`; grep first and match.)

- [ ] **Step 2: Run** — Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-zarr-v3-read.R
git commit -m "test(zarr-v3): end-to-end read of DAF 0.3.0 flat fixture"
```

---

# Phase 3: Flat write orchestration + round-trip

Goal: `zarr_daf(<dir>, "w")` writes a v3 store that DAF 0.3.0 opens and reads back identically. Re-enable the interop round-trip tests.

### Task 3.1: scalar + axis write

**Files:**
- Modify: `R/zarr_format.R:315-329` (`.zarr_write_scalar`), `R/zarr_format.R:372-380` (`.zarr_axes_set` / axis write path)
- Test: `tests/testthat/test-zarr-v3-write.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-zarr-v3-write.R
test_that("v3 scalar + axis write then read back in-process", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_scalar(d, "title", "hello")
  set_scalar(d, "n", 7L)
  d2 <- zarr_daf(dir, mode = "r")
  expect_equal(get_scalar(d2, "title"), "hello")
  expect_equal(as.integer(get_scalar(d2, "n")), 7L)
  expect_equal(axis_vector(d2, "cell"), c("c1", "c2", "c3"))
  # on-disk shape is the v3 layout
  expect_true(file.exists(file.path(dir, "scalars", "n", "zarr.json")))
  expect_true(file.exists(file.path(dir, "scalars", "n", "c", "0")))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-write.R")'`
Expected: FAIL — write helpers still emit `.zarray`/`/0`.

- [ ] **Step 3: Read the current `.zarr_write_scalar` and axis-write paths**, then rewrite to v3. A scalar is a shape-`[1]` array; an axis is a shape-`[N]` string array. Use `zarr_v3_array_meta` + `zarr_v3_write_array` + `zarr_v3_chunk_path(base, 1L)`, then refresh consolidated metadata:

```r
.zarr_write_scalar <- function(store, name, value) {
    base <- paste0("scalars/", name)
    dtype <- zarr_v3_dtype_for_r(value)
    meta <- zarr_v3_array_meta(shape = length(value), dtype = dtype)
    zarr_v3_write_array(store, base, meta)
    chunk <- if (dtype == "string") zarr_v3_encode_strings(value) else
        zarr_v3_encode_chunk(value, dtype)
    store_set_bytes(store, zarr_v3_chunk_path(base, 1L), chunk)
    zarr_v3_write_consolidated(store)
}
```
For the axis write path (read the current `.zarr_axes_set`/axis-entry writer first), write `axes/<axis>` as a `string` array of the entries, chunk `c/0`, then `zarr_v3_write_consolidated(store)`.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-write.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-write.R
git commit -m "feat(zarr-v3): scalar + axis write"
```

### Task 3.2: dense + sparse vector write

**Files:**
- Modify: `R/zarr_format.R:726-765` (`.zarr_write_dense_vector`, `.zarr_write_sparse_vector`)
- Test: `tests/testthat/test-zarr-v3-write.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("v3 dense + sparse vector write round-trips in-process", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_vector(d, "cell", "score", c(1.5, 2.5, 3.5))
  set_vector(d, "cell", "sparse_v", Matrix::sparseVector(c(4, 9), c(1, 3), 3))
  d2 <- zarr_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "score")), c(1.5, 2.5, 3.5))
  expect_equal(unname(as.numeric(get_vector(d2, "cell", "sparse_v"))),
               c(4, 0, 9))
})
```
(Use whatever sparse-vector input type the existing `set_vector` path accepts; grep `.zarr_write_sparse_vector` for the expected class and match it.)

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-v3-write.R")'`
Expected: FAIL.

- [ ] **Step 3: Read the current vector write helpers (`R/zarr_format.R:726-765`)** and translate them: dense uses `zarr_v3_array_meta` + `zarr_v3_chunk_path(base,1L)`; sparse writes a group marker (`zarr_v3_write_group(store, base)`) + `nzind`/`nzval` arrays. Match DAF's sparse component dtypes: **`nzind` is `int64`, 1-based**; `nzval` matches the value dtype; omit `nzval` for all-true Bool. End each with `zarr_v3_write_consolidated(store)`. Concretely for the dense path:

```r
.zarr_write_dense_vector <- function(store, base, vec) {
    dtype <- zarr_v3_dtype_for_r(vec)
    meta <- zarr_v3_array_meta(shape = length(vec), dtype = dtype)
    zarr_v3_write_array(store, base, meta)
    chunk <- if (dtype == "string") zarr_v3_encode_strings(vec) else
        zarr_v3_encode_chunk(vec, dtype)
    store_set_bytes(store, zarr_v3_chunk_path(base, 1L), chunk)
    zarr_v3_write_consolidated(store)
}
```
For sparse, mirror the current `.zarr_write_sparse_vector` structure but write `nzind` as `int64` (`bit64::as.integer64(idx_1based)`), `zarr_v3_array_meta(shape=k, dtype="int64")`, chunk via `zarr_v3_encode_chunk(.., "int64")`.

- [ ] **Step 4: Run test to verify it passes** — Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-write.R
git commit -m "feat(zarr-v3): dense + sparse vector write (int64 nzind)"
```

### Task 3.3: dense + sparse matrix write

**Files:**
- Modify: `R/zarr_format.R:1017-1090` (matrix delete/write paths)
- Test: `tests/testthat/test-zarr-v3-write.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("v3 dense + sparse matrix write round-trips in-process", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "expr", matrix(c(1, 3, 5, 2, 4, 6), nrow = 3))
  set_matrix(d, "cell", "gene", "sp",
             Matrix::sparseMatrix(i = c(3, 1), j = c(1, 2), x = c(5, 2),
                                  dims = c(3, 2)))
  d2 <- zarr_daf(dir, mode = "r")
  expect_equal(unname(as.matrix(get_matrix(d2, "cell", "gene", "expr"))),
               matrix(c(1, 3, 5, 2, 4, 6), nrow = 3))
  expect_equal(unname(as.matrix(get_matrix(d2, "cell", "gene", "sp"))),
               matrix(c(0, 0, 5, 2, 0, 0), nrow = 3))
  # on-disk shape is reversed [n_cols, n_rows]
  m <- jsonlite::fromJSON(file.path(dir, "matrices", "cell", "gene", "expr",
                                    "zarr.json"))
  expect_equal(as.integer(m$shape), c(2L, 3L))
})
```

- [ ] **Step 2: Run test to verify it fails** — Expected: FAIL.

- [ ] **Step 3: Update the matrix delete + write paths.** In `format_set_matrix` (`R/zarr_format.R:1017-1023`) the delete branch must drop `zarr.json` + `c/0/0` (dense) or list-and-delete (sparse) instead of `.zarray`/`0.0`. Then rewrite the writers:

```r
.zarr_write_dense_matrix <- function(store, base, mat, nr, nc) {
    dimnames(mat) <- NULL
    flat <- as.vector(mat)                # R column-major == Julia column-major
    dtype <- zarr_v3_dtype_for_r(flat)
    # On-disk shape reversed [n_cols, n_rows] (matches DAF; no `order` field).
    meta <- zarr_v3_array_meta(shape = c(nc, nr), dtype = dtype)
    zarr_v3_write_array(store, base, meta)
    chunk <- if (dtype == "string") zarr_v3_encode_strings(flat) else
        zarr_v3_encode_chunk(flat, dtype)
    store_set_bytes(store, zarr_v3_chunk_path(base, 2L), chunk)
    zarr_v3_write_consolidated(store)
}

.zarr_write_sparse_matrix <- function(store, base, mat) {
    zarr_v3_write_group(store, base)
    is_lg <- methods::is(mat, "lgCMatrix")
    is_all_true_bool <- is_lg && length(mat@x) > 0L && all(mat@x, na.rm = FALSE)
    # colptr + rowval: int64, 1-based (DAF convention).
    colptr_1 <- bit64::as.integer64(mat@p + 1L)
    zarr_v3_write_array(store, paste0(base, "/colptr"),
                        zarr_v3_array_meta(length(colptr_1), "int64"))
    store_set_bytes(store, zarr_v3_chunk_path(paste0(base, "/colptr"), 1L),
                    zarr_v3_encode_chunk(colptr_1, "int64"))
    rowval_1 <- bit64::as.integer64(mat@i + 1L)
    zarr_v3_write_array(store, paste0(base, "/rowval"),
                        zarr_v3_array_meta(length(rowval_1), "int64"))
    store_set_bytes(store, zarr_v3_chunk_path(paste0(base, "/rowval"), 1L),
                    zarr_v3_encode_chunk(rowval_1, "int64"))
    if (!is_all_true_bool) {
        nzval_dtype <- if (is_lg) "bool" else zarr_v3_dtype_for_r(mat@x)
        zarr_v3_write_array(store, paste0(base, "/nzval"),
                            zarr_v3_array_meta(length(mat@x), nzval_dtype))
        store_set_bytes(store, zarr_v3_chunk_path(paste0(base, "/nzval"), 1L),
                        zarr_v3_encode_chunk(mat@x, nzval_dtype))
    }
    zarr_v3_write_consolidated(store)
}
```

- [ ] **Step 4: Run test to verify it passes** — Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-write.R
git commit -m "feat(zarr-v3): dense + sparse matrix write (reversed shape, int64 CSC)"
```

### Task 3.4: sweep remaining v2 write artifacts (delete paths, vector delete)

**Files:**
- Modify: `R/zarr_format.R` (any remaining `.zarray`/`.zgroup`/`/0`/`zarr_v2_*`/`.ZARR_ZGROUP_BYTES`/`zarr_v2_write_zmetadata` references)
- Test: existing `tests/testthat/test-zarr-v3-write.R` + a delete test

- [ ] **Step 1: Find every remaining v2 reference.**

Run: `grep -nE "zarr_v2_|\.zarray|\.zgroup|\.zmetadata|ZARR_ZGROUP|/0\b|/0\.0|/\\.zattrs" R/zarr_format.R`
Expected: a short list (vector/matrix delete helpers, `format_relayout_matrix`, description header). Each must be translated to its v3 equivalent (`zarr.json`, `c/0`, `c/0/0`, `zarr_v3_*`, `zarr_v3_write_consolidated`).

- [ ] **Step 2: Write a delete round-trip test.**

```r
test_that("v3 vector + matrix delete remove the node and refresh the index", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2"))
  set_vector(d, "cell", "v", c(1, 2)); delete_vector(d, "cell", "v")
  expect_false(has_vector(d, "cell", "v"))
  expect_false(file.exists(file.path(dir, "vectors", "cell", "v", "zarr.json")))
})
```

- [ ] **Step 3: Translate each remaining reference** (read each helper, swap to v3 keys/funcs). For deletes, remove `<base>/zarr.json` + `<base>/c/0[/0]` (dense) or list-and-delete `<base>` subtree (sparse), then `zarr_v3_write_consolidated(store)`.

- [ ] **Step 4: Run the full zarr test files.**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter="zarr-v3")'`
Expected: PASS. Also `grep` from Step 1 returns nothing.

- [ ] **Step 5: Commit**

```bash
git add R/zarr_format.R tests/testthat/test-zarr-v3-write.R
git commit -m "refactor(zarr-v3): port remaining delete/relayout paths off v2"
```

### Task 3.5: Julia round-trip (dafr writes, DAF reads) + flip skip guard

**Files:**
- Modify: `tests/testthat/helper-julia.R` (the `.daf_jl_uses_zarr_v3` guard usage), `tests/testthat/test-zarr-julia-interop.R`
- Test: `tests/testthat/test-zarr-julia-interop.R`

- [ ] **Step 1: Read `tests/testthat/test-zarr-julia-interop.R` and `helper-julia.R`** to see how the three round-trip tests currently `skip` when `.daf_jl_uses_zarr_v3()` is TRUE. The guard must invert: **run** when the env DAF is v3 (≥ 0.3.0), **skip** when it is older.

- [ ] **Step 2: Flip the guard.** Wherever a test does `skip_if(.daf_jl_uses_zarr_v3(), "...")`, change to `skip_if_not(.daf_jl_uses_zarr_v3(), "requires DAF >= 0.3.0 (Zarr v3)")`. (Keep `.daf_jl_uses_zarr_v3()` itself; only its polarity in the guards changes.)

- [ ] **Step 3: Confirm the round-trip assertions.** The existing tests already write a store from R and read it in Julia (and vice versa). With write now producing v3, they should pass. If a test wrote v2-specific expectations (e.g. asserting `.zarray` exists), update it to the v3 path (`zarr.json`).

- [ ] **Step 4: Run the interop tests against the live env.**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-zarr-julia-interop.R")'`
Expected: PASS (round-trips now execute instead of skipping). If `close()`/finalizer issues surface from the Julia side, capture the message and resolve (it is a real interop blocker for this task, not optional).

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test-zarr-julia-interop.R tests/testthat/helper-julia.R
git commit -m "test(zarr-v3): run R<->Julia round-trips on DAF >= 0.3.0 (Zarr v3)"
```

---

# Phase 4: Remove v2 + housekeeping

### Task 4.1: delete `zarr_v2.R` and its tests

**Files:**
- Delete: `R/zarr_v2.R`, `tests/testthat/test-zarr-v2.R`, `tests/testthat/test-zarr-v2-strings.R`, `tests/testthat/test-zarr-zmetadata.R`
- Modify: any `@include`/collation references; `tests/testthat/test-zarr-python.R` and `test-zarr-convert.R` if they assert v2

- [ ] **Step 1: Find every reference to the v2 layer.**

Run: `grep -rnE "zarr_v2|test-zarr-v2|zmetadata|\.zarray|\.zgroup" R/ tests/ NAMESPACE DESCRIPTION`
Expected: references only in the files to delete plus any collation entry.

- [ ] **Step 2: Delete the v2 source + tests.**

```bash
git rm R/zarr_v2.R tests/testthat/test-zarr-v2.R \
       tests/testthat/test-zarr-v2-strings.R tests/testthat/test-zarr-zmetadata.R
```
For `test-zarr-python.R` / `test-zarr-convert.R`: read them; if they assert the v2 on-disk shape, either port the assertions to v3 or remove the v2-only cases (note removals in the commit message).

- [ ] **Step 3: Re-document + run the whole suite.**

Run:
```bash
Rscript -e 'devtools::document("."); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```
Expected: green. Investigate any failure (a missed v2 reference, a collation gap).

- [ ] **Step 4: Verify the grep from Step 1 now returns nothing** (outside historical NEWS).

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "refactor(zarr): remove the Zarr v2 codec layer (clean break to v3)"
```

### Task 4.2: docs, version, environment

**Files:**
- Modify: `NEWS.md`, `DESCRIPTION` (Version), `environment.yml`, `R/zarr_store.R:1-9` (comment wording)

- [ ] **Step 1: Update `R/zarr_store.R:1-9`** — replace "Zarr v2" wording with "Zarr" (the store layer is format-version-agnostic). No behavior change.

- [ ] **Step 2: Add a `NEWS.md` entry** at the top:

```markdown
# dafr (development)

## ZarrDaf: Zarr v3 (DataAxesFormats.jl 0.3.0 interop)

* ZarrDaf now reads and writes the **Zarr v3** on-disk format used by
  DataAxesFormats.jl 0.3.0 (single `zarr.json` per node, `c/`-prefixed chunk
  keys, the `daf` version marker as a root-group attribute, inline consolidated
  metadata). Flat (uncompressed) read and write are supported.
* **Breaking:** the legacy Zarr v2 reader/writer is removed. Opening a Zarr v2
  `.daf.zarr` now errors with a conversion hint (`python -m zarr v2_to_v3`),
  matching DataAxesFormats.jl 0.3.0's own behaviour.
* Reading packed/sharded (`packed=true`) v3 stores is not yet supported (a
  follow-up); default DAF writes are flat and read fully.
```

- [ ] **Step 3: Bump `DESCRIPTION` Version** (e.g. `0.3.1` → `0.4.0` for the breaking format change; match the project's scheme — check the current value first).

- [ ] **Step 4: Add a note to `environment.yml`** that `c-blosc` will be required for the (future) sharded-read capability, so the conda path is ready:

```yaml
  # - c-blosc        # uncomment to enable reading packed/sharded ZarrDaf (v3)
```

- [ ] **Step 5: Run `R CMD check` (no Julia, no blosc) to confirm the flat core is self-contained and CRAN-clean.**

Run: `Rscript -e 'devtools::check(".", args = c("--no-manual"), error_on = "warning")'`
Expected: 0 errors, 0 warnings. NOTES reviewed.

- [ ] **Step 6: Commit**

```bash
git add NEWS.md DESCRIPTION environment.yml R/zarr_store.R
git commit -m "docs(zarr-v3): NEWS + version bump + environment note for v3 port"
```

---

## Self-review checklist (run after implementing)

- **Spec coverage:** flat read (§2 phase 2), flat write (§3), v2 rejection/clean break (Task 2.1, 4.1), mmap (Task 2.5), consolidated metadata write + tree-walk read (Task 1.4, 2.x), dtype map (Task 1.1), dense byte-order pin (Task 2.4 test), Julia round-trips (Task 3.5). Sharded read (spec phases 5-6) is intentionally a separate follow-up plan.
- **Type consistency:** the codec API names used across phases — `zarr_v3_read_node`, `zarr_v3_read_array`, `zarr_v3_array_meta`, `zarr_v3_write_array`, `zarr_v3_write_group`, `zarr_v3_write_root`, `zarr_v3_write_consolidated`, `zarr_v3_daf_marker_exists`, `zarr_v3_daf_version`, `zarr_v3_chunk_path`, `zarr_v3_encode_chunk`/`decode_chunk`, `zarr_v3_encode_strings`/`decode_strings` — are defined in Phase 1 and used unchanged in Phases 2-4.
- **No placeholders:** every code step shows real code; "read the current body and translate X→Y" steps name exact source line ranges and the exact substitutions.

## Known follow-ups (out of scope here)

- **Sharded/packed read** (spec phases 5-6): `configure`-gated optional system c-blosc + crc32c + `R/zarr_sharded.R`. Separate plan, written after this core lands.
- If DAF rejects an int32 sparse index or a missing `consolidated_metadata`, tighten Task 3.x against the round-trip failure (the round-trip test is the gate).
