# Julia JSON-schema parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make dafr read/write DataAxesFormats.jl's exact JSON for the `base_daf_view` chain spec (Track B) and the root `metadata.json` store index (Track A), verified bidirectionally against live DAF 0.3.0.

**Architecture:** Two independent tracks. Track B rewrites the view-spec serializer/parser in `R/complete.R` to Julia's object form. Track A replaces the `metadata.zip` bundle with Julia's single root `metadata.json` object (inlining the per-property descriptors dafr already writes), swaps the writer call sites, repoints HttpDaf, and deletes the zip module. Clean break: no metadata.zip.

**Tech Stack:** R (jsonlite, S7), testthat, live DataAxesFormats.jl 0.3.0 via `conda run -n dafr-mcview julia`.

---

## Verified ground truth (live DAF 0.3.0, 2026-06-19) - do not re-derive

### Track B - `base_daf_view` scalar JSON (Julia writer output)
For `axes=["cell"=>"=","renamed_cell"=>"@ cell"]`,
`data=["quality"=>"=", ("cell","age")=>"=", ("cell","gene","umi")=>"="]`:
```json
{"axes":{"cell":"=","renamed_cell":"@ cell"},"data":{"quality":"=","(\"cell\", \"age\")":"=","(\"cell\", \"gene\", \"umi\")":"="}}
```
- `axes`/`data` are JSON **objects** (not arrays). Omit a key entirely if empty.
- data key: scalar -> plain name; vector -> `("cell", "age")`; matrix -> `("cell", "gene", "umi")` (literal parens, double-quoted elements, `, ` separators).
- Julia reader maps `(`->`[`, `)`->`]`, JSON-parses the key back to a tuple.
- dafr CURRENTLY writes positional arrays (`{"axes":[["cell","="]],"data":[[["cell","gene","umi"],"="]]}`) - different; this is real work.

### Track A - root `metadata.json` (Julia writer output)
A Julia FilesDaf with axes cell(5)/gene(3), scalars title="hello"/k=Int32(7), dense vec score(Float64)/label(String), dense+sparse matrices produced:
```json
{"axes/cell":{"format":"axis","n_entries":5},
 "axes/gene":{"format":"axis","n_entries":3},
 "scalars/title":{"type":"String","value":"hello"},
 "scalars/k":{"type":"Int32","value":7},
 "vectors/cell/score":{"format":"dense","eltype":"Float64"},
 "vectors/cell/label":{"format":"dense","eltype":"String"},
 "matrices/cell/gene/dense":{"format":"dense","eltype":"Float64"},
 "matrices/gene/cell/dense":{"format":"dense","eltype":"Float64"},
 "matrices/cell/gene/sp":{"format":"sparse","colptr":{"format":"dense","eltype":"Int64","n_elements":4},"rowval":{"format":"dense","eltype":"Int64","n_elements":2},"nzval":{"format":"dense","eltype":"Float64","n_elements":2}},
 "matrices/gene/cell/sp":{...}}
```
- Single object keyed by relative property path, sorted by key.
- Matrices appear in BOTH orientations (relayout); the rebuild walks every `matrices/<r>/<c>/` dir.
- **KEY SIMPLIFICATION**: dafr's existing on-disk per-property descriptors ALREADY match the inlined values:
  - `scalars/<name>.json` = `{"type":"Int32","value":7}` (dafr `.write_scalar_json`, files_io.R:282-315) - identical.
  - `vectors/.../<name>.json` = `{"format":"dense","eltype":"Float64"}` / sparse with `n_elements` (dafr `.write_descriptor_dense`/`.write_descriptor_sparse`, files_io.R:81,101-115) - identical (modulo the accepted UInt16 indtype divergence).
  - So `metadata.json` is built by INLINING each existing `.json` verbatim, keyed by its rel-path-minus-`.json`; only the axis descriptor `{"format":"axis","n_entries":N}` is computed (`N = length(readLines(axes/<axis>.txt))`).

### dafr current state (file:line)
- `R/complete.R`: writer L60-63 (`jsonlite::toJSON(list(axes=axes,data=data),auto_unbox=TRUE)`), reader L165-177 (`fromJSON(...,simplifyVector=FALSE)` + `.normalise_json_spec` L194-209).
- `R/view_daf.R` `.parse_view_item` L515-524: a view item is `list(key,value)` (positional) or `list(name=value)`; key is a string (axis/scalar) or length-2 (vector) / length-3 (matrix) char vector.
- `R/files_metadata_zip.R`: `.write_axes_metadata` L11-21, `.metadata_zip_rebuild` L25-109, `.metadata_zip_append` L120-158, `.ensure_metadata_zip` L164-169, `pack_files_daf_metadata` L192-207.
- Call sites in `R/files_daf_write.R`: set_scalar L14 (append), delete_scalar L30 (rebuild), add_axis L96 (rebuild), delete_axis L131 (rebuild), set_vector L272 (append), delete_vector L318 (rebuild), set_matrix L389/L414 (append), delete_matrix L436 (rebuild), reorder cleanup L738 (rebuild), reorder recovery L805 (rebuild). Plus `R/files_daf.R` writable-open `.ensure_metadata_zip`.
- `R/http_format.R`: `http_daf()` L55-119 fetches `metadata.zip`, `unzip(list=TRUE)`, reads members via `.http_zip_json(daf, relpath)` L127-130; scalar decode `.http_scalar_from_json` L146-168; axis list from `axes/metadata.json` in the zip.
- `R/files_daf_read.R`: local reads tree-walk (scalar L48-66/.read_scalar_json L324-381, axis L82-154, vector L394-438, matrix L502-644) - NO metadata dependency.
- dtype names are Julia names via `.dtype_for_r_vector` (files_io.R:50-70) / `.dtype_canonical` (L14-32).

---

## File Structure
- **Modify** `R/complete.R` - Track B writer + reader (add `.view_spec_to_julia_json`, `.view_data_key`, `.view_spec_from_julia_json`; drop `.normalise_json_spec`).
- **Create** `R/files_metadata_json.R` - Track A index module (rebuild/append/ensure/read + `pack_files_daf_metadata`).
- **Delete** `R/files_metadata_zip.R`.
- **Modify** `R/files_daf.R`, `R/files_daf_write.R` - swap `_zip_` call sites to `_json_`.
- **Modify** `R/http_format.R` - fetch + serve from `metadata.json`.
- **Tests**: `test-complete*.R` (update), new `test-json-parity-interop.R`; rename `test-files-metadata-zip.R` -> `test-files-metadata-json.R`; update `test-http-daf.R`.
- **Modify** `NEWS.md`, `DESCRIPTION` (version), `dev/parity-audit-2026-06-11/REMAINING-GAP.md`.

Test invocation everywhere: `BLOSC_HOME=/home/aviezerl/.julia/artifacts/b50f03cd3f0ce8f8e4dc931a016a2ff30de18fc3 NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/<f>.R")'` (BLOSC_HOME keeps the build consistent; ignore ".bashrc bind" lines).

---

# Phase 1 - Track B: base_daf_view (Julia object schema)

## Task 1: View-spec writer (dafr spec -> Julia object JSON)

**Files:** Modify `R/complete.R`; Test `tests/testthat/test-complete-view-json.R` (new)

- [ ] **Step 1: Failing test** `tests/testthat/test-complete-view-json.R`:
```r
test_that(".view_spec_to_julia_json matches the Julia object schema", {
    axes <- list(list("cell", "="), list("renamed_cell", "@ cell"))
    data <- list(list("quality", "="), list(c("cell", "age"), "="),
                 list(c("cell", "gene", "umi"), "="))
    js <- dafr:::.view_spec_to_julia_json(axes, data)
    obj <- jsonlite::fromJSON(js, simplifyVector = FALSE)
    expect_equal(obj$axes$cell, "=")
    expect_equal(obj$axes$renamed_cell, "@ cell")
    expect_equal(obj$data$quality, "=")
    expect_equal(obj$data[['("cell", "age")']], "=")
    expect_equal(obj$data[['("cell", "gene", "umi")']], "=")
    # empty axes/data omitted entirely
    expect_false(grepl("data", dafr:::.view_spec_to_julia_json(axes, NULL)))
})
```

- [ ] **Step 2: Run - expect FAIL.**
`BLOSC_HOME=... NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-complete-view-json.R")'`

- [ ] **Step 3: Implement** in `R/complete.R` (above `complete_chain`):
```r
# Julia data-key for a view-data entry key: a scalar name stays a plain string;
# a 2/3-element vector/matrix key becomes the stringified Julia tuple, e.g.
# c("cell","age") -> '("cell", "age")' (matches DataAxesFormats JSON.json of a
# Tuple dict key; the reader maps ()->[] and JSON-parses it back).
.view_data_key <- function(key) {
    if (length(key) == 1L) return(as.character(key))
    paste0("(", paste0('"', key, '"', collapse = ", "), ")")
}

# Serialize viewer axes/data (each a list of list(key,value) or list(name=value)
# items) to Julia's base_daf_view object JSON: {"axes":{name:query},
# "data":{datakey:query}}. Empty axes/data are omitted.
.view_spec_to_julia_json <- function(axes, data) {
    to_obj <- function(items, is_data) {
        if (is.null(items) || length(items) == 0L) return(NULL)
        parsed <- lapply(items, .parse_view_item)
        keys <- vapply(parsed, function(p)
            if (is_data) .view_data_key(p$key) else as.character(p$key),
            character(1L))
        vals <- lapply(parsed, function(p) jsonlite::unbox(as.character(p$value)))
        stats::setNames(vals, keys)
    }
    spec <- list()
    a <- to_obj(axes, FALSE); if (!is.null(a)) spec$axes <- a
    d <- to_obj(data, TRUE);  if (!is.null(d)) spec$data <- d
    as.character(jsonlite::toJSON(spec, auto_unbox = TRUE))
}
```
(`.parse_view_item` already exists in R/view_daf.R and is package-internal.)

- [ ] **Step 4: Run - expect PASS.**

- [ ] **Step 5: Commit.**
```bash
git add R/complete.R tests/testthat/test-complete-view-json.R
git commit -m "feat(json-parity): base_daf_view writer in Julia object schema"
```

## Task 2: View-spec reader (Julia object JSON -> dafr spec)

**Files:** Modify `R/complete.R`; Test `tests/testthat/test-complete-view-json.R`

- [ ] **Step 1: Failing test** (append) - round-trip + parses the literal Julia bytes from the ground truth:
```r
test_that(".view_spec_from_julia_json round-trips and reads Julia bytes", {
    julia <- '{"axes":{"cell":"=","renamed_cell":"@ cell"},"data":{"quality":"=","(\\"cell\\", \\"age\\")":"=","(\\"cell\\", \\"gene\\", \\"umi\\")":"="}}'
    spec <- jsonlite::fromJSON(julia, simplifyVector = FALSE)
    ax <- dafr:::.view_spec_from_julia_json(spec$axes, is_data = FALSE)
    dt <- dafr:::.view_spec_from_julia_json(spec$data, is_data = TRUE)
    # axes: list of list(name, query)
    expect_equal(ax[[1]], list("cell", "="))
    expect_equal(ax[[2]], list("renamed_cell", "@ cell"))
    # data: scalar key is a string; vector/matrix keys are char vectors
    expect_equal(dt[[1]], list("quality", "="))
    expect_equal(dt[[2]], list(c("cell", "age"), "="))
    expect_equal(dt[[3]], list(c("cell", "gene", "umi"), "="))
})
```

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Implement** in `R/complete.R` (replace `.normalise_json_spec`):
```r
# Decode a Julia data-key back to a dafr view key: a tuple-encoded string
# '("cell", "age")' -> c("cell","age"); a plain name stays a string. Mirrors
# Julia's parse: map ()->[] and JSON-parse.
.view_decode_key <- function(key) {
    if (startsWith(key, "(") && endsWith(key, ")")) {
        bracketed <- paste0("[", substr(key, 2L, nchar(key) - 1L), "]")
        return(unlist(jsonlite::fromJSON(bracketed), use.names = FALSE))
    }
    key
}

# Parse a Julia base_daf_view object (axes or data) into dafr's viewer spec form:
# a list of list(key, query). `spec_obj` is the parsed named list (from
# fromJSON(simplifyVector=FALSE)); names are the keys, values the query strings.
.view_spec_from_julia_json <- function(spec_obj, is_data) {
    if (is.null(spec_obj) || length(spec_obj) == 0L) return(NULL)
    keys <- names(spec_obj)
    lapply(seq_along(spec_obj), function(i) {
        k <- if (is_data) .view_decode_key(keys[[i]]) else keys[[i]]
        list(k, as.character(spec_obj[[i]]))
    })
}
```

- [ ] **Step 4: Run - expect PASS.** Commit.
```bash
git add R/complete.R tests/testthat/test-complete-view-json.R
git commit -m "feat(json-parity): base_daf_view reader for Julia object schema"
```

## Task 3: Wire writer/reader into complete_chain / complete_daf

**Files:** Modify `R/complete.R`; Test `tests/testthat/test-complete-view-roundtrip.R`

- [ ] **Step 1: Failing test** (append to test-complete-view-roundtrip.R) - the on-disk scalar is now Julia-shaped and complete_daf still round-trips:
```r
test_that("complete_chain writes Julia-schema base_daf_view and reopens", {
    root <- withr::local_tempdir()
    bdir <- file.path(root, "base"); ndir <- file.path(root, "new")
    base <- files_daf(bdir, name = "base", mode = "w+")
    add_axis(base, "cell", paste0("c", 1:4)); add_axis(base, "gene", paste0("g", 1:3))
    set_matrix(base, "cell", "gene", "expr", matrix(as.numeric(1:12), 4, 3))
    new <- files_daf(ndir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE,
                   axes = list(list("cell", "=")),
                   data = list(list(c("cell", "gene", "expr"), "=")))
    raw <- format_get_scalar(open_daf(ndir, "r"), "base_daf_view")$value
    obj <- jsonlite::fromJSON(raw, simplifyVector = FALSE)
    expect_equal(obj$axes$cell, "=")                       # object, not array
    expect_equal(obj$data[['("cell", "gene", "expr")']], "=")
    ch <- complete_daf(ndir, "r")
    expect_equal(as.numeric(get_matrix(ch, "cell", "gene", "expr")),
                 as.numeric(1:12))
})
```

- [ ] **Step 2: Run - expect FAIL** (writer still emits arrays).

- [ ] **Step 3: Wire it.** In `R/complete.R`:
  - Writer (L60-63): replace the `spec <- list(...); jsonlite::toJSON(...)` with
    `format_set_scalar(new_daf, "base_daf_view", .view_spec_to_julia_json(axes, data), overwrite = TRUE)`.
  - Reader (L175-177): replace the `.normalise_json_spec(spec$axes)` / `.normalise_json_spec(spec$data)` args with
    `axes = .view_spec_from_julia_json(spec$axes, is_data = FALSE)` and
    `data = .view_spec_from_julia_json(spec$data, is_data = TRUE)`.
  - Delete the now-unused `.normalise_json_spec`.

- [ ] **Step 4: Run - expect PASS.** Also run the full `test-complete*.R` to confirm no regression.

- [ ] **Step 5: Commit.**
```bash
git add R/complete.R tests/testthat/test-complete-view-roundtrip.R
git commit -m "feat(json-parity): complete_chain/complete_daf use Julia base_daf_view schema"
```

## Task 4: Track B live interop (Julia <-> dafr)

**Files:** Test `tests/testthat/test-json-parity-interop.R` (new)

- [ ] **Step 1: Test** (uses the `run_julia` / `.daf_jl_uses_zarr_v3` helpers in `tests/testthat/helper-julia.R`):
```r
test_that("Julia reads a dafr-written complete_daf view; dafr reads Julia's", {
    skip_on_cran(); skip_if_not(.daf_jl_uses_zarr_v3())
    root <- withr::local_tempdir()
    bdir <- file.path(root, "base"); ndir <- file.path(root, "new")
    base <- files_daf(bdir, name = "base", mode = "w+")
    add_axis(base, "cell", paste0("c", 1:4)); add_axis(base, "gene", paste0("g", 1:3))
    set_matrix(base, "cell", "gene", "expr", matrix(as.numeric(1:12), 4, 3))
    new <- files_daf(ndir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE,
                   axes = list(list("cell", "=")),
                   data = list(list(c("cell", "gene", "expr"), "=")))
    # Julia opens the dafr-written chain via complete_daf and reads through the view.
    res <- run_julia(c(
        "using DataAxesFormats",
        sprintf('d = complete_daf(raw"%s", "r")', ndir),
        'm = get_matrix(d, "cell", "gene", "expr")',
        'println(size(m)==(4,3) && m[1,1]==1.0 && m[4,3]==12.0 ? "ALLOK" : "BAD $(m)")'))
    if (!any(grepl("ALLOK", res))) cat(paste(res, collapse="\n"), "\n")
    expect_true(any(grepl("ALLOK", res)))
})
```
(Confirm the Julia reopen API: `complete_daf(path, "r")`. If Julia's signature differs, adjust per `~/src/DataAxesFormats.jl/src/complete.jl`.)

- [ ] **Step 2: Run - expect PASS** (Julia env live). If Julia errors on the view spec, diff dafr's `base_daf_view` bytes against the §ground-truth and fix the writer.

- [ ] **Step 3: Commit.**
```bash
git add tests/testthat/test-json-parity-interop.R
git commit -m "test(json-parity): Julia reads dafr complete_daf view (interop)"
```

---

# Phase 2 - Track A: metadata.json (clean break from metadata.zip)

## Task 5: metadata.json rebuild (inline existing descriptors)

**Files:** Create `R/files_metadata_json.R`; Test `tests/testthat/test-files-metadata-json.R` (new)

- [ ] **Step 1: Failing test** - rebuild produces Julia's structure (pin against the §ground truth):
```r
test_that(".metadata_json_rebuild matches Julia's metadata.json structure", {
    root <- withr::local_tempdir()
    d <- files_daf(root, mode = "w+", name = "m")
    add_axis(d, "cell", paste0("c", 1:5)); add_axis(d, "gene", paste0("g", 1:3))
    set_scalar(d, "title", "hello"); set_scalar(d, "k", 7L)
    set_vector(d, "cell", "score", as.numeric(1:5))
    set_matrix(d, "cell", "gene", "dense", matrix(as.numeric(1:15), 5, 3))
    dafr:::.metadata_json_rebuild(root)
    m <- jsonlite::fromJSON(file.path(root, "metadata.json"), simplifyVector = FALSE)
    expect_equal(m[["axes/cell"]], list(format = "axis", n_entries = 5L))
    expect_equal(m[["scalars/title"]], list(type = "String", value = "hello"))
    expect_equal(m[["scalars/k"]], list(type = "Int32", value = 7L))
    expect_equal(m[["vectors/cell/score"]], list(format = "dense", eltype = "Float64"))
    expect_equal(m[["matrices/cell/gene/dense"]], list(format = "dense", eltype = "Float64"))
})
```

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Implement** `R/files_metadata_json.R`. The rebuild walks the tree; for axes it computes `{"format":"axis","n_entries":N}`; for scalars/vectors/matrices it INLINES the existing per-property `.json` (already Julia-shaped):
```r
# R/files_metadata_json.R
# Julia-compatible root metadata.json: a single JSON object mapping each relative
# property path to its descriptor (DataAxesFormats FilesFormat). Replaces the old
# metadata.zip bundle. dafr's per-property .json descriptors already match Julia's
# inlined values, so rebuild inlines them verbatim; only the axis descriptor
# (n_entries) is computed. Local FilesDaf reads tree-walk and do not need this;
# it exists for HttpDaf enumeration and Julia interop.

.METADATA_JSON <- "metadata.json"

# Read a per-property descriptor file's raw JSON text (already Julia-shaped).
.metadata_json_inline <- function(json_path) {
    paste(readLines(json_path, warn = FALSE), collapse = "")
}

# Collect (rel_key -> raw descriptor JSON) for the whole store, sorted by key.
.metadata_json_entries <- function(root) {
    ent <- list()
    # axes: {"format":"axis","n_entries":N}
    adir <- file.path(root, "axes")
    if (dir.exists(adir)) {
        for (f in sort(list.files(adir, pattern = "\\.txt$"))) {
            axis <- sub("\\.txt$", "", f)
            n <- length(readLines(file.path(adir, f), warn = FALSE))
            ent[[paste0("axes/", axis)]] <-
                sprintf('{"format":"axis","n_entries":%d}', n)
        }
    }
    # scalars / vectors / matrices: inline the existing .json descriptors.
    for (sub in c("scalars", "vectors", "matrices")) {
        sdir <- file.path(root, sub)
        if (!dir.exists(sdir)) next
        for (jf in sort(list.files(sdir, pattern = "\\.json$", recursive = TRUE))) {
            if (basename(jf) == "metadata.json") next   # legacy axes/metadata.json
            key <- paste0(sub, "/", sub("\\.json$", "", jf))
            ent[[key]] <- .metadata_json_inline(file.path(sdir, jf))
        }
    }
    ent[order(names(ent))]
}

# Assemble the JSON object text from the (key -> raw descriptor) map.
.metadata_json_assemble <- function(entries) {
    if (length(entries) == 0L) return("{}")
    body <- paste0('"', names(entries), '":', unlist(entries, use.names = FALSE),
                   collapse = ",")
    paste0("{", body, "}")
}

# Rebuild <root>/metadata.json from the tree (atomic via .new + rename).
.metadata_json_rebuild <- function(root) {
    text <- .metadata_json_assemble(.metadata_json_entries(root))
    tmp <- file.path(root, paste0(.METADATA_JSON, ".new"))
    writeLines(text, tmp, useBytes = TRUE)
    file.rename(tmp, file.path(root, .METADATA_JSON))
    invisible()
}
```
NOTE: `axes/metadata.json` from the old zip path must NOT be written anymore; if `.write_axes_metadata` is referenced elsewhere it is removed in Task 7. The `basename(jf) == "metadata.json"` guard skips any stale legacy file.

- [ ] **Step 4: Run - expect PASS.**

- [ ] **Step 5: Commit.**
```bash
git add R/files_metadata_json.R tests/testthat/test-files-metadata-json.R
git commit -m "feat(json-parity): metadata.json rebuild (inline descriptors, Julia schema)"
```

## Task 6: metadata.json append + ensure + migration helper

**Files:** Modify `R/files_metadata_json.R`; Test `tests/testthat/test-files-metadata-json.R`

- [ ] **Step 1: Failing test** (append):
```r
test_that(".metadata_json_append adds one entry without a full rebuild", {
    root <- withr::local_tempdir()
    d <- files_daf(root, mode = "w+", name = "m")
    add_axis(d, "cell", paste0("c", 1:3))
    dafr:::.metadata_json_rebuild(root)
    dafr:::.metadata_json_append(root, "scalars/x",
                                 '{"type":"Int32","value":9}')
    m <- jsonlite::fromJSON(file.path(root, "metadata.json"), simplifyVector = FALSE)
    expect_equal(m[["scalars/x"]], list(type = "Int32", value = 9L))
    expect_equal(m[["axes/cell"]], list(format = "axis", n_entries = 3L))
})

test_that("pack_files_daf_metadata rebuilds a valid metadata.json", {
    root <- withr::local_tempdir()
    d <- files_daf(root, mode = "w+", name = "m"); add_axis(d, "cell", c("a","b"))
    unlink(file.path(root, "metadata.json"))
    pack_files_daf_metadata(root)
    expect_true(file.exists(file.path(root, "metadata.json")))
    m <- jsonlite::fromJSON(file.path(root, "metadata.json"), simplifyVector = FALSE)
    expect_equal(m[["axes/cell"]], list(format = "axis", n_entries = 2L))
})
```

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Implement** (append to `R/files_metadata_json.R`). Append via read/strip-trailing-`}`/concat (rebuild on missing or collision):
```r
# Append "<key>":<descriptor> to an existing metadata.json (byte-light: read,
# insert before the trailing "}"). Rebuilds if the file is missing or the key is
# already present (overwrite must not duplicate).
.metadata_json_append <- function(root, key, descriptor) {
    p <- file.path(root, .METADATA_JSON)
    if (!file.exists(p)) return(.metadata_json_rebuild(root))
    cur <- paste(readLines(p, warn = FALSE), collapse = "")
    if (grepl(paste0('"', key, '":'), cur, fixed = TRUE)) {
        return(.metadata_json_rebuild(root))   # collision -> rebuild (overwrite)
    }
    entry <- paste0('"', key, '":', descriptor)
    inner <- sub("\\}\\s*$", "", cur)
    sep <- if (identical(trimws(inner), "{")) "" else ","
    writeLines(paste0(inner, sep, entry, "}"), p, useBytes = TRUE)
    invisible()
}

# Rebuild if metadata.json is absent (writable-open seed).
.metadata_json_ensure <- function(root) {
    if (!file.exists(file.path(root, .METADATA_JSON))) .metadata_json_rebuild(root)
    invisible()
}

#' Rebuild a FilesDaf store's root metadata.json index.
#'
#' Writes the DataAxesFormats-compatible `metadata.json` consolidated index from
#' the on-disk tree. Use to migrate a store written by an older dafr (which used
#' `metadata.zip`) or modified outside dafr, so it can be HTTP-served and read by
#' DataAxesFormats.jl.
#' @param path FilesDaf store root directory.
#' @export
pack_files_daf_metadata <- function(path) {
    if (!dir.exists(path)) {
        stop(sprintf("not a directory: %s", sQuote(path)), call. = FALSE)
    }
    .metadata_json_rebuild(path)
    invisible(path)
}
```

- [ ] **Step 4: Run - expect PASS.** Commit.
```bash
git add R/files_metadata_json.R tests/testthat/test-files-metadata-json.R
git commit -m "feat(json-parity): metadata.json append/ensure + pack migration helper"
```

## Task 7: Swap write call sites; delete metadata.zip module

**Files:** Modify `R/files_daf.R`, `R/files_daf_write.R`; Delete `R/files_metadata_zip.R`, `tests/testthat/test-files-metadata-zip.R`; Test `tests/testthat/test-files-metadata-json.R`

- [ ] **Step 1: Failing test** - a FilesDaf write produces metadata.json (not .zip) and round-trips:
```r
test_that("files_daf writable open + set_* maintain metadata.json, no zip", {
    root <- withr::local_tempdir()
    d <- files_daf(root, mode = "w+", name = "m")
    add_axis(d, "cell", paste0("c", 1:3))
    set_vector(d, "cell", "v", as.numeric(1:3))
    expect_true(file.exists(file.path(root, "metadata.json")))
    expect_false(file.exists(file.path(root, "metadata.zip")))
    m <- jsonlite::fromJSON(file.path(root, "metadata.json"), simplifyVector = FALSE)
    expect_equal(m[["vectors/cell/v"]], list(format = "dense", eltype = "Float64"))
    # delete -> rebuild drops the entry
    delete_vector(d, "cell", "v")
    m2 <- jsonlite::fromJSON(file.path(root, "metadata.json"), simplifyVector = FALSE)
    expect_null(m2[["vectors/cell/v"]])
})
```

- [ ] **Step 2: Run - expect FAIL** (still writes metadata.zip).

- [ ] **Step 3: Swap call sites.** In `R/files_daf_write.R` and `R/files_daf.R`, replace each metadata.zip call with the json equivalent (same trigger points listed in the ground-truth):
  - `.ensure_metadata_zip(root)` -> `.metadata_json_ensure(root)` (files_daf.R writable open).
  - `.metadata_zip_append(root, "<rel>.json")` (set_scalar L14, set_vector L272, set_matrix L389/L414) -> `.metadata_json_append(root, "<rel>", <descriptor-text>)`. The descriptor text is the SAME JSON just written to the per-property `.json`; read it back via `.metadata_json_inline(<that .json path>)` (simplest, guarantees identical bytes), or pass the descriptor object the writer already built. For axes (which write `{"format":"axis","n_entries":N}`), `add_axis` should append `axes/<axis>` with the computed descriptor.
  - `.metadata_zip_rebuild(root)` (delete_scalar L30, add_axis L96 [or append], delete_axis L131, delete_vector L318, delete_matrix L436, reorder L738/L805) -> `.metadata_json_rebuild(root)`.
  NOTE: `add_axis` previously rebuilt (to refresh `axes/metadata.json`); with metadata.json an append of the single `axes/<axis>` key suffices, but rebuild is also correct - prefer `.metadata_json_append(root, paste0("axes/", axis), sprintf('{"format":"axis","n_entries":%d}', n))` for O(1), falling back to rebuild semantics already built in.

- [ ] **Step 4: Delete the zip module + its test.**
```bash
git rm R/files_metadata_zip.R tests/testthat/test-files-metadata-zip.R
```
Then grep to confirm no remaining references: `grep -rn "metadata_zip\|\.write_axes_metadata\|ensure_metadata_zip" R/ tests/` returns nothing (except possibly http_format.R, fixed in Task 8). Run `Rscript -e 'devtools::document()'` to refresh Collate (drops files_metadata_zip.R, adds files_metadata_json.R if `@include`d - add `#' @include` if the package uses Collate).

- [ ] **Step 5: Run - expect PASS** (the new test + existing files tests, except http which Task 8 fixes). Confirm `grep -rn metadata_zip R/` is empty.

- [ ] **Step 6: Commit.**
```bash
git add R/files_daf.R R/files_daf_write.R R/files_metadata_json.R DESCRIPTION
git commit -m "feat(json-parity): FilesDaf writes metadata.json; remove metadata.zip module"
```

## Task 8: HttpDaf reads metadata.json

**Files:** Modify `R/http_format.R`; Test `tests/testthat/test-http-daf.R`

- [ ] **Step 1: Failing test** - point an HttpDaf at a local dafr store (served from `file://` or the existing http test harness) and read scalars/vectors via metadata.json. Use whatever local-HTTP harness `test-http-daf.R` already uses; assert enumeration + a descriptor read works against a store that has only `metadata.json` (no `.zip`). (Read the current test-http-daf.R harness first and mirror it.)

- [ ] **Step 2: Run - expect FAIL** (http_daf fetches metadata.zip).

- [ ] **Step 3: Rewrite the HttpDaf metadata path** in `R/http_format.R`:
  - `http_daf()` (L55-119): fetch `paste0(url, "/metadata.json")` instead of metadata.zip; `meta <- jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE)`; store `internal$meta <- meta` (the parsed dict). Validate it's a daf store by fetching/também checking `daf.json` over HTTP (the version marker) OR accept presence of a well-formed metadata.json. Drop the tempfile/unzip.
  - Replace `.http_zip_json(daf, relpath)` with `.http_meta(daf, key)` that looks up `key` (the rel-path WITHOUT `.json`) in `internal$meta` and returns the descriptor list. Update the scalar (`.http_scalar_from_json`), vector, matrix descriptor accessors to read from `.http_meta()`; their decode logic is unchanged (same descriptor shape).
  - Axis list: keys of `internal$meta` matching `^axes/` -> axis names; `n_entries` from the axis descriptor. Individual axis `.txt` entries still fetched over HTTP (unchanged).
  - Missing/!parseable metadata.json -> `stop("not a daf data set (no metadata.json): <url>; re-pack with pack_files_daf_metadata()")`.

- [ ] **Step 4: Run - expect PASS.** Run full `test-http-daf.R` + `test-files-*.R` for no regression.

- [ ] **Step 5: Commit.**
```bash
git add R/http_format.R tests/testthat/test-http-daf.R
git commit -m "feat(json-parity): HttpDaf enumerates via metadata.json"
```

## Task 9: Track A live interop (Julia <-> dafr)

**Files:** Test `tests/testthat/test-json-parity-interop.R`

- [ ] **Step 1: Test** (append) - Julia FilesDaf reads a dafr-written store, and dafr reads a Julia-written store:
```r
test_that("Julia FilesDaf reads a dafr-written store's metadata.json", {
    skip_on_cran(); skip_if_not(.daf_jl_uses_zarr_v3())
    root <- withr::local_tempdir(); path <- file.path(root, "s")
    d <- files_daf(path, mode = "w+", name = "s")
    add_axis(d, "cell", paste0("c", 1:5))
    set_vector(d, "cell", "v", as.numeric(1:5))
    set_scalar(d, "title", "hi")
    res <- run_julia(c("using DataAxesFormats",
        sprintf('d = FilesDaf(raw"%s", "r")', path),
        'ok = get_scalar(d, "title")=="hi" && get_vector(d,"cell","v")[5]==5.0',
        'println(ok ? "ALLOK" : "BAD")'))
    if (!any(grepl("ALLOK", res))) cat(paste(res, collapse="\n"), "\n")
    expect_true(any(grepl("ALLOK", res)))
})

test_that("dafr FilesDaf reads a Julia-written store's metadata.json", {
    skip_on_cran(); skip_if_not(.daf_jl_uses_zarr_v3())
    root <- withr::local_tempdir(); path <- file.path(root, "j")
    res <- run_julia(c("using DataAxesFormats",
        sprintf('d = FilesDaf(raw"%s", "w"; name="j")', path),
        'add_axis!(d, "cell", ["c$(i)" for i in 1:5])',
        'set_vector!(d, "cell", "v", Float64.(1:5))',
        'set_scalar!(d, "title", "hi")', 'println("WROTE")'))
    skip_if_not(any(grepl("WROTE", res)))
    dd <- files_daf(path, mode = "r")
    expect_equal(get_scalar(dd, "title"), "hi")
    expect_equal(as.numeric(get_vector(dd, "cell", "v")), as.numeric(1:5))
})
```

- [ ] **Step 2: Run - expect PASS.** Commit.
```bash
git add tests/testthat/test-json-parity-interop.R
git commit -m "test(json-parity): FilesDaf metadata.json interop (Julia <-> dafr)"
```

---

# Phase 3 - Docs + ship

## Task 10: Docs, gap-doc, version, full verification, ship

**Files:** Modify `NEWS.md`, `DESCRIPTION`, `dev/parity-audit-2026-06-11/REMAINING-GAP.md`

- [ ] **Step 1: Version + NEWS.** Bump `DESCRIPTION` Version 0.4.9 -> 0.5.0 (new cross-language interop feature). Add a `# dafr 0.5.0` NEWS section:
```markdown
# dafr 0.5.0

Cross-language JSON parity with DataAxesFormats.jl: dafr now reads and writes
Julia's exact `metadata.json` store index and `base_daf_view` chain spec.

* **FilesDaf/HttpDaf root `metadata.json`.** FilesDaf now writes a single root
  `metadata.json` (the DataAxesFormats consolidated index) instead of a
  `metadata.zip` bundle; HttpDaf enumerates a remote store from it. A
  dafr-written store is now readable by DataAxesFormats.jl (and vice-versa), and
  the index now works on Windows. Migrate an older (metadata.zip) store with
  `pack_files_daf_metadata(path)`.
* **complete_daf `base_daf_view`.** The persisted view spec now uses Julia's
  object schema, so a chain written by one language reopens in the other.
```

- [ ] **Step 2: REMAINING-GAP.md.** Move both items out of "OPEN - needs a DESIGN decision first" into a DONE section (decision: adopt Julia schema; both implemented + interop-verified). That section now has zero open items.

- [ ] **Step 3: document() + full suite (libs present).**
`rm -f src/*.o src/*.so src/Makevars && BLOSC_HOME=/home/aviezerl/.julia/artifacts/b50f03cd3f0ce8f8e4dc931a016a2ff30de18fc3 ./configure && BLOSC_HOME=... Rscript -e 'devtools::document()' && BLOSC_HOME=... NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=FALSE)'`
Expect 0 failures.

- [ ] **Step 4: CRAN no-lib check.** `rm -f src/*.o src/*.so src/Makevars && DAFR_NO_PACKED_CODECS=1 ./configure && DAFR_NO_PACKED_CODECS=1 NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=FALSE)'` -> 0 failures.

- [ ] **Step 5: rcmdcheck ship gate.** `rm -f src/*.o src/*.so src/Makevars && Rscript -e 'rcmdcheck::rcmdcheck(args=c("--as-cran","--no-manual"), error_on="warning")'` -> 0 errors/0 warnings. (Watch for any new non-portable test; nothing here re-compresses, so low risk.) Restore libs-present build after.

- [ ] **Step 6: Commit.**
```bash
git add R man DESCRIPTION NEWS.md dev/parity-audit-2026-06-11/REMAINING-GAP.md
git commit -m "docs(json-parity): NEWS, gap-doc, 0.5.0 bump; full verification"
```

- [ ] **Step 7: Ship** (after merge to dev): `bash dev/skills/dafr-ship/ship.sh "Release dafr 0.5.0" --push`, then watch CI (R-CMD-check 5-OS) to green - re-running the ubuntu-devel job if it hangs in R-devel setup.

---

## Self-review notes
- **Spec coverage:** §3 Track B ground truth -> Tasks 1-4; §3 Track A ground truth -> Tasks 5-9; §4 Track A clean break (delete zip, HttpDaf, migration) -> Tasks 7-8, 6; §6 verification -> per-task interop + Task 10. Decision B (clean break) -> Task 7 deletes the module; Decision C (pack_files_daf_metadata migration) -> Task 6; Decision D (B before A) -> phase order.
- **Reused/verified facts:** dafr per-property descriptors already match Julia (so rebuild inlines them); `.parse_view_item` exists; eltype already uses Julia names; n_entries = `length(readLines(axes/<axis>.txt))`.
- **[PIN] residual:** the metadata.json descriptor match is asserted structurally in Task 5 + byte-validated by the Julia-reads-dafr interop in Task 9; the base_daf_view bytes are pinned to the live capture in Task 2 + interop in Task 4.
- **Type consistency:** `.view_spec_to_julia_json`/`.view_spec_from_julia_json`/`.view_data_key`/`.view_decode_key` (Track B); `.metadata_json_rebuild`/`_append`/`_ensure`/`_inline`/`_entries`/`_assemble` + `pack_files_daf_metadata` (Track A) - names consistent across tasks.
- **Deferred/seam:** Task 8 HttpDaf rewrite depends on the current test-http-daf harness (read it first); if HttpDaf has accessors beyond scalar/vector/matrix/axis, route them all through `.http_meta()`.
```
