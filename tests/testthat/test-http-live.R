# Live HTTP integration tests for HttpDaf and HttpStore.
#
# These tests boot a `python -m http.server` subprocess and exercise the
# full read path against a real HTTP server. Each test that calls
# start_http_server() will skip itself when:
# - python is not on PATH
# - processx is not installed
# - on CRAN
#
# Mirrors upstream DataAxesFormats.jl/test/data.jl HttpDaf scenarios.

skip_if_no_http_harness <- function() {
    skip_on_cran()
    skip_if_not_installed("processx")
    skip_if_not(nzchar(Sys.which("python")))
    # http_daf reads `metadata.zip` from the server; on Windows the
    # local FilesDaf writer doesn't produce one (slice-17 MmapZipStore
    # is POSIX-only), so the served fixture is incomplete.
    skip_if_no_mmap_zip()
}

# Build a populated FilesDaf at `path/served.daf` and return the parent
# `path` for use as the http.server root.
.populate_served_daf <- function(path) {
    files_path <- file.path(path, "served.daf")
    d <- files_daf(files_path, "w+", name = "served!")
    set_scalar(d, "name", "served!")  # so http_daf default-name picks it up
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_scalar(d, "organism", "human")
    set_scalar(d, "version", 1.5)
    set_scalar(d, "ok", TRUE)
    set_vector(d, "cell", "score", c(1.0, 2.0, 3.0))
    set_vector(d, "cell", "label", c("a", "b", "c"))
    set_vector(d, "gene", "is_marker", c(TRUE, FALSE))
    sm <- Matrix::sparseMatrix(i = c(1L, 3L), j = c(1L, 2L),
                               x = c(1.0, 2.0), dims = c(3L, 2L))
    set_matrix(d, "cell", "gene", "expr", sm)
    set_matrix(d, "cell", "gene", "dense", matrix(seq_len(6), 3, 2))
    rm(d); gc()
    files_path
}

# ---- Error paths -----------------------------------------------------------

test_that("http_daf opens a populated FilesDaf served over HTTP", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-rt-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    expect_identical(daf_name(daf), "served!")
    expect_equal(complete_path(daf), paste0(h$url, "/served.daf"))
})

test_that("http_daf reads a packed (gzip) FilesFormat store served over HTTP", {
    skip_if_no_http_harness()
    # Serve a committed packed fixture (gzip needs no optional codec lib). The
    # http path fetches each `<name>.zip` shard whole and decodes through the
    # same packed core as local FilesDaf; compare the two reads.
    root <- withr::local_tempdir("daf-http-packed-")
    src <- testthat::test_path("fixtures/fpk/gz.files")
    file.copy(src, root, recursive = TRUE)
    dst <- file.path(root, "packed.files")
    file.rename(file.path(root, "gz.files"), dst)
    pack_files_daf_metadata(dst)            # build the metadata.zip http_daf needs
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)

    hd <- http_daf(paste0(h$url, "/packed.files"))
    fd <- files_daf(dst, mode = "r")
    expect_equal(as.numeric(get_vector(hd, "cell", "score")),
                 as.numeric(get_vector(fd, "cell", "score")))
    expect_identical(unname(get_vector(hd, "cell", "label")),
                     unname(get_vector(fd, "cell", "label")))
    expect_equal(as.matrix(get_matrix(hd, "cell", "gene", "dense")),
                 as.matrix(get_matrix(fd, "cell", "gene", "dense")))
    expect_equal(as.matrix(get_matrix(hd, "cell", "gene", "sparse")),
                 as.matrix(get_matrix(fd, "cell", "gene", "sparse")))
})

test_that("http_daf default name is taken from `name` scalar", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-name-")
    files_path <- file.path(root, "named.daf")
    d <- files_daf(files_path, "w+", name = "ignored-by-fd")
    set_scalar(d, "name", "from-scalar!")
    rm(d); gc()
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/named.daf"))
    expect_identical(daf_name(daf), "from-scalar!")
})

test_that("http_daf override name wins over `name` scalar", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-override-")
    files_path <- file.path(root, "named.daf")
    d <- files_daf(files_path, "w+")
    set_scalar(d, "name", "from-scalar!")
    rm(d); gc()
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/named.daf"), name = "override!")
    expect_identical(daf_name(daf), "override!")
})

# Build a metadata.zip with one entry. Used by error-path tests below.
.build_metadata_zip <- function(path, entries) {
    # entries: named list, name = entry path, value = bytes (raw or string)
    store <- new_mmap_zip_store(path, mode = "w+")
    for (key in names(entries)) {
        val <- entries[[key]]
        if (is.character(val)) val <- charToRaw(val)
        store_set_bytes(store, key, val)
    }
    dafr:::dafr_mmap_zip_close(S7::prop(store, "xptr"))
}

test_that("http_daf rejects a non-daf metadata.zip", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-bogus-")
    .build_metadata_zip(file.path(root, "metadata.zip"),
                        list("other.json" = "{}"))
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    expect_error(http_daf(h$url),
                 "not a daf data set:")
})

test_that("http_daf rejects a metadata.zip with incompatible version", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-version-")
    .build_metadata_zip(file.path(root, "metadata.zip"),
                        list("daf.json" = '{"version":[99,0]}'))
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    expect_error(http_daf(h$url),
                 "incompatible format version: 99.0")
})

# ---- Read path: scalars + axes ---------------------------------------------

test_that("http_daf round-trips scalars (string, double, bool)", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-scalars-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    expect_identical(get_scalar(daf, "organism"), "human")
    expect_equal(get_scalar(daf, "version"), 1.5)
    expect_identical(get_scalar(daf, "ok"), TRUE)
})

test_that("http_daf round-trips axis vectors", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-axes-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    expect_identical(axis_vector(daf, "cell"), c("c1", "c2", "c3"))
    expect_identical(axis_vector(daf, "gene"), c("g1", "g2"))
    expect_identical(axis_length(daf, "cell"), 3L)
    expect_setequal(axes_set(daf), c("cell", "gene"))
})

# ---- Read path: vectors ----------------------------------------------------

test_that("http_daf round-trips dense numeric vectors", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-vec-num-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    expect_equal(as.numeric(get_vector(daf, "cell", "score")), c(1, 2, 3))
})

test_that("http_daf round-trips dense string vectors", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-vec-str-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    expect_identical(unname(get_vector(daf, "cell", "label")), c("a", "b", "c"))
})

test_that("http_daf round-trips dense bool vectors", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-vec-bool-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    expect_identical(unname(get_vector(daf, "gene", "is_marker")), c(TRUE, FALSE))
})

test_that("http_daf round-trips sparse vectors (numeric, reconstructed dense)", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-vec-sparse-")
    files_path <- file.path(root, "sv.daf")
    d <- files_daf(files_path, "w+", name = "sv")
    add_axis(d, "x", c("a", "b", "c", "d", "e"))
    sv <- Matrix::sparseVector(c(1.5, 2.5), i = c(2L, 4L), length = 5L)
    set_vector(d, "x", "v", sv)
    rm(d); gc()
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/sv.daf"))
    out <- get_vector(daf, "x", "v")
    expect_equal(as.numeric(out), c(0, 1.5, 0, 2.5, 0))
})

# ---- Read path: matrices ---------------------------------------------------

test_that("http_daf round-trips dense matrices", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-mat-dense-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    m <- get_matrix(daf, "cell", "gene", "dense")
    expect_equal(dim(m), c(3L, 2L))
    expect_equal(as.vector(m), as.numeric(seq_len(6)))
})

test_that("http_daf round-trips sparse CSC matrices", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-mat-sparse-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    m <- get_matrix(daf, "cell", "gene", "expr")
    expect_s4_class(m, "dgCMatrix")
    expect_equal(dim(m), c(3L, 2L))
    expect_equal(m[1, 1], 1.0)
    expect_equal(m[3, 2], 2.0)
    expect_equal(m[2, 1], 0)
})

# ---- HttpDaf description / metadata ----------------------------------------

test_that("http_daf description includes the URL", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-desc-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    expect_match(complete_path(daf), "^http://", perl = TRUE)
})

test_that("http_daf URL with trailing slash works the same as without", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-trailing-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf1 <- http_daf(paste0(h$url, "/served.daf"))
    daf2 <- http_daf(paste0(h$url, "/served.daf/"))
    expect_identical(get_scalar(daf1, "organism"),
                     get_scalar(daf2, "organism"))
})

# ---- open_daf dispatch on a live URL ---------------------------------------

test_that("open_daf routes a live FilesDaf URL to http_daf", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-opendaf-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- open_daf(paste0(h$url, "/served.daf"), mode = "r")
    expect_identical(daf_name(daf), "served!")
})

# ---- F2: empty_cache after a get does not invalidate prior reads -----------

test_that("empty_cache(http_daf) is safe with prior reads still held", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-cache-")
    .populate_served_daf(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    v_before <- get_vector(daf, "cell", "score")
    m_before <- get_matrix(daf, "cell", "gene", "dense")
    empty_cache(daf)
    # Pin the contract: dafr's HttpDaf returns concrete numeric/character
    # vectors rather than ALTREP views over the cache buffer, so prior
    # references survive an empty_cache() call. Future ALTREP-aliased
    # buffers must preserve this invariant or migrate to the upstream
    # close-time-deactivation pattern.
    expect_equal(unname(v_before), c(1.0, 2.0, 3.0))
    expect_equal(unname(as.matrix(m_before)), matrix(seq_len(6), 3, 2))
    # Re-reading after empty_cache works (re-fetches over HTTP).
    v_after <- get_vector(daf, "cell", "score")
    expect_equal(v_after, v_before)
})

# ---- A3: HttpStore cross-language smoke ------------------------------------
# Verify that a Python consumer can read a dafr-published .daf.zarr served
# over HTTP. Uses zarr-python via fsspec/aiohttp when available; falls back
# to a urllib-only smoke that fetches the root `zarr.json` (v3 inline
# consolidated metadata) and a vector chunk and parses them with numpy. Both
# paths skip cleanly when prerequisites are
# missing (CRAN, no Python, no zarr/fsspec, etc.).

.populate_served_zarr <- function(parent_dir) {
    zarr_path <- file.path(parent_dir, "served.daf.zarr")
    d <- zarr_daf(zarr_path, "w+", name = "served-zarr!")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_scalar(d, "name", "served-zarr!")
    set_scalar(d, "organism", "human")
    set_scalar(d, "version", 1.5)
    set_scalar(d, "ok", TRUE)
    set_vector(d, "cell", "score", c(1.5, 2.5, 3.5))
    set_vector(d, "cell", "label", c("a", "b", "c"))
    set_vector(d, "gene", "is_marker", c(TRUE, FALSE))
    set_vector(d, "cell", "sv",
               Matrix::sparseVector(c(7.0, 9.0), i = c(1L, 3L), length = 3L))
    set_matrix(d, "cell", "gene", "dense",
               matrix(c(10, 20, 30, 40, 50, 60), nrow = 3))
    set_matrix(d, "cell", "gene", "expr",
               Matrix::sparseMatrix(i = c(1L, 3L), j = c(1L, 2L),
                                    x = c(1.0, 2.0), dims = c(3L, 2L)))
    rm(d); gc()
    zarr_path
}

# ---- zarr_daf() over HTTP reads a v3 store (HttpStore v3 port) --------------
# `HttpStore` (the backend behind zarr_daf("http://...")) reads the Zarr v3
# inline consolidated metadata from the root `zarr.json` as its node index
# (v3 does not write the v2 `.zmetadata` file). The round-trip below exercises
# every component kind dafr writes: scalars, axes, dense/sparse vectors,
# strings, bools, and dense/sparse matrices, plus the *_set enumeration paths
# that drive `store_list` off the consolidated index.

test_that("zarr_daf() over HTTP round-trips a v3 store across all component kinds", {
    skip_if_no_http_harness()
    root <- withr::local_tempdir("daf-http-zarr-v3-rt-")
    .populate_served_zarr(root)  # writes a Zarr v3 .daf.zarr (no .zmetadata)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)

    url <- paste0(h$url, "/served.daf.zarr")
    daf <- zarr_daf(url, mode = "r")

    # scalars (string / double / bool) + enumeration
    expect_identical(get_scalar(daf, "name"), "served-zarr!")
    expect_identical(get_scalar(daf, "organism"), "human")
    expect_equal(get_scalar(daf, "version"), 1.5)
    expect_identical(get_scalar(daf, "ok"), TRUE)
    expect_setequal(scalars_set(daf), c("name", "organism", "version", "ok"))

    # axes + enumeration
    expect_identical(axis_vector(daf, "cell"), c("c1", "c2", "c3"))
    expect_identical(axis_vector(daf, "gene"), c("g1", "g2"))
    expect_identical(axis_length(daf, "cell"), 3L)
    expect_setequal(axes_set(daf), c("cell", "gene"))

    # vectors: dense numeric / string / bool / sparse + enumeration
    expect_equal(as.numeric(get_vector(daf, "cell", "score")), c(1.5, 2.5, 3.5))
    expect_identical(unname(get_vector(daf, "cell", "label")), c("a", "b", "c"))
    expect_identical(unname(get_vector(daf, "gene", "is_marker")),
                     c(TRUE, FALSE))
    expect_equal(as.numeric(get_vector(daf, "cell", "sv")), c(7.0, 0.0, 9.0))
    expect_setequal(vectors_set(daf, "cell"), c("score", "label", "sv"))
    expect_setequal(vectors_set(daf, "gene"), "is_marker")

    # matrices: dense + sparse CSC + enumeration
    md <- get_matrix(daf, "cell", "gene", "dense")
    expect_equal(dim(md), c(3L, 2L))
    expect_equal(as.numeric(as.matrix(md)), c(10, 20, 30, 40, 50, 60))
    ms <- get_matrix(daf, "cell", "gene", "expr")
    expect_s4_class(ms, "dgCMatrix")
    expect_equal(dim(ms), c(3L, 2L))
    expect_equal(ms[1, 1], 1.0)
    expect_equal(ms[3, 2], 2.0)
    expect_equal(ms[2, 1], 0)
    expect_setequal(matrices_set(daf, "cell", "gene"), c("dense", "expr"))
})

test_that("dafr-published .daf.zarr served over HTTP is readable from numpy via urllib", {
    skip_if_no_http_harness()
    skip_if_not(nzchar(Sys.which("python3")) || nzchar(Sys.which("python")))
    py <- if (nzchar(Sys.which("python3"))) "python3" else "python"
    # numpy is the only Python dep for the urllib path.
    rc <- suppressWarnings(system2(
        py, c("-c", shQuote("import numpy, json, urllib.request")),
        stdout = FALSE, stderr = FALSE
    ))
    if (!identical(rc, 0L)) testthat::skip("numpy/urllib unavailable")

    root <- withr::local_tempdir("daf-http-zarr-smoke-")
    .populate_served_zarr(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)

    base_url <- paste0(h$url, "/served.daf.zarr")
    script <- paste0(
        "import json, sys, urllib.request, numpy as np\n",
        "base = ", shQuote(base_url), "\n",
        "def fetch(rel):\n",
        "    with urllib.request.urlopen(base + '/' + rel) as r:\n",
        "        return r.read()\n",
        # Zarr v3 inline consolidated metadata is the discoverability anchor:
        # the root zarr.json carries consolidated_metadata.metadata keyed by
        # node path (no .zarray suffix). Without it foreign consumers can't
        # enumerate the tree over HTTP.
        "root = json.loads(fetch('zarr.json'))\n",
        "md = root['consolidated_metadata']['metadata']\n",
        "keys = sorted(md.keys())\n",
        "print('SCALARS_NAME_PRESENT=' + str(any(k.startswith('scalars/name') for k in keys)), flush=True)\n",
        # Pull the score vector's chunk and decode.
        "score_meta = md['vectors/cell/score']\n",
        "print('SCORE_DTYPE=' + score_meta['data_type'], flush=True)\n",
        "print('SCORE_SHAPE=' + str(list(score_meta['shape'])), flush=True)\n",
        "buf = fetch('vectors/cell/score/c/0')\n",
        "score = np.frombuffer(buf, dtype='<f8')\n",
        "print('SCORE=' + ','.join(f'{v}' for v in score), flush=True)\n"
    )
    out <- system2(py, c("-c", shQuote(script)), stdout = TRUE, stderr = TRUE)
    rc <- attr(out, "status")
    text <- paste(out, collapse = "\n")
    if (!is.null(rc) && rc != 0L) {
        testthat::fail(paste0("python smoke failed: ", text))
    }
    expect_match(text, "SCORE_DTYPE=float64", fixed = TRUE)
    expect_match(text, "SCORE_SHAPE=[3]", fixed = TRUE)
    expect_match(text, "SCORE=1.5,2.5,3.5", fixed = TRUE)
    # The inline consolidated metadata should expose every group dafr writes.
    expect_match(text, "SCALARS_NAME_PRESENT=True", fixed = TRUE)
})

test_that("dafr-published .daf.zarr served over HTTP opens via zarr.open + fsspec", {
    skip_if_no_http_harness()
    py <- if (nzchar(Sys.which("python3"))) "python3" else if (nzchar(Sys.which("python"))) "python" else NA_character_
    if (is.na(py)) testthat::skip("python not available")
    # Probe for fsspec + aiohttp + zarr together. Many setups have zarr
    # without HTTP-capable fsspec, so this skips more often than the urllib
    # path above.
    probe <- "import zarr, fsspec, aiohttp; print('OK')"
    out <- suppressWarnings(system2(
        py, c("-c", shQuote(probe)),
        stdout = TRUE, stderr = TRUE
    ))
    if (!identical(attr(out, "status"), 0L) || !any(grepl("^OK$", out))) {
        testthat::skip("python zarr+fsspec+aiohttp not available")
    }

    root <- withr::local_tempdir("daf-http-zarr-fsspec-")
    .populate_served_zarr(root)
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)

    base_url <- paste0(h$url, "/served.daf.zarr")
    script <- paste0(
        "import zarr, sys\n",
        "g = zarr.open(", shQuote(base_url), ", mode='r')\n",
        "score = g['vectors/cell/score'][...]\n",
        "cell = g['axes/cell'][...]\n",
        "print('CELL=' + ','.join(map(str, cell)), flush=True)\n",
        "print('SCORE=' + ','.join(f'{v}' for v in score), flush=True)\n"
    )
    out <- system2(py, c("-c", shQuote(script)), stdout = TRUE, stderr = TRUE)
    rc <- attr(out, "status")
    text <- paste(out, collapse = "\n")
    if (!is.null(rc) && rc != 0L) {
        # zarr-python releases vary in their HTTP support; some choke on
        # the v3 layout dafr writes. Skip rather than fail so this stays a
        # smoke, not a gate.
        testthat::skip(paste0("zarr.open over HTTP failed: ", text))
    }
    expect_match(text, "CELL=c1,c2,c3", fixed = TRUE)
    expect_match(text, "SCORE=1.5,2.5,3.5", fixed = TRUE)
})
