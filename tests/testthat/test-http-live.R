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
