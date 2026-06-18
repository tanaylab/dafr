# tests/testthat/test-files-packed-write.R
# Unit tests for .files_packed_descriptor - the packed FilesDaf JSON sidecar
# descriptor builder. Field names and values are pinned against the Julia-written
# fixtures in tests/testthat/fixtures/fpk/gz.files.

test_that(".files_packed_descriptor emits the expected fields", {
    d <- dafr:::.files_packed_descriptor("Float64", n = 1200L, inner = 1024L,
                                         codec = "gzip", level = 5L)
    expect_equal(d$format, "dense")
    expect_equal(d$eltype, "Float64")
    expect_equal(d$packed_format, "indexed+zipped")
    expect_equal(unlist(d$chunk_shape), 1024L)
    expect_equal(d$compression, "gzip")
    expect_equal(d$compression_level, 5L)
    expect_equal(d$index_location, "start")
    expect_equal(d$n_elements, 1200L)
})

test_that(".files_packed_descriptor omits n_elements when n is NULL", {
    d <- dafr:::.files_packed_descriptor("Float64", n = NULL, inner = 1024L,
                                         codec = "gzip", level = 5L)
    expect_null(d$n_elements)
    expect_equal(d$format, "dense")
    expect_equal(d$packed_format, "indexed+zipped")
})

test_that(".files_packed_descriptor handles matrix chunk_shape", {
    # Matrix packed descriptor uses a 2-element chunk_shape, e.g. [1024, 1].
    # Pinned against gz.files/matrices/cell/gene/dense.json fixture.
    d <- dafr:::.files_packed_descriptor("Float64", n = NULL,
                                         inner = c(1024L, 1L),
                                         codec = "gzip", level = 5L)
    expect_equal(unlist(d$chunk_shape), c(1024L, 1L))
    expect_null(d$n_elements)
})

test_that(".files_packed_descriptor preserves codec string for blosc variants", {
    d <- dafr:::.files_packed_descriptor("Float64", n = NULL, inner = 1024L,
                                         codec = "blosc_lz4_bitshuffle", level = 5L)
    expect_equal(d$compression, "blosc_lz4_bitshuffle")
})

# ---- end-to-end packed FilesDaf write -> dafr read round-trip ----
# Uses blosc when c-blosc is built in, otherwise the CRAN-safe gzip codec, so
# this exercises the full write path on every build.

test_that("files_daf(packed=TRUE) round-trips dense, matrix, sparse", {
    codec <- if (dafr:::dafr_have_blosc_cpp()) "blosc_zstd_bitshuffle" else "gzip"
    withr::local_options(list(dafr.packed_compression = codec))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.files-daf")
    daf <- files_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200)); add_axis(daf, "gene", paste0("g", 1:8))
    set_vector(daf, "cell", "score", as.numeric(1:1200))
    set_matrix(daf, "cell", "gene", "dense", matrix(as.numeric(1:(1200*8)), 1200, 8))
    sm <- Matrix::sparseMatrix(i = 1:1200, j = rep(1L,1200), x = as.numeric(1:1200), dims = c(1200,8))
    set_matrix(daf, "cell", "gene", "sp", sm)
    ro <- files_daf(path, "r")
    expect_equal(as.numeric(get_vector(ro, "cell", "score")), as.numeric(1:1200))
    expect_equal(as.numeric(get_matrix(ro, "cell", "gene", "dense")), as.numeric(1:(1200*8)))
    expect_equal(Matrix::nnzero(get_matrix(ro, "cell", "gene", "sp")), 1200L)
    expect_equal(as.numeric(get_matrix(ro, "cell", "gene", "sp")[1:1200, 1]),
                 as.numeric(1:1200))
    expect_true(file.exists(file.path(path, "vectors", "cell", "score.zip")))
    expect_true(file.exists(file.path(path, "matrices", "cell", "gene", "dense.zip")))
    # The large Float64 nzval component packs; the small index components stay flat.
    expect_true(file.exists(file.path(path, "matrices", "cell", "gene", "sp.nzval.zip")))
})

test_that("files_daf(packed=TRUE) round-trips a sparse vector with a packed value component", {
    codec <- if (dafr:::dafr_have_blosc_cpp()) "blosc_zstd_bitshuffle" else "gzip"
    withr::local_options(list(dafr.packed_compression = codec))
    dir <- withr::local_tempdir(); path <- file.path(dir, "sv.files-daf")
    daf <- files_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:5000))
    v <- numeric(5000); idx <- seq(1L, 5000L, by = 2L); v[idx] <- as.numeric(idx)
    set_vector(daf, "cell", "sv", v)
    expect_true(file.exists(file.path(path, "vectors", "cell", "sv.nzval.zip")))
    ro <- files_daf(path, "r")
    expect_equal(as.numeric(get_vector(ro, "cell", "sv")), v)
})

test_that("files_daf(packed=FALSE) stays flat", {
    dir <- withr::local_tempdir(); path <- file.path(dir, "f.files-daf")
    daf <- files_daf(path, "w")
    add_axis(daf, "cell", paste0("c", 1:1200))
    set_vector(daf, "cell", "score", as.numeric(1:1200))
    expect_true(file.exists(file.path(path, "vectors", "cell", "score.data")))
    expect_false(file.exists(file.path(path, "vectors", "cell", "score.zip")))
    ro <- files_daf(path, "r")
    expect_equal(as.numeric(get_vector(ro, "cell", "score")), as.numeric(1:1200))
})

test_that("FilesDaf packed write errors actionably when the zstd lib is absent", {
    if (dafr:::dafr_have_zstd_cpp()) skip("libzstd present")
    withr::local_options(list(dafr.packed_compression = "zstd"))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.files-daf")
    daf <- files_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200))
    expect_error(set_vector(daf, "cell", "score", as.numeric(1:1200)),
                 "requires libzstd")
})

test_that("FilesDaf packed write with gzip works without any optional lib", {
    withr::local_options(list(dafr.packed_compression = "gzip"))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.files-daf")
    daf <- files_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200))
    set_vector(daf, "cell", "score", as.numeric(1:1200))
    ro <- files_daf(path, "r")
    expect_equal(as.numeric(get_vector(ro, "cell", "score")), as.numeric(1:1200))
    expect_true(file.exists(file.path(path, "vectors", "cell", "score.zip")))
})
