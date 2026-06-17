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
