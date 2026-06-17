test_that("a dual-format blosc shard is a legal ZIP and round-trips", {
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble(vals, "float64", shape = 1200L, inner = 1024L,
                                   codec = "blosc_zstd_bitshuffle", level = 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "blosc_zstd_bitshuffle",
             chunk_shape = list(1024L)), shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
    tmp <- tempfile(fileext = ".zip"); writeBin(blob, tmp)
    z <- zip::zip_list(tmp)
    expect_true(all(c("c/0", "c/1", "codec.json") %in% z$filename))
})
