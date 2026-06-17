test_that(".packed_opts reads defaults and overrides", {
    withr::local_options(list(dafr.packed_compression = NULL,
                              dafr.packed_compression_level = NULL,
                              dafr.packed_target_chunk_kb = NULL))
    o <- dafr:::.packed_opts()
    expect_equal(o$compression, "blosc_zstd_bitshuffle")
    expect_equal(o$level, 5L); expect_equal(o$target_kb, 8L)
    withr::local_options(list(dafr.packed_compression = "gzip"))
    expect_equal(dafr:::.packed_opts()$compression, "gzip")
})

test_that(".packed_validate_codec errors when the lib is absent", {
    if (!dafr:::dafr_have_zstd_cpp())
        expect_error(dafr:::.packed_validate_codec("zstd"), "requires libzstd")
    expect_silent(dafr:::.packed_validate_codec("gzip"))
})

test_that(".packed_validate_codec rejects an unknown codec", {
    expect_error(dafr:::.packed_validate_codec("bogus_codec"),
                 "unknown compression")
})
