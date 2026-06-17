test_that("zstd compress round-trips through the decoder", {
    skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
    x <- writeBin(as.double(1:1024), raw(), size = 8L, endian = "little")
    comp <- dafr:::dafr_zstd_compress_cpp(x, 5L)
    expect_true(length(comp) < length(x))
    back <- dafr:::dafr_zstd_decompress_cpp(comp, length(x))
    expect_identical(back, x)
})

test_that("blosc bitshuffle compress round-trips through the decoder", {
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    x <- writeBin(as.double(1:1024), raw(), size = 8L, endian = "little")
    comp <- dafr:::dafr_blosc_compress_cpp(x, 5L, "zstd", 2L, 8L)
    back <- dafr:::dafr_blosc_decompress_cpp(comp, length(x))
    expect_identical(back, x)
})
