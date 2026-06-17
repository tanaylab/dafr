test_that(".shard_effective_sizeof and threshold match Julia", {
    expect_equal(dafr:::.shard_effective_sizeof("float64"), 8L)
    expect_equal(dafr:::.shard_effective_sizeof("string"), 16L)
    # 1200 float64 = 9600 B >= 8192 -> pack
    expect_true(dafr:::.shard_should_pack(1200L, "float64", 8L))
    # 1000 float64 = 8000 B < 8192 -> flat
    expect_false(dafr:::.shard_should_pack(1000L, "float64", 8L))
})

test_that(".shard_inner_chunk_shape gives column-slab chunks", {
    expect_equal(dafr:::.shard_inner_chunk_shape(c(1200L), "float64", 8L), 1024L)
    expect_equal(dafr:::.shard_inner_chunk_shape(c(1200L, 8L), "float64", 8L),
                 c(1024L, 1L))
})

test_that(".shard_inner_compress inverts .zarr_inner_decompress (gzip)", {
    cfg_gzip <- list(codecs = list(list(name = "bytes"), list(name = "gzip")))
    raw_bytes <- writeBin(as.double(1:1024), raw(), size = 8L, endian = "little")
    comp <- dafr:::.shard_inner_compress(raw_bytes, cfg_gzip, level = 5L)
    back <- dafr:::.zarr_inner_decompress(comp, cfg_gzip, out_nbytes = length(raw_bytes))
    expect_identical(back, raw_bytes)
})

test_that(".shard_inner_compress inverts .zarr_inner_decompress (zstd)", {
    skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
    cfg <- list(codecs = list(list(name = "bytes"), list(name = "zstd")))
    raw_bytes <- writeBin(as.double(1:1024), raw(), size = 8L, endian = "little")
    comp <- dafr:::.shard_inner_compress(raw_bytes, cfg, level = 5L)
    back <- dafr:::.zarr_inner_decompress(comp, cfg, out_nbytes = length(raw_bytes))
    expect_identical(back, raw_bytes)
})

test_that(".shard_inner_compress inverts .zarr_inner_decompress (blosc)", {
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    cfg <- list(codecs = list(list(name = "bytes"), list(name = "blosc")),
                .blosc_cname = "zstd")
    raw_bytes <- writeBin(as.double(1:1024), raw(), size = 8L, endian = "little")
    comp <- dafr:::.shard_inner_compress(raw_bytes, cfg, level = 5L, typesize = 8L)
    back <- dafr:::.zarr_inner_decompress(comp, cfg, out_nbytes = length(raw_bytes))
    expect_identical(back, raw_bytes)
})
