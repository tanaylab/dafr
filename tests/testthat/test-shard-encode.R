test_that(".shard_effective_sizeof and threshold match Julia", {
    expect_equal(dafr:::.shard_effective_sizeof("float64"), 8L)
    expect_equal(dafr:::.shard_effective_sizeof("string"), 16L)
    # 1200 float64 = 9600 B >= 8192 -> pack
    expect_true(dafr:::.shard_should_pack(1200L, "float64", 8L))
    # 1000 float64 = 8000 B < 8192 -> flat
    expect_false(dafr:::.shard_should_pack(1000L, "float64", 8L))
})

test_that(".shard_slab_rows and .shard_inner_chunk_shape give column-slab row counts", {
    # .shard_slab_rows: shared helper used by vector, ZarrDaf matrix, FilesDaf matrix paths
    expect_equal(dafr:::.shard_slab_rows("float64", 8L, 1200L), 1024L)
    expect_equal(dafr:::.shard_slab_rows("float64", 8L, 500L),   500L)  # capped at n_rows
    # .shard_inner_chunk_shape: 1-D wrapper (scalar result)
    expect_equal(dafr:::.shard_inner_chunk_shape(c(1200L), "float64", 8L), 1024L)
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

test_that("plain shard blob round-trips a vector through the read core", {
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble_plain(vals, "float64", shape = 1200L,
                                         inner = 1024L, codec = "gzip", level = 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "gzip", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
})

test_that("plain shard blob round-trips a matrix through the read core", {
    m <- matrix(as.numeric(1:(1200 * 8)), nrow = 1200, ncol = 8)
    # On-disk reversed shape [ncol, nrow]; inner column-slab [1, 1024] on disk.
    blob <- dafr:::.shard_assemble_plain(as.vector(m), "float64",
                                         shape = c(8L, 1200L), inner = c(1L, 1024L),
                                         codec = "gzip", level = 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "gzip",
             chunk_shape = list(1L, 1024L)),
        shape = c(8L, 1200L), chunk_shape = c(1L, 1024L))
    expect_equal(as.numeric(dafr:::.shard_decode_matrix(blob, node)), as.numeric(m))
})

test_that("plain shard blob round-trips an int64 vector through the read core", {
    vals <- bit64::as.integer64(1:1200)
    blob <- dafr:::.shard_assemble_plain(vals, "int64", shape = 1200L,
                                         inner = 1024L, codec = "gzip", level = 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Int64", compression = "gzip", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
})

test_that("plain shard blob round-trips an int64 matrix through the read core", {
    # as.vector() drops the integer64 class on a bit64 matrix, so build the
    # flat column-major vector directly (1:9600 column-major == sequential fill).
    # Comparison baseline uses bit64::as.double.integer64 explicitly to avoid
    # the unattached-bit64 trap where as.numeric() bit-reinterprets the matrix.
    vals_flat <- bit64::as.integer64(1:(1200 * 8))
    blob <- dafr:::.shard_assemble_plain(vals_flat, "int64",
                                         shape = c(8L, 1200L), inner = c(1L, 1024L),
                                         codec = "gzip", level = 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Int64", compression = "gzip", chunk_shape = list(1L, 1024L)),
        shape = c(8L, 1200L), chunk_shape = c(1L, 1024L))
    dec <- dafr:::.shard_decode_matrix(blob, node)
    expect_equal(bit64::as.double.integer64(dec), as.numeric(vals_flat))
})
