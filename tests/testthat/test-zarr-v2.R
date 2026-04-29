test_that("zarr_v2_dtype_for_r maps R types correctly", {
    expect_identical(dafr:::zarr_v2_dtype_for_r(1.5), "<f8")
    expect_identical(dafr:::zarr_v2_dtype_for_r(1L), "<i4")
    expect_identical(dafr:::zarr_v2_dtype_for_r(TRUE), "|b1")
    expect_identical(dafr:::zarr_v2_dtype_for_r("hello"), "|O")
    expect_identical(dafr:::zarr_v2_dtype_for_r(c(1.0, 2.0)), "<f8")
    expect_identical(dafr:::zarr_v2_dtype_for_r(c(1L, 2L)), "<i4")
})

test_that("zarr_v2_dtype_for_r maps integer64 to <i8", {
    skip_if_not_installed("bit64")
    big <- bit64::as.integer64(c("12345678901", "98765432109"))
    expect_identical(dafr:::zarr_v2_dtype_for_r(big), "<i8")
})

test_that("zarr_v2_r_kind_for_dtype reverse mapping", {
    expect_identical(dafr:::zarr_v2_r_kind_for_dtype("<f8"), "double")
    expect_identical(dafr:::zarr_v2_r_kind_for_dtype("<i4"), "integer")
    expect_identical(dafr:::zarr_v2_r_kind_for_dtype("<i8"), "integer64")
    expect_identical(dafr:::zarr_v2_r_kind_for_dtype("|b1"), "logical")
    expect_identical(dafr:::zarr_v2_r_kind_for_dtype("|O"), "character")
})

test_that("zarr_v2_r_kind_for_dtype rejects unsupported dtypes", {
    expect_error(dafr:::zarr_v2_r_kind_for_dtype("<f4"), "unsupported")
    expect_error(dafr:::zarr_v2_r_kind_for_dtype(">i4"), "unsupported")
})

test_that("zarr_v2_size_for_dtype returns correct sizes", {
    expect_identical(dafr:::zarr_v2_size_for_dtype("<f8"), 8L)
    expect_identical(dafr:::zarr_v2_size_for_dtype("<i4"), 4L)
    expect_identical(dafr:::zarr_v2_size_for_dtype("<i8"), 8L)
    expect_identical(dafr:::zarr_v2_size_for_dtype("|b1"), 1L)
    expect_true(is.na(dafr:::zarr_v2_size_for_dtype("|O")))
})

test_that("zarr_v2_zarray builds a 1D descriptor with default chunks", {
    z <- dafr:::zarr_v2_zarray(shape = 100L, dtype = "<f8")
    expect_identical(z$zarr_format, 2L)
    expect_identical(z$shape, list(100L))
    expect_identical(z$chunks, list(100L))
    expect_identical(z$dtype, "<f8")
    expect_identical(z$order, "C")
    expect_identical(z$dimension_separator, "/")
})

test_that("zarr_v2_zarray builds a 2D descriptor", {
    z <- dafr:::zarr_v2_zarray(shape = c(10L, 20L), dtype = "<i4")
    expect_identical(z$shape, list(10L, 20L))
    expect_identical(z$chunks, list(10L, 20L))
})

test_that("zarr_v2 .zarray round-trips through DictStore", {
    s <- new_dict_store()
    z <- dafr:::zarr_v2_zarray(shape = 5L, dtype = "<f8", fill_value = 0)
    dafr:::zarr_v2_write_zarray(s, "vectors/cell/x", z)
    expect_true(store_exists(s, "vectors/cell/x/.zarray"))
    parsed <- dafr:::zarr_v2_read_zarray(s, "vectors/cell/x")
    expect_identical(parsed$zarr_format, 2L)
    expect_identical(parsed$shape, list(5L))
    expect_identical(parsed$dtype, "<f8")
})

test_that("zarr_v2_read_zarray returns NULL when path missing", {
    s <- new_dict_store()
    expect_null(dafr:::zarr_v2_read_zarray(s, "nonexistent"))
})

test_that("zarr_v2 .zattrs round-trips through DictStore", {
    s <- new_dict_store()
    attrs <- list(units = "counts", source = "test")
    dafr:::zarr_v2_write_zattrs(s, "vectors/cell/x", attrs)
    parsed <- dafr:::zarr_v2_read_zattrs(s, "vectors/cell/x")
    expect_identical(parsed$units, "counts")
    expect_identical(parsed$source, "test")
})

test_that("zarr_v2_write_zattrs is a no-op for empty attrs", {
    s <- new_dict_store()
    dafr:::zarr_v2_write_zattrs(s, "vectors/cell/x", list())
    expect_false(store_exists(s, "vectors/cell/x/.zattrs"))
})

test_that("zarr_v2_read_zattrs returns empty list when path missing", {
    s <- new_dict_store()
    result <- dafr:::zarr_v2_read_zattrs(s, "nonexistent")
    expect_identical(result, list())
})

test_that("zarr_v2 round-trip via DirStore (filesystem) too", {
    s <- new_dir_store(tempfile())
    z <- dafr:::zarr_v2_zarray(shape = 3L, dtype = "<i4")
    dafr:::zarr_v2_write_zarray(s, "axes/cell", z)
    parsed <- dafr:::zarr_v2_read_zarray(s, "axes/cell")
    expect_identical(parsed$dtype, "<i4")
    expect_identical(parsed$shape, list(3L))
})

# ---- chunk encode/decode --------------------------------------------------

test_that("zarr_v2 numeric chunk: <f8 round-trip", {
    bytes <- dafr:::zarr_v2_encode_chunk(c(1.5, -2.5, 3.14159), "<f8")
    expect_length(bytes, 24L)  # 3 values * 8 bytes
    decoded <- dafr:::zarr_v2_decode_chunk(bytes, "<f8", n = 3L)
    expect_equal(decoded, c(1.5, -2.5, 3.14159))
})

test_that("zarr_v2 numeric chunk: <i4 round-trip", {
    bytes <- dafr:::zarr_v2_encode_chunk(c(1L, -2L, 100000L), "<i4")
    expect_length(bytes, 12L)
    decoded <- dafr:::zarr_v2_decode_chunk(bytes, "<i4", n = 3L)
    expect_identical(decoded, c(1L, -2L, 100000L))
})

test_that("zarr_v2 numeric chunk: <i8 round-trip", {
    skip_if_not_installed("bit64")
    big <- bit64::as.integer64(c("12345678901", "-98765432109", "0"))
    bytes <- dafr:::zarr_v2_encode_chunk(big, "<i8")
    expect_length(bytes, 24L)
    decoded <- dafr:::zarr_v2_decode_chunk(bytes, "<i8", n = 3L)
    expect_s3_class(decoded, "integer64")
    expect_identical(as.character(decoded), as.character(big))
})

test_that("zarr_v2 numeric chunk: |b1 round-trip", {
    bytes <- dafr:::zarr_v2_encode_chunk(c(TRUE, FALSE, TRUE, TRUE, FALSE), "|b1")
    expect_length(bytes, 5L)
    decoded <- dafr:::zarr_v2_decode_chunk(bytes, "|b1", n = 5L)
    expect_identical(decoded, c(TRUE, FALSE, TRUE, TRUE, FALSE))
})

test_that("zarr_v2 chunk encode rejects |O (use string encoder)", {
    expect_error(dafr:::zarr_v2_encode_chunk(c("a", "b"), "|O"),
                 "use zarr_v2_encode_strings")
})

test_that("zarr_v2 chunk decode rejects |O (use string decoder)", {
    expect_error(dafr:::zarr_v2_decode_chunk(raw(0L), "|O", n = 0L),
                 "use zarr_v2_decode_strings")
})

test_that("zarr_v2 chunk decode handles gzip-compressed input", {
    plain <- dafr:::zarr_v2_encode_chunk(c(1.0, 2.0, 3.0), "<f8")
    gz <- memCompress(plain, type = "gzip")
    decoded <- dafr:::zarr_v2_decode_chunk(
        gz, "<f8", n = 3L,
        compressor = list(id = "gzip")
    )
    expect_equal(decoded, c(1.0, 2.0, 3.0))
})

test_that("zarr_v2 chunk decode errors clearly on blosc compressor", {
    expect_error(
        dafr:::zarr_v2_decode_chunk(raw(0L), "<f8", n = 0L,
                                    compressor = list(id = "blosc")),
        "codec.*blosc.*not supported"
    )
})

test_that("zarr_v2 chunk decode errors on zstd compressor", {
    expect_error(
        dafr:::zarr_v2_decode_chunk(raw(0L), "<f8", n = 0L,
                                    compressor = list(id = "zstd")),
        "codec.*zstd.*not supported"
    )
})

test_that("zarr_v2 chunk decode errors on lz4 compressor", {
    expect_error(
        dafr:::zarr_v2_decode_chunk(raw(0L), "<f8", n = 0L,
                                    compressor = list(id = "lz4")),
        "codec.*lz4.*not supported"
    )
})

test_that("zarr_v2 chunk encode: empty vector → empty bytes", {
    expect_identical(dafr:::zarr_v2_encode_chunk(double(0L), "<f8"), raw(0L))
    expect_identical(dafr:::zarr_v2_encode_chunk(integer(0L), "<i4"), raw(0L))
})

test_that("zarr_v2 chunk decode: empty bytes → empty vector", {
    expect_identical(dafr:::zarr_v2_decode_chunk(raw(0L), "<f8", n = 0L), double(0L))
    expect_identical(dafr:::zarr_v2_decode_chunk(raw(0L), "<i4", n = 0L), integer(0L))
})

test_that("zarr_v2 NaN and Inf round-trip through <f8", {
    vals <- c(NaN, Inf, -Inf, NA_real_)
    bytes <- dafr:::zarr_v2_encode_chunk(vals, "<f8")
    decoded <- dafr:::zarr_v2_decode_chunk(bytes, "<f8", n = 4L)
    expect_true(is.nan(decoded[1]))
    expect_true(is.infinite(decoded[2]) && decoded[2] > 0)
    expect_true(is.infinite(decoded[3]) && decoded[3] < 0)
    expect_true(is.na(decoded[4]))
})
