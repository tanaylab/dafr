# crc32c (Castagnoli) is always compiled and validates the Zarr v3 shard index.

test_that("crc32c matches the canonical check value", {
    # The standard CRC-32C check value for "123456789" is 0xE3069283.
    x <- charToRaw("123456789")
    expect_equal(dafr:::dafr_crc32c_cpp(x), 0xE3069283)
})

test_that("crc32c of empty input is 0", {
    expect_equal(dafr:::dafr_crc32c_cpp(raw(0L)), 0)
})
