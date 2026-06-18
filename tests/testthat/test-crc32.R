test_that("zlib crc32 matches the canonical check value", {
    # crc32("123456789") == 0xCBF43926 (zlib check value).
    expect_equal(dafr:::dafr_crc32_cpp(charToRaw("123456789")), 0xCBF43926)
})

test_that("zlib crc32 of empty input is 0", {
    expect_equal(dafr:::dafr_crc32_cpp(raw(0L)), 0)
})
