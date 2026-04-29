# Tests for cache_group constants and the per-item classification
# returned by format_get_* / format_set_*. Phase 2 populates the bulk
# of these; Phase 9 adds boundary-case assertions.

test_that("cache_group constants have expected values", {
    expect_identical(MEMORY_DATA, "MemoryData")
    expect_identical(MAPPED_DATA, "MappedData")
    expect_identical(QUERY_DATA, "QueryData")
})

test_that(".is_cache_group accepts the three constants", {
    .is_cache_group <- dafr:::.is_cache_group
    expect_true(.is_cache_group(MEMORY_DATA))
    expect_true(.is_cache_group(MAPPED_DATA))
    expect_true(.is_cache_group(QUERY_DATA))
})

test_that(".is_cache_group rejects bad inputs", {
    .is_cache_group <- dafr:::.is_cache_group
    expect_false(.is_cache_group("memory"))
    expect_false(.is_cache_group(""))
    expect_false(.is_cache_group(NA_character_))
    expect_false(.is_cache_group(c(MEMORY_DATA, MAPPED_DATA)))
    expect_false(.is_cache_group(NULL))
    expect_false(.is_cache_group(1L))
})
