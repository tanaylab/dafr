# Test empty_cache function
test_that("empty_cache works", {
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    test_matrix <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
    set_matrix(daf, "cell", "gene", "UMIs", test_matrix)

    # Get matrix to populate cache
    get_matrix(daf, "cell", "gene", "UMIs")

    # Clear cache
    empty_cache(daf)

    # Should still work after cache is cleared
    result_matrix <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_equal(result_matrix, test_matrix, ignore_attr = TRUE)

    # Test selective cache clearing
    empty_cache(daf, clear = "MappedData")
    empty_cache(daf, keep = "MemoryData")

    # Should still work after cache is cleared
    result_matrix <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_equal(result_matrix, test_matrix, ignore_attr = TRUE)
})
