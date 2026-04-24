test_that("get_query hits cache on second call", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "age", c(1, 2))
    v1 <- get_query(d, "@ cell : age")
    v2 <- get_query(d, "@ cell : age")
    expect_identical(v1, v2)
    # At least one entry lives in the query tier
    expect_gt(length(ls(S7::prop(d, "cache")$query)), 0L)
})

test_that("cache is invalidated when a vector is overwritten", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "age", c(1, 2))
    v1 <- get_query(d, "@ cell : age")
    set_vector(d, "cell", "age", c(10, 20), overwrite = TRUE)
    v2 <- get_query(d, "@ cell : age")
    expect_equal(unname(v1), c(1, 2))
    expect_equal(unname(v2), c(10, 20))
})
