# Verify that verbs outside v1's support surface raise a helpful,
# daf-specific error instead of R's generic "no applicable method".

test_that("joins error with a helpful 'not supported' message", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    x <- dplyr::tbl(d, "cell")
    y <- tibble::tibble(donor = c("A", "B"), age = c(30, 40))
    for (verb in c("inner_join", "left_join", "right_join", "full_join",
                   "semi_join", "anti_join", "cross_join", "nest_join")) {
        fn <- getExportedValue("dplyr", verb)
        expect_error(fn(x, y), "not supported", info = verb)
        expect_error(fn(x, y), "collect", info = verb)
    }
})

test_that("set ops error with a helpful 'not supported' message", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    x <- dplyr::tbl(d, "cell")
    y <- tibble::tibble(name = c("c1", "c2"))
    for (verb in c("union", "union_all", "intersect", "setdiff")) {
        fn <- getExportedValue("dplyr", verb)
        expect_error(fn(x, y), "not supported", info = verb)
    }
})

test_that("rowwise() errors with vectorization hint", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    x <- dplyr::tbl(d, "cell")
    expect_error(dplyr::rowwise(x), "vectorized|collect")
})
