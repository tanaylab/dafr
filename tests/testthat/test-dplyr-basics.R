test_that("tbl() on a Daf returns a daf_axis_tbl for the named axis", {
    skip_if_not_installed("dplyr")
    skip_if_not_installed("tibble")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    x <- dplyr::tbl(d, "cell")
    expect_s3_class(x, "daf_axis_tbl")
    expect_identical(attr(x, "axis"), "cell")
    expect_true(identical(attr(x, "daf"), d))
})

test_that("tbl() errors for missing axis", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    expect_error(dplyr::tbl(d, "cell"), "axis")
})
