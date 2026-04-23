test_that("select() trims visible columns; returns daf_axis_tbl", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    set_vector(d, "cell", "age", c(1L, 2L))
    out <- dplyr::tbl(d, "cell") |> dplyr::select(donor)
    expect_s3_class(out, "daf_axis_tbl")
    df <- dplyr::collect(out)
    expect_setequal(colnames(df), c("name", "donor"))
})

test_that("pull() returns a vector for a stored column", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10L, 20L, 30L))
    v <- dplyr::tbl(d, "cell") |> dplyr::pull(age)
    expect_identical(v, c(10L, 20L, 30L))
})

test_that("pull() without arg returns the last column", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "age", c(1L, 2L))
    v <- dplyr::tbl(d, "cell") |> dplyr::pull()
    expect_identical(v, c(1L, 2L))
})
