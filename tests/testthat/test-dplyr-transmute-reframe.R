test_that("transmute() keeps only new columns (plus axis name)", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "a", c(1, 2, 3))
    set_vector(d, "cell", "b", c(10, 20, 30))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::transmute(s = a + b) |>
        dplyr::collect()
    expect_setequal(colnames(df), c("name", "s"))
    expect_identical(df$s, c(11, 22, 33))
})

test_that("transmute() returns a daf_axis_tbl (row-preserving)", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "a", c(1, 2))
    out <- dplyr::tbl(d, "cell") |> dplyr::transmute(b = a * 2)
    expect_s3_class(out, "daf_axis_tbl")
})

test_that("reframe() allows multi-row per group; returns plain tibble", {
    skip_if_not_installed("dplyr")
    skip_if(packageVersion("dplyr") < "1.1.0")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "A", "B", "B"))
    set_vector(d, "cell", "x", c(1, 2, 10, 20))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(donor) |>
        dplyr::reframe(q = quantile(x, c(0.25, 0.75)))
    expect_s3_class(out, "tbl_df")
    expect_false(inherits(out, "daf_axis_tbl"))
    # 2 groups x 2 quantiles = 4 rows.
    expect_identical(nrow(out), 4L)
})
