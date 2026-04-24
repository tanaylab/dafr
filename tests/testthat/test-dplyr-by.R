test_that("filter(.by = ...) groups for the call only", {
    skip_if_not_installed("dplyr")
    skip_if(packageVersion("dplyr") < "1.1.0")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4", "c5"))
    set_vector(d, "cell", "donor", c("A", "A", "A", "B", "B"))
    set_vector(d, "cell", "x", c(3L, 1L, 2L, 20L, 10L))
    # Per-donor: keep rows where x equals donor min.
    df <- dplyr::tbl(d, "cell") |>
        dplyr::filter(x == min(x), .by = donor) |>
        dplyr::collect()
    expect_setequal(df$name, c("c2", "c5"))
})

test_that("mutate(.by = ...) computes per group without persistent grouping", {
    skip_if_not_installed("dplyr")
    skip_if(packageVersion("dplyr") < "1.1.0")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "A", "B", "B"))
    set_vector(d, "cell", "x", c(10, 20, 100, 200))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(x_norm = x / sum(x), .by = donor)
    # Result should still be a daf_axis_tbl (mutate preserves rows).
    expect_s3_class(out, "daf_axis_tbl")
    df <- dplyr::collect(out)
    # A: x_norm = 10/30, 20/30.  B: 100/300, 200/300.
    expect_equal(df$x_norm[df$donor == "A"], c(1/3, 2/3))
    expect_equal(df$x_norm[df$donor == "B"], c(1/3, 2/3))
})

test_that("summarise(.by = ...) ties back when .by names an axis", {
    skip_if_not_installed("dplyr")
    skip_if(packageVersion("dplyr") < "1.1.0")
    d <- memory_daf()
    add_axis(d, "donor", c("A", "B"))
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "A", "B", "B"))
    set_vector(d, "cell", "x", c(1, 2, 10, 20))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::summarise(mx = mean(x), .by = donor)
    expect_s3_class(out, "daf_axis_tbl")
    expect_identical(attr(out, "axis"), "donor")
})

test_that("summarise(.by = ...) is exclusive with group_by()", {
    skip_if_not_installed("dplyr")
    skip_if(packageVersion("dplyr") < "1.1.0")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    set_vector(d, "cell", "x", c(1, 2))
    expect_error(
        dplyr::tbl(d, "cell") |>
            dplyr::group_by(donor) |>
            dplyr::summarise(m = mean(x), .by = donor),
        "both"
    )
})
