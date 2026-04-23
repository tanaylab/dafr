test_that("summarise() ungrouped returns a 1-row tibble", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10, 20, 30))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::summarise(m = mean(age), n = dplyr::n())
    expect_s3_class(out, "tbl_df")
    expect_false(inherits(out, "daf_axis_tbl"))
    expect_identical(nrow(out), 1L)
    expect_identical(out$m, 20)
    expect_identical(out$n, 3L)
})

test_that("filter() |> summarise() respects the filter", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10, 20, 30))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::filter(age > 15) |>
        dplyr::summarise(m = mean(age))
    expect_identical(out$m, 25)
})

test_that("group_by() stores group var on the tbl", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    g <- dplyr::tbl(d, "cell") |> dplyr::group_by(donor)
    expect_s3_class(g, "daf_axis_tbl")
    expect_identical(attr(g, "groups"), "donor")
})

test_that("grouped summarise with non-axis group var returns a plain tibble", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "B", "A", "B"))
    set_vector(d, "cell", "age", c(10, 20, 30, 40))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(donor) |>
        dplyr::summarise(m = mean(age), n = dplyr::n())
    expect_s3_class(out, "tbl_df")
    expect_false(inherits(out, "daf_axis_tbl"))
    expect_setequal(out$donor, c("A", "B"))
    expect_identical(out$m[out$donor == "A"], 20)
    expect_identical(out$n[out$donor == "A"], 2L)
})

test_that("grouped summarise with axis group var ties back to that axis", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "donor", c("A", "B"))
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "B", "A", "B"))
    set_vector(d, "cell", "age", c(10, 20, 30, 40))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(donor) |>
        dplyr::summarise(mean_age = mean(age), n = dplyr::n())
    expect_s3_class(out, "daf_axis_tbl")
    expect_identical(attr(out, "axis"), "donor")
    df <- dplyr::collect(out)
    expect_setequal(df$name, c("A", "B"))
    expect_true("mean_age" %in% colnames(df))
})

test_that("auto-tie-back with partial coverage is fine", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "donor", c("A", "B", "C"))
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    set_vector(d, "cell", "age", c(10, 20, 30))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(donor) |>
        dplyr::summarise(n = dplyr::n())
    expect_s3_class(out, "daf_axis_tbl")
    expect_identical(attr(out, "axis"), "donor")
    df <- dplyr::collect(out)
    expect_setequal(df$name, c("A", "B"))
})

test_that("ungroup() drops grouping", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    g <- dplyr::tbl(d, "cell") |> dplyr::group_by(donor)
    expect_identical(attr(g, "groups"), "donor")
    u <- dplyr::ungroup(g)
    expect_identical(attr(u, "groups"), character())
})
