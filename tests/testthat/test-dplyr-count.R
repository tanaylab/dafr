test_that("count() by a non-axis var returns a plain tibble", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "B", "A", "B"))
    out <- dplyr::tbl(d, "cell") |> dplyr::count(donor)
    expect_s3_class(out, "tbl_df")
    expect_false(inherits(out, "daf_axis_tbl"))
    expect_setequal(out$donor, c("A", "B"))
    expect_identical(out$n[out$donor == "A"], 2L)
})

test_that("count() by an axis var ties back to that axis", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "donor", c("A", "B"))
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "B", "A", "B"))
    out <- dplyr::tbl(d, "cell") |> dplyr::count(donor)
    expect_s3_class(out, "daf_axis_tbl")
    expect_identical(attr(out, "axis"), "donor")
    df <- dplyr::collect(out)
    expect_setequal(df$name, c("A", "B"))
    expect_identical(df$n[df$name == "A"], 2L)
})

test_that("count(name = 'total') uses the given name", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    out <- dplyr::tbl(d, "cell") |> dplyr::count(donor, name = "total")
    df <- dplyr::collect(out)
    expect_true("total" %in% colnames(df))
    expect_false("n" %in% colnames(df))
})

test_that("count(sort = TRUE) orders by descending count", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4", "c5"))
    set_vector(d, "cell", "donor", c("A", "A", "A", "B", "B"))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::count(donor, sort = TRUE) |>
        dplyr::collect()
    expect_identical(out$donor, c("A", "B"))
})

test_that("tally() with existing groups counts within groups", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    # "donor" is not an axis here, so tally returns a plain tibble.
    out <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(donor) |>
        dplyr::tally()
    expect_s3_class(out, "tbl_df")
    expect_setequal(out$donor, c("A", "B"))
    expect_identical(out$n[out$donor == "A"], 2L)
})

test_that("add_count() adds n without reducing rows", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("A", "B", "A", "B"))
    set_vector(d, "cell", "x", c(1L, 2L, 3L, 4L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::add_count(donor) |>
        dplyr::collect()
    expect_identical(nrow(df), 4L)
    expect_identical(df$n[df$donor == "A"], c(2L, 2L))
    expect_identical(df$n[df$donor == "B"], c(2L, 2L))
})

test_that("add_tally() uses existing groups", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::group_by(donor) |>
        dplyr::add_tally() |>
        dplyr::collect()
    expect_identical(nrow(df), 3L)
    expect_identical(df$n[df$donor == "A"], c(2L, 2L))
    expect_identical(df$n[df$donor == "B"], 1L)
})
