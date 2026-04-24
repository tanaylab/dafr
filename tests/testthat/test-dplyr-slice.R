test_that("slice() by integer indices returns a daf_axis_tbl", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "x", c(10L, 20L, 30L, 40L))
    out <- dplyr::tbl(d, "cell") |> dplyr::slice(c(1L, 3L))
    expect_s3_class(out, "daf_axis_tbl")
    df <- dplyr::collect(out)
    expect_identical(df$name, c("c1", "c3"))
    expect_identical(df$x, c(10L, 30L))
})

test_that("slice() with negative indices drops rows", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(1L, 2L, 3L))
    df <- dplyr::tbl(d, "cell") |> dplyr::slice(-2L) |> dplyr::collect()
    expect_identical(df$name, c("c1", "c3"))
})

test_that("slice_head() takes the first n rows", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "x", c(1L, 2L, 3L, 4L))
    df <- dplyr::tbl(d, "cell") |> dplyr::slice_head(n = 2) |> dplyr::collect()
    expect_identical(df$name, c("c1", "c2"))
})

test_that("slice_tail() takes the last n rows", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "x", c(1L, 2L, 3L, 4L))
    df <- dplyr::tbl(d, "cell") |> dplyr::slice_tail(n = 2) |> dplyr::collect()
    expect_identical(df$name, c("c3", "c4"))
})

test_that("slice_min() picks rows with smallest order_by", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "x", c(30L, 10L, 20L, 40L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::slice_min(x, n = 2) |>
        dplyr::collect()
    expect_identical(df$name, c("c2", "c3"))
})

test_that("slice_max() picks rows with largest order_by", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "x", c(30L, 10L, 20L, 40L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::slice_max(x, n = 2) |>
        dplyr::collect()
    expect_identical(df$name, c("c4", "c1"))
})

test_that("slice_sample() returns n rows", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", paste0("c", 1:10))
    set_vector(d, "cell", "x", seq_len(10L))
    withr::with_seed(42, {
        df <- dplyr::tbl(d, "cell") |>
            dplyr::slice_sample(n = 3) |>
            dplyr::collect()
    })
    expect_identical(nrow(df), 3L)
    expect_true(all(df$name %in% paste0("c", 1:10)))
})

test_that("slice_head() after mutate() preserves the mutation", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(1, 2, 3))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(sq = x * x) |>
        dplyr::slice_head(n = 2) |>
        dplyr::collect()
    expect_identical(df$name, c("c1", "c2"))
    expect_identical(df$sq, c(1, 4))
})
