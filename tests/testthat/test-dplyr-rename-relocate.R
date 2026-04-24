test_that("rename() renames a stored column", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "age", c(10L, 20L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::rename(years = age) |>
        dplyr::collect()
    expect_true("years" %in% colnames(df))
    expect_false("age" %in% colnames(df))
    expect_identical(df$years, c(10L, 20L))
})

test_that("rename() renames an override (mutated) column", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "x", c(1, 2))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(sq = x * x) |>
        dplyr::rename(squared = sq) |>
        dplyr::collect()
    expect_true("squared" %in% colnames(df))
    expect_false("sq" %in% colnames(df))
    expect_identical(df$squared, c(1, 4))
})

test_that("rename() survives a subsequent filter", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10L, 20L, 30L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::rename(years = age) |>
        dplyr::filter(years > 15) |>
        dplyr::collect()
    expect_identical(df$years, c(20L, 30L))
    expect_identical(df$name, c("c2", "c3"))
})

test_that("relocate() reorders columns", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "a", c(1L, 2L))
    set_vector(d, "cell", "b", c(10L, 20L))
    set_vector(d, "cell", "c", c(100L, 200L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::relocate(c, b) |>
        dplyr::collect()
    expect_identical(colnames(df), c("c", "b", "name", "a"))
})

test_that("relocate(.after = ...) puts columns after a named one", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "a", c(1L, 2L))
    set_vector(d, "cell", "b", c(10L, 20L))
    set_vector(d, "cell", "c", c(100L, 200L))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::relocate(c, .after = a) |>
        dplyr::collect()
    # name is always first (we put it there in .realize); stored cols in
    # declared order a, b; after relocating c after a: name, a, c, b
    expect_identical(colnames(df), c("name", "a", "c", "b"))
})
