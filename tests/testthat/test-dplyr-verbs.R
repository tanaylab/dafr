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

test_that("filter() reduces rows; result is still a daf_axis_tbl", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10L, 20L, 30L))
    out <- dplyr::tbl(d, "cell") |> dplyr::filter(age > 15)
    expect_s3_class(out, "daf_axis_tbl")
    df <- dplyr::collect(out)
    expect_identical(df$name, c("c2", "c3"))
    expect_identical(df$age, c(20L, 30L))
})

test_that("filter() composes with select()", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10L, 20L, 30L))
    set_vector(d, "cell", "donor", c("A", "B", "C"))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::filter(age > 15) |>
        dplyr::select(donor) |>
        dplyr::collect()
    expect_identical(df$name, c("c2", "c3"))
    expect_setequal(colnames(df), c("name", "donor"))
})

test_that("filter() supports multi-variable predicates", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10L, 20L, 30L))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::filter(age > 15, donor == "A") |>
        dplyr::collect()
    expect_identical(df$name, "c3")
})

test_that("mutate() adds a computed column visible to later verbs", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "a", c(1, 2, 3))
    set_vector(d, "cell", "b", c(10, 20, 30))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(s = a + b) |>
        dplyr::collect()
    expect_identical(df$s, c(11, 22, 33))
})

test_that("mutate() chains — later expressions see earlier ones", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "a", c(1, 2))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(b = a * 2, c = b + 1) |>
        dplyr::collect()
    expect_identical(df$b, c(2, 4))
    expect_identical(df$c, c(3, 5))
})

test_that("mutate() before filter() survives the filter", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "a", c(1, 2, 3))
    df <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(sq = a * a) |>
        dplyr::filter(sq > 1) |>
        dplyr::collect()
    expect_identical(df$name, c("c2", "c3"))
    expect_identical(df$sq, c(4, 9))
})
