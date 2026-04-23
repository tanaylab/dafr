test_that("compute() with vectors= writes named overrides back as vectors", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "a", c(1, 2, 3))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::mutate(sq = a * a, double = a * 2)
    dplyr::compute(out, vectors = "sq")
    expect_true(has_vector(d, "cell", "sq"))
    expect_false(has_vector(d, "cell", "double"))
    expect_identical(unname(get_vector(d, "cell", "sq")), c(1, 4, 9))
})

test_that("compute() errors when row_mask is partial (filtered tbl)", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "a", c(1, 2, 3))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::filter(a > 1) |>
        dplyr::mutate(sq = a * a)
    expect_error(dplyr::compute(out, vectors = "sq"), "partial|filter|row")
})

test_that("compute() handles permuted-but-full row_mask from arrange()", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "a", c(30, 10, 20))
    out <- dplyr::tbl(d, "cell") |>
        dplyr::arrange(a) |>
        dplyr::mutate(sq = a * a)
    dplyr::compute(out, vectors = "sq")
    expect_identical(unname(get_vector(d, "cell", "sq")), c(900, 100, 400))
})

test_that("compute() errors for names not in overrides", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "a", c(1, 2))
    out <- dplyr::tbl(d, "cell")
    expect_error(dplyr::compute(out, vectors = "sq"), "sq|override|mutate")
})

test_that("compute() default vectors=NULL errors with a hint", {
    skip_if_not_installed("dplyr")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "a", c(1, 2))
    out <- dplyr::tbl(d, "cell") |> dplyr::mutate(sq = a * a)
    expect_error(dplyr::compute(out), "vectors")
})
