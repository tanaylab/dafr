# DataAxesFormats.jl >> Median on a vector containing NaN returns NaN.
# Base R stats::median treats NaN as NA and returns NA, so dafr's Median
# diverged (Mean / Sum / Min / Max already match because base R's
# mean / sum / min / max preserve NaN).

.fx_nan <- function() {
    d <- memory_daf(name = "mnan")
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    set_vector(d, "cell", "x", c(1.0, NaN, 3.0, NaN, 5.0))
    d
}

test_that(">> Median on a vector containing NaN returns NaN (Julia parity)", {
    d <- .fx_nan()
    expect_true(is.nan(get_query(d, "@ cell : x >> Median")))
})

test_that(">> Median on a vector containing NA (no NaN) still returns NA", {
    d <- memory_daf(name = "mna")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.0, NA_real_, 3.0))
    out <- get_query(d, "@ cell : x >> Median")
    expect_true(is.na(out) && !is.nan(out))
})

test_that(">> Median on a clean vector still computes the median", {
    d <- memory_daf(name = "mc")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.0, 2.0, 3.0))
    expect_equal(get_query(d, "@ cell : x >> Median"), 2.0)
})

test_that("Mean / Sum / Max / Min on NaN already return NaN (regression guard)", {
    d <- .fx_nan()
    expect_true(is.nan(get_query(d, "@ cell : x >> Mean")))
    expect_true(is.nan(get_query(d, "@ cell : x >> Sum")))
    expect_true(is.nan(get_query(d, "@ cell : x >> Max")))
    expect_true(is.nan(get_query(d, "@ cell : x >> Min")))
})
