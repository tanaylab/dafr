# Per-op reduction behaviour tests. Slice-7 split: this file carries the new
# ops (Var, Std, VarN, StdN, Median, Quantile, GeoMean, Mode) plus the legacy
# ones moved from test-operations-registry.R in Phase J.

test_that("Var returns the uncorrected variance (n-denom, not n-1)", {
    fn <- get_reduction("Var")
    # x = c(1, 2, 3); mean = 2; corrected (n-1) var = 1; uncorrected (n) = 2/3
    expect_equal(fn(c(1, 2, 3)), 2 / 3)
    expect_equal(fn(c(2, 2, 2)), 0)
})

test_that("Std is sqrt(Var) — uncorrected", {
    fn <- get_reduction("Std")
    expect_equal(fn(c(1, 2, 3)), sqrt(2 / 3))
    expect_equal(fn(c(2, 2, 2)), 0)
})

test_that("Var / Std handle NA via na_rm", {
    expect_true(is.na(get_reduction("Var")(c(1, NA, 3))))
    expect_equal(get_reduction("Var")(c(1, NA, 3), na_rm = TRUE), 1)
    # c(1, 3): mean = 2, uncorrected var = ((1-2)^2 + (3-2)^2) / 2 = 1
    expect_true(is.na(get_reduction("Std")(c(1, NA, 3))))
    expect_equal(get_reduction("Std")(c(1, NA, 3), na_rm = TRUE), 1)
})

test_that("Var on integer input returns double", {
    expect_type(get_reduction("Var")(c(1L, 2L, 3L)), "double")
})

test_that("Var / Std attach .dafr_builtin", {
    expect_identical(attr(get_reduction("Var"), ".dafr_builtin"), "Var")
    expect_identical(attr(get_reduction("Std"), ".dafr_builtin"), "Std")
})

test_that("VarN divides uncorrected variance by mean + eps", {
    fn <- get_reduction("VarN")
    # x = c(1, 2, 3); var = 2/3; mean = 2; VarN = (2/3) / 2 = 1/3
    expect_equal(fn(c(1, 2, 3)), (2 / 3) / 2)
    # with eps: (2/3) / (2 + 1)
    expect_equal(fn(c(1, 2, 3), eps = 1), (2 / 3) / 3)
})

test_that("VarN gives Inf or NaN when mean + eps == 0", {
    # c(-1, 0, 1): mean = 0, var = 2/3; VarN = (2/3) / 0 = Inf
    expect_equal(get_reduction("VarN")(c(-1, 0, 1)), Inf)
})

test_that("StdN divides uncorrected stdev by mean + eps", {
    fn <- get_reduction("StdN")
    expect_equal(fn(c(1, 2, 3)), sqrt(2 / 3) / 2)
    expect_equal(fn(c(1, 2, 3), eps = 1), sqrt(2 / 3) / 3)
})

test_that("VarN / StdN reject negative eps", {
    expect_error(get_reduction("VarN")(1:3, eps = -1), "eps")
    expect_error(get_reduction("StdN")(1:3, eps = -0.5), "eps")
})

test_that("VarN / StdN attach .dafr_builtin", {
    expect_identical(attr(get_reduction("VarN"), ".dafr_builtin"), "VarN")
    expect_identical(attr(get_reduction("StdN"), ".dafr_builtin"), "StdN")
})
