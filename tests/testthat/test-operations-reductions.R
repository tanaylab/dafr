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
