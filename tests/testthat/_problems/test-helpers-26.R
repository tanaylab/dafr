# Extracted from test-helpers.R:26

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "dafr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if(
        (requireNamespace("pkgload", quietly = TRUE) &&
            pkgload::is_dev_package("dafr")) ||
            nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")),
        "S4 trace test: skip under devtools::test() and R CMD check; run manually"
    )
m <- Matrix::sparseMatrix(i = 1:3, j = 1:3, x = c(1, 2, 3))
withr::with_package("Matrix", {
        # positive case: Matrix::rowSums does not densify
        expect_silent(assert_no_densify_during(Matrix::rowSums(m)))
        # negative case: as.matrix(m) does densify
        expect_failure(assert_no_densify_during(as.matrix(m)))
    })
