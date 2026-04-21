# tests/testthat/test-helpers.R
test_that("assert_no_densify_during flags as.matrix calls", {
    m <- Matrix::sparseMatrix(i = 1:3, j = 1:3, x = c(1, 2, 3))
    expect_silent(assert_no_densify_during(Matrix::rowSums(m)))
    expect_failure(assert_no_densify_during(as.matrix(m)))
})

test_that("assert_no_densify_during works in S4 branch (Matrix attached)", {
    # The S4 branch (trace() on the standardGeneric) requires a real
    # library(Matrix) session.  Under pkgload::load_all() (devtools::test),
    # pkgload re-routes S4 dispatch through the S3 method table, so the S4
    # trace never fires — making this test untestable in CI.  The test is
    # kept for manual verification (run testthat::test_file() outside devtools).
    skip_if(
        requireNamespace("pkgload", quietly = TRUE) &&
            pkgload::is_dev_package("dafr"),
        "S4 dispatch rerouted through S3 under pkgload::load_all; skip in CI"
    )
    m <- Matrix::sparseMatrix(i = 1:3, j = 1:3, x = c(1, 2, 3))
    withr::with_package("Matrix", {
        # positive case: Matrix::rowSums does not densify
        expect_silent(assert_no_densify_during(Matrix::rowSums(m)))
        # negative case: as.matrix(m) does densify
        expect_failure(assert_no_densify_during(as.matrix(m)))
    })
})

test_that("assert_no_densify_during handles nested calls without counter collision", {
    m <- Matrix::sparseMatrix(i = 1:3, j = 1:3, x = c(1, 2, 3))
    expect_silent({
        assert_no_densify_during({
            # inner call uses Matrix::rowSums -> must not fire densify
            assert_no_densify_during(Matrix::rowSums(m))
            # outer expression: another non-densifying op
            Matrix::colSums(m)
        })
    })
})
