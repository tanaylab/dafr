# tests/testthat/test-helpers.R
test_that("assert_no_densify_during flags as.matrix calls", {
    m <- Matrix::sparseMatrix(i = 1:3, j = 1:3, x = c(1, 2, 3))
    expect_silent(assert_no_densify_during(Matrix::rowSums(m)))
    expect_failure(assert_no_densify_during(as.matrix(m)))
})
