test_that("Convert sparse->integer preserves sparsity", {
    m <- Matrix::sparseMatrix(i = c(1L, 3L), j = c(1L, 2L),
        x = c(2.0, 3.0), dims = c(4L, 3L))
    out <- dafr:::.op_convert(m, type = "integer")
    expect_s4_class(out, "dgCMatrix")
    expect_equal(out@x, c(2, 3))
})

test_that("Convert sparse->integer errors on non-integer values", {
    m <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 2.5, dims = c(2L, 2L))
    expect_error(dafr:::.op_convert(m, type = "integer"), "integer")
})

test_that("Convert sparse->logical preserves sparsity", {
    m <- Matrix::sparseMatrix(i = c(1L, 2L, 3L), j = c(1L, 2L, 3L),
        x = c(5, 0, -2), dims = c(3L, 3L))
    out <- dafr:::.op_convert(m, type = "logical")
    expect_s4_class(out, "dgCMatrix")
    # After drop0, only entries that were originally nonzero remain
    expect_true(all(out@x %in% c(1)))  # zeros dropped by drop0
})

test_that("Convert sparse->double is a no-op", {
    m <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 3, dims = c(3L, 3L))
    out <- dafr:::.op_convert(m, type = "double")
    expect_identical(out, m)
})

test_that("Convert dense->integer still works", {
    m <- matrix(c(1.0, 2.0, 3.0, 4.0), 2L, 2L)
    out <- dafr:::.op_convert(m, type = "integer")
    expect_true(is.integer(out))
    expect_equal(dim(out), c(2L, 2L))
})
