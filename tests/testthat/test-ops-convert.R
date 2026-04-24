# Tests for Julia type-name aliases and Int64 branch in .op_convert.
# The canonical R names (double/integer/logical) are already tested in
# test-operations-eltwise.R.  This file covers only the new surface:
#   Float32/Float64 -> double
#   Int32           -> integer
#   Int64           -> integer64 (bit64)
#   Bool            -> logical

test_that(".op_convert accepts Julia aliases for Float32/Float64", {
    x <- c(1L, 2L, 3L)
    expect_identical(.op_convert(x, type = "Float32"),
                     .op_convert(x, type = "double"))
    expect_identical(.op_convert(x, type = "Float64"),
                     .op_convert(x, type = "double"))
})

test_that(".op_convert accepts Int32 as integer alias", {
    x <- c(1.0, 2.0, 3.0)
    expect_identical(.op_convert(x, type = "Int32"),
                     .op_convert(x, type = "integer"))
})

test_that(".op_convert accepts Bool as logical alias", {
    x <- c(0, 1, 2)
    expect_identical(.op_convert(x, type = "Bool"),
                     .op_convert(x, type = "logical"))
})

test_that(".op_convert Int64 returns bit64::integer64", {
    x <- c(1L, 2L, 3L)
    out <- .op_convert(x, type = "Int64")
    expect_s3_class(out, "integer64")
    expect_identical(as.integer(out), c(1L, 2L, 3L))
})

test_that(".op_convert Int64 on dense numeric", {
    x <- c(10, 20, 30)
    out <- .op_convert(x, type = "Int64")
    expect_s3_class(out, "integer64")
    expect_identical(as.numeric(out), c(10, 20, 30))
})

test_that(".op_convert Int64 on dgCMatrix densifies (documented)", {
    m <- Matrix::sparseMatrix(i = c(1, 2), j = c(1, 2),
                              x = c(1, 2), dims = c(2, 2))
    out <- .op_convert(m, type = "Int64")
    expect_s3_class(out, "integer64")
    expect_false(methods::is(out, "dgCMatrix"))
})

test_that(".op_convert still errors on unknown type", {
    expect_error(.op_convert(c(1, 2), type = "Float16"),
                 "'type' must be one of")
})

test_that(".op_convert Int64 preserves dim/dimnames on matrix input", {
    m <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3,
                dimnames = list(c("a", "b"), c("x", "y", "z")))
    out <- .op_convert(m, type = "Int64")
    expect_s3_class(out, "integer64")
    expect_equal(dim(out), c(2L, 3L))
    expect_equal(dimnames(out), list(c("a", "b"), c("x", "y", "z")))
    expect_equal(as.integer(out), c(1L, 2L, 3L, 4L, 5L, 6L))
})

test_that(".op_convert Int64 preserves dim after sparse densification", {
    m <- Matrix::sparseMatrix(i = c(1, 2), j = c(1, 2),
                              x = c(10, 20), dims = c(2, 2),
                              dimnames = list(c("r1", "r2"), c("c1", "c2")))
    out <- .op_convert(m, type = "Int64")
    expect_s3_class(out, "integer64")
    expect_equal(dim(out), c(2L, 2L))
    expect_equal(dimnames(out), list(c("r1", "r2"), c("c1", "c2")))
})
