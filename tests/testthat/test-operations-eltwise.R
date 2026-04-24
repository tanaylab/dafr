# Per-op eltwise behaviour tests. Registration / lookup / collision are in
# test-operations-registry.R.  Slice-7 split: this file carries the new ops
# (Clamp, Convert, Fraction, Significant) plus the legacy ones moved from
# test-operations-registry.R in Phase J.

test_that("Clamp on numeric vector respects min and max", {
    fn <- get_eltwise("Clamp")
    expect_equal(fn(c(-2, -1, 0, 1, 2), min = -1, max = 1), c(-1, -1, 0, 1, 1))
})

test_that("Clamp defaults are -Inf / +Inf (pass-through)", {
    fn <- get_eltwise("Clamp")
    expect_equal(fn(c(-5, 0, 5)), c(-5, 0, 5))
})

test_that("Clamp errors on min >= max", {
    fn <- get_eltwise("Clamp")
    expect_error(fn(1:3, min = 2, max = 1), "min.*max")
    expect_error(fn(1:3, min = 1, max = 1), "min.*max")
})

test_that("Clamp preserves sparsity when 0 is in range", {
    m <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = c(-5, 5), dims = c(3, 2))
    out <- get_eltwise("Clamp")(m, min = -2, max = 2)
    expect_s4_class(out, "dgCMatrix")
    expect_equal(out@x, c(-2, 2))
})

test_that("Clamp dense-coerces sparse input when 0 not in range", {
    m <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = c(-5, 5), dims = c(3, 2))
    out <- get_eltwise("Clamp")(m, min = 1, max = 10)
    expect_false(methods::is(out, "dgCMatrix"))
    expect_equal(sum(out == 1), 5)   # 4 former-zero entries + 1 negative entry clamped to 1
    expect_equal(out[1, 1], 1)       # -5 -> 1
    expect_equal(out[3, 2], 5)       # 5 stays 5
})

test_that("Clamp attaches .dafr_builtin = 'Clamp'", {
    fn <- get_eltwise("Clamp")
    expect_identical(attr(fn, ".dafr_builtin"), "Clamp")
})

test_that("Convert changes vector storage mode", {
    fn <- get_eltwise("Convert")
    expect_type(fn(c(1.0, 2.0, 3.0), type = "integer"), "integer")
    expect_equal(fn(c(1.5, 2.9), type = "integer"), c(1L, 2L))  # truncation
    expect_type(fn(c(1L, 2L, 3L), type = "double"), "double")
    expect_type(fn(c(0, 1, 1), type = "logical"), "logical")
    expect_equal(fn(c(0, 1, 2), type = "logical"), c(FALSE, TRUE, TRUE))
})

test_that("Convert requires type parameter", {
    fn <- get_eltwise("Convert")
    expect_error(fn(c(1, 2, 3)), "type")
})

test_that("Convert rejects unknown type names", {
    fn <- get_eltwise("Convert")
    expect_error(fn(c(1, 2, 3), type = "float64"), "type.*double.*integer.*logical")
    expect_error(fn(c(1, 2, 3), type = "string"), "type")
})

test_that("Convert preserves sparsity for target 'double'", {
    m <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = c(1.5, 2.5), dims = c(3, 2))
    out <- get_eltwise("Convert")(m, type = "double")
    expect_s4_class(out, "dgCMatrix")
    expect_equal(out@x, c(1.5, 2.5))
})

test_that("Convert attaches .dafr_builtin", {
    expect_identical(attr(get_eltwise("Convert"), ".dafr_builtin"), "Convert")
})

test_that("Fraction normalises a numeric vector to sum 1", {
    fn <- get_eltwise("Fraction")
    expect_equal(fn(c(1, 1, 2)), c(0.25, 0.25, 0.5))
    expect_equal(fn(c(1, 2, 3)), c(1 / 6, 2 / 6, 3 / 6))
})

test_that("Fraction returns zeros when the vector total is 0", {
    fn <- get_eltwise("Fraction")
    expect_equal(fn(c(0, 0, 0)), c(0, 0, 0))
    expect_equal(fn(c(1, -1, 0)), c(0, 0, 0))  # sum == 0 -> zeros
})

test_that("Fraction normalises each matrix column independently", {
    fn <- get_eltwise("Fraction")
    m <- matrix(c(1, 1, 2, 4), nrow = 2, ncol = 2)  # col sums: 2, 6
    expect_equal(fn(m), matrix(c(0.5, 0.5, 2 / 6, 4 / 6), nrow = 2))
})

test_that("Fraction preserves sparsity on a dgCMatrix", {
    m <- Matrix::sparseMatrix(
        i = c(1, 2, 1, 3), j = c(1, 1, 2, 2), x = c(1, 1, 2, 4),
        dims = c(3, 2)
    )
    out <- get_eltwise("Fraction")(m)
    expect_s4_class(out, "dgCMatrix")
    expect_equal(as.numeric(Matrix::colSums(out)), c(1, 1))
    expect_equal(out[1, 1], 0.5)
    expect_equal(out[3, 2], 4 / 6)
})

test_that("Fraction on a sparse column with sum 0 yields that column all-zero", {
    m <- Matrix::sparseMatrix(
        i = c(1, 2, 1), j = c(1, 1, 2), x = c(1, -1, 5),
        dims = c(3, 2)
    )  # col 1 sum == 0; col 2 sum == 5
    out <- get_eltwise("Fraction")(m)
    expect_s4_class(out, "dgCMatrix")
    expect_equal(as.numeric(Matrix::colSums(out)[1]), 0)
    expect_equal(as.numeric(Matrix::colSums(out)[2]), 1)
})

test_that("Fraction on a bare scalar errors", {
    expect_error(get_eltwise("Fraction")(5), "scalar")
})

test_that("Fraction attaches .dafr_builtin", {
    expect_identical(attr(get_eltwise("Fraction"), ".dafr_builtin"), "Fraction")
})

test_that("Significant zeroes a vector whose max absolute value is below 'high'", {
    fn <- get_eltwise("Significant")
    expect_equal(fn(c(0.1, 0.2, 0.3), high = 1), c(0, 0, 0))
})

test_that("Significant with low == high keeps only entries >= high in abs", {
    fn <- get_eltwise("Significant")
    expect_equal(fn(c(0.5, 2, -3), high = 1), c(0, 2, -3))
})

test_that("Significant with low < high keeps entries >= low once any >= high exists", {
    fn <- get_eltwise("Significant")
    expect_equal(fn(c(0.1, 0.3, 2), high = 1, low = 0.2), c(0, 0.3, 2))
    # 0.1 zeroed (below low); 0.3 kept (above low); 2 kept (above high)
    # but if NO entry reaches high, whole vector is zeroed:
    expect_equal(fn(c(0.1, 0.3, 0.9), high = 1, low = 0.2), c(0, 0, 0))
})

test_that("Significant errors on invalid thresholds", {
    fn <- get_eltwise("Significant")
    expect_error(fn(1:3, high = -1), "high")
    expect_error(fn(1:3, high = 0), "high")
    expect_error(fn(1:3, high = 1, low = -1), "low")
    expect_error(fn(1:3, high = 1, low = 2), "low")
})

test_that("Significant on a matrix operates column-wise", {
    fn <- get_eltwise("Significant")
    m <- matrix(c(0.1, 0.2, 2, 3, 0.05, 0.05), nrow = 2)
    # col 1: max |x| = 0.2 < 1 -> zero out
    # col 2: max |x| = 3 >= 1 -> keep >= 1 in abs -> keep 2, 3
    # col 3: max |x| = 0.05 < 1 -> zero out
    out <- fn(m, high = 1)
    expect_equal(out[, 1], c(0, 0))
    expect_equal(out[, 2], c(2, 3))
    expect_equal(out[, 3], c(0, 0))
})

test_that("Significant on sparse preserves/drops zeros", {
    m <- Matrix::sparseMatrix(
        i = c(1, 2, 1, 3), j = c(1, 1, 2, 2), x = c(0.1, 0.2, 2, 0.5),
        dims = c(3, 2)
    )
    # col 1: max |x| = 0.2 < 1 -> zero out
    # col 2: max |x| = 2 >= 1, low default = 1 -> keep 2, drop 0.5
    out <- get_eltwise("Significant")(m, high = 1)
    expect_s4_class(out, "dgCMatrix")
    expect_equal(as.numeric(out[, 1]), c(0, 0, 0))
    expect_equal(as.numeric(out[, 2]), c(2, 0, 0))
})

test_that("Significant on scalar errors", {
    expect_error(get_eltwise("Significant")(5, high = 1), "scalar")
})

test_that("Significant attaches .dafr_builtin", {
    expect_identical(attr(get_eltwise("Significant"), ".dafr_builtin"), "Significant")
})
