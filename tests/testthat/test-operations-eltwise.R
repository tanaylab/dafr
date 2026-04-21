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
