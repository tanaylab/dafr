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
