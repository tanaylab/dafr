# tests/testthat/test-kernel-dense-quantile.R
# Correctness tests for kernel_quantile_dense_cpp (Slice 9c).
# The kernel accepts INTSXP or REALSXP and returns a doubles vector.

# ---------------------------------------------------------------------------
# Test 1: Double matrix, axis=1 (per-column), matches matrixStats::colQuantiles.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense axis=1 double matches matrixStats::colQuantiles", {
    set.seed(201)
    m <- matrix(rnorm(100L * 50L), nrow = 100L, ncol = 50L)
    for (q in c(0, 0.25, 0.5, 0.75, 1.0)) {
        got <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = q, threshold = 1L)
        ref <- matrixStats::colQuantiles(m, probs = q, type = 7L, useNames = FALSE)
        expect_equal(got, as.numeric(ref), tolerance = sqrt(.Machine$double.eps),
                     label = sprintf("axis=1 double q=%g", q))
    }
})

# ---------------------------------------------------------------------------
# Test 2: Double matrix, axis=0 (per-row), matches matrixStats::rowQuantiles.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense axis=0 double matches matrixStats::rowQuantiles", {
    set.seed(202)
    m <- matrix(rnorm(100L * 50L), nrow = 100L, ncol = 50L)
    for (q in c(0, 0.25, 0.5, 0.75, 1.0)) {
        got <- dafr:::kernel_quantile_dense_cpp(m, axis = 0L, q = q, threshold = 1L)
        ref <- matrixStats::rowQuantiles(m, probs = q, type = 7L, useNames = FALSE)
        expect_equal(got, as.numeric(ref), tolerance = sqrt(.Machine$double.eps),
                     label = sprintf("axis=0 double q=%g", q))
    }
})

# ---------------------------------------------------------------------------
# Test 3: Int-aware parity: kernel(int_mat, q) ~= kernel(as.double(int_mat), q).
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense int vs double parity", {
    set.seed(203)
    mi <- matrix(sample(0L:100L, 60L * 30L, replace = TRUE),
                 nrow = 60L, ncol = 30L)
    expect_true(is.integer(mi))
    md <- mi + 0.0
    for (q in c(0.1, 0.5, 0.9)) {
        got_int <- dafr:::kernel_quantile_dense_cpp(mi, axis = 1L, q = q, threshold = 1L)
        got_dbl <- dafr:::kernel_quantile_dense_cpp(md, axis = 1L, q = q, threshold = 1L)
        expect_equal(got_int, got_dbl, tolerance = sqrt(.Machine$double.eps),
                     label = sprintf("int-parity q=%g", q))
    }
})

# ---------------------------------------------------------------------------
# Test 4: Empty input -> NA_REAL (we define empty via zero-length dim).
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense empty column yields NA_REAL", {
    m <- matrix(double(0), nrow = 0L, ncol = 3L)
    got <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = 0.5, threshold = 1L)
    expect_length(got, 3L)
    expect_true(all(is.na(got)))
})

# ---------------------------------------------------------------------------
# Test 5: Single-value column returns that value at any q.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense single-value column is identity", {
    m <- matrix(c(7.0, 42.0, -3.14), nrow = 1L, ncol = 3L)
    for (q in c(0, 0.25, 0.5, 0.75, 1.0)) {
        got <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = q, threshold = 1L)
        expect_equal(got, c(7.0, 42.0, -3.14), tolerance = 0)
    }
})

# ---------------------------------------------------------------------------
# Test 6: NaN in column yields NA_REAL for that column (strict semantics).
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense NaN column yields NA_REAL", {
    m <- matrix(c(1.0, 2.0, NaN, 4.0,     # col 1 has NaN
                  5.0, 6.0, 7.0, 8.0),    # col 2 clean
                nrow = 4L, ncol = 2L)
    got <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = 0.5, threshold = 1L)
    expect_true(is.na(got[1L]))
    expect_false(is.na(got[2L]))
    expect_equal(got[2L], 6.5, tolerance = sqrt(.Machine$double.eps))
})

# ---------------------------------------------------------------------------
# Test 7: NA_INTEGER in Int column yields NA_REAL.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense NA_INTEGER column yields NA_REAL", {
    mi <- matrix(c(1L, 2L, NA_integer_, 4L,
                   5L, 6L, 7L, 8L),
                 nrow = 4L, ncol = 2L)
    got <- dafr:::kernel_quantile_dense_cpp(mi, axis = 1L, q = 0.5, threshold = 1L)
    expect_true(is.na(got[1L]))
    expect_false(is.na(got[2L]))
    expect_equal(got[2L], 6.5, tolerance = sqrt(.Machine$double.eps))
})

# ---------------------------------------------------------------------------
# Test 8: Parallelism invariance: output identical with threshold=1 vs threshold=huge.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense output is threshold-invariant", {
    set.seed(204)
    m <- matrix(rnorm(80L * 40L), nrow = 80L, ncol = 40L)
    par  <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = 0.3, threshold = 1L)
    serial <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = 0.3,
                                               threshold = .Machine$integer.max)
    expect_identical(par, serial)
})
