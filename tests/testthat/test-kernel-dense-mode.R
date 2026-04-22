# tests/testthat/test-kernel-dense-mode.R
# Correctness tests for kernel_mode_dense_cpp (Slice 9c).
# The kernel accepts INTSXP or REALSXP; returns doubles vector.

# Authoritative reference: .op_mode in R/operations.R.
.op_mode_ref <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# ---------------------------------------------------------------------------
# Test 1: Double axis=1 (per-column) exact match vs apply(.op_mode).
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense axis=1 double matches apply(.op_mode)", {
    set.seed(301)
    m <- matrix(sample(c(-1.5, 0.0, 1.5, 2.5), 80L * 20L, replace = TRUE),
                nrow = 80L, ncol = 20L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    ref <- apply(m, 2L, .op_mode_ref)
    expect_identical(got, as.numeric(ref))
})

# ---------------------------------------------------------------------------
# Test 2: Double axis=0 (per-row) exact match.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense axis=0 double matches apply(.op_mode)", {
    set.seed(302)
    m <- matrix(sample(c(-1.5, 0.0, 1.5, 2.5), 20L * 80L, replace = TRUE),
                nrow = 20L, ncol = 80L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 0L, threshold = 1L)
    ref <- apply(m, 1L, .op_mode_ref)
    expect_identical(got, as.numeric(ref))
})

# ---------------------------------------------------------------------------
# Test 3: Tiebreak — first-seen row wins on count equality.
# Column c(1, 2, 1, 2): counts tie at 2; row 0 value 1 is first -> 1.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense tie breaks on first-seen position", {
    m <- matrix(c(1.0, 2.0, 1.0, 2.0,
                  2.0, 1.0, 2.0, 1.0),
                nrow = 4L, ncol = 2L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    # col 1: first row = 1 -> mode = 1
    # col 2: first row = 2 -> mode = 2
    expect_identical(got, c(1.0, 2.0))
})

# ---------------------------------------------------------------------------
# Test 4: All-equal column and all-zeros column.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense all-equal and all-zero columns", {
    m <- matrix(c(7.0, 7.0, 7.0, 7.0,
                  0.0, 0.0, 0.0, 0.0),
                nrow = 4L, ncol = 2L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    expect_identical(got, c(7.0, 0.0))
})

# ---------------------------------------------------------------------------
# Test 5: Int-aware parity: kernel(int_mat) == kernel(as.double(int_mat)).
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense int vs double parity", {
    set.seed(305)
    mi <- matrix(sample(0L:5L, 40L * 15L, replace = TRUE),
                 nrow = 40L, ncol = 15L)
    expect_true(is.integer(mi))
    md <- mi + 0.0
    got_int <- dafr:::kernel_mode_dense_cpp(mi, axis = 1L, threshold = 1L)
    got_dbl <- dafr:::kernel_mode_dense_cpp(md, axis = 1L, threshold = 1L)
    expect_identical(got_int, got_dbl)
})

# ---------------------------------------------------------------------------
# Test 6: NaN column — NaN bucketed as winnable mode.
# Column: NaN appears 3 times, 1.0 appears 2 times -> mode is NaN.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense NaN bucket can win as mode", {
    m <- matrix(c(NaN, NaN, NaN, 1.0, 1.0,
                  1.0, 1.0, 1.0, NaN, NaN),
                nrow = 5L, ncol = 2L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    # col 1: NaN wins (3 vs 2).  col 2: 1.0 wins (3 vs 2).
    expect_true(is.nan(got[1L]))
    # In R, is.na(NaN) is TRUE; distinguish from NA_real_ via is.nan().
    expect_equal(got[2L], 1.0)
})

# ---------------------------------------------------------------------------
# Test 7: NA_REAL bucketed separately from NaN.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense NA_REAL bucket distinct from NaN", {
    m <- matrix(c(NA_real_, NA_real_, NA_real_, 1.0, 1.0), nrow = 5L, ncol = 1L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    expect_true(is.na(got[1L]))
    expect_false(is.nan(got[1L]))  # NA_REAL should be distinguishable
})

# ---------------------------------------------------------------------------
# Test 8: Empty column -> NA_REAL.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense empty column yields NA_REAL", {
    m <- matrix(double(0), nrow = 0L, ncol = 3L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    expect_length(got, 3L)
    expect_true(all(is.na(got)))
})

# ---------------------------------------------------------------------------
# Test 9: Parallelism invariance.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense output is threshold-invariant", {
    set.seed(309)
    m <- matrix(sample(c(0.0, 1.0, 2.0), 100L * 50L, replace = TRUE),
                nrow = 100L, ncol = 50L)
    par    <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    serial <- dafr:::kernel_mode_dense_cpp(m, axis = 1L,
                                           threshold = .Machine$integer.max)
    expect_identical(par, serial)
})
