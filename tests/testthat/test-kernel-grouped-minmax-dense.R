# tests/testthat/test-kernel-grouped-minmax-dense.R
# Correctness tests for kernel_grouped_minmax_dense_cpp (Slice 9c).

# Authoritative reference builders (match the pre-slice R fallback at
# R/query_eval.R:1104-1122 and L1152-1170).
# Assumes gi covers groups 1..ngroups contiguously so split() list
# positions equal group labels.  Tests with gaps in the group labels
# should build references inline instead of calling these helpers.
.ref_g2 <- function(m, gi, ngroups, fn) {
    idx <- split(seq_len(nrow(m)), gi)
    out <- matrix(0, ngroups, ncol(m))
    for (g in seq_len(ngroups)) {
        if (length(idx[[g]]) == 0L) { out[g, ] <- NA_real_; next }
        out[g, ] <- fn(m[idx[[g]], , drop = FALSE])
    }
    out
}
.ref_g3 <- function(m, gi, ngroups, fn) {
    idx <- split(seq_len(ncol(m)), gi)
    out <- matrix(0, nrow(m), ngroups)
    for (g in seq_len(ngroups)) {
        if (length(idx[[g]]) == 0L) { out[, g] <- NA_real_; next }
        out[, g] <- fn(m[, idx[[g]], drop = FALSE])
    }
    out
}

# ---------------------------------------------------------------------------
# Test 1: G2 Max (axis=2) on integer matrix.
# Note: the kernel returns +Inf / -Inf for empty groups; the R dispatch
# layer post-processes to NA_REAL via .minmax_empty_to_na.  We test the
# kernel output pre-post-process here.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax G2 Max integer matches per-group colMaxs", {
    set.seed(401)
    m <- matrix(sample(0L:20L, 30L, replace = TRUE), nrow = 6L, ncol = 5L)
    expect_true(is.integer(m))
    gi <- c(1L, 1L, 2L, 2L, 3L, 3L)
    ngroups <- 3L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, groups = gi, ngroups = ngroups, axis = 2L, variant = 1L)
    expected <- .ref_g2(m, gi, ngroups, matrixStats::colMaxs)
    expect_equal(unname(got), unname(expected + 0.0), tolerance = 0)
})

# ---------------------------------------------------------------------------
# Test 2: G2 Min (axis=2).
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax G2 Min integer matches per-group colMins", {
    set.seed(402)
    m <- matrix(sample(0L:20L, 30L, replace = TRUE), nrow = 6L, ncol = 5L)
    gi <- c(1L, 1L, 2L, 2L, 3L, 3L)
    ngroups <- 3L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 0L)
    expected <- .ref_g2(m, gi, ngroups, matrixStats::colMins)
    expect_equal(unname(got), unname(expected + 0.0), tolerance = 0)
})

# ---------------------------------------------------------------------------
# Test 3: G3 Max (axis=3).
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax G3 Max integer matches per-group rowMaxs", {
    set.seed(403)
    m <- matrix(sample(0L:20L, 20L, replace = TRUE), nrow = 4L, ncol = 5L)
    gi <- c(1L, 2L, 1L, 2L, 1L)
    ngroups <- 2L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 3L, variant = 1L)
    expected <- .ref_g3(m, gi, ngroups, matrixStats::rowMaxs)
    expect_equal(unname(got), unname(expected + 0.0), tolerance = 0)
})

# ---------------------------------------------------------------------------
# Test 4: G3 Min (axis=3).
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax G3 Min integer matches per-group rowMins", {
    set.seed(404)
    m <- matrix(sample(0L:20L, 20L, replace = TRUE), nrow = 4L, ncol = 5L)
    gi <- c(1L, 2L, 1L, 2L, 1L)
    ngroups <- 2L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 3L, variant = 0L)
    expected <- .ref_g3(m, gi, ngroups, matrixStats::rowMins)
    expect_equal(unname(got), unname(expected + 0.0), tolerance = 0)
})

# ---------------------------------------------------------------------------
# Test 5: Int vs double parity, all four variants.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax int vs double parity, all variants", {
    set.seed(405)
    mi <- matrix(sample(0L:50L, 6L * 5L, replace = TRUE), nrow = 6L, ncol = 5L)
    expect_true(is.integer(mi))
    md <- mi + 0.0
    gi2 <- c(1L, 1L, 2L, 2L, 3L, 3L); ngroups2 <- 3L
    gi3 <- c(1L, 2L, 1L, 2L, 1L);     ngroups3 <- 2L
    for (axis in c(2L, 3L)) {
        for (variant in c(0L, 1L)) {
            gi      <- if (axis == 2L) gi2 else gi3
            ngroups <- if (axis == 2L) ngroups2 else ngroups3
            got_int <- dafr:::kernel_grouped_minmax_dense_cpp(
                mi, gi, ngroups, axis = axis, variant = variant)
            got_dbl <- dafr:::kernel_grouped_minmax_dense_cpp(
                md, gi, ngroups, axis = axis, variant = variant)
            expect_identical(got_int, got_dbl,
                label = sprintf("axis=%d variant=%d", axis, variant))
        }
    }
})

# ---------------------------------------------------------------------------
# Test 6: NA propagation — NA in one cell makes (row, group) output NA.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax NA propagation per (row, group)", {
    m <- matrix(c(1L, 2L, 3L,    # row 1: group 1
                  4L, 5L, 6L,    # row 2: group 2
                  7L, 8L, 9L,    # row 3: group 1
                  10L, 11L, 12L),
                nrow = 4L, ncol = 3L)
    m[1L, 2L] <- NA_integer_     # row 1 col 2 belongs to group 1
    gi <- c(1L, 2L, 1L, 2L); ngroups <- 2L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 1L)  # G2 Max
    # col 2: group 1 aggregated rows {1, 3} -> row 1 is NA -> NA.
    expect_true(is.na(got[1L, 2L]))
    expect_false(is.na(got[2L, 2L]))
    expect_false(is.na(got[1L, 1L]))
})

# ---------------------------------------------------------------------------
# Test 6b: NaN in double input is treated as NA (propagation).
# ISNAN covers both NaN and NA_REAL, so NaN must also short-circuit
# the (row, group) cell to NA_REAL.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax NaN in double propagates to NA_REAL", {
    m <- matrix(c(1.0, 2.0, 3.0,
                  4.0, 5.0, 6.0,
                  7.0, 8.0, 9.0,
                  10.0, 11.0, 12.0),
                nrow = 4L, ncol = 3L)
    m[1L, 2L] <- NaN   # row 1 col 2 -> group 1 (gi[1] == 1)
    gi <- c(1L, 2L, 1L, 2L); ngroups <- 2L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 1L)
    # col 2, group 1 (rows 1 & 3) has NaN at row 1 -> NA_REAL
    expect_true(is.na(got[1L, 2L]))
    # Other cells unaffected
    expect_false(is.na(got[2L, 2L]))
    expect_false(is.na(got[1L, 1L]))
})

# ---------------------------------------------------------------------------
# Test 7: Empty group — sentinel remains (+Inf or -Inf) pre-post-process.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax empty group produces sentinel", {
    m <- matrix(1:12 + 0.0, nrow = 4L, ncol = 3L)
    gi <- c(1L, 1L, 3L, 3L); ngroups <- 3L   # group 2 is empty
    got_max <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 1L)
    got_min <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 0L)
    expect_true(all(got_max[2L, ] == -Inf))
    expect_true(all(got_min[2L, ] ==  Inf))
})

# ---------------------------------------------------------------------------
# Test 8: Single-element group — that element for every position.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax single-element group returns that element", {
    # R fills matrices column-major: col1 = c(10,20,30), col2 = c(40,50,60).
    # Row 1: c(10, 40), Row 2: c(20, 50), Row 3: c(30, 60).
    m <- matrix(c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0), nrow = 3L, ncol = 2L)
    gi <- c(1L, 2L, 3L); ngroups <- 3L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 1L)  # G2 Max
    # Each group has one row; the max equals that row.
    expect_equal(got[1L, ], c(10.0, 40.0))
    expect_equal(got[2L, ], c(20.0, 50.0))
    expect_equal(got[3L, ], c(30.0, 60.0))
})
