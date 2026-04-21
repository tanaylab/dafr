# tests/testthat/test-kernels-slice8.R
# Slice 8 kernel tests — per-task additions.

# ---------------------------------------------------------------------------
# Task 2: kernel_minmax_csc_cpp
# ---------------------------------------------------------------------------

test_that("kernel_minmax_csc matches slow-path on random sparse input", {
    skip_if_not_installed("Matrix")
    set.seed(42)
    m <- Matrix::rsparsematrix(50L, 40L, density = 0.3, rand.x = rnorm)
    # Per-column min/max (axis=1): collapse rows -> one value per col.
    fast_min <- dafr:::kernel_minmax_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 1L, variant = "Min", threshold = 1L)
    fast_max <- dafr:::kernel_minmax_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 1L, variant = "Max", threshold = 1L)
    dense <- as.matrix(m)
    expect_equal(fast_min, matrixStats::colMins(dense))
    expect_equal(fast_max, matrixStats::colMaxs(dense))
    # Per-row min/max (axis=0): collapse cols -> one value per row.
    fast_min_r <- dafr:::kernel_minmax_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 0L, variant = "Min", threshold = 1L)
    fast_max_r <- dafr:::kernel_minmax_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 0L, variant = "Max", threshold = 1L)
    expect_equal(fast_min_r, matrixStats::rowMins(dense))
    expect_equal(fast_max_r, matrixStats::rowMaxs(dense))
})

test_that("kernel_minmax_csc handles all-zero columns and rows", {
    m <- Matrix::sparseMatrix(
        i = c(1L, 3L), j = c(1L, 3L), x = c(5, -2),
        dims = c(4L, 4L))
    # Col 2 and col 4 are all zero: min=max=0.
    mins <- dafr:::kernel_minmax_csc_cpp(
        m@x, m@i, m@p, 4L, 4L, axis = 1L, variant = "Min", threshold = 1L)
    maxs <- dafr:::kernel_minmax_csc_cpp(
        m@x, m@i, m@p, 4L, 4L, axis = 1L, variant = "Max", threshold = 1L)
    expect_equal(mins, c(0, 0, -2, 0))
    expect_equal(maxs, c(5, 0, 0, 0))
})

test_that("Min/Max on sparse query does not densify", {
    m <- Matrix::rsparsematrix(30L, 30L, density = 0.2, rand.x = rnorm)
    daf <- memory_daf("t")
    add_axis(daf, "r", paste0("r", seq_len(30L)))
    add_axis(daf, "c", paste0("c", seq_len(30L)))
    set_matrix(daf, "r", "c", "x", m)
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- Max"))
})
