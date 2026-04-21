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

test_that("Min/Max on sparse query does not densify (all four branches)", {
    m <- Matrix::rsparsematrix(30L, 30L, density = 0.2, rand.x = rnorm)
    daf <- memory_daf("t")
    add_axis(daf, "r", paste0("r", seq_len(30L)))
    add_axis(daf, "c", paste0("c", seq_len(30L)))
    set_matrix(daf, "r", "c", "x", m)
    # ReduceToRow (>-): collapse rows -> per-col vector (axis=1 in kernel)
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- Max"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- Min"))
    # ReduceToColumn (>|): collapse cols -> per-row vector (axis=0 in kernel)
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| Max"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| Min"))
})

test_that("lgCMatrix Min/Max falls through without crashing", {
    skip_if_not_installed("Matrix")
    daf <- memory_daf("t")
    add_axis(daf, "r", c("a", "b", "c"))
    add_axis(daf, "c", c("x", "y", "z"))
    m <- Matrix::sparseMatrix(i = 1L, j = 1L, x = TRUE, dims = c(3L, 3L))
    set_matrix(daf, "r", "c", "flag", m)
    expect_no_error(out_max <- get_query(daf, "@ r @ c :: flag >- Max"))
    expect_no_error(out_min <- get_query(daf, "@ r @ c :: flag >- Min"))
    expect_length(out_max, 3L)
    expect_length(out_min, 3L)
})

test_that("dafr.kernel_threshold option is registered", {
    expect_equal(dafr_opt("dafr.kernel_threshold"), 1024L)
    old <- options(dafr.kernel_threshold = 64L); on.exit(options(old))
    expect_equal(dafr_opt("dafr.kernel_threshold"), 64L)
})

test_that("kernel_minmax_csc handles fully-populated columns (no implicit zero fold-in)", {
    # Column is fully populated with negatives: nnz == nrow, so zero should NOT
    # be folded in. Max over this column is the largest negative, NOT 0.
    m <- Matrix::sparseMatrix(
        i = c(1L, 2L, 3L), j = c(1L, 1L, 1L), x = c(-5, -2, -3),
        dims = c(3L, 1L))
    mx <- dafr:::kernel_minmax_csc_cpp(m@x, m@i, m@p, 3L, 1L,
        axis = 1L, variant = "Max", threshold = 1L)
    mn <- dafr:::kernel_minmax_csc_cpp(m@x, m@i, m@p, 3L, 1L,
        axis = 1L, variant = "Min", threshold = 1L)
    expect_equal(mx, -2)   # NOT 0
    expect_equal(mn, -5)
})
