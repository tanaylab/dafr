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

# ---------------------------------------------------------------------------
# Task 3: kernel_var_csc_cpp
# ---------------------------------------------------------------------------

test_that("kernel_var_csc matches base var (uncorrected) on random sparse input", {
    skip_if_not_installed("Matrix")
    set.seed(43)
    m <- Matrix::rsparsematrix(80L, 60L, density = 0.25, rand.x = rnorm)
    dense <- as.matrix(m)

    for (axis in c(0L, 1L)) {
        expected_var <- if (axis == 1L)
            apply(dense, 2L, function(v) {
                n <- length(v); mu <- mean(v); sum((v - mu)^2) / n
            })
        else
            apply(dense, 1L, function(v) {
                n <- length(v); mu <- mean(v); sum((v - mu)^2) / n
            })
        got <- dafr:::kernel_var_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = axis, variant = "Var", eps = 0, threshold = 1L)
        expect_equal(got, expected_var, tolerance = 1e-9,
            info = sprintf("axis=%d", axis))

        got_std <- dafr:::kernel_var_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = axis, variant = "Std", eps = 0, threshold = 1L)
        expect_equal(got_std, sqrt(expected_var), tolerance = 1e-9)
    }
})

test_that("kernel_var_csc VarN and StdN match manual computation with eps", {
    skip_if_not_installed("Matrix")
    set.seed(44)
    m <- Matrix::rsparsematrix(50L, 30L, density = 0.3, rand.x = function(n) runif(n, 0.5, 2))
    dense <- as.matrix(m)
    eps <- 1e-3
    # axis=1: per-col (ReduceToRow equivalent)
    means_col <- colMeans(dense)
    vars_col  <- apply(dense, 2L, function(v) {
        n <- length(v); mu <- mean(v); sum((v - mu)^2) / n
    })
    expect_equal(
        dafr:::kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 1L, variant = "VarN", eps = eps, threshold = 1L),
        vars_col / (means_col + eps), tolerance = 1e-9)
    expect_equal(
        dafr:::kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 1L, variant = "StdN", eps = eps, threshold = 1L),
        sqrt(vars_col) / (means_col + eps), tolerance = 1e-9)
    # axis=0: per-row (ReduceToColumn equivalent) — exercises thread-bucket path
    means_row <- rowMeans(dense)
    vars_row  <- apply(dense, 1L, function(v) {
        n <- length(v); mu <- mean(v); sum((v - mu)^2) / n
    })
    expect_equal(
        dafr:::kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, variant = "VarN", eps = eps, threshold = 1L),
        vars_row / (means_row + eps), tolerance = 1e-9)
    expect_equal(
        dafr:::kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, variant = "StdN", eps = eps, threshold = 1L),
        sqrt(vars_row) / (means_row + eps), tolerance = 1e-9)
})

test_that("sparse Var query does not densify (both ReduceToRow and ReduceToColumn)", {
    skip_if_not_installed("Matrix")
    m <- Matrix::rsparsematrix(40L, 40L, density = 0.2, rand.x = rnorm)
    daf <- memory_daf("t")
    add_axis(daf, "r", paste0("r", seq_len(40L)))
    add_axis(daf, "c", paste0("c", seq_len(40L)))
    set_matrix(daf, "r", "c", "x", m)
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- Var"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- Std"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| Var"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| Std"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- VarN eps 0.1"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- StdN eps 0.1"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| VarN eps 0.1"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| StdN eps 0.1"))
})

# ---------------------------------------------------------------------------
# Task 4: kernel_geomean_csc_cpp
# ---------------------------------------------------------------------------

test_that("kernel_geomean_csc matches .op_geomean slow path", {
    skip_if_not_installed("Matrix")
    set.seed(45)
    m <- Matrix::rsparsematrix(50L, 40L, density = 0.2,
        rand.x = function(n) runif(n, 0, 5))
    dense <- as.matrix(m)
    eps <- 1.0
    for (axis in c(0L, 1L)) {
        expected <- if (axis == 1L)
            apply(dense, 2L, function(v) dafr:::.op_geomean(v, eps = eps))
        else
            apply(dense, 1L, function(v) dafr:::.op_geomean(v, eps = eps))
        got <- dafr:::kernel_geomean_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = axis, eps = eps, threshold = 1L)
        expect_equal(got, expected, tolerance = 1e-9,
            info = sprintf("axis=%d", axis))
    }
})

test_that("kernel_geomean_csc eps=0 branch matches .op_geomean (no implicit zeros)", {
    # With eps=0 the kernel should only handle fully-populated columns/rows without
    # implicit zeros (implicit zeros would produce -Inf). Use a fully dense sparse
    # matrix so every entry is stored explicitly.
    skip_if_not_installed("Matrix")
    # Build a small fully-populated dgCMatrix (all positive)
    set.seed(46)
    d <- matrix(runif(12L, 1, 5), 4L, 3L)
    m <- Matrix::Matrix(d, sparse = TRUE)  # dgCMatrix with no structural zeros
    eps <- 0.0
    for (axis in c(0L, 1L)) {
        expected <- if (axis == 1L)
            apply(d, 2L, function(v) dafr:::.op_geomean(v, eps = eps))
        else
            apply(d, 1L, function(v) dafr:::.op_geomean(v, eps = eps))
        got <- dafr:::kernel_geomean_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = axis, eps = eps, threshold = 1L)
        expect_equal(got, expected, tolerance = 1e-9,
            info = sprintf("axis=%d eps=0", axis))
    }
})

test_that("kernel_geomean_csc all-zero column produces correct result", {
    # All-zero column with eps=1: exp(nrow * log(0+1) / nrow) - 1 = exp(0) - 1 = 0
    m <- Matrix::sparseMatrix(
        i = c(1L), j = c(1L), x = c(3.0),
        dims = c(3L, 2L))
    # col 1 has one nnz=3, col 2 is all-zero
    got <- dafr:::kernel_geomean_csc_cpp(
        m@x, m@i, m@p, 3L, 2L, axis = 1L, eps = 1.0, threshold = 1L)
    # col 2: exp(3*log(1)/3) - 1 = exp(0) - 1 = 0
    expect_equal(got[2L], 0.0, tolerance = 1e-12)
    # col 1: exp((log(4) + 2*log(1))/3) - 1
    expected_col1 <- exp((log(4) + 2 * log(1)) / 3) - 1
    expect_equal(got[1L], expected_col1, tolerance = 1e-12)
})

test_that("sparse GeoMean query does not densify (both reduce directions)", {
    skip_if_not_installed("Matrix")
    m <- Matrix::rsparsematrix(30L, 30L, density = 0.3,
        rand.x = function(n) runif(n, 0, 3))
    daf <- memory_daf("t")
    add_axis(daf, "r", paste0("r", seq_len(30L)))
    add_axis(daf, "c", paste0("c", seq_len(30L)))
    set_matrix(daf, "r", "c", "x", m)
    # >- is ReduceToRow (per-column): axis=1
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- GeoMean eps 1.0"))
    # >| is ReduceToColumn (per-row): axis=0
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| GeoMean eps 1.0"))
})

# ---------------------------------------------------------------------------
# Task 5: kernel_quantile_csc_cpp
# ---------------------------------------------------------------------------

test_that("kernel_quantile_csc matches stats::quantile type=7 on random sparse input", {
    skip_if_not_installed("Matrix")
    set.seed(46)
    m <- Matrix::rsparsematrix(100L, 50L, density = 0.3, rand.x = rnorm)
    dense <- as.matrix(m)
    for (q in c(0.0, 0.25, 0.5, 0.75, 1.0)) {
        # axis=1: per-column (ReduceToRow)
        got <- dafr:::kernel_quantile_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 1L, q = q, threshold = 1L)
        expected <- apply(dense, 2L,
            function(v) stats::quantile(v, q, type = 7L, names = FALSE))
        expect_equal(got, unname(expected), tolerance = 1e-9,
            info = sprintf("q=%.2f axis=col", q))

        # axis=0: per-row (ReduceToColumn)
        got_r <- dafr:::kernel_quantile_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, q = q, threshold = 1L)
        expected_r <- apply(dense, 1L,
            function(v) stats::quantile(v, q, type = 7L, names = FALSE))
        expect_equal(got_r, unname(expected_r), tolerance = 1e-9,
            info = sprintf("q=%.2f axis=row", q))
    }
})

test_that("kernel_quantile_csc handles all-zero column (median is 0)", {
    skip_if_not_installed("Matrix")
    m <- Matrix::sparseMatrix(i = integer(), j = integer(), x = double(),
        dims = c(10L, 5L))
    got <- dafr:::kernel_quantile_csc_cpp(m@x, m@i, m@p, 10L, 5L,
        axis = 1L, q = 0.5, threshold = 1L)
    expect_equal(got, rep(0, 5L))
})

test_that("sparse Median/Quantile query does not densify", {
    skip_if_not_installed("Matrix")
    m <- Matrix::rsparsematrix(40L, 30L, density = 0.2, rand.x = rnorm)
    daf <- memory_daf("t")
    add_axis(daf, "r", paste0("r", 1:40))
    add_axis(daf, "c", paste0("c", 1:30))
    set_matrix(daf, "r", "c", "x", m)
    # >- is ReduceToRow (per-col, axis=1); >| is ReduceToColumn (per-row, axis=0)
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- Median"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| Median"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- Quantile p 0.25"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| Quantile p 0.75"))
})

# ---------------------------------------------------------------------------
# Task 6: kernel_mode_csc_cpp
# ---------------------------------------------------------------------------

test_that("kernel_mode_csc matches .op_mode (including tied-count tiebreak)", {
    skip_if_not_installed("Matrix")
    set.seed(47)
    # integer-valued so ties are common
    x <- sample(c(-1, 0, 1, 2, 3), 600L, replace = TRUE,
                prob = c(0.1, 0.5, 0.2, 0.15, 0.05))
    m <- Matrix::sparseMatrix(
        i = rep(seq_len(30L), 20L),
        j = rep(seq_len(20L), each = 30L),
        x = x, dims = c(30L, 20L))
    dense <- as.matrix(m)
    # axis=1: per-column (ReduceToRow direction)
    got <- dafr:::kernel_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 1L, threshold = 1L)
    expected <- apply(dense, 2L, function(v) dafr:::.op_mode(v))
    expect_equal(got, expected)
    # axis=0: per-row (ReduceToColumn direction)
    got_r <- dafr:::kernel_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 0L, threshold = 1L)
    expected_r <- apply(dense, 1L, function(v) dafr:::.op_mode(v))
    expect_equal(got_r, expected_r)
})

test_that("kernel_mode_csc returns 0 when implicit zeros dominate", {
    skip_if_not_installed("Matrix")
    m <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 7, dims = c(10L, 1L))
    got <- dafr:::kernel_mode_csc_cpp(m@x, m@i, m@p, 10L, 1L,
        axis = 1L, threshold = 1L)
    expect_equal(got, 0)
})

test_that("kernel_mode_csc tiebreak: nonzero wins when it appears before first implicit zero", {
    # Column: nonzero at row 0 (value 5), zero at rows 1..3 -> counts tie 1:3.
    # Wait: 5 appears 1x, zero appears 3x -> zero wins, not a tie.
    # Make actual tie: 2 nonzeros at rows 0,1 (value 5); 2 implicit zeros at rows 2,3.
    # First-seen: 5 at row 0, 0 at row 2 -> 5 wins.
    skip_if_not_installed("Matrix")
    m <- Matrix::sparseMatrix(i = c(1L, 2L), j = c(1L, 1L), x = c(5, 5), dims = c(4L, 1L))
    got <- dafr:::kernel_mode_csc_cpp(m@x, m@i, m@p, 4L, 1L,
        axis = 1L, threshold = 1L)
    dense <- as.matrix(m)[, 1L]
    expected <- dafr:::.op_mode(dense)
    expect_equal(got, expected)   # should be 5, not 0
    expect_equal(got, 5)
})

test_that("kernel_mode_csc tiebreak: zero wins when it appears before nonzero", {
    # Nonzeros at rows 3,4 (value 5); implicit zeros at rows 1,2 -> tie 2:2.
    # First-seen: 0 at row 1 (0-based: row 0), 5 at row 3 (0-based: row 2) -> 0 wins.
    skip_if_not_installed("Matrix")
    m <- Matrix::sparseMatrix(i = c(3L, 4L), j = c(1L, 1L), x = c(5, 5), dims = c(4L, 1L))
    got <- dafr:::kernel_mode_csc_cpp(m@x, m@i, m@p, 4L, 1L,
        axis = 1L, threshold = 1L)
    dense <- as.matrix(m)[, 1L]
    expected <- dafr:::.op_mode(dense)
    expect_equal(got, expected)   # should be 0
    expect_equal(got, 0)
})

test_that("kernel_mode_csc handles all-zero column", {
    skip_if_not_installed("Matrix")
    m <- Matrix::sparseMatrix(i = integer(), j = integer(), x = double(), dims = c(5L, 1L))
    got <- dafr:::kernel_mode_csc_cpp(m@x, m@i, m@p, 5L, 1L,
        axis = 1L, threshold = 1L)
    expect_equal(got, 0)
})

test_that("sparse Mode query does not densify", {
    skip_if_not_installed("Matrix")
    set.seed(48)
    m <- Matrix::rsparsematrix(30L, 30L, density = 0.3,
        rand.x = function(n) sample(c(1, 2, 3), n, replace = TRUE))
    daf <- memory_daf("t")
    add_axis(daf, "r", paste0("r", 1:30))
    add_axis(daf, "c", paste0("c", 1:30))
    set_matrix(daf, "r", "c", "x", m)
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >- Mode"))
    assert_no_densify_during(get_query(daf, "@ r @ c :: x >| Mode"))
})

# ---------------------------------------------------------------------------
# Task 7: dense ungrouped fast-path audit smoke test
# ---------------------------------------------------------------------------
# Audit matrix (confirmed by reading .apply_reduction_fast in R/query_eval.R):
#
# Builtin  | sparse (is_dg)           | dense (is_matrix)                 | lgCMatrix
# ---------|--------------------------|-----------------------------------|----------
# Sum      | Matrix::rowSums/colSums  | rowSums/colSums                   | Matrix::rowSums/colSums
# Mean     | Matrix::rowMeans/colMeans| rowMeans/colMeans                 | Matrix::rowMeans/colMeans
# Min      | kernel_minmax_csc_cpp    | matrixStats::rowMins/colMins      | fall-through (NULL -> slow)
# Max      | kernel_minmax_csc_cpp    | matrixStats::rowMaxs/colMaxs      | fall-through (NULL -> slow)
# Var      | kernel_var_csc_cpp       | rowMeans(m*m)-rowMeans(m)^2       | fall-through (NULL -> slow)
# Std      | kernel_var_csc_cpp       | sqrt(rowMeans(m*m)-rowMeans(m)^2) | fall-through (NULL -> slow)
# VarN     | kernel_var_csc_cpp       | v/(mu+eps)                        | fall-through (NULL -> slow)
# StdN     | kernel_var_csc_cpp       | s/(mu+eps)                        | fall-through (NULL -> slow)
# GeoMean  | kernel_geomean_csc_cpp   | exp(rowMeans(log(m+eps)))-eps     | fall-through (NULL -> slow)
# Median   | kernel_quantile_csc_cpp  | matrixStats::rowMedians/colMedians| fall-through (NULL -> slow)
# Quantile | kernel_quantile_csc_cpp  | matrixStats::rowQuantiles/colQ..  | fall-through (NULL -> slow)
# Mode     | kernel_mode_csc_cpp      | apply(.op_mode)                   | fall-through (NULL -> slow)
# Count    | (no fast-path case)      | (no fast-path case)               | fall-through (NULL -> slow)
#
# Sum and Mean are the only ops that handle all three matrix types (is_sparse
# covers both dg and lg). All others correctly fall through for lgCMatrix.
# Count intentionally has no fast path in either branch; slow path always runs.

test_that("dense ungrouped fast paths: shape + finiteness smoke test", {
    set.seed(71)
    # Normal-valued dense matrix: suitable for Sum, Mean, Min, Max, Var, Std, Median, Mode.
    # NOT suitable for GeoMean (negatives -> log(negative) = NaN).
    m <- matrix(rnorm(60L * 40L), 60L, 40L)
    # Strictly-positive dense matrix: required for GeoMean and *N variants
    # (VarN/StdN use mu+eps in denominator; with rnorm mu~0 and eps=0.1 this is
    # fine, but GeoMean calls log(m+eps) which can still be NaN if m+eps<=0).
    mp <- matrix(runif(60L * 40L, 0.5, 2.0), 60L, 40L)

    daf <- memory_daf("t")
    add_axis(daf, "r", paste0("r", 1:60))
    add_axis(daf, "c", paste0("c", 1:40))
    set_matrix(daf, "r", "c", "x", m)
    set_matrix(daf, "r", "c", "xp", mp)

    # ReduceToColumn (>|): collapse cols -> per-row vector; expect length == nrow == 60
    for (op in c("Sum", "Mean", "Min", "Max", "Var", "Std", "Median", "Mode")) {
        out <- get_query(daf, paste("@ r @ c :: x >|", op))
        expect_equal(length(out), 60L,
            label = sprintf("length(ReduceToColumn %s)", op))
        expect_true(all(is.finite(out)),
            label = sprintf("all.finite(ReduceToColumn %s)", op))
    }
    # ReduceToRow (>-): collapse rows -> per-col vector; expect length == ncol == 40
    for (op in c("Sum", "Mean", "Min", "Max", "Var", "Std", "Median", "Mode")) {
        out <- get_query(daf, paste("@ r @ c :: x >-", op))
        expect_equal(length(out), 40L,
            label = sprintf("length(ReduceToRow %s)", op))
        expect_true(all(is.finite(out)),
            label = sprintf("all.finite(ReduceToRow %s)", op))
    }
    # Parametric ops using positive-valued matrix (xp) to avoid log(negative).
    # VarN/StdN: mu+eps with eps=0.1; mp values in [0.5,2] so mu~1.25 >> eps.
    # GeoMean eps=1.0: log(mp+1) always finite since mp>0.
    for (op in c("VarN eps 0.1", "StdN eps 0.1", "GeoMean eps 1.0",
                 "Quantile p 0.25", "Quantile p 0.75")) {
        mat_name <- if (startsWith(op, "Quantile")) "x" else "xp"
        out <- get_query(daf, paste("@ r @ c ::", mat_name, ">|", op))
        expect_equal(length(out), 60L,
            label = sprintf("length(ReduceToColumn %s)", op))
        expect_true(all(is.finite(out)),
            label = sprintf("all.finite(ReduceToColumn %s)", op))
    }
})
