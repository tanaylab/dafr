# benchmarks/slice-8-reduction-kernels.R
# Slice 8 performance gates. Each benchmark block must pass its stated
# ratio for the slice exit. Run with:
#   Rscript benchmarks/slice-8-reduction-kernels.R
# Results are also appended to dev/benchmarks/slice-8-results-<date>.csv.

suppressPackageStartupMessages({
    library(dafr)
    library(bench)
    library(Matrix)
    library(matrixStats)
})

set.seed(20260421L)

# --- Shared fixtures -------------------------------------------------------
make_sparse <- function(nrow = 10000L, ncol = 10000L, density = 0.05) {
    nnz <- as.integer(nrow * ncol * density)
    i <- sample.int(nrow, nnz, replace = TRUE)
    j <- sample.int(ncol, nnz, replace = TRUE)
    x <- rexp(nnz, rate = 1)
    Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(nrow, ncol))
}
make_dense <- function(nrow = 5000L, ncol = 5000L) {
    matrix(rnorm(nrow * ncol), nrow, ncol)
}
make_groups <- function(n, k = 100L) sample.int(k, n, replace = TRUE)

# --- Gate registry ---------------------------------------------------------
# Filled in by each kernel task. Final format: list of
#   list(name, baseline_expr, fast_expr, ratio_target, mem_target)
gates <- list()

gates$minmax_sparse <- list(
    name = "Min/Max sparse row-reduce (10k x 10k, 5% nnz)",
    setup = function() make_sparse(),
    baseline = function(m) matrixStats::rowMaxs(as.matrix(m)),
    fast     = function(m) dafr:::kernel_minmax_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 0L, variant = "Max", threshold = 1024L),
    ratio_target = 5.0,
    mem_ratio_target = 10.0
)

gates$var_sparse <- list(
    name = "Var sparse row-reduce (10k x 10k, 5% nnz)",
    setup = function() make_sparse(),
    baseline = function(m) apply(m, 1L, var),
    fast     = function(m) dafr:::kernel_var_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 0L, variant = "Var",
        eps = 0, threshold = 1024L),
    ratio_target = 10.0,
    mem_ratio_target = NA
)
gates$var_dense <- list(
    name = "Var dense row-reduce (5k x 5k)",
    setup = function() make_dense(),
    baseline = function(m) apply(m, 1L, var),
    fast     = function(m) {
        mu <- rowMeans(m)
        rowMeans(m * m) - mu^2
    },
    ratio_target = 10.0,
    mem_ratio_target = NA
)

gates$geomean_sparse <- list(
    name = "GeoMean sparse row-reduce (10k x 10k, 5% nnz)",
    setup = function() make_sparse(),
    baseline = function(m) apply(m, 1L, function(v) dafr:::.op_geomean(v, eps = 1.0)),
    fast     = function(m) dafr:::kernel_geomean_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 0L, eps = 1.0, threshold = 1024L),
    ratio_target = 10.0,
    mem_ratio_target = NA
)

gates$median_sparse <- list(
    name = "Median sparse row-reduce (10k x 10k, 5% nnz)",
    setup = function() make_sparse(),
    baseline = function(m) apply(m, 1L, median),
    fast     = function(m) dafr:::kernel_quantile_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m), axis = 0L, q = 0.5,
        threshold = 1024L),
    ratio_target = 10.0,
    mem_ratio_target = NA
)

# --- Task 8: grouped reduction engine gates --------------------------------

gates$grouped_sum_sparse_g3 <- list(
    name = "Grouped Sum G3 (100 groups, 10k x 10k, 5% nnz)",
    setup = function() list(m = make_sparse(), g = make_groups(10000L, 100L)),
    baseline = function(d) {
        idx <- split(seq_len(ncol(d$m)), d$g)
        sapply(idx, function(j) Matrix::rowSums(d$m[, j, drop = FALSE]))
    },
    fast = function(d) dafr:::kernel_grouped_reduce_csc_cpp(
        d$m@x, d$m@i, d$m@p, nrow(d$m), ncol(d$m),
        group = as.integer(d$g), ngroups = 100L,
        n_in_group = as.integer(tabulate(d$g, 100L)),
        axis = 3L, op = "Sum", eps = 0, threshold = 1024L),
    ratio_target = 20.0,
    mem_ratio_target = NA
)

gates$grouped_var_sparse_g3 <- list(
    name = "Grouped Var G3 (100 groups, 10k x 10k, 5% nnz)",
    setup = function() list(m = make_sparse(), g = make_groups(10000L, 100L)),
    baseline = function(d) {
        idx <- split(seq_len(ncol(d$m)), d$g)
        sapply(idx, function(j) {
            sub <- d$m[, j, drop = FALSE]
            apply(sub, 1L, function(r) {
                n <- length(r); mu <- mean(r); sum((r - mu)^2) / n
            })
        })
    },
    fast = function(d) dafr:::kernel_grouped_reduce_csc_cpp(
        d$m@x, d$m@i, d$m@p, nrow(d$m), ncol(d$m),
        group = as.integer(d$g), ngroups = 100L,
        n_in_group = as.integer(tabulate(d$g, 100L)),
        axis = 3L, op = "Var", eps = 0, threshold = 1024L),
    ratio_target = 20.0,
    mem_ratio_target = NA
)

# --- Runner (wired in Task 15) ---------------------------------------------
# (Placeholder — replaced in Task 15 with a harness that evaluates every gate.)
cat("Slice 8 benchmark skeleton.\n",
    "Gates so far:", length(gates), "\n")
