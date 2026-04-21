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

# --- Runner (wired in Task 15) ---------------------------------------------
# (Placeholder — replaced in Task 15 with a harness that evaluates every gate.)
cat("Slice 8 benchmark skeleton.\n",
    "Gates so far:", length(gates), "\n")
