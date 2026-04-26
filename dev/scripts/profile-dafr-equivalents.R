# Measure dafr's equivalents of the Julia DAF.jl queries used in the
# email. Runnable on the lab cluster:
#
#   cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
#   Rscript dev/scripts/profile-dafr-equivalents.R
#
# Uses devtools::load_all() so the source-tree dafr is what's measured.

suppressPackageStartupMessages({
    library(devtools)
    devtools::load_all(".", quiet = TRUE)
    library(bench)
    library(Matrix)
})

# Match the Julia setup: 1 thread.
options(dafr.num_threads = 1L)
Sys.setenv(OMP_NUM_THREADS = "1")

FIXTURE <- "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/benchmarks/fixture/data/big_sparse"

d <- files_daf(FIXTURE, mode = "r")
m <- get_matrix(d, "row", "col", "value")
cat(sprintf("matrix: %d x %d, nnz = %d (%.2f%%)\n\n",
            nrow(m), ncol(m), Matrix::nnzero(m),
            100 * Matrix::nnzero(m) / (nrow(m) * ncol(m))))

# ---- (a) bare Matrix kernels ----
cat("\n=== Bare Matrix kernels (R-level, no dafr DSL) ===\n")
b_colsums  <- bench::mark(Matrix::colSums(m), iterations = 20, check = FALSE)
b_colmeans <- bench::mark(Matrix::colMeans(m), iterations = 20, check = FALSE)
# colVars: Matrix has no method; sparseMatrixStats does.
have_smstat <- requireNamespace("sparseMatrixStats", quietly = TRUE)
b_colvars  <- if (have_smstat) {
    bench::mark(sparseMatrixStats::colVars(m), iterations = 20, check = FALSE)
} else NULL
# colMedians: also via sparseMatrixStats (sparse-aware!) for comparison
b_colmedians <- if (have_smstat) {
    bench::mark(sparseMatrixStats::colMedians(m), iterations = 5, check = FALSE)
} else NULL

# ---- (b) dafr full DSL queries ----
cat("\n=== dafr full DSL queries ===\n")
b_sum <- bench::mark({
    empty_cache(d)
    get_query(d, "@ row @ col :: value >- Sum")
}, iterations = 20, check = FALSE)

b_var <- bench::mark({
    empty_cache(d)
    get_query(d, "@ row @ col :: value >- Var")
}, iterations = 20, check = FALSE)

b_median <- bench::mark({
    empty_cache(d)
    get_query(d, "@ row @ col :: value >- Median")
}, iterations = 5, check = FALSE)

# ---- summary ----
fmt <- function(b) {
    if (is.null(b)) return(c(NA_character_, NA_character_, NA_integer_))
    t  <- bench:::format.bench_time(median(b$time[[1]]))
    mb <- formatC(as.numeric(b$mem_alloc[[1]]) / 1024^2, format = "f", digits = 2)
    n  <- length(b$time[[1]])
    c(t, mb, n)
}

cat("\n========== SUMMARY (dafr side) ==========\n")
cat(sprintf("%-50s %12s %14s %4s\n", "label", "time(median)", "memory(MiB)", "n"))
cat(strrep("-", 84), "\n", sep = "")
rows <- list(
    "(a1) Matrix::colSums(m)"                           = b_colsums,
    "(a2) Matrix::colMeans(m)"                          = b_colmeans,
    "(a3) sparseMatrixStats::colVars(m)"                = b_colvars,
    "(a4) sparseMatrixStats::colMedians(m) [sparse]"    = b_colmedians,
    "(b1) dafr get_query(... Sum)"                      = b_sum,
    "(b2) dafr get_query(... Var)"                      = b_var,
    "(b3) dafr get_query(... Median)"                   = b_median
)
for (lbl in names(rows)) {
    f <- fmt(rows[[lbl]])
    cat(sprintf("%-50s %12s %14s %4s\n", lbl, f[1], f[2], f[3]))
}
