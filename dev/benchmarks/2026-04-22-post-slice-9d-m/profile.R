#!/usr/bin/env Rscript
# Baseline profile for Slice 9d-M — G3 thread-bucket memory fix.
#
# Runs each of the three grouped CSC kernels in their G3 (axis = 3) branch
# at the locked stress fixture (10k x 10k CSC, 100 groups, density 0.01)
# across OMP_NUM_THREADS in {1, 8, 32, 128}. Reports wall-time per call.
# Peak RSS is collected by wrapping this script in /usr/bin/time -v; see
# run.sh in the same directory.
#
# Writes a CSV at results-<threads>.csv so the wrapper can aggregate.
# Assumes `devtools::load_all` will compile src/ against the current tree.

options(error = NULL)
suppressPackageStartupMessages({
    library(Matrix)
    library(devtools)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("usage: profile.R <threads>")
threads <- as.integer(args[[1]])
stopifnot(!is.na(threads), threads >= 1L)
Sys.setenv(OMP_NUM_THREADS = as.character(threads))

pkg_root <- normalizePath("../../..", mustWork = TRUE)
devtools::load_all(pkg_root, quiet = TRUE)

set.seed(42L)
nrow <- 10000L
ncol <- 10000L
ngroups <- 100L
density <- 0.01
nnz <- as.integer(nrow * ncol * density)

# Fabricate a CSC matrix: uniform random (i, j) with random nonzero values.
ii <- sample.int(nrow, nnz, replace = TRUE)
jj <- sample.int(ncol, nnz, replace = TRUE)
vv <- runif(nnz, min = 0.1, max = 10.0)
m <- sparseMatrix(i = ii, j = jj, x = vv,
                  dims = c(nrow, ncol),
                  repr = "C", giveCsparse = TRUE)
# Column-group vector: round-robin groups 1..ngroups over columns.
group <- rep_len(seq_len(ngroups), ncol)
n_in_group <- tabulate(group, nbins = ngroups)

# Force parallel dispatch regardless of ncol by setting threshold = 1L.
threshold <- 1L

time_kernel <- function(label, fn) {
    # Warm-up pass (JIT compile, etc.).
    invisible(fn())
    gc(verbose = FALSE)
    t0 <- proc.time()
    out <- fn()
    t1 <- proc.time()
    wall_s <- (t1 - t0)[["elapsed"]]
    invisible(out)
    cat(sprintf("[threads=%d] %-18s  wall = %.3f s\n", threads, label, wall_s))
    data.frame(threads = threads, kernel = label, wall_s = wall_s)
}

cat(sprintf("nrow=%d ncol=%d ngroups=%d nnz=%d threads=%d\n",
            nrow, ncol, ngroups, nnz, threads))

rows <- list()
rows[[1]] <- time_kernel("reduce_csc_Sum", function() {
    kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group = group, ngroups = ngroups, n_in_group = n_in_group,
        axis = 3L, op = "Sum", eps = 0.0, threshold = threshold)
})
rows[[2]] <- time_kernel("reduce_csc_Var", function() {
    kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group = group, ngroups = ngroups, n_in_group = n_in_group,
        axis = 3L, op = "Var", eps = 0.0, threshold = threshold)
})
rows[[3]] <- time_kernel("mode_csc", function() {
    kernel_grouped_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group = group, ngroups = ngroups, n_in_group = n_in_group,
        axis = 3L, threshold = threshold)
})
rows[[4]] <- time_kernel("quantile_csc_p50", function() {
    kernel_grouped_quantile_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group = group, ngroups = ngroups, n_in_group = n_in_group,
        axis = 3L, q = 0.5, threshold = threshold)
})

df <- do.call(rbind, rows)
out_path <- sprintf("results-threads-%d.csv", threads)
write.csv(df, out_path, row.names = FALSE)
cat(sprintf("wrote %s\n", out_path))
