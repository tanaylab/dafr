#!/usr/bin/env Rscript
# Build the big_sparse fixture: 10k × 10k dgCMatrix, ~5 % nnz,
# plus group_100 and group_1000 vectors on rows.

options(error = NULL)   # prevent interactive recover() from ~/.Rprofile in batch mode

suppressPackageStartupMessages({
    library(dafr)
    library(Matrix)
})

.script_dir <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", args, value = TRUE)
    if (length(file_arg) == 0L) stop("cannot determine script path")
    normalizePath(dirname(sub("--file=", "", file_arg)), winslash = "/")
}

fixture_root <- file.path(.script_dir(), "data")
big_dir <- file.path(fixture_root, "big_sparse")
if (dir.exists(big_dir)) unlink(big_dir, recursive = TRUE)

N <- 10000L
nnz_target <- as.integer(0.05 * N * N)
set.seed(9001L)

# Reproducible uniform-random sparse: pick nnz_target (i, j) pairs, dedupe.
idx <- unique(data.frame(
    i = sample.int(N, nnz_target, replace = TRUE),
    j = sample.int(N, nnz_target, replace = TRUE)
))
# top up to ~5 % nnz after dedupe
while (nrow(idx) < nnz_target) {
    extra <- data.frame(
        i = sample.int(N, nnz_target - nrow(idx), replace = TRUE),
        j = sample.int(N, nnz_target - nrow(idx), replace = TRUE)
    )
    idx <- unique(rbind(idx, extra))
}
idx <- idx[seq_len(nnz_target), ]
vals <- runif(nnz_target, min = 0.1, max = 10)

m <- Matrix::sparseMatrix(i = idx$i, j = idx$j, x = vals, dims = c(N, N))
stopifnot(inherits(m, "dgCMatrix"))
rownames(m) <- sprintf("row_%05d", seq_len(N))
colnames(m) <- sprintf("col_%05d", seq_len(N))

# Reproducible group labels
group_100  <- sprintf("g100_%03d",  sample.int(100L,  N, replace = TRUE))
group_1000 <- sprintf("g1000_%04d", sample.int(1000L, N, replace = TRUE))

d <- files_daf(big_dir, name = "big_sparse", mode = "w+")
add_axis(d, "row", rownames(m))
add_axis(d, "col", colnames(m))
set_matrix(d, "row", "col", "value", m)
set_vector(d, "row", "group_100",  group_100)
set_vector(d, "row", "group_1000", group_1000)
rm(d); gc()

cat(sprintf("  built big_sparse           nnz=%d (%.3f%%)\n",
            nnz_target, 100 * nnz_target / (N * N)))
