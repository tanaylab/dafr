# Workload generators for Slice 0 benchmarks.
# Produces synthetic FilesDaf-like directories; real FilesDaf format comes
# in Slice 2, so for now we emit the raw slot files only.

make_synthetic_sparse <- function(nrow = 30000L, ncol = 30000L, density = 0.1) {
  nnz_est <- as.integer(nrow * ncol * density)
  j <- sort(sample.int(ncol, nnz_est, replace = TRUE))
  i <- sample.int(nrow, nnz_est, replace = TRUE) - 1L  # 0-based
  x <- rpois(nnz_est, lambda = 3)

  # Build dgCMatrix and write its slots.
  m <- Matrix::sparseMatrix(
    i = i + 1L, j = j, x = x,
    dims = c(nrow, ncol), index1 = TRUE
  )

  list(matrix = m, nnz = length(m@x))
}

write_csc_slots <- function(m, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeBin(m@x, file.path(out_dir, "x.bin"), size = 8L)
  writeBin(as.integer(m@i), file.path(out_dir, "i.bin"), size = 4L)
  writeBin(as.integer(m@p), file.path(out_dir, "p.bin"), size = 4L)
  invisible(out_dir)
}
