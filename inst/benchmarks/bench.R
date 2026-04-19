# Slice 0 benchmark script.
#
# Invocation:
#   Rscript inst/benchmarks/bench.R [--small]
#
# Small mode uses 3K x 3K matrix for CI; full mode uses 30K x 30K.

suppressPackageStartupMessages({
  # Work under both install and devtools::load_all() workflows.
  if (!isNamespaceLoaded("dafr")) library(dafr)
  library(Matrix)
  library(bench)
})
source(system.file("benchmarks", "workloads.R", package = "dafr"))

args <- commandArgs(trailingOnly = TRUE)
small <- "--small" %in% args

nrow <- if (small) 3000L else 30000L
ncol <- if (small) 3000L else 30000L
density <- 0.1

cat("Generating synthetic sparse matrix", nrow, "x", ncol, "density", density, "\n")
wl <- make_synthetic_sparse(nrow, ncol, density)
out_dir <- tempfile(); dir.create(out_dir)
write_csc_slots(wl$matrix, out_dir)

# --- Benchmark 1: open cold (mmap_dgCMatrix construction time) ---
bm_open <- bench::mark(
  mmap = mmap_dgCMatrix(
    x_path = file.path(out_dir, "x.bin"),
    i_path = file.path(out_dir, "i.bin"),
    p_path = file.path(out_dir, "p.bin"),
    nrow = nrow, ncol = ncol, nnz = wl$nnz
  ),
  iterations = 20, check = FALSE
)

# --- Benchmark 2: colSums ---
m_mmap <- mmap_dgCMatrix(
  x_path = file.path(out_dir, "x.bin"),
  i_path = file.path(out_dir, "i.bin"),
  p_path = file.path(out_dir, "p.bin"),
  nrow = nrow, ncol = ncol, nnz = wl$nnz
)
bm_cs <- bench::mark(
  native = Matrix::colSums(wl$matrix),
  mmap   = Matrix::colSums(m_mmap),
  iterations = 20, check = TRUE
)

# --- Benchmark 3: transpose ---
bm_t <- bench::mark(
  native = Matrix::t(wl$matrix),
  mmap   = Matrix::t(m_mmap),
  iterations = 5, check = FALSE
)

# Write results.
results_dir <- "~/src/dafr-native/dev/benchmarks"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
ts <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
csv_out <- file.path(results_dir, paste0("slice-0-baseline-", ts, ".csv"))

out <- rbind(
  data.frame(benchmark = "open_cold", expression = as.character(bm_open$expression),
             median_s = as.numeric(bm_open$median)),
  data.frame(benchmark = "colSums",   expression = as.character(bm_cs$expression),
             median_s = as.numeric(bm_cs$median)),
  data.frame(benchmark = "transpose", expression = as.character(bm_t$expression),
             median_s = as.numeric(bm_t$median))
)
write.csv(out, csv_out, row.names = FALSE)
cat("Wrote", csv_out, "\n")
print(out)
