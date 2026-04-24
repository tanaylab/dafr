# Bake-off runner: D (cpp11+BLAS, in dafr) vs B (RcppEigen, in scratch package)
# Reports median time and memory allocations.

suppressPackageStartupMessages({
  devtools::load_all("/home/aviezerl/src/dafr-native", quiet = TRUE)
  library(dafrBakeoffEigen)
  library(bench)
  library(Matrix)
})

set.seed(0)

# ---- Kernel 1: eltwise log(x) + y on 30K*30K doubles ----
n <- 30000L * 30000L  # 900M doubles -> 6.7 GiB each
if (Sys.getenv("SMALL") == "1") n <- 1e7  # 10M for CI

x <- abs(rnorm(n)) + 1e-3
y <- rnorm(n)
bm_log <- bench::mark(
  D_cpp11 = dafr:::kernel_log_add_cpp(x, y),
  B_eigen = eigen_log_add(x, y),
  iterations = 5, check = FALSE, memory = FALSE
)

# ---- Kernel 2: CSC col-sum on 100K*1M, 1% density ----
nr <- 100000L; nc <- 1000000L; dens <- 0.01
if (Sys.getenv("SMALL") == "1") { nr <- 1000L; nc <- 10000L }
nnz <- as.integer(nr * nc * dens)
i_ix <- sample.int(nr, nnz, replace = TRUE)
j_ix <- sort(sample.int(nc, nnz, replace = TRUE))
m <- Matrix::sparseMatrix(i = i_ix, j = j_ix, x = rpois(nnz, 3),
                          dims = c(nr, nc))
bm_cs <- bench::mark(
  D_cpp11 = dafr:::kernel_csc_colsums_cpp(m@x, m@p, ncol(m)),
  B_eigen = eigen_csc_colsums(m),
  iterations = 5, check = TRUE, memory = FALSE
)

# ---- Kernel 3: CSC -> CSR transpose on 100K*100K, 5% density ----
nr <- 100000L; nc <- 100000L; dens <- 0.05
if (Sys.getenv("SMALL") == "1") { nr <- 2000L; nc <- 2000L }
nnz <- as.integer(nr * nc * dens)
i_ix <- sample.int(nr, nnz, replace = TRUE)
j_ix <- sort(sample.int(nc, nnz, replace = TRUE))
m <- Matrix::sparseMatrix(i = i_ix, j = j_ix, x = rpois(nnz, 3),
                          dims = c(nr, nc))
bm_t <- bench::mark(
  D_cpp11 = dafr:::kernel_csc_to_csr_cpp(m@x, m@i, m@p, nr, nc),
  B_eigen = eigen_csc_to_csr(m),
  iterations = 3, check = FALSE, memory = FALSE
)

summary <- list(
  log_add = bm_log[, c("expression", "median")],
  colsums = bm_cs[, c("expression", "median")],
  transpose = bm_t[, c("expression", "median")]
)

print(summary)

out_path <- "/home/aviezerl/src/dafr-native/dev/benchmarks/bake-off-results.csv"
write.csv(do.call(rbind, lapply(names(summary), function(nm) {
  df <- as.data.frame(summary[[nm]])
  df$kernel <- nm
  df$median_s <- as.numeric(df$median)
  df$median <- NULL
  df
})), out_path, row.names = FALSE)
cat("Wrote", out_path, "\n")
