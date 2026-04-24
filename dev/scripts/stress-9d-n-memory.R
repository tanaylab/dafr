# Slice 9d-N memory/perf profile harness.
# Runs one representative query per CSC axis-0 kernel on the stress fixture
# and reports wall-time and peak RSS (via bench::bench_process_memory).
#
# Reproduce: Rscript dev/scripts/stress-9d-n-memory.R
# For external peak-RSS sampling use:
#   /usr/bin/time -v Rscript dev/scripts/stress-9d-n-memory.R

suppressPackageStartupMessages({
    library(Matrix)
    library(dafr)
    library(bench)
})

set.seed(42L)
nr <- 100000L
nc <- 5000L
density <- 0.02
nnz <- as.integer(nr * nc * density)

cat(sprintf("Fixture: nr=%d nc=%d density=%.3f nnz=%d\n",
            nr, nc, density, nnz))
cat(sprintf("OMP_NUM_THREADS (env)     : %s\n",
            Sys.getenv("OMP_NUM_THREADS", "<unset>")))
cat(sprintf("parallel::detectCores()   : %d\n", parallel::detectCores()))
cat(sprintf("dafr.kernel_threshold     : %s\n",
            format(getOption("dafr.kernel_threshold"))))

m <- Matrix::sparseMatrix(
    i = sample.int(nr, nnz, replace = TRUE),
    j = sample.int(nc, nnz, replace = TRUE),
    x = runif(nnz, 0.1, 10.0),
    dims = c(nr, nc),
    repr = "C"
)

run <- function(label, fn) {
    gc(full = TRUE)
    before <- bench::bench_process_memory()
    t0 <- proc.time()[["elapsed"]]
    out <- fn()
    dt <- proc.time()[["elapsed"]] - t0
    after <- bench::bench_process_memory()
    delta <- as.numeric(after["max"]) - as.numeric(before["max"])
    cat(sprintf("%-40s  wall=%7.3fs  RSS_delta=%7.1f MB  len=%d\n",
                label, dt, delta / (1024 * 1024), length(out)))
    invisible(out)
}

# Category A: thread-bucket kernels.
run("kernel_var_csc (Var, axis=0)", function() {
    dafr:::kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                              axis = 0L, variant = "Var", eps = 0,
                              threshold = 1L)
})
run("kernel_minmax_csc (Max, axis=0)", function() {
    dafr:::kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                 axis = 0L, variant = "Max",
                                 threshold = 1L)
})
run("kernel_log_reduce (Sum, axis=0)", function() {
    dafr:::kernel_log_reduce_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     eps = 1e-5, base = 2,
                                     axis = 0L, reducer = "Sum",
                                     threshold = 1L)
})
run("kernel_geomean_csc (axis=0)", function() {
    dafr:::kernel_geomean_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                  axis = 0L, eps = 1e-5,
                                  threshold = 1L)
})

# Category B: serial-fill kernels.
run("kernel_mode_csc (axis=0)", function() {
    dafr:::kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                               axis = 0L, threshold = 1L)
})
run("kernel_quantile_csc (q=0.5, axis=0)", function() {
    dafr:::kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                   axis = 0L, q = 0.5,
                                   threshold = 1L)
})

cat("\nDone.\n")
