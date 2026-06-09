#!/usr/bin/env Rscript
# Quantifies the dafr (R) vs DataAxesFormats.jl efficiency gaps surfaced by the
# 2026-06 parity review:
#   1. ZarrDaf reads decode-on-read (readBin + copy) where Julia mmaps the chunk
#      zero-copy.
#   2. Dense matrix reductions (>| Sum) run single-threaded in R; Julia
#      parallelises per-column.
#   3. Dense element-wise ops (% Abs) run single-threaded in R; Julia
#      parallelises.
#   4. (Contrast) % Log already has an OpenMP kernel in R -> expected near-parity,
#      showing the gap is the *missing* kernels, not the architecture.
#
# Run the Julia twin (benchmarks/efficiency-gaps.jl) against the SAME fixture,
# then compare. See bottom of this file for the one-liner.
#
# NOTE: uses devtools::load_all of the source tree so the just-edited code is
# exercised. The compute hot paths are C++/vectorised, so load_all vs installed
# is immaterial here.

suppressMessages(devtools::load_all(
    Sys.getenv("DAFR_PKG", "~/src/dafr-native"), quiet = TRUE))
suppressMessages(library(Matrix))

fixture <- Sys.getenv("DAFR_BENCH_FIXTURE", "/tmp/daf_bench/bench.daf.zarr")
N       <- as.integer(Sys.getenv("DAFR_BENCH_N", "4000"))
M       <- as.integer(Sys.getenv("DAFR_BENCH_M", "2500"))
REPS    <- as.integer(Sys.getenv("DAFR_BENCH_REPS", "7"))
THREADS <- as.integer(Sys.getenv("DAFR_BENCH_THREADS", "16"))
out_csv <- Sys.getenv("DAFR_BENCH_R_OUT", "/tmp/daf_bench/r.csv")

set_num_threads(THREADS)
dir.create(dirname(fixture), showWarnings = FALSE, recursive = TRUE)

if (!dir.exists(fixture)) {
    set.seed(1)
    # Non-negative (resembles expression counts) so `% Log` is well-defined.
    expr <- matrix(rexp(as.numeric(N) * M, rate = 1), N, M)
    d <- zarr_daf(fixture, "w")
    add_axis(d, "cell", sprintf("c%d", seq_len(N)))
    add_axis(d, "gene", sprintf("g%d", seq_len(M)))
    set_matrix(d, "cell", "gene", "expr", expr)
    rm(d, expr)
    gc()
}

med_ms <- function(fn, reps = REPS) {
    fn()  # warmup (JIT/compile, page-cache)
    ts <- numeric(reps)
    for (i in seq_len(reps)) {
        gc(FALSE)
        ts[i] <- system.time(fn())[["elapsed"]]
    }
    stats::median(ts) * 1000
}

# (1) cold ZarrDaf read of the dense matrix (fresh open each rep; sum() forces
# full materialisation).
read_ms <- med_ms(function() {
    d <- zarr_daf(fixture, "r")
    sum(as.numeric(get_matrix(d, "cell", "gene", "expr")))
})

# Warm reader for the compute benches: populate the matrix into the data cache
# once, then clear only the QueryData tier between reps so we time the
# operation, not the read.
dw <- zarr_daf(fixture, "r")
invisible(get_matrix(dw, "cell", "gene", "expr"))

reduce_ms <- med_ms(function() {
    empty_cache(dw, clear = "query")
    get_query(dw, "@ cell @ gene :: expr >| Sum")
})
eltwise_ms <- med_ms(function() {
    empty_cache(dw, clear = "query")
    get_query(dw, "@ cell @ gene :: expr % Abs")
})
log_ms <- med_ms(function() {
    empty_cache(dw, clear = "query")
    get_query(dw, "@ cell @ gene :: expr % Log base 2.0 eps 1.0")
})

# Result checksums so the Julia twin can confirm it computed the same thing.
chk <- function(q) sum(as.numeric(get_query(dw, q)))
res <- data.frame(
    op    = c("zarr_read", "reduce_sum", "eltwise_abs", "log"),
    r_ms  = round(c(read_ms, reduce_ms, eltwise_ms, log_ms), 2),
    check = round(c(NA,
                    chk("@ cell @ gene :: expr >| Sum"),
                    chk("@ cell @ gene :: expr % Abs"),
                    chk("@ cell @ gene :: expr % Log base 2.0 eps 1.0")), 4)
)
write.csv(res, out_csv, row.names = FALSE)
cat(sprintf("fixture %dx%d  reps=%d  R threads=%d\n", N, M, REPS,
            get_num_threads()))
print(res)

# Julia twin:
#   conda run -n dafr-mcview julia -t 16 benchmarks/efficiency-gaps.jl
