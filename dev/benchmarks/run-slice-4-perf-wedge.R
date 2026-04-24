#!/usr/bin/env Rscript
# Slice 4 perf-wedge benchmark.
#
# Measures wall-clock + peak RSS for the dominant motif
#   @ cell @ gene :: UMIs % Log eps: 1 >| Sum
# at two representative sizes, with fast paths ON vs OFF.
# Writes results to dev/benchmarks/slice-4-perf-wedge-<DATE>.csv.
#
# IMPORTANT: uses devtools::load_all(".") rather than library(dafr) so the
# in-source dafr.perf.fast_paths gate is actually in effect. The installed
# package does not know about the option.

suppressPackageStartupMessages({
    devtools::load_all(".", quiet = TRUE)
    library(Matrix)
})

make_umis <- function(nrow, ncol, density = 0.05, max_count = 50L, seed = 42L) {
    set.seed(seed)
    nnz <- ceiling(nrow * ncol * density)
    i <- sample.int(nrow, nnz, replace = TRUE)
    j <- sample.int(ncol, nnz, replace = TRUE)
    x <- as.double(sample.int(max_count, nnz, replace = TRUE))
    sparseMatrix(i = i, j = j, x = x, dims = c(nrow, ncol)) |> as("dgCMatrix")
}

# Peak RSS via gc()[,6] = max Vcells used in Mb since last gc(reset=TRUE).
# Resetting "max used" requires gc(reset = TRUE) before the timed block.
bench_one <- function(label, daf, fast_paths) {
    options(dafr.perf.fast_paths = fast_paths)
    # Flush the query cache so we measure a real evaluation, not a cache hit.
    empty_cache(daf, clear = "query")
    invisible(gc(reset = TRUE, verbose = FALSE))
    gc_before <- gc(reset = TRUE, verbose = FALSE)
    # gc() columns: 1=used(ncells|vcells), 2=Mb, 3=gc trigger, 4=Mb,
    # 5=max used (reset by reset=TRUE), 6=Mb of max used.
    base_vcells_mb <- gc_before["Vcells", 2L]
    t0 <- Sys.time()
    result <- get_query(daf, "@ cell @ gene :: UMIs % Log eps: 1 >| Sum")
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")
    gc_after <- gc(verbose = FALSE)
    peak_vcells_mb <- gc_after["Vcells", 6L]
    list(
        label = label,
        fast_paths = fast_paths,
        elapsed_s = elapsed,
        peak_mb = as.numeric(peak_vcells_mb - base_vcells_mb),
        result_len = length(result),
        result_sum = sum(result)
    )
}

shapes <- list(
    list(label = "10k_x_10k", nrow = 10000L, ncol = 10000L),
    list(label = "30k_x_30k", nrow = 30000L, ncol = 30000L)
)

rows <- list()
for (sh in shapes) {
    cat(sprintf("> building %s matrix ...\n", sh$label))
    m <- make_umis(sh$nrow, sh$ncol)
    d <- memory_daf(name = sprintf("bench-%s", sh$label))
    add_axis(d, "cell", sprintf("c%d", seq_len(sh$nrow)))
    add_axis(d, "gene", sprintf("g%d", seq_len(sh$ncol)))
    set_matrix(d, "cell", "gene", "UMIs", m)
    rm(m)
    invisible(gc(verbose = FALSE))
    for (fp in c(FALSE, TRUE)) {
        cat(sprintf("> %s fast_paths=%s\n", sh$label, fp))
        rows[[length(rows) + 1L]] <- bench_one(sh$label, d, fp)
    }
    rm(d)
    invisible(gc(verbose = FALSE))
}

df <- do.call(rbind, lapply(rows, as.data.frame))
date_str <- format(Sys.Date(), "%Y-%m-%d")
out_path <- file.path("dev", "benchmarks",
    sprintf("slice-4-perf-wedge-%s.csv", date_str))
write.csv(df, out_path, row.names = FALSE)
cat("wrote", out_path, "\n")
print(df)
