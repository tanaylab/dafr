# Live interop: DataAxesFormats.jl reads a packed ZarrDaf store WRITTEN by dafr.
# The reverse of test-zarr-packed-interop.R. Skips unless the Julia env is DAF
# >= 0.3.0 (and the matching optional codec is compiled into dafr). Proves dafr's
# dual-format shards + sharded zarr.json are read correctly by the Julia writer's
# own reader - the strongest cross-language parity check.

.dafr_write_packed_store <- function(path, codec) {
    withr::local_options(list(dafr.packed_compression = codec))
    daf <- zarr_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1500))
    add_axis(daf, "gene", paste0("g", 1:6))
    set_vector(daf, "cell", "v", as.numeric(1:1500))
    set_matrix(daf, "cell", "gene", "dense",
               matrix(as.numeric(1:9000), nrow = 1500, ncol = 6))
    sm <- Matrix::sparseMatrix(i = 1:1500, j = rep(1L, 1500),
                               x = as.numeric(1:1500), dims = c(1500, 6))
    set_matrix(daf, "cell", "gene", "sp", sm)
    invisible(path)
}

# Julia opens the dafr-written store and prints checks.
.julia_read_checks <- function(path) {
    run_julia(c(
        "using DataAxesFormats, SparseArrays",
        sprintf('d = ZarrDaf(raw"%s", "r")', path),
        'v = get_vector(d, "cell", "v")',
        'm = get_matrix(d, "cell", "gene", "dense")',
        'sp = get_matrix(d, "cell", "gene", "sp")',
        'ok = (v[1]==1.0 && v[1500]==1500.0 && size(m)==(1500,6) && m[1,1]==1.0 && m[1500,6]==9000.0 && sp[1,1]==1.0 && sp[1500,1]==1500.0)',
        'println(ok ? "ALLOK" : "MISMATCH v1=$(v[1]) v1500=$(v[1500]) m11=$(m[1,1]) m_end=$(m[1500,6]) sp=$(sp[1,1]),$(sp[1500,1])")'
    ))
}

for (codec in c("blosc_zstd_bitshuffle", "zstd", "gzip")) {
    local({
        cc <- codec
        test_that(paste0("DataAxesFormats.jl reads a dafr-written packed store: ", cc), {
            skip_on_cran()
            skip_if_not(.daf_jl_uses_zarr_v3())
            if (cc == "blosc_zstd_bitshuffle")
                skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
            if (cc == "zstd")
                skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
            out <- withr::local_tempdir(paste0("dafr-packed-", cc, "-"))
            path <- file.path(out, "p.daf.zarr")
            .dafr_write_packed_store(path, cc)
            res <- .julia_read_checks(path)
            ok <- any(grepl("ALLOK", res))
            if (!ok) cat("JULIA OUTPUT:\n", paste(res, collapse = "\n"), "\n")
            expect_true(ok)
        })
    })
}
