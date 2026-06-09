# Live interop: dafr reads a packed ZarrDaf written on the fly by
# DataAxesFormats.jl. Skips unless the Julia env is DAF >= 0.3.0 AND the
# matching optional codec was compiled in. The committed fixtures already give
# deterministic, no-Julia coverage; this pins parity against the live writer.

.write_packed_julia <- function(dir, codec) {
    run_julia(c(
        "using DataAxesFormats, SparseArrays",
        "import DataAxesFormats.PackedFormat as PF",
        sprintf("PF.DAF_PACKED_COMPRESSION = :%s", codec),
        sprintf('d = ZarrDaf(raw"%s/p.daf.zarr", "w"; name="live", packed=true)',
                dir),
        'add_axis!(d, "cell", ["c$(i)" for i in 1:1500])',
        'add_axis!(d, "gene", ["g$(j)" for j in 1:6])',
        'set_vector!(d, "cell", "v", Float64.(1:1500))',
        'set_matrix!(d, "cell", "gene", "dense", reshape(Float64.(1:9000), 1500, 6))',
        'set_matrix!(d, "cell", "gene", "sp", sparse(1:1500, fill(1,1500), Float64.(1:1500), 1500, 6))',
        'println("OK")'))
}

test_that("dafr reads a freshly Julia-written packed store (blosc default)", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3())
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    out <- withr::local_tempdir("daf-packed-live-blosc-")
    res <- .write_packed_julia(out, "blosc_zstd_bitshuffle")
    skip_if_not(any(grepl("^OK$", res)), paste("julia write failed:",
                                               paste(res, collapse = "\n")))
    daf <- zarr_daf(file.path(out, "p.daf.zarr"), mode = "r")
    expect_equal(as.numeric(get_vector(daf, "cell", "v")), as.numeric(1:1500))
    m <- get_matrix(daf, "cell", "gene", "dense")
    expect_equal(dim(m), c(1500L, 6L))
    expect_equal(as.numeric(m), as.numeric(1:9000))
    sp <- get_matrix(daf, "cell", "gene", "sp")
    expect_s4_class(sp, "dgCMatrix")
    expect_equal(sp[1, 1], 1)
    expect_equal(sp[1500, 1], 1500)
})

test_that("dafr reads a freshly Julia-written packed store (plain zstd)", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3())
    skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
    out <- withr::local_tempdir("daf-packed-live-zstd-")
    res <- .write_packed_julia(out, "zstd")
    skip_if_not(any(grepl("^OK$", res)), paste("julia write failed:",
                                               paste(res, collapse = "\n")))
    daf <- zarr_daf(file.path(out, "p.daf.zarr"), mode = "r")
    expect_equal(as.numeric(get_vector(daf, "cell", "v")), as.numeric(1:1500))
    expect_equal(as.numeric(get_matrix(daf, "cell", "gene", "dense")),
                 as.numeric(1:9000))
})
