# End-to-end packed/sharded read through files_daf() across all inner codecs.
# A packed FilesDaf property is a `<name>.zip` (or `<name>.<component>.zip`)
# dual-format shard; dafr reads it through the start-located Zarr index, reusing
# the shard-decode core. gzip runs on every build (base-R decode); blosc/zstd
# run only where the optional library was compiled in. Flat sub-threshold
# components (the sparse colptr, the scalar) in the same store must still read.

# Short fixture dir names keep tarball paths under the 100-byte portable limit
# (R's internal tar, used by R CMD build on Windows, errors otherwise).
.FILES_PACKED_DIR <- c(gzip = "gz", zstd = "zs",
                       blosc_zstd_bitshuffle = "bz", blosc_lz4_bitshuffle = "bl")
.files_packed_path <- function(codec) {
    testthat::test_path(sprintf("fixtures/fpk/%s.files", .FILES_PACKED_DIR[[codec]]))
}

.skip_if_no_codec <- function(codec) {
    if (startsWith(codec, "blosc")) {
        testthat::skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    } else if (codec == "zstd") {
        testthat::skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
    }
}

for (codec in c("gzip", "blosc_zstd_bitshuffle", "blosc_lz4_bitshuffle", "zstd")) {
    local({
        cc <- codec
        test_that(paste0("files_daf reads a packed store end-to-end: ", cc), {
            .skip_if_no_codec(cc)
            daf <- files_daf(.files_packed_path(cc), mode = "r")

            # sharded dense vector
            expect_equal(as.numeric(get_vector(daf, "cell", "score")),
                         as.numeric(1:1200))
            # sharded dense matrix (column-major)
            m <- get_matrix(daf, "cell", "gene", "dense")
            expect_equal(dim(m), c(1200L, 8L))
            expect_equal(as.numeric(m), as.numeric(1:(1200 * 8)))
            # sparse matrix with sharded nzval + rowval, flat colptr
            sm <- get_matrix(daf, "cell", "gene", "sparse")
            expect_s4_class(sm, "dgCMatrix")
            expect_equal(dim(sm), c(1200L, 8L))
            expect_equal(sm[1, 1], 1)         # entry k=1: (cell1,gene1)=1
            expect_equal(sm[1, 2], 1201)      # entry k=1201: (cell1,gene2)=ncell+1
            expect_equal(length(sm@x), 2000L) # all distinct nonzeros preserved
            # sharded string vector reads via vlen-utf8
            lab <- get_vector(daf, "cell", "label")
            expect_identical(unname(lab)[1:3], c("v1", "v2", "v3"))
            expect_identical(unname(lab)[1200], "v1200")
            # flat scalar still reads
            expect_identical(get_scalar(daf, "name"), "files-packed!")
        })
    })
}

test_that("packed FilesDaf read errors actionably when c-blosc is absent", {
    if (dafr:::dafr_have_blosc_cpp()) testthat::skip("c-blosc present")
    daf <- files_daf(.files_packed_path("blosc_zstd_bitshuffle"), mode = "r")
    expect_error(get_vector(daf, "cell", "score"), "requires c-blosc")
})

test_that("dafr reads a Julia-written packed Float32 store (live, gzip)", {
    # Parity with DataAxesFormats.jl test/packed_format.jl, which exercises
    # Float32 (4-byte element) packed components; the committed fixtures are
    # Float64. gzip needs no optional codec lib. Live-gated on the Julia env.
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3(), "DAF >= 0.3.0 not available")
    out <- withr::local_tempdir("daf-f32-packed-")
    p <- file.path(out, "f.files")
    res <- run_julia(c(
        "using DataAxesFormats",
        "import DataAxesFormats.PackedFormat as PF",
        "PF.DAF_PACKED_COMPRESSION = :gzip",
        sprintf('d = FilesDaf(raw"%s", "w"; packed=true)', p),
        'add_axis!(d, "cell", ["c$(i)" for i in 1:1500])',
        'add_axis!(d, "gene", ["g$(j)" for j in 1:4])',
        'set_vector!(d, "cell", "v32", Float32.(1:1500))',
        'set_matrix!(d, "cell", "gene", "m32", Matrix{Float32}(reshape(Float32.(1:6000), 1500, 4)))',
        'println("OK")'
    ))
    skip_if_not(any(grepl("^OK$", res)),
                paste0("julia packed write failed:\n", paste(res, collapse = "\n")))
    daf <- files_daf(p, mode = "r")
    expect_equal(as.numeric(get_vector(daf, "cell", "v32")), as.numeric(1:1500))
    m <- get_matrix(daf, "cell", "gene", "m32")
    expect_equal(dim(m), c(1500L, 4L))
    expect_equal(as.numeric(m), as.numeric(1:6000))
})
