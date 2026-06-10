# End-to-end packed/sharded read through zarr_daf() across all inner codecs.
# gzip runs on every build (base-R decode); blosc/zstd run only where the
# optional library was compiled in (skip otherwise). Flat sub-threshold
# components in the same packed store must still read.

.packed_path <- function(codec) {
    testthat::test_path(sprintf("fixtures/daf030-packed/%s.daf.zarr", codec))
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
        test_that(paste0("zarr_daf reads a packed store end-to-end: ", cc), {
            .skip_if_no_codec(cc)
            daf <- zarr_daf(.packed_path(cc), mode = "r")

            # sharded dense vector
            expect_equal(as.numeric(get_vector(daf, "cell", "score")),
                         as.numeric(1:1200))
            # sharded dense matrix (column-major)
            m <- get_matrix(daf, "cell", "gene", "dense")
            expect_equal(dim(m), c(1200L, 8L))
            expect_equal(as.numeric(m), as.numeric(1:(1200 * 8)))
            # sparse matrix with a sharded nzval/rowval component
            sm <- get_matrix(daf, "cell", "gene", "sparse")
            expect_s4_class(sm, "dgCMatrix")
            expect_equal(dim(sm), c(1200L, 8L))
            expect_equal(sm[1, 1], 1)         # entry k=1: (cell1,gene1)=1
            expect_equal(sm[1, 2], 1201)      # entry k=1201: (cell1,gene2)=ncell+1
            expect_equal(length(sm@x), 2000L) # all distinct nonzeros preserved
            # sharded string vector in the same packed store reads via vlen-utf8
            lab <- get_vector(daf, "cell", "label")
            expect_identical(unname(lab)[1:3], c("v1", "v2", "v3"))
            expect_identical(unname(lab)[1200], "v1200")
            # flat scalar still reads
            expect_identical(get_scalar(daf, "name"), "packed!")
        })
    })
}

test_that("packed read errors actionably when c-blosc is absent", {
    if (dafr:::dafr_have_blosc_cpp()) testthat::skip("c-blosc present")
    daf <- zarr_daf(.packed_path("blosc_zstd_bitshuffle"), mode = "r")
    expect_error(get_vector(daf, "cell", "score"), "requires c-blosc")
})
