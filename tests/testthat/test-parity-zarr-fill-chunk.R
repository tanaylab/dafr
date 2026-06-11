# Parity fix: reading a Zarr dense vector whose chunk is ABSENT (Zarr's all-fill
# optimization - Julia/Zarr.jl omit a chunk that is entirely fill_value) must
# reconstruct the fill_value, not error "missing chunk". dafr crashed on any
# all-zero dense vector written by a Zarr that elides fill chunks.
# Audit probe zarr-all-zero-dense-missing-chunk.

test_that("zarr get_vector reconstructs an elided all-fill chunk as fill_value", {
    path <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(path, "w")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "zeros", c(0, 0, 0, 0))
    set_vector(d, "cell", "izeros", as.integer(c(0L, 0L, 0L, 0L)))

    # Simulate Zarr's fill-value chunk elision: delete the data chunk files,
    # keeping zarr.json (which carries fill_value).
    for (vec in c("zeros", "izeros")) {
        vdir <- file.path(path, "vectors", "cell", vec)
        chunks <- list.files(vdir, recursive = TRUE, full.names = TRUE)
        chunks <- chunks[!grepl("zarr\\.json$", chunks)]
        file.remove(chunks)
    }

    d2 <- zarr_daf(path, "r")
    expect_identical(unname(get_vector(d2, "cell", "zeros")), c(0, 0, 0, 0))
    expect_identical(unname(get_vector(d2, "cell", "izeros")), c(0L, 0L, 0L, 0L))
})
