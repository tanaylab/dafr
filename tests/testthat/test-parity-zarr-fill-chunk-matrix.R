# Parity fix (follow-up to the dense-vector case): reading a Zarr dense MATRIX
# whose chunk is ABSENT (Zarr's all-fill optimization - Julia/Zarr.jl omit a
# chunk that is entirely fill_value) must reconstruct the fill_value, not error
# "missing chunk". Audit probe zarr-all-zero-dense-missing-chunk (matrix path).

test_that("zarr get_matrix reconstructs an elided all-fill chunk as fill_value", {
    path <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(path, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    # a dense (non-sparse) matrix so it is stored on the dense array path
    set_matrix(d, "cell", "gene", "m",
               matrix(as.double(1:6), 3L, 2L), relayout = FALSE)

    # Simulate Zarr's fill-value chunk elision: delete the data chunk file(s),
    # keeping zarr.json (which carries fill_value, default 0).
    mdir <- file.path(path, "matrices", "cell", "gene", "m")
    chunks <- list.files(mdir, recursive = TRUE, full.names = TRUE)
    chunks <- chunks[!grepl("zarr\\.json$", chunks)]
    file.remove(chunks)

    d2 <- zarr_daf(path, "r")
    m <- as.matrix(get_matrix(d2, "cell", "gene", "m"))
    expect_equal(unname(m), matrix(0, 3L, 2L))
})
