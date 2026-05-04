# `.zmetadata` consolidation invariant (Slice 16, Phase 8). The
# invariant: every ZarrDaf store carries a `.zmetadata` JSON at root
# that mirrors all `.zarray` / `.zattrs` / `.zgroup` files in the tree.
# Updated atomically on every set / delete.

test_that(".zmetadata exists after zarr_daf init", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    expect_true(store_exists(store, ".zmetadata"))
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_identical(zmd$zarr_consolidated_format, 1L)
})

test_that(".zmetadata picks up scalar after set_scalar", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    set_scalar(d, "n", 42L)
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_true("scalars/n/.zarray" %in% names(zmd$metadata))
    # Inline content matches per-file content.
    inline <- zmd$metadata[["scalars/n/.zarray"]]
    expect_identical(inline$dtype, "<i4")
    expect_identical(inline$shape, list(1L))
})

test_that(".zmetadata picks up axis after add_axis", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", c("A", "B"))
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_true("axes/cell/.zarray" %in% names(zmd$metadata))
})

test_that(".zmetadata picks up dense vector after set_vector", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.0, 2.0, 3.0))
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_true("vectors/cell/x/.zarray" %in% names(zmd$metadata))
})

test_that(".zmetadata picks up sparse vector after set_vector", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", paste0("c", 1:5))
    sp <- Matrix::sparseVector(x = c(10, 20), i = c(2L, 4L), length = 5L)
    set_vector(d, "cell", "x", sp)
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_true("vectors/cell/x/.zgroup" %in% names(zmd$metadata))
    expect_true("vectors/cell/x/nzind/.zarray" %in% names(zmd$metadata))
    expect_true("vectors/cell/x/nzval/.zarray" %in% names(zmd$metadata))
    # Asymmetry fixup: NO .zattrs for sparse vectors.
    expect_false("vectors/cell/x/.zattrs" %in% names(zmd$metadata))
})

test_that(".zmetadata picks up dense matrix after set_matrix", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "M", matrix(c(1, 2, 3, 4), nrow = 2))
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_true("matrices/cell/gene/M/.zarray" %in% names(zmd$metadata))
})

test_that(".zmetadata picks up sparse matrix after set_matrix", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1L, 3L), j = c(1L, 2L), x = c(1.0, 2.0),
        dims = c(3L, 2L)
    )
    set_matrix(d, "cell", "gene", "M", sp)
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_true("matrices/cell/gene/M/.zgroup" %in% names(zmd$metadata))
    expect_true("matrices/cell/gene/M/colptr/.zarray" %in% names(zmd$metadata))
    expect_true("matrices/cell/gene/M/rowval/.zarray" %in% names(zmd$metadata))
    expect_true("matrices/cell/gene/M/nzval/.zarray" %in% names(zmd$metadata))
})

test_that(".zmetadata removes scalar entry after delete_scalar", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    set_scalar(d, "x", 1L)
    delete_scalar(d, "x")
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_false("scalars/x/.zarray" %in% names(zmd$metadata))
})

test_that(".zmetadata removes axis entry after delete_axis", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", c("A", "B"))
    delete_axis(d, "cell")
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_false("axes/cell/.zarray" %in% names(zmd$metadata))
})

test_that(".zmetadata removes vector entry after delete_vector", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1, 2))
    delete_vector(d, "cell", "x")
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_false("vectors/cell/x/.zarray" %in% names(zmd$metadata))
})

test_that(".zmetadata removes sparse-vector subtree after delete_vector", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", paste0("c", 1:3))
    sp <- Matrix::sparseVector(x = c(10), i = c(2L), length = 3L)
    set_vector(d, "cell", "x", sp)
    delete_vector(d, "cell", "x")
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    matched <- grep("^vectors/cell/x", names(zmd$metadata), value = TRUE)
    expect_length(matched, 0L)
})

test_that(".zmetadata removes matrix entry after delete_matrix", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "M", matrix(c(1, 2, 3, 4), nrow = 2))
    delete_matrix(d, "cell", "gene", "M")
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_false("matrices/cell/gene/M/.zarray" %in% names(zmd$metadata))
})

test_that(".zmetadata persists through DirStore round-trip", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    set_scalar(d, "n", 7L)
    add_axis(d, "cell", c("A"))
    rm(d)
    expect_true(file.exists(file.path(tmp, ".zmetadata")))
    # Reopen and verify `.zmetadata` reflects what's on disk.
    d2 <- zarr_daf(tmp, mode = "r")
    store <- S7::prop(d2, "store")
    zmd <- dafr:::zarr_v2_read_zmetadata(store)
    expect_true("scalars/n/.zarray" %in% names(zmd$metadata))
    expect_true("axes/cell/.zarray" %in% names(zmd$metadata))
})

# ---- Sparse-vector asymmetry fixup --------------------------------------
#
# Phase 6 wrote `.zattrs` for sparse vectors with `n` and `all_true`.
# Phase 8 drops this — the vector's full length comes from the axis
# length, and `all_true` is inferred from the absence of `nzval/`.
# This aligns with the sparse-matrix layout (no `.zattrs` either).

test_that("sparse vector no longer writes .zattrs (upstream parity)", {
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", paste0("c", 1:5))
    sp <- Matrix::sparseVector(x = c(10, 20), i = c(2L, 4L), length = 5L)
    set_vector(d, "cell", "x", sp)
    expect_false(store_exists(store, "vectors/cell/x/.zattrs"))
    expect_true(store_exists(store, "vectors/cell/x/.zgroup"))
})

test_that("sparse vector all-TRUE Bool inferred from absence of nzval", {
    # No .zattrs, so all_true must be derived from the layout itself.
    d <- zarr_daf(NULL, mode = "w")
    store <- S7::prop(d, "store")
    add_axis(d, "cell", paste0("c", 1:5))
    sp <- Matrix::sparseVector(x = c(TRUE, TRUE, TRUE),
                               i = c(1L, 3L, 5L), length = 5L)
    set_vector(d, "cell", "flag", sp)
    expect_false(store_exists(store, "vectors/cell/flag/.zattrs"))
    expect_false(store_exists(store, "vectors/cell/flag/nzval/.zarray"))
    out <- get_vector(d, "cell", "flag")
    expect_identical(unname(out), c(TRUE, FALSE, TRUE, FALSE, TRUE))
})
