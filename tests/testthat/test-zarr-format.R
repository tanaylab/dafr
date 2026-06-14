# Helper: create a fresh in-memory ZarrDaf for tests.
.fresh_zarr_daf <- function() {
    zarr_daf(NULL, mode = "w", name = "test")
}

# ---- Constructor + class ------------------------------------------------

test_that("zarr_daf() with no uri returns a ZarrDaf with DictStore", {
    d <- zarr_daf(NULL, mode = "w")
    expect_s7_class(d, ZarrDaf)
    expect_s7_class(S7::prop(d, "store"), DictStore)
})

test_that("zarr_daf() with directory path uses DirStore", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    expect_s7_class(d, ZarrDaf)
    expect_s7_class(S7::prop(d, "store"), DirStore)
})

test_that("zarr_daf() mode='r' on missing store errors", {
    expect_error(zarr_daf(tempfile(), mode = "r"),
        "store does not exist"
    )
})

test_that("zarr_daf() mode='w+' truncates an existing store", {
    tmp <- tempfile(fileext = ".daf.zarr")
    dir.create(tmp)
    file.create(file.path(tmp, "stale.txt"))
    d <- zarr_daf(tmp, mode = "w+")
    expect_false(file.exists(file.path(tmp, "stale.txt")))
})

test_that("zarr_daf() mode='r' returns ZarrDafReadOnly", {
    tmp <- tempfile(fileext = ".daf.zarr")
    zarr_daf(tmp, mode = "w")
    d <- zarr_daf(tmp, mode = "r")
    expect_s7_class(d, ZarrDafReadOnly)
})

# ---- Scalars ------------------------------------------------------------

test_that("ZarrDaf scalars round-trip integer / double / logical / character", {
    d <- .fresh_zarr_daf()
    set_scalar(d, "n_int", 42L)
    set_scalar(d, "n_dbl", 3.14)
    set_scalar(d, "flag", TRUE)
    set_scalar(d, "name", "hello")
    expect_identical(get_scalar(d, "n_int"), 42L)
    expect_equal(get_scalar(d, "n_dbl"), 3.14)
    expect_identical(get_scalar(d, "flag"), TRUE)
    expect_identical(get_scalar(d, "name"), "hello")
})

test_that("ZarrDaf has_scalar and scalars_set", {
    d <- .fresh_zarr_daf()
    expect_false(has_scalar(d, "x"))
    set_scalar(d, "x", 1L)
    set_scalar(d, "y", 2L)
    expect_true(has_scalar(d, "x"))
    expect_setequal(scalars_set(d), c("x", "y"))
})

test_that("ZarrDaf delete_scalar removes scalar", {
    d <- .fresh_zarr_daf()
    set_scalar(d, "x", 1L)
    delete_scalar(d, "x")
    expect_false(has_scalar(d, "x"))
})

test_that("ZarrDaf set_scalar overwrite=FALSE errors on existing", {
    d <- .fresh_zarr_daf()
    set_scalar(d, "x", 1L)
    expect_error(set_scalar(d, "x", 2L), "existing scalar:")
})

test_that("ZarrDaf scalars persist through DirStore round-trip", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    set_scalar(d, "n", 42L)
    set_scalar(d, "name", "hello")
    rm(d)
    d2 <- zarr_daf(tmp, mode = "r")
    expect_identical(get_scalar(d2, "n"), 42L)
    expect_identical(get_scalar(d2, "name"), "hello")
})

test_that("ZarrDafReadOnly rejects set_scalar / delete_scalar", {
    tmp <- tempfile(fileext = ".daf.zarr")
    zarr_daf(tmp, mode = "w")
    d <- zarr_daf(tmp, mode = "r")
    expect_error(set_scalar(d, "x", 1L), "read-only")
    expect_error(delete_scalar(d, "x", must_exist = FALSE), "read-only")
})

# ---- Axes ---------------------------------------------------------------

test_that("ZarrDaf add_axis + axis_vector + axis_length", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    expect_identical(axis_vector(d, "cell"), c("A", "B", "C"))
    expect_identical(axis_length(d, "cell"), 3L)
    expect_setequal(axes_set(d), "cell")
})

test_that("ZarrDaf add_axis rejects duplicates / NA / empty", {
    d <- .fresh_zarr_daf()
    expect_error(add_axis(d, "x", c("A", "A")), "non-unique entries")
    expect_error(add_axis(d, "x", c("A", NA_character_)), "NA")
    expect_error(add_axis(d, "x", c("A", "")), "empty")
})

test_that("ZarrDaf add_axis rejects re-adding existing axis", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A"))
    expect_error(add_axis(d, "cell", c("B")), "existing axis:")
})

test_that("ZarrDaf delete_axis removes axis", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    delete_axis(d, "cell")
    expect_false(has_axis(d, "cell"))
})

test_that("ZarrDaf axes persist through DirStore round-trip", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    rm(d)
    d2 <- zarr_daf(tmp, mode = "r")
    expect_setequal(axes_set(d2), c("cell", "gene"))
    expect_identical(axis_vector(d2, "cell"), c("A", "B", "C"))
    expect_identical(axis_vector(d2, "gene"), c("X", "Y"))
})

test_that("ZarrDafReadOnly rejects add_axis / delete_axis", {
    tmp <- tempfile(fileext = ".daf.zarr")
    zarr_daf(tmp, mode = "w")
    d <- zarr_daf(tmp, mode = "r")
    expect_error(add_axis(d, "x", c("A")), "read-only")
    expect_error(delete_axis(d, "x", must_exist = FALSE), "read-only")
})

test_that("ZarrDaf scalar with multibyte UTF-8 round-trips", {
    d <- .fresh_zarr_daf()
    set_scalar(d, "greeting", "日本語 \U0001F38C")
    expect_identical(
        get_scalar(d, "greeting"),
        "日本語 \U0001F38C"
    )
})

test_that("ZarrDaf axis with multibyte UTF-8 round-trips", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("café", "naïve", "α"))
    expect_identical(
        axis_vector(d, "cell"),
        c("café", "naïve", "α")
    )
})

# ---- Vectors: dense ------------------------------------------------------

test_that("ZarrDaf dense numeric vector round-trip", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.5, 2.5, 3.5))
    expect_equal(unname(get_vector(d, "cell", "x")), c(1.5, 2.5, 3.5))
})

test_that("ZarrDaf dense integer vector round-trip", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(10L, 20L, 30L))
    expect_identical(unname(get_vector(d, "cell", "x")), c(10L, 20L, 30L))
})

test_that("ZarrDaf dense logical vector round-trip", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(TRUE, FALSE, TRUE))
    expect_identical(unname(get_vector(d, "cell", "x")), c(TRUE, FALSE, TRUE))
})

test_that("ZarrDaf dense character vector round-trip", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "label", c("alpha", "beta", "gamma"))
    expect_identical(unname(get_vector(d, "cell", "label")),
                     c("alpha", "beta", "gamma"))
})

test_that("ZarrDaf vectors_set returns sorted names", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "z", c(1, 2))
    set_vector(d, "cell", "x", c(3, 4))
    expect_identical(vectors_set(d, "cell"), c("x", "z"))
})

test_that("ZarrDaf set_vector rejects length mismatch", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    # The error now comes from the centralised .validate_vector_value
    # in set_vector (Round-7 Bug F fix). The message format is
    # `the length: 2 of the vector: x is different from the length: 3
    # of the axis: cell`.
    expect_error(set_vector(d, "cell", "x", c(1, 2)),
                 "length:? 2.*length:? 3")
})

test_that("ZarrDaf set_vector overwrite=FALSE errors on existing", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1, 2))
    expect_error(set_vector(d, "cell", "x", c(3, 4)),
                 "existing vector:")
})

test_that("ZarrDaf delete_vector removes the vector", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1, 2))
    delete_vector(d, "cell", "x")
    expect_false(has_vector(d, "cell", "x"))
})

test_that("ZarrDaf vectors persist through DirStore round-trip", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.0, 2.0, 3.0))
    set_vector(d, "cell", "label", c("alpha", "beta", "gamma"))
    rm(d)
    d2 <- zarr_daf(tmp, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "x")), c(1.0, 2.0, 3.0))
    expect_identical(unname(get_vector(d2, "cell", "label")),
                     c("alpha", "beta", "gamma"))
})

# ---- Vectors: sparse -----------------------------------------------------

test_that("ZarrDaf sparse numeric vector round-trip (densified on read)", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", paste0("c", 1:5))
    sp <- Matrix::sparseVector(x = c(10, 20), i = c(2L, 4L), length = 5L)
    set_vector(d, "cell", "x", sp)
    # Stored sparse but read densified - matches FilesDaf contract
    # (api convention is named atomic vector, not S4 sparseVector).
    store <- S7::prop(d, "store")
    node <- zarr_v3_read_node(store, "vectors/cell/x")
    expect_equal(node$node_type, "group")  # sparse layout is a v3 group
    out <- get_vector(d, "cell", "x")
    expect_equal(unname(out), c(0, 10, 0, 20, 0))
})

test_that("ZarrDaf sparse Bool vector with mixed values", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", paste0("c", 1:4))
    sp <- Matrix::sparseVector(x = c(TRUE, FALSE), i = c(1L, 3L), length = 4L)
    set_vector(d, "cell", "flag", sp)
    out <- get_vector(d, "cell", "flag")
    # Sparse stores only the explicit nzind positions; FALSE in @x for an
    # explicit position still rounds-trips to FALSE (zero-fill).
    expect_identical(unname(out), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("ZarrDaf sparse Bool all-TRUE skips nzval (storage compaction)", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", paste0("c", 1:5))
    sp <- Matrix::sparseVector(x = c(TRUE, TRUE, TRUE),
                               i = c(1L, 3L, 5L), length = 5L)
    set_vector(d, "cell", "flag", sp)
    # Verify the nzval array node does NOT exist in the store.
    store <- S7::prop(d, "store")
    expect_false(store_exists(store, "vectors/cell/flag/nzval/zarr.json"))
    expect_true(store_exists(store, "vectors/cell/flag/nzind/zarr.json"))
    # Round-trip preserves all-TRUE meaning.
    out <- get_vector(d, "cell", "flag")
    expect_identical(unname(out), c(TRUE, FALSE, TRUE, FALSE, TRUE))
})

test_that("ZarrDaf sparse vector persists through DirStore", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", paste0("c", 1:5))
    sp <- Matrix::sparseVector(x = c(10, 20), i = c(2L, 4L), length = 5L)
    set_vector(d, "cell", "x", sp)
    rm(d)
    d2 <- zarr_daf(tmp, mode = "r")
    out <- get_vector(d2, "cell", "x")
    expect_equal(unname(out), c(0, 10, 0, 20, 0))
})

test_that("ZarrDaf vectors_set picks up both dense and sparse names", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", paste0("c", 1:5))
    set_vector(d, "cell", "dense_x", c(1.0, 2.0, 3.0, 4.0, 5.0))
    sp <- Matrix::sparseVector(x = c(10), i = c(2L), length = 5L)
    set_vector(d, "cell", "sparse_x", sp)
    expect_identical(vectors_set(d, "cell"), c("dense_x", "sparse_x"))
})

test_that("ZarrDafReadOnly rejects set_vector / delete_vector", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1, 2))
    rm(d)
    d_ro <- zarr_daf(tmp, mode = "r")
    expect_error(set_vector(d_ro, "cell", "x", c(3, 4), overwrite = TRUE),
                 "read-only")
    expect_error(delete_vector(d_ro, "cell", "x"),
                 "read-only")
})

test_that("ZarrDaf set_vector overwrite=TRUE replaces dense with sparse", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1, 2, 3))
    sp <- Matrix::sparseVector(x = c(10), i = c(2L), length = 3L)
    set_vector(d, "cell", "x", sp, overwrite = TRUE)
    # On disk the layout flipped from a dense array node to a sparse group node.
    store <- S7::prop(d, "store")
    node <- zarr_v3_read_node(store, "vectors/cell/x")
    expect_equal(node$node_type, "group")
    out <- get_vector(d, "cell", "x")
    expect_equal(unname(out), c(0, 10, 0))
})

# ---- Matrices: dense ----------------------------------------------------

test_that("ZarrDaf dense numeric matrix round-trip", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
    set_matrix(d, "cell", "gene", "M", m)
    out <- get_matrix(d, "cell", "gene", "M")
    expect_equal(unname(out), m)
})

test_that("ZarrDaf dense integer matrix round-trip", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- matrix(c(10L, 20L, 30L, 40L, 50L, 60L), nrow = 2)
    set_matrix(d, "cell", "gene", "M", m)
    out <- get_matrix(d, "cell", "gene", "M")
    expect_identical(unname(out), m)
})

test_that("ZarrDaf dense logical matrix round-trip", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2)
    set_matrix(d, "cell", "gene", "M", m)
    out <- get_matrix(d, "cell", "gene", "M")
    expect_identical(unname(out), m)
})

test_that("ZarrDaf dense matrix shape on disk is reversed (upstream-compatible)", {
    # DAF 0.3.0 stores the v3 array shape as [n_cols, n_rows] (column-major,
    # no `order` field in Zarr v3). Pin this so cross-language reads work.
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
    set_matrix(d, "cell", "gene", "M", m)
    store <- S7::prop(d, "store")
    meta <- zarr_v3_read_array(store, "matrices/cell/gene/M")
    expect_equal(as.integer(meta$shape[[1L]]), 2L)  # n_cols (gene)
    expect_equal(as.integer(meta$shape[[2L]]), 3L)  # n_rows (cell)
})

test_that("ZarrDaf matrices_set returns sorted names", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4), nrow = 2)
    set_matrix(d, "cell", "gene", "z", m)
    set_matrix(d, "cell", "gene", "x", m)
    expect_identical(matrices_set(d, "cell", "gene"), c("x", "z"))
})

test_that("ZarrDaf set_matrix rejects shape mismatch", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    # Error now comes from the centralised .validate_matrix_value
    # (Round-7 follow-up G7). Format is `matrix 'M' has dim 2 x 2
    # (expected 3 x 2)`.
    expect_error(
        set_matrix(d, "cell", "gene", "M", matrix(c(1, 2, 3, 4), nrow = 2)),
        "dim.*require|has dim.*expected"
    )
})

test_that("ZarrDaf set_matrix overwrite errors", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4), nrow = 2)
    set_matrix(d, "cell", "gene", "M", m)
    expect_error(set_matrix(d, "cell", "gene", "M", m + 10),
                 "existing matrix:")
})

test_that("ZarrDaf delete_matrix removes matrix", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4), nrow = 2)
    set_matrix(d, "cell", "gene", "M", m)
    delete_matrix(d, "cell", "gene", "M")
    expect_false(has_matrix(d, "cell", "gene", "M"))
})

test_that("ZarrDaf dense matrix persists through DirStore", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 3)
    set_matrix(d, "cell", "gene", "M", m)
    rm(d)
    d2 <- zarr_daf(tmp, mode = "r")
    out <- get_matrix(d2, "cell", "gene", "M")
    expect_equal(unname(out), m)
})

# ---- Matrices: sparse ---------------------------------------------------

test_that("ZarrDaf sparse numeric matrix round-trip", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1L, 2L, 3L), j = c(1L, 2L, 1L),
        x = c(10.0, 20.0, 30.0),
        dims = c(3L, 2L)
    )
    set_matrix(d, "cell", "gene", "S", sp)
    out <- get_matrix(d, "cell", "gene", "S")
    expect_s4_class(out, "dgCMatrix")
    expect_equal(as.matrix(unname(out)), as.matrix(sp))
})

test_that("ZarrDaf sparse Bool matrix mixed values", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1L, 2L), j = c(1L, 2L),
        x = c(TRUE, FALSE),
        dims = c(2L, 2L)
    )
    set_matrix(d, "cell", "gene", "F", sp)
    out <- get_matrix(d, "cell", "gene", "F")
    expect_s4_class(out, "lgCMatrix")
    expect_identical(as.matrix(unname(out)), as.matrix(sp))
})

test_that("ZarrDaf sparse Bool all-TRUE skips nzval", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1L, 2L), j = c(1L, 2L),
        x = c(TRUE, TRUE),
        dims = c(2L, 2L)
    )
    set_matrix(d, "cell", "gene", "F", sp)
    store <- S7::prop(d, "store")
    expect_false(store_exists(store, "matrices/cell/gene/F/nzval/zarr.json"))
    expect_true(store_exists(store, "matrices/cell/gene/F/colptr/zarr.json"))
    expect_true(store_exists(store, "matrices/cell/gene/F/rowval/zarr.json"))
    out <- get_matrix(d, "cell", "gene", "F")
    expect_s4_class(out, "lgCMatrix")
    expect_identical(as.matrix(unname(out)), as.matrix(sp))
})

test_that("ZarrDaf sparse matrix on-disk indices are 1-based (upstream-compatible)", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1L, 3L), j = c(1L, 2L),
        x = c(10.0, 20.0),
        dims = c(3L, 2L)
    )
    set_matrix(d, "cell", "gene", "S", sp)
    store <- S7::prop(d, "store")
    # rowval on disk should be 1-based int64 (DAF 0.3.0 CSC index type).
    rowval_meta <- zarr_v3_read_array(store, "matrices/cell/gene/S/rowval")
    expect_equal(rowval_meta$data_type, "int64")
    rowval <- zarr_v3_decode_chunk(
        store_get_bytes(store, zarr_v3_chunk_path("matrices/cell/gene/S/rowval", 1L)),
        rowval_meta$data_type,
        n = as.integer(rowval_meta$shape[[1L]])
    )
    expect_identical(as.integer(rowval), c(1L, 3L))
    # colptr on disk should be 1-based: starts at 1.
    colptr_meta <- zarr_v3_read_array(store, "matrices/cell/gene/S/colptr")
    expect_equal(colptr_meta$data_type, "int64")
    colptr <- zarr_v3_decode_chunk(
        store_get_bytes(store, zarr_v3_chunk_path("matrices/cell/gene/S/colptr", 1L)),
        colptr_meta$data_type,
        n = as.integer(colptr_meta$shape[[1L]])
    )
    expect_equal(as.integer(colptr[[1L]]), 1L)
})

test_that("ZarrDaf sparse matrix persists through DirStore", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1L, 3L), j = c(1L, 2L),
        x = c(10.0, 20.0),
        dims = c(3L, 2L)
    )
    set_matrix(d, "cell", "gene", "S", sp)
    rm(d)
    d2 <- zarr_daf(tmp, mode = "r")
    out <- get_matrix(d2, "cell", "gene", "S")
    expect_equal(as.matrix(unname(out)), as.matrix(sp))
})

test_that("ZarrDaf relayout_matrix creates flipped layout", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
    set_matrix(d, "cell", "gene", "M", m, relayout = FALSE)
    relayout_matrix(d, "cell", "gene", "M")
    expect_true(has_matrix(d, "gene", "cell", "M"))
    out <- get_matrix(d, "gene", "cell", "M")
    expect_equal(unname(out), t(m))
})

test_that("ZarrDafReadOnly rejects all matrix mutations", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4), nrow = 2)
    set_matrix(d, "cell", "gene", "M", m, relayout = FALSE)
    rm(d)
    d_ro <- zarr_daf(tmp, mode = "r")
    expect_error(set_matrix(d_ro, "cell", "gene", "M", m + 10,
                            overwrite = TRUE),
                 "read-only")
    expect_error(delete_matrix(d_ro, "cell", "gene", "M"),
                 "read-only")
    expect_error(relayout_matrix(d_ro, "cell", "gene", "M"),
                 "read-only")
})
