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

test_that("zarr_daf() with .daf.zarr.zip errors with slice-17 placeholder", {
    expect_error(zarr_daf("/some/path.daf.zarr.zip"),
        "lands in slice 17"
    )
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
    expect_error(set_scalar(d, "x", 2L), "already exists")
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
    expect_error(add_axis(d, "x", c("A", "A")), "duplicate")
    expect_error(add_axis(d, "x", c("A", NA_character_)), "NA")
    expect_error(add_axis(d, "x", c("A", "")), "empty")
})

test_that("ZarrDaf add_axis rejects re-adding existing axis", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A"))
    expect_error(add_axis(d, "cell", c("B")), "already exists")
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
    expect_error(set_vector(d, "cell", "x", c(1, 2)),
                 "length 2.*length 3")
})

test_that("ZarrDaf set_vector overwrite=FALSE errors on existing", {
    d <- .fresh_zarr_daf()
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1, 2))
    expect_error(set_vector(d, "cell", "x", c(3, 4)),
                 "already exists")
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
    # Stored sparse but read densified — matches FilesDaf contract
    # (api convention is named atomic vector, not S4 sparseVector).
    store <- S7::prop(d, "store")
    expect_true(store_exists(store, "vectors/cell/x/.zgroup"))
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
    # Verify nzval/.zarray does NOT exist in the store.
    store <- S7::prop(d, "store")
    expect_false(store_exists(store, "vectors/cell/flag/nzval/.zarray"))
    expect_true(store_exists(store, "vectors/cell/flag/nzind/.zarray"))
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
    # On disk the layout flipped to sparse subgroup.
    store <- S7::prop(d, "store")
    expect_true(store_exists(store, "vectors/cell/x/.zgroup"))
    expect_false(store_exists(store, "vectors/cell/x/.zarray"))
    out <- get_vector(d, "cell", "x")
    expect_equal(unname(out), c(0, 10, 0))
})
