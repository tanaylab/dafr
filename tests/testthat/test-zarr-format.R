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
