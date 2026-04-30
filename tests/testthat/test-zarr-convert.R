# Tests for files_to_zarr / zarr_to_files conversion (Slice 16, Phase 9).

# Build a small files_daf with scalars, axes, dense+string+sparse vectors,
# and dense+sparse matrices. Returns the on-disk path.
.fixture_files_daf <- function() {
    tmp <- tempfile(fileext = ".daf")
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C"))
    add_axis(d_mem, "gene", c("X", "Y"))
    set_scalar(d_mem, "n", 42L)
    set_scalar(d_mem, "label", "demo")
    set_vector(d_mem, "cell", "x", c(1.0, 2.0, 3.0))
    set_vector(d_mem, "gene", "y", c(10L, 20L))
    set_vector(d_mem, "cell", "label", c("alpha", "beta", "gamma"))
    set_matrix(
        d_mem, "cell", "gene", "M",
        matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 3)
    )
    sp <- Matrix::sparseMatrix(
        i = c(1L, 3L), j = c(1L, 2L),
        x = c(100.0, 200.0), dims = c(3L, 2L)
    )
    set_matrix(d_mem, "cell", "gene", "S", sp)
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    tmp
}

test_that("files_to_zarr converts a complete files_daf to zarr_daf", {
    src <- .fixture_files_daf()
    dst <- tempfile(fileext = ".daf.zarr")
    files_to_zarr(src, dst)
    expect_true(dir.exists(dst))
    expect_true(file.exists(file.path(dst, "daf.json")))
    expect_true(file.exists(file.path(dst, ".zmetadata")))

    d <- zarr_daf(dst, mode = "r")
    expect_setequal(scalars_set(d), c("n", "label"))
    expect_identical(get_scalar(d, "n"), 42L)
    expect_identical(get_scalar(d, "label"), "demo")
    expect_setequal(axes_set(d), c("cell", "gene"))
    expect_identical(axis_vector(d, "cell"), c("A", "B", "C"))
    expect_equal(unname(get_vector(d, "cell", "x")), c(1.0, 2.0, 3.0))
    expect_identical(unname(get_vector(d, "gene", "y")), c(10L, 20L))
    expect_identical(
        unname(get_vector(d, "cell", "label")),
        c("alpha", "beta", "gamma")
    )
    m <- get_matrix(d, "cell", "gene", "M")
    expect_equal(
        unname(m),
        matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 3)
    )
    s <- get_matrix(d, "cell", "gene", "S")
    expect_s4_class(s, "dgCMatrix")
})

test_that("zarr_to_files converts a complete zarr_daf to files_daf", {
    src <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(src, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    set_scalar(d, "n", 7L)
    set_vector(d, "cell", "x", c(10.0, 20.0))
    set_matrix(
        d, "cell", "cell", "I",
        matrix(c(1.0, 0, 0, 1.0), nrow = 2)
    )
    rm(d)
    dst <- tempfile(fileext = ".daf")
    zarr_to_files(src, dst)
    expect_true(file.exists(file.path(dst, "daf.json")))
    d2 <- files_daf(dst, mode = "r")
    expect_identical(get_scalar(d2, "n"), 7L)
    expect_identical(axis_vector(d2, "cell"), c("A", "B"))
    expect_equal(unname(get_vector(d2, "cell", "x")), c(10.0, 20.0))
})

test_that("files_to_zarr -> zarr_to_files round-trip preserves data", {
    src <- .fixture_files_daf()
    zarr_dst <- tempfile(fileext = ".daf.zarr")
    files_dst <- tempfile(fileext = ".daf")
    files_to_zarr(src, zarr_dst)
    zarr_to_files(zarr_dst, files_dst)
    d <- files_daf(files_dst, mode = "r")
    expect_identical(get_scalar(d, "n"), 42L)
    expect_identical(axis_vector(d, "cell"), c("A", "B", "C"))
    expect_equal(unname(get_vector(d, "cell", "x")), c(1.0, 2.0, 3.0))
    s <- get_matrix(d, "cell", "gene", "S")
    expect_s4_class(s, "dgCMatrix")
})

test_that("files_to_zarr errors when destination exists", {
    src <- .fixture_files_daf()
    dst <- tempfile(fileext = ".daf.zarr")
    dir.create(dst)
    expect_error(files_to_zarr(src, dst), "already exists")
})

test_that("files_to_zarr errors when source missing", {
    dst <- tempfile(fileext = ".daf.zarr")
    expect_error(
        files_to_zarr(tempfile(), dst),
        "does not exist"
    )
})

test_that("files_to_zarr cleans up partial destination on error", {
    # Source dir without daf.json — files_daf("r") will refuse to open.
    src <- tempfile()
    dir.create(src)
    dst <- tempfile(fileext = ".daf.zarr")
    expect_error(files_to_zarr(src, dst))
    expect_false(dir.exists(dst))
})

test_that("conversion preserves all-TRUE Bool sparse optimization", {
    src <- tempfile(fileext = ".daf")
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C"))
    add_axis(d_mem, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1L, 2L, 3L), j = c(1L, 2L, 1L),
        x = c(TRUE, TRUE, TRUE),
        dims = c(3L, 2L)
    )
    set_matrix(d_mem, "cell", "gene", "F", sp)
    fd_w <- files_daf(src, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    rm(fd_w)

    dst <- tempfile(fileext = ".daf.zarr")
    files_to_zarr(src, dst)

    # Verify nzval was NOT written in the zarr destination — upstream
    # all-TRUE Bool compaction must survive the conversion.
    d <- zarr_daf(dst, mode = "r")
    store <- S7::prop(d, "store")
    expect_false(
        store_exists(store, "matrices/cell/gene/F/nzval/.zarray")
    )

    out <- get_matrix(d, "cell", "gene", "F")
    expect_s4_class(out, "lgCMatrix")
})
