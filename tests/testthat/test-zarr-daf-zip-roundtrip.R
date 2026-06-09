# Slice 17 phase 11: zarr_daf round-trip via the MmapZipStore backend.
#
# `open_daf("foo.daf.zarr.zip")` → `zarr_daf` → `new_mmap_zip_store(..)`
# → wrapped in ZarrDaf / ZarrDafReadOnly. Tests cover the same matrix
# of operations as test-zarr-format.R but against the zip backend.

new_zip_path <- function() {
    f <- new_tempfile("zip")
    # Ensure the *.daf.zarr.zip suffix dispatches through zarr_daf.
    sub <- paste0(f, ".daf.zarr.zip")
    sub
}

test_that("open_daf of a fresh .daf.zarr.zip path creates a writable ZarrDaf", {
    skip_if_no_mmap_zip()
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    expect_s7_class(d, ZarrDaf)
    expect_s7_class(S7::prop(d, "store"), MmapZipStore)
    expect_true(file.exists(path))
    # Upstream `daf` marker array present.
    expect_true(store_exists(S7::prop(d, "store"), "daf/.zarray"))
    expect_false(store_exists(S7::prop(d, "store"), "daf.json"))
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))
})

test_that("zarr_daf .daf.zarr.zip mode='r' on missing store errors", {
    skip_if_no_mmap_zip()
    path <- new_zip_path()
    expect_error(open_daf(path, mode = "r"), "store does not exist")
})

test_that("set_axis + get_axis round-trips through .daf.zarr.zip", {
    skip_if_no_mmap_zip()
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    expect_setequal(axes_set(d), "cell")
    expect_identical(axis_entries(d, "cell"), c("c1", "c2", "c3"))
    expect_equal(axis_length(d, "cell"), 3L)
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))
})

test_that("set_scalar of int/double/character round-trips", {
    skip_if_no_mmap_zip()
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    set_scalar(d, "n_int", 42L)
    set_scalar(d, "n_dbl", 3.14)
    set_scalar(d, "name", "hello")
    expect_identical(get_scalar(d, "n_int"), 42L)
    expect_equal(get_scalar(d, "n_dbl"), 3.14)
    expect_identical(get_scalar(d, "name"), "hello")
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))
})

test_that("set_vector + get_vector round-trips", {
    skip_if_no_mmap_zip()
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(1.5, 2.5, 3.5))
    set_vector(d, "cell", "label", c("alpha", "beta", "gamma"))
    got_x <- get_vector(d, "cell", "x")
    expect_equal(unname(got_x), c(1.5, 2.5, 3.5))
    expect_identical(unname(get_vector(d, "cell", "label")),
                     c("alpha", "beta", "gamma"))
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))
})

test_that("set_matrix dense round-trips", {
    skip_if_no_mmap_zip()
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3,
                dimnames = list(c("c1", "c2"), c("g1", "g2", "g3")))
    set_matrix(d, "cell", "gene", "expr", m)
    got <- get_matrix(d, "cell", "gene", "expr")
    expect_equal(as.matrix(got), m)
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))
})

test_that("set_matrix sparse (dgCMatrix) round-trips", {
    skip_if_no_mmap_zip()
    skip_if_not_installed("Matrix")
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    m <- Matrix::sparseMatrix(
        i = c(1L, 3L),
        j = c(1L, 2L),
        x = c(7.0, 11.0),
        dims = c(3L, 2L),
        dimnames = list(c("c1", "c2", "c3"), c("g1", "g2"))
    )
    set_matrix(d, "cell", "gene", "sp", m)
    got <- get_matrix(d, "cell", "gene", "sp")
    expect_equal(as.matrix(got), as.matrix(m))
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))
})

test_that("close the writer, reopen in 'r' mode, all data is intact", {
    skip_if_no_mmap_zip()
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    add_axis(d, "cell", c("c1", "c2"))
    set_scalar(d, "n", 42L)
    set_vector(d, "cell", "x", c(10, 20))
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))

    d2 <- open_daf(path, mode = "r")
    expect_s7_class(d2, ZarrDafReadOnly)
    expect_setequal(axes_set(d2), "cell")
    expect_identical(axis_entries(d2, "cell"), c("c1", "c2"))
    expect_identical(get_scalar(d2, "n"), 42L)
    expect_equal(unname(get_vector(d2, "cell", "x")), c(10, 20))
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d2, "store"), "xptr"))
})

test_that("re-open in 'r' mode after partial write doesn't double-init the daf marker", {
    skip_if_no_mmap_zip()
    # First write: create + populate.
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    add_axis(d, "cell", c("c1", "c2"))
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))

    # Reopen 'r' should NOT re-init the `daf` marker array (which is
    # append-only in the zip store and would error). Just reads the existing one.
    d2 <- open_daf(path, mode = "r")
    expect_s7_class(d2, ZarrDafReadOnly)
    expect_identical(axis_entries(d2, "cell"), c("c1", "c2"))
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d2, "store"), "xptr"))
})

test_that("zarr_daf 'w+' on an existing zip truncates+recreates", {
    skip_if_no_mmap_zip()
    path <- new_zip_path()
    d <- open_daf(path, mode = "w")
    add_axis(d, "cell", c("c1", "c2"))
    set_scalar(d, "old", 1L)
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d, "store"), "xptr"))
    expect_true(file.exists(path))

    # 'w+' on existing zip should wipe the old contents (mirrors the
    # directory-store branch which does `unlink(uri, recursive=TRUE)`
    # under 'w+').
    d2 <- open_daf(path, mode = "w+")
    expect_s7_class(d2, ZarrDaf)
    expect_false(has_scalar(d2, "old"))
    expect_setequal(axes_set(d2), character(0L))
    dafr:::dafr_mmap_zip_close(S7::prop(S7::prop(d2, "store"), "xptr"))
})
