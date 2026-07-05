test_that("zip_daf round-trips scalars, axes, vectors, matrices", {
    skip_if_no_mmap_zip()
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    set_scalar(d, "pi", 3.14)
    set_scalar(d, "cells", bit64::as.integer64(100))
    set_scalar(d, "note", "hello")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "donor", c(1L, 2L, 3L, 4L))
    set_vector(d, "cell", "sx", c(0, 10, 0, 30)) # sparsifies
    set_vector(d, "cell", "label", c("p", "q", "r", "s")) # dense string
    set_matrix(d, "cell", "gene", "dm", matrix(1:8, nrow = 4))
    sp <- Matrix::sparseMatrix(
        i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30), dims = c(4, 2)
    )
    set_matrix(d, "cell", "gene", "sm", sp)
    rm(d)
    gc()

    d2 <- zip_daf(p, mode = "r")
    expect_equal(get_scalar(d2, "pi"), 3.14)
    expect_equal(get_scalar(d2, "cells"), bit64::as.integer64(100))
    expect_equal(get_scalar(d2, "note"), "hello")
    expect_setequal(scalars_set(d2), c("pi", "cells", "note"))
    expect_equal(axis_vector(d2, "cell"), c("A", "B", "C", "D"))
    expect_setequal(axes_set(d2), c("cell", "gene"))
    expect_equal(unname(get_vector(d2, "cell", "donor")), c(1L, 2L, 3L, 4L))
    expect_equal(unname(get_vector(d2, "cell", "sx")), c(0, 10, 0, 30))
    expect_equal(unname(get_vector(d2, "cell", "label")), c("p", "q", "r", "s"))
    expect_setequal(vectors_set(d2, "cell"), c("donor", "sx", "label"))
    expect_equal(
        unname(get_matrix(d2, "cell", "gene", "dm")),
        matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4)
    )
    sm2 <- get_matrix(d2, "cell", "gene", "sm")
    expect_s4_class(sm2, "dgCMatrix")
    expect_equal(as.matrix(unname(sm2)), as.matrix(sp))
})

test_that("zip_daf round-trips a Bool sparse vector and matrix (nzval absent)", {
    skip_if_no_mmap_zip()
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "flags", c(TRUE, FALSE, TRUE, FALSE))
    mk <- methods::new("lgCMatrix",
        i = c(0L, 1L), p = c(0L, 1L, 2L),
        Dim = c(4L, 2L), Dimnames = list(NULL, NULL), x = c(TRUE, TRUE)
    )
    set_matrix(d, "cell", "gene", "mask", mk)
    rm(d)
    gc()
    d2 <- zip_daf(p, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "flags")), c(TRUE, FALSE, TRUE, FALSE))
    mk2 <- get_matrix(d2, "cell", "gene", "mask")
    expect_s4_class(mk2, "lgCMatrix")
    expect_equal(as.matrix(unname(mk2)), as.matrix(mk))
})

test_that("zip_daf is append-only: overwrite and delete raise", {
    skip_if_no_mmap_zip()
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    set_scalar(d, "k", 1L)
    set_vector(d, "cell", "v", c(1, 2))
    expect_error(set_scalar(d, "k", 2L))
    expect_error(set_scalar(d, "k", 2L, overwrite = TRUE), "append-only")
    expect_error(set_vector(d, "cell", "v", c(3, 4), overwrite = TRUE), "append-only")
    expect_error(delete_scalar(d, "k"), "append-only")
})

test_that("open_daf dispatches .daf.zip to zip_daf", {
    skip_if_no_mmap_zip()
    p <- tempfile(fileext = ".daf.zip")
    d <- open_daf(p, mode = "w")
    expect_true(inherits(d, "dafr::ZipDaf"))
    add_axis(d, "cell", c("A", "B"))
    rm(d)
    gc()
    d2 <- open_daf(p, mode = "r")
    expect_true(inherits(d2, "dafr::ZipDafReadOnly"))
    expect_equal(axis_vector(d2, "cell"), c("A", "B"))
    expect_error(
        open_daf(paste0(p, "#/group"), mode = "r"),
        "not supported"
    )
})

test_that("zip_daf 'r' on missing archive errors; 'w' on existing daf errors", {
    skip_if_no_mmap_zip()
    p <- tempfile(fileext = ".daf.zip")
    expect_error(zip_daf(p, mode = "r"))
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", "A")
    rm(d)
    gc()
    expect_error(zip_daf(p, mode = "w"), "w\\+")
})

test_that("read-only zip_daf rejects mutation", {
    skip_if_no_mmap_zip()
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    rm(d)
    gc()
    ro <- zip_daf(p, mode = "r")
    expect_error(set_scalar(ro, "x", 1), "read-only")
    expect_error(add_axis(ro, "gene", "G"), "read-only")
})
