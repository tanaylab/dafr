test_that("can read Julia-written scalars", {
    expect_true(dir.exists(fixture_path()))
    d <- files_daf(fixture_path(), mode = "r")
    expect_equal(get_scalar(d, "pi"), 3.14)
    expect_equal(get_scalar(d, "cells"), bit64::as.integer64(100))
    expect_equal(get_scalar(d, "note"), "hello")
})

test_that("can read Julia-written axes + dense + sparse vectors", {
    d <- files_daf(fixture_path(), mode = "r")
    expect_equal(axis_vector(d, "cell"), c("A", "B", "C", "D"))
    expect_equal(axis_vector(d, "gene"), c("X", "Y"))
    expect_equal(unname(get_vector(d, "cell", "donor")), c(1L, 2L, 3L, 4L))
    expect_equal(unname(get_vector(d, "cell", "sparse_x")), c(0, 10, 0, 30))
    expect_equal(
        unname(get_vector(d, "cell", "flags")),
        c(TRUE, FALSE, TRUE, FALSE)
    )
})

test_that("can read Julia-written dense + sparse + Bool matrices", {
    d <- files_daf(fixture_path(), mode = "r")
    dm <- get_matrix(d, "cell", "gene", "dense_m")
    expect_equal(dim(dm), c(4L, 2L))
    expect_equal(unname(dm), matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4))
    sm <- get_matrix(d, "cell", "gene", "sparse_m")
    expect_equal(dim(sm), c(4L, 2L))
    expect_s4_class(sm, "dgCMatrix")
    expect_equal(
        as.matrix(unname(sm)),
        matrix(c(10, 0, 20, 0, 0, 30, 0, 0), nrow = 4)
    )
    mk <- get_matrix(d, "cell", "gene", "mask")
    expect_s4_class(mk, "lgCMatrix")
    expect_equal(
        as.matrix(unname(mk)),
        matrix(c(
            TRUE, FALSE, FALSE, FALSE,
            FALSE, TRUE, FALSE, FALSE
        ), nrow = 4)
    )
})

.copy_all_memory_to_files <- function(src, dir) {
    dst <- files_daf(dir, mode = "w+")
    for (nm in scalars_set(src)) set_scalar(dst, nm, get_scalar(src, nm))
    for (ax in axes_set(src)) add_axis(dst, ax, axis_vector(src, ax))
    for (ax in axes_set(src)) {
        for (nm in vectors_set(src, ax)) {
            set_vector(dst, ax, nm, unname(get_vector(src, ax, nm)))
        }
    }
    for (ra in axes_set(src)) {
        for (ca in axes_set(src)) {
            for (nm in matrices_set(src, ra, ca)) {
                set_matrix(dst, ra, ca, nm, unname(get_matrix(src, ra, ca, nm)))
            }
        }
    }
    dst
}

test_that("MemoryDaf -> FilesDaf -> MemoryDaf round-trip preserves data", {
    m <- memory_daf(name = "src")
    set_scalar(m, "k", 5L)
    add_axis(m, "cell", c("A", "B"))
    add_axis(m, "gene", c("X", "Y", "Z"))
    set_vector(m, "cell", "v", c(10.0, 20.0))
    set_matrix(m, "cell", "gene", "m", matrix(1:6, 2, 3))
    dir <- new_tempdir()
    f <- .copy_all_memory_to_files(m, dir)
    f2 <- files_daf(dir, mode = "r")
    expect_equal(get_scalar(f2, "k"), 5L)
    expect_equal(axis_vector(f2, "cell"), c("A", "B"))
    expect_equal(unname(get_vector(f2, "cell", "v")), c(10.0, 20.0))
    expect_equal(
        unname(get_matrix(f2, "cell", "gene", "m")),
        matrix(1:6, 2, 3)
    )
})

test_that("sparse dgCMatrix round-trip through FilesDaf", {
    m <- memory_daf()
    add_axis(m, "cell", c("A", "B", "C"))
    add_axis(m, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30),
        dims = c(3, 2)
    )
    set_matrix(m, "cell", "gene", "sm", sp)
    dir <- new_tempdir()
    f <- .copy_all_memory_to_files(m, dir)
    f2 <- files_daf(dir, mode = "r")
    sm2 <- get_matrix(f2, "cell", "gene", "sm")
    expect_s4_class(sm2, "dgCMatrix")
    expect_equal(as.matrix(unname(sm2)), as.matrix(sp))
})

test_that("R-written store is readable by Julia with identical values", {
    skip_if_not(.have_julia_env())
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    set_scalar(d, "pi", 3.14)
    set_scalar(d, "cells", bit64::as.integer64(100))
    set_scalar(d, "note", "hello")
    set_vector(d, "cell", "donor", c(1L, 2L, 3L, 4L))
    set_vector(d, "cell", "sx", c(0, 10, 0, 30))
    set_matrix(d, "cell", "gene", "dm", matrix(1:8, nrow = 4))
    sp <- Matrix::sparseMatrix(
        i = c(1, 3, 2), j = c(1, 1, 2),
        x = c(10, 20, 30), dims = c(4, 2)
    )
    set_matrix(d, "cell", "gene", "sm", sp)

    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = FilesDaf(raw"%s", "r")', dir),
        '@assert get_scalar(daf, "pi")    == 3.14',
        '@assert get_scalar(daf, "cells") == Int64(100)',
        '@assert get_scalar(daf, "note")  == "hello"',
        '@assert axis_vector(daf, "cell")  == ["A","B","C","D"]',
        '@assert get_vector(daf, "cell", "donor") == Int32[1,2,3,4]',
        '@assert get_vector(daf, "cell", "sx") == Float64[0,10,0,30]',
        '@assert size(get_matrix(daf, "cell", "gene", "dm")) == (4,2)',
        '@assert get_matrix(daf, "cell", "gene", "dm") == Float64[1 5; 2 6; 3 7; 4 8]',
        'sm = get_matrix(daf, "cell", "gene", "sm")',
        "@assert size(sm) == (4,2)",
        "@assert nnz(sm) == 3",
        'println("JULIA_OK")'
    )
    out <- run_julia(script)
    expect_true(any(grepl("JULIA_OK", out)),
        info = paste(out, collapse = "\n")
    )
})

test_that("R -> Julia copy -> R preserves dense + sparse bytes", {
    skip_if_not(.have_julia_env())
    dir_src <- new_tempdir()
    dir_dst <- new_tempdir()
    d <- files_daf(dir_src, mode = "w+")
    add_axis(d, "cell", sprintf("c%d", 1:50))
    add_axis(d, "gene", sprintf("g%d", 1:10))
    v <- numeric(50)
    v[c(5, 25, 45)] <- c(1, 2, 3)
    set_vector(d, "cell", "x", v)
    set_matrix(d, "cell", "gene", "dm", matrix(seq_len(500), nrow = 50))
    sp <- Matrix::rsparsematrix(50, 10, density = 0.05)
    set_matrix(d, "cell", "gene", "sm", sp)

    script <- c(
        "using DataAxesFormats",
        sprintf('src = FilesDaf(raw"%s", "r")', dir_src),
        sprintf('dst = FilesDaf(raw"%s", "w")', dir_dst),
        "for axis in axes_set(src); add_axis!(dst, axis, axis_vector(src, axis)); end",
        "for name in scalars_set(src); set_scalar!(dst, name, get_scalar(src, name)); end",
        "for axis in axes_set(src)",
        "  for name in vectors_set(src, axis)",
        "    set_vector!(dst, axis, name, get_vector(src, axis, name))",
        "  end",
        "end",
        "for rows in axes_set(src), cols in axes_set(src)",
        "  for name in matrices_set(src, rows, cols; relayout = false)",
        "    set_matrix!(dst, rows, cols, name, get_matrix(src, rows, cols, name))",
        "  end",
        "end",
        'println("COPY_OK")'
    )
    out <- run_julia(script)
    expect_true(any(grepl("COPY_OK", out)),
        info = paste(out, collapse = "\n")
    )

    d2 <- files_daf(dir_dst, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "x")), v)
    expect_equal(
        unname(get_matrix(d2, "cell", "gene", "dm")),
        matrix(seq_len(500), nrow = 50)
    )
    rt_sp <- get_matrix(d2, "cell", "gene", "sm")
    expect_s4_class(rt_sp, "dgCMatrix")
    expect_equal(as.matrix(unname(rt_sp)), as.matrix(sp))
})
