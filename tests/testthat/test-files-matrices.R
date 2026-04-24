test_that("FilesDaf matrices_set lists descriptor-backed matrices", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
    writeLines(c("X", "Y"), file.path(dir, "axes", "gene.txt"))
    writeLines(
        '{"format":"dense","eltype":"Float64"}',
        file.path(dir, "matrices", "cell", "gene", "m.json")
    )
    writeBin(c(1.0, 2.0, 3.0, 4.0),
        file.path(dir, "matrices", "cell", "gene", "m.data"),
        size = 8L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    expect_equal(matrices_set(d, "cell", "gene"), "m")
    expect_true(has_matrix(d, "cell", "gene", "m"))
})

test_that("has_matrix returns FALSE on missing axis", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    d <- files_daf(dir, mode = "r")
    expect_false(has_matrix(d, "cell", "gene", "m"))
    expect_equal(matrices_set(d, "cell", "gene"), character(0L))
})

test_that("format_get_matrix dense Float64 round-trips with correct shape", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
    writeLines(c("X", "Y"), file.path(dir, "axes", "gene.txt"))
    writeLines(
        '{"format":"dense","eltype":"Float64"}',
        file.path(dir, "matrices", "cell", "gene", "m.json")
    )
    writeBin(c(1, 2, 3, 4, 5, 6),
        file.path(dir, "matrices", "cell", "gene", "m.data"),
        size = 8L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    m <- format_get_matrix(d, "cell", "gene", "m")
    expect_equal(dim(m), c(3L, 2L))
    expect_equal(m[2, 2], 5)
    expect_equal(m[, 1], c(1, 2, 3))
})

test_that("format_get_matrix dense Int32", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
    writeLines(c("X", "Y"), file.path(dir, "axes", "gene.txt"))
    writeLines(
        '{"format":"dense","eltype":"Int32"}',
        file.path(dir, "matrices", "cell", "gene", "mi.json")
    )
    writeBin(c(1L, 2L, 3L, 4L),
        file.path(dir, "matrices", "cell", "gene", "mi.data"),
        size = 4L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    m <- format_get_matrix(d, "cell", "gene", "mi")
    expect_equal(dim(m), c(2L, 2L))
    expect_true(is.integer(m))
    expect_equal(unname(m), matrix(1:4, nrow = 2))
})

test_that("format_get_matrix dense String round-trip (column-major)", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
    writeLines(c("X", "Y"), file.path(dir, "axes", "gene.txt"))
    writeLines(
        '{"format":"dense","eltype":"String"}',
        file.path(dir, "matrices", "cell", "gene", "ms.json")
    )
    # Column-major: column 1 (X) then column 2 (Y)
    writeLines(
        c("aX", "bX", "aY", "bY"),
        file.path(dir, "matrices", "cell", "gene", "ms.txt")
    )
    d <- files_daf(dir, mode = "r")
    m <- format_get_matrix(d, "cell", "gene", "ms")
    expect_equal(dim(m), c(2L, 2L))
    expect_equal(m[1, 1], "aX")
    expect_equal(m[2, 2], "bY")
})

test_that("set_matrix + get_matrix dense Float64 round-trip", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    set_matrix(d, "cell", "gene", "m", m)
    d2 <- files_daf(dir, mode = "r")
    m2 <- get_matrix(d2, "cell", "gene", "m")
    expect_equal(unname(m2), m)
    expect_equal(dimnames(m2), list(c("A", "B", "C"), c("X", "Y")))
})

test_that("set_matrix dense Int32", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(1:4, nrow = 2)
    set_matrix(d, "cell", "gene", "mi", m)
    d2 <- files_daf(dir, mode = "r")
    expect_equal(unname(get_matrix(d2, "cell", "gene", "mi")), m)
})

test_that("set_matrix dense String", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c("a", "b", "c", "d"), nrow = 2)
    set_matrix(d, "cell", "gene", "ms", m)
    d2 <- files_daf(dir, mode = "r")
    expect_equal(unname(get_matrix(d2, "cell", "gene", "ms")), m)
})

test_that("delete_matrix removes all payload + descriptor", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "m", matrix(1:4, 2))
    delete_matrix(d, "cell", "gene", "m")
    expect_false(file.exists(file.path(dir, "matrices", "cell", "gene", "m.json")))
    expect_false(file.exists(file.path(dir, "matrices", "cell", "gene", "m.data")))
})

test_that("delete_matrix must_exist semantics", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", "A")
    add_axis(d, "gene", "X")
    expect_silent(delete_matrix(d, "cell", "gene", "nope", must_exist = FALSE))
    expect_error(delete_matrix(d, "cell", "gene", "nope"), "does not exist")
})

test_that("format_get_matrix densifies sparse CSC written Julia-style", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
    writeLines(c("X", "Y"), file.path(dir, "axes", "gene.txt"))
    writeLines(
        '{"format":"sparse","eltype":"Float64","indtype":"UInt32"}',
        file.path(dir, "matrices", "cell", "gene", "sm.json")
    )
    # colptr (1-based): col1 starts at 1, col2 starts at 3, end at 4 (nnz=3)
    writeBin(c(1L, 3L, 4L),
        file.path(dir, "matrices", "cell", "gene", "sm.colptr"),
        size = 4L, endian = "little"
    )
    # rowval (1-based): col1 has rows 1 and 3; col2 has row 2
    writeBin(c(1L, 3L, 2L),
        file.path(dir, "matrices", "cell", "gene", "sm.rowval"),
        size = 4L, endian = "little"
    )
    writeBin(c(10.0, 20.0, 30.0),
        file.path(dir, "matrices", "cell", "gene", "sm.nzval"),
        size = 8L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    m <- format_get_matrix(d, "cell", "gene", "sm")
    expect_s4_class(m, "dgCMatrix")
    expect_equal(dim(m), c(3L, 2L))
    expect_equal(as.matrix(m), matrix(c(10, 0, 20, 0, 30, 0), nrow = 3))
})

test_that("format_get_matrix sparse Bool without nzval synthesizes TRUE", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
    writeLines(c("X", "Y"), file.path(dir, "axes", "gene.txt"))
    writeLines(
        '{"format":"sparse","eltype":"Bool","indtype":"UInt32"}',
        file.path(dir, "matrices", "cell", "gene", "sb.json")
    )
    writeBin(c(1L, 2L, 3L),
        file.path(dir, "matrices", "cell", "gene", "sb.colptr"),
        size = 4L, endian = "little"
    )
    writeBin(c(1L, 2L),
        file.path(dir, "matrices", "cell", "gene", "sb.rowval"),
        size = 4L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    m <- format_get_matrix(d, "cell", "gene", "sb")
    expect_s4_class(m, "lgCMatrix")
    expect_equal(dim(m), c(2L, 2L))
    expect_equal(as.matrix(m), matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2))
})

test_that("set_matrix + get_matrix sparse dgCMatrix round-trip", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30),
        dims = c(3, 2)
    )
    set_matrix(d, "cell", "gene", "sm", sp)
    cp <- readBin(file.path(dir, "matrices", "cell", "gene", "sm.colptr"),
        what = "integer", n = 3L, size = 4L, endian = "little"
    )
    expect_equal(cp, c(1L, 3L, 4L))
    rv <- readBin(file.path(dir, "matrices", "cell", "gene", "sm.rowval"),
        what = "integer", n = 3L, size = 4L, endian = "little"
    )
    expect_equal(rv, c(1L, 3L, 2L))
    d2 <- files_daf(dir, mode = "r")
    m <- get_matrix(d2, "cell", "gene", "sm")
    expect_s4_class(m, "dgCMatrix")
    expect_equal(as.matrix(unname(m)), as.matrix(sp))
})

test_that("set_matrix sparse lgCMatrix all-TRUE omits .nzval", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1, 3), j = c(1, 2), x = c(TRUE, TRUE),
        dims = c(3, 2)
    )
    set_matrix(d, "cell", "gene", "sb", sp)
    expect_true(file.exists(file.path(dir, "matrices", "cell", "gene", "sb.colptr")))
    expect_true(file.exists(file.path(dir, "matrices", "cell", "gene", "sb.rowval")))
    expect_false(file.exists(file.path(dir, "matrices", "cell", "gene", "sb.nzval")))
    d2 <- files_daf(dir, mode = "r")
    m <- get_matrix(d2, "cell", "gene", "sb")
    expect_s4_class(m, "lgCMatrix")
    expect_equal(as.matrix(unname(m)), as.matrix(sp))
})

test_that("relayout_matrix stores transpose at flipped axis pair", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    set_matrix(d, "cell", "gene", "m", m)
    relayout_matrix(d, "cell", "gene", "m")
    expect_true(file.exists(file.path(dir, "matrices", "gene", "cell", "m.json")))
    d2 <- files_daf(dir, mode = "r")
    m_flipped <- get_matrix(d2, "gene", "cell", "m")
    expect_equal(unname(m_flipped), t(m))
})

test_that("relayout_matrix on sparse matrix stores transposed CSC", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30),
        dims = c(3, 2)
    )
    set_matrix(d, "cell", "gene", "sm", sp)
    relayout_matrix(d, "cell", "gene", "sm")
    d2 <- files_daf(dir, mode = "r")
    m_flipped <- get_matrix(d2, "gene", "cell", "sm")
    expect_s4_class(m_flipped, "dgCMatrix")
    expect_equal(as.matrix(unname(m_flipped)), t(as.matrix(sp)))
})
