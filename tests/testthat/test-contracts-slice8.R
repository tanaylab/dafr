test_that(".matrix_type_ok accepts character dense matrix", {
    m <- matrix(c("a","b","c","d"), 2L, 2L)
    expect_true(dafr:::.matrix_type_ok(m, "character"))
})

test_that(".matrix_type_ok accepts integer-valued sparse dgCMatrix", {
    m <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 3.0, dims = c(3L, 3L))
    expect_true(dafr:::.matrix_type_ok(m, "integer"))
})

test_that(".matrix_type_ok rejects non-integer-valued sparse for integer", {
    m <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 3.5, dims = c(3L, 3L))
    expect_false(dafr:::.matrix_type_ok(m, "integer"))
})

test_that(".matrix_type_ok accepts logical-valued sparse for logical", {
    m <- Matrix::sparseMatrix(i = c(1L, 2L), j = c(1L, 2L), x = c(1, 0),
        dims = c(3L, 3L))
    expect_true(dafr:::.matrix_type_ok(m, "logical"))
})

test_that(".matrix_type_ok still accepts native integer dense matrix", {
    m <- matrix(1:4, 2L, 2L)
    expect_true(dafr:::.matrix_type_ok(m, "integer"))
})

test_that(".matrix_type_ok rejects dense character for integer", {
    m <- matrix(c("a","b"), 2L, 1L)
    expect_false(dafr:::.matrix_type_ok(m, "integer"))
})
