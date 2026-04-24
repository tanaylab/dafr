test_that("Log eps: 1 on dgCMatrix preserves sparse class", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- methods::as(matrix(c(0, 1, 0, 2, 0, 0, 0, 3, 4), nrow = 3), "dgCMatrix")
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 1")
    expect_s4_class(result, "dgCMatrix")
    expect_equal(result@x, log1p(m@x))
    expect_identical(result@i, m@i)
    expect_identical(result@p, m@p)
})

test_that("Log eps: 0 on dgCMatrix densifies (no fast path)", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", "A")
    add_axis(d, "gene", "X")
    m <- methods::as(matrix(2.0, 1, 1), "dgCMatrix")
    set_matrix(d, "cell", "gene", "UMIs", m)
    # eps == 0 (default of the parser if user writes no eps): log(0) is -Inf,
    # which densifies. We do NOT short-circuit here.
    result <- get_query(d, "@ cell @ gene :: UMIs % Log")
    expect_false(methods::is(result, "sparseMatrix")) # densified to dense
    expect_equal(as.numeric(result[1, 1]), log(2))
})
