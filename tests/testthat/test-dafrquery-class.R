test_that("DafrQuery constructs with ast and canonical", {
    ast <- list(list(op = "Axis", axis_name = "cell"))
    q <- DafrQuery(ast = ast, canonical = "@ cell")
    expect_identical(q@ast, ast)
    expect_identical(q@canonical, "@ cell")
})

test_that("DafrQuery validator rejects non-scalar canonical", {
    expect_error(DafrQuery(ast = list(), canonical = c("a", "b")))
})

test_that("DafrQuery format/as.character return canonical", {
    q <- DafrQuery(ast = list(list(op = "Axis", axis_name = "cell")), canonical = "@ cell")
    expect_identical(format(q), "@ cell")
    expect_identical(as.character(q), "@ cell")
})

test_that("DafrQuery print emits canonical and returns invisibly", {
    q <- DafrQuery(ast = list(list(op = "Axis", axis_name = "cell")), canonical = "@ cell")
    out <- capture.output(ret <- print(q))
    expect_true(any(grepl("@ cell", out)))
    expect_identical(ret, q)
})

test_that("DafrQuery length returns AST length", {
    q <- DafrQuery(ast = list(list(op = "Axis", axis_name = "cell")), canonical = "@ cell")
    expect_identical(length(q), 1L)
    q2 <- DafrQuery(
        ast = list(
            list(op = "Axis", axis_name = "cell"),
            list(op = "LookupVector", name = "donor")
        ),
        canonical = "@ cell : donor"
    )
    expect_identical(length(q2), 2L)
})
