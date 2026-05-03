# Parity port of DataAxesFormats.jl test/queries.jl > {vector,matrix} >
# compare > {!string,!regex,!number,>}.

# ---- Vector comparator validation -----------------------------------------

test_that("vector ~ on numeric raises 'unsupported element type'", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "score", c(0.5, 1.5))
    expect_error(
        get_query(d, "@ cell [ score ~ ABC ] : score"),
        "unsupported.*element type|IsMatch"
    )
})

test_that("vector ~ with malformed regex raises 'invalid regular expression'", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "type", c("U", "V"))
    expect_error(
        get_query(d, "@ cell [ type ~ [UV ] : type"),
        "invalid regular expression|IsMatch"
    )
})

test_that("vector = U on numeric raises 'cannot parse number'", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "score", c(0.5, 1.5))
    expect_error(
        get_query(d, "@ cell [ score = U ] : score"),
        "error parsing.*comparison|cannot parse"
    )
})

# ---- Vector comparator happy paths ----------------------------------------

test_that("vector comparators on String / Float yield filtered axes", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "type", c("U", "V"))
    set_vector(d, "cell", "score", c(0.5, 1.5))
    expect_equal(get_query(d, "@ cell [ type < V ] : type"), c(X = "U"))
    expect_equal(get_query(d, "@ cell [ type <= U ] : type"), c(X = "U"))
    expect_equal(get_query(d, "@ cell [ type = U ] : type"), c(X = "U"))
    expect_equal(get_query(d, "@ cell [ type != V ] : type"), c(X = "U"))
    expect_equal(get_query(d, "@ cell [ type >= V ] : type"), c(Y = "V"))
    expect_equal(get_query(d, "@ cell [ type > U ] : type"), c(Y = "V"))
    expect_equal(get_query(d, "@ cell [ score < 1.0 ] : score"), c(X = 0.5))
    expect_equal(get_query(d, "@ cell [ score > 1.0 ] : score"), c(Y = 1.5))
})

# ---- Matrix comparator validation -----------------------------------------

test_that("matrix ~ on numeric raises 'unsupported matrix element type'", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    expect_error(
        get_query(d, "@ cell @ gene :: UMIs ~ ABC"),
        "unsupported.*element type|IsMatch"
    )
})

test_that("matrix ~ with malformed regex on text matrix raises", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "text", matrix(
        as.character(c(0, 3, 1, 4, 2, 5)), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    expect_error(
        get_query(d, "@ cell @ gene :: text ~ [UV"),
        "invalid regular expression|IsMatch"
    )
})

test_that("matrix = U on numeric raises 'cannot parse number'", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    expect_error(
        get_query(d, "@ cell @ gene :: UMIs = U"),
        "error parsing.*comparison|cannot parse"
    )
})

# ---- Matrix comparator as operation ---------------------------------------

test_that(":: UMIs > 0 produces a Bool matrix (operation, not just mask)", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    out <- get_query(d, "@ cell @ gene :: UMIs > 0")
    expect_equal(dim(out), c(2L, 3L))
    expect_equal(unname(out["X", ]), c(FALSE, TRUE, TRUE))
    expect_equal(unname(out["Y", ]), c(TRUE, TRUE, TRUE))
})
