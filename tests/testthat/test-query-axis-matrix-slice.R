# Tests for the `@ axis :: matrix_prop @ other_axis = entry` phrase.
# Julia parity: DataAxesFormats.jl queries.jl docstring example
#   metacells["@ gene :: fraction @ metacell = M412.08"]
# This phrase looks up a matrix property and slices a single column of the
# other axis, returning a vector indexed by the in-scope axis.

.fx_mat <- function() {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(c(1, 2, 3, 4, 5, 6), 3, 2,
            dimnames = list(c("A", "B", "C"), c("X", "Y")))
    )
    d
}

test_that("matrix-by-axis slice: `@ rows :: m @ cols = C` returns a vector", {
    d <- .fx_mat()
    out <- get_query(d, "@ cell :: UMIs @ gene = Y")
    expect_equal(unname(out), c(4, 5, 6))
    expect_equal(names(out), c("A", "B", "C"))
})

test_that("matrix-by-axis slice along the row axis: `@ rows :: m @ cols = X`", {
    # Same direction as storage order; the in-scope axis becomes the row
    # axis of the matrix lookup, so this tests the typical case.
    d <- .fx_mat()
    out <- get_query(d, "@ cell :: UMIs @ gene = X")
    expect_equal(unname(out), c(1, 2, 3))
    expect_equal(names(out), c("A", "B", "C"))
})

test_that("matrix-by-axis slice errors on a non-existent entry", {
    d <- .fx_mat()
    expect_error(
        get_query(d, "@ cell :: UMIs @ gene = Z"),
        "entry"
    )
})

test_that("matrix-by-axis slice errors on a non-existent matrix property", {
    d <- .fx_mat()
    expect_error(
        get_query(d, "@ cell :: nope @ gene = Y"),
        "no matrix"
    )
})

test_that("matrix-by-axis slice with IfMissing default fills the vector", {
    # Without an explicit `type ...` clause the default is left as-is, the
    # same convention as the two-axis matrix lookup. With `type Float32` we
    # get a numeric vector back.
    d <- .fx_mat()
    out <- get_query(d, "@ cell :: nope || 0 type Float32 @ gene = Y")
    expect_equal(unname(out), c(0, 0, 0))
    expect_equal(names(out), c("A", "B", "C"))
})

test_that("matrix-by-axis slice composes with a row mask", {
    # The row axis is mask-filtered before the matrix lookup; the result
    # should only contain the surviving rows.
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "age", c(10, 20, 30))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(c(1, 2, 3, 4, 5, 6), 3, 2,
            dimnames = list(c("A", "B", "C"), c("X", "Y")))
    )
    out <- get_query(d, "@ cell [ age > 15 ] :: UMIs @ gene = Y")
    expect_equal(unname(out), c(5, 6))
    expect_equal(names(out), c("B", "C"))
})

test_that("matrix-by-axis slice on the documented metacells example", {
    # Exact query from the DAF.jl docstring (queries.jl line 357).
    mc <- example_metacells_daf()
    out <- mc["@ gene :: fraction @ metacell = M412.08"]
    # Result is a vector along the gene axis. Length should equal the
    # number of genes in the example daf.
    expect_true(is.numeric(out))
    expect_gt(length(out), 0L)
    # Names are gene names; the example has 683 genes per the doctest.
    expect_equal(length(out), length(mc["? @ gene"]))
})
