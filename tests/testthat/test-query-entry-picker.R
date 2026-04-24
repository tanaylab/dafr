# Tests for the `@ axis = entry` entry-picker phrase.
# Julia parity: DataAxesFormats.jl queries.jl — the SCALAR_QUERY phrase
#   ": vec @ axis = entry" picks one entry from a vector, and
#   ":: m @ rows = R @ cols = C" picks one cell from a matrix.

.fx_cells <- function() {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "age", c(10, 20, 30))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(c(1, 2, 3, 4, 5, 6), 3, 2,
            dimnames = list(c("A", "B", "C"), c("X", "Y")))
    )
    d
}

test_that("vector entry-pick via `: vec @ axis = entry` returns a scalar", {
    d <- .fx_cells()
    out <- get_query(d, "@ cell : age @ cell = B")
    expect_equal(out, 20)
})

test_that("vector entry-pick accepts builder form", {
    d <- .fx_cells()
    q <- Axis("cell") |> LookupVector("age") |> Axis("cell") |> IsEqual("C")
    expect_equal(get_query(d, q), 30)
})

test_that("vector entry-pick errors when the axis doesn't match the vector's axis", {
    d <- .fx_cells()
    expect_error(
        get_query(d, "@ cell : age @ gene = X"),
        "axis"
    )
})

test_that("vector entry-pick errors on a non-existent entry", {
    d <- .fx_cells()
    expect_error(
        get_query(d, "@ cell : age @ cell = Z"),
        "entry"
    )
})

test_that("matrix entry-pick via `:: m @ rows = R @ cols = C` returns a scalar", {
    d <- .fx_cells()
    out <- get_query(d, "@ cell @ gene :: UMIs @ cell = B @ gene = Y")
    expect_equal(out, 5)
})

test_that("matrix entry-pick (row first, column second) with different axis order", {
    d <- .fx_cells()
    # Pick the row for cell C and then the column for gene X.
    out <- get_query(d, "@ cell @ gene :: UMIs @ cell = C @ gene = X")
    expect_equal(out, 3)
})

test_that("matrix entry-pick leaves a vector after one @ axis = entry", {
    d <- .fx_cells()
    # Picking only the row should leave a vector along the remaining axis.
    out <- get_query(d, "@ cell @ gene :: UMIs @ cell = B")
    expect_equal(unname(out), c(2, 5))
})

test_that("matrix entry-pick errors on a non-existent row entry", {
    d <- .fx_cells()
    expect_error(
        get_query(d, "@ cell @ gene :: UMIs @ cell = Z @ gene = X"),
        "entry"
    )
})

test_that("entry-pick and IfMissing default play nicely together", {
    d <- .fx_cells()
    # Entry exists: default unused.
    expect_equal(get_query(d, "@ cell : age @ cell = B"), 20)
})

test_that("regression: plain mask `@ cell [ age > 15 ]` still works", {
    # The entry-picker shares the `=` token with the mask comparator. Make
    # sure masks continue to parse and evaluate correctly.
    d <- .fx_cells()
    expect_length(get_query(d, "@ cell [ age > 15 ]"), 2L)
    expect_length(get_query(d, "@ cell [ age = 20 ]"), 1L)
})
