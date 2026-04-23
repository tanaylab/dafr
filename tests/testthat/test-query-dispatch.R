test_that("get_query accepts DafrQuery", {
    d <- example_cells_daf()
    q <- DafrQuery(
        ast = parse_query("@ cell : donor"),
        canonical = "@ cell : donor"
    )
    expect_identical(get_query(d, q), get_query(d, "@ cell : donor"))
})

test_that("get_query rejects non-character non-DafrQuery", {
    d <- memory_daf()
    expect_error(get_query(d, 42L), "character scalar or DafrQuery")
    expect_error(get_query(d, TRUE), "character scalar or DafrQuery")
    expect_error(get_query(d, NULL), "character scalar or DafrQuery")
})

test_that("has_query accepts DafrQuery", {
    d <- example_cells_daf()
    q <- DafrQuery(
        ast = parse_query(". organism"),
        canonical = ". organism"
    )
    expect_true(has_query(d, q))
})

test_that("[.DafReader accepts both character and DafrQuery", {
    d <- example_cells_daf()
    q <- DafrQuery(
        ast = parse_query("@ cell : donor"),
        canonical = "@ cell : donor"
    )
    expect_identical(d[q], d["@ cell : donor"])
})

test_that("[.DafReader errors on bad input", {
    d <- memory_daf()
    expect_error(d[42L])
})
