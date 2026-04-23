test_that("escape_value is a public alias of .escape_value", {
    expect_identical(escape_value("plain"), "plain")
    expect_identical(escape_value("has space"), "\"has space\"")
    expect_identical(escape_value("quo\"ted"), "\"quo\\\"ted\"")
    expect_identical(escape_value("back\\slash"), "\"back\\\\slash\"")
})

test_that("unescape_value inverts escape_value", {
    cases <- c(
        "plain",
        "has space",
        "has\ttab",
        "quo\"ted",
        "back\\slash",
        "colons:are:special",
        "and!bangs",
        "and*stars",
        "and%percent",
        "and.dots",
        "and/slashes",
        "and<less",
        "and=equal",
        "and>greater",
        "and?q",
        "and@at",
        "and[bracket",
        "and]bracket",
        "and^caret",
        "and|pipe",
        "and~tilde",
        "and\"quote",
        "and&amp",
        "embedded\nnewline",
        "embedded\rreturn",
        "", # empty
        " ", # single space
        "unicode é",
        "combining ́",
        "a\\b\"c d"
    )
    for (s in cases) {
        expect_identical(unescape_value(escape_value(s)), s,
            info = sprintf("case: %s", paste(charToRaw(s), collapse = " "))
        )
    }
})

test_that("unescape_value leaves unquoted strings unchanged", {
    expect_identical(unescape_value("plain"), "plain")
    expect_identical(unescape_value(""), "")
})

test_that("query_requires_relayout TRUE when matrix axes swap", {
    # Build a minimal daf with UMIs stored only as (cell, gene).
    # example_cells_daf() also stores the relaid-out (gene, cell) copy, so
    # we use a fresh MemoryDaf with a single-direction matrix instead.
    d <- memory_daf(name = "relayout_test")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(1:6L, nrow = 2L))
    # (cell, gene) is the stored order → no relayout needed.
    expect_false(query_requires_relayout(d, "@ cell @ gene :: UMIs"))
    # (gene, cell) is the swapped order → relayout required.
    expect_true(query_requires_relayout(d, "@ gene @ cell :: UMIs"))
})

test_that("query_requires_relayout FALSE for non-matrix queries", {
    d <- example_cells_daf()
    expect_false(query_requires_relayout(d, "@ cell : donor"))
    expect_false(query_requires_relayout(d, ". organism"))
    expect_false(query_requires_relayout(d, "@ cell"))
})

test_that("query_requires_relayout errors on parse failure", {
    d <- example_cells_daf()
    expect_error(query_requires_relayout(d, "@ @ @"))
})
