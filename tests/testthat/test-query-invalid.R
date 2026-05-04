# Parity port of DataAxesFormats.jl test/queries.jl > "queries > invalid"
# nested test set, plus the "names > unexpected" leaf.

test_that("parse_query errors on bare value (no operator)", {
    expect_error(parse_query("cell"), "expected operator")
})

test_that("parse_query errors on '>>' with no reduction name", {
    expect_error(parse_query(">>"), "expected reduction")
})

test_that("parse_query errors on '>>' followed by an operator (no value)", {
    expect_error(parse_query(">> ."), "expected reduction")
})

test_that("partial query (@ cell @ gene with no lookup) is invalid", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    expect_error(get_query(d, "@ cell @ gene"), "invalid query")
    expect_false(has_query(d, "@ cell @ gene"))
})

test_that("mask on a scalar query is rejected", {
    d <- memory_daf(name = "memory!")
    set_scalar(d, "score", 1.0)
    expect_error(get_query(d, ". score [ is_first ]"), "mask|axis")
})

test_that("parse_query errors on unknown eltwise operation name", {
    expect_error(
        parse_query(". score % Frobulate"),
        "unknown eltwise operation"
    )
})

test_that("parse_query errors on unknown reduction operation name", {
    expect_error(
        parse_query("@ cell : x >> Frobulate"),
        "unknown reduction operation"
    )
})

test_that("parse_query errors on unknown parameter for eltwise op", {
    expect_error(
        parse_query(". score % Log phase 2"),
        "phase.*Log|Log.*phase"
    )
})

test_that("parse_query errors on unknown parameter for reduction op", {
    expect_error(
        parse_query("@ cell : x >> Sum nonexistent 1"),
        "nonexistent.*Sum|Sum.*nonexistent"
    )
})

test_that("parse_query errors on repeated parameter for eltwise op", {
    expect_error(
        parse_query(". score % Log base pi base e"),
        "repeated parameter"
    )
})

test_that("parse_query errors on repeated parameter for reduction op", {
    expect_error(
        parse_query("@ cell : x >> Quantile p 0.5 p 0.9"),
        "repeated parameter"
    )
})

test_that("'? ?' double-Names is rejected", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    expect_error(get_query(d, "? ?"), "invalid|unexpected")
})
