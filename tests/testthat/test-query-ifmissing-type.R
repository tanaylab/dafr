# Tests for `|| value type T` scalar-default type coercion.
# Julia parity: DataAxesFormats.jl queries.jl `IfMissing(value, type)`.
# Slice: dev-branch "parser slice" 2026-04-24.

test_that("parse_query captures `type T` suffix on IfMissing", {
    # `0.0.0` contains `.` (the scalar-lookup operator) so it must be
    # quoted as a literal to survive the tokenizer.
    ast <- parse_query('. version || "0.0.0" type String')
    im <- ast[[2L]]
    expect_equal(im$op, "IfMissing")
    expect_equal(im$default, "0.0.0")
    expect_equal(im$type, "String")

    ast2 <- parse_query(". n || 0 type Int64")
    expect_equal(ast2[[2L]]$default, "0")
    expect_equal(ast2[[2L]]$type, "Int64")
})

test_that("parse_query treats bare `|| value` as unchanged (no type)", {
    ast <- parse_query(". foo || 0")
    im <- ast[[2L]]
    expect_equal(im$op, "IfMissing")
    expect_equal(im$default, "0")
    expect_null(im$type)
})

test_that("canonical_query preserves `|| value type T`", {
    # Canonicaliser quotes defaults containing `.` (matches existing
    # escape_value behaviour for plain IfMissing defaults).
    expect_equal(
        canonical_query(". n || 0 type Int64"),
        ". n || 0 type Int64"
    )
    expect_equal(
        canonical_query(". pi || 3.14 type Float32"),
        ". pi || \"3.14\" type Float32"
    )
    # Round-trip: parse-then-canonicalise of the quoted form is idempotent.
    q <- ". pi || \"3.14\" type Float32"
    expect_equal(canonical_query(q), q)
})

test_that("IfMissing with Int64 type coerces scalar default to integer64", {
    d <- memory_daf(name = "t")
    out <- get_query(d, ". missing || 0 type Int64")
    expect_s3_class(out, "integer64")
    expect_equal(unclass(out), unclass(bit64::as.integer64(0)))
})

test_that("IfMissing with Int32 type coerces scalar default to integer", {
    d <- memory_daf(name = "t")
    out <- get_query(d, ". missing || 42 type Int32")
    expect_true(is.integer(out))
    expect_equal(out, 42L)
})

test_that("IfMissing with Float64 type coerces scalar default to double", {
    d <- memory_daf(name = "t")
    out <- get_query(d, ". missing || 3.14 type Float64")
    expect_type(out, "double")
    expect_equal(out, 3.14)
})

test_that("IfMissing with Float32 type coerces scalar default to double", {
    d <- memory_daf(name = "t")
    out <- get_query(d, ". missing || 2.5 type Float32")
    expect_type(out, "double")
    expect_equal(out, 2.5)
})

test_that("IfMissing with Bool type coerces scalar default to logical", {
    d <- memory_daf(name = "t")
    expect_identical(
        get_query(d, ". missing || 1 type Bool"),
        TRUE
    )
    expect_identical(
        get_query(d, ". missing || 0 type Bool"),
        FALSE
    )
})

test_that("IfMissing with String type keeps default as character", {
    d <- memory_daf(name = "t")
    out <- get_query(d, ". missing || hello type String")
    expect_type(out, "character")
    expect_equal(out, "hello")
})

test_that("IfMissing with unknown type raises an error", {
    d <- memory_daf(name = "t")
    expect_error(
        get_query(d, ". missing || 0 type Nonsense"),
        "IfMissing.*type"
    )
})

test_that("IfMissing type only applies when the scalar is actually missing", {
    d <- memory_daf(name = "t")
    set_scalar(d, "n", 7)
    # Present: default/type ignored, value returned as stored.
    out <- get_query(d, ". n || 0 type Int64")
    expect_equal(out, 7)
    expect_false(bit64::is.integer64(out))
})

test_that("IfMissing() builder accepts type= and round-trips via canonical_query", {
    q <- Axis("cell") |> LookupVector("age") |> IfMissing(0, type = "Int64")
    expect_equal(as.character(q), "@ cell : age || 0 type Int64")
})
