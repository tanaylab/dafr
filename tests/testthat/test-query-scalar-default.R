# Parity port of DataAxesFormats.jl test/queries.jl > "queries > scalar >
# lookup > with_default" leaves: const {pi, e, true, false}, !int, plus
# the matching reduction-on-string and matrix scalar-lookup defaults.

test_that("|| pi returns Float64(pi) when scalar is missing", {
    d <- memory_daf(name = "memory!")
    expect_equal(get_query(d, ". version || pi"), pi)
})

test_that("|| e returns Float64(e) when scalar is missing", {
    d <- memory_daf(name = "memory!")
    expect_equal(get_query(d, ". version || e"), exp(1))
})

test_that("|| true returns Bool TRUE when scalar is missing", {
    d <- memory_daf(name = "memory!")
    expect_identical(get_query(d, ". version || true"), TRUE)
})

test_that("|| false returns Bool FALSE when scalar is missing", {
    d <- memory_daf(name = "memory!")
    expect_identical(get_query(d, ". version || false"), FALSE)
})

test_that("|| 1.0 Int32 errors on coercion (Julia-style strictness)", {
    d <- memory_daf(name = "memory!")
    expect_error(
        get_query(d, ". version || 1.0 Int32"),
        "Int32|invalid value"
    )
})

test_that("|| 1.0 Int64 also errors on coercion", {
    d <- memory_daf(name = "memory!")
    expect_error(
        get_query(d, ". version || 1.0 Int64"),
        "Int64|invalid value"
    )
})

test_that("|| 1.0 Float32 succeeds (Float can hold 1.0)", {
    d <- memory_daf(name = "memory!")
    expect_equal(get_query(d, ". version || 1.0 Float32"), 1.0)
})

test_that("|| 1.0 String returns '1.0' (string default)", {
    d <- memory_daf(name = "memory!")
    expect_identical(get_query(d, ". version || 1.0 String"), "1.0")
})

test_that("|| foo returns 'foo' (untyped string default)", {
    d <- memory_daf(name = "memory!")
    expect_identical(get_query(d, ". version || foo"), "foo")
})

# --- Constants set on actual lookups (not default path) ---------------------
# These verify that named constants do NOT shadow real scalar values that
# happen to be named "pi", "e", "true", "false".

test_that("|| pi only fires on missing scalar (does not shadow real value)", {
    d <- memory_daf(name = "memory!")
    set_scalar(d, "pi_value", "3.0")
    expect_identical(get_query(d, ". pi_value || pi"), "3.0")
})
