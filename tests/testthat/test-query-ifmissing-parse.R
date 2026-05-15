# Julia DAF.jl validates `|| <value> <Type>` at parse time, so a broken
# default raises even when the surrounding lookup succeeds and the
# IfMissing branch is never taken. dafr used to defer the check to
# .coerce_if_missing_default, which only fires when the default is
# actually used; queries like `. intver || 1.5 Int32` therefore returned
# the existing scalar without flagging the malformed default.

test_that("|| 1.5 Int32 raises at parse even when scalar exists", {
    d <- memory_daf()
    set_scalar(d, "intver", 7L)
    expect_error(get_query(d, ". intver || 1.5 Int32"),
        "invalid value.*Int32")
})

test_that("|| abc Float32 raises at parse even when scalar exists", {
    d <- memory_daf()
    set_scalar(d, "x", 7.0)
    expect_error(get_query(d, ". x || abc Float32"),
        "invalid value.*Float32")
})

test_that("|| foo Bool raises at parse", {
    d <- memory_daf()
    set_scalar(d, "flag", TRUE)
    expect_error(get_query(d, ". flag || foo Bool"),
        "invalid value.*Bool")
})

test_that("|| 7 Int32 still parses and works when missing scalar absent", {
    d <- memory_daf()
    expect_equal(as.integer(get_query(d, ". no_such || 7 Int32")), 7L)
})

test_that("|| pi Float32 still works (named constant)", {
    d <- memory_daf()
    expect_equal(get_query(d, ". no_such || pi Float32"), pi)
})

test_that("|| hello String still works", {
    d <- memory_daf()
    expect_equal(get_query(d, ". no_such || hello String"), "hello")
})
