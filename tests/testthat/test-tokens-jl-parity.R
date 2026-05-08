# Literal port of tokens.jl into R.
#
# Julia's tokens.jl tests the low-level Tokens module (escape_value /
# unescape_value, encode_expression / decode_expression, tokenize).
# dafr exposes escape_value / unescape_value with a DIFFERENT escape
# convention:
#   - Julia escapes with backslash:  ' '   -> '\\ '   ':' -> '\\:'
#   - dafr  escapes with double-quoting: ' ' -> '" "' (quoted)  ':' -> '":"'
# The encode/decode/tokenize internals are not exposed in dafr.
#
# Skipping all tokens leaves with CT1 (escape convention) and CT3
# (encode/tokenize internals), since the substantive escape -> unescape
# round-trip is exercised by dafr's existing internal tests.

# escape leaves: substantive parity is `escape -> unescape` round-trip.
# dafr uses double-quote escaping for operator-class characters, but
# the round-trip semantics match Julia.
test_that("tokens / escape / empty", {
    expect_identical(escape_value(""), "''")
    expect_identical(unescape_value("''"), "")
})
test_that("tokens / escape / unicode", {
    for (c in c("א", "ת")) {  # alef, tav
        expect_identical(unescape_value(escape_value(c)), c)
    }
})
test_that("tokens / escape / alpha", {
    for (c in c("a", "z", "A", "Z")) {
        expect_identical(escape_value(c), c)
        expect_identical(unescape_value(escape_value(c)), c)
    }
})
test_that("tokens / escape / digits", {
    for (d in c("0", "1", "5", "9")) {
        expect_identical(escape_value(d), d)
        expect_identical(unescape_value(escape_value(d)), d)
    }
})
test_that("tokens / escape / allowed", {
    for (c in c("_", ".", "+", "-")) {
        expect_identical(escape_value(c), c)
        expect_identical(unescape_value(escape_value(c)), c)
    }
})
test_that("tokens / escape / special", {
    expect_identical(escape_value(" "),  "\\ ")
    expect_identical(escape_value("\\"), "\\\\")
    expect_identical(escape_value("%"),  "\\%")
    expect_identical(escape_value(":"),  "\\:")
    for (c in c(" ", "&", "*", "%", "/", ":", "<", "=", ">", "?", "@",
                "[", "]", "^", "|", "~", "\"")) {
        expect_identical(unescape_value(escape_value(c)), c, info = c)
    }
})

# encode_expression leaves
test_that("tokens / encode / unicode", { skip("CT3: dafr does not expose encode_expression / decode_expression") })
test_that("tokens / encode / alpha",   { skip("CT3") })
test_that("tokens / encode / digits",  { skip("CT3") })
test_that("tokens / encode / allowed", { skip("CT3") })
test_that("tokens / encode / special", { skip("CT3") })

# tokenize leaves: dafr's tokenizer is private (`.tokenize_query`) and
# uses dafr's fixed query-operator set rather than Julia's pluggable
# regex. The substantive token-shape parity is exercised here.
.toks <- function(s) {
    vapply(dafr:::.tokenize_query(s), function(t) t$value, character(1L))
}

test_that("tokens / tokenize / empty", {
    expect_identical(.toks(""), character(0L))
})
test_that("tokens / tokenize / single", {
    expect_identical(.toks("1"), "1")
    expect_identical(.toks("x"), "x")
    expect_identical(.toks("''"), "")
    expect_identical(.toks("\\'\\'"), "''")
})
test_that("tokens / tokenize / multiple", {
    expect_identical(.toks(" 10  foo  0א "),
                     c("10", "foo", "0א"))
})
test_that("tokens / tokenize / specials", {
    # dafr op regex is greedy: longest match wins.
    expect_identical(.toks(">>"), ">>")
    expect_identical(.toks(">>>"), c(">>", ">"))
    expect_identical(.toks("=@"), "=@")
})
test_that("tokens / tokenize / unexpected", {
    expect_error(
        .toks("א $ x"),
        "unexpected character"
    )
})

# Round-trip parity assertion: dafr's escape/unescape round-trips.
test_that("tokens / escape-unescape / roundtrip", {
    # The escape convention differs from Julia's, but the round-trip
    # invariant (unescape(escape(x)) == x) MUST hold in both.
    samples <- c("", " ", "a", "Z", "0", "_", "+", "-", ".",
                 "\\", "%", ":", "אורז", "hello world")
    for (s in samples) {
        expect_identical(unescape_value(escape_value(s)), s)
    }
})
