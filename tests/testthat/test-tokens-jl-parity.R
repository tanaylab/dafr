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

# escape leaves
test_that("tokens / escape / empty",   { skip("CT1: dafr escape convention differs from Julia") })
test_that("tokens / escape / unicode", { skip("CT1") })
test_that("tokens / escape / alpha",   { skip("CT1") })
test_that("tokens / escape / digits",  { skip("CT1") })
test_that("tokens / escape / allowed", { skip("CT1") })
test_that("tokens / escape / special", { skip("CT1") })

# encode_expression leaves
test_that("tokens / encode / unicode", { skip("CT3: dafr does not expose encode_expression / decode_expression") })
test_that("tokens / encode / alpha",   { skip("CT3") })
test_that("tokens / encode / digits",  { skip("CT3") })
test_that("tokens / encode / allowed", { skip("CT3") })
test_that("tokens / encode / special", { skip("CT3") })

# tokenize leaves
test_that("tokens / tokenize / empty",      { skip("CT3: dafr does not expose Tokens.tokenize") })
test_that("tokens / tokenize / single",     { skip("CT3") })
test_that("tokens / tokenize / multiple",   { skip("CT3") })
test_that("tokens / tokenize / specials",   { skip("CT3") })
test_that("tokens / tokenize / unexpected", { skip("CT3") })

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
