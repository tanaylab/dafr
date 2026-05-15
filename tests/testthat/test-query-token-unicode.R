# Julia tokens.jl VALUE_REGEX = `r"^(?:[0-9a-zA-Z_.+-]|[^\x00-\xFF])+"`.
# That is: ASCII safe chars OR codepoints > 0xFF. Codepoints in the
# Latin-1 supplement (é, ñ, ü, ...) are NOT in the safe set; Julia
# raises `unexpected character: 'é'`. dafr previously used `[^\x00-\x7F]`
# which let any non-ASCII through.

test_that("unescaped Latin-1 char in value token is rejected (Julia parity)", {
    d <- memory_daf()
    expect_error(get_query(d, ". x || héllo"),
        "unexpected character")
})

test_that("unescaped Latin-1 char in axis entry value is rejected", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "lbl", c("é", "f"))
    expect_error(get_query(d, "@ cell = é : lbl"),
        "unexpected character")
})

test_that("escaped Latin-1 char in value token is accepted", {
    d <- memory_daf()
    expect_equal(get_query(d, ". missing || h\\éllo"), "héllo")
})

test_that("codepoints above 0xFF (e.g. CJK) pass through unescaped", {
    d <- memory_daf()
    expect_equal(get_query(d, ". missing || あいう"), "あいう")
})

test_that("ASCII safe chars still tokenise as before", {
    d <- memory_daf()
    expect_equal(get_query(d, ". missing || hello"), "hello")
    expect_equal(get_query(d, ". missing || 42"), 42L)
})
