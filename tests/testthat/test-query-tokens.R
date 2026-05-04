test_that(".tokenize_query splits simple axis lookup", {
    toks <- .tokenize_query("@ cell")
    expect_equal(length(toks), 2L)
    expect_equal(toks[[1]]$type, "operator")
    expect_equal(toks[[1]]$value, "@")
    expect_equal(toks[[2]]$type, "value")
    expect_equal(toks[[2]]$value, "cell")
})

test_that(".tokenize_query splits vector lookup", {
    toks <- .tokenize_query("@ cell : UMIs")
    expect_equal(length(toks), 4L)
    expect_equal(vapply(toks, `[[`, "", "value"), c("@", "cell", ":", "UMIs"))
})

test_that(".tokenize_query handles double operators (::, >>, >-)", {
    toks <- .tokenize_query("@ cell @ gene :: UMIs >| Sum")
    vals <- vapply(toks, `[[`, "", "value")
    expect_equal(vals, c("@", "cell", "@", "gene", "::", "UMIs", ">|", "Sum"))
})

test_that(".tokenize_query preserves escaped values", {
    toks <- .tokenize_query("@ cell : \"weird name\"")
    vals <- vapply(toks, `[[`, "", "value")
    expect_equal(vals[[4L]], "weird name")
})

test_that(".tokenize_query records 1-based positions", {
    toks <- .tokenize_query("@ cell")
    expect_equal(toks[[1]]$pos, 1L)
    expect_equal(toks[[2]]$pos, 3L)
})

test_that(".tokenize_query rejects unknown operator characters", {
    expect_error(.tokenize_query("@ cell $ weird"), "unexpected character")
})

test_that(".tokenize_query treats `.` as a value char inside an unquoted name", {
    # Per Julia tokens.jl: `.` is a safe value character. So `M412.08`
    # is a single value token, not three (M412, ., 08). A standalone `.`
    # surrounded by whitespace remains the LookupScalar operator.
    toks <- .tokenize_query("@ metacell = M412.08")
    vals <- vapply(toks, `[[`, "", "value")
    expect_equal(vals, c("@", "metacell", "=", "M412.08"))
})

test_that(".tokenize_query keeps a leading `.` as the LookupScalar operator", {
    toks <- .tokenize_query(". organism")
    expect_equal(toks[[1]]$type, "operator")
    expect_equal(toks[[1]]$value, ".")
    expect_equal(toks[[2]]$value, "organism")
})

test_that(".tokenize_query unescapes `\\X` inside an unquoted value", {
    # `\.` should yield a literal `.` in the value token, matching Julia's
    # encode/unescape pipeline for embedding user-supplied names.
    toks <- .tokenize_query("@ gene = AL627309\\.1")
    vals <- vapply(toks, `[[`, "", "value")
    expect_equal(vals, c("@", "gene", "=", "AL627309.1"))
})

test_that(".tokenize_query produces an empty-value token for `''`", {
    # Round-trip with Julia's escape_value(\"\") == \"''\". Required so a
    # query string emitted by Julia (or by dafr's own canonicaliser for
    # an empty-string axis entry) parses back identically.
    toks <- .tokenize_query("@ metacell : type = ''")
    types <- vapply(toks, `[[`, "", "type")
    vals  <- vapply(toks, `[[`, "", "value")
    expect_equal(types, c("operator", "value", "operator", "value", "operator", "value"))
    expect_equal(vals,  c("@", "metacell", ":", "type", "=", ""))
})

test_that(".tokenize_query is linear in value length (DoS guard)", {
    # Pre-fix this was O(n^2) due to a per-char loop with `c(out, ch)`,
    # so a 100k-char value blew past the 2s test timeout. After the fix,
    # 100k characters tokenize in well under a second on any modern host.
    big <- strrep("a", 100000L)
    t <- system.time(toks <- .tokenize_query(big))
    expect_length(toks, 1L)
    expect_equal(nchar(toks[[1]]$value), 100000L)
    expect_lt(t["elapsed"], 1.0)
})

test_that(".tokenize_query unescapes embedded `\\X` runs in linear time", {
    # Mixed escape + non-escape pattern. Should be fast and produce a
    # single value with the escape characters dropped.
    big <- paste0(strrep("a\\.", 10000L), "z")
    t <- system.time(toks <- .tokenize_query(big))
    expect_length(toks, 1L)
    expect_equal(nchar(toks[[1]]$value), 20001L)
    expect_lt(t["elapsed"], 1.0)
})

test_that(".tokenize_query treats `# ... <eol>` as a line comment", {
    # Per Julia tokens.jl SPACE_REGEX, `#` to end-of-line is whitespace.
    toks <- .tokenize_query("@ cell # this is a comment\n: donor")
    vals <- vapply(toks, `[[`, "", "value")
    expect_equal(vals, c("@", "cell", ":", "donor"))
    # Leading and trailing comments also collapse to whitespace.
    expect_equal(
        vapply(.tokenize_query("# leading\n@ cell"), `[[`, "", "value"),
        c("@", "cell")
    )
    expect_equal(
        vapply(.tokenize_query("@ cell # trailing"), `[[`, "", "value"),
        c("@", "cell")
    )
})
