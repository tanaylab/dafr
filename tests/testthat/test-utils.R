test_that(".assert_name rejects filesystem-hostile characters", {
    # Note: R cannot construct a character scalar containing an embedded NUL
    # (both `"a\0b"` at parse time and `rawToChar(as.raw(c(0x61, 0x00, 0x62)))`
    # at runtime error out), so the `\0` case from the spec is omitted here.
    # The regex in `.assert_name` still guards against it defensively.
    for (bad in c(
        "a/b", "a\\b", "a:b", "a,b", "a\nb", "a\rb",
        " leading", "trailing ", ""
    )) {
        expect_error(
            dafr:::.assert_name(bad, "name"),
            "must be a non-NA character scalar|contains forbidden|may not be empty|may not have leading/trailing whitespace"
        )
    }
})

test_that(".assert_name accepts ordinary names", {
    for (ok in c("cell", "UMIs", "donor_1", "gene.count", "x-y", "β")) {
        expect_silent(dafr:::.assert_name(ok, "name"))
    }
})
