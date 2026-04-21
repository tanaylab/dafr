test_that("IfNot stores sentinel on evaluator state for downstream chain lookup", {
    # Parsing / canonicalisation of IfNot already worked in Slice 3; this
    # test establishes the file that F4 will overwrite with the full
    # IfNot+AsAxis integration tests. F3 itself only captures the
    # sentinel on state; F4 consumes it.
    canon <- canonical_query('@ cell : metacell ?? "UNK" =@ type')
    expect_match(canon, "metacell \\?\\? UNK =@ type")
})
