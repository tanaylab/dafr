test_that("chain_writer exists and produces a DafWriter", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_s3_class(ch, "dafr::DafWriter")
    expect_identical(S7::prop(ch, "name"), "chain")
})

test_that("chain_writer with empty list raises", {
    expect_error(chain_writer(list(), name = "empty"), "empty chain")
})
