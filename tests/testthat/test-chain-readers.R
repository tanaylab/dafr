test_that("chain_reader exists and produces a DafReadOnly", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_s3_class(ch, "dafr::DafReadOnly")
    expect_identical(S7::prop(ch, "name"), "chain")
})

test_that("chain_reader with empty list raises", {
    expect_error(chain_reader(list(), name = "empty"), "empty chain")
})

test_that("chain_reader with single daf returns a read-only view of it", {
    d <- memory_daf(name = "only")
    ch <- chain_reader(list(d), name = "chain")
    expect_s3_class(ch, "dafr::DafReadOnly")
})
