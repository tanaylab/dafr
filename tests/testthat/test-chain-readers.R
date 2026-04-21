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

test_that("chain_reader: scalar falls through in reverse order (last wins)", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    set_scalar(d1, "version", 1L)
    set_scalar(d2, "version", 2L)
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_identical(get_scalar(ch, "version"), 2L)
})

test_that("chain_reader: scalar from only-first daf is visible", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    set_scalar(d1, "version", 1L)
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_identical(get_scalar(ch, "version"), 1L)
})

test_that("chain_reader: scalars_set is union of all dafs", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    set_scalar(d1, "a", 1L)
    set_scalar(d2, "b", 2L)
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_setequal(scalars_set(ch), c("a", "b"))
})
