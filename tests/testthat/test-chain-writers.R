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

test_that("chain_writer: set_scalar writes to top writer", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_writer(list(d1, d2), name = "chain")
    set_scalar(ch, "version", 7L)
    expect_false(has_scalar(d1, "version"))
    expect_true(has_scalar(d2, "version"))
    expect_identical(get_scalar(ch, "version"), 7L)
})

test_that("chain_writer: delete_scalar errors when scalar exists in earlier daf", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    set_scalar(d1, "version", 1L)
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_error(delete_scalar(ch, "version"),
        "because it exists in the earlier: one"
    )
})

test_that("chain_writer: delete_scalar removes from top writer only", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_writer(list(d1, d2), name = "chain")
    set_scalar(ch, "version", 1L)
    expect_true(has_scalar(d2, "version"))
    delete_scalar(ch, "version")
    expect_false(has_scalar(d2, "version"))
    expect_false(has_scalar(ch, "version"))
})

test_that("chain_writer: add_axis adds on top writer only", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_writer(list(d1, d2), name = "chain")
    add_axis(ch, "cell", c("A", "B"))
    expect_false(has_axis(d1, "cell"))
    expect_true(has_axis(d2, "cell"))
    expect_identical(axis_vector(ch, "cell"), c("A", "B"))
})

test_that("chain_writer: delete_axis rejects deletion of earlier-only axis", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_error(delete_axis(ch, "cell"),
        "because it exists in the earlier: one"
    )
})

test_that("chain_writer: set_vector auto-adds missing axis on writer", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    ch <- chain_writer(list(d1, d2), name = "chain")
    set_vector(ch, "cell", "age", c(1L, 2L))
    expect_true(has_axis(d2, "cell"))
    expect_identical(axis_vector(d2, "cell"), c("A", "B"))
    expect_identical(unname(get_vector(ch, "cell", "age")), c(1L, 2L))
})

test_that("chain_writer: delete_vector error when vector exists in earlier", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", "A")
    set_vector(d1, "cell", "age", 1L)
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_error(delete_vector(ch, "cell", "age"),
        "because it exists in the earlier: one"
    )
})
