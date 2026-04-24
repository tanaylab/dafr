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

test_that("chain_reader: axis union + entry agreement", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "B"))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_true(has_axis(ch, "cell"))
    expect_identical(axis_vector(ch, "cell"), c("A", "B"))
    expect_identical(axis_length(ch, "cell"), 2L)
})

test_that("chain_reader: axis mismatch raises at construction", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "C"))
    expect_error(chain_reader(list(d1, d2), name = "chain"),
        "different entry#2: C"
    )
})

test_that("chain_reader: axis length mismatch raises at construction", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A"))
    expect_error(chain_reader(list(d1, d2), name = "chain"),
        "different number of entries"
    )
})

test_that("chain_reader: vector falls through in reverse order (last wins)", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "B"))
    set_vector(d1, "cell", "age", c(1L, 2L))
    set_vector(d2, "cell", "age", c(3L, 4L))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_identical(unname(get_vector(ch, "cell", "age")), c(3L, 4L))
})

test_that("chain_reader: vector from only-first daf is visible", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "B"))
    set_vector(d1, "cell", "age", c(1L, 2L))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_true(has_vector(ch, "cell", "age"))
    expect_identical(unname(get_vector(ch, "cell", "age")), c(1L, 2L))
})

test_that("chain_reader: vectors_set is union across all dafs that have the axis", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "B"))
    set_vector(d1, "cell", "age", c(1L, 2L))
    set_vector(d2, "cell", "donor", c("x", "y"))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_setequal(vectors_set(ch, "cell"), c("age", "donor"))
})

test_that("chain_reader: matrix falls through last wins", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d1, "gene", c("X", "Y"))
    add_axis(d2, "cell", c("A", "B"))
    add_axis(d2, "gene", c("X", "Y"))
    m1 <- matrix(1:4, nrow = 2)
    m2 <- matrix(5:8, nrow = 2)
    set_matrix(d1, "cell", "gene", "UMIs", m1)
    set_matrix(d2, "cell", "gene", "UMIs", m2)
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_identical(unname(as.matrix(get_matrix(ch, "cell", "gene", "UMIs"))), m2)
})

test_that("chain_reader: matrices_set is union", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", "A")
    add_axis(d1, "gene", "X")
    add_axis(d2, "cell", "A")
    add_axis(d2, "gene", "X")
    set_matrix(d1, "cell", "gene", "A", matrix(1, 1, 1))
    set_matrix(d2, "cell", "gene", "B", matrix(2, 1, 1))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_setequal(matrices_set(ch, "cell", "gene"), c("A", "B"))
})
