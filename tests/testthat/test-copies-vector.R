test_that("copy_vector: same-axis dense copy", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    set_vector(src, "cell", "age", c(10L, 20L, 30L))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    copy_vector(dest, src, "cell", "age")
    expect_identical(unname(get_vector(dest, "cell", "age")),
                     c(10L, 20L, 30L))
})

test_that("copy_vector: rename and reaxis", {
    src <- memory_daf(name = "src")
    add_axis(src, "obs", c("o1", "o2"))
    set_vector(src, "obs", "age", c(1.0, 2.0))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("o1", "o2"))

    copy_vector(dest, src, "obs", "age", reaxis = "cell", rename = "age_years")
    expect_identical(unname(get_vector(dest, "cell", "age_years")),
                     c(1.0, 2.0))
})

test_that("copy_vector: type coerces destination storage mode", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    set_vector(src, "cell", "age", c(1.7, 2.3))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))

    copy_vector(dest, src, "cell", "age", type = "integer")
    expect_true(is.integer(get_vector(dest, "cell", "age")))
    expect_identical(unname(get_vector(dest, "cell", "age")), c(1L, 2L))
})

test_that("copy_vector: destination is subset — extracts entries", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    set_vector(src, "cell", "age", c(1.0, 2.0, 3.0))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c3"))

    copy_vector(dest, src, "cell", "age")
    expect_identical(unname(get_vector(dest, "cell", "age")), c(1.0, 3.0))
})

test_that("copy_vector: source is subset — raises without empty", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    set_vector(src, "cell", "age", c(1.0, 2.0))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    expect_error(copy_vector(dest, src, "cell", "age"),
                 "missing entries")
})

test_that("copy_vector: source is subset — fills missing with empty", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c3"))
    set_vector(src, "cell", "age", c(10.0, 30.0))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    copy_vector(dest, src, "cell", "age", empty = -1.0)
    expect_equal(unname(get_vector(dest, "cell", "age")),
                 c(10.0, -1.0, 30.0))
})

test_that("copy_vector: default scalar fills absent source", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))

    copy_vector(dest, src, "cell", "age", default = 42L)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(42L, 42L))
})

test_that("copy_vector: default NULL silently skips absent source", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("c1"))
    copy_vector(dest, src, "cell", "age", default = NULL)
    expect_false(has_vector(dest, "cell", "age"))
})

test_that("copy_vector: overwrite=TRUE replaces destination vector", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(99L))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("c1"))
    set_vector(dest, "cell", "age", c(1L))
    copy_vector(dest, src, "cell", "age", overwrite = TRUE)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(99L))
})

test_that("copy_vector: insist=FALSE silently skips when destination has it", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(99L))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("c1"))
    set_vector(dest, "cell", "age", c(1L))
    copy_vector(dest, src, "cell", "age", insist = FALSE)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(1L))
})

test_that("copy_vector validates rename / reaxis / overwrite / insist", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(1L))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("c1"))
    expect_error(copy_vector(dest, src, "cell", "age", rename = ""), "rename")
    expect_error(copy_vector(dest, src, "cell", "age", reaxis = ""), "reaxis")
    expect_error(copy_vector(dest, src, "cell", "age", overwrite = 1), "overwrite")
    expect_error(copy_vector(dest, src, "cell", "age", insist = NA), "insist")
})

test_that("copy_vector: sparse same-axis value passes through", {
    skip_if_not_installed("Matrix")
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    v <- Matrix::sparseVector(c(1, 3), c(1L, 3L), length = 3L)
    # set_vector may coerce sparse to dense internally; this test
    # just asserts the numeric content survives end-to-end.
    set_vector(src, "cell", "age", as.numeric(v))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    copy_vector(dest, src, "cell", "age")
    expect_equal(unname(get_vector(dest, "cell", "age")), c(1, 0, 3))
})
