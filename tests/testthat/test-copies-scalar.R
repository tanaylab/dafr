test_that("copy_scalar copies a scalar verbatim", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    set_scalar(src, "alpha", "a")
    copy_scalar(dest, src, "alpha")
    expect_identical(get_scalar(dest, "alpha"), "a")
})

test_that("copy_scalar honors rename", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    set_scalar(src, "alpha", "a")
    copy_scalar(dest, src, "alpha", rename = "beta")
    expect_identical(get_scalar(dest, "beta"), "a")
    expect_false(has_scalar(dest, "alpha"))
})

test_that("copy_scalar casts numeric scalars when type specified", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    set_scalar(src, "n", 3.7)
    copy_scalar(dest, src, "n", type = "integer")
    expect_true(is.integer(get_scalar(dest, "n")))
    expect_identical(get_scalar(dest, "n"), 3L)
})

test_that("copy_scalar errors when source missing and no default", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    expect_error(copy_scalar(dest, src, "missing"),
                 "missing scalar")
})

test_that("copy_scalar with default = NULL silently skips missing source", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    copy_scalar(dest, src, "missing", default = NULL)
    expect_false(has_scalar(dest, "missing"))
})

test_that("copy_scalar with explicit default uses it when source missing", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    copy_scalar(dest, src, "missing", default = "fallback")
    expect_identical(get_scalar(dest, "missing"), "fallback")
})

test_that("copy_scalar errors if destination has it and insist=TRUE overwrite=FALSE", {
    src <- memory_daf(name = "src"); set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "dest"); set_scalar(dest, "x", 2L)
    expect_error(copy_scalar(dest, src, "x"), "already exists")
})

test_that("copy_scalar with insist=FALSE silently skips when destination has it", {
    src <- memory_daf(name = "src"); set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "dest"); set_scalar(dest, "x", 2L)
    copy_scalar(dest, src, "x", insist = FALSE)
    expect_identical(get_scalar(dest, "x"), 2L)
})

test_that("copy_scalar with overwrite=TRUE replaces destination value", {
    src <- memory_daf(name = "src"); set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "dest"); set_scalar(dest, "x", 2L)
    copy_scalar(dest, src, "x", overwrite = TRUE)
    expect_identical(get_scalar(dest, "x"), 1L)
})
