test_that("FilesDaf scalar round-trip", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    set_scalar(d, "pi", 3.14)
    set_scalar(d, "cells", 100L)
    set_scalar(d, "is_ok", TRUE)
    set_scalar(d, "label", "batch_A")

    d2 <- files_daf(dir, mode = "r")
    expect_true(has_scalar(d2, "pi"))
    expect_equal(get_scalar(d2, "pi"), 3.14)
    expect_equal(get_scalar(d2, "cells"), 100L)
    expect_equal(get_scalar(d2, "is_ok"), TRUE)
    expect_equal(get_scalar(d2, "label"), "batch_A")
    expect_equal(
        scalars_set(d2),
        sort(c("pi", "cells", "is_ok", "label"), method = "radix")
    )
})

test_that("FilesDaf set_scalar overwrite behaviour matches MemoryDaf", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    set_scalar(d, "x", 1)
    expect_error(set_scalar(d, "x", 2), "existing scalar:")
    set_scalar(d, "x", 2, overwrite = TRUE)
    expect_equal(get_scalar(d, "x"), 2)
})

test_that("FilesDaf delete_scalar removes the file", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    set_scalar(d, "x", 1)
    expect_true(file.exists(file.path(dir, "scalars", "x.json")))
    delete_scalar(d, "x")
    expect_false(file.exists(file.path(dir, "scalars", "x.json")))
    expect_error(get_scalar(d, "x"), "missing scalar:")
    expect_silent(delete_scalar(d, "x", must_exist = FALSE))
    expect_error(delete_scalar(d, "x"), "missing scalar:")
})
