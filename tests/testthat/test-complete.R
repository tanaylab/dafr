test_that("open_daf opens a FilesDaf directory in read mode", {
    tmp <- withr::local_tempdir()
    f <- files_daf(tmp, name = "orig", mode = "w+")
    add_axis(f, "cell", c("c1", "c2"))
    set_vector(f, "cell", "age", c(1L, 2L))

    d <- open_daf(tmp, "r")
    expect_true(S7::S7_inherits(d, DafReadOnly) || S7::S7_inherits(d, DafReader))
    expect_identical(unname(get_vector(d, "cell", "age")), c(1L, 2L))
})

test_that("open_daf opens a FilesDaf directory in r+ mode", {
    tmp <- withr::local_tempdir()
    f <- files_daf(tmp, name = "orig", mode = "w+")
    add_axis(f, "cell", c("c1"))

    d <- open_daf(tmp, "r+")
    set_vector(d, "cell", "tag", c("x"))
    expect_identical(unname(get_vector(d, "cell", "tag")), c("x"))
})

test_that("open_daf rejects H5df paths", {
    expect_error(open_daf("x.h5df", "r"), "H5df backend not supported yet")
    expect_error(open_daf("x.h5dfs#/grp", "r"), "H5df backend not supported yet")
})

test_that("complete_chain sets base_daf_repository and returns a write chain", {
    tmp_base <- withr::local_tempdir()
    tmp_new <- withr::local_tempdir()
    base <- files_daf(tmp_base, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1", "c2"))
    set_vector(base, "cell", "age", c(1L, 2L))

    new <- files_daf(tmp_new, name = "new", mode = "w+")
    chain <- complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    expect_true(format_has_scalar(new, "base_daf_repository"))
    expect_identical(format_get_scalar(new, "base_daf_repository")$value,
                     normalizePath(tmp_base))
    expect_identical(unname(get_vector(chain, "cell", "age")), c(1L, 2L))
    set_vector(chain, "cell", "tag", c("x", "y"))
    expect_true(has_vector(new, "cell", "tag"))
})

test_that("complete_daf reopens a chain that complete_chain persisted", {
    tmp_root <- withr::local_tempdir()
    base_dir <- file.path(tmp_root, "base")
    new_dir <- file.path(tmp_root, "new")
    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1", "c2"))
    set_vector(base, "cell", "age", c(10L, 20L))
    new <- files_daf(new_dir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    chain <- complete_daf(new_dir, mode = "r")
    expect_identical(unname(get_vector(chain, "cell", "age")), c(10L, 20L))
})

test_that("complete_daf in r+ mode allows writes to leaf", {
    tmp_root <- withr::local_tempdir()
    base_dir <- file.path(tmp_root, "base")
    new_dir <- file.path(tmp_root, "new")
    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1"))
    new <- files_daf(new_dir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    chain <- complete_daf(new_dir, mode = "r+")
    set_vector(chain, "cell", "tag", c("t1"))
    leaf_reopen <- open_daf(new_dir, "r")
    expect_true(has_vector(leaf_reopen, "cell", "tag"))
})

test_that("complete_daf rejects invalid mode", {
    tmp <- withr::local_tempdir()
    files_daf(tmp, name = "t", mode = "w+")
    expect_error(complete_daf(tmp, "w"), "must be")
})

test_that(".is_absolute_path recognises unix, windows and UNC paths", {
    f <- dafr:::.is_absolute_path
    expect_true(f("/tmp/foo"))
    expect_true(f("C:/tmp/foo"))
    expect_true(f("c:/tmp/foo"))
    expect_true(f("C:\\tmp\\foo"))
    expect_true(f("\\\\server\\share"))
    expect_false(f("relative/path"))
    expect_false(f("foo"))
    expect_false(f("."))
    expect_false(f("./x"))
})
