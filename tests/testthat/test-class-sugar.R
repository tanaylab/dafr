test_that("is_daf recognises every daf subclass and rejects non-dafs", {
    d_mem <- memory_daf(name = "m")
    expect_true(is_daf(d_mem))
    overlay <- memory_daf(name = "o")
    d_chain <- chain_reader(list(d_mem, overlay))
    expect_true(is_daf(d_chain))
    expect_false(is_daf(NULL))
    expect_false(is_daf(list()))
    expect_false(is_daf(42L))
    expect_false(is_daf("memory_daf"))
})

test_that("daf_name returns the name property", {
    d <- memory_daf(name = "hello")
    expect_identical(daf_name(d), "hello")
})

test_that("daf_name errors on non-daf input", {
    expect_error(daf_name(NULL), "DafReader")
    expect_error(daf_name(42L), "DafReader")
})

test_that("complete_path matches the internal .complete_path", {
    tmp <- tempfile("dafr-10c-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    fd <- files_daf(tmp, mode = "w+", name = "fd")
    expect_identical(complete_path(fd), dafr:::.complete_path(fd))
})

test_that("read_only wraps a writer into a read-only chain", {
    d <- memory_daf(name = "inner")
    add_axis(d, "cell", c("c1", "c2"))
    set_scalar(d, "x", "y")
    ro <- read_only(d)
    expect_true(is_daf(ro))
    expect_s3_class(ro, "dafr::DafReadOnly")
    expect_identical(daf_name(ro), "inner")
    expect_identical(get_scalar(ro, "x"), "y")
    expect_error(set_scalar(ro, "z", "w"))
})

test_that("read_only accepts an explicit name override", {
    d <- memory_daf(name = "inner")
    ro <- read_only(d, name = "outer")
    expect_identical(daf_name(ro), "outer")
})

test_that("read_only errors on non-daf input", {
    expect_error(read_only(NULL), "DafReader")
})
