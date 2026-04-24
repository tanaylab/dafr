test_that("set_num_threads changes get_num_threads result", {
    skip_if_not(dafr:::dafr_has_openmp(),
                "OpenMP not available in this build")
    old <- get_num_threads()
    on.exit(set_num_threads(old), add = TRUE)

    set_num_threads(1L)
    expect_identical(get_num_threads(), 1L)

    set_num_threads(2L)
    expect_identical(get_num_threads(), 2L)
})

test_that("set_num_threads clamps values below 1 to 1", {
    skip_if_not(dafr:::dafr_has_openmp(),
                "OpenMP not available in this build")
    old <- get_num_threads()
    on.exit(set_num_threads(old), add = TRUE)

    set_num_threads(0L)
    expect_identical(get_num_threads(), 1L)

    set_num_threads(-5L)
    expect_identical(get_num_threads(), 1L)
})

test_that("set_num_threads persists as dafr.num_threads option", {
    old <- get_num_threads()
    old_opt <- getOption("dafr.num_threads")
    on.exit({
        set_num_threads(old)
        options(dafr.num_threads = old_opt)
    }, add = TRUE)

    set_num_threads(3L)
    expect_identical(getOption("dafr.num_threads"), 3L)
})

test_that("set_num_threads errors on non-numeric input", {
    expect_error(set_num_threads("two"))
    expect_error(set_num_threads(NA_integer_))
    expect_error(set_num_threads(c(1L, 2L)))
})

test_that("get_num_threads is 1L on builds without OpenMP", {
    skip_if(dafr:::dafr_has_openmp(),
            "OpenMP is available; this test is for no-OpenMP builds only")
    expect_identical(get_num_threads(), 1L)
    # set_num_threads is a no-op without OpenMP.
    set_num_threads(4L)
    expect_identical(get_num_threads(), 1L)
})
