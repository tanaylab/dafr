test_that("kernel_log_add_cpp accepts a threshold argument", {
    args <- formals(dafr:::kernel_log_add_cpp)
    expect_true("threshold" %in% names(args))
})

test_that("kernel_csc_colsums_cpp accepts a threshold argument", {
    args <- formals(dafr:::kernel_csc_colsums_cpp)
    expect_true("threshold" %in% names(args))
})

test_that("dafr.omp_threshold default propagates through dafr_opt()", {
    expect_identical(dafr:::dafr_opt("dafr.omp_threshold"), 10000L)
    withr::local_options(dafr.omp_threshold = 1L)
    expect_identical(dafr:::dafr_opt("dafr.omp_threshold"), 1L)
})
