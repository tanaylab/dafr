skip_if_no_hdf5r <- function() testthat::skip_if_not_installed("hdf5r")

test_that("h5df creates a store, marks it, and reopens read-only", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    expect_s3_class(d, "dafr::H5df")
    expect_true(file.exists(p))
    rm(d)
    gc()
    r <- h5df(p, mode = "r")
    expect_s3_class(r, "dafr::H5dfReadOnly")
    expect_equal(dafr:::.is_leaf_dispatch(r), TRUE)
    rm(r)
    gc()
})

test_that("h5df mode guards", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    expect_error(h5df(p, mode = "r"), "not a daf")
    d <- h5df(p, mode = "w"); rm(d); gc()
    expect_error(h5df(p, mode = "w"), "already a daf")   # use w+
    d2 <- h5df(p, mode = "w+"); rm(d2); gc()             # append ok
})

test_that("open_daf dispatches .h5df and rejects grouped", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- open_daf(p, mode = "w")
    expect_s3_class(d, "dafr::H5df")
    rm(d); gc()
    expect_error(open_daf("x.h5dfs#/g", mode = "r"), "not supported")
})
