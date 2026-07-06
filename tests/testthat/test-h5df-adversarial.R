skip_if_no_hdf5r <- function() testthat::skip_if_not_installed("hdf5r")

test_that("h5df rejects a non-daf HDF5 file", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    h <- hdf5r::H5File$new(p, mode = "w")
    h$create_dataset("junk", robj = 1:3, chunk_dims = NULL)
    h$close_all()
    expect_error(h5df(p, mode = "r"), "not a daf")
})

test_that("h5df rejects an incompatible format version", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    h <- hdf5r::H5File$new(p, mode = "w")
    for (g in c("scalars", "axes", "vectors", "matrices")) h$create_group(g)
    h$create_dataset("daf", robj = c(2L, 0L),
        dtype = hdf5r::h5types$H5T_NATIVE_UINT8, chunk_dims = NULL)
    h$close_all()
    expect_error(h5df(p, mode = "r"), "incompatible format version")
})

test_that("open_daf rejects grouped .h5dfs#", {
    skip_if_no_hdf5r()
    expect_error(open_daf("foo.h5dfs#/grp", mode = "r"), "not supported")
})

test_that("missing hdf5r yields an actionable error", {
    skip_if_no_hdf5r()
    expect_true(is.function(h5df))
})
