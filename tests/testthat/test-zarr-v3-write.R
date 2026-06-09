# v3 flat WRITE paths: write via zarr_daf(dir, "w"), read back via
# zarr_daf(dir, "r"). On-disk-shape assertions pin the v3 layout
# (zarr.json + c/-prefixed chunk keys, reversed dense-matrix shape).

test_that("v3 scalar + axis write then read back in-process", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_scalar(d, "title", "hello")
  set_scalar(d, "n", 7L)
  d2 <- zarr_daf(dir, mode = "r")
  expect_equal(get_scalar(d2, "title"), "hello")
  expect_equal(as.integer(get_scalar(d2, "n")), 7L)
  expect_equal(axis_vector(d2, "cell"), c("c1", "c2", "c3"))
  # on-disk shape is the v3 layout
  expect_true(file.exists(file.path(dir, "scalars", "n", "zarr.json")))
  expect_true(file.exists(file.path(dir, "scalars", "n", "c", "0")))
})

test_that("v3 dense + sparse vector write round-trips in-process", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_vector(d, "cell", "score", c(1.5, 2.5, 3.5))
  set_vector(d, "cell", "sparse_v", Matrix::sparseVector(c(4, 9), c(1, 3), 3))
  d2 <- zarr_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "score")), c(1.5, 2.5, 3.5))
  expect_equal(unname(as.numeric(get_vector(d2, "cell", "sparse_v"))),
               c(4, 0, 9))
  # sparse vector is a v3 group; nzind is int64
  expect_true(file.exists(file.path(dir, "vectors", "cell", "sparse_v",
                                    "zarr.json")))
  expect_true(file.exists(file.path(dir, "vectors", "cell", "sparse_v",
                                    "nzind", "zarr.json")))
  nzind_meta <- jsonlite::fromJSON(file.path(dir, "vectors", "cell",
                                             "sparse_v", "nzind", "zarr.json"))
  expect_equal(nzind_meta$data_type, "int64")
})
