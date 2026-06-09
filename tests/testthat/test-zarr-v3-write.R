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
