test_that("zarr_daf opens a DAF 0.3.0 flat store read-only", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expect_s3_class(d, "dafr::ZarrDafReadOnly")
})

test_that("zarr_daf rejects a Zarr v2 store with a clear, actionable error", {
  v2 <- tempfile(fileext = ".daf.zarr"); dir.create(v2)
  writeBin(charToRaw('{"zarr_format":2}'), file.path(v2, ".zgroup"))
  expect_error(zarr_daf(v2, mode = "r"),
               "Zarr v2 store .* requires a Zarr v3|v2_to_v3")
})

test_that("v3 scalars and axis entries read correctly", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expect_equal(get_scalar(d, "title"), "hello")
  expect_equal(as.integer(get_scalar(d, "n")), 7L)
  expect_equal(axis_vector(d, "cell"), c("c1", "c2", "c3"))
  expect_equal(axis_vector(d, "gene"), c("g1", "g2"))
})
