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

test_that("v3 dense vector reads with correct values", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expect_equal(unname(get_vector(d, "cell", "score")), c(1.5, 2.5, 3.5))
})

test_that("v3 dense + sparse matrix read with correct orientation", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expr <- get_matrix(d, "cell", "gene", "expr")
  expect_equal(dim(expr), c(3L, 2L))
  expect_equal(unname(as.matrix(expr)),
               matrix(c(1, 3, 5, 2, 4, 6), nrow = 3))   # cols [1,3,5],[2,4,6]
  sp <- get_matrix(d, "cell", "gene", "sp")
  expect_equal(dim(sp), c(3L, 2L))
  expect_equal(unname(as.matrix(sp)),
               matrix(c(0, 0, 5, 2, 0, 0), nrow = 3))
})

test_that("v3 flat dense float64 vector reads via the mmap ALTREP fast path", {
  withr::local_options(list(dafr.mmap = TRUE))
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  v <- get_vector(d, "cell", "score")
  expect_true(is_altrep_cpp(v) || isTRUE(all.equal(unname(v), c(1.5, 2.5, 3.5))))
})

test_that("v3 flat fixture reads end-to-end", {
  d <- zarr_daf(daf030_flat_fixture(), mode = "r")
  expect_setequal(scalars_set(d), c("title", "n"))
  expect_setequal(axes_set(d), c("cell", "gene"))
  expect_equal(get_scalar(d, "title"), "hello")
  expect_equal(unname(get_vector(d, "cell", "score")), c(1.5, 2.5, 3.5))
  expect_equal(dim(get_matrix(d, "cell", "gene", "expr")), c(3L, 2L))
  expect_equal(dim(get_matrix(d, "cell", "gene", "sp")), c(3L, 2L))
})
