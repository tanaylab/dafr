test_that("format_has_axis reflects added axes", {
  d <- memory_daf()
  expect_false(format_has_axis(d, "cell"))
  d@internal$axes$cell <- list(
    entries = c("A", "B"),
    dict    = list2env(list(A = 1L, B = 2L), parent = emptyenv())
  )
  expect_true(format_has_axis(d, "cell"))
})

test_that("format_axes_set returns sorted character vector of axis names", {
  d <- memory_daf()
  d@internal$axes$gene <- list(entries = character(), dict = new.env(parent = emptyenv()))
  d@internal$axes$cell <- list(entries = character(), dict = new.env(parent = emptyenv()))
  expect_equal(format_axes_set(d), c("cell", "gene"))
})

test_that("format_axis_length + format_axis_array + format_axis_dict are consistent", {
  d <- memory_daf()
  dict <- list2env(list(A = 1L, B = 2L, C = 3L), parent = emptyenv())
  d@internal$axes$cell <- list(entries = c("A", "B", "C"), dict = dict)
  expect_equal(format_axis_length(d, "cell"), 3L)
  expect_equal(format_axis_array(d, "cell"),  c("A", "B", "C"))
  expect_identical(format_axis_dict(d, "cell"), dict)
})

test_that("format_axis_* reject unknown axis", {
  d <- memory_daf()
  expect_error(format_axis_length(d, "cell"), "does not exist")
  expect_error(format_axis_array(d, "cell"),  "does not exist")
  expect_error(format_axis_dict(d, "cell"),   "does not exist")
})
