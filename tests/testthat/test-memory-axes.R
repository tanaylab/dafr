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

test_that("format_add_axis stores entries + builds a 1-based index dict", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A", "B", "C"))
  expect_equal(format_axis_array(d, "cell"),  c("A", "B", "C"))
  expect_equal(format_axis_length(d, "cell"), 3L)
  dict <- format_axis_dict(d, "cell")
  expect_equal(dict$A, 1L)
  expect_equal(dict$B, 2L)
  expect_equal(dict$C, 3L)
})

test_that("format_add_axis bumps the axis version counter", {
  d <- memory_daf()
  counters <- S7::prop(d, "axis_version_counter")
  expect_null(counters$cell)
  format_add_axis(d, "cell", c("A"))
  expect_equal(counters$cell, 1L)
})

test_that("format_add_axis rejects duplicate axis", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A"))
  expect_error(format_add_axis(d, "cell", c("A")), "already exists")
})

test_that("format_add_axis rejects duplicate / NA / empty entries", {
  d <- memory_daf()
  expect_error(format_add_axis(d, "cell", c("A", "A")),    "duplicate")
  expect_error(format_add_axis(d, "cell", c("A", NA)),     "NA")
  expect_error(format_add_axis(d, "cell", c("A", "")),     "empty")
  expect_error(format_add_axis(d, "cell", integer(0)),     "find method")  # S7 dispatch rejects non-character before reaching the guard
})

test_that("format_delete_axis removes axis + bumps counter", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A"))
  counters <- S7::prop(d, "axis_version_counter")
  stamp <- counters$cell
  format_delete_axis(d, "cell", must_exist = TRUE)
  expect_false(format_has_axis(d, "cell"))
  expect_gt(counters$cell, stamp)
})

test_that("format_delete_axis with must_exist=FALSE ignores missing", {
  d <- memory_daf()
  expect_silent(format_delete_axis(d, "cell", must_exist = FALSE))
})

test_that("format_delete_axis with must_exist=TRUE errors on missing", {
  d <- memory_daf()
  expect_error(format_delete_axis(d, "cell", must_exist = TRUE), "does not exist")
})

test_that("format_delete_axis also removes vectors/matrices on that axis", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A", "B"))
  format_add_axis(d, "gene", c("X", "Y"))
  vectors <- S7::prop(d, "internal")$vectors
  vectors$cell <- new.env(parent = emptyenv())
  vectors$cell$score <- c(1.0, 2.0)
  matrices <- S7::prop(d, "internal")$matrices
  matrices$cell <- new.env(parent = emptyenv())
  matrices$cell$gene <- new.env(parent = emptyenv())
  matrices$cell$gene$UMIs <- matrix(0, 2, 2)
  format_delete_axis(d, "cell", must_exist = TRUE)
  expect_false(exists("cell", envir = vectors, inherits = FALSE))
  expect_false(exists("cell", envir = matrices, inherits = FALSE))
})

test_that("format_delete_axis also removes matrices where the axis is cols-only (never rows)", {
  d <- memory_daf()
  format_add_axis(d, "cell", c("A", "B"))
  format_add_axis(d, "gene", c("X", "Y"))
  matrices <- S7::prop(d, "internal")$matrices
  matrices$gene <- new.env(parent = emptyenv())
  matrices$gene$cell <- new.env(parent = emptyenv())
  matrices$gene$cell$UMIs <- matrix(0, 2, 2)
  # "cell" appears only as cols_axis under matrices$gene$cell — never as a rows_axis key.
  expect_false(exists("cell", envir = matrices, inherits = FALSE))
  format_delete_axis(d, "cell", must_exist = TRUE)
  expect_false(exists("cell", envir = matrices$gene, inherits = FALSE))
})

# ---- User-facing axis API ---------------------------------------------------

test_that("add_axis + has_axis + axes_set compose", {
  d <- memory_daf()
  expect_false(has_axis(d, "cell"))
  add_axis(d, "cell", c("A", "B"))
  expect_true(has_axis(d, "cell"))
  expect_equal(axes_set(d), "cell")
})

test_that("axis_length / axis_vector / axis_entries mirror Julia semantics", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  expect_equal(axis_length(d, "cell"), 3L)
  expect_equal(axis_vector(d, "cell"), c("A", "B", "C"))
  expect_equal(axis_entries(d, "cell"), c("A", "B", "C"))
  expect_equal(axis_entries(d, "cell", 2L), "B")
  expect_equal(axis_entries(d, "cell", c(1L, 3L)), c("A", "C"))
  expect_error(axis_entries(d, "cell", "A"),  "integer")
  expect_error(axis_entries(d, "cell", 5L),   "out of range")
  expect_error(axis_entries(d, "cell", -1L),  "out of range")
})

test_that("axis_vector default handling", {
  d <- memory_daf()
  expect_error(axis_vector(d, "cell"), "does not exist")
  expect_null(axis_vector(d, "cell", null_if_missing = TRUE))
})

test_that("axis_indices maps entries to 1-based positions", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  expect_equal(axis_indices(d, "cell", c("A", "C")), c(1L, 3L))
  expect_equal(axis_indices(d, "cell", "B"),         2L)
  expect_error(axis_indices(d, "cell", c(1L, 2L)),   "character")
  expect_error(axis_indices(d, "cell", c("A", "Z")), "not found")
})

test_that("axis_dict is queryable by [[", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  dd <- axis_dict(d, "cell")
  expect_equal(dd[["A"]], 1L)
  expect_equal(dd[["B"]], 2L)
})

test_that("delete_axis composes with axes_set", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  delete_axis(d, "cell")
  expect_equal(length(axes_set(d)), 0L)
  expect_error(delete_axis(d, "cell"),                          "does not exist")
  expect_silent(delete_axis(d, "cell", must_exist = FALSE))
})

test_that("axis_entries rejects NA in indices", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(axis_entries(d, "cell", c(1L, NA_integer_)), "must not contain NA")
})

test_that("axis_indices rejects NA in entries", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(axis_indices(d, "cell", c("A", NA_character_)), "must not contain NA")
})

test_that("axis_indices rejects non-scalar axis", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  expect_error(axis_indices(d, c("cell", "gene"), "A"), "must be a non-NA character scalar")
})

test_that("axis_entries rejects non-integer-valued numeric", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(axis_entries(d, "cell", 1.5), "integer-valued")
})
