test_that("format_has_scalar / format_get_scalar / format_scalars_set query scalars env", {
  d <- memory_daf()
  expect_false(format_has_scalar(d, "pi"))
  expect_equal(format_scalars_set(d), character(0L))
  d@internal$scalars$pi <- 3.14
  expect_true(format_has_scalar(d, "pi"))
  expect_equal(format_get_scalar(d, "pi"), 3.14)
  expect_equal(format_scalars_set(d), "pi")
})

test_that("format_get_scalar errors on unknown name", {
  d <- memory_daf()
  expect_error(format_get_scalar(d, "pi"), "does not exist")
})

test_that("format_set_scalar stores new scalars and respects overwrite=FALSE", {
  d <- memory_daf()
  format_set_scalar(d, "foo", "bar", overwrite = FALSE)
  expect_equal(format_get_scalar(d, "foo"), "bar")
  expect_error(format_set_scalar(d, "foo", "baz", overwrite = FALSE), "already exists")
  expect_equal(format_get_scalar(d, "foo"), "bar")
})

test_that("format_set_scalar with overwrite=TRUE replaces value", {
  d <- memory_daf()
  format_set_scalar(d, "foo", "bar", overwrite = FALSE)
  format_set_scalar(d, "foo", "baz", overwrite = TRUE)
  expect_equal(format_get_scalar(d, "foo"), "baz")
})

test_that("format_set_scalar rejects NA, NULL, and length != 1", {
  d <- memory_daf()
  expect_error(format_set_scalar(d, "foo", NA,              overwrite = FALSE), "NA")
  expect_error(format_set_scalar(d, "foo", NULL,            overwrite = FALSE), "scalar")
  expect_error(format_set_scalar(d, "foo", c("a", "b"),     overwrite = FALSE), "length 1")
  expect_error(format_set_scalar(d, "foo", list(1),         overwrite = FALSE), "atomic")
})

test_that("format_delete_scalar removes + respects must_exist", {
  d <- memory_daf()
  format_set_scalar(d, "foo", "bar", overwrite = FALSE)
  format_delete_scalar(d, "foo", must_exist = TRUE)
  expect_false(format_has_scalar(d, "foo"))
  expect_error (format_delete_scalar(d, "foo", must_exist = TRUE),  "does not exist")
  expect_silent(format_delete_scalar(d, "foo", must_exist = FALSE))
})

test_that("scalar user-facing round-trip with default handling", {
  d <- memory_daf()
  expect_false(has_scalar(d, "foo"))
  expect_equal(length(scalars_set(d)), 0L)
  expect_error(get_scalar(d, "foo"), "does not exist")
  expect_equal(get_scalar(d, "foo", default = 17), 17)

  set_scalar(d, "foo", "bar")
  expect_true(has_scalar(d, "foo"))
  expect_equal(get_scalar(d, "foo"), "bar")
  expect_equal(scalars_set(d), "foo")

  expect_error(set_scalar(d, "foo", "baz"),               "already exists")
  set_scalar(d, "foo", "baz", overwrite = TRUE)
  expect_equal(get_scalar(d, "foo"), "baz")

  delete_scalar(d, "foo")
  expect_false(has_scalar(d, "foo"))
  expect_error (delete_scalar(d, "foo"),                  "does not exist")
  expect_silent(delete_scalar(d, "foo", must_exist = FALSE))
})

test_that("set_scalar rejects NA (per Julia DAF rules)", {
  d <- memory_daf()
  expect_error(set_scalar(d, "foo", NA))
})

test_that("get_scalar(default = NULL) is distinct from omitting the default", {
  d <- memory_daf()
  expect_error(get_scalar(d, "missing"), "does not exist")
  expect_null(get_scalar(d, "missing", default = NULL))
})
