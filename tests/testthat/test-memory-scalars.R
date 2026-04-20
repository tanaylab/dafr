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
