test_that("register_reduction stores a function retrievable by name", {
  f <- function(x, ...) sum(x)
  register_reduction("TestSum", f)
  expect_identical(get_reduction("TestSum"), f)
  expect_true("TestSum" %in% registered_reductions())
})

test_that("register_eltwise stores a function retrievable by name", {
  f <- function(x, ...) x + 1
  register_eltwise("TestAdd1", f)
  expect_identical(get_eltwise("TestAdd1"), f)
  expect_true("TestAdd1" %in% registered_eltwise())
})

test_that("get_reduction raises for unknown name", {
  expect_error(get_reduction("NoSuchOp"), "unknown reduction operation")
})

test_that("get_eltwise raises for unknown name", {
  expect_error(get_eltwise("NoSuchOp"), "unknown eltwise operation")
})
