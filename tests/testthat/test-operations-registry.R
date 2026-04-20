# Helper: remove any test-created names from the shared ops env so tests
# do not pollute each other.  Call withr::defer(.clear_test_ops()) inside
# each test_that block that registers a name.
.clear_test_ops <- function() {
  test_names <- c("TestSum", "TestAdd1", "DupTest", "OverTest")
  for (n in test_names) {
    .ops_env$reductions[[n]] <- NULL
    .ops_env$eltwise[[n]]    <- NULL
  }
}

test_that("register_reduction stores a function retrievable by name", {
  withr::defer(.clear_test_ops())
  f <- function(x, ...) sum(x)
  register_reduction("TestSum", f)
  expect_identical(get_reduction("TestSum"), f)
  expect_true("TestSum" %in% registered_reductions())
})

test_that("register_eltwise stores a function retrievable by name", {
  withr::defer(.clear_test_ops())
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

test_that("register_reduction errors on non-function fn", {
  expect_error(register_reduction("x", "not a function"),
               "must be a function")
})

test_that("register_eltwise errors on non-function fn", {
  expect_error(register_eltwise("x", list(a = 1)),
               "must be a function")
})

test_that("register_reduction rejects names with forbidden chars", {
  expect_error(register_reduction("a/b", identity), "forbidden")
})

test_that("register_reduction errors on collision without overwrite", {
  withr::defer(.clear_test_ops())
  register_reduction("DupTest", function(x, ...) sum(x))
  expect_error(register_reduction("DupTest", function(x, ...) mean(x)),
               "already registered")
})

test_that("register_reduction accepts overwrite = TRUE", {
  withr::defer(.clear_test_ops())
  register_reduction("OverTest", function(x, ...) sum(x))
  register_reduction("OverTest", function(x, ...) 0, overwrite = TRUE)
  expect_equal(get_reduction("OverTest")(c(1, 2)), 0)
})
