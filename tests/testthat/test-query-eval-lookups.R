test_that("get_query returns scalar values", {
  d <- memory_daf(name = "t"); set_scalar(d, "organism", "human")
  expect_equal(get_query(d, ". organism"), "human")
})

test_that("get_query returns axis entry vector", {
  d <- memory_daf(name = "t"); add_axis(d, "cell", c("c1", "c2", "c3"))
  expect_equal(get_query(d, "@ cell"), c("c1", "c2", "c3"))
})

test_that("get_query with '?' returns scalar names", {
  d <- memory_daf(name = "t")
  set_scalar(d, "organism", "human"); set_scalar(d, "reference", "test")
  expect_setequal(get_query(d, ". ?"), c("organism", "reference"))
})

test_that("get_query with '? @' returns axis names", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", "c1"); add_axis(d, "gene", "g1")
  expect_setequal(get_query(d, "@ ?"), c("cell", "gene"))
})

test_that("get_query errors on missing scalar unless IfMissing", {
  d <- memory_daf(name = "t")
  expect_error(get_query(d, ". missing"), "no scalar")
  expect_equal(get_query(d, ". missing || 0"), "0")
})
