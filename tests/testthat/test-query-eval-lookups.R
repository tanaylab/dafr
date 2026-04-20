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

test_that("get_query returns a vector", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_vector(d, "cell", "age", c(1, 2, 3))
  expect_equal(get_query(d, "@ cell : age"), c(1, 2, 3))
})

test_that("get_query with '@ axis : ?' returns vector names", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", "c1")
  set_vector(d, "cell", "age",  1)
  set_vector(d, "cell", "name", "x")
  expect_setequal(get_query(d, "@ cell : ?"), c("age", "name"))
})

test_that("get_query returns a matrix", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2"))
  add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2, dimnames = list(c("c1","c2"), c("g1","g2"))))
  m <- get_query(d, "@ cell @ gene :: UMIs")
  expect_equal(dim(m), c(2L, 2L))
  expect_equal(m[1, 1], 1)
})

test_that("get_query errors on missing vector/matrix with no IfMissing", {
  d <- memory_daf(name = "t"); add_axis(d, "cell", "c1")
  expect_error(get_query(d, "@ cell : nope"), "no vector")
})
