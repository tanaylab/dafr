test_that("mask with '>' comparator filters axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
  set_vector(d, "donor", "age", c(10, 50, 70, 90))
  expect_equal(get_query(d, "@ donor [ age > 60 ]"), c("d3", "d4"))
})

test_that("mask with '=' comparator filters axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3"))
  set_vector(d, "donor", "sex", c("M", "F", "M"))
  expect_equal(get_query(d, "@ donor [ sex = M ]"), c("d1", "d3"))
})

test_that("negated mask filters axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "gene", c("g1", "g2", "g3"))
  set_vector(d, "gene", "is_lateral", c(TRUE, FALSE, TRUE))
  expect_equal(get_query(d, "@ gene [ ! is_lateral ]"), "g2")
})

test_that("mask with '~' regex match filters axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "gene", c("HOX1", "MYC", "HOX2"))
  set_vector(d, "gene", "symbol", c("HOX1", "MYC", "HOX2"))
  expect_equal(get_query(d, "@ gene [ symbol ~ ^HOX ]"), c("HOX1", "HOX2"))
})
