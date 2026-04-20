test_that("get_frame with default columns returns all vectors for axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2"))
  set_vector(d, "donor", "age", c(30, 60))
  set_vector(d, "donor", "sex", c("M", "F"))
  frame <- get_frame(d, "@ donor")
  expect_s3_class(frame, "data.frame")
  expect_setequal(names(frame), c("age", "sex"))
  expect_equal(nrow(frame), 2L)
})

test_that("get_frame with filtered axis respects mask", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3"))
  set_vector(d, "donor", "age", c(10, 70, 90))
  frame <- get_frame(d, "@ donor [ age > 50 ]", columns = "age")
  expect_equal(frame$age, c(70, 90))
  expect_equal(rownames(frame), c("d2", "d3"))
})

test_that("get_frame with explicit columns list preserves order", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", "d1")
  set_vector(d, "donor", "x", 1); set_vector(d, "donor", "y", 2)
  frame <- get_frame(d, "@ donor", columns = c("y", "x"))
  expect_equal(names(frame), c("y", "x"))
})
