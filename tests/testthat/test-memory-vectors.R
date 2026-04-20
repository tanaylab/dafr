test_that("format_has_vector / format_vectors_set reflect stored vectors", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_false(format_has_vector(d, "cell", "score"))
  expect_equal(format_vectors_set(d, "cell"), character(0L))
  vectors <- S7::prop(d, "internal")$vectors
  vectors$cell <- new.env(parent = emptyenv())
  vectors$cell$score <- c(1.0, 2.0)
  expect_true(format_has_vector(d, "cell", "score"))
  expect_equal(format_vectors_set(d, "cell"), "score")
})

test_that("format_vectors_set errors on unknown axis", {
  d <- memory_daf()
  expect_error(format_vectors_set(d, "cell"), "does not exist")
})

test_that("format_get_vector returns the stored SEXP unchanged", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  vectors <- S7::prop(d, "internal")$vectors
  vectors$cell <- new.env(parent = emptyenv())
  vectors$cell$score <- c(1.5, 2.5)
  expect_equal(format_get_vector(d, "cell", "score"), c(1.5, 2.5))
})

test_that("format_get_vector errors on unknown axis / vector", {
  d <- memory_daf()
  expect_error(format_get_vector(d, "cell", "score"), "axis .* does not exist")
  add_axis(d, "cell", c("A", "B"))
  expect_error(format_get_vector(d, "cell", "score"), "vector .* does not exist")
})
