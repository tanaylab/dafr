test_that("% Log applies logarithm to vector", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_vector(d, "cell", "UMIs", c(1, 10, 100))
  v <- get_query(d, "@ cell : UMIs % Log base: 10")
  expect_equal(unname(v), c(0, 1, 2))
})

test_that("% Log respects eps", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", "c1"); set_vector(d, "cell", "UMIs", 0)
  expect_equal(unname(get_query(d, "@ cell : UMIs % Log eps: 1")), 0)
})

test_that("% Abs + Sqrt + Exp + Round chains preserve order", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); set_vector(d, "cell", "x", c(-4, 9))
  expect_equal(unname(get_query(d, "@ cell : x % Abs")),   c(4, 9))
  expect_equal(unname(get_query(d, "@ cell : x % Abs % Sqrt")), c(2, 3))
})
