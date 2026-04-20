test_that("ViewDaf is a subclass of DafReadOnly", {
  expect_true(S7::S7_inherits(viewer(memory_daf("tmp")), DafReadOnly))
})

test_that("ViewDaf can be constructed with just a base daf", {
  d <- memory_daf(name = "base")
  v <- viewer(d, name = "v")
  expect_s7_class(v, ViewDaf)
  expect_equal(S7::prop(v, "name"), "v")
})

# The "no-override mirrors base" test lives in V2, which registers the
# format_* S7 methods that make ViewDaf actually dispatch reads.

test_that("ViewDaf format_has_scalar / format_get_scalar delegate to base", {
  d <- memory_daf(name = "base"); set_scalar(d, "organism", "human")
  v <- viewer(d, name = "v")
  expect_true(format_has_scalar(v, "organism"))
  expect_equal(format_get_scalar(v, "organism"), "human")
  expect_false(format_has_scalar(v, "nope"))
})

test_that("ViewDaf format_has_axis / format_axis_array delegate to base", {
  d <- memory_daf(name = "base"); add_axis(d, "cell", c("c1","c2"))
  v <- viewer(d, name = "v")
  expect_true(format_has_axis(v, "cell"))
  expect_equal(format_axis_array(v, "cell"), c("c1","c2"))
})

test_that("ViewDaf format_get_vector delegates via query", {
  d <- memory_daf(name = "base")
  add_axis(d, "cell", c("c1","c2"))
  set_vector(d, "cell", "age", c(10, 20))
  v <- viewer(d, name = "v")
  expect_equal(format_get_vector(v, "cell", "age"), c(10, 20))
})

test_that("ViewDaf with no overrides mirrors base daf (smoke)", {
  d <- memory_daf(name = "base")
  set_scalar(d, "organism", "human")
  add_axis(d, "cell", c("c1", "c2"))
  set_vector(d, "cell", "age", c(10, 20))
  v <- viewer(d, name = "v")
  expect_equal(get_scalar(v, "organism"), "human")
  expect_equal(axis_vector(v, "cell"), c("c1", "c2"))
  expect_equal(unname(get_vector(v, "cell", "age")), c(10, 20))
})

test_that("viewer with axis override renames axis", {
  d <- memory_daf(name = "base"); add_axis(d, "cell", c("c1","c2"))
  v <- viewer(d, axes = list(list("obs", "@ cell")))
  expect_equal(axes_set(v), "obs")
  expect_equal(axis_vector(v, "obs"), c("c1","c2"))
})

test_that("viewer with filtered axis query", {
  d <- memory_daf(name = "base")
  add_axis(d, "cell", c("c1","c2","c3"))
  set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE))
  v <- viewer(d, axes = list(list("cell", "@ cell [ keep ]")))
  expect_equal(axis_vector(v, "cell"), c("c1","c3"))
})

test_that("viewer with vector override", {
  d <- memory_daf(name = "base")
  add_axis(d, "cell", c("c1","c2"))
  set_vector(d, "cell", "UMIs", c(1, 10))
  v <- viewer(d, data = list(
    list(c("cell", "log_UMIs"), "@ cell : UMIs % Log eps: 1")))
  expect_equal(unname(get_vector(v, "cell", "log_UMIs")), c(log(2), log(11)))
})

test_that("viewer with scalar override", {
  d <- memory_daf(name = "base")
  set_scalar(d, "organism", "human")
  v <- viewer(d, data = list(list("species", ". organism")))
  expect_equal(get_scalar(v, "species"), "human")
})
