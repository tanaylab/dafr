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
