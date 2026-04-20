test_that(".qop_axis constructs an Axis node", {
  n <- .qop_axis("cell")
  expect_s3_class(n, "qop_Axis")
  expect_equal(n$op, "Axis")
  expect_equal(n$axis_name, "cell")
})

test_that(".qop_lookup_scalar / _vector / _matrix construct lookup nodes", {
  expect_equal(.qop_lookup_scalar("organism")$op,  "LookupScalar")
  expect_equal(.qop_lookup_vector("UMIs")$op,      "LookupVector")
  expect_equal(.qop_lookup_matrix("UMIs")$op,      "LookupMatrix")
})

test_that(".qop_names constructs a Names node (no args)", {
  expect_equal(.qop_names()$op, "Names")
})

test_that(".qop_if_missing captures default value", {
  n <- .qop_if_missing(0)
  expect_equal(n$op, "IfMissing")
  expect_identical(n$default, 0)
})

test_that("canonicalise_ast emits the canonical string", {
  ast <- list(.qop_axis("cell"), .qop_lookup_vector("UMIs"))
  expect_equal(.canonicalise_ast(ast), "@ cell : UMIs")
})
