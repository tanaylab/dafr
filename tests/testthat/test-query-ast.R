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

test_that("mask AST nodes construct cleanly", {
  expect_equal(.qop_begin_mask("age")$op, "BeginMask")
  expect_equal(.qop_begin_mask("age", negated = TRUE)$op, "BeginNegatedMask")
  expect_equal(.qop_end_mask()$op, "EndMask")
})

test_that("comparator nodes capture operator + value", {
  expect_equal(.qop_is_less(5)$op,           "IsLess")
  expect_equal(.qop_is_less_equal(5)$op,     "IsLessEqual")
  expect_equal(.qop_is_equal("x")$op,        "IsEqual")
  expect_equal(.qop_is_not_equal("x")$op,    "IsNotEqual")
  expect_equal(.qop_is_greater(5)$op,        "IsGreater")
  expect_equal(.qop_is_greater_equal(5)$op,  "IsGreaterEqual")
  expect_equal(.qop_is_match("^a")$op,       "IsMatch")
  expect_equal(.qop_is_not_match("^a")$op,   "IsNotMatch")
  expect_equal(.qop_is_less(5)$value,        5)
  expect_equal(.qop_is_match("^a")$pattern,  "^a")
})

test_that("logical mask nodes construct", {
  expect_equal(.qop_and_mask("x")$op, "AndMask")
  expect_equal(.qop_or_mask("x", negated = TRUE)$op, "OrNegatedMask")
  expect_equal(.qop_xor_mask("x")$op, "XorMask")
})

test_that("square slice nodes construct", {
  expect_equal(.qop_square_row_is("x")$op, "SquareRowIs")
  expect_equal(.qop_square_column_is("x")$op, "SquareColumnIs")
})

test_that("reduction / grouping nodes construct", {
  n <- .qop_reduce_to_column("Sum", params = list(type = "Int64"))
  expect_equal(n$op, "ReduceToColumn")
  expect_equal(n$reduction, "Sum")
  expect_equal(n$params, list(type = "Int64"))
  expect_equal(.qop_group_by("donor")$op, "GroupBy")
  expect_equal(.qop_count_by("age")$op, "CountBy")
})

test_that("eltwise node carries name + params", {
  n <- .qop_eltwise("Log", params = list(eps = 1, base = 2))
  expect_equal(n$op, "Eltwise")
  expect_equal(n$name, "Log")
  expect_equal(n$params, list(eps = 1, base = 2))
})
