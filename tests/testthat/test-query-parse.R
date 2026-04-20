test_that("parse_query handles empty string -> empty AST", {
  expect_equal(parse_query(""), list())
})

test_that("parse_query handles axis lookup", {
  ast <- parse_query("@ cell")
  expect_length(ast, 1L)
  expect_equal(ast[[1]]$op, "Axis")
  expect_equal(ast[[1]]$axis_name, "cell")
})

test_that("parse_query handles axis + vector lookup", {
  ast <- parse_query("@ cell : UMIs")
  expect_equal(vapply(ast, `[[`, "", "op"), c("Axis", "LookupVector"))
  expect_equal(ast[[2]]$name, "UMIs")
})

test_that("parse_query handles matrix lookup", {
  ast <- parse_query("@ cell @ gene :: UMIs")
  expect_equal(vapply(ast, `[[`, "", "op"),
               c("Axis", "Axis", "LookupMatrix"))
})

test_that("parse_query handles scalar lookup", {
  ast <- parse_query(". organism")
  expect_equal(ast[[1]]$op, "LookupScalar")
  expect_equal(ast[[1]]$name, "organism")
})

test_that("parse_query handles Names query (?)", {
  expect_equal(parse_query("?")[[1]]$op, "Names")
})

test_that("parse_query round-trips via canonicalise", {
  for (s in c("@ cell", "@ cell : UMIs", "@ cell @ gene :: UMIs",
              ". organism", "?")) {
    expect_equal(.canonicalise_ast(parse_query(s)), s, info = s)
  }
})

test_that("parse_query reports token position on error", {
  expect_error(parse_query("@ @"), "position 3")
})

test_that("parse_query handles bracketed mask with comparator", {
  ast <- parse_query("@ donor [ age > 60 ]")
  ops <- vapply(ast, `[[`, "", "op")
  expect_equal(ops, c("Axis", "BeginMask", "IsGreater", "EndMask"))
  expect_equal(ast[[2]]$property, "age")
  expect_equal(ast[[3]]$value,    "60")
})

test_that("parse_query handles negated mask", {
  ast <- parse_query("@ gene [ ! is_lateral ]")
  ops <- vapply(ast, `[[`, "", "op")
  expect_equal(ops, c("Axis", "BeginNegatedMask", "EndMask"))
})

test_that("parse_query handles AND / OR / XOR mask combinators", {
  ast <- parse_query("@ donor [ age > 60 & sex = male ]")
  ops <- vapply(ast, `[[`, "", "op")
  expect_equal(ops, c("Axis", "BeginMask", "IsGreater",
                       "AndMask", "IsEqual", "EndMask"))
})

test_that("parse_query handles IsMatch operator", {
  ast <- parse_query("@ donor [ name ~ ^a ]")
  expect_equal(ast[[3]]$op, "IsMatch")
  expect_equal(ast[[3]]$pattern, "^a")
})

test_that("parse_query canonical-string round-trips for masks", {
  for (s in c("@ donor [ age > 60 ]",
              "@ gene [ ! is_lateral ]",
              "@ donor [ age > 60 & sex = male ]",
              "@ donor [ name ~ ^a ]")) {
    expect_equal(.canonicalise_ast(parse_query(s)), s, info = s)
  }
})

test_that("parse_query handles square slicing", {
  ast <- parse_query("@ cell @ gene :: UMIs @- cell1")
  ops <- vapply(ast, `[[`, "", "op")
  expect_equal(tail(ops, 1L), "SquareRowIs")
})

test_that("parse_query handles ReduceToColumn / ReduceToRow", {
  ast <- parse_query("@ cell @ gene :: UMIs >| Sum")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "ReduceToColumn")
  ast <- parse_query("@ cell @ gene :: UMIs >- Sum")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "ReduceToRow")
})

test_that("parse_query handles reduction with named params", {
  ast <- parse_query("@ cell @ gene :: UMIs >| Sum type: Int64")
  red <- tail(ast, 1L)[[1]]
  expect_equal(red$reduction, "Sum")
  expect_equal(red$params, list(type = "Int64"))
})

test_that("parse_query handles GroupBy / CountBy / GroupRowsBy / GroupColumnsBy", {
  ast <- parse_query("@ cell : UMIs / donor")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "GroupBy")
  ast <- parse_query("@ donor : age * sex")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "CountBy")
  ast <- parse_query("@ cell @ gene :: UMIs -/ donor")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "GroupRowsBy")
  ast <- parse_query("@ cell @ gene :: UMIs |/ type")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "GroupColumnsBy")
})

test_that("parse_query handles eltwise with params", {
  ast <- parse_query("@ cell : UMIs % Log eps: 1 base: 2")
  last <- tail(ast, 1L)[[1]]
  expect_equal(last$op, "Eltwise")
  expect_equal(last$name, "Log")
  expect_equal(last$params, list(eps = "1", base = "2"))
})

test_that("parse_query handles IfMissing / IfNot / AsAxis modifiers", {
  expect_equal(parse_query(". foo || 0")[[2]]$op, "IfMissing")
  expect_equal(parse_query("@ cell : bar ??")[[3]]$op, "IfNot")
  expect_equal(parse_query("=@ cell")[[1]]$op, "AsAxis")
})
