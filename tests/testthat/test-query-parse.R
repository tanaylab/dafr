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
