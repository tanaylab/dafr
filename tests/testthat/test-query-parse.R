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
