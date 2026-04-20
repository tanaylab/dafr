test_that(".tokenize_query splits simple axis lookup", {
  toks <- .tokenize_query("@ cell")
  expect_equal(length(toks), 2L)
  expect_equal(toks[[1]]$type, "operator"); expect_equal(toks[[1]]$value, "@")
  expect_equal(toks[[2]]$type, "value");    expect_equal(toks[[2]]$value, "cell")
})

test_that(".tokenize_query splits vector lookup", {
  toks <- .tokenize_query("@ cell : UMIs")
  expect_equal(length(toks), 4L)
  expect_equal(vapply(toks, `[[`, "", "value"), c("@", "cell", ":", "UMIs"))
})

test_that(".tokenize_query handles double operators (::, >>, >-)", {
  toks <- .tokenize_query("@ cell @ gene :: UMIs >| Sum")
  vals <- vapply(toks, `[[`, "", "value")
  expect_equal(vals, c("@", "cell", "@", "gene", "::", "UMIs", ">|", "Sum"))
})

test_that(".tokenize_query preserves escaped values", {
  toks <- .tokenize_query("@ cell : \"weird name\"")
  vals <- vapply(toks, `[[`, "", "value")
  expect_equal(vals[[4L]], "weird name")
})

test_that(".tokenize_query records 1-based positions", {
  toks <- .tokenize_query("@ cell")
  expect_equal(toks[[1]]$pos, 1L)
  expect_equal(toks[[2]]$pos, 3L)
})

test_that(".tokenize_query rejects unknown operator characters", {
  expect_error(.tokenize_query("@ cell $ weird"), "unexpected character")
})
