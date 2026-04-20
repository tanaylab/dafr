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

test_that("format_set_vector stores dense numeric/integer/logical/character vectors", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  for (v in list(c(1.0, 2.0, 3.0), c(1L, 2L, 3L), c(TRUE, FALSE, TRUE), c("x", "y", "z"))) {
    format_set_vector(d, "cell", "v", v, overwrite = TRUE)
    expect_identical(format_get_vector(d, "cell", "v"), v)
  }
})

test_that("format_set_vector strips names to the axis entry order (named input)", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  format_set_vector(d, "cell", "v",
                    c(B = 20.0, A = 10.0, C = 30.0),
                    overwrite = FALSE)
  got <- format_get_vector(d, "cell", "v")
  expect_equal(got, c(10.0, 20.0, 30.0), ignore_attr = TRUE)
  expect_null(names(got))
})

test_that("format_set_vector errors on length mismatch / unknown axis / NULL", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(format_set_vector(d, "gene", "v", c(1, 2), overwrite = FALSE),
               "axis .* does not exist")
  expect_error(format_set_vector(d, "cell", "v", c(1, 2, 3), overwrite = FALSE),
               "length 3.*expected 2")
  expect_error(format_set_vector(d, "cell", "v", NULL, overwrite = FALSE),
               "atomic")
})

test_that("format_set_vector errors on named vector with unknown entries", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(
    format_set_vector(d, "cell", "v",
                      c(A = 1.0, Z = 2.0),
                      overwrite = FALSE),
    "not in axis"
  )
})

test_that("format_set_vector honours overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  expect_error(format_set_vector(d, "cell", "v", c(3.0, 4.0), overwrite = FALSE),
               "already exists")
  format_set_vector(d, "cell", "v", c(3.0, 4.0), overwrite = TRUE)
  expect_equal(format_get_vector(d, "cell", "v"), c(3.0, 4.0))
})

test_that("format_set_vector bumps the vector version counter", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  vc <- S7::prop(d, "vector_version_counter")
  expect_null(vc[["cell:v"]])
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  expect_equal(vc[["cell:v"]], 1L)
  format_set_vector(d, "cell", "v", c(3.0, 4.0), overwrite = TRUE)
  expect_equal(vc[["cell:v"]], 2L)
})

test_that("format_delete_vector removes + respects must_exist", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  format_delete_vector(d, "cell", "v", must_exist = TRUE)
  expect_false(format_has_vector(d, "cell", "v"))
  expect_error (format_delete_vector(d, "cell", "v", must_exist = TRUE),  "does not exist")
  expect_silent(format_delete_vector(d, "cell", "v", must_exist = FALSE))
})

test_that("get_vector returns axis-named vector", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  format_set_vector(d, "cell", "v", c(10.0, 20.0), overwrite = FALSE)
  got <- get_vector(d, "cell", "v")
  expect_equal(names(got), c("A", "B"))
  expect_equal(unname(got), c(10.0, 20.0))
})

test_that("get_vector default recycles a scalar across the axis", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(get_vector(d, "cell", "missing"), "does not exist")
  na_vec <- get_vector(d, "cell", "missing", default = NA)
  expect_equal(names(na_vec), c("A", "B"))
  expect_true(all(is.na(na_vec)))
  str_vec <- get_vector(d, "cell", "missing", default = "savta")
  expect_equal(str_vec, c(A = "savta", B = "savta"))
})

test_that("get_vector hits the memory-tier cache on repeated reads", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  first  <- get_vector(d, "cell", "v")
  second <- get_vector(d, "cell", "v")
  expect_identical(first, second)
  cache_env <- S7::prop(d, "cache")
  expect_true(exists(cache_key_vector("cell", "v"),
                     envir = cache_env$memory, inherits = FALSE))
})

test_that("get_vector cache invalidates after overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  expect_equal(unname(get_vector(d, "cell", "v")), c(1.0, 2.0))
  format_set_vector(d, "cell", "v", c(10.0, 20.0), overwrite = TRUE)
  expect_equal(unname(get_vector(d, "cell", "v")), c(10.0, 20.0))
})
