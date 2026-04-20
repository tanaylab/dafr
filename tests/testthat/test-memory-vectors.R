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

test_that("get_vector cache invalidates when the axis counter bumps", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  format_set_vector(d, "cell", "v", c(1.0, 2.0), overwrite = FALSE)
  first <- get_vector(d, "cell", "v")
  cache_env <- S7::prop(d, "cache")
  key <- cache_key_vector("cell", "v")
  stale_entry <- get(key, envir = cache_env$memory, inherits = FALSE)
  expect_equal(stale_entry$stamp[[1L]], 1L)  # axis counter == 1 when first cached

  bump_axis_counter(d, "cell")
  # Cached entry's stamp now disagrees with current axis_stamp.
  second <- get_vector(d, "cell", "v")
  expect_identical(first, second)            # values unchanged (no data mutation)
  fresh_entry <- get(key, envir = cache_env$memory, inherits = FALSE)
  expect_equal(fresh_entry$stamp[[1L]], 2L)  # restamped to current axis counter
})

test_that("set_vector with named input reorders by axis entries", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  set_vector(d, "cell", "v", c(B = 2.0, A = 1.0))
  expect_equal(get_vector(d, "cell", "v"), c(A = 1.0, B = 2.0))
})

test_that("set_vector rejects length mismatch", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  expect_error(set_vector(d, "cell", "v", c(1.0, 2.0)), "length 2")
})

test_that("set_vector respects overwrite = FALSE", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  set_vector(d, "cell", "v", 1.0)
  expect_error(set_vector(d, "cell", "v", 2.0), "already exists")
  set_vector(d, "cell", "v", 2.0, overwrite = TRUE)
  expect_equal(unname(get_vector(d, "cell", "v")), 2.0)
})

test_that("delete_vector invalidates cached read", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  set_vector(d, "cell", "v", 1.0)
  get_vector(d, "cell", "v")  # populate cache
  delete_vector(d, "cell", "v")
  expect_false(has_vector(d, "cell", "v"))
  expect_error (delete_vector(d, "cell", "v"),                 "does not exist")
  expect_silent(delete_vector(d, "cell", "v", must_exist = FALSE))
})

test_that("has_vector + vectors_set expose current state", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_equal(vectors_set(d, "cell"), character(0L))
  expect_false(has_vector(d, "cell", "v"))
  set_vector(d, "cell", "v", c(1.0, 2.0))
  expect_true(has_vector(d, "cell", "v"))
  expect_equal(vectors_set(d, "cell"), "v")
})

test_that("set_vector round-trips bit64::integer64 vectors", {
  skip_if_not_installed("bit64")
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  big <- bit64::as.integer64(c(1e10, 2e10, 3e10))
  set_vector(d, "cell", "big", big)
  got <- get_vector(d, "cell", "big")
  expect_s3_class(got, "integer64")
  expect_equal(as.numeric(got), as.numeric(big), ignore_attr = TRUE)
  expect_equal(names(got), c("A", "B", "C"))
})

test_that("named bit64 vector reorders to axis order", {
  skip_if_not_installed("bit64")
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  v <- bit64::as.integer64(c(2e10, 1e10))
  names(v) <- c("B", "A")
  set_vector(d, "cell", "big", v)
  got <- get_vector(d, "cell", "big")
  expect_equal(as.numeric(unname(got)), c(1e10, 2e10))
})

test_that("get_vector default recycles a numeric scalar across the axis", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  got <- get_vector(d, "cell", "missing", default = 1)
  expect_equal(got, c(A = 1, B = 1))
})

test_that("set_vector / get_vector round-trip a named string vector", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_equal(vectors_set(d, "cell"), character(0L))
  expect_false(has_vector(d, "cell", "kind"))
  set_vector(d, "cell", "kind", c(A = "X", B = "Y"))
  expect_true(has_vector(d, "cell", "kind"))
  expect_equal(vectors_set(d, "cell"), "kind")
  expect_equal(get_vector(d, "cell", "kind"), c(A = "X", B = "Y"))
  delete_vector(d, "cell", "kind")
  expect_equal(vectors_set(d, "cell"), character(0L))
  expect_false(has_vector(d, "cell", "kind"))
})

test_that("get_vector(default = <length-N vector>) passes through", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  default <- c(10.0, 20.0, 30.0)
  out <- get_vector(d, "cell", "absent", default = default)
  expect_equal(unname(out), default)
  expect_equal(names(out), c("A", "B", "C"))
})

test_that("get_vector(default = <scalar>) recycles", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  out <- get_vector(d, "cell", "absent", default = NA)
  expect_equal(unname(out), rep(NA, 3L))
})

test_that("get_vector(default = <wrong-length vector>) errors", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  expect_error(get_vector(d, "cell", "absent", default = c(1, 2, 3)),
               "default has length 3|expected 2")
})
