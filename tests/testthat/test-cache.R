# Minimal concrete subclass for testing the cache + counter helpers.
TestDaf <- S7::new_class(
  "TestDaf",
  package = "dafr",
  parent = DafReader,
  constructor = function() {
    S7::new_object(
      S7::S7_object(),
      name = "test",
      internal = new_internal_env(),
      cache = new_cache_env(),
      axis_version_counter = new_counter_env(),
      vector_version_counter = new_counter_env(),
      matrix_version_counter = new_counter_env()
    )
  }
)

test_that("cache keys are canonical strings", {
  expect_equal(cache_key_scalar("pi"), "scalar:pi")
  expect_equal(cache_key_axis("cell"), "axis:cell")
  expect_equal(cache_key_vector("cell", "n_counts"), "vector:cell:n_counts")
  expect_equal(cache_key_matrix("cell", "gene", "UMIs"), "matrix:cell:gene:UMIs")
  expect_equal(cache_key_query("/cell"), "query:/cell")
})

test_that("cache_put/get/remove round-trip through a tier", {
  daf <- TestDaf()
  cache_env <- S7::prop(daf, "cache")

  cache_put(cache_env, "memory", "k", 42L)
  expect_equal(cache_get(cache_env, "memory", "k"), 42L)
  expect_null(cache_get(cache_env, "mapped", "k"))
  expect_null(cache_get(cache_env, "query", "k"))

  cache_remove(cache_env, "memory", "k")
  expect_null(cache_get(cache_env, "memory", "k"))
})

test_that("empty_cache clears all three tiers by default", {
  daf <- TestDaf()
  ce <- S7::prop(daf, "cache")
  cache_put(ce, "mapped", "v:a:x", "mapped-value")
  cache_put(ce, "memory", "v:a:y", "memory-value")
  cache_put(ce, "query",  "q:1",   "query-value")

  empty_cache(daf)

  expect_null(cache_get(ce, "mapped", "v:a:x"))
  expect_null(cache_get(ce, "memory", "v:a:y"))
  expect_null(cache_get(ce, "query",  "q:1"))
})

test_that("empty_cache with group targets a subset", {
  daf <- TestDaf()
  ce <- S7::prop(daf, "cache")
  cache_put(ce, "mapped", "a", 1L)
  cache_put(ce, "memory", "b", 2L)
  cache_put(ce, "query",  "c", 3L)

  empty_cache(daf, group = c("memory", "query"))

  expect_equal(cache_get(ce, "mapped", "a"), 1L)
  expect_null(cache_get(ce, "memory", "b"))
  expect_null(cache_get(ce, "query",  "c"))
})

test_that("bump_axis_counter increments monotonically from 0", {
  daf <- TestDaf()
  counters <- S7::prop(daf, "axis_version_counter")
  expect_null(counters$cell)

  bump_axis_counter(daf, "cell")
  expect_equal(counters$cell, 1L)

  bump_axis_counter(daf, "cell")
  bump_axis_counter(daf, "cell")
  expect_equal(counters$cell, 3L)

  bump_axis_counter(daf, "gene")
  expect_equal(counters$gene, 1L)
  expect_equal(counters$cell, 3L)  # unchanged
})

test_that("bump_vector_counter / bump_matrix_counter use composite keys", {
  daf <- TestDaf()
  vc <- S7::prop(daf, "vector_version_counter")
  mc <- S7::prop(daf, "matrix_version_counter")

  bump_vector_counter(daf, "cell", "n_counts")
  expect_equal(vc[["cell:n_counts"]], 1L)

  bump_matrix_counter(daf, "cell", "gene", "UMIs")
  expect_equal(mc[["cell:gene:UMIs"]], 1L)

  # Different axes -> different keys.
  bump_vector_counter(daf, "gene", "n_counts")
  expect_equal(vc[["gene:n_counts"]], 1L)
  expect_equal(vc[["cell:n_counts"]], 1L)  # unchanged
})
