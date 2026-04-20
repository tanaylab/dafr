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

test_that("cache_lookup returns NULL and evicts when stamps differ", {
  ce <- new_cache_env()
  cache_store(ce, "memory", "v:x", "v1", stamp = c(1L, 1L), size_bytes = 100)
  expect_equal(cache_lookup(ce, "memory", "v:x", c(1L, 1L)), "v1")
  expect_null( cache_lookup(ce, "memory", "v:x", c(1L, 2L)))
  expect_false(exists("v:x", envir = ce$memory, inherits = FALSE))
})

test_that("cache_lookup returns NULL for missing key", {
  ce <- new_cache_env()
  expect_null(cache_lookup(ce, "memory", "v:missing", c(0L, 0L)))
})

test_that("cache_store persists stamp + size alongside value", {
  ce <- new_cache_env()
  cache_store(ce, "memory", "k", 42L, c(3L, 4L), size_bytes = 8)
  entry <- get("k", envir = ce$memory, inherits = FALSE)
  expect_equal(entry$value, 42L)
  expect_equal(entry$stamp, c(3L, 4L))
  expect_equal(entry$size,  8)
})

test_that("axis_stamp / vector_stamp / matrix_stamp mirror counter state", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  # axis_stamp: scalar
  expect_equal(axis_stamp(d, "cell"), 1L)   # bumped by add_axis
  expect_equal(axis_stamp(d, "gene"), 0L)   # missing axis -> 0
  # vector_stamp: c(axis_stamp, vector_counter)
  expect_equal(vector_stamp(d, "cell", "v"), c(1L, 0L))
  bump_vector_counter(d, "cell", "v")
  expect_equal(vector_stamp(d, "cell", "v"), c(1L, 1L))
  # matrix_stamp: c(rows_axis_stamp, columns_axis_stamp, matrix_counter)
  add_axis(d, "gene", "X")
  expect_equal(matrix_stamp(d, "cell", "gene", "m"), c(1L, 1L, 0L))
  bump_matrix_counter(d, "cell", "gene", "m")
  expect_equal(matrix_stamp(d, "cell", "gene", "m"), c(1L, 1L, 1L))
})

# ---- I2: LRU + memory-cap eviction ------------------------------------------

test_that("cache_store evicts LRU entries when memory cap is exceeded", {
  ce <- new_cache_env()
  cache_set_cap(ce, 1000)
  cache_store(ce, "memory", "a", "A", c(0L), size_bytes = 400)
  cache_store(ce, "memory", "b", "B", c(0L), size_bytes = 400)
  cache_store(ce, "memory", "c", "C", c(0L), size_bytes = 400)
  # "a" should have been evicted (it became LRU after b, c).
  expect_false(exists("a", envir = ce$memory, inherits = FALSE))
  expect_true( exists("b", envir = ce$memory, inherits = FALSE))
  expect_true( exists("c", envir = ce$memory, inherits = FALSE))
})

test_that("cache_lookup touches LRU on hit (moves entry to MRU)", {
  ce <- new_cache_env()
  cache_set_cap(ce, 1000)
  cache_store(ce, "memory", "a", "A", c(0L), size_bytes = 400)
  cache_store(ce, "memory", "b", "B", c(0L), size_bytes = 400)
  # Access "a" - now "b" is LRU.
  cache_lookup(ce, "memory", "a", c(0L))
  cache_store(ce, "memory", "c", "C", c(0L), size_bytes = 400)
  # "b" should have been evicted, not "a".
  expect_true( exists("a", envir = ce$memory, inherits = FALSE))
  expect_false(exists("b", envir = ce$memory, inherits = FALSE))
  expect_true( exists("c", envir = ce$memory, inherits = FALSE))
})

test_that("mapped tier is exempt from the memory cap", {
  ce <- new_cache_env()
  cache_set_cap(ce, 100)
  cache_store(ce, "mapped", "a", "A", c(0L), size_bytes = 1e9)
  cache_store(ce, "memory", "b", "B", c(0L), size_bytes = 50)
  expect_true(exists("a", envir = ce$mapped, inherits = FALSE))
  expect_true(exists("b", envir = ce$memory, inherits = FALSE))
})

test_that("entries larger than the cap are stored and immediately evict others", {
  ce <- new_cache_env()
  cache_set_cap(ce, 100)
  cache_store(ce, "memory", "small", "x", c(0L), size_bytes = 50)
  # Oversized entry: stored, others evicted; still present.
  cache_store(ce, "memory", "big", "X", c(0L), size_bytes = 500)
  expect_false(exists("small", envir = ce$memory, inherits = FALSE))
  expect_true( exists("big",   envir = ce$memory, inherits = FALSE))
})

# ---- I3: Julia-style empty_cache clear/keep ---------------------------------

test_that("empty_cache accepts 'clear' and 'keep' groups (Julia parity)", {
  d <- memory_daf()
  add_axis(d, "cell", c("A"))
  set_vector(d, "cell", "v", 1.0)
  get_vector(d, "cell", "v")   # populate memory tier
  ce <- S7::prop(d, "cache")
  expect_true(length(ls(ce$memory)) > 0L)

  empty_cache(d, clear = "MappedData")
  expect_true(length(ls(ce$memory)) > 0L)       # memory left alone

  empty_cache(d, keep = "MemoryData")
  expect_true(length(ls(ce$memory)) > 0L)       # memory preserved, others cleared

  empty_cache(d)
  expect_equal(length(ls(ce$memory)), 0L)
})
