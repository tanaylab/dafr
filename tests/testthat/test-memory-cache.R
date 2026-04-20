test_that("empty_cache on a populated memory tier leaves the store intact", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
  set_matrix(d, "cell", "gene", "UMIs", m)
  get_matrix(d, "cell", "gene", "UMIs")   # populate cache
  empty_cache(d)
  expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
               m, ignore_attr = TRUE)
  empty_cache(d, clear = "MappedData")
  empty_cache(d, keep  = "MemoryData")
  expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
               m, ignore_attr = TRUE)
})

test_that("get_vector returns identical cached result on second call", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  set_vector(d, "cell", "score", c(1.0, 2.0, 3.0))
  expect_identical(get_vector(d, "cell", "score"),
                   get_vector(d, "cell", "score"))
})

test_that("get_vector cache invalidates after overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  set_vector(d, "cell", "score", c(1.0, 2.0, 3.0))
  expect_equal(unname(get_vector(d, "cell", "score")), c(1.0, 2.0, 3.0))
  set_vector(d, "cell", "score", c(10.0, 20.0, 30.0), overwrite = TRUE)
  expect_equal(unname(get_vector(d, "cell", "score")), c(10.0, 20.0, 30.0))
})

test_that("empty_cache clears the memory tier", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B", "C"))
  set_vector(d, "cell", "score", c(1.0, 2.0, 3.0))
  get_vector(d, "cell", "score")
  ce <- S7::prop(d, "cache")
  expect_gt(length(ls(ce$memory)), 0L)
  empty_cache(d)
  expect_equal(length(ls(ce$memory)), 0L)
})
