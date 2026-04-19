test_that("mmap_real reads doubles without copying", {
  f <- new_tempfile("bin")
  vals <- seq(1.0, 1000.0, length.out = 1000)
  writeBin(vals, f, size = 8L)

  v <- mmap_real(f, 1000)
  expect_equal(length(v), 1000L)
  expect_equal(v[1], 1)
  expect_equal(v[1000], 1000)
  expect_equal(sum(v), sum(vals))
})

test_that("mmap_int reads int32 elements", {
  f <- new_tempfile("bin")
  vals <- 1:1000L
  writeBin(vals, f, size = 4L)

  v <- mmap_int(f, 1000)
  expect_equal(length(v), 1000L)
  expect_equal(v[500], 500L)
  expect_equal(sum(v), sum(vals))
})

test_that("mmap_lgl reads logical (int32-stored)", {
  f <- new_tempfile("bin")
  vals <- as.integer(rep(c(1L, 0L, NA_integer_), length.out = 300))
  writeBin(vals, f, size = 4L)

  v <- mmap_lgl(f, 300)
  expect_equal(length(v), 300L)
  expect_true(is.logical(v))
  expect_identical(v[1:3], c(TRUE, FALSE, NA))
})

test_that("writing to an mmap-backed vector triggers materialization", {
  f <- new_tempfile("bin")
  writeBin(c(1.0, 2.0, 3.0), f, size = 8L)
  v <- mmap_real(f, 3)
  expect_silent({ v[1] <- 99 })        # triggers Dataptr(writeable=TRUE)
  expect_equal(v, c(99, 2, 3))         # user-side copy now owns the mutation
})

test_that("zero-length file mmap is a length-0 vector", {
  f <- new_tempfile("bin")
  file.create(f)
  v <- mmap_real(f, 0)
  expect_equal(length(v), 0L)
})
