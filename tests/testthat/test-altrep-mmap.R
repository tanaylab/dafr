test_that("mmap_real reads doubles without copying", {
  skip_on_os("windows")
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
  skip_on_os("windows")
  f <- new_tempfile("bin")
  vals <- 1:1000L
  writeBin(vals, f, size = 4L)

  v <- mmap_int(f, 1000)
  expect_equal(length(v), 1000L)
  expect_equal(v[500], 500L)
  expect_equal(sum(v), sum(vals))
})

test_that("mmap_lgl reads logical (int32-stored)", {
  skip_on_os("windows")
  f <- new_tempfile("bin")
  vals <- as.integer(rep(c(1L, 0L, NA_integer_), length.out = 300))
  writeBin(vals, f, size = 4L)

  v <- mmap_lgl(f, 300)
  expect_equal(length(v), 300L)
  expect_true(is.logical(v))
  expect_identical(v[1:3], c(TRUE, FALSE, NA))
})

test_that("writing to an mmap-backed vector triggers materialization", {
  skip_on_os("windows")
  f <- new_tempfile("bin")
  writeBin(c(1.0, 2.0, 3.0), f, size = 8L)
  v <- mmap_real(f, 3)
  expect_silent({ v[1] <- 99 })        # triggers Dataptr(writeable=TRUE)
  expect_equal(v, c(99, 2, 3))         # user-side copy now owns the mutation
})

test_that("zero-length file mmap is a length-0 vector", {
  skip_on_os("windows")
  f <- new_tempfile("bin")
  file.create(f)
  v <- mmap_real(f, 0)
  expect_equal(length(v), 0L)
})

test_that("mmap_real rejects invalid inputs", {
  skip_on_os("windows")
  expect_error(mmap_real("/no/such/file", 10), "file.exists")
  f <- new_tempfile("bin"); file.create(f)
  expect_error(mmap_real(f, "ten"), "is.numeric")
  expect_error(mmap_real(f, -1), "length")
})

test_that("mmap_real rejects a file too small for the requested length", {
  skip_on_os("windows")
  f <- new_tempfile("bin")
  writeBin(c(1.0, 2.0, 3.0), f, size = 8L)  # 24 bytes
  expect_error(mmap_real(f, 100), "need at least")
})

test_that("second write after materialization uses materialized buffer (idempotent)", {
  skip_on_os("windows")
  f <- new_tempfile("bin")
  writeBin(c(1.0, 2.0, 3.0), f, size = 8L)
  v <- mmap_real(f, 3)
  v[1] <- 99
  v[2] <- 77
  expect_equal(v, c(99, 77, 3))
})

test_that("serialize roundtrip produces a normal vector with same values", {
  skip_on_os("windows")
  f <- new_tempfile("bin")
  vals <- as.double(seq(1, 100))
  writeBin(vals, f, size = 8L)
  v <- mmap_real(f, 100)

  tf <- new_tempfile("rds")
  saveRDS(v, tf)
  restored <- readRDS(tf)

  expect_equal(as.numeric(restored), vals)
  expect_equal(length(restored), 100L)
})

test_that("mmap_int serialize roundtrip preserves values", {
  skip_on_os("windows")
  f <- new_tempfile("bin")
  vals <- 1:50L
  writeBin(vals, f, size = 4L)
  v <- mmap_int(f, 50)

  tf <- new_tempfile("rds")
  saveRDS(v, tf)
  restored <- readRDS(tf)

  expect_equal(as.integer(restored), vals)
})

test_that("mmap_lgl serialize roundtrip preserves logicals including NA", {
  skip_on_os("windows")
  f <- new_tempfile("bin")
  vals <- as.integer(c(1L, 0L, NA_integer_, 1L, 0L))
  writeBin(vals, f, size = 4L)
  v <- mmap_lgl(f, 5)

  tf <- new_tempfile("rds")
  saveRDS(v, tf)
  restored <- readRDS(tf)

  expect_identical(as.logical(restored), c(TRUE, FALSE, NA, TRUE, FALSE))
})
