test_that("FilesDaf vectors_set lists descriptor-backed vectors", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "vectors", "cell", "donor.json"))
  writeBin(c(1.0, 2.0),
           file.path(dir, "vectors", "cell", "donor.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  expect_equal(vectors_set(d, "cell"), "donor")
  expect_true(has_vector(d, "cell", "donor"))
})

test_that("format_get_vector returns an ALTREP-backed vector for Float64 dense", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "vectors", "cell", "x.json"))
  writeBin(c(1.5, 2.5, -3.25),
           file.path(dir, "vectors", "cell", "x.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  v <- format_get_vector(d, "cell", "x")
  expect_equal(v, c(1.5, 2.5, -3.25))
  expect_true(is_altrep(v))
})

test_that("format_get_vector eager-reads Float64 when dafr.mmap = FALSE", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "vectors", "cell", "x.json"))
  writeBin(c(10.0, 20.0), file.path(dir, "vectors", "cell", "x.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  v <- withr::with_options(list(dafr.mmap = FALSE),
                           format_get_vector(d, "cell", "x"))
  expect_equal(v, c(10.0, 20.0))
  expect_false(is_altrep(v))
})

test_that("format_get_vector densifies Int32 via mmap", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Int32"}',
             file.path(dir, "vectors", "cell", "i.json"))
  writeBin(c(1L, -2L, 3L), file.path(dir, "vectors", "cell", "i.data"),
           size = 4L, endian = "little")
  d <- files_daf(dir, mode = "r")
  v <- format_get_vector(d, "cell", "i")
  expect_equal(v, c(1L, -2L, 3L))
  expect_true(is_altrep(v) || is.integer(v))
})

test_that("format_get_vector Bool dense (eager read path, not mmap)", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Bool"}',
             file.path(dir, "vectors", "cell", "b.json"))
  writeBin(as.raw(c(1L, 0L, 1L)), file.path(dir, "vectors", "cell", "b.data"))
  d <- files_daf(dir, mode = "r")
  v <- format_get_vector(d, "cell", "b")
  expect_equal(v, c(TRUE, FALSE, TRUE))
})

test_that("format_get_vector String dense round-trip", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"String"}',
             file.path(dir, "vectors", "cell", "s.json"))
  writeLines(c("foo", "bar"),
             file.path(dir, "vectors", "cell", "s.txt"))
  d <- files_daf(dir, mode = "r")
  v <- format_get_vector(d, "cell", "s")
  expect_equal(v, c("foo", "bar"))
})

test_that("format_get_vector errors on missing descriptor", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines("A", file.path(dir, "axes", "cell.txt"))
  d <- files_daf(dir, mode = "r")
  expect_error(format_get_vector(d, "cell", "nope"), "does not exist")
})

test_that("format_get_vector errors on truncated payload", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "vectors", "cell", "t.json"))
  # Write only 2 doubles instead of 3:
  writeBin(c(1.0, 2.0), file.path(dir, "vectors", "cell", "t.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  expect_error(format_get_vector(d, "cell", "t"), "truncated")
})
