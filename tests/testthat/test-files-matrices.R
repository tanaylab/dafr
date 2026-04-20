test_that("FilesDaf matrices_set lists descriptor-backed matrices", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
  writeLines(c("X", "Y"), file.path(dir, "axes", "gene.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "matrices", "cell", "gene", "m.json"))
  writeBin(c(1.0, 2.0, 3.0, 4.0),
           file.path(dir, "matrices", "cell", "gene", "m.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  expect_equal(matrices_set(d, "cell", "gene"), "m")
  expect_true(has_matrix(d, "cell", "gene", "m"))
})

test_that("has_matrix returns FALSE on missing axis", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  d <- files_daf(dir, mode = "r")
  expect_false(has_matrix(d, "cell", "gene", "m"))
  expect_equal(matrices_set(d, "cell", "gene"), character(0L))
})

test_that("format_get_matrix dense Float64 round-trips with correct shape", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A","B","C"), file.path(dir, "axes", "cell.txt"))
  writeLines(c("X","Y"), file.path(dir, "axes", "gene.txt"))
  writeLines('{"format":"dense","eltype":"Float64"}',
             file.path(dir, "matrices", "cell", "gene", "m.json"))
  writeBin(c(1,2,3,4,5,6),
           file.path(dir, "matrices", "cell", "gene", "m.data"),
           size = 8L, endian = "little")
  d <- files_daf(dir, mode = "r")
  m <- format_get_matrix(d, "cell", "gene", "m")
  expect_equal(dim(m), c(3L, 2L))
  expect_equal(m[2, 2], 5)
  expect_equal(m[, 1], c(1, 2, 3))
})

test_that("format_get_matrix dense Int32", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A","B"), file.path(dir, "axes", "cell.txt"))
  writeLines(c("X","Y"), file.path(dir, "axes", "gene.txt"))
  writeLines('{"format":"dense","eltype":"Int32"}',
             file.path(dir, "matrices", "cell", "gene", "mi.json"))
  writeBin(c(1L,2L,3L,4L),
           file.path(dir, "matrices", "cell", "gene", "mi.data"),
           size = 4L, endian = "little")
  d <- files_daf(dir, mode = "r")
  m <- format_get_matrix(d, "cell", "gene", "mi")
  expect_equal(dim(m), c(2L, 2L))
  expect_true(is.integer(m))
  expect_equal(unname(m), matrix(1:4, nrow = 2))
})

test_that("format_get_matrix dense String round-trip (column-major)", {
  dir <- new_tempdir()
  dir.create(file.path(dir, "axes"), recursive = TRUE)
  dir.create(file.path(dir, "matrices", "cell", "gene"), recursive = TRUE)
  writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
  writeLines(c("A","B"), file.path(dir, "axes", "cell.txt"))
  writeLines(c("X","Y"), file.path(dir, "axes", "gene.txt"))
  writeLines('{"format":"dense","eltype":"String"}',
             file.path(dir, "matrices", "cell", "gene", "ms.json"))
  # Column-major: column 1 (X) then column 2 (Y)
  writeLines(c("aX", "bX", "aY", "bY"),
             file.path(dir, "matrices", "cell", "gene", "ms.txt"))
  d <- files_daf(dir, mode = "r")
  m <- format_get_matrix(d, "cell", "gene", "ms")
  expect_equal(dim(m), c(2L, 2L))
  expect_equal(m[1, 1], "aX")
  expect_equal(m[2, 2], "bY")
})
