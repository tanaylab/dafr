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
