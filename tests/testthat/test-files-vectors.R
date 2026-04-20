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
