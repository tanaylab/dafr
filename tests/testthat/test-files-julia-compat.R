test_that("can read Julia-written scalars", {
  expect_true(dir.exists(fixture_path()))
  d <- files_daf(fixture_path(), mode = "r")
  expect_equal(get_scalar(d, "pi"),    3.14)
  expect_equal(get_scalar(d, "cells"), bit64::as.integer64(100))
  expect_equal(get_scalar(d, "note"),  "hello")
})

test_that("can read Julia-written axes + dense + sparse vectors", {
  d <- files_daf(fixture_path(), mode = "r")
  expect_equal(axis_vector(d, "cell"), c("A", "B", "C", "D"))
  expect_equal(axis_vector(d, "gene"), c("X", "Y"))
  expect_equal(unname(get_vector(d, "cell", "donor")),     c(1L, 2L, 3L, 4L))
  expect_equal(unname(get_vector(d, "cell", "sparse_x")),  c(0, 10, 0, 30))
  expect_equal(unname(get_vector(d, "cell", "flags")),
               c(TRUE, FALSE, TRUE, FALSE))
})

test_that("can read Julia-written dense + sparse + Bool matrices", {
  d <- files_daf(fixture_path(), mode = "r")
  dm <- get_matrix(d, "cell", "gene", "dense_m")
  expect_equal(dim(dm), c(4L, 2L))
  expect_equal(unname(dm), matrix(c(1,2,3,4,5,6,7,8), nrow = 4))
  sm <- get_matrix(d, "cell", "gene", "sparse_m")
  expect_equal(dim(sm), c(4L, 2L))
  expect_s4_class(sm, "dgCMatrix")
  expect_equal(as.matrix(unname(sm)),
               matrix(c(10,0,20,0,  0,30,0,0), nrow = 4))
  mk <- get_matrix(d, "cell", "gene", "mask")
  expect_s4_class(mk, "lgCMatrix")
  expect_equal(as.matrix(unname(mk)),
               matrix(c(TRUE, FALSE, FALSE, FALSE,
                        FALSE, TRUE,  FALSE, FALSE), nrow = 4))
})
