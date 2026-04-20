test_that("files_daf() w+ creates a new store with daf.json + skeleton dirs", {
  dir <- new_tempdir()
  d <- files_daf(dir, mode = "w+")
  expect_s3_class(d, "dafr::FilesDaf")
  expect_true(inherits(d, "dafr::DafWriter"))
  expect_true(file.exists(file.path(dir, "daf.json")))
  for (sub in c("scalars", "axes", "vectors", "matrices")) {
    expect_true(dir.exists(file.path(dir, sub)))
  }
})

test_that("files_daf() w errors on existing directory with daf.json", {
  dir <- new_tempdir()
  files_daf(dir, mode = "w+")
  expect_error(files_daf(dir, mode = "w"), "already exists")
})

test_that("files_daf() r opens an existing store read-only", {
  dir <- new_tempdir()
  files_daf(dir, mode = "w+")
  d <- files_daf(dir, mode = "r")
  expect_s3_class(d, "dafr::FilesDafReadOnly")
  expect_true(inherits(d, "dafr::DafReadOnly"))
  expect_false(inherits(d, "dafr::DafWriter"))
})

test_that("files_daf() r errors on missing daf.json", {
  dir <- new_tempdir()
  expect_error(files_daf(dir, mode = "r"), "not a daf directory|does not exist")
})

test_that("files_daf() default name is the basename of path", {
  dir <- new_tempdir()
  base <- basename(dir)
  d <- files_daf(dir, mode = "w+")
  expect_equal(S7::prop(d, "name"), base)
})

test_that("files_daf() rejects unknown mode", {
  dir <- new_tempdir()
  files_daf(dir, mode = "w+")
  expect_error(files_daf(dir, mode = "rw"), "'arg' should be")
})

test_that("read-only FilesDaf rejects set_scalar / add_axis", {
  dir <- new_tempdir()
  files_daf(dir, mode = "w+")
  d <- files_daf(dir, mode = "r")
  expect_error(set_scalar(d, "pi", 3.14),
               "read-only|DafReadOnly")
  expect_error(add_axis(d, "cell", "A"),
               "read-only|DafReadOnly")
})
