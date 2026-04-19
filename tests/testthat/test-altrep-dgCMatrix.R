make_test_dgC_files <- function() {
  # Build a small dgCMatrix, write its slots as plain binary files.
  set.seed(1)
  dense <- matrix(
    ifelse(runif(100) < 0.3, rnorm(100), 0),
    nrow = 10, ncol = 10
  )
  m <- methods::as(dense, "CsparseMatrix")  # dgCMatrix
  stopifnot(inherits(m, "dgCMatrix"))

  # Create a tempdir whose cleanup is scoped to the calling test_that frame.
  # (Cannot use new_tempdir() here: its withr::defer_parent() defers to this
  # helper's own frame, which would unlink the dir before the test returns.)
  d <- tempfile()
  dir.create(d)
  withr::defer(unlink(d, recursive = TRUE, force = TRUE),
               envir = parent.frame())
  writeBin(m@x, file.path(d, "x.bin"), size = 8L)
  writeBin(as.integer(m@i), file.path(d, "i.bin"), size = 4L)  # 0-based
  writeBin(as.integer(m@p), file.path(d, "p.bin"), size = 4L)

  list(dir = d, dense = dense, nnz = length(m@x),
       reference = m)
}

test_that("mmap_dgCMatrix reconstructs slots correctly", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow   = 10, ncol = 10, nnz = tf$nnz
  )
  expect_equal(dim(m), c(10L, 10L))
  expect_equal(as.matrix(m), tf$reference |> as.matrix())
})

test_that("colSums on mmap-backed dgCMatrix matches Matrix::colSums", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow = 10, ncol = 10, nnz = tf$nnz
  )
  expect_equal(Matrix::colSums(m), Matrix::colSums(tf$reference))
})

test_that("rowSums on mmap-backed dgCMatrix matches Matrix::rowSums", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow = 10, ncol = 10, nnz = tf$nnz
  )
  expect_equal(Matrix::rowSums(m), Matrix::rowSums(tf$reference))
})

test_that("Matrix::t() round-trip yields same values", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow = 10, ncol = 10, nnz = tf$nnz
  )
  mt <- Matrix::t(m)
  expect_equal(as.matrix(mt), t(as.matrix(tf$reference)))
})

test_that("element assignment triggers materialization, doesn't crash", {
  tf <- make_test_dgC_files()
  m <- mmap_dgCMatrix(
    x_path = file.path(tf$dir, "x.bin"),
    i_path = file.path(tf$dir, "i.bin"),
    p_path = file.path(tf$dir, "p.bin"),
    nrow = 10, ncol = 10, nnz = tf$nnz
  )
  expect_silent(m[1, 1] <- 99)
})
