# v3 flat WRITE paths: write via zarr_daf(dir, "w"), read back via
# zarr_daf(dir, "r"). On-disk-shape assertions pin the v3 layout
# (zarr.json + c/-prefixed chunk keys, reversed dense-matrix shape).

test_that("v3 scalar + axis write then read back in-process", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_scalar(d, "title", "hello")
  set_scalar(d, "n", 7L)
  d2 <- zarr_daf(dir, mode = "r")
  expect_equal(get_scalar(d2, "title"), "hello")
  expect_equal(as.integer(get_scalar(d2, "n")), 7L)
  expect_equal(axis_vector(d2, "cell"), c("c1", "c2", "c3"))
  # on-disk shape is the v3 layout
  expect_true(file.exists(file.path(dir, "scalars", "n", "zarr.json")))
  expect_true(file.exists(file.path(dir, "scalars", "n", "c", "0")))
})

test_that("v3 dense + sparse vector write round-trips in-process", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_vector(d, "cell", "score", c(1.5, 2.5, 3.5))
  set_vector(d, "cell", "sparse_v", Matrix::sparseVector(c(4, 9), c(1, 3), 3))
  d2 <- zarr_daf(dir, mode = "r")
  expect_equal(unname(get_vector(d2, "cell", "score")), c(1.5, 2.5, 3.5))
  expect_equal(unname(as.numeric(get_vector(d2, "cell", "sparse_v"))),
               c(4, 0, 9))
  # sparse vector is a v3 group; nzind is int64
  expect_true(file.exists(file.path(dir, "vectors", "cell", "sparse_v",
                                    "zarr.json")))
  expect_true(file.exists(file.path(dir, "vectors", "cell", "sparse_v",
                                    "nzind", "zarr.json")))
  nzind_meta <- jsonlite::fromJSON(file.path(dir, "vectors", "cell",
                                             "sparse_v", "nzind", "zarr.json"))
  expect_equal(nzind_meta$data_type, "int64")
})

test_that("v3 dense + sparse matrix write round-trips in-process", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "expr", matrix(c(1, 3, 5, 2, 4, 6), nrow = 3))
  set_matrix(d, "cell", "gene", "sp",
             Matrix::sparseMatrix(i = c(3, 1), j = c(1, 2), x = c(5, 2),
                                  dims = c(3, 2)))
  d2 <- zarr_daf(dir, mode = "r")
  expect_equal(unname(as.matrix(get_matrix(d2, "cell", "gene", "expr"))),
               matrix(c(1, 3, 5, 2, 4, 6), nrow = 3))
  expect_equal(unname(as.matrix(get_matrix(d2, "cell", "gene", "sp"))),
               matrix(c(0, 0, 5, 2, 0, 0), nrow = 3))
  # on-disk shape is reversed [n_cols, n_rows]
  m <- jsonlite::fromJSON(file.path(dir, "matrices", "cell", "gene", "expr",
                                    "zarr.json"))
  expect_equal(as.integer(m$shape), c(2L, 3L))
  expect_true(file.exists(file.path(dir, "matrices", "cell", "gene", "expr",
                                    "c", "0", "0")))
  # sparse CSC colptr/rowval are int64
  colptr_meta <- jsonlite::fromJSON(file.path(dir, "matrices", "cell", "gene",
                                              "sp", "colptr", "zarr.json"))
  rowval_meta <- jsonlite::fromJSON(file.path(dir, "matrices", "cell", "gene",
                                              "sp", "rowval", "zarr.json"))
  expect_equal(colptr_meta$data_type, "int64")
  expect_equal(rowval_meta$data_type, "int64")
})

test_that("v3 vector + matrix delete remove the node and refresh the index", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2"))
  add_axis(d, "gene", c("g1", "g2"))
  set_vector(d, "cell", "v", c(1, 2))
  set_matrix(d, "cell", "gene", "m", matrix(c(1, 2, 3, 4), nrow = 2))
  delete_vector(d, "cell", "v")
  delete_matrix(d, "cell", "gene", "m")
  expect_false(has_vector(d, "cell", "v"))
  expect_false(has_matrix(d, "cell", "gene", "m"))
  expect_false(file.exists(file.path(dir, "vectors", "cell", "v", "zarr.json")))
  expect_false(file.exists(file.path(dir, "matrices", "cell", "gene", "m",
                                     "zarr.json")))
  # the consolidated index no longer lists the deleted nodes
  root <- jsonlite::fromJSON(file.path(dir, "zarr.json"),
                             simplifyVector = FALSE)
  idx <- names(root$consolidated_metadata$metadata)
  expect_false("vectors/cell/v" %in% idx)
  expect_false("matrices/cell/gene/m" %in% idx)
})

test_that("v3 scalar + axis delete remove the node and the on-disk zarr.json", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2"))
  set_scalar(d, "title", "hello")
  delete_scalar(d, "title")
  delete_axis(d, "cell")
  expect_false(has_scalar(d, "title"))
  expect_false(has_axis(d, "cell"))
  expect_false(file.exists(file.path(dir, "scalars", "title", "zarr.json")))
  expect_false(file.exists(file.path(dir, "axes", "cell", "zarr.json")))
})

test_that("v3 sparse vector delete removes the whole subtree", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_vector(d, "cell", "sv", Matrix::sparseVector(c(4, 9), c(1, 3), 3))
  delete_vector(d, "cell", "sv")
  expect_false(has_vector(d, "cell", "sv"))
  expect_false(file.exists(file.path(dir, "vectors", "cell", "sv", "nzind",
                                     "zarr.json")))
})

test_that("v3 empty / all-true-bool sparse edge-case round-trips", {
  dir <- tempfile(fileext = ".daf.zarr")
  d <- zarr_daf(dir, mode = "w")
  # daf requires axes with >= 1 entry, so the nearest meaningful "empty" edge
  # is a length-matching all-zero dense vector and a zero-nonzero sparse vector
  # over a small (length-3) axis.
  add_axis(d, "cell", c("c1", "c2", "c3"))
  add_axis(d, "gene", c("g1", "g2"))

  # All-zero dense numeric vector round-trips.
  set_vector(d, "cell", "zeros_v", c(0, 0, 0))
  # Zero-nonzero sparse vector round-trips to an all-zero dense result.
  set_vector(d, "cell", "empty_sv",
             Matrix::sparseVector(numeric(0), integer(0), length = 3))

  # Sparse matrix with zero non-zeros: dims preserved, zero nnz.
  empty_sm <- Matrix::sparseMatrix(i = integer(0), j = integer(0),
                                   x = numeric(0), dims = c(3, 2))
  set_matrix(d, "cell", "gene", "empty_sm", empty_sm)

  # All-true Bool sparse matrix (lgCMatrix): writer omits nzval/ on disk,
  # reader re-synthesizes TRUE.
  all_true_sm <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = TRUE,
                                      dims = c(3, 2))
  expect_s4_class(all_true_sm, "lgCMatrix")
  set_matrix(d, "cell", "gene", "alltrue_sm", all_true_sm)

  d2 <- zarr_daf(dir, mode = "r")

  # All-zero dense vector round-trips.
  expect_equal(unname(get_vector(d2, "cell", "zeros_v")), c(0, 0, 0))

  # Zero-nonzero sparse vector densifies to all-zero of length 3.
  expect_equal(unname(as.numeric(get_vector(d2, "cell", "empty_sv"))),
               c(0, 0, 0))

  # Empty sparse matrix: 3x2 all-zero, zero nnz, dims preserved.
  got_empty <- get_matrix(d2, "cell", "gene", "empty_sm")
  expect_equal(dim(got_empty), c(3L, 2L))
  expect_equal(length(got_empty@x), 0L)
  expect_equal(unname(as.matrix(got_empty)), matrix(0, nrow = 3, ncol = 2))

  # All-true Bool sparse matrix: TRUE values recovered exactly.
  got_all_true <- get_matrix(d2, "cell", "gene", "alltrue_sm")
  expect_equal(unname(as.matrix(got_all_true)),
               unname(as.matrix(all_true_sm)))
  # On disk the all-true-bool case omits the nzval child entirely.
  expect_false(file.exists(file.path(dir, "matrices", "cell", "gene",
                                     "alltrue_sm", "nzval", "zarr.json")))
})
