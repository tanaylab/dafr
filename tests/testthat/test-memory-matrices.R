test_that("format_has_matrix / format_matrices_set empty case", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  expect_false(format_has_matrix(d, "cell", "gene", "UMIs"))
  expect_equal(format_matrices_set(d, "cell", "gene"), character(0L))
})

test_that("format_get_matrix returns stored dense matrix unchanged", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(seq_len(6), nrow = 2, ncol = 3)
  matrices <- S7::prop(d, "internal")$matrices
  matrices$cell <- new.env(parent = emptyenv())
  matrices$cell$gene <- new.env(parent = emptyenv())
  matrices$cell$gene$UMIs <- m
  expect_true(format_has_matrix(d, "cell", "gene", "UMIs"))
  expect_equal(format_matrices_set(d, "cell", "gene"), "UMIs")
  expect_identical(format_get_matrix(d, "cell", "gene", "UMIs"), m)
})

test_that("format_get_matrix errors on unknown axes / missing matrix", {
  d <- memory_daf()
  expect_error(format_get_matrix(d, "cell", "gene", "UMIs"), "axis .* does not exist")
  add_axis(d, "cell", "A")
  expect_error(format_get_matrix(d, "cell", "gene", "UMIs"), "axis .* does not exist")
  add_axis(d, "gene", "X")
  expect_error(format_get_matrix(d, "cell", "gene", "UMIs"), "matrix .* does not exist")
})

test_that("format_set_matrix accepts dense double / int / logical with correct shape", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m_d <- matrix(seq_len(6) + 0.5, nrow = 2, ncol = 3)
  m_i <- matrix(seq_len(6),       nrow = 2, ncol = 3)
  m_l <- matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), nrow = 2, ncol = 3)
  format_set_matrix(d, "cell", "gene", "d", m_d, overwrite = FALSE)
  format_set_matrix(d, "cell", "gene", "i", m_i, overwrite = FALSE)
  format_set_matrix(d, "cell", "gene", "l", m_l, overwrite = FALSE)
  expect_identical(format_get_matrix(d, "cell", "gene", "d"), m_d)
  expect_identical(format_get_matrix(d, "cell", "gene", "i"), m_i)
  expect_identical(format_get_matrix(d, "cell", "gene", "l"), m_l)
})

test_that("format_set_matrix accepts dgCMatrix + lgCMatrix sparse", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m_d <- Matrix::Matrix(c(0, 1, 2, 0, 0, 3), nrow = 2, ncol = 3, sparse = TRUE)
  m_l <- as(m_d != 0, "lgCMatrix")
  expect_s4_class(m_d, "dgCMatrix")
  expect_s4_class(m_l, "lgCMatrix")
  format_set_matrix(d, "cell", "gene", "d", m_d, overwrite = FALSE)
  format_set_matrix(d, "cell", "gene", "l", m_l, overwrite = FALSE)
  expect_equal(as.matrix(format_get_matrix(d, "cell", "gene", "d")), as.matrix(m_d))
  expect_equal(as.matrix(format_get_matrix(d, "cell", "gene", "l")), as.matrix(m_l))
})

test_that("format_set_matrix rejects shape mismatch / non-matrix / overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  expect_error(format_set_matrix(d, "cell", "gene", "m",
                                 matrix(0, 3, 3), overwrite = FALSE),
               "dim .*expected 2 x 3")
  expect_error(format_set_matrix(d, "cell", "gene", "m",
                                 c(1, 2, 3, 4, 5, 6), overwrite = FALSE),
               "not a matrix")
  format_set_matrix(d, "cell", "gene", "m", matrix(0, 2, 3), overwrite = FALSE)
  expect_error(format_set_matrix(d, "cell", "gene", "m",
                                 matrix(1, 2, 3), overwrite = FALSE),
               "already exists")
  format_set_matrix(d, "cell", "gene", "m", matrix(1, 2, 3), overwrite = TRUE)
  expect_equal(format_get_matrix(d, "cell", "gene", "m"), matrix(1, 2, 3))
})

test_that("format_set_matrix strips dimnames at storage layer", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(seq_len(6), 2, 3, dimnames = list(c("A", "B"), c("X", "Y", "Z")))
  format_set_matrix(d, "cell", "gene", "m", m, overwrite = FALSE)
  got <- format_get_matrix(d, "cell", "gene", "m")
  expect_null(dimnames(got))
})

test_that("format_set_matrix bumps matrix version counter", {
  d <- memory_daf()
  add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
  mc <- S7::prop(d, "matrix_version_counter")
  format_set_matrix(d, "cell", "gene", "m", matrix(1, 1, 1), overwrite = FALSE)
  expect_equal(mc[["cell:gene:m"]], 1L)
  format_set_matrix(d, "cell", "gene", "m", matrix(2, 1, 1), overwrite = TRUE)
  expect_equal(mc[["cell:gene:m"]], 2L)
})

test_that("format_delete_matrix removes + respects must_exist", {
  d <- memory_daf()
  add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
  format_set_matrix(d, "cell", "gene", "m", matrix(1, 1, 1), overwrite = FALSE)
  format_delete_matrix(d, "cell", "gene", "m", must_exist = TRUE)
  expect_false(format_has_matrix(d, "cell", "gene", "m"))
  expect_error (format_delete_matrix(d, "cell", "gene", "m", must_exist = TRUE),  "does not exist")
  expect_silent(format_delete_matrix(d, "cell", "gene", "m", must_exist = FALSE))
})

test_that("format_relayout_matrix writes the transposed layout", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(seq_len(6), 2, 3)
  format_set_matrix(d, "cell", "gene", "UMIs", m, overwrite = FALSE)
  expect_false(format_has_matrix(d, "gene", "cell", "UMIs"))
  format_relayout_matrix(d, "cell", "gene", "UMIs")
  expect_true(format_has_matrix(d, "gene", "cell", "UMIs"))
  expect_equal(format_get_matrix(d, "gene", "cell", "UMIs"), t(m))
})

test_that("format_relayout_matrix works for sparse (CSC -> transposed CSC)", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- Matrix::Matrix(c(0, 1, 2, 0, 0, 3), 2, 3, sparse = TRUE)
  format_set_matrix(d, "cell", "gene", "UMIs", m, overwrite = FALSE)
  format_relayout_matrix(d, "cell", "gene", "UMIs")
  got <- format_get_matrix(d, "gene", "cell", "UMIs")
  expect_s4_class(got, "dgCMatrix")
  expect_equal(as.matrix(got), as.matrix(Matrix::t(m)))
})

test_that("format_relayout_matrix errors when source matrix missing", {
  d <- memory_daf()
  add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
  expect_error(format_relayout_matrix(d, "cell", "gene", "UMIs"),
               "does not exist")
})
