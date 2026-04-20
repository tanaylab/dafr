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
