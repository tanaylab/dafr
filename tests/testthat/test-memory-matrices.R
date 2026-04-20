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

test_that("get_matrix returns matrix with axis-name dimnames", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  format_set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 2, 3), overwrite = FALSE)
  m <- get_matrix(d, "cell", "gene", "UMIs")
  expect_equal(rownames(m), c("A", "B"))
  expect_equal(colnames(m), c("X", "Y", "Z"))
})

test_that("get_matrix falls back to transposed layout when only the other is stored", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  format_set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 2, 3), overwrite = FALSE)
  expect_false(has_matrix(d, "gene", "cell", "UMIs"))
  m <- get_matrix(d, "gene", "cell", "UMIs")
  expect_equal(dim(m), c(3L, 2L))
  expect_equal(rownames(m), c("X", "Y", "Z"))
  expect_equal(colnames(m), c("A", "B"))
})

test_that("get_matrix default returns a constant-valued dimnamed matrix", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y"))
  expect_error(get_matrix(d, "cell", "gene", "missing"), "does not exist")
  m <- get_matrix(d, "cell", "gene", "missing", default = NA)
  expect_equal(dim(m), c(2L, 2L))
  expect_equal(rownames(m), c("A", "B"))
  expect_equal(colnames(m), c("X", "Y"))
  expect_true(all(is.na(m)))
})

test_that("get_matrix hits the cache on repeated reads and invalidates on overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A")); add_axis(d, "gene", c("X"))
  format_set_matrix(d, "cell", "gene", "UMIs", matrix(1, 1, 1), overwrite = FALSE)
  first  <- get_matrix(d, "cell", "gene", "UMIs")
  second <- get_matrix(d, "cell", "gene", "UMIs")
  expect_identical(first, second)
  format_set_matrix(d, "cell", "gene", "UMIs", matrix(2, 1, 1), overwrite = TRUE)
  third <- get_matrix(d, "cell", "gene", "UMIs")
  expect_equal(as.numeric(third), 2)
})

test_that("get_matrix with sparse input returns sparse with dimnames", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- Matrix::Matrix(c(0, 1, 2, 0, 0, 3), 2, 3, sparse = TRUE)
  format_set_matrix(d, "cell", "gene", "UMIs", m, overwrite = FALSE)
  got <- get_matrix(d, "cell", "gene", "UMIs")
  expect_s4_class(got, "dgCMatrix")
  expect_equal(rownames(got), c("A", "B"))
  expect_equal(colnames(got), c("X", "Y", "Z"))
})

test_that("get_matrix primary + flipped calls share one cache entry", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  format_set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 2, 3), overwrite = FALSE)
  cache_env <- S7::prop(d, "cache")

  expect_equal(length(ls(cache_env$memory)), 0L)
  primary <- get_matrix(d, "cell", "gene", "UMIs")
  keys_after_primary <- ls(cache_env$memory)
  expect_equal(keys_after_primary, cache_key_matrix("cell", "gene", "UMIs"))

  flipped <- get_matrix(d, "gene", "cell", "UMIs")
  # Same single cache key — no second entry for the flipped view.
  expect_equal(ls(cache_env$memory), keys_after_primary)

  # Orientations disagree, dimnames disagree.
  expect_equal(dim(primary), c(2L, 3L))
  expect_equal(dim(flipped), c(3L, 2L))
  expect_equal(rownames(primary), c("A", "B"))
  expect_equal(rownames(flipped), c("X", "Y", "Z"))

  # Cached entry retains its null dimnames; subsequent primary call rebuilds
  # dimnames without polluting the stored copy.
  stored <- get(cache_key_matrix("cell", "gene", "UMIs"),
                envir = cache_env$memory, inherits = FALSE)$value
  expect_null(dimnames(stored))
})

test_that("get_matrix flipped + sparse preserves class and applies rotated dimnames", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- Matrix::Matrix(c(0, 1, 2, 0, 0, 3), 2, 3, sparse = TRUE)
  format_set_matrix(d, "cell", "gene", "UMIs", m, overwrite = FALSE)
  got <- get_matrix(d, "gene", "cell", "UMIs")
  expect_s4_class(got, "dgCMatrix")
  expect_equal(dim(got), c(3L, 2L))
  expect_equal(rownames(got), c("X", "Y", "Z"))
  expect_equal(colnames(got), c("A", "B"))
  expect_equal(as.matrix(got), t(as.matrix(m)), ignore_attr = TRUE)
})

test_that("set_matrix round-trips dense + sparse + respects overwrite", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- matrix(seq_len(6), 2, 3)
  set_matrix(d, "cell", "gene", "UMIs", m)
  expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
               m, ignore_attr = TRUE)
  expect_error(set_matrix(d, "cell", "gene", "UMIs", m), "already exists")
  set_matrix(d, "cell", "gene", "UMIs", m * 10, overwrite = TRUE)
  expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
               m * 10, ignore_attr = TRUE)
})

test_that("delete_matrix removes + respects must_exist", {
  d <- memory_daf()
  add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
  set_matrix(d, "cell", "gene", "m", matrix(1, 1, 1))
  delete_matrix(d, "cell", "gene", "m")
  expect_false(has_matrix(d, "cell", "gene", "m"))
  expect_error (delete_matrix(d, "cell", "gene", "m"),                     "does not exist")
  expect_silent(delete_matrix(d, "cell", "gene", "m", must_exist = FALSE))
})

test_that("relayout_matrix makes the flipped layout physical", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 2, 3))
  expect_false(has_matrix(d, "gene", "cell", "UMIs"))
  relayout_matrix(d, "cell", "gene", "UMIs")
  expect_true(has_matrix(d, "gene", "cell", "UMIs"))
})

test_that("get_matrix default accepts a numeric scalar and adds dimnames", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  m <- get_matrix(d, "cell", "gene", "UMIs", default = 1)
  expect_equal(dim(m), c(2L, 3L))
  expect_equal(rownames(m), c("A", "B"))
  expect_equal(colnames(m), c("X", "Y", "Z"))
  expect_true(all(m == 1))
})

test_that("get_matrix default accepts a matrix value and adds dimnames", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  custom <- matrix(c(1, 3, 5, 2, 4, 6), nrow = 2, ncol = 3)
  m <- get_matrix(d, "cell", "gene", "UMIs", default = custom)
  expect_equal(m, custom, ignore_attr = TRUE)
  expect_equal(rownames(m), c("A", "B"))
  expect_equal(colnames(m), c("X", "Y", "Z"))
})

test_that("set_matrix / get_matrix round-trip a sparseMatrix preserving explicit zeros", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y", "Z"))
  sm <- Matrix::sparseMatrix(
    i = c(1, 1, 1, 2, 2),
    j = c(1, 2, 3, 1, 2),
    x = c(0, 1, 2, 3, 4),
    dims = c(2, 3)
  )
  expect_equal(length(matrices_set(d, "cell", "gene")), 0)
  expect_false(has_matrix(d, "cell", "gene", "UMIs"))
  set_matrix(d, "cell", "gene", "UMIs", sm)
  expect_true(has_matrix(d, "cell", "gene", "UMIs"))
  expect_equal(matrices_set(d, "cell", "gene"), "UMIs")
  got <- get_matrix(d, "cell", "gene", "UMIs")
  expect_s4_class(got, "dgCMatrix")
  expect_equal(as.matrix(got), as.matrix(sm), ignore_attr = TRUE)
  expect_equal(got@x, sm@x)
  expect_equal(rownames(got), c("A", "B"))
  expect_equal(colnames(got), c("X", "Y", "Z"))
})

test_that("user-facing matrix wrappers accept columns_axis = by name", {
  d <- memory_daf()
  add_axis(d, "cell", c("A", "B"))
  add_axis(d, "gene", c("X", "Y"))
  m <- matrix(1:4, nrow = 2, ncol = 2)
  expect_silent(set_matrix(d, rows_axis = "cell", columns_axis = "gene",
                           name = "n", mat = m))
  expect_true(has_matrix(d, rows_axis = "cell", columns_axis = "gene", "n"))
  expect_equal(dim(get_matrix(d, rows_axis = "cell",
                              columns_axis = "gene", "n")), c(2L, 2L))
})
