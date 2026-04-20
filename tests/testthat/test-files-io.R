test_that("dtype_for_r_vector picks the on-disk type", {
  expect_equal(dafr:::.dtype_for_r_vector(c(1.5, 2.5)),   "Float64")
  expect_equal(dafr:::.dtype_for_r_vector(1:3),           "Int32")
  expect_equal(dafr:::.dtype_for_r_vector(c(TRUE, FALSE)), "Bool")
  expect_equal(dafr:::.dtype_for_r_vector(bit64::as.integer64(c(1, 2))), "Int64")
  expect_equal(dafr:::.dtype_for_r_vector(c("a", "b")),   "String")
})

test_that(".dtype_size returns on-disk bytes per element", {
  expect_equal(dafr:::.dtype_size("Bool"),    1L)
  expect_equal(dafr:::.dtype_size("Int32"),   4L)
  expect_equal(dafr:::.dtype_size("Int64"),   8L)
  expect_equal(dafr:::.dtype_size("Float64"), 8L)
  expect_error( dafr:::.dtype_size("String"), "no fixed byte size")
})

test_that(".dtype_canonical accepts lowercase aliases", {
  expect_equal(dafr:::.dtype_canonical("int32"),   "Int32")
  expect_equal(dafr:::.dtype_canonical("FLOAT64"), "Float64")
  expect_equal(dafr:::.dtype_canonical("Int"),     "Int64")
  expect_error(dafr:::.dtype_canonical("Banana"),  "unsupported")
})

test_that(".path_for_* builds store paths", {
  root <- "/tmp/store"
  expect_equal(dafr:::.path_scalar(root, "pi"),
               "/tmp/store/scalars/pi.json")
  expect_equal(dafr:::.path_axis(root, "cell"),
               "/tmp/store/axes/cell.txt")
  expect_equal(dafr:::.path_vector_dir(root, "cell"),
               "/tmp/store/vectors/cell")
  expect_equal(dafr:::.path_matrix_dir(root, "cell", "gene"),
               "/tmp/store/matrices/cell/gene")
})
