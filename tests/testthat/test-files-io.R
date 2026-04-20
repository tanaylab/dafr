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

test_that(".write_descriptor_dense / .read_descriptor round-trip", {
  tmp <- tempfile(fileext = ".json")
  dafr:::.write_descriptor_dense(tmp, dtype = "Float64")
  on.exit(unlink(tmp))
  d <- dafr:::.read_descriptor(tmp)
  expect_equal(d$format, "dense")
  expect_equal(d$eltype, "Float64")
})

test_that(".write_descriptor_sparse / .read_descriptor round-trip", {
  tmp <- tempfile(fileext = ".json")
  dafr:::.write_descriptor_sparse(tmp, dtype = "Float64", indtype = "UInt32")
  on.exit(unlink(tmp))
  d <- dafr:::.read_descriptor(tmp)
  expect_equal(d$format, "sparse")
  expect_equal(d$eltype, "Float64")
  expect_equal(d$indtype, "UInt32")
})

test_that(".read_descriptor rejects malformed JSON", {
  tmp <- tempfile(fileext = ".json")
  writeLines('{"nope":true}', tmp); on.exit(unlink(tmp))
  expect_error(dafr:::.read_descriptor(tmp), "format|eltype")
})

test_that(".read_scalar_json returns typed value", {
  tmp <- tempfile(fileext = ".json")
  writeLines('{"type":"Float64","value":3.14}', tmp); on.exit(unlink(tmp))
  expect_equal(dafr:::.read_scalar_json(tmp), 3.14)
})

test_that(".write_scalar_json writes Julia-compatible format", {
  tmp <- tempfile(fileext = ".json")
  dafr:::.write_scalar_json(tmp, 42L)
  on.exit(unlink(tmp))
  raw <- readLines(tmp)
  expect_match(raw, '"type":\\s*"Int32"')
  expect_match(raw, '"value":\\s*42')
})

test_that(".write_bin_dense round-trips doubles (little-endian)", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  x <- c(1.5, 2.5, -3.25)
  dafr:::.write_bin_dense(tmp, x, dtype = "Float64")
  expect_equal(file.size(tmp), length(x) * 8L)
  out <- dafr:::.read_bin_dense(tmp, n = length(x), dtype = "Float64")
  expect_equal(out, x)
})

test_that(".write_bin_dense round-trips int32", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  x <- c(1L, -2L, 3L)
  dafr:::.write_bin_dense(tmp, x, dtype = "Int32")
  expect_equal(file.size(tmp), length(x) * 4L)
  out <- dafr:::.read_bin_dense(tmp, n = length(x), dtype = "Int32")
  expect_equal(out, x)
})

test_that(".write_bin_dense for logicals writes one byte per element", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  x <- c(TRUE, FALSE, TRUE)
  dafr:::.write_bin_dense(tmp, x, dtype = "Bool")
  expect_equal(file.size(tmp), length(x))
  out <- dafr:::.read_bin_dense(tmp, n = length(x), dtype = "Bool")
  expect_equal(out, x)
})

test_that(".indtype_for_size picks UInt32 vs UInt64", {
  expect_equal(dafr:::.indtype_for_size(2^30L), "UInt32")
  expect_equal(dafr:::.indtype_for_size(2^32),  "UInt64")
})

test_that(".should_sparsify_numeric picks sparse when nnz is small enough", {
  expect_true (dafr:::.should_sparsify_numeric(c(0,0,0,0,5),   "Float64", "UInt32"))
  expect_false(dafr:::.should_sparsify_numeric(c(1,2,3,4,5),   "Float64", "UInt32"))
  expect_false(dafr:::.should_sparsify_numeric(c(0,1,2,3,4,5), "Float64", "UInt32"))
})

test_that(".should_sparsify_numeric bool threshold", {
  expect_true (dafr:::.should_sparsify_numeric(c(rep(FALSE, 90), rep(TRUE, 10)),
                                               "Bool", "UInt32"))
  expect_false(dafr:::.should_sparsify_numeric(c(rep(FALSE, 80), rep(TRUE, 20)),
                                               "Bool", "UInt32"))
})

test_that(".should_sparsify_string applies Julia §8.4 formula", {
  indtype <- "UInt32"
  v <- c("hi", "", "hi", "", "hi")
  expect_false(dafr:::.should_sparsify_string(v, indtype))
  v2 <- c("hi", rep("", 20))
  expect_true(dafr:::.should_sparsify_string(v2, indtype))
})
