# tests/testthat/test-zarr-v3.R
test_that("zarr_v3 dtype mapping covers R types and v3 names", {
  expect_equal(zarr_v3_dtype_for_r("x"), "string")
  expect_equal(zarr_v3_dtype_for_r(TRUE), "bool")
  expect_equal(zarr_v3_dtype_for_r(bit64::as.integer64(1)), "int64")
  expect_equal(zarr_v3_dtype_for_r(1L), "int32")
  expect_equal(zarr_v3_dtype_for_r(1.5), "float64")

  expect_equal(zarr_v3_r_kind_for_dtype("float64"), "double")
  expect_equal(zarr_v3_r_kind_for_dtype("float32"), "double")
  expect_equal(zarr_v3_r_kind_for_dtype("int32"), "integer")
  expect_equal(zarr_v3_r_kind_for_dtype("uint8"), "integer")
  expect_equal(zarr_v3_r_kind_for_dtype("int64"), "integer64")
  expect_equal(zarr_v3_r_kind_for_dtype("uint64"), "integer64")
  expect_equal(zarr_v3_r_kind_for_dtype("bool"), "logical")
  expect_equal(zarr_v3_r_kind_for_dtype("string"), "character")

  expect_equal(zarr_v3_size_for_dtype("float64"), 8L)
  expect_equal(zarr_v3_size_for_dtype("int32"), 4L)
  expect_equal(zarr_v3_size_for_dtype("int64"), 8L)
  expect_equal(zarr_v3_size_for_dtype("uint8"), 1L)
  expect_true(is.na(zarr_v3_size_for_dtype("string")))
})

test_that("zarr_v3 chunk keys use c/ prefix and / separator", {
  expect_equal(zarr_v3_chunk_key(1L), "c/0")
  expect_equal(zarr_v3_chunk_key(2L), "c/0/0")
  # path-qualified
  expect_equal(zarr_v3_chunk_path("vectors/cell/score", 1L), "vectors/cell/score/c/0")
  expect_equal(zarr_v3_chunk_path("matrices/cell/gene/expr", 2L),
               "matrices/cell/gene/expr/c/0/0")
})

test_that("zarr_v3 array metadata round-trips through a DictStore", {
  store <- new_dict_store()
  meta <- zarr_v3_array_meta(shape = c(3L), dtype = "float64")
  zarr_v3_write_array(store, "vectors/cell/score", meta)

  # the per-node file is exactly zarr.json
  expect_true(store_exists(store, "vectors/cell/score/zarr.json"))
  # ancestor groups got group markers
  expect_true(store_exists(store, "vectors/zarr.json"))
  expect_true(store_exists(store, "vectors/cell/zarr.json"))

  rt <- zarr_v3_read_array(store, "vectors/cell/score")
  expect_equal(rt$node_type, "array")
  expect_equal(rt$zarr_format, 3L)
  expect_equal(as.integer(rt$shape[[1L]]), 3L)
  expect_equal(rt$data_type, "float64")
  expect_equal(as.integer(rt$chunk_grid$configuration$chunk_shape[[1L]]), 3L)
  expect_equal(rt$codecs[[1L]]$name, "bytes")
  expect_equal(rt$codecs[[1L]]$configuration$endian, "little")

  smeta <- zarr_v3_array_meta(shape = c(2L), dtype = "string")
  expect_equal(smeta$codecs[[1L]]$name, "vlen-utf8")
  expect_equal(smeta$data_type, "string")
  expect_equal(smeta$fill_value, "")
})
