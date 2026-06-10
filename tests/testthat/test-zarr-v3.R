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

test_that("zarr_v3 root marker carries daf attribute and reads back", {
  store <- new_dict_store()
  zarr_v3_write_root(store)
  expect_true(zarr_v3_daf_marker_exists(store))
  expect_equal(zarr_v3_daf_version(store), c(1L, 0L))

  # an empty dict store with no root is not a marker
  expect_false(zarr_v3_daf_marker_exists(new_dict_store()))
})

test_that("zarr_v3 consolidated metadata indexes non-root nodes", {
  store <- new_dict_store()
  zarr_v3_write_root(store)
  zarr_v3_write_group(store, "scalars")
  zarr_v3_write_array(store, "vectors/cell/score",
                      zarr_v3_array_meta(c(3L), "float64"))
  zarr_v3_write_consolidated(store)

  root <- zarr_v3_read_node(store, "")
  md <- root$consolidated_metadata$metadata
  expect_true("scalars" %in% names(md))
  expect_true("vectors/cell/score" %in% names(md))
  expect_equal(md[["vectors/cell/score"]]$node_type, "array")
  expect_false("" %in% names(md))   # root is not listed in its own index
  # the block shape the DAF reader keys on
  expect_equal(root$consolidated_metadata$kind, "inline")
  expect_false(root$consolidated_metadata$must_understand)
})

test_that("zarr_v3 numeric chunk encode/decode round-trips", {
  for (case in list(
      list(v = c(1.5, 2.5, 3.5), d = "float64"),
      list(v = c(1L, 2L, 3L), d = "int32"),
      list(v = bit64::as.integer64(c(1, 2, 3)), d = "int64"),
      list(v = c(TRUE, FALSE, TRUE), d = "bool"))) {
    enc <- zarr_v3_encode_chunk(case$v, case$d)
    dec <- zarr_v3_decode_chunk(enc, case$d, n = length(case$v))
    expect_equal(dec, case$v, info = case$d)
  }
})

test_that("zarr_v3 decodes float32 by widening to double", {
  raw <- writeBin(c(1.0, 3.0, 5.0), raw(), size = 4L, endian = "little")
  expect_equal(zarr_v3_decode_chunk(raw, "float32", n = 3L), c(1, 3, 5))
})

test_that("zarr_v3 vlen-utf8 strings round-trip", {
  enc <- zarr_v3_encode_strings(c("alpha", "", "βeta"))
  expect_equal(zarr_v3_decode_strings(enc, n = 3L), c("alpha", "", "βeta"))
})

test_that("zarr_v3_decode_chunk compressor branch resolves v3 name and errors clearly", {
  # v3 codec spec is keyed by `name`; an unsupported codec must raise a
  # non-empty, informative error naming the codec.
  expect_error(
    zarr_v3_decode_chunk(raw(8), "float64", n = 1L,
                         compressor = list(name = "zstd")),
    "zstd")
  # a compressor with neither name nor id is rejected explicitly
  expect_error(
    zarr_v3_decode_chunk(raw(8), "float64", n = 1L,
                         compressor = list(foo = "bar")),
    "no name/id")
  # the legacy v2 `id` key still works as a fallback
  expect_error(
    zarr_v3_decode_chunk(raw(8), "float64", n = 1L,
                         compressor = list(id = "blosc")),
    "blosc")
  # gzip is accepted via the v3 `name` key and decodes the payload
  payload <- zarr_v3_encode_chunk(c(1.5, 2.5, 3.5), "float64")
  gz <- memCompress(payload, type = "gzip")
  dec <- zarr_v3_decode_chunk(gz, "float64", n = 3L,
                              compressor = list(name = "gzip"))
  expect_equal(dec, c(1.5, 2.5, 3.5))
})

test_that("zarr_v3 string array round-trips through a DictStore", {
  store <- new_dict_store()
  vals <- c("alpha", "", "βeta")
  meta <- zarr_v3_array_meta(shape = c(length(vals)), dtype = "string")
  zarr_v3_write_array(store, "vectors/cell/name", meta)
  store_set_bytes(store, zarr_v3_chunk_path("vectors/cell/name", 1L),
                  zarr_v3_encode_strings(vals))

  rt <- zarr_v3_read_array(store, "vectors/cell/name")
  expect_equal(rt$data_type, "string")
  chunk <- store_get_bytes(store, zarr_v3_chunk_path("vectors/cell/name", 1L))
  expect_equal(zarr_v3_decode_strings(chunk, n = length(vals)), vals)
})

test_that("zarr_v3 empty attributes serialize as {} not []", {
  store <- new_dict_store()
  zarr_v3_write_array(store, "vectors/cell/score",
                      zarr_v3_array_meta(c(3L), "float64"))
  raw <- store_get_bytes(store, "vectors/cell/score/zarr.json")
  expect_match(rawToChar(raw), '"attributes":\\{\\}')
})
