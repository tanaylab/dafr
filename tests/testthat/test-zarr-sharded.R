# Unit tests for the Zarr v3 sharded reader against committed DAF 0.3.0
# `packed=true` fixtures. The gzip fixture is used because gzip inner chunks
# decode with base-R memDecompress - no optional c-blosc/libzstd needed - so
# the index parse, crc32c check, inner-codec dispatch, and reassembly logic are
# exercised on every build.

.packed_fixture <- function(codec) {
    short <- c(gzip = "gz", zstd = "zs",
               blosc_zstd_bitshuffle = "bz", blosc_lz4_bitshuffle = "bl")[[codec]]
    new_dir_store(testthat::test_path(
        sprintf("fixtures/zpk/%s.daf.zarr", short)))
}

test_that("a packed dense vector is detected as sharded", {
    store <- .packed_fixture("gzip")
    node <- zarr_v3_read_array(store, "vectors/cell/score")
    expect_true(.zarr_is_sharded(node))
    # A small flat string vector in the same store is NOT sharded.
    flat <- zarr_v3_read_array(store, "axes/cell")
    expect_false(.zarr_is_sharded(flat))
})

test_that("the start shard index parses to the right number of inner chunks", {
    store <- .packed_fixture("gzip")
    node <- zarr_v3_read_array(store, "vectors/cell/score")
    cfg <- .zarr_sharding_config(node)
    shard <- store_get_bytes(store, zarr_v3_chunk_path("vectors/cell/score", 1L))
    idx <- .zarr_shard_index(shard, node, cfg)
    expect_equal(nrow(idx), 2L)               # ceil(1200/1024)
    expect_true(all(idx$offset > 0))
    expect_true(all(idx$nbytes > 0))
})

test_that("a single gzip inner chunk decodes to its raw elements", {
    store <- .packed_fixture("gzip")
    node <- zarr_v3_read_array(store, "vectors/cell/score")
    cfg <- .zarr_sharding_config(node)
    shard <- store_get_bytes(store, zarr_v3_chunk_path("vectors/cell/score", 1L))
    idx <- .zarr_shard_index(shard, node, cfg)
    raw0 <- .zarr_shard_chunk_bytes(shard, idx, 1L)
    elem <- .zarr_inner_decode(raw0, cfg, n_elem = 1024L, dtype = node$data_type)
    expect_equal(length(elem), 1024L)
    expect_equal(elem[1:3], c(1, 2, 3))       # score = 1:1200
})

test_that("a sharded 1-D vector reassembles fully (gzip)", {
    store <- .packed_fixture("gzip")
    node <- zarr_v3_read_array(store, "vectors/cell/score")
    v <- .zarr_read_sharded_vector(store, "vectors/cell/score", node)
    expect_equal(length(v), 1200L)
    expect_equal(v, as.numeric(1:1200))
})

test_that("a sharded 2-D dense matrix reassembles column-major (gzip)", {
    store <- .packed_fixture("gzip")
    node <- zarr_v3_read_array(store, "matrices/cell/gene/dense")
    m <- .zarr_read_sharded_matrix(store, "matrices/cell/gene/dense", node)
    expect_equal(dim(m), c(1200L, 8L))
    expect_equal(as.numeric(m), as.numeric(1:(1200 * 8)))
})
