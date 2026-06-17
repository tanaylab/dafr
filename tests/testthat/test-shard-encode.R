test_that(".shard_effective_sizeof and threshold match Julia", {
    expect_equal(dafr:::.shard_effective_sizeof("float64"), 8L)
    expect_equal(dafr:::.shard_effective_sizeof("string"), 16L)
    # 1200 float64 = 9600 B >= 8192 -> pack
    expect_true(dafr:::.shard_should_pack(1200L, "float64", 8L))
    # 1000 float64 = 8000 B < 8192 -> flat
    expect_false(dafr:::.shard_should_pack(1000L, "float64", 8L))
})

test_that(".shard_inner_chunk_shape gives column-slab chunks", {
    expect_equal(dafr:::.shard_inner_chunk_shape(c(1200L), "float64", 8L), 1024L)
    expect_equal(dafr:::.shard_inner_chunk_shape(c(1200L, 8L), "float64", 8L),
                 c(1024L, 1L))
})
