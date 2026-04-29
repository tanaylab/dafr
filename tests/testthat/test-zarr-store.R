# Round-trip pattern, parameterized over store impls.
.zarr_store_round_trip_tests <- function(store_factory, label) {
    test_that(sprintf("%s: round-trip set / get / exists / delete", label), {
        s <- store_factory()
        expect_false(store_exists(s, "foo"))
        expect_null(store_get_bytes(s, "foo"))
        store_set_bytes(s, "foo", as.raw(c(1, 2, 3)))
        expect_true(store_exists(s, "foo"))
        expect_identical(store_get_bytes(s, "foo"), as.raw(c(1, 2, 3)))
        store_delete(s, "foo")
        expect_false(store_exists(s, "foo"))
        expect_null(store_get_bytes(s, "foo"))
    })

    test_that(sprintf("%s: nested paths via slashes", label), {
        s <- store_factory()
        store_set_bytes(s, "vectors/cell/x", as.raw(c(10)))
        store_set_bytes(s, "vectors/cell/y", as.raw(c(20)))
        store_set_bytes(s, "scalars/n", as.raw(c(42)))
        keys <- sort(store_list(s, ""))
        expect_true(all(c("scalars/n", "vectors/cell/x",
                          "vectors/cell/y") %in% keys))
    })

    test_that(sprintf("%s: store_list with prefix returns matching keys", label), {
        s <- store_factory()
        store_set_bytes(s, "vectors/cell/x", as.raw(c(1)))
        store_set_bytes(s, "vectors/cell/y", as.raw(c(2)))
        store_set_bytes(s, "scalars/n", as.raw(c(3)))
        v_keys <- sort(store_list(s, "vectors"))
        expect_setequal(v_keys, c("vectors/cell/x", "vectors/cell/y"))
        s_keys <- sort(store_list(s, "scalars"))
        expect_setequal(s_keys, "scalars/n")
    })

    test_that(sprintf("%s: store_delete on nonexistent key is silent no-op", label), {
        s <- store_factory()
        expect_silent(store_delete(s, "nonexistent"))
    })

    test_that(sprintf("%s: empty bytes round-trip", label), {
        s <- store_factory()
        store_set_bytes(s, "empty", raw(0L))
        expect_identical(store_get_bytes(s, "empty"), raw(0L))
        expect_true(store_exists(s, "empty"))
    })
}

.zarr_store_round_trip_tests(
    function() new_dict_store(),
    "DictStore"
)

.zarr_store_round_trip_tests(
    function() new_dir_store(tempfile()),
    "DirStore"
)

# MmapZipStore stub
test_that("new_mmap_zip_store errors pointing to slice 17", {
    expect_error(new_mmap_zip_store("/some/path.daf.zarr.zip"),
                 "lands in slice 17")
})

# Edge cases for DirStore: large bytes, binary data with all 256 byte values
test_that("DirStore: round-trip 1 KiB of mixed binary content", {
    s <- new_dir_store(tempfile())
    bytes <- as.raw(rep(0:255, length.out = 1024L))
    store_set_bytes(s, "blob", bytes)
    expect_identical(store_get_bytes(s, "blob"), bytes)
})

test_that("DictStore: round-trip 1 KiB of mixed binary content", {
    s <- new_dict_store()
    bytes <- as.raw(rep(0:255, length.out = 1024L))
    store_set_bytes(s, "blob", bytes)
    expect_identical(store_get_bytes(s, "blob"), bytes)
})

# Type / class checks
test_that("DirStore is a ZarrStore", {
    s <- new_dir_store(tempfile())
    expect_s7_class(s, ZarrStore)
    expect_s7_class(s, DirStore)
})

test_that("DictStore is a ZarrStore", {
    s <- new_dict_store()
    expect_s7_class(s, ZarrStore)
    expect_s7_class(s, DictStore)
})
