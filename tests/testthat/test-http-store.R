test_that("new_http_store fails when the root zarr.json is unreachable", {
    skip_on_cran()
    expect_error(new_http_store("http://localhost:1/"),
                 "HTTP GET failed for: http://localhost:1/zarr.json")
})

test_that("zarr_daf rejects writable HTTP modes", {
    expect_error(zarr_daf("http://example.com/foo.daf.zarr", mode = "w"),
                 "HTTP URLs are read-only")
    expect_error(zarr_daf("http://example.com/foo.daf.zarr", mode = "r+"),
                 "HTTP URLs are read-only")
    expect_error(zarr_daf("http://example.com/foo.daf.zarr", mode = "w+"),
                 "HTTP URLs are read-only")
})

test_that("HttpStore writes hard-error", {
    # Construct a degenerate HttpStore directly (bypass the network fetch) to
    # test the read-only enforcement methods.
    s <- HttpStore(
        url = "http://example.com/foo",
        index = list(),
        chunk_cache = new.env(parent = emptyenv())
    )
    expect_error(store_set_bytes(s, "x", as.raw(0L)),
                 "HttpStore is read-only")
    expect_error(store_delete(s, "x"),
                 "HttpStore is read-only")
})

test_that("HttpStore store_list/store_exists resolve against the v3 index", {
    # An in-memory index mirrors what new_http_store() parses from the root
    # consolidated metadata; no network access is exercised here.
    s <- HttpStore(
        url = "http://example.com/foo",
        index = list(
            "scalars" = list(node_type = "group"),
            "scalars/name" = list(node_type = "array"),
            "axes/cell" = list(node_type = "array"),
            "vectors/cell/score" = list(node_type = "array")
        ),
        chunk_cache = new.env(parent = emptyenv())
    )
    # store_exists answers metadata keys from the index without a network hit.
    expect_true(store_exists(s, "zarr.json"))
    expect_true(store_exists(s, "scalars/name/zarr.json"))
    expect_false(store_exists(s, "scalars/missing/zarr.json"))

    # store_list synthesises <node>/zarr.json keys under the prefix, mirroring
    # DirStore (root prefix also yields the root marker).
    expect_setequal(
        store_list(s, "scalars"),
        c("scalars/zarr.json", "scalars/name/zarr.json")
    )
    expect_setequal(store_list(s, "vectors/cell"),
                    "vectors/cell/score/zarr.json")
    expect_true("zarr.json" %in% store_list(s, ""))

    # store_get_bytes re-serialises an index node back to parseable JSON.
    raw <- store_get_bytes(s, "scalars/name/zarr.json")
    expect_type(raw, "raw")
    node <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
    expect_identical(node$node_type, "array")
    expect_null(store_get_bytes(s, "scalars/missing/zarr.json"))
})
