test_that("new_http_store fails when .zmetadata is unreachable", {
    skip_on_cran()
    expect_error(new_http_store("http://localhost:1/"),
                 "HTTP GET failed for: http://localhost:1/.zmetadata")
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
    # Construct a degenerate HttpStore directly (bypass the zmetadata
    # fetch) to test the read-only enforcement methods.
    s <- HttpStore(
        url = "http://example.com/foo",
        zmetadata = list(metadata = list(), zarr_consolidated_format = 1L),
        chunk_cache = new.env(parent = emptyenv())
    )
    expect_error(store_set_bytes(s, "x", as.raw(0L)),
                 "HttpStore is read-only")
    expect_error(store_delete(s, "x"),
                 "HttpStore is read-only")
})
