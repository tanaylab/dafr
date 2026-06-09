test_that("open_daf() with no uri returns a memory_daf", {
    d <- open_daf()
    expect_s7_class(d, MemoryDaf)
})

test_that("open_daf('memory://') returns a memory_daf", {
    d <- open_daf("memory://")
    expect_s7_class(d, MemoryDaf)
})

test_that("open_daf with name argument respects it on memory_daf", {
    d <- open_daf("memory://", name = "demo")
    expect_identical(S7::prop(d, "name"), "demo")
})

test_that("open_daf with a filesystem path returns a files_daf", {
    tmp <- tempfile()
    dir.create(tmp)
    d <- open_daf(tmp, mode = "w")
    expect_s7_class(d, FilesDaf)
})

test_that("open_daf with .daf.zarr URI returns a ZarrDaf", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- open_daf(tmp, mode = "w")
    expect_s7_class(d, ZarrDaf)
})

test_that("open_daf with .daf.zarr URI mode='r' opens existing", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d_w <- zarr_daf(tmp, mode = "w")
    add_axis(d_w, "cell", c("A", "B"))
    rm(d_w)
    d <- open_daf(tmp, mode = "r")
    expect_s7_class(d, ZarrDafReadOnly)
    expect_identical(axis_vector(d, "cell"), c("A", "B"))
})

test_that("open_daf with name argument respects it for ZarrDaf", {
    tmp <- tempfile(fileext = ".daf.zarr")
    d <- open_daf(tmp, mode = "w", name = "demo-zarr")
    expect_identical(S7::prop(d, "name"), "demo-zarr")
})

test_that("open_daf rejects writable HTTP modes", {
    expect_error(open_daf("http://example.com/foo", mode = "w"),
                 "HTTP backend is read-only")
    expect_error(open_daf("http://example.com/foo", mode = "r+"),
                 "HTTP backend is read-only")
})

test_that("open_daf rejects HTTP zip-archive URLs with redirect message", {
    expect_error(open_daf("http://example.com/foo.daf.zarr.zip"),
                 "HTTP zip-archive URLs are not supported")
})

test_that("open_daf routes http(s):// to http_daf or zarr_daf based on suffix", {
    skip_on_cran()
    # localhost:1 is unreachable; we just verify that dispatch reached the
    # right backend's HTTP fetch (different paths for HttpDaf vs HttpStore).
    expect_error(open_daf("http://localhost:1/served"),
                 "HTTP GET failed for: http://localhost:1/served/metadata.zip")
    expect_error(open_daf("http://localhost:1/foo.daf.zarr/", mode = "r"),
                 "HTTP GET failed for: http://localhost:1/foo.daf.zarr/zarr.json")
})

test_that("open_daf validates uri shape", {
    expect_error(open_daf(uri = c("a", "b")),
                 "single character scalar")
    expect_error(open_daf(uri = NA_character_),
                 "single character scalar")
    expect_error(open_daf(uri = 42),
                 "single character scalar")
})

test_that("open_daf works end-to-end (smoke test)", {
    d <- open_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_scalar(d, "n", 7L)
    expect_equal(get_scalar(d, "n"), 7L)
    expect_equal(unname(axis_vector(d, "cell")), c("c1", "c2"))
})
