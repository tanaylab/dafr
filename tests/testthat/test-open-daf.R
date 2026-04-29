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

test_that("open_daf rejects zarr URIs (slice-16 placeholder)", {
    expect_error(open_daf("/some/path.daf.zarr"),
                 "lands in slice 16")
    expect_error(open_daf("/some/path.daf.zarr.zip"),
                 "lands in slice 16")
})

test_that("open_daf rejects http URIs (slice-18 placeholder)", {
    expect_error(open_daf("http://example.com/foo"),
                 "lands in slice 18")
    expect_error(open_daf("https://example.com/foo"),
                 "lands in slice 18")
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
