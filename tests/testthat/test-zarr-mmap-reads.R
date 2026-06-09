# Phase 2: dense ZarrDaf reads from a directory store should be mmap-backed
# (ALTREP), matching the FilesDaf typed-mmap fast path, instead of decoding the
# whole chunk into a fresh R vector on every read. See
# benchmarks/efficiency-gaps-results.md (the 63x read gap).

test_that("dense Float64 ZarrDaf directory-store reads are mmap-backed", {
    skip_if_not(isTRUE(dafr:::dafr_opt("dafr.mmap")), "mmap disabled")
    path <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)

    d <- zarr_daf(path, "w")
    add_axis(d, "cell", sprintf("c%d", 1:5))
    add_axis(d, "gene", sprintf("g%d", 1:3))
    set_vector(d, "cell", "x", as.double(1:5))
    set_matrix(d, "cell", "gene", "M", matrix(as.double(1:15), 5, 3))
    rm(d)

    d2 <- zarr_daf(path, "r")
    v <- get_vector(d2, "cell", "x")
    expect_true(is_altrep(v))                       # zero-copy mmap, not a decoded copy
    expect_equal(unname(v), as.double(1:5))

    m <- get_matrix(d2, "cell", "gene", "M")
    expect_true(is_altrep(m))
    expect_equal(as.matrix(m), matrix(as.double(1:15), 5, 3), ignore_attr = TRUE)
})

test_that("dense Int32 ZarrDaf directory-store reads are mmap-backed", {
    skip_if_not(isTRUE(dafr:::dafr_opt("dafr.mmap")), "mmap disabled")
    path <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)

    d <- zarr_daf(path, "w")
    add_axis(d, "cell", sprintf("c%d", 1:4))
    set_vector(d, "cell", "i", 10:13)               # integer -> <i4
    rm(d)

    d2 <- zarr_daf(path, "r")
    v <- get_vector(d2, "cell", "i")
    expect_true(is_altrep(v))
    expect_equal(unname(v), 10:13)
})

test_that("non-mmap-able ZarrDaf stores still read correctly (decode fallback)", {
    # An in-memory DictStore has no chunk file to mmap -> must fall back to
    # decode and still return correct values (not ALTREP).
    d <- zarr_daf(":memory:", "w")
    add_axis(d, "cell", sprintf("c%d", 1:5))
    set_vector(d, "cell", "x", as.double(1:5))
    rm(d)
    # Re-read from the same in-memory store is not possible (it is gone with d),
    # so build + read within one writer: the read path still exercises decode.
    d2 <- zarr_daf(":memory:", "w")
    add_axis(d2, "cell", sprintf("c%d", 1:5))
    set_vector(d2, "cell", "x", as.double(1:5))
    empty_cache(d2)  # drop the set value so the read goes through the store
    v <- get_vector(d2, "cell", "x")
    expect_false(is_altrep(v))                       # DictStore: no file -> decode
    expect_equal(unname(v), as.double(1:5))
})
