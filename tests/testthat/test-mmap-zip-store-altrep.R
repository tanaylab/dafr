# Slice 17 phase 3: ALTREP RAWSXP backed by MmapZipStore::file_mmap, with
# close-time deactivation safety net. Stored entries return zero-copy
# ALTREP views; deflate entries return ordinary RAWSXP. After close,
# every previously produced ALTREP must read as raw(0) — no segfault.

test_that("store_get_bytes returns an ALTREP RAW for stored entries", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "the-payload"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    v <- store_get_bytes(s, "k")
    expect_true(is.raw(v))
    expect_true(is_altrep(v))
})

test_that("ALTREP RAW reads byte-identical to source", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    payload <- as.raw(c(0x00, 0xff, 0x42, 0x10, 0x80, 0x7f, 0x01))
    build_zip(path, list("bytes" = payload), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    v <- store_get_bytes(s, "bytes")
    expect_true(is_altrep(v))
    expect_identical(v, payload)
})

test_that("ALTREP RAW survives length() / element access / `[`", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    payload <- as.raw(seq.int(0L, 255L))
    build_zip(path, list("seq" = payload), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    v <- store_get_bytes(s, "seq")
    expect_equal(length(v), 256L)
    expect_identical(v[[1L]], as.raw(0L))
    expect_identical(v[[256L]], as.raw(255L))
    expect_identical(v[10:12], payload[10:12])
    # Round-trip via integer for full coverage of the Get_region path.
    expect_identical(as.integer(v), as.integer(payload))
})

test_that("close-time deactivation: vector reads as raw(0) after store close", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "abcdef"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    v <- store_get_bytes(s, "k")
    expect_equal(length(v), 6L)
    # Explicit close: deactivates the slot and unmaps the file.
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    gc()
    expect_no_error(len <- length(v))
    expect_equal(len, 0L)
    expect_no_error(out <- as.integer(v))
    expect_identical(out, integer(0))
})

test_that("close-time deactivation: vector accepted by base ops without segfault", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "deadbeef"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    v <- store_get_bytes(s, "k")
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    gc()
    expect_no_error(s_ch <- as.character(v))
    expect_equal(length(s_ch), 0L)
    expect_no_error(joined <- paste(v, collapse = ""))
    expect_equal(joined, "")
    expect_no_error(total <- sum(as.integer(v)))
    expect_equal(total, 0L)
})

test_that("write-on-ALTREP materializes a copy and leaves the on-disk bytes untouched", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    payload <- as.raw(c(0x10, 0x20, 0x30, 0x40))
    build_zip(path, list("k" = payload), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    v <- store_get_bytes(s, "k")
    expect_true(is_altrep(v))
    # Force materialization via writeable assignment.
    v[[1L]] <- as.raw(0xff)
    expect_identical(v, as.raw(c(0xff, 0x20, 0x30, 0x40)))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    # Reopen and verify the on-disk bytes didn't change.
    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    v2 <- store_get_bytes(s2, "k")
    expect_identical(as.raw(v2), payload)
})

test_that("decompressed entries return a regular RAWSXP, not ALTREP", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    payload <- paste(rep("the quick brown fox ", 50L), collapse = "")
    build_zip(path, list("d" = payload), compression = "deflate")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    v <- store_get_bytes(s, "d")
    expect_true(is.raw(v))
    expect_false(is_altrep(v))
    expect_identical(rawToChar(v), payload)
})

test_that("garbage collection of ALTREP without close doesn't double-free", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "hello"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    local({
        v <- store_get_bytes(s, "k")
        expect_true(is_altrep(v))
        expect_equal(length(v), 5L)
    })
    # v has gone out of scope — GC should drop the ALTREP and decrement
    # the slot's shared_ptr refcount cleanly. Subsequent gets must still
    # work.
    gc()
    v2 <- store_get_bytes(s, "k")
    expect_equal(length(v2), 5L)
    expect_identical(rawToChar(as.raw(v2)), "hello")
})

test_that("ALTREP keeps the store alive even without an R-side reference", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "stay-alive"), compression = "stored")
    # Get an ALTREP and immediately drop the store from R-side scope.
    v <- local({
        s <- new_mmap_zip_store(path, mode = "r")
        store_get_bytes(s, "k")
    })
    gc()
    # The store xptr has no R-side reference, but the ALTREP's protected
    # slot keeps it alive — so reads must still succeed.
    expect_true(is_altrep(v))
    expect_identical(rawToChar(as.raw(v)), "stay-alive")
})

test_that("stress: open/get/close 10 stores, deref every vector, no segfault", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    paths <- replicate(10L, new_tempfile("zip"))
    payloads <- lapply(seq_len(10L), function(i) {
        as.raw(rep(i, 32L * i))
    })
    for (i in seq_along(paths)) {
        build_zip(paths[[i]], list("k" = payloads[[i]]), compression = "stored")
    }
    stores <- lapply(paths, function(p) new_mmap_zip_store(p, mode = "r"))
    vecs <- lapply(stores, function(s) store_get_bytes(s, "k"))
    # Sanity: every vector is ALTREP and reports the right length while
    # the store is alive. NB: `identical()` on a RAW vector forces full-
    # buffer access via DATAPTR, which triggers materialization (a private
    # copy) — so we deliberately avoid byte-comparison here, since the
    # whole point of the deactivation half is that the ORIGINAL ALTREP is
    # still alive and unmaterialized when its store closes.
    for (i in seq_along(vecs)) {
        expect_true(is_altrep(vecs[[i]]))
        expect_equal(length(vecs[[i]]), length(payloads[[i]]))
        expect_equal(vecs[[i]][[1L]], payloads[[i]][[1L]])
    }
    # Close every store explicitly.
    for (s in stores) {
        dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    }
    rm(stores)
    gc()
    # All vectors are now deactivated — must read as raw(0).
    for (v in vecs) {
        expect_equal(length(v), 0L)
        expect_no_error(as.integer(v))
    }
})
