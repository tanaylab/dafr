# Slice 17 phase 7: reserve / patch_crc — two-phase fill protocol.
#
# `reserve(key, size)` runs the full append commit with placeholder
# CRC32 = 0 and returns a writable ALTREP RAW view of the just-reserved
# data region. The caller fills the bytes in place, then calls
# `patch_crc(key)` which computes the real CRC and patches both the LFH
# and CD CRC fields. If the process crashes after `reserve` and before
# `patch_crc`, the next write-mode open detects the placeholder-CRC
# mismatch and rolls the entry back via the same recovery path used for
# tick-#3/#4 crashes.

reserve_view <- function(store, key, size) {
    dafr:::dafr_mmap_zip_reserve(S7::prop(store, "xptr"), key, as.numeric(size))
}

patch_crc <- function(store, key) {
    dafr:::dafr_mmap_zip_patch_crc(S7::prop(store, "xptr"), key)
}

test_that("reserve returns a writable ALTREP RAW view of the data region", {
    skip_if_no_mmap_zip()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    v <- reserve_view(s, "k", 16L)
    expect_true(is.raw(v))
    expect_true(is_altrep(v))
    expect_equal(length(v), 16L)
})

test_that("write into the reserved view, patch_crc, close, reopen — verify bytes", {
    skip_if_no_mmap_zip()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    payload <- as.raw(c(0xde, 0xad, 0xbe, 0xef, 0x00, 0xff, 0x42, 0x10))
    v <- reserve_view(s, "k", length(payload))
    # Writable in place: the assignment goes directly into the mmap.
    for (i in seq_along(payload)) v[[i]] <- payload[[i]]
    patch_crc(s, "k")
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "k")
    got <- store_get_bytes(s2, "k")
    expect_identical(as.raw(got), payload)
})

test_that("reserve rejects overwriting an existing key", {
    skip_if_no_mmap_zip()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    v1 <- reserve_view(s, "k", 4L)
    for (i in 1:4) v1[[i]] <- as.raw(i)
    patch_crc(s, "k")
    expect_error(reserve_view(s, "k", 4L), "append-only|cannot overwrite")
})

test_that("reserve rejects on read-only stores", {
    skip_if_no_mmap_zip()
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "v"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    expect_error(reserve_view(s, "new", 4L), "read-only|cannot reserve")
})

test_that("reserve of size 0 succeeds and patch_crc is a no-op verification", {
    skip_if_no_mmap_zip()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    v <- reserve_view(s, "empty", 0L)
    expect_equal(length(v), 0L)
    patch_crc(s, "empty")
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    got <- store_get_bytes(s2, "empty")
    expect_equal(length(got), 0L)
})

test_that("multiple reserve+patch round-trips preserve append order", {
    skip_if_no_mmap_zip()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    payloads <- list(
        a = charToRaw("alpha"),
        b = as.raw(c(0x01, 0x02, 0x03, 0x04)),
        c = charToRaw("gamma-payload-with-text")
    )
    for (nm in names(payloads)) {
        p <- payloads[[nm]]
        v <- reserve_view(s, nm, length(p))
        for (i in seq_along(p)) v[[i]] <- p[[i]]
        patch_crc(s, nm)
    }
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), names(payloads))
    for (nm in names(payloads)) {
        expect_identical(as.raw(store_get_bytes(s2, nm)), payloads[[nm]],
                         info = paste0("entry: ", nm))
    }
})

# Recovery test: write via reserve, fail to patch_crc, reopen 'r+' triggers
# rollback. Gated on NOT_CRAN to match other recovery tests.
test_that("reserve without patch_crc rolls back on r+ reopen", {
    skip_if_no_mmap_zip()
    if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
        testthat::skip_on_cran()
    }
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    # First commit a regular entry so we have something to keep.
    store_set_bytes(s, "kept", charToRaw("survives"))

    # Now reserve + write bytes BUT don't patch CRC.
    v <- reserve_view(s, "doomed", 8L)
    payload <- as.raw(c(1, 2, 3, 4, 5, 6, 7, 8))
    for (i in seq_along(payload)) v[[i]] <- payload[[i]]
    # Note: NO patch_crc call. The CD/LFH still hold CRC=0 which won't
    # match CRC32 of the payload above.
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    rm(s, v); gc()

    # Reopen r+: recovery sees the trailing 'doomed' entry as invalid
    # (CD CRC=0 ≠ data CRC) and rolls it back, keeping only 'kept'.
    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "kept")
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "kept"))), "survives")
})
