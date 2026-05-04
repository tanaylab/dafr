# Slice 17 phase 5+6: write-mode-open recovery for partially-written
# MmapZipStore archives, plus crash-counter tick injection points
# inside `commit_new_entry` for deterministic rollback testing.
#
# Recovery model: on a writable open, walk the parsed entries from
# back to front. The first run of trailing entries that fail
# `entry_is_valid` (LFH signature missing OR data CRC mismatch) is
# rolled back: we rebuild the CD+EOCD at the oldest invalid entry's
# `local_file_header_offset` and `ftruncate` the file to the new EOA.
#
# Tests are gated on NOT_CRAN to keep the synthetic-crash injection
# (and the `gc()` between writable opens) off CRAN. The full SIGKILL
# stress test lives in a separate file (Phase 13).

skip_if_not_full_run <- function() {
    if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
        testthat::skip_on_cran()
    }
}

# Compute the central-directory offset of an existing zip by parsing
# its EOCD region. Returns a list with `cd_offset`, `cd_size`, and
# `total_entries`. We use this in clobber tests to find LFHs.
read_eocd <- function(path) {
    sz <- file.info(path)$size
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    seek(con, where = sz - 22L)
    eocd <- readBin(con, what = "raw", n = 22L)
    sig <- sum(as.integer(eocd[1:4]) * c(1L, 256L, 65536L, 16777216L))
    stopifnot(sig == 0x06054b50)
    # Legacy fields all sentinel since we always write ZIP64.
    # Pull cd_offset, cd_size, total_entries from the ZIP64 EOCD record
    # at sz - 22 - 20 - 56.
    z64_off <- sz - 22L - 20L - 56L
    seek(con, where = z64_off)
    z64 <- readBin(con, what = "raw", n = 56L)
    z64_sig <- sum(as.integer(z64[1:4]) * c(1L, 256L, 65536L, 16777216L))
    stopifnot(z64_sig == 0x06064b50)
    # offsets within ZIP64 EOCD: total_entries at +32 (8B), cd_size at
    # +40 (8B), cd_offset at +48 (8B).
    rd_u64 <- function(bytes) {
        # 8-byte little-endian; we only ever build small files in
        # tests, so this fits in R double.
        b <- as.integer(bytes)
        sum(b * (256 ^ (0:7)))
    }
    list(
        total_entries = rd_u64(z64[33:40]),
        cd_size       = rd_u64(z64[41:48]),
        cd_offset     = rd_u64(z64[49:56])
    )
}

# Clobber `n_bytes` bytes at file offset `offset` to `value` (default 0).
clobber_bytes <- function(path, offset, n_bytes, value = as.raw(0L)) {
    con <- file(path, "r+b")
    on.exit(close(con), add = TRUE)
    seek(con, where = offset, rw = "write")
    writeBin(rep(value, n_bytes), con)
    invisible()
}

# Return the LFH offset of the i-th entry (1-based) by walking the CD
# in CD order. Mirrors the C++ parser's CD walk just enough to find
# the offset for clobbering.
lfh_offset_of_entry <- function(path, idx) {
    eocd <- read_eocd(path)
    sz <- file.info(path)$size
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    cursor <- eocd$cd_offset
    rd_u16 <- function(bytes) {
        b <- as.integer(bytes)
        b[1] + b[2] * 256L
    }
    rd_u64 <- function(bytes) {
        b <- as.integer(bytes)
        sum(b * (256 ^ (0:7)))
    }
    for (i in seq_len(eocd$total_entries)) {
        seek(con, where = cursor)
        hdr <- readBin(con, what = "raw", n = 46L)
        name_len  <- rd_u16(hdr[29:30])
        extra_len <- rd_u16(hdr[31:32])
        comment_len <- rd_u16(hdr[33:34])
        # legacy LFH offset at +42 (4B); but always sentinel in our
        # writes — the actual offset is in the ZIP64 extra (last 8B).
        # Since we always emit a ZIP64 extra of fixed 28B (4B header +
        # 24B payload: uncomp(8) + comp(8) + lfh_off(8)), find the
        # ZIP64 extra by scanning the extras region.
        extra_start <- cursor + 46L + name_len
        seek(con, where = extra_start)
        ex <- readBin(con, what = "raw", n = extra_len)
        # First extra is the ZIP64 record (we always write it first).
        z64_payload <- ex[5:(5L + 24L - 1L)]
        lfh_off <- rd_u64(z64_payload[17:24])
        if (i == idx) return(lfh_off)
        cursor <- cursor + 46L + name_len + extra_len + comment_len
    }
    stop("entry index out of range")
}

# Return the data offset (mmap-base-relative) of entry `idx` (1-based)
# by reading its LFH. Used to clobber data bytes for CRC-mismatch
# tests.
data_offset_of_entry <- function(path, idx) {
    lfh_off <- lfh_offset_of_entry(path, idx)
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    seek(con, where = lfh_off)
    hdr <- readBin(con, what = "raw", n = 30L)
    rd_u16 <- function(bytes) {
        b <- as.integer(bytes)
        b[1] + b[2] * 256L
    }
    name_len  <- rd_u16(hdr[27:28])
    extra_len <- rd_u16(hdr[29:30])
    lfh_off + 30L + name_len + extra_len
}

# ---- Direct rollback (byte-clobber) tests --------------------------------

test_that("does not roll back valid entries", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "a", charToRaw("AAA"))
    store_set_bytes(s, "b", charToRaw("BBB"))
    store_set_bytes(s, "c", charToRaw("CCC"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    # Reopen writable — recovery runs; nothing to roll back.
    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), c("a", "b", "c"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "a"))), "AAA")
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "b"))), "BBB")
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "c"))), "CCC")
})

test_that("rolls back when LFH signature is missing on the last entry", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    store_set_bytes(s, "second", charToRaw("SECOND"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    # Clobber the LFH signature of entry 2.
    lfh2 <- lfh_offset_of_entry(path, 2L)
    clobber_bytes(path, lfh2, 4L)

    # Reopen writable — recovery should drop the second entry.
    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "first")
    expect_null(store_get_bytes(s2, "second"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "first"))), "FIRST")
})

test_that("rolls back when CRC mismatches on the last entry", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    store_set_bytes(s, "second", charToRaw("SECOND"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    # Flip a single byte in the data of entry 2.
    data2 <- data_offset_of_entry(path, 2L)
    con <- file(path, "r+b")
    seek(con, where = data2, rw = "read")
    orig <- readBin(con, what = "raw", n = 1L)
    seek(con, where = data2, rw = "write")
    writeBin(as.raw(bitwXor(as.integer(orig), 0xFFL)), con)
    close(con)

    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "first")
    expect_null(store_get_bytes(s2, "second"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "first"))), "FIRST")
})

test_that("rolls back multiple trailing invalid entries", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    for (i in 1:5) {
        store_set_bytes(s, paste0("e", i), charToRaw(sprintf("payload-%d", i)))
    }
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    # Clobber LFH signatures of entries 4 and 5.
    clobber_bytes(path, lfh_offset_of_entry(path, 4L), 4L)
    clobber_bytes(path, lfh_offset_of_entry(path, 5L), 4L)

    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), c("e1", "e2", "e3"))
    expect_null(store_get_bytes(s2, "e4"))
    expect_null(store_get_bytes(s2, "e5"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "e3"))), "payload-3")
})

test_that("read-only open does not roll back invalid trailing entries", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    store_set_bytes(s, "second", charToRaw("SECOND"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    clobber_bytes(path, lfh_offset_of_entry(path, 2L), 4L)

    # Read-only opens use the strict parser (no recovery). Reject the
    # corrupt LFH at parse time — read-mode does NOT silently truncate
    # the archive (that's a write-mode side effect by design).
    expect_error(
        new_mmap_zip_store(path, mode = "r"),
        "local file header signature missing|corrupt"
    )
})

test_that("after recovery, the archive can accept fresh appends", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    store_set_bytes(s, "doomed", charToRaw("DOOMED"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    clobber_bytes(path, lfh_offset_of_entry(path, 2L), 4L)

    # Reopen, recover, append a third entry.
    s2 <- new_mmap_zip_store(path, mode = "r+")
    expect_setequal(store_list(s2, ""), "first")
    store_set_bytes(s2, "third", charToRaw("THIRD"))
    dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr"))

    # Reopen and verify the rolled-back archive plus the new entry.
    s3 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s3, "xptr")), add = TRUE)
    expect_setequal(store_list(s3, ""), c("first", "third"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s3, "first"))), "FIRST")
    expect_identical(rawToChar(as.raw(store_get_bytes(s3, "third"))), "THIRD")
    expect_null(store_get_bytes(s3, "doomed"))
})

# ---- Tick-driven (synthetic crash) tests ---------------------------------
#
# The C++ append protocol calls `tick_crash_counter` at five named
# points — see `tick_crash_counter(impl)` in mmap_zip_store.cpp.
# Setting a counter of N causes the N-th tick to throw
# `SimulatedCrash`; subsequent ticks continue to fire (the counter
# wraps below zero), but execution has already left the function.
#
# Tick #1: BEFORE resize_file_overlay     — no on-disk change.
# Tick #2: AFTER resize, BEFORE CD copy   — file grew, sparse zeros at tail.
# Tick #3: AFTER CD copy, BEFORE LFH copy — new CD claims an entry whose
#          LFH still holds old CD bytes (signature mismatch). ROLLBACK.
# Tick #4: AFTER LFH copy, BEFORE data    — LFH valid, data is sparse zeros
#          (stored CRC won't match all-zero bytes for nonempty payload).
#          ROLLBACK.
# Tick #5: AFTER data copy                — entry is fully valid. No rollback.

with_crash_counter <- function(store, n) {
    cc <- dafr:::new_crash_counter(n)
    dafr:::dafr_mmap_zip_set_crash_counter(
        S7::prop(store, "xptr"), cc, getNamespace("dafr"))
    cc
}

test_that("tick #1 (before resize): SimulatedCrash leaves archive untouched", {
    skip_if_not_full_run()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    sz_before <- file.info(path)$size
    with_crash_counter(s, 1L)
    expect_error(
        store_set_bytes(s, "second", charToRaw("SECOND")),
        class = "SimulatedCrash"
    )
    # Drop the now-undefined-state store object.
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    rm(s); gc()

    # File size unchanged (tick fired before ftruncate).
    expect_equal(file.info(path)$size, sz_before)

    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "first")
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "first"))), "FIRST")
})

test_that("tick #2 (after resize, before CD copy): reopen rolls back cleanly", {
    skip_if_not_full_run()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    with_crash_counter(s, 2L)
    expect_error(
        store_set_bytes(s, "second", charToRaw("SECOND")),
        class = "SimulatedCrash"
    )
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    rm(s); gc()

    # File grew but the old EOCD was never overwritten; the parser
    # finds the old CD, which still claims one entry. Recovery either
    # is a no-op (only one valid entry) OR rebuilds CD at the new EOA
    # — either way, the archive is healthy.
    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "first")
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "first"))), "FIRST")
})

test_that("tick #3 (after CD copy, before LFH): rollback drops bad entry", {
    skip_if_not_full_run()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    with_crash_counter(s, 3L)
    expect_error(
        store_set_bytes(s, "second", charToRaw("SECOND")),
        class = "SimulatedCrash"
    )
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    rm(s); gc()

    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "first")
    expect_null(store_get_bytes(s2, "second"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "first"))), "FIRST")
})

test_that("tick #4 (after LFH, before data): rollback drops bad entry", {
    skip_if_not_full_run()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    with_crash_counter(s, 4L)
    expect_error(
        store_set_bytes(s, "second", charToRaw("SECOND")),
        class = "SimulatedCrash"
    )
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    rm(s); gc()

    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "first")
    expect_null(store_get_bytes(s2, "second"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "first"))), "FIRST")
})

test_that("tick #5 (after data copy): SimulatedCrash, archive is valid", {
    skip_if_not_full_run()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    with_crash_counter(s, 5L)
    expect_error(
        store_set_bytes(s, "second", charToRaw("SECOND")),
        class = "SimulatedCrash"
    )
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    rm(s); gc()

    s2 <- new_mmap_zip_store(path, mode = "r+")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    # Tick #5 fires after the data is fully written — both entries
    # survive recovery.
    expect_setequal(store_list(s2, ""), c("first", "second"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "first"))), "FIRST")
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "second"))), "SECOND")
})

test_that("rollback recovery + fresh append round-trips cross-process", {
    skip_if_not_full_run()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("FIRST"))
    with_crash_counter(s, 3L)  # rollback path
    expect_error(
        store_set_bytes(s, "second", charToRaw("SECOND")),
        class = "SimulatedCrash"
    )
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    rm(s); gc()

    s2 <- new_mmap_zip_store(path, mode = "r+")
    expect_setequal(store_list(s2, ""), "first")
    store_set_bytes(s2, "third", charToRaw("THIRD"))
    dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr"))

    s3 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s3, "xptr")), add = TRUE)
    expect_setequal(store_list(s3, ""), c("first", "third"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s3, "first"))), "FIRST")
    expect_identical(rawToChar(as.raw(store_get_bytes(s3, "third"))), "THIRD")
})
