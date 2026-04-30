# Slice 17 phase 2: read-only MmapZipStore over Python-built ZIP fixtures.
# Writes happen in phase 4; this file only tests open/list/get/exists.

test_that("opens an existing stored-mode zip and lists its entries", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list(
        "hello/foo" = "alpha-payload",
        "hello/bar" = "bravo-payload",
        "world"     = "charlie-payload"
    ), compression = "stored")

    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)

    keys <- store_list(s, "")
    expect_setequal(keys, c("hello/foo", "hello/bar", "world"))
})

test_that("returns the right bytes for each entry (stored)", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list(
        "a/b" = "first content",
        "c"   = "second content"
    ), compression = "stored")

    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)

    got_ab <- store_get_bytes(s, "a/b")
    expect_true(is.raw(got_ab))
    expect_identical(rawToChar(got_ab), "first content")

    got_c <- store_get_bytes(s, "c")
    expect_identical(rawToChar(got_c), "second content")
})

test_that("returns NULL for missing keys", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("only" = "x"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    expect_null(store_get_bytes(s, "missing"))
})

test_that("store_exists reports presence", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("present" = "yes"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    expect_true(store_exists(s, "present"))
    expect_false(store_exists(s, "absent"))
})

test_that("rejects non-existent zip files with a clear error", {
    path <- file.path(tempdir(), "definitely-does-not-exist.zip")
    if (file.exists(path)) unlink(path)
    expect_error(new_mmap_zip_store(path, mode = "r"), "open|exist|stat")
})

test_that("rejects empty / corrupt files with a clear error", {
    skip_if_no_python_zipfile()
    # Empty file: smaller than EOCD.
    empty_path <- new_tempfile("zip")
    file.create(empty_path)
    expect_error(new_mmap_zip_store(empty_path, mode = "r"), "zip|too small|corrupt")

    # Random bytes: large enough to mmap, but no EOCD.
    junk_path <- new_tempfile("zip")
    writeBin(as.raw(rep(0x55, 200L)), junk_path)
    expect_error(new_mmap_zip_store(junk_path, mode = "r"), "zip|EOCD|corrupt")
})

test_that("decompresses method-8 (deflate) entries", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    payload <- paste(rep("the quick brown fox jumps over the lazy dog ", 50L),
                     collapse = "")
    build_zip(path, list("doc.txt" = payload), compression = "deflate")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    got <- store_get_bytes(s, "doc.txt")
    expect_identical(rawToChar(got), payload)
})

test_that("rejects unsupported compression methods", {
    skip_if_no_python_zipfile()
    # bzip2 is method 12 — neither stored (0) nor deflate (8).
    rc <- system2("python3",
                  c("-c", shQuote("import zipfile; zipfile.ZIP_BZIP2")),
                  stdout = FALSE, stderr = FALSE)
    if (rc != 0L) testthat::skip("python3 bzip2 unavailable")

    path <- new_tempfile("zip")
    build_zip(path, list("z" = paste(rep("payload ", 100L), collapse = "")),
              compression = "bzip2")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    expect_error(store_get_bytes(s, "z"), "unsupported compression")
})

test_that("store_list with a directory prefix returns matching keys", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list(
        "dir/a"   = "1",
        "dir/b"   = "2",
        "other/c" = "3"
    ), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    expect_setequal(store_list(s, "dir"), c("dir/a", "dir/b"))
})

test_that("set_bytes on a read-only store errors", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "v"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    expect_error(store_set_bytes(s, "x", as.raw(0L)), "read-only|cannot set")
})

test_that("delete on a MmapZipStore errors with append-only message", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "v"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    expect_error(store_delete(s, "k"), "append-only|deletion")
})

# ---- Phase 4 write-side tests --------------------------------------------

test_that("creates an empty archive in mode='w'", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))
    # Reopen read-only — empty archive must list as character(0).
    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_identical(store_list(s2, ""), character(0))
})

test_that("appends a single entry and reopens to read", {
    path <- new_tempfile("zip")
    payload <- charToRaw("phase-4 write payload")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "k", payload)
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), "k")
    got <- store_get_bytes(s2, "k")
    expect_identical(as.raw(got), payload)
})

test_that("appends multiple entries and reads each back", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "a", charToRaw("alpha"))
    store_set_bytes(s, "b/c", charToRaw("beta-gamma"))
    store_set_bytes(s, "really/deep/nested/key", charToRaw("zzz"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    expect_setequal(store_list(s2, ""), c("a", "b/c", "really/deep/nested/key"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "a"))), "alpha")
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "b/c"))), "beta-gamma")
    expect_identical(rawToChar(as.raw(store_get_bytes(s2, "really/deep/nested/key"))), "zzz")
})

test_that("rejects overwriting an existing entry with append-only error", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    store_set_bytes(s, "k", charToRaw("first"))
    expect_error(store_set_bytes(s, "k", charToRaw("second")),
                 "append-only|cannot overwrite")
})

test_that("rejects entries with names longer than 64 KiB", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    long_key <- paste(rep("a", 70000L), collapse = "")
    expect_error(store_set_bytes(s, long_key, as.raw(0L)),
                 "name too long|too long")
})

test_that("data offsets for written entries are 8-byte aligned", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    # Names of varying lengths so unaligned LFHs would be exposed.
    store_set_bytes(s, "a", charToRaw("X"))
    store_set_bytes(s, "ab", charToRaw("YY"))
    store_set_bytes(s, "abc", charToRaw("ZZZ"))
    store_set_bytes(s, "abcd", charToRaw("WWWW"))
    store_set_bytes(s, "abcde", charToRaw("VVVVV"))
    store_set_bytes(s, "abcdef", charToRaw("UUUUUU"))
    store_set_bytes(s, "abcdefg", charToRaw("TTTTTTT"))
    store_set_bytes(s, "abcdefgh", charToRaw("SSSSSSSS"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    offsets <- dafr:::dafr_mmap_zip_data_offsets(S7::prop(s2, "xptr"))
    expect_length(offsets, 8L)
    for (off in offsets) {
        expect_equal(off %% 8, 0)
    }
})

test_that("Python's zipfile reads entries written by MmapZipStore", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "k", charToRaw("hello-from-dafr"))
    store_set_bytes(s, "n/sub", charToRaw("nested-payload"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    out <- read_zip_via_python(path)
    expect_setequal(names(out), c("k", "n/sub"))
    expect_identical(rawToChar(out[["k"]]), "hello-from-dafr")
    expect_identical(rawToChar(out[["n/sub"]]), "nested-payload")
})

test_that("write empty bytes round-trips correctly", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "empty", raw(0))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    s2 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr")), add = TRUE)
    got <- store_get_bytes(s2, "empty")
    expect_equal(length(got), 0L)
})

test_that("w+ mode preserves existing entries and allows appends", {
    path <- new_tempfile("zip")
    s <- new_mmap_zip_store(path, mode = "w")
    store_set_bytes(s, "first", charToRaw("AA"))
    dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr"))

    s2 <- new_mmap_zip_store(path, mode = "w+")
    store_set_bytes(s2, "second", charToRaw("BBB"))
    dafr:::dafr_mmap_zip_close(S7::prop(s2, "xptr"))

    s3 <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s3, "xptr")), add = TRUE)
    expect_setequal(store_list(s3, ""), c("first", "second"))
    expect_identical(rawToChar(as.raw(store_get_bytes(s3, "first"))), "AA")
    expect_identical(rawToChar(as.raw(store_get_bytes(s3, "second"))), "BBB")
})
