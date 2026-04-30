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

test_that("set_bytes / delete error informatively (write API in phase 4)", {
    skip_if_no_python_zipfile()
    path <- new_tempfile("zip")
    build_zip(path, list("k" = "v"), compression = "stored")
    s <- new_mmap_zip_store(path, mode = "r")
    on.exit(dafr:::dafr_mmap_zip_close(S7::prop(s, "xptr")), add = TRUE)
    expect_error(store_set_bytes(s, "x", as.raw(0L)), "phase 4")
    expect_error(store_delete(s, "k"), "phase 4")
})
