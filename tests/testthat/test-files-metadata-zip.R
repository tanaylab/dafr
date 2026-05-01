test_that("pack_files_daf_metadata bundles every JSON in tree", {
    path <- withr::local_tempdir("daf-pack-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    set_scalar(d, "k", "v")
    set_vector(d, "cell", "x", c(1, 2))
    rm(d); gc()  # release file handles before inspecting from outside

    out <- pack_files_daf_metadata(path)
    expect_identical(out, file.path(path, "metadata.zip"))
    expect_true(file.exists(out))

    names <- unzip(out, list = TRUE)$Name
    expected <- c("daf.json", "axes/metadata.json", "scalars/k.json",
                  "vectors/cell/x.json")
    expect_setequal(names, expected)

    # Verify the contents match the tree byte-for-byte.
    for (n in expected) {
        con <- unz(out, n, "rb")
        in_zip <- readBin(con, what = "raw",
                          n = file.size(file.path(path, n)))
        close(con)
        on_disk <- readBin(file.path(path, n), what = "raw",
                           n = file.size(file.path(path, n)))
        expect_identical(in_zip, on_disk, info = n)
    }
})

test_that("pack_files_daf_metadata is atomic via .new + rename", {
    path <- withr::local_tempdir("daf-pack-atomic-")
    files_daf(path, "w+")
    pack_files_daf_metadata(path)
    expect_false(file.exists(file.path(path, "metadata.zip.new")))
})

# Determinism relies on MmapZipStore writing fixed 1980-01-01 timestamps
# (src/mmap_zip_store.cpp). If that ever changes, this test will fail.
test_that("pack_files_daf_metadata is deterministic on a stable tree", {
    path <- withr::local_tempdir("daf-pack-determ-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    set_scalar(d, "k", "v")
    rm(d); gc()

    pack_files_daf_metadata(path)
    bytes1 <- readBin(file.path(path, "metadata.zip"), what = "raw",
                      n = file.size(file.path(path, "metadata.zip")))
    pack_files_daf_metadata(path)
    bytes2 <- readBin(file.path(path, "metadata.zip"), what = "raw",
                      n = file.size(file.path(path, "metadata.zip")))
    expect_identical(bytes1, bytes2)
})

test_that("pack_files_daf_metadata writes entries in deterministic sorted order", {
    path <- withr::local_tempdir("daf-pack-order-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    set_scalar(d, "z", "v")
    set_scalar(d, "a", "v")
    set_vector(d, "cell", "y", c(1, 2))
    set_vector(d, "cell", "x", c(1, 2))
    rm(d); gc()

    pack_files_daf_metadata(path)
    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_identical(names, c(
        "daf.json", "axes/metadata.json",
        "scalars/a.json", "scalars/z.json",
        "vectors/cell/x.json", "vectors/cell/y.json"
    ))
})

test_that("pack_files_daf_metadata errors on non-FilesDaf directory", {
    path <- withr::local_tempdir("daf-pack-bad-")
    expect_error(pack_files_daf_metadata(path),
                 "not a FilesDaf directory")
})

test_that("pack_files_daf_metadata bundles matrix descriptor JSON", {
    path <- withr::local_tempdir("daf-pack-mat-")
    d <- files_daf(path, "w+")
    add_axis(d, "row", c("r1", "r2"))
    add_axis(d, "col", c("c1", "c2"))
    set_matrix(d, "row", "col", "M", matrix(1:4, 2, 2))
    rm(d); gc()

    pack_files_daf_metadata(path)
    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_true("matrices/row/col/M.json" %in% names)
})

test_that("set_scalar appends to metadata.zip", {
    path <- withr::local_tempdir("daf-append-scalar-")
    d <- files_daf(path, "w+")
    set_scalar(d, "k", "v")
    rm(d); gc()

    expect_true(file.exists(file.path(path, "metadata.zip")))
    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_true("scalars/k.json" %in% names)
})

test_that("set_vector appends to metadata.zip; entry set matches rebuild", {
    path <- withr::local_tempdir("daf-append-vec-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "x", c(1, 2))
    rm(d); gc()

    after_append <- sort(unzip(file.path(path, "metadata.zip"), list = TRUE)$Name)
    pack_files_daf_metadata(path)
    after_rebuild <- sort(unzip(file.path(path, "metadata.zip"), list = TRUE)$Name)
    expect_identical(after_append, after_rebuild)
})

test_that("set_matrix (dense) appends to metadata.zip", {
    path <- withr::local_tempdir("daf-append-mat-dense-")
    d <- files_daf(path, "w+")
    add_axis(d, "row", c("r1", "r2"))
    add_axis(d, "col", c("c1", "c2"))
    set_matrix(d, "row", "col", "M", matrix(1:4, 2, 2))
    rm(d); gc()

    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_true("matrices/row/col/M.json" %in% names)
})

test_that("set_matrix (sparse) appends to metadata.zip", {
    path <- withr::local_tempdir("daf-append-mat-sparse-")
    d <- files_daf(path, "w+")
    add_axis(d, "row", c("r1", "r2", "r3"))
    add_axis(d, "col", c("c1", "c2"))
    sm <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = c(1, 2),
                               dims = c(3, 2))
    set_matrix(d, "row", "col", "S", sm)
    rm(d); gc()

    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_true("matrices/row/col/S.json" %in% names)
})

test_that("set_vector with sparseVector input appends to metadata.zip", {
    path <- withr::local_tempdir("daf-append-svec-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    sv <- Matrix::sparseVector(c(1, 2), i = c(1, 3), length = 3L)
    set_vector(d, "cell", "x", sv)
    rm(d); gc()

    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_true("vectors/cell/x.json" %in% names)
})

test_that("metadata_zip_append exercises append path on second set_*", {
    # First set creates metadata.zip via missing-zip fallback. Second set
    # exercises the actual append branch (line 115+ of files_metadata_zip.R).
    path <- withr::local_tempdir("daf-append-second-")
    d <- files_daf(path, "w+")
    set_scalar(d, "k1", "v1")
    expect_true(file.exists(file.path(path, "metadata.zip")))
    set_scalar(d, "k2", "v2")  # this hits the append branch
    rm(d); gc()

    names <- sort(unzip(file.path(path, "metadata.zip"), list = TRUE)$Name)
    expect_true("scalars/k1.json" %in% names)
    expect_true("scalars/k2.json" %in% names)

    # Byte-equal content check: append path must serialize the JSON correctly.
    con <- unz(file.path(path, "metadata.zip"), "scalars/k2.json", "rb")
    on.exit(close(con))
    in_zip <- readBin(con, what = "raw", n = 1024L)
    on_disk <- readBin(file.path(path, "scalars", "k2.json"),
                       what = "raw",
                       n = file.size(file.path(path, "scalars", "k2.json")))
    expect_identical(in_zip[seq_along(on_disk)], on_disk)
})

test_that("set_scalar with overwrite=TRUE rewrites metadata.zip entry", {
    # Pins the dafr-only collision-fallback-to-rebuild divergence: dafr's
    # MmapZipStore is append-only, so an overwriting set_* triggers a full
    # rebuild. Verify exactly one entry remains and content is the new value.
    path <- withr::local_tempdir("daf-overwrite-")
    d <- files_daf(path, "w+")
    set_scalar(d, "k", "v1")
    set_scalar(d, "k", "v2", overwrite = TRUE)
    rm(d); gc()

    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    # Exactly one entry for scalars/k.json (no duplicate from append-overwrite).
    expect_equal(sum(names == "scalars/k.json"), 1L)

    con <- unz(file.path(path, "metadata.zip"), "scalars/k.json", "rb")
    on.exit(close(con))
    bytes <- readBin(con, "raw", n = 1024L)
    text <- rawToChar(bytes)
    expect_match(text, "v2", fixed = TRUE)
    expect_false(grepl("v1", text, fixed = TRUE))
})

test_that("delete_scalar rebuilds metadata.zip", {
    path <- withr::local_tempdir("daf-del-scalar-")
    d <- files_daf(path, "w+")
    set_scalar(d, "k1", "v1")
    set_scalar(d, "k2", "v2")
    delete_scalar(d, "k1")
    rm(d); gc()

    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_false("scalars/k1.json" %in% names)
    expect_true("scalars/k2.json" %in% names)

    con <- unz(file.path(path, "metadata.zip"), "scalars/k2.json", "rb")
    on.exit(close(con))
    in_zip <- readBin(con, "raw", n = 1024L)
    on_disk <- readBin(file.path(path, "scalars/k2.json"), "raw",
                       n = file.size(file.path(path, "scalars/k2.json")))
    expect_identical(in_zip[seq_along(on_disk)], on_disk)
})

test_that("delete_vector rebuilds metadata.zip", {
    path <- withr::local_tempdir("daf-del-vec-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "x", c(1, 2))
    set_vector(d, "cell", "y", c(3, 4))
    delete_vector(d, "cell", "x")
    rm(d); gc()

    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_false("vectors/cell/x.json" %in% names)
    expect_true("vectors/cell/y.json" %in% names)

    con <- unz(file.path(path, "metadata.zip"), "vectors/cell/y.json", "rb")
    on.exit(close(con))
    in_zip <- readBin(con, "raw", n = 1024L)
    on_disk <- readBin(file.path(path, "vectors/cell/y.json"), "raw",
                       n = file.size(file.path(path, "vectors/cell/y.json")))
    expect_identical(in_zip[seq_along(on_disk)], on_disk)
})

test_that("delete_matrix rebuilds metadata.zip", {
    path <- withr::local_tempdir("daf-del-mat-")
    d <- files_daf(path, "w+")
    add_axis(d, "row", c("r1", "r2"))
    add_axis(d, "col", c("c1", "c2"))
    set_matrix(d, "row", "col", "M1", matrix(1:4, 2, 2))
    set_matrix(d, "row", "col", "M2", matrix(5:8, 2, 2))
    delete_matrix(d, "row", "col", "M1")
    rm(d); gc()

    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_false("matrices/row/col/M1.json" %in% names)
    expect_true("matrices/row/col/M2.json" %in% names)

    con <- unz(file.path(path, "metadata.zip"), "matrices/row/col/M2.json", "rb")
    on.exit(close(con))
    in_zip <- readBin(con, "raw", n = 1024L)
    on_disk <- readBin(file.path(path, "matrices/row/col/M2.json"), "raw",
                       n = file.size(file.path(path, "matrices/row/col/M2.json")))
    expect_identical(in_zip[seq_along(on_disk)], on_disk)
})

test_that("delete_axis rebuilds metadata.zip and updates axes/metadata.json", {
    path <- withr::local_tempdir("daf-del-axis-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "x", c(1, 2))
    delete_axis(d, "cell")
    rm(d); gc()

    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    # All cell-related entries gone
    expect_false(any(startsWith(names, "vectors/cell/")))
    # axes/metadata.json reflects the post-delete state
    expect_true("axes/metadata.json" %in% names)

    # Verify axes/metadata.json content via on-disk + zip-entry parity
    j_disk <- jsonlite::fromJSON(file.path(path, "axes", "metadata.json"),
                                 simplifyVector = TRUE)
    expect_identical(j_disk, "gene")

    con <- unz(file.path(path, "metadata.zip"), "axes/metadata.json", "rb")
    on.exit(close(con))
    in_zip_bytes <- readBin(con, what = "raw", n = 1024L)
    on_disk_bytes <- readBin(file.path(path, "axes", "metadata.json"),
                             what = "raw",
                             n = file.size(file.path(path, "axes", "metadata.json")))
    expect_identical(in_zip_bytes[seq_along(on_disk_bytes)], on_disk_bytes)
})

test_that("reorder_axes rebuilds metadata.zip", {
    # After reorder, metadata.zip must match what a fresh rebuild would
    # produce on the post-reorder tree.
    path <- withr::local_tempdir("daf-reorder-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(10, 20, 30))

    reorder_axes(d, cell = c(3L, 1L, 2L))
    rm(d); gc()

    bytes_after_reorder <- readBin(file.path(path, "metadata.zip"),
                                   what = "raw",
                                   n = file.size(file.path(path, "metadata.zip")))
    pack_files_daf_metadata(path)
    bytes_after_rebuild <- readBin(file.path(path, "metadata.zip"),
                                   what = "raw",
                                   n = file.size(file.path(path, "metadata.zip")))
    expect_identical(bytes_after_reorder, bytes_after_rebuild)
})

test_that("reset_reorder_axes is idempotent (no-op) when no backup exists", {
    # After a successful reorder there is no .reorder.backup/, so calling
    # reset_reorder_axes is a no-op and metadata.zip must remain a
    # consistent rebuild of the current (post-reorder) tree. The real
    # rollback rebuild path is exercised in test-reorder-crash.R.
    path <- withr::local_tempdir("daf-reset-reorder-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "x", c(10, 20, 30))

    # Snapshot metadata.zip as-built (post-set, pre-reorder).
    bytes_before <- readBin(file.path(path, "metadata.zip"),
                            what = "raw",
                            n = file.size(file.path(path, "metadata.zip")))

    reorder_axes(d, cell = c(3L, 1L, 2L))
    reset_reorder_axes(d)  # no-op: no backup exists
    rm(d); gc()

    # metadata.zip should still be a consistent rebuild of the current tree.
    bytes_after <- readBin(file.path(path, "metadata.zip"),
                           what = "raw",
                           n = file.size(file.path(path, "metadata.zip")))
    pack_files_daf_metadata(path)
    bytes_rebuild <- readBin(file.path(path, "metadata.zip"),
                             what = "raw",
                             n = file.size(file.path(path, "metadata.zip")))
    expect_identical(bytes_after, bytes_rebuild)
})

test_that("reorder_axes does not produce stale metadata.zip entries", {
    # Verify the entry SET is what a rebuild would produce (no orphaned
    # entries from pre-reorder state).
    path <- withr::local_tempdir("daf-reorder-set-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "x", c(10, 20))
    set_vector(d, "cell", "y", c(30, 40))

    reorder_axes(d, cell = c(2L, 1L))
    rm(d); gc()

    after_reorder <- sort(unzip(file.path(path, "metadata.zip"), list = TRUE)$Name)
    pack_files_daf_metadata(path)
    after_rebuild <- sort(unzip(file.path(path, "metadata.zip"), list = TRUE)$Name)
    expect_identical(after_reorder, after_rebuild)
})

test_that("ensure_metadata_zip rebuilds when missing", {
    path <- withr::local_tempdir("daf-ensure-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    rm(d); gc()
    unlink(file.path(path, "metadata.zip"))
    expect_false(file.exists(file.path(path, "metadata.zip")))

    files_daf(path, mode = "r+")
    expect_true(file.exists(file.path(path, "metadata.zip")))
    # Verify content matches a fresh rebuild
    bytes_after_open <- readBin(file.path(path, "metadata.zip"), what = "raw",
                                n = file.size(file.path(path, "metadata.zip")))
    pack_files_daf_metadata(path)
    bytes_after_pack <- readBin(file.path(path, "metadata.zip"), what = "raw",
                                n = file.size(file.path(path, "metadata.zip")))
    expect_identical(bytes_after_open, bytes_after_pack)
})

test_that("ensure_metadata_zip is a no-op when metadata.zip is fresh", {
    path <- withr::local_tempdir("daf-ensure-fresh-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    rm(d); gc()

    bytes_before <- readBin(file.path(path, "metadata.zip"), what = "raw",
                            n = file.size(file.path(path, "metadata.zip")))
    files_daf(path, mode = "r+")
    bytes_after <- readBin(file.path(path, "metadata.zip"), what = "raw",
                           n = file.size(file.path(path, "metadata.zip")))
    # Bytes should be identical — ensure is a no-op when zip exists
    expect_identical(bytes_before, bytes_after)
})

test_that("read-only open does not write metadata.zip", {
    path <- withr::local_tempdir("daf-ensure-ro-")
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    rm(d); gc()
    unlink(file.path(path, "metadata.zip"))

    # Read-only: no rebuild attempt; the existing FilesDaf opens because
    # we don't require metadata.zip on r-mode opens (only writable opens
    # use it).
    expect_silent(files_daf(path, mode = "r"))
    expect_false(file.exists(file.path(path, "metadata.zip")))
})

test_that("fresh files_daf(w+) creates metadata.zip via init", {
    # Phase 6 Change 2: .files_daf_init now produces metadata.zip on
    # fresh init.
    path <- withr::local_tempdir("daf-init-")
    files_daf(path, "w+")
    expect_true(file.exists(file.path(path, "metadata.zip")))
    names <- unzip(file.path(path, "metadata.zip"), list = TRUE)$Name
    expect_setequal(names, c("daf.json", "axes/metadata.json"))
})
