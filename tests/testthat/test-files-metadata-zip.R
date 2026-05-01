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
