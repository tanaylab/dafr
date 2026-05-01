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
