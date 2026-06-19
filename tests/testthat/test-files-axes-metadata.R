# Tests for axis index maintenance in the root metadata.json (DataAxesFormats
# FilesFormat v1.1). The old tests checked axes/metadata.json (a file written
# inside the axes/ directory by the now-removed files_metadata_zip.R). The new
# format inlines each axis as "axes/<name>":{"format":"axis","n_entries":N} in
# the root metadata.json. These tests preserve coverage of add_axis / delete_axis
# keeping the index up to date.

test_that("files_daf w+ creates root metadata.json (no axes/metadata.json)", {
    path <- withr::local_tempdir("daf-axes-meta-init-")
    files_daf(path, "w+")
    expect_true(file.exists(file.path(path, "metadata.json")))
    expect_false(file.exists(file.path(path, "axes", "metadata.json")))
    m <- jsonlite::fromJSON(file.path(path, "metadata.json"), simplifyVector = FALSE)
    # No axes yet - the root object is empty (or has only non-axis keys).
    axis_keys <- grep("^axes/", names(m), value = TRUE)
    expect_length(axis_keys, 0L)
})

test_that("add_axis inserts axes/<name> entry in root metadata.json", {
    path <- withr::local_tempdir("daf-axes-meta-add-")
    d <- files_daf(path, "w+")
    add_axis(d, "gene", c("g1", "g2"))
    add_axis(d, "cell", c("c1", "c2", "c3"))
    m <- jsonlite::fromJSON(file.path(path, "metadata.json"), simplifyVector = FALSE)
    expect_equal(m[["axes/gene"]], list(format = "axis", n_entries = 2L))
    expect_equal(m[["axes/cell"]], list(format = "axis", n_entries = 3L))
})

test_that("delete_axis removes axes/<name> entry from root metadata.json", {
    path <- withr::local_tempdir("daf-axes-meta-del-")
    d <- files_daf(path, "w+")
    add_axis(d, "gene", c("g1", "g2"))
    add_axis(d, "cell", c("c1", "c2"))
    delete_axis(d, "gene")
    m <- jsonlite::fromJSON(file.path(path, "metadata.json"), simplifyVector = FALSE)
    expect_null(m[["axes/gene"]])
    expect_equal(m[["axes/cell"]], list(format = "axis", n_entries = 2L))
})
