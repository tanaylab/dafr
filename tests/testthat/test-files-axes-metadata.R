test_that("axes/metadata.json is created on init with empty array", {
    path <- withr::local_tempdir("daf-axes-meta-init-")
    files_daf(path, "w+")
    expect_true(file.exists(file.path(path, "axes", "metadata.json")))
    # jsonlite::fromJSON("[]") returns list() regardless of simplifyVector;
    # check raw file is exactly "[]" plus trailing newline.
    raw <- readLines(file.path(path, "axes", "metadata.json"), warn = FALSE)
    expect_identical(raw, "[]")
    j <- jsonlite::fromJSON(file.path(path, "axes", "metadata.json"),
                            simplifyVector = TRUE)
    expect_identical(as.character(j), character(0))
})

test_that("add_axis updates axes/metadata.json with sorted list", {
    path <- withr::local_tempdir("daf-axes-meta-add-")
    d <- files_daf(path, "w+")
    add_axis(d, "gene", c("g1", "g2"))
    add_axis(d, "cell", c("c1", "c2", "c3"))
    j <- jsonlite::fromJSON(file.path(path, "axes", "metadata.json"),
                            simplifyVector = TRUE)
    expect_identical(j, c("cell", "gene"))
})

test_that("delete_axis updates axes/metadata.json", {
    path <- withr::local_tempdir("daf-axes-meta-del-")
    d <- files_daf(path, "w+")
    add_axis(d, "gene", c("g1", "g2"))
    add_axis(d, "cell", c("c1", "c2"))
    delete_axis(d, "gene")
    j <- jsonlite::fromJSON(file.path(path, "axes", "metadata.json"),
                            simplifyVector = TRUE)
    expect_identical(j, "cell")
})
