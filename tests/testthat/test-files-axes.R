test_that("FilesDaf read axis entries", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("BRCA1", "TP53", "MYC"), file.path(dir, "axes", "gene.txt"))
    d <- files_daf(dir, mode = "r")
    expect_true(has_axis(d, "gene"))
    expect_equal(axis_length(d, "gene"), 3L)
    expect_equal(axis_vector(d, "gene"), c("BRCA1", "TP53", "MYC"))
    dict <- axis_dict(d, "gene")
    expect_equal(dict[["TP53"]], 2L)
    expect_equal(axes_set(d), "gene")
})

test_that("axis parsing rejects empty lines", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "", "B"), file.path(dir, "axes", "bad.txt"))
    d <- files_daf(dir, mode = "r")
    expect_error(axis_vector(d, "bad"), "empty")
})

test_that("add_axis writes a UTF-8 \\n-terminated file", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "gene", c("BRCA1", "TP53", "MYC"))
    p <- file.path(dir, "axes", "gene.txt")
    expect_true(file.exists(p))
    raw <- readBin(p, what = "raw", n = file.size(p))
    expect_equal(rawToChar(raw), "BRCA1\nTP53\nMYC\n")
})

test_that("add_axis rejects existing", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", "A")
    expect_error(add_axis(d, "cell", "B"), "existing axis:")
})

test_that("delete_axis without dependents removes the file", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    expect_true(has_axis(d, "cell"))
    delete_axis(d, "cell")
    expect_false(has_axis(d, "cell"))
    expect_false(file.exists(file.path(dir, "axes", "cell.txt")))
    # must_exist = FALSE on missing axis is a no-op:
    expect_silent(delete_axis(d, "cell", must_exist = FALSE))
    # must_exist = TRUE on missing axis errors:
    expect_error(delete_axis(d, "cell"), "missing axis:")
})

test_that("delete_axis cascades to dependent vectors + matrices on FilesDaf", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "donor", c(1, 2))
    set_matrix(d, "cell", "gene", "m", matrix(1:4, 2, 2))
    set_matrix(d, "gene", "cell", "m2", matrix(1:4, 2, 2))
    delete_axis(d, "cell")
    expect_false(has_axis(d, "cell"))
    expect_false(dir.exists(file.path(dir, "vectors", "cell")))
    expect_false(dir.exists(file.path(dir, "matrices", "cell")))
    expect_false(dir.exists(file.path(dir, "matrices", "gene", "cell")))
    # matrices/gene/ still exists (only gene/cell subtree gone):
    expect_true(dir.exists(file.path(dir, "matrices", "gene")))
})
