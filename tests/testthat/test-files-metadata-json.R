test_that(".metadata_json_rebuild matches Julia's metadata.json structure", {
    root <- withr::local_tempdir()
    d <- files_daf(root, mode = "w+", name = "m")
    add_axis(d, "cell", paste0("c", 1:5)); add_axis(d, "gene", paste0("g", 1:3))
    set_scalar(d, "title", "hello"); set_scalar(d, "k", 7L)
    set_vector(d, "cell", "score", as.numeric(1:5))
    set_matrix(d, "cell", "gene", "dense", matrix(as.numeric(1:15), 5, 3))
    dafr:::.metadata_json_rebuild(root)
    m <- jsonlite::fromJSON(file.path(root, "metadata.json"), simplifyVector = FALSE)
    expect_equal(m[["axes/cell"]], list(format = "axis", n_entries = 5L))
    expect_equal(m[["scalars/title"]], list(type = "String", value = "hello"))
    expect_equal(m[["scalars/k"]], list(type = "Int32", value = 7L))
    expect_equal(m[["vectors/cell/score"]], list(format = "dense", eltype = "Float64"))
    expect_equal(m[["matrices/cell/gene/dense"]], list(format = "dense", eltype = "Float64"))
})

test_that(".metadata_json_rebuild covers sparse + both relayout orientations", {
    root <- withr::local_tempdir()
    d <- files_daf(root, mode = "w+", name = "m")
    add_axis(d, "cell", paste0("c", 1:5)); add_axis(d, "gene", paste0("g", 1:3))
    sm <- Matrix::sparseMatrix(i = c(1L, 3L), j = c(1L, 2L),
                               x = c(10, 20), dims = c(5, 3))
    set_matrix(d, "cell", "gene", "sp", sm)            # relayout writes both dirs
    dafr:::.metadata_json_rebuild(root)
    m <- jsonlite::fromJSON(file.path(root, "metadata.json"), simplifyVector = FALSE)
    # both orientations indexed (set_matrix relayout = TRUE by default)
    expect_false(is.null(m[["matrices/cell/gene/sp"]]))
    expect_false(is.null(m[["matrices/gene/cell/sp"]]))
    # sparse descriptor inlined with per-component n_elements
    sp <- m[["matrices/cell/gene/sp"]]
    expect_equal(sp$format, "sparse")
    expect_equal(sp$nzval$format, "dense")
    expect_equal(sp$nzval$n_elements, 2L)
    expect_equal(sp$colptr$n_elements, 4L)   # ncol+1 = 3+1
})
