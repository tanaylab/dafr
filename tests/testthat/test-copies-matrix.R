test_that("copy_matrix: same-axes dense copy", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2", "g3"))
    m <- matrix(1:6, nrow = 2, ncol = 3, dimnames = list(c("c1","c2"), c("g1","g2","g3")))
    set_matrix(src, "cell", "gene", "UMIs", m, overwrite = TRUE)

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))
    add_axis(dest, "gene", c("g1", "g2", "g3"))

    copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE)
    expect_equal(as.matrix(get_matrix(dest, "cell", "gene", "UMIs")),
                 as.matrix(m))
})

test_that("copy_matrix: rename + reaxis", {
    src <- memory_daf(name = "src")
    add_axis(src, "obs", c("o1", "o2"))
    add_axis(src, "var", c("v1", "v2"))
    m <- matrix(1:4, nrow = 2, dimnames = list(c("o1","o2"), c("v1","v2")))
    set_matrix(src, "obs", "var", "X", m)

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("o1", "o2"))
    add_axis(dest, "gene", c("v1", "v2"))

    copy_matrix(dest, src, "obs", "var", "X",
                rows_reaxis = "cell", columns_reaxis = "gene",
                rename = "counts", relayout = FALSE)
    expect_true(has_matrix(dest, "cell", "gene", "counts"))
})

test_that("copy_matrix: insist=TRUE raises when destination has it", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1")); add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1, 1, 1,
               dimnames = list("c1", "g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1")); add_axis(dest, "gene", c("g1"))
    set_matrix(dest, "cell", "gene", "UMIs", matrix(9, 1, 1,
               dimnames = list("c1", "g1")))
    expect_error(copy_matrix(dest, src, "cell", "gene", "UMIs"),
                 "already exists")
})

test_that("copy_matrix: overwrite replaces destination matrix", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1")); add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1, 1, 1,
               dimnames = list("c1", "g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1")); add_axis(dest, "gene", c("g1"))
    set_matrix(dest, "cell", "gene", "UMIs", matrix(9, 1, 1,
               dimnames = list("c1", "g1")))
    copy_matrix(dest, src, "cell", "gene", "UMIs",
                overwrite = TRUE, relayout = FALSE)
    expect_equal(as.numeric(get_matrix(dest, "cell", "gene", "UMIs")), 1)
})

test_that("copy_matrix: destination rows subset — extracts rows", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    add_axis(src, "gene", c("g1", "g2"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1:6, 3, 2,
               dimnames = list(c("c1","c2","c3"), c("g1","g2"))))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c3"))
    add_axis(dest, "gene", c("g1", "g2"))

    copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE)
    expect_equal(as.matrix(get_matrix(dest, "cell", "gene", "UMIs")),
                 matrix(c(1,3,4,6), 2, 2,
                        dimnames = list(c("c1","c3"), c("g1","g2"))))
})

test_that("copy_matrix: source rows subset — requires empty; then fills", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1:4, 2, 2,
               dimnames = list(c("c1","c2"), c("g1","g2"))))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))
    add_axis(dest, "gene", c("g1", "g2"))

    expect_error(
        copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE),
        "missing entries"
    )
    copy_matrix(dest, src, "cell", "gene", "UMIs",
                empty = -1, relayout = FALSE)
    expect_equal(as.matrix(get_matrix(dest, "cell", "gene", "UMIs")),
                 matrix(c(1,2,-1, 3,4,-1), 3, 2,
                        dimnames = list(c("c1","c2","c3"), c("g1","g2"))))
})

test_that("copy_matrix: source cols subset — fills with empty", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1:2, 2, 1,
               dimnames = list(c("c1","c2"), "g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))
    add_axis(dest, "gene", c("g1", "g2"))

    copy_matrix(dest, src, "cell", "gene", "UMIs",
                empty = 0, relayout = FALSE)
    expect_equal(as.matrix(get_matrix(dest, "cell", "gene", "UMIs")),
                 matrix(c(1,2,0,0), 2, 2,
                        dimnames = list(c("c1","c2"), c("g1","g2"))))
})

test_that("copy_matrix: sparse source + source-is-subset + empty=0 stays sparse", {
    skip_if_not_installed("Matrix")
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2"))
    m_sp <- Matrix::sparseMatrix(
        i = c(1L, 2L), j = c(1L, 2L), x = c(10, 20),
        dims = c(2L, 2L),
        dimnames = list(c("c1","c2"), c("g1","g2"))
    )
    set_matrix(src, "cell", "gene", "UMIs", m_sp)

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))
    add_axis(dest, "gene", c("g1", "g2"))

    copy_matrix(dest, src, "cell", "gene", "UMIs",
                empty = 0, relayout = FALSE)
    result <- format_get_matrix(dest, "cell", "gene", "UMIs")
    expect_s4_class(result, "dgCMatrix")
    expect_equal(unname(as.matrix(result)),
                 matrix(c(10,0,0, 0,20,0), 3, 2))
})

test_that("copy_matrix: relayout=TRUE writes transposed layout", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2")); add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs",
               matrix(1:2, 2, 1, dimnames = list(c("c1","c2"), "g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2")); add_axis(dest, "gene", c("g1"))

    copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = TRUE)
    expect_true(has_matrix(dest, "cell", "gene", "UMIs"))
    expect_true(has_matrix(dest, "gene", "cell", "UMIs"))
})

test_that("copy_matrix validates rename / rows_reaxis / columns_reaxis / relayout / overwrite / insist", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1")); add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1, 1, 1, dimnames = list("c1","g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1")); add_axis(dest, "gene", c("g1"))
    expect_error(copy_matrix(dest, src, "cell", "gene", "UMIs", rename = ""), "rename")
    expect_error(copy_matrix(dest, src, "cell", "gene", "UMIs", rows_reaxis = ""), "rows_reaxis")
    expect_error(copy_matrix(dest, src, "cell", "gene", "UMIs", columns_reaxis = ""), "columns_reaxis")
    expect_error(copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = 1), "relayout")
    expect_error(copy_matrix(dest, src, "cell", "gene", "UMIs", overwrite = 1), "overwrite")
    expect_error(copy_matrix(dest, src, "cell", "gene", "UMIs", insist = NA), "insist")
})
