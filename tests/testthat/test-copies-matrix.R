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
