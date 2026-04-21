test_that("copy_tensor copies per-main-axis matrices", {
    src <- memory_daf(name = "src")
    add_axis(src, "batch", c("b1", "b2"))
    add_axis(src, "gene", c("g1", "g2"))
    add_axis(src, "cell", c("c1"))
    set_matrix(src, "gene", "cell", "b1_counts",
               matrix(1:2, 2, 1, dimnames = list(c("g1","g2"), "c1")))
    set_matrix(src, "gene", "cell", "b2_counts",
               matrix(3:4, 2, 1, dimnames = list(c("g1","g2"), "c1")))

    dest <- memory_daf(name = "dest")
    add_axis(dest, "batch", c("b1", "b2"))
    add_axis(dest, "gene", c("g1", "g2"))
    add_axis(dest, "cell", c("c1"))

    copy_tensor(dest, src,
                main_axis = "batch", rows_axis = "gene",
                columns_axis = "cell", name = "counts",
                relayout = FALSE)
    expect_true(has_matrix(dest, "gene", "cell", "b1_counts"))
    expect_true(has_matrix(dest, "gene", "cell", "b2_counts"))
    expect_equal(as.numeric(get_matrix(dest, "gene", "cell", "b1_counts")),
                 c(1, 2))
})

test_that("copy_tensor fills missing source matrices with empty", {
    src <- memory_daf(name = "src")
    add_axis(src, "batch", c("b1"))
    add_axis(src, "gene", c("g1"))
    add_axis(src, "cell", c("c1"))
    set_matrix(src, "gene", "cell", "b1_counts",
               matrix(5, 1, 1, dimnames = list("g1", "c1")))

    dest <- memory_daf(name = "dest")
    add_axis(dest, "batch", c("b1", "b2"))
    add_axis(dest, "gene", c("g1"))
    add_axis(dest, "cell", c("c1"))

    copy_tensor(dest, src,
                main_axis = "batch", rows_axis = "gene",
                columns_axis = "cell", name = "counts",
                empty = 0, relayout = FALSE)
    expect_equal(as.numeric(get_matrix(dest, "gene", "cell", "b1_counts")), 5)
    expect_equal(as.numeric(get_matrix(dest, "gene", "cell", "b2_counts")), 0)
})
