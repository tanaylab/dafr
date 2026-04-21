test_that("viewer rename: get_vector(view, renamed_axis, name) resolves", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
    v <- viewer(d, name = "view",
        axes = list(list("obs", "@ cell")),
        data = list(list(c("obs", "donor"), "="))
    )
    out <- get_vector(v, "obs", "donor")
    expect_identical(as.character(out), c("d1", "d2", "d1"))
    expect_identical(names(out), c("A", "B", "C"))
})

test_that("viewer rename: get_matrix(view, renamed_rows, cols, name) resolves", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    v <- viewer(d, name = "view",
        axes = list(
            list("obs",  "@ cell"),
            list("feat", "@ gene")
        ),
        data = list(list(c("obs", "feat", "UMIs"), "="))
    )
    out <- get_matrix(v, "obs", "feat", "UMIs")
    expect_equal(unname(as.matrix(out)), m)
    expect_identical(rownames(out), c("A", "B"))
    expect_identical(colnames(out), c("X", "Y"))
})
