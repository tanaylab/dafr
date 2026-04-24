test_that("viewer filter: get_vector(view, axis, name) returns filtered entries", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE, FALSE))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1", "d3"))
    v <- viewer(d, name = "view",
        axes = list(list("cell", "@ cell [ keep ]")),
        data = list(list(c("cell", "donor"), "="))
    )
    out <- get_vector(v, "cell", "donor")
    expect_identical(as.character(out), c("d1", "d1"))
    expect_identical(names(out), c("A", "C"))
})

test_that("viewer filter: get_matrix rows/cols filtered", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE))
    m <- matrix(1:6, nrow = 3, ncol = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    v <- viewer(d, name = "view",
        axes = list(
            list("cell", "@ cell [ keep ]"),
            list("gene", "@ gene")
        ),
        data = list(list(c("cell", "gene", "UMIs"), "="))
    )
    out <- get_matrix(v, "cell", "gene", "UMIs")
    expect_equal(unname(as.matrix(out)), m[c(1, 3), , drop = FALSE])
    expect_identical(rownames(out), c("A", "C"))
    expect_identical(colnames(out), c("X", "Y"))
})
