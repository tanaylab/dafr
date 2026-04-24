test_that(">| Sum reduces across columns to per-row vector", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2"))
    set_matrix(
        d, "cell", "gene", "UMIs",
        matrix(c(1, 2, 3, 4), 2, 2,
            dimnames = list(c("c1", "c2"), c("g1", "g2"))
        )
    )
    v <- get_query(d, "@ cell @ gene :: UMIs >| Sum")
    # ReduceToColumn: one value per row (per cell), summing across genes
    expect_equal(v, c(c1 = 1 + 3, c2 = 2 + 4))
})

test_that(">- Mean reduces across rows to per-column vector", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2"))
    set_matrix(
        d, "cell", "gene", "UMIs",
        matrix(c(1, 2, 3, 4), 2, 2,
            dimnames = list(c("c1", "c2"), c("g1", "g2"))
        )
    )
    v <- get_query(d, "@ cell @ gene :: UMIs >- Mean")
    # ReduceToRow: one value per column (per gene), averaging across cells
    expect_equal(v, c(g1 = mean(c(1, 2)), g2 = mean(c(3, 4))))
})
