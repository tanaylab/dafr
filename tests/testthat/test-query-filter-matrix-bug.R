test_that("filter + matrix: mask is applied to the matrix row axis", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE, FALSE))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(1:8, nrow = 4, ncol = 2))

    # Without filter: full matrix.
    full <- get_query(d, "@ cell @ gene :: UMIs")
    expect_identical(dim(full), c(4L, 2L))

    # With filter on first axis: rows subset.
    filtered <- get_query(d, "@ cell [ keep ] @ gene :: UMIs")
    expect_identical(dim(filtered), c(2L, 2L))
    expect_identical(as.vector(filtered[, 1]), c(1L, 3L))
    expect_identical(as.vector(filtered[, 2]), c(5L, 7L))
})

test_that("unfiltered two-axis matrix is unchanged", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(1:6, nrow = 2, ncol = 3))
    m <- get_query(d, "@ cell @ gene :: UMIs")
    expect_identical(dim(m), c(2L, 3L))
})

test_that("filter + matrix with all-pass mask returns full matrix", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "keep", c(TRUE, TRUE))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(1:4, 2, 2))
    m <- get_query(d, "@ cell [ keep ] @ gene :: UMIs")
    expect_identical(dim(m), c(2L, 2L))
})

test_that("filter + missing matrix with if_missing respects filter length", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE))
    m <- get_query(d, "@ cell [ keep ] @ gene :: missing_matrix || 0")
    expect_identical(dim(m), c(2L, 2L))
    expect_true(all(m == 0))
})
