test_that(".reorder_planned_axis validates permutation length", {
    expect_error(dafr:::.reorder_planned_axis(c(1L, 2L), c("a", "b", "c")),
                 "does not match")
})

test_that(".reorder_planned_axis rejects duplicates and out-of-range", {
    expect_error(dafr:::.reorder_planned_axis(c(1L, 1L, 2L), c("a", "b", "c")),
                 "must be a permutation")
    expect_error(dafr:::.reorder_planned_axis(c(0L, 1L, 2L), c("a", "b", "c")),
                 "must be a permutation")
    expect_error(dafr:::.reorder_planned_axis(c(1L, 2L, 4L), c("a", "b", "c")),
                 "must be a permutation")
})

test_that(".reorder_planned_axis computes inverse permutation correctly", {
    pa <- dafr:::.reorder_planned_axis(c(3L, 1L, 2L), c("a", "b", "c"))
    expect_identical(pa$permutation, c(3L, 1L, 2L))
    # Verify the inverse: perm[inverse[i]] should == i, and inverse[perm[i]] should == i
    for (i in seq_along(pa$permutation)) {
        expect_identical(pa$permutation[pa$inverse[i]], i)
    }
    expect_identical(pa$new_entries, c("c", "a", "b"))
})

test_that(".build_reorder_plan walks vectors on permuted axes", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "x", c(10, 20, 30))
    set_vector(d, "gene", "y", c(1, 2))
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(2L, 3L, 1L)))
    expect_named(plan$planned_axes, "cell")
    expect_length(plan$planned_vectors, 1L)
    expect_identical(plan$planned_vectors[[1L]]$axis, "cell")
    expect_identical(plan$planned_vectors[[1L]]$name, "x")
    expect_identical(plan$planned_vectors[[1L]]$n_replacement_elements, 3L)
})

test_that(".build_reorder_plan walks matrices on permuted-axis row+col", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "M",
               matrix(c(1, 2, 3, 4, 5, 6), nrow = 3))
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(2L, 3L, 1L)))
    expect_length(plan$planned_matrices, 1L)
    expect_identical(plan$planned_matrices[[1L]]$rows_axis, "cell")
    expect_identical(plan$planned_matrices[[1L]]$columns_axis, "gene")
})

test_that(".build_reorder_plan ignores axes that don't exist on the daf", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B"))
    plan <- dafr:::.build_reorder_plan(d, list(
        cell = c(2L, 1L),
        nonexistent = c(1L)
    ))
    expect_named(plan$planned_axes, "cell")
})

test_that(".build_reorder_plan handles sparse matrix nnz correctly", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    m <- Matrix::sparseMatrix(i = c(1L, 3L), j = c(1L, 2L),
                              x = c(1.0, 2.0), dims = c(3L, 2L))
    set_matrix(d, "cell", "gene", "S", m)
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(3L, 2L, 1L)))
    expect_length(plan$planned_matrices, 1L)
    expect_identical(plan$planned_matrices[[1L]]$n_replacement_elements, 2L)
})

test_that(".build_reorder_plan rejects non-list / unnamed permutations", {
    d <- memory_daf()
    expect_error(dafr:::.build_reorder_plan(d, c(1L, 2L)),
                 "named list")
    expect_error(dafr:::.build_reorder_plan(d, list(c(1L, 2L))),
                 "named list")
})
