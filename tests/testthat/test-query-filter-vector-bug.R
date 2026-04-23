# Regression tests for: `[filter] : vector` query evaluation skips the filter.
# The evaluator used to return the full vector after EndMask closed the mask,
# ignoring the filtered axis-entries state. See slice-11 cleanup.

test_that("filter + vector: mask is applied to the vector lookup", {
    d <- example_cells_daf()
    # experiment is a character vector on the cell axis in example_cells_daf.
    experiments <- unique(get_vector(d, "cell", "experiment"))
    stopifnot(length(experiments) >= 2L)
    target <- experiments[[1L]]

    q_mask_only <- sprintf('@ cell [ experiment = %s ]',
                           escape_value(target))
    filtered_entries <- get_query(d, q_mask_only)

    q_with_lookup <- sprintf('@ cell [ experiment = %s ] : donor',
                             escape_value(target))
    filtered_vec <- get_query(d, q_with_lookup)
    full_vec <- get_vector(d, "cell", "donor")

    # The filter is not a tautology for the equality predicate on experiment,
    # so the filtered vector is strictly shorter than the full one.
    expect_lt(length(filtered_vec), length(full_vec))
    # And it matches the number of cells that survived the mask.
    expect_identical(length(filtered_vec), length(filtered_entries))

    # Content check: the values at the surviving indices match.
    keep <- get_vector(d, "cell", "experiment") == target
    expect_identical(unname(filtered_vec), unname(full_vec[keep]))
})

test_that("filter + negated mask + vector: mask is applied to the vector lookup", {
    d <- example_cells_daf()
    experiments <- unique(get_vector(d, "cell", "experiment"))
    stopifnot(length(experiments) >= 2L)
    target <- experiments[[1L]]

    q <- sprintf('@ cell [ experiment != %s ] : donor',
                 escape_value(target))
    filtered_vec <- get_query(d, q)
    full_vec <- get_vector(d, "cell", "donor")
    keep <- get_vector(d, "cell", "experiment") != target

    expect_identical(length(filtered_vec), sum(keep))
    expect_identical(unname(filtered_vec), unname(full_vec[keep]))
})

test_that("filter + vector: no-op mask (all pass) yields full vector", {
    d <- example_cells_daf()
    experiments <- unique(get_vector(d, "cell", "experiment"))
    # Mask that matches every cell.
    # Use ! with a never-equal value so every cell passes.
    q <- '@ cell [ experiment != "__no_such_experiment__" ] : donor'
    filtered_vec <- get_query(d, q)
    full_vec <- get_vector(d, "cell", "donor")
    expect_identical(length(filtered_vec), length(full_vec))
    expect_identical(unname(filtered_vec), unname(full_vec))
})
