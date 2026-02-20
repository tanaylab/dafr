# Tests for newly implemented functions

test_that("example_chain_daf works", {
    chain <- example_chain_daf()
    expect_true(is_daf(chain))
    expect_equal(name(chain), "chain!")

    # Should contain data from both cells and metacells
    expect_true(has_axis(chain, "cell"))
    expect_true(has_axis(chain, "metacell"))
    expect_true(has_axis(chain, "gene"))

    chain_named <- example_chain_daf(name = "my_chain!")
    expect_equal(name(chain_named), "my_chain!")
})

test_that("version counters work", {
    daf <- memory_daf(name = "version_test!")
    add_axis(daf, "cell", c("A", "B", "C"))

    # Initial version
    v1 <- axis_version_counter(daf, "cell")
    expect_true(is.numeric(v1))

    # Add a vector and check its version
    set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))
    vec_v1 <- vector_version_counter(daf, "cell", "score")
    expect_true(is.numeric(vec_v1))

    # Update vector
    set_vector(daf, "cell", "score", c(4.0, 5.0, 6.0), overwrite = TRUE)
    vec_v2 <- vector_version_counter(daf, "cell", "score")
    expect_true(vec_v2 > vec_v1)

    # Add matrix and check version
    add_axis(daf, "gene", c("X", "Y"))
    set_matrix(daf, "cell", "gene", "expr", matrix(1:6, nrow = 3, ncol = 2))
    mat_v1 <- matrix_version_counter(daf, "cell", "gene", "expr")
    expect_true(is.numeric(mat_v1))

    # Update matrix
    set_matrix(daf, "cell", "gene", "expr", matrix(7:12, nrow = 3, ncol = 2), overwrite = TRUE)
    mat_v2 <- matrix_version_counter(daf, "cell", "gene", "expr")
    expect_true(mat_v2 > mat_v1)
})

test_that("is_axis_query works", {
    # Axis query should return TRUE
    expect_true(is_axis_query("/ cell"))

    # Vector query should return FALSE
    expect_false(is_axis_query("/ cell : age"))

    # Query with Axis() should return TRUE
    axis_q <- Axis("cell")
    expect_true(is_axis_query(axis_q))
})

test_that("query_axis_name works", {
    expect_equal(query_axis_name("/ cell"), "cell")
    expect_equal(query_axis_name("/ gene"), "gene")

    axis_q <- Axis("metacell")
    expect_equal(query_axis_name(axis_q), "metacell")
})

test_that("collect_group_members works", {
    # Simple case: entries 1,3 in group 1, entry 2 in group 2
    group_indices <- c(1L, 2L, 1L, 0L)
    members <- collect_group_members(group_indices)

    expect_equal(length(members), 2)
    expect_equal(sort(members[[1]]), c(1L, 3L))
    expect_equal(members[[2]], 2L)

    # All in one group
    all_one <- c(1L, 1L, 1L)
    members_one <- collect_group_members(all_one)
    expect_equal(length(members_one), 1)
    expect_equal(members_one[[1]], c(1L, 2L, 3L))
})

test_that("group_names works", {
    daf <- memory_daf(name = "group_test!")
    add_axis(daf, "cell", c("A", "B", "C", "D"))

    # Two groups: {A, C} and {B, D}
    entries_of_groups <- list(c(1L, 3L), c(2L, 4L))
    names <- group_names(daf, "cell", entries_of_groups, prefix = "G")

    expect_equal(length(names), 2)
    expect_true(all(startsWith(names, "G")))

    # Same members should produce same name
    entries_of_groups2 <- list(c(1L, 3L), c(2L, 4L))
    names2 <- group_names(daf, "cell", entries_of_groups2, prefix = "G")
    expect_equal(names, names2)
})

test_that("group_names with different prefixes", {
    daf <- memory_daf(name = "prefix_test!")
    add_axis(daf, "cell", c("A", "B"))

    entries_of_groups <- list(c(1L, 2L))

    # Different prefixes should produce different names
    names_m <- group_names(daf, "cell", entries_of_groups, prefix = "M")
    names_g <- group_names(daf, "cell", entries_of_groups, prefix = "G")

    expect_true(startsWith(names_m[[1]], "M"))
    expect_true(startsWith(names_g[[1]], "G"))
})

test_that("collect_group_members with empty groups", {
    # All entries in no group
    all_zeros <- c(0L, 0L, 0L)
    members <- collect_group_members(all_zeros)
    expect_equal(length(members), 0)
})

test_that("collect_group_members with single group", {
    # All entries in group 1
    single_group <- c(1L, 1L, 1L, 1L)
    members <- collect_group_members(single_group)

    expect_equal(length(members), 1)
    expect_equal(members[[1]], c(1L, 2L, 3L, 4L))
})

test_that("compact_groups works", {
    # Non-compact indices: gaps in group numbering
    group_indices <- c(5L, 10L, 5L, 0L, 10L)
    result <- compact_groups(group_indices)

    # Should return the number of groups (2 in this case: groups 5 and 10)
    expect_equal(result$n_groups, 2)

    # Indices should be compacted to 1, 2 (and 0 stays 0)
    expect_equal(result$group_indices[1], result$group_indices[3]) # Both were group 5
    expect_equal(result$group_indices[2], result$group_indices[5]) # Both were group 10
    expect_equal(result$group_indices[4], 0L) # 0 stays 0

    # All non-zero values should be in range 1 to n_groups
    non_zero <- result$group_indices[result$group_indices != 0]
    expect_true(all(non_zero >= 1))
    expect_true(all(non_zero <= result$n_groups))

    # Already compact case
    compact_indices <- c(1L, 2L, 1L, 2L)
    result2 <- compact_groups(compact_indices)
    expect_equal(result2$n_groups, 2)
    expect_equal(result2$group_indices, compact_indices)

    # All zeros case
    all_zeros <- c(0L, 0L, 0L)
    result3 <- compact_groups(all_zeros)
    expect_equal(result3$n_groups, 0)
    expect_equal(result3$group_indices, all_zeros)
})

test_that("VIEW_ALL constants are defined correctly", {
    expect_true(exists("VIEW_ALL_AXES"))
    expect_true(exists("VIEW_ALL_SCALARS"))
    expect_true(exists("VIEW_ALL_VECTORS"))
    expect_true(exists("VIEW_ALL_MATRICES"))
    expect_true(exists("VIEW_ALL_DATA"))

    # VIEW_ALL_DATA should contain the other three
    expect_equal(length(VIEW_ALL_DATA), 3)
})
