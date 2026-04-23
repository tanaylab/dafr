test_that("compact_groups renumbers non-zero indices in first-seen order", {
    res <- compact_groups(c(5L, 10L, 5L, 0L, 10L))
    expect_identical(res$n_groups, 2L)
    expect_identical(res$group_indices, c(1L, 2L, 1L, 0L, 2L))
})

test_that("compact_groups is identity on already-compact input", {
    res <- compact_groups(c(1L, 2L, 1L, 0L, 2L))
    expect_identical(res$n_groups, 2L)
    expect_identical(res$group_indices, c(1L, 2L, 1L, 0L, 2L))
})

test_that("compact_groups handles all-zero input", {
    res <- compact_groups(c(0L, 0L, 0L))
    expect_identical(res$n_groups, 0L)
    expect_identical(res$group_indices, c(0L, 0L, 0L))
})

test_that("compact_groups handles empty input", {
    res <- compact_groups(integer(0L))
    expect_identical(res$n_groups, 0L)
    expect_identical(res$group_indices, integer(0L))
})

test_that("collect_group_members is the inverse of group-index assignment", {
    members <- collect_group_members(c(1L, 2L, 1L, 0L, 2L))
    expect_length(members, 2L)
    expect_identical(members[[1L]], c(1L, 3L))
    expect_identical(members[[2L]], c(2L, 5L))
})

test_that("collect_group_members ignores 0-index entries", {
    members <- collect_group_members(c(0L, 0L, 1L))
    expect_length(members, 1L)
    expect_identical(members[[1L]], 3L)
})

test_that("group_names produces deterministic prefix+hash names", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    g1 <- group_names(d, "cell", list(c(1L, 2L), c(3L, 4L)), prefix = "grp_")
    g2 <- group_names(d, "cell", list(c(1L, 2L), c(3L, 4L)), prefix = "grp_")
    expect_identical(g1, g2)
    expect_true(all(startsWith(g1, "grp_")))
    expect_length(g1, 2L)
})

test_that("group_names differs for different members", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    g1 <- group_names(d, "cell", list(c(1L, 2L)), prefix = "grp_")
    g2 <- group_names(d, "cell", list(c(1L, 3L)), prefix = "grp_")
    expect_false(identical(g1, g2))
})

test_that("group_names is member-order invariant", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    g1 <- group_names(d, "cell", list(c(1L, 2L, 3L)), prefix = "")
    g2 <- group_names(d, "cell", list(c(3L, 1L, 2L)), prefix = "")
    expect_identical(g1, g2)
})

test_that("group_names errors on out-of-range indices", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    expect_error(group_names(d, "cell", list(c(1L, 5L)), prefix = ""))
})
