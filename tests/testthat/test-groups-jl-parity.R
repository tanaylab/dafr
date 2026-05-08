# Literal port of groups.jl into R. 3 leaves.

test_that("groups / compact", {
    group_indices <- c(0L, 3L, 1L, 3L, 1L)
    res <- compact_groups(group_indices)
    # dafr's compact_groups returns list(n_groups, group_indices); Julia's
    # compact_groups! mutates in place and returns the count of groups.
    # Both convey the same content (renumbered vector + count); the
    # parity assertion is on the substantive output.
    expect_identical(res$n_groups, 2L)
    expect_identical(res$group_indices, c(0L, 1L, 2L, 1L, 2L))
})

test_that("groups / collect", {
    res <- collect_group_members(c(0L, 1L, 2L, 1L, 2L))
    expect_length(res, 2L)
    expect_identical(unname(res[[1L]]), c(2L, 4L))
    expect_identical(unname(res[[2L]]), c(3L, 5L))
})

test_that("groups / names", {
    # CR2 (R divergence): dafr's group_names uses FNV32 hex hashes
    # ("Ge7dbdb80") while Julia uses simhash producing decimal-fraction
    # names ("G1.61"). Both are member-order invariant and stable on
    # input; only the hash function differs. The parity assertion is on
    # the SHAPE: 2 names, both prefix-tagged, deterministic.
    d <- memory_daf(name = "memory!")
    add_axis(d, "entry", c("A", "B", "C", "D", "E"))
    n1 <- group_names(d, "entry", list(c(2L, 4L), c(3L, 5L)), prefix = "G")
    expect_length(n1, 2L)
    expect_match(n1, "^G")
    # Stable hash: invariant to member order
    n1b <- group_names(d, "entry", list(c(4L, 2L), c(5L, 3L)), prefix = "G")
    expect_identical(n1, n1b)
    n2 <- group_names(d, "entry", list(c(2L, 3L), c(4L, 5L)), prefix = "G")
    expect_length(n2, 2L)
    # Different membership produces different names
    expect_false(any(n1 == n2))
})
