# Regression guard for copy_all: axes must be added exactly once and the
# call must not warn or error. Flagged in slice-9d-n carry-over as a
# possible double-write via copy_vector's internal add_axis path; with the
# current copies.R ordering (axes first, vectors later, axes guarded by
# `if (!format_has_axis(destination, ax))`) this is already prevented.
# Keep this test as a regression fence.

test_that("copy_all does not warn or error, and adds each axis exactly once", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2", "g3"))
    set_scalar(src, "organism", "human")
    set_vector(src, "cell", "donor", c("A", "B"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1:6, 2, 3))

    dst <- memory_daf(name = "dst")
    expect_no_warning(copy_all(dst, src, relayout = FALSE))
    expect_no_error({
        actual_cell <- axis_entries(dst, "cell")
        actual_gene <- axis_entries(dst, "gene")
    })
    expect_identical(actual_cell, c("c1", "c2"))
    expect_identical(actual_gene, c("g1", "g2", "g3"))

    # Full round-trip: each property made it across.
    expect_identical(get_scalar(dst, "organism"), "human")
    expect_identical(unname(get_vector(dst, "cell", "donor")), c("A", "B"))

    # copy_all on a fresh destination must also succeed without warnings.
    dst2 <- memory_daf(name = "dst2")
    expect_no_warning(copy_all(dst2, src, relayout = FALSE))
})
