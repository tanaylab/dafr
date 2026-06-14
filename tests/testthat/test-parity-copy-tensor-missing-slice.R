# Parity fix: copy_tensor with empty=NULL (the default) must SKIP a missing
# per-entry slice, matching Julia (default=empty=nothing -> get_matrix returns
# nothing -> copy_matrix! is a no-op). dafr mapped empty=NULL to .DAFR_UNDEF,
# which makes copy_matrix error "missing matrix" on the absent slice. copy_matrix
# already skips when default=NULL, so copy_tensor should pass default <- empty.
# Audit probe copy-tensor-missing-slice-no-empty.

test_that("copy_tensor with empty=NULL skips a missing slice (Julia parity)", {
    src <- memory_daf()
    add_axis(src, "batch", c("b1", "b2"))
    add_axis(src, "gene", c("g1", "g2"))
    add_axis(src, "cell", c("c1", "c2", "c3"))
    set_matrix(src, "gene", "cell", "b1_counts",
               matrix(seq_len(6), 2L, 3L), relayout = FALSE)
    # b2_counts deliberately absent

    dst <- memory_daf()
    add_axis(dst, "batch", c("b1", "b2"))
    add_axis(dst, "gene", c("g1", "g2"))
    add_axis(dst, "cell", c("c1", "c2", "c3"))

    expect_no_error(
        copy_tensor(dst, src, main_axis = "batch", rows_axis = "gene",
                    columns_axis = "cell", name = "counts",
                    empty = NULL, relayout = FALSE)
    )
    expect_true(has_matrix(dst, "gene", "cell", "b1_counts", relayout = FALSE))
    expect_false(has_matrix(dst, "gene", "cell", "b2_counts", relayout = FALSE))
})
