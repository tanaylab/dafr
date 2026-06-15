# Parity fix: copy_all over a source that physically stores a matrix in BOTH
# layouts must copy each logical matrix once (when relayout=TRUE), like Julia's
# copy_matrices guard (columns_axis >= rows_axis). dafr iterated every ordered
# axis pair and collided on the second orientation ("existing matrix").
# Audit probe copy-all-both-layouts.

test_that("copy_all copies a both-layouts matrix once when relayout=TRUE", {
    src <- memory_daf()
    add_axis(src, "cell", c("A", "B"))
    add_axis(src, "gene", c("X", "Y", "Z"))
    set_matrix(src, "cell", "gene", "UMIs",
               matrix(seq_len(6), 2L, 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))),
               relayout = TRUE)
    # source physically has both orientations
    expect_true(has_matrix(src, "cell", "gene", "UMIs", relayout = FALSE))
    expect_true(has_matrix(src, "gene", "cell", "UMIs", relayout = FALSE))

    dst <- memory_daf()
    expect_no_error(copy_all(dst, src, relayout = TRUE))
    # both orientations present in the destination, copied without collision
    expect_true(has_matrix(dst, "cell", "gene", "UMIs", relayout = FALSE))
    expect_true(has_matrix(dst, "gene", "cell", "UMIs", relayout = FALSE))
})
