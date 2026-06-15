# Parity fix: copy_matrix must transpose-read a matrix that the source stores
# only in the FLIPPED layout, matching Julia copy_matrix! (which reads via
# get_matrix(...; relayout = true)). dafr used the orientation-strict
# format_has_matrix and errored "missing matrix". Audit probe copy-flipped-matrix.

test_that("copy_matrix transpose-reads from a flipped-only source (Julia parity)", {
    src <- memory_daf()
    add_axis(src, "cell", c("A", "B"))
    add_axis(src, "gene", c("X", "Y"))
    # store ONLY the flipped (gene, cell) orientation
    set_matrix(src, "gene", "cell", "UMIs",
               matrix(c(1, 2, 3, 4), 2L, 2L,
                      dimnames = list(c("X", "Y"), c("A", "B"))),
               relayout = FALSE)
    expect_false(has_matrix(src, "cell", "gene", "UMIs", relayout = FALSE))

    dst <- memory_daf()
    add_axis(dst, "cell", c("A", "B"))
    add_axis(dst, "gene", c("X", "Y"))
    # request the canonical (cell, gene) orientation: source has only the flip
    copy_matrix(dst, src, "cell", "gene", "UMIs", relayout = FALSE)

    expect_true(has_matrix(dst, "cell", "gene", "UMIs", relayout = FALSE))
    m <- as.matrix(get_matrix(dst, "cell", "gene", "UMIs"))
    # transpose of the stored [X,Y]x[A,B] = (cell A,B)x(gene X,Y) = [[1,2],[3,4]]
    expect_equal(unname(m), matrix(c(1, 3, 2, 4), 2L, 2L))
})
