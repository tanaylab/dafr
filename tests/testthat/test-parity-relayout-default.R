# Parity fix: set_matrix's `relayout` defaults to TRUE, matching Julia
# set_matrix!(...; relayout = true), which persists the flipped (transposed)
# layout so the matrix is directly available (and fast) in both orientations.
# dafr previously defaulted FALSE, so has_matrix(other, axis, relayout=FALSE)
# returned FALSE after a plain set_matrix - a silent behavioral divergence.
# Audit probes mem-setmat-relayout-default, P5-set-matrix-relayout-default.

test_that("set_matrix persists the flipped layout by default (Julia relayout=TRUE)", {
    d <- memory_daf()
    add_axis(d, "r", c("a", "b"))
    add_axis(d, "c", c("x", "y", "z"))
    m <- matrix(seq_len(6L), 2L, 3L, dimnames = list(c("a", "b"), c("x", "y", "z")))
    set_matrix(d, "r", "c", "M", m)   # default relayout (now TRUE)
    expect_true(has_matrix(d, "r", "c", "M", relayout = FALSE))
    expect_true(has_matrix(d, "c", "r", "M", relayout = FALSE))
})

test_that("set_matrix relayout=FALSE still stores only the given orientation", {
    d <- memory_daf()
    add_axis(d, "r", c("a", "b"))
    add_axis(d, "c", c("x", "y", "z"))
    m <- matrix(seq_len(6L), 2L, 3L, dimnames = list(c("a", "b"), c("x", "y", "z")))
    set_matrix(d, "r", "c", "M", m, relayout = FALSE)
    expect_true(has_matrix(d, "r", "c", "M", relayout = FALSE))
    expect_false(has_matrix(d, "c", "r", "M", relayout = FALSE))
})
