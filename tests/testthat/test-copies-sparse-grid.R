# G8 (Round-7 follow-up): copies.jl > matrix > sparse > {superset, disjoint}
# > {rows, columns} > empty > {!zero, zero, nothing, value}.
#
# Julia exercises a full grid of sparse-matrix copy cases where the
# destination axis is a strict superset of the source axis (missing
# rows / cols to fill) or disjoint from it (no overlap). dafr's
# copy_matrix already matches Julia semantically; these tests lock
# the behaviour in so a future refactor cannot silently regress.
#
# Source: a 2 x 3 sparse Int CSC matrix
#   cell = [A, B]    gene = [X, Y, Z]
#   UMIs = [[0, 1, 2],
#           [3, 4, 0]]   (nnz = 4)

suppressMessages(library(Matrix))

.mk_src <- function() {
    s <- memory_daf("source")
    add_axis(s, "cell", c("A", "B"))
    add_axis(s, "gene", c("X", "Y", "Z"))
    set_matrix(
        s, "cell", "gene", "UMIs",
        as(matrix(c(0, 3, 1, 4, 2, 0), nrow = 2L, ncol = 3L),
           "CsparseMatrix")
    )
    s
}

# ---- superset rows: dst.cell = [A, B, C] -----------------------------

test_that("copy_matrix sparse / superset rows / () raises (missing empty)", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B", "C"))
    add_axis(dst, "gene", c("X", "Y", "Z"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs"),
        "missing entries"
    )
})

test_that("copy_matrix sparse / superset rows / empty=nothing raises", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B", "C"))
    add_axis(dst, "gene", c("X", "Y", "Z"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = NULL),
        "missing entries"
    )
})

test_that("copy_matrix sparse / superset rows / empty=!zero fills missing rows", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B", "C"))
    add_axis(dst, "gene", c("X", "Y", "Z"))
    expect_no_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = -1)
    )
    m <- as.matrix(get_matrix(dst, "cell", "gene", "UMIs"))
    expect_equal(unname(m),
                 matrix(c(0, 3, -1,  1, 4, -1,  2, 0, -1),
                        nrow = 3L, ncol = 3L))
})

test_that("copy_matrix sparse / superset rows / empty=0 fills with zeros", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B", "C"))
    add_axis(dst, "gene", c("X", "Y", "Z"))
    expect_no_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = 0)
    )
    m <- get_matrix(dst, "cell", "gene", "UMIs")
    expect_equal(unname(as.matrix(m)),
                 matrix(c(0, 3, 0,  1, 4, 0,  2, 0, 0),
                        nrow = 3L, ncol = 3L))
})

# ---- superset cols: dst.gene = [W, X, Y, Z] --------------------------

test_that("copy_matrix sparse / superset cols / () raises", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B"))
    add_axis(dst, "gene", c("W", "X", "Y", "Z"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs"),
        "missing entries"
    )
})

test_that("copy_matrix sparse / superset cols / empty=nothing raises", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B"))
    add_axis(dst, "gene", c("W", "X", "Y", "Z"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = NULL),
        "missing entries"
    )
})

test_that("copy_matrix sparse / superset cols / empty=!zero fills missing col", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B"))
    add_axis(dst, "gene", c("W", "X", "Y", "Z"))
    expect_no_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = -1)
    )
    m <- get_matrix(dst, "cell", "gene", "UMIs")
    expect_equal(unname(as.matrix(m)),
                 matrix(c(-1, -1,  0, 3,  1, 4,  2, 0),
                        nrow = 2L, ncol = 4L))
})

test_that("copy_matrix sparse / superset cols / empty=0 fills with zeros", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B"))
    add_axis(dst, "gene", c("W", "X", "Y", "Z"))
    expect_no_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = 0)
    )
    m <- get_matrix(dst, "cell", "gene", "UMIs")
    expect_equal(unname(as.matrix(m)),
                 matrix(c(0, 0,  0, 3,  1, 4,  2, 0),
                        nrow = 2L, ncol = 4L))
})

# ---- disjoint rows: dst.cell = [B, C] (overlap = {B}) ---------------

test_that("copy_matrix sparse / disjoint rows / () raises 'disjoint entries'", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("B", "C"))
    add_axis(dst, "gene", c("X", "Y", "Z"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs"),
        "disjoint entries.*axis: cell"
    )
})

test_that("copy_matrix sparse / disjoint rows / empty=nothing raises", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("B", "C"))
    add_axis(dst, "gene", c("X", "Y", "Z"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = NULL),
        "disjoint entries.*axis: cell"
    )
})

test_that("copy_matrix sparse / disjoint rows / empty=value still raises (any empty)", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("B", "C"))
    add_axis(dst, "gene", c("X", "Y", "Z"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = -1),
        "disjoint entries.*axis: cell"
    )
})

# ---- disjoint cols: dst.gene = [W, X, Y] (overlap = {X, Y}) ---------

test_that("copy_matrix sparse / disjoint cols / () raises 'disjoint entries'", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B"))
    add_axis(dst, "gene", c("W", "X", "Y"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs"),
        "disjoint entries.*axis: gene"
    )
})

test_that("copy_matrix sparse / disjoint cols / empty=nothing raises", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B"))
    add_axis(dst, "gene", c("W", "X", "Y"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = NULL),
        "disjoint entries.*axis: gene"
    )
})

test_that("copy_matrix sparse / disjoint cols / empty=value still raises", {
    dst <- memory_daf("dest")
    add_axis(dst, "cell", c("A", "B"))
    add_axis(dst, "gene", c("W", "X", "Y"))
    expect_error(
        copy_matrix(dst, .mk_src(), "cell", "gene", "UMIs", empty = -1),
        "disjoint entries.*axis: gene"
    )
})
