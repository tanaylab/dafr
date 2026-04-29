test_that("memory_daf format_replace_reorder permutes axis entries + vector values", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(10, 20, 30))
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(2L, 3L, 1L)))
    dafr:::format_replace_reorder(d, plan)
    expect_identical(axis_vector(d, "cell"), c("B", "C", "A"))
    expect_equal(unname(get_vector(d, "cell", "x")), c(20, 30, 10))
})

test_that("memory_daf permutes named vector preserving names alignment", {
    # The slice-14 contract: get_vector returns a named vector. After
    # reorder, names must align with values per the new axis order.
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(10, 20, 30))
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(2L, 3L, 1L)))
    dafr:::format_replace_reorder(d, plan)
    v <- get_vector(d, "cell", "x")
    expect_identical(names(v), c("B", "C", "A"))
    expect_equal(unname(v), c(20, 30, 10))
})

test_that("memory_daf permutes dense matrix rows + cols", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "M",
               matrix(c(1, 2, 3, 4, 5, 6), nrow = 3))
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(3L, 1L, 2L)))
    dafr:::format_replace_reorder(d, plan)
    m <- get_matrix(d, "cell", "gene", "M")
    # Row 1 was old row 3: c(3, 6) -> (3, 6)
    # Row 2 was old row 1: c(1, 4)
    # Row 3 was old row 2: c(2, 5)
    expect_equal(unname(m[, 1]), c(3, 1, 2))
    expect_equal(unname(m[, 2]), c(6, 4, 5))
    # Dimnames should reflect new axis order:
    expect_identical(rownames(m), c("C", "A", "B"))
})

test_that("memory_daf permutes sparse matrix correctly", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(i = c(1L, 2L, 3L), j = c(1L, 2L, 1L),
                               x = c(1.0, 2.0, 3.0), dims = c(3L, 2L))
    set_matrix(d, "cell", "gene", "S", sp)
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(3L, 1L, 2L)))
    dafr:::format_replace_reorder(d, plan)
    m <- get_matrix(d, "cell", "gene", "S")
    # Row 1 (was row 3): nz at col 1 with value 3
    # Row 2 (was row 1): nz at col 1 with value 1
    # Row 3 (was row 2): nz at col 2 with value 2
    expect_equal(as.numeric(m[1, ]), c(3.0, 0.0))
    expect_equal(as.numeric(m[2, ]), c(1.0, 0.0))
    expect_equal(as.numeric(m[3, ]), c(0.0, 2.0))
})

test_that("memory_daf bumps version counters after reorder", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1, 2))
    a0 <- axis_version_counter(d, "cell")
    v0 <- vector_version_counter(d, "cell", "x")
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(2L, 1L)))
    dafr:::format_replace_reorder(d, plan)
    expect_gt(axis_version_counter(d, "cell"), a0)
    expect_gt(vector_version_counter(d, "cell", "x"), v0)
})

test_that("memory_daf cache invalidates after reorder", {
    # Prime the cache, then reorder, then re-read -- must see new order.
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(10, 20, 30))
    v_before <- get_vector(d, "cell", "x")
    plan <- dafr:::.build_reorder_plan(d, list(cell = c(2L, 3L, 1L)))
    dafr:::format_replace_reorder(d, plan)
    v_after <- get_vector(d, "cell", "x")
    expect_equal(unname(v_after), c(20, 30, 10))
    expect_false(identical(v_before, v_after))
})

test_that("memory_daf format_cleanup_reorder is silent no-op", {
    d <- memory_daf()
    add_axis(d, "cell", c("A"))
    plan <- list(planned_axes = list(), planned_vectors = list(),
                 planned_matrices = list())
    expect_silent(dafr:::format_cleanup_reorder(d, plan))
})

test_that("memory_daf format_reset_reorder is silent no-op", {
    d <- memory_daf()
    expect_silent(dafr:::format_reset_reorder(d))
})

test_that("memory_daf reorder both axes of a matrix simultaneously", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    set_matrix(d, "cell", "gene", "M",
               matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
    plan <- dafr:::.build_reorder_plan(d, list(
        cell = c(2L, 1L),
        gene = c(3L, 1L, 2L)
    ))
    dafr:::format_replace_reorder(d, plan)
    m <- get_matrix(d, "cell", "gene", "M")
    # Original (cell x gene):
    #     X(1) Y(3) Z(5)
    # A   1    3    5
    # B   2    4    6
    # After cell perm c(2,1) and gene perm c(3,1,2):
    #     Z(5) X(1) Y(3)
    # B   6    2    4
    # A   5    1    3
    expect_equal(unname(m[1, ]), c(6, 2, 4))
    expect_equal(unname(m[2, ]), c(5, 1, 3))
    expect_identical(rownames(m), c("B", "A"))
    expect_identical(colnames(m), c("Z", "X", "Y"))
})

# ---- Public API ------------------------------------------------------------

test_that("reorder_axes() with no permutations is a no-op", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B"))
    expect_silent(reorder_axes(d))
    expect_identical(axis_vector(d, "cell"), c("A", "B"))
})

test_that("reorder_axes() rejects unnamed arguments", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B"))
    expect_error(reorder_axes(d, c(2L, 1L)),
                 "must be named")
})

test_that("reorder_axes() permutes the axis", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(10, 20, 30))
    reorder_axes(d, cell = c(2L, 3L, 1L))
    expect_identical(axis_vector(d, "cell"), c("B", "C", "A"))
    expect_equal(unname(get_vector(d, "cell", "x")), c(20, 30, 10))
})

test_that("reorder_axes() permutes multiple axes simultaneously", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    set_matrix(d, "cell", "gene", "M",
               matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
    reorder_axes(d, cell = c(2L, 1L), gene = c(3L, 1L, 2L))
    expect_identical(axis_vector(d, "cell"), c("B", "A"))
    expect_identical(axis_vector(d, "gene"), c("Z", "X", "Y"))
    m <- get_matrix(d, "cell", "gene", "M")
    expect_equal(unname(m[1, ]), c(6, 2, 4))  # cell B (was 2), genes Z/X/Y
    expect_equal(unname(m[2, ]), c(5, 1, 3))  # cell A (was 1), genes Z/X/Y
})

test_that("reset_reorder_axes() on a fresh memory_daf is a no-op", {
    d <- memory_daf()
    expect_silent(reset_reorder_axes(d))
})

# ---- Integration: categorical + idempotency ------------------------------

test_that("reorder_axes() preserves a categorical/factor vector's levels", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C", "D"))
    f <- factor(c("low", "high", "low", "med"), levels = c("low", "med", "high"))
    set_vector(d, "cell", "g", f)
    reorder_axes(d, cell = c(4L, 2L, 1L, 3L))
    out <- get_vector(d, "cell", "g")
    expect_s3_class(out, "factor")
    expect_identical(levels(out), c("low", "med", "high"))
    expect_equal(as.character(unname(out)), c("med", "high", "low", "low"))
})

test_that("reorder_axes() with identity permutation is functionally a no-op", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(10, 20, 30))
    reorder_axes(d, cell = c(1L, 2L, 3L))
    expect_identical(axis_vector(d, "cell"), c("A", "B", "C"))
    expect_equal(unname(get_vector(d, "cell", "x")), c(10, 20, 30))
})

test_that("reorder_axes() applied twice with the same permutation = identity inverse", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(10, 20, 30))
    perm <- c(2L, 3L, 1L)
    reorder_axes(d, cell = perm)
    # Compute the inverse: for perm[i] = j, inverse[j] = i.
    inverse <- integer(length(perm))
    inverse[perm] <- seq_along(perm)
    reorder_axes(d, cell = inverse)
    expect_identical(axis_vector(d, "cell"), c("A", "B", "C"))
    expect_equal(unname(get_vector(d, "cell", "x")), c(10, 20, 30))
})

test_that("reorder_axes() with empty axis (length 0) is a no-op", {
    d <- memory_daf()
    add_axis(d, "cell", character(0L))
    expect_silent(reorder_axes(d, cell = integer(0L)))
    expect_identical(axis_vector(d, "cell"), character(0L))
})

test_that("reset_reorder_axes() called repeatedly is idempotent", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B"))
    expect_silent(reset_reorder_axes(d))
    expect_silent(reset_reorder_axes(d))
    expect_silent(reset_reorder_axes(d))
})

# ---- Flipped-orientation matrix coverage ----------------------------------

test_that("memory_daf reorder permutes columns when matrix is at flipped orientation", {
    # Matrix stored as (gene, cell) where cell is being permuted.
    # Cell-permute should permute matrix COLUMNS, not rows.
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "gene", "cell", "M",
               matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))  # 2 genes x 3 cells
    # M[X, A]=1, M[Y, A]=2, M[X, B]=3, M[Y, B]=4, M[X, C]=5, M[Y, C]=6
    reorder_axes(d, cell = c(3L, 1L, 2L))
    m <- get_matrix(d, "gene", "cell", "M")
    # After permutation, cells in order C, A, B:
    expect_equal(unname(m[1, ]), c(5, 1, 3))    # gene X across new cell order
    expect_equal(unname(m[2, ]), c(6, 2, 4))    # gene Y
    expect_identical(rownames(m), c("X", "Y"))      # rows unchanged
    expect_identical(colnames(m), c("C", "A", "B")) # cols permuted
})
