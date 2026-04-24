# Tests for top_k_per_col — exported user-facing kernel (U2).

test_that("top_k_per_col on dense matrix returns largest values", {
    m <- matrix(c(
        1,  2,  3,
       10, 20, 30,
      100, 50,  5,
        0, 40, 25
    ), nrow = 4, ncol = 3, byrow = TRUE)
    rownames(m) <- paste0("r", 1:4)
    colnames(m) <- paste0("c", 1:3)

    out <- top_k_per_col(m, k = 2L)
    expect_equal(dim(out$indices), c(2L, 3L))
    expect_equal(dim(out$values),  c(2L, 3L))
    expect_equal(out$indices[, "c1"], c(3L, 2L))  # 100, 10
    expect_equal(out$values[,  "c1"], c(100, 10))
    expect_equal(out$indices[, "c2"], c(3L, 4L))  # 50, 40
    expect_equal(out$indices[, "c3"], c(2L, 4L))  # 30, 25
})

test_that("top_k_per_col with use_abs keeps largest-magnitude values", {
    m <- matrix(c(
       -5,  1,
        2, -3,
        0,  4
    ), nrow = 3, ncol = 2, byrow = TRUE)
    out <- top_k_per_col(m, k = 1L, use_abs = TRUE)
    expect_equal(out$indices[1, 1], 1L)  # -5 has largest |.|
    expect_equal(out$values[1, 1], -5)   # value preserves sign
    expect_equal(out$indices[1, 2], 3L)
    expect_equal(out$values[1, 2], 4)
})

test_that("top_k_per_col pads with NA when k > number of qualifying rows", {
    m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
    out <- top_k_per_col(m, k = 5L)
    # Each column has only 2 real values; remaining 3 slots are NA.
    expect_equal(sum(!is.na(out$values)), 4L)
    expect_equal(sum(!is.na(out$indices)), 4L)
    expect_true(all(is.na(out$values[3:5, ])))
    expect_true(all(is.na(out$indices[3:5, ])))
})

test_that("top_k_per_col agrees between dense and CSC for positive matrices", {
    set.seed(3L)
    m <- matrix(pmax(0, rnorm(200) * 3), nrow = 20, ncol = 10)
    sp <- methods::as(m, "CsparseMatrix")

    a <- top_k_per_col(m, k = 3L)
    b <- top_k_per_col(sp, k = 3L)

    # Same indices/values up to ties. For truly distinct positive values
    # the ordering is deterministic.
    m_jit <- m + 1e-9 * matrix(seq_along(m), nrow = 20, ncol = 10)
    sp_jit <- methods::as(m_jit, "CsparseMatrix")
    aj <- top_k_per_col(m_jit, k = 3L)
    bj <- top_k_per_col(sp_jit, k = 3L)
    expect_equal(aj$indices, bj$indices)
    expect_equal(aj$values,  bj$values,  tolerance = 1e-12)
})

test_that("top_k_per_col rejects non-numeric or wrong shape", {
    expect_error(top_k_per_col(list(1, 2, 3), k = 1L), "must be a dense numeric matrix")
    expect_error(top_k_per_col(matrix(1:4, 2, 2), k = 0L), "positive integer")
    expect_error(top_k_per_col(matrix(1:4, 2, 2), k = -1L), "positive integer")
})

test_that("top_k_per_col preserves column names and ignores NAs in dense", {
    m <- matrix(c(
        1, NA,
        2,  3,
       NA,  4
    ), nrow = 3, ncol = 2, byrow = TRUE)
    colnames(m) <- c("alpha", "beta")

    out <- top_k_per_col(m, k = 2L)
    expect_equal(colnames(out$indices), c("alpha", "beta"))
    expect_equal(out$indices[, "alpha"], c(2L, 1L))
    # beta has values 3 (row 2) and 4 (row 3); largest first
    expect_equal(out$indices[, "beta"], c(3L, 2L))
    expect_equal(out$values[,  "beta"], c(4, 3))
})
