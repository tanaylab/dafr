# tests/testthat/test-kernel-grouped-rowsum.R
# Correctness tests for kernel_grouped_rowsum_dense_cpp (Slice 9b Fix C).
# The kernel accepts INTSXP or REALSXP, returns list(sum, sq).

# ---------------------------------------------------------------------------
# Test 1: Small integer matrix, G2 (row-grouped), Sum vs rowsum() reference.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_rowsum G2 integer Sum matches rowsum()", {
    set.seed(101)
    m <- matrix(sample(0L:10L, 30L, replace = TRUE), nrow = 6L, ncol = 5L)
    expect_true(is.integer(m))
    gi <- c(1L, 1L, 2L, 2L, 3L, 3L)  # 3 groups, 2 rows each
    ngroups <- 3L
    res <- dafr:::kernel_grouped_rowsum_dense_cpp(m, gi, ngroups,
                                                  need_sq = FALSE, axis = 2L, threshold = 1L)
    expected <- rowsum(m, gi, reorder = FALSE)
    # rowsum returns matrix with rownames = group labels and numeric (double) values.
    expect_equal(unname(res$sum), unname(expected + 0.0), tolerance = 1e-12)
    expect_null(res$sq)
})

# ---------------------------------------------------------------------------
# Test 2: Small double matrix, G2, Sum vs rowsum() reference.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_rowsum G2 double Sum matches rowsum()", {
    set.seed(102)
    m <- matrix(rnorm(24L), nrow = 6L, ncol = 4L)
    expect_true(is.double(m))
    gi <- c(1L, 2L, 1L, 2L, 1L, 2L)  # 2 groups, alternating rows
    ngroups <- 2L
    res <- dafr:::kernel_grouped_rowsum_dense_cpp(m, gi, ngroups,
                                                  need_sq = FALSE, axis = 2L, threshold = 1L)
    expected <- rowsum(m, gi, reorder = FALSE)
    expect_equal(unname(res$sum), unname(expected), tolerance = 1e-12)
    expect_null(res$sq)
})

# ---------------------------------------------------------------------------
# Test 3: Small integer matrix, G3 (col-grouped), Sum vs t(rowsum(t(m), g)).
# ---------------------------------------------------------------------------
test_that("kernel_grouped_rowsum G3 integer Sum matches t(rowsum(t(m), g))", {
    set.seed(103)
    m <- matrix(sample(0L:20L, 20L, replace = TRUE), nrow = 4L, ncol = 5L)
    expect_true(is.integer(m))
    gi <- c(1L, 2L, 1L, 2L, 1L)   # 2 groups for 5 columns
    ngroups <- 2L
    res <- dafr:::kernel_grouped_rowsum_dense_cpp(m, gi, ngroups,
                                                  need_sq = FALSE, axis = 3L, threshold = 1L)
    expected <- t(rowsum(t(m + 0.0), gi, reorder = FALSE))
    expect_equal(unname(res$sum), unname(expected), tolerance = 1e-12)
    expect_null(res$sq)
})

# ---------------------------------------------------------------------------
# Test 4: Integer matrix, G2, Var vs manual rowsum(m*m)/n - mu^2.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_rowsum G2 integer sum-of-squares matches manual formula", {
    set.seed(104)
    m <- matrix(sample(1L:50L, 40L, replace = TRUE), nrow = 8L, ncol = 5L)
    gi <- c(1L, 1L, 2L, 2L, 3L, 3L, 1L, 2L)  # 3 groups, unequal sizes
    ngroups <- 3L
    n_in_group <- as.integer(tabulate(gi, ngroups))
    res <- dafr:::kernel_grouped_rowsum_dense_cpp(m, gi, ngroups,
                                                  need_sq = TRUE, axis = 2L, threshold = 1L)
    # Reference: rowsum on double
    md <- m + 0.0
    rs  <- rowsum(md, gi, reorder = FALSE)
    rs2 <- rowsum(md * md, gi, reorder = FALSE)
    expect_equal(unname(res$sum), unname(rs),  tolerance = 1e-12)
    expect_equal(unname(res$sq),  unname(rs2), tolerance = 1e-12)
    # Verify the Var formula end-to-end.
    n  <- n_in_group
    mu <- rs / n
    var_ref <- pmax(rs2 / n - mu * mu, 0)
    mu2 <- res$sum / n
    var_got <- pmax(res$sq / n - mu2 * mu2, 0)
    expect_equal(unname(var_got), unname(var_ref), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Test 5: NA propagation — one NA in input makes corresponding accumulator NA.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_rowsum propagates NA correctly", {
    # Integer matrix: inject NA_integer_ at row 1, col 2 -> group 1 (gi[1]=1)
    m_int <- matrix(c(1L, 2L, 3L,     # row 1: group 1
                      4L, 5L, 6L,     # row 2: group 2
                      7L, 8L, 9L,     # row 3: group 1
                      10L, 11L, 12L), # row 4: group 2
                    nrow = 4L, ncol = 3L)
    m_int[1L, 2L] <- NA_integer_      # row 1 col 2 -> group 1, col 2 should be NA
    gi <- c(1L, 2L, 1L, 2L)
    ngroups <- 2L
    res_int <- dafr:::kernel_grouped_rowsum_dense_cpp(m_int, gi, ngroups,
                                                      need_sq = TRUE, axis = 2L, threshold = 1L)
    # col 2: group 1 has rows 1,3 -> row 1 is NA -> accumulator is NA
    expect_true(is.na(res_int$sum[1L, 2L]),  label = "sum: group 1 col 2 is NA")
    expect_true(is.na(res_int$sq[1L, 2L]),   label = "sq:  group 1 col 2 is NA")
    # Other cells are unaffected
    expect_false(is.na(res_int$sum[2L, 2L]), label = "sum: group 2 col 2 not NA")
    expect_false(is.na(res_int$sum[1L, 1L]), label = "sum: group 1 col 1 not NA")

    # Double matrix: inject NA_real_
    m_dbl <- matrix(c(1.0, 2.0, 3.0,
                      4.0, 5.0, 6.0,
                      7.0, 8.0, 9.0,
                      10.0, 11.0, 12.0),
                    nrow = 4L, ncol = 3L)
    m_dbl[3L, 1L] <- NA_real_         # row 3 col 1 -> group 1, col 1 should be NA
    res_dbl <- dafr:::kernel_grouped_rowsum_dense_cpp(m_dbl, gi, ngroups,
                                                      need_sq = FALSE, axis = 2L, threshold = 1L)
    expect_true(is.na(res_dbl$sum[1L, 1L]),  label = "dbl: group 1 col 1 is NA")
    expect_false(is.na(res_dbl$sum[2L, 1L]), label = "dbl: group 2 col 1 not NA")
})
