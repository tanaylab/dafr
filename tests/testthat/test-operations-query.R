# End-to-end: verify every new Slice-7 op is reachable from a query string.
# Uses a small in-memory daf: a stored vector for eltwise, a matrix for
# reductions. Tests round-trip through the parser, evaluator, and op dispatch.

.slice7_query_daf <- function() {
    d <- memory_daf(name = "s7")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "values", c(1, 2, 3, 4))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4, ncol = 2,
            dimnames = list(c("A", "B", "C", "D"), c("g1", "g2"))))
    d
}

test_that("% Clamp via query clamps a vector", {
    d <- .slice7_query_daf()
    out <- get_query(d, "@ cell : values % Clamp min: 2 max: 3")
    expect_equal(unname(out), c(2, 2, 3, 3))
})

test_that("% Convert via query converts to integer", {
    d <- .slice7_query_daf()
    out <- get_query(d, "@ cell : values % Convert type: integer")
    expect_type(out, "integer")
})

test_that("% Fraction via query normalises a vector to sum 1", {
    d <- .slice7_query_daf()
    out <- get_query(d, "@ cell : values % Fraction")
    expect_equal(sum(out), 1)
    expect_equal(unname(out), c(1, 2, 3, 4) / 10)
})

test_that("% Significant via query zeroes below-threshold values", {
    d <- .slice7_query_daf()
    out <- get_query(d, "@ cell : values % Significant high: 3")
    # c(1,2,3,4): max |x| = 4 >= 3 -> keep entries >= low=3 -> c(0, 0, 3, 4)
    expect_equal(unname(out), c(0, 0, 3, 4))
})

test_that(">| Var and >| Std reduce a matrix column-wise to per-row vector", {
    d <- .slice7_query_daf()
    # Row i has values c(i, i+4). Uncorrected Var = 4 for every row.
    v_var <- get_query(d, "@ cell @ gene :: UMIs >| Var")
    expect_equal(unname(v_var), c(4, 4, 4, 4))
    v_std <- get_query(d, "@ cell @ gene :: UMIs >| Std")
    expect_equal(unname(v_std), c(2, 2, 2, 2))
})

test_that(">| VarN / StdN divide by row mean", {
    d <- .slice7_query_daf()
    # Row i: var = 4, mean = i+2. VarN = 4/(i+2); StdN = 2/(i+2).
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| VarN")),
        4 / (seq_len(4) + 2)
    )
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| StdN")),
        2 / (seq_len(4) + 2)
    )
})

test_that(">| Median / Quantile / GeoMean / Mode reach through query", {
    d <- .slice7_query_daf()
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| Median")),
        c(3, 4, 5, 6)
    )
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| Quantile p: 0.5")),
        c(3, 4, 5, 6)
    )
    # GeoMean of (1,5) = sqrt(5); (2,6) = sqrt(12); (3,7) = sqrt(21); (4,8) = sqrt(32)
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| GeoMean")),
        sqrt(c(5, 12, 21, 32))
    )
    # Mode of each row (all unique) -> first element: 1,2,3,4
    expect_equal(
        unname(get_query(d, "@ cell @ gene :: UMIs >| Mode")),
        c(1, 2, 3, 4)
    )
})
