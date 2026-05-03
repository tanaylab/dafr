# Parity port of DataAxesFormats.jl test/queries.jl > eltwise leaves on
# scalar / vector / matrix queries, plus the matching !string error cases.

# ---------------------------------------------------------------------------
# Scalar % Abs
# ---------------------------------------------------------------------------

test_that(". score % Abs returns the absolute value of a scalar", {
    d <- memory_daf(name = "memory!")
    set_scalar(d, "score", -0.5)
    expect_equal(get_query(d, ". score % Abs"), 0.5)
})

test_that(". version % Abs errors when the scalar is a String", {
    d <- memory_daf(name = "memory!")
    set_scalar(d, "version", "0.1.2")
    expect_error(get_query(d, ". version % Abs"), "non-numeric|String|Abs")
})

# ---------------------------------------------------------------------------
# Vector % Abs
# ---------------------------------------------------------------------------

test_that("@ cell : score % Abs maps Abs over a numeric vector", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "score", c(-0.25, 0.5))
    expect_equal(get_query(d, "@ cell : score % Abs"), c(X = 0.25, Y = 0.5))
})

test_that("@ cell : type % Abs errors on a String vector", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "type", c("U", "V"))
    expect_error(get_query(d, "@ cell : type % Abs"), "non-numeric|String|Abs")
})

# ---------------------------------------------------------------------------
# Matrix % Abs
# ---------------------------------------------------------------------------

test_that("@ cell @ gene :: score % Abs maps Abs over a numeric matrix", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "score", matrix(
        c(0, 3, -1, -4, 2, 5),
        nrow = 2L, dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    out <- get_query(d, "@ cell @ gene :: score % Abs")
    expect_equal(dim(out), c(2L, 3L))
    expect_equal(out["X", ], c(A = 0, B = 1, C = 2))
    expect_equal(out["Y", ], c(A = 3, B = 4, C = 5))
})

test_that("@ cell @ gene :: level % Abs errors on a String matrix", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "level", matrix(
        c("low", "high", "middle", "middle", "high", "low"),
        nrow = 2L, dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    expect_error(
        get_query(d, "@ cell @ gene :: level % Abs"),
        "non-numeric|String|Abs"
    )
})
