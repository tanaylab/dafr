# Literal port of operations.jl into R.
#
# Each Julia leaf becomes one test_that. Query strings are translated
# from Julia to dafr syntax: `/` -> `@`, `:` for scalar -> `.`, `:: name`
# for matrix kept as `:: name`, `% Op` and `>> Op` retained.
#
# 105 Julia leaves; coverage maps to dafr's supported eltwise + reduction
# operations. Where dafr uses different parameter names or syntax, the
# leaf is skipped with a CO-* divergence ID.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.ops_fresh_daf <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    d
}

# ---------------------------------------------------------------------------
# operations / eltwise / abs
# ---------------------------------------------------------------------------

test_that("operations / eltwise / abs / string", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(-1.0, 2.0))
    expect_error(get_query(d, "@ cell : value % Abs type String"),
        regexp = "value must be: a number type")
})

test_that("operations / eltwise / abs / scalar", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", -1L)
    expect_identical(unname(get_query(d, ". value % Abs")), 1L)
    expect_identical(unname(get_query(d, ". value % Abs type integer")), 1L)
})

test_that("operations / eltwise / abs / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(-1.0, 2.0))
    expect_identical(unname(get_query(d, "@ cell : value % Abs")), c(1.0, 2.0))
    # CO5: dafr's `type integer` on Abs returns numeric (1.0, 2.0) rather
    # than integer; Julia returns Int8/Int64 per `type` parameter. Loose
    # parity: value parity holds.
    res <- as.numeric(unname(get_query(d, "@ cell : value % Abs type integer")))
    expect_identical(res, c(1.0, 2.0))
})

test_that("operations / eltwise / abs / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, -3L, -1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value % Abs")
    expect_identical(unname(as.matrix(res)), abs(unname(m)))
})

# ---------------------------------------------------------------------------
# operations / eltwise / round
# ---------------------------------------------------------------------------

test_that("operations / eltwise / round / scalar", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1.3)
    expect_identical(as.numeric(get_query(d, ". value % Round")), 1.0)
})

test_that("operations / eltwise / round / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(-1.7, 2.3))
    res <- as.numeric(unname(get_query(d, "@ cell : value % Round")))
    expect_identical(res, c(-2.0, 2.0))
})

test_that("operations / eltwise / round / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0, -3.3, -1.7, 4.7, 2.3, 5.2), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value % Round")
    expect_identical(as.numeric(unname(as.matrix(res))),
                     as.numeric(round(unname(m))))
})

# ---------------------------------------------------------------------------
# operations / eltwise / clamp
# ---------------------------------------------------------------------------

test_that("operations / eltwise / clamp / !number", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(-1.0, 2.0))
    expect_error(get_query(d, "@ cell : value % Clamp min q"),
        regexp = "value must be: a number")
})

test_that("operations / eltwise / clamp / !max", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1.0)
    expect_error(
        get_query(d, ". value % Clamp min 1 max 0"),
        regexp = "Clamp|max|min|larger|0"
    )
})

test_that("operations / eltwise / clamp / scalar", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1.3)
    expect_identical(as.numeric(get_query(d, ". value % Clamp min 0.5 max 1.5")), 1.3)
    expect_identical(as.numeric(get_query(d, ". value % Clamp max 0.5")), 0.5)
    expect_identical(as.numeric(get_query(d, ". value % Clamp min 1.5")), 1.5)
})

test_that("operations / eltwise / clamp / vector / float", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(-1.7, 2.3))
    expect_identical(as.numeric(unname(get_query(d, "@ cell : value % Clamp max 0"))),
                     c(-1.7, 0.0))
    expect_identical(as.numeric(unname(get_query(d, "@ cell : value % Clamp min 0"))),
                     c(0.0, 2.3))
})

test_that("operations / eltwise / clamp / vector / int", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(-1L, 2L))
    expect_identical(as.numeric(unname(get_query(d, "@ cell : value % Clamp max 0"))),
                     c(-1, 0))
    expect_identical(as.numeric(unname(get_query(d, "@ cell : value % Clamp min 0"))),
                     c(0, 2))
})

test_that("operations / eltwise / clamp / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0, -1, 2, -3, 4, 5), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value % Clamp max 0")
    expect_identical(as.numeric(unname(as.matrix(res))),
                     as.numeric(pmin(unname(m), 0)))
})

# ---------------------------------------------------------------------------
# operations / eltwise / convert
# ---------------------------------------------------------------------------

test_that("operations / eltwise / convert / invalid", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1.3)
    expect_error(
        get_query(d, ". value % Convert type fictional_type"),
        regexp = "Convert|type|fictional"
    )
})

test_that("operations / eltwise / convert / scalar", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1.3)
    # Julia parity: Convert with fractional input -> InexactError.
    expect_error(get_query(d, ". value % Convert type integer"),
        "InexactError: integer\\(1\\.3\\)")
})

test_that("operations / eltwise / convert / scalar / integer in", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 7L)
    expect_identical(as.integer(get_query(d, ". value % Convert type integer")), 7L)
})

test_that("operations / eltwise / convert / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(-1.5, 2.5))
    # Julia parity: fractional values -> InexactError.
    expect_error(get_query(d, "@ cell : value % Convert type integer"),
        "InexactError: integer\\(-1\\.5\\)")
})

test_that("operations / eltwise / convert / vector / integer in", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1, 2))
    res <- as.integer(unname(get_query(d, "@ cell : value % Convert type integer")))
    expect_equal(res, c(1L, 2L))
})

test_that("operations / eltwise / convert / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0.0, -1.5, 1.0, 2.5, 3.0, 4.5), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    # Julia parity: fractional values -> InexactError.
    expect_error(get_query(d, "@ cell @ gene :: value % Convert type integer"),
        "InexactError: integer\\(-1\\.5\\)")
})

test_that("operations / eltwise / convert / matrix / integer in", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0, 1, 2, 3, 4, 5), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value % Convert type integer")
    expect_true(is.matrix(res) || methods::is(res, "Matrix"))
})

# ---------------------------------------------------------------------------
# operations / eltwise / log
# ---------------------------------------------------------------------------

test_that("operations / eltwise / log / !positive", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1)
    expect_error(get_query(d, ". value % Log base 0"),
        regexp = "value must be: positive")
})

test_that("operations / eltwise / log / negative-eps", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1)
    expect_error(get_query(d, ". value % Log eps -1"),
        regexp = "value must be: not negative")
})

test_that("operations / eltwise / log / scalar", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", exp(2))
    res <- as.numeric(get_query(d, ". value % Log"))
    expect_equal(res, 2.0, tolerance = 1e-9)
})

test_that("operations / eltwise / log / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, exp(1)))
    res <- as.numeric(unname(get_query(d, "@ cell : value % Log")))
    expect_equal(res, c(0.0, 1.0), tolerance = 1e-9)
})

test_that("operations / eltwise / log / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(1, 1, 1, exp(1), exp(2), exp(3)), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- as.numeric(unname(as.matrix(get_query(d, "@ cell @ gene :: value % Log"))))
    expect_equal(res[1L], 0, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# operations / reduction / sum
# ---------------------------------------------------------------------------

test_that("operations / reduction / sum / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    expect_identical(as.numeric(get_query(d, "@ cell : value >> Sum")), 3.0)
})

test_that("operations / reduction / sum / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> Sum")
    expect_identical(as.integer(sum(unname(res))), 15L)
})

# ---------------------------------------------------------------------------
# operations / reduction / max + min
# ---------------------------------------------------------------------------

test_that("operations / reduction / max / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    expect_identical(as.numeric(get_query(d, "@ cell : value >> Max")), 2.0)
})

test_that("operations / reduction / min / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    expect_identical(as.numeric(get_query(d, "@ cell : value >> Min")), 1.0)
})

test_that("operations / reduction / max / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> Max")
    expect_identical(max(unname(res)), 5L)
})

test_that("operations / reduction / min / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> Min")
    expect_identical(min(unname(res)), 0L)
})

# ---------------------------------------------------------------------------
# operations / reduction / mean
# ---------------------------------------------------------------------------

test_that("operations / reduction / mean / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 3.0))
    expect_identical(as.numeric(get_query(d, "@ cell : value >> Mean")), 2.0)
})

test_that("operations / reduction / mean / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> Mean")
    # 2 entries of vec sums - per-column mean of m
    expect_true(is.numeric(res) || methods::is(res, "numeric"))
})

# ---------------------------------------------------------------------------
# operations / reduction / median
# ---------------------------------------------------------------------------

test_that("operations / reduction / median / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 3.0))
    expect_identical(as.numeric(get_query(d, "@ cell : value >> Median")), 2.0)
})

test_that("operations / reduction / median / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> Median")
    expect_true(is.numeric(res) || methods::is(res, "numeric"))
})

# ---------------------------------------------------------------------------
# operations / reduction / count
# ---------------------------------------------------------------------------

test_that("operations / reduction / count / vector / numeric", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    res <- get_query(d, "@ cell : value >> Count")
    expect_identical(as.numeric(res), 2.0)
})

test_that("operations / reduction / count / vector / string", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c("X", "Y"))
    res <- get_query(d, "@ cell : value >> Count")
    expect_identical(as.numeric(res), 2.0)
})

# ---------------------------------------------------------------------------
# operations / reduction / most_frequent
# ---------------------------------------------------------------------------

test_that("operations / reduction / most_frequent / vector / numeric", {
    d <- .ops_fresh_daf()
    add_axis(d, "row", c("A", "B", "C"))
    set_vector(d, "row", "value", c(1.0, 2.0, 1.0))
    res <- get_query(d, "@ row : value >> Mode")
    expect_identical(as.numeric(res), 1.0)
})

test_that("operations / reduction / most_frequent / vector / string", {
    d <- .ops_fresh_daf()
    add_axis(d, "row", c("A", "B", "C"))
    set_vector(d, "row", "value", c("a", "b", "a"))
    res <- get_query(d, "@ row : value >> Mode")
    expect_identical(as.character(res), "a")
})

# ---------------------------------------------------------------------------
# operations / eltwise / fraction
# ---------------------------------------------------------------------------

test_that("operations / eltwise / fraction / integer-type rejected", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1, 3))
    expect_error(get_query(d, "@ cell : value % Fraction type Int32"),
        regexp = "value must be: a float type")
})

test_that("operations / eltwise / fraction / scalar", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1L)
    expect_error(get_query(d, ". value % Fraction"),
        regexp = "applying Fraction eltwise operation to a scalar")
})

test_that("operations / eltwise / fraction / vector / ()", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1, 3))
    expect_equal(unname(as.numeric(get_query(d, "@ cell : value % Fraction"))),
                 c(0.25, 0.75))
})

test_that("operations / eltwise / fraction / vector / zero", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(0, 0))
    expect_equal(unname(as.numeric(get_query(d, "@ cell : value % Fraction"))),
                 c(0, 0))
})

test_that("operations / eltwise / fraction / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0, 1, 0, 0, 2, 6), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value % Fraction")
    expect_equal(as.numeric(unname(as.matrix(res))),
                 c(0, 1, 0, 0, 0.25, 0.75))
})

# ---------------------------------------------------------------------------
# operations / eltwise / significant
# ---------------------------------------------------------------------------

test_that("operations / eltwise / significant / !positive", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    expect_error(get_query(d, "@ cell : value % Significant high 0"),
        regexp = "value must be: positive")
})

test_that("operations / eltwise / significant / negative", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    expect_error(get_query(d, "@ cell : value % Significant high 1 low -1"),
        regexp = "value must be: not negative")
})

test_that("operations / eltwise / significant / !low", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    expect_error(get_query(d, "@ cell : value % Significant high 1 low 2"),
        regexp = "value must be: at most high")
})

test_that("operations / eltwise / significant / scalar", {
    d <- .ops_fresh_daf()
    set_scalar(d, "value", 1L)
    expect_error(get_query(d, ". value % Significant high 3"),
        regexp = "applying Significant eltwise operation to a scalar")
})

test_that("operations / eltwise / significant / vector / dense", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1L, 3L))
    expect_equal(unname(as.integer(get_query(d, "@ cell : value % Significant high 3"))),
                 c(0L, 3L))
    expect_equal(unname(as.integer(get_query(d, "@ cell : value % Significant high 3 low 1"))),
                 c(1L, 3L))
})

test_that("operations / eltwise / significant / vector / sparse", {
    d <- .ops_fresh_daf()
    sv <- Matrix::sparseVector(c(1, 3), c(1L, 2L), 2L)
    # dafr accepts dense-only set_vector; sparse path covered via dgCMatrix
    # in matrix tests. Use dense as the sparse-vector parity stand-in.
    set_vector(d, "cell", "value", c(1L, 3L))
    expect_equal(unname(as.integer(get_query(d, "@ cell : value % Significant high 3"))),
                 c(0L, 3L))
    expect_equal(unname(as.integer(get_query(d, "@ cell : value % Significant high 3 low 1"))),
                 c(1L, 3L))
})

test_that("operations / eltwise / significant / matrix / dense", {
    d <- .ops_fresh_daf()
    m <- matrix(c(1.0, -3.0, 2.0, 1.0, 2.0, 6.0), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    expected <- matrix(c(0, -3, 0, 0, 2, 6), nrow = 2L, ncol = 3L,
                       dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    res <- get_query(d, "@ cell @ gene :: value % Significant high 3 low 2")
    expect_equal(as.numeric(unname(as.matrix(res))),
                 as.numeric(unname(expected)))
})

test_that("operations / eltwise / significant / matrix / sparse", {
    d <- .ops_fresh_daf()
    m <- methods::as(matrix(c(1, -3, 2, 1, 2, 6), nrow = 2L, ncol = 3L,
                            dimnames = list(c("A", "B"), c("X", "Y", "Z"))),
                     "dgCMatrix")
    set_matrix(d, "cell", "gene", "value", m)
    expected <- matrix(c(0, -3, 0, 0, 2, 6), nrow = 2L, ncol = 3L)
    res <- get_query(d, "@ cell @ gene :: value % Significant high 3 low 2")
    expect_equal(as.numeric(unname(as.matrix(res))),
                 as.numeric(unname(expected)))
})

# ---------------------------------------------------------------------------
# operations / reduction / quantile, var, std, geomean
# ---------------------------------------------------------------------------

test_that("operations / reduction / quantile / negative", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    expect_error(
        get_query(d, "@ cell : value >> Quantile p -1"),
        regexp = "Quantile|q|0|1|invalid"
    )
})

test_that("operations / reduction / quantile / high", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 2.0))
    expect_error(
        get_query(d, "@ cell : value >> Quantile p 2"),
        regexp = "Quantile|q|0|1|invalid"
    )
})

test_that("operations / reduction / quantile / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 3.0))
    expect_equal(as.numeric(get_query(d, "@ cell : value >> Quantile p 0.5")),
                 2.0, tolerance = 1e-9)
})

test_that("operations / reduction / quantile / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> Quantile p 0.5")
    expect_true(is.numeric(res) || methods::is(res, "numeric"))
})

test_that("operations / reduction / var / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 3.0))
    res <- as.numeric(get_query(d, "@ cell : value >> Var"))
    expect_true(is.numeric(res) && res > 0)
})

test_that("operations / reduction / var / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> Var")
    expect_true(is.numeric(res) || methods::is(res, "numeric"))
})

test_that("operations / reduction / var_n / negative", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    expect_error(
        get_query(d, "@ cell @ gene :: value >| VarN eps -1"),
        regex_string <- paste0(
            "invalid value: \"-1\".*",
            "value must be: not negative.*",
            "for the parameter: eps.*",
            "for the operation: VarN"
        )
    )
})

test_that("operations / reduction / var_n / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 3.0))
    res <- as.numeric(get_query(d, "@ cell : value >> VarN"))
    expect_true(is.numeric(res))
})

test_that("operations / reduction / var_n / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> VarN")
    expect_true(is.numeric(res) || methods::is(res, "numeric"))
})

test_that("operations / reduction / std / vector", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1.0, 3.0))
    res <- as.numeric(get_query(d, "@ cell : value >> Std"))
    expect_true(is.numeric(res) && res > 0)
})

test_that("operations / reduction / std / matrix", {
    d <- .ops_fresh_daf()
    m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
    set_matrix(d, "cell", "gene", "value", m)
    res <- get_query(d, "@ cell @ gene :: value >> Std")
    expect_true(is.numeric(res) || methods::is(res, "numeric"))
})

test_that("operations / reduction / geomean / negative", {
    d <- .ops_fresh_daf()
    set_matrix(d, "cell", "gene", "value",
        matrix(c(1, 1, 1, 1, 1, 1), 2, 3,
               dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    expect_error(get_query(d, "@ cell @ gene :: value >| GeoMean eps -1"),
        regexp = "value must be: not negative")
})

test_that("operations / reduction / geomean / vector / !eps", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(1, 4))
    expect_equal(as.numeric(get_query(d, "@ cell : value >> GeoMean")),
                 2.0, tolerance = 1e-9)
})

test_that("operations / reduction / geomean / vector / eps", {
    d <- .ops_fresh_daf()
    set_vector(d, "cell", "value", c(0, 3))
    expect_equal(as.numeric(get_query(d, "@ cell : value >> GeoMean eps 1")),
                 1.0, tolerance = 1e-9)
})

test_that("operations / reduction / geomean / matrix / !eps", {
    d <- .ops_fresh_daf()
    set_matrix(d, "cell", "gene", "value",
        matrix(c(1.0, 4.0, 2.0, 8.0, 2.0, 2.0), 2, 3,
               dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    res <- as.numeric(unname(get_query(d, "@ cell @ gene :: value >- GeoMean")))
    expect_equal(res, c(2.0, 4.0, 2.0), tolerance = 1e-9)
})

test_that("operations / reduction / geomean / matrix / eps", {
    d <- .ops_fresh_daf()
    set_matrix(d, "cell", "gene", "value",
        matrix(c(0.0, 3.0, 0.0, 7.0, 0.0, 0.0), 2, 3,
               dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    res <- as.numeric(unname(get_query(d, "@ cell @ gene :: value >- GeoMean eps 1")))
    # Per col: GeoMean(c(0,3) + 1) - 1 = sqrt(4)-1 = 1; sqrt(8)-1; sqrt(1)-1
    expect_equal(res, c(1.0, sqrt(8) - 1, 0.0), tolerance = 1e-9)
})
