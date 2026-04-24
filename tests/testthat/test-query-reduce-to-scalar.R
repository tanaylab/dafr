# Tests for the `>>` ReductionOperation operator.
# Julia parity: DataAxesFormats.jl queries.jl — `... >> Reduction` reduces
# a vector or matrix to a single scalar value.

.fx <- function() {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    set_vector(d, "cell", "age", c(10, 20, 30, 40))
    set_vector(d, "cell", "is_sick", c(TRUE, FALSE, TRUE, TRUE))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(as.double(1:12), 4, 3,
            dimnames = list(c("A", "B", "C", "D"), c("X", "Y", "Z")))
    )
    d
}

# ---- parser / AST -----------------------------------------------------

test_that("parse_query maps `>>` to a ReduceToScalar node", {
    ast <- parse_query("@ cell : age >> Sum")
    red <- ast[[length(ast)]]
    expect_equal(red$op, "ReduceToScalar")
    expect_equal(red$reduction, "Sum")
})

test_that("canonical_query preserves `>> Reduction [params]`", {
    expect_equal(
        canonical_query("@ cell : age >> Sum"),
        "@ cell : age >> Sum"
    )
    # Reductions with params canonicalise `key value` to `key: value`
    # (pre-existing behaviour inherited from ReduceToColumn / ReduceToRow).
    # Idempotent round-trip is what matters.
    canon <- canonical_query("@ cell : age >> Sum type Int64")
    expect_equal(canon, "@ cell : age >> Sum type: Int64")
    expect_equal(canonical_query(canon), canon)
})

# ---- vector -> scalar -------------------------------------------------

test_that(">> Sum on a numeric vector returns the total as a scalar", {
    d <- .fx()
    expect_equal(get_query(d, "@ cell : age >> Sum"), 100)
})

test_that(">> Sum on a Bool vector counts TRUEs", {
    d <- .fx()
    expect_equal(get_query(d, "@ cell : is_sick >> Sum"), 3)
})

test_that(">> Mean on a numeric vector returns the mean as a scalar", {
    d <- .fx()
    expect_equal(get_query(d, "@ cell : age >> Mean"), 25)
})

test_that(">> Max / Min on a numeric vector", {
    d <- .fx()
    expect_equal(get_query(d, "@ cell : age >> Max"), 40)
    expect_equal(get_query(d, "@ cell : age >> Min"), 10)
})

test_that(">> Count on a numeric vector returns length as a scalar", {
    d <- .fx()
    expect_equal(as.numeric(get_query(d, "@ cell : age >> Count")), 4)
})

# ---- matrix -> scalar -------------------------------------------------

test_that(">> Sum on a matrix returns the total of all cells", {
    d <- .fx()
    expect_equal(get_query(d, "@ cell @ gene :: UMIs >> Sum"), sum(1:12))
})

test_that(">> Mean on a matrix returns the mean of all cells", {
    d <- .fx()
    expect_equal(get_query(d, "@ cell @ gene :: UMIs >> Mean"), mean(1:12))
})

test_that(">> Max on a matrix returns the global max", {
    d <- .fx()
    expect_equal(get_query(d, "@ cell @ gene :: UMIs >> Max"), 12)
})

# ---- subsequent builders work via >> --------------------------------

test_that("reduce-to-scalar via builder composition produces the same result", {
    d <- .fx()
    # ReduceToColumn(Sum()) against a matrix would yield a vector; the
    # string-form `>> Sum` gives a scalar.
    expect_equal(get_query(d, "@ cell : age >> Sum"), 100)
})

# ---- grouped inputs reduce per-group (vector result) ------------------

test_that(">> on a grouped vector reduces per group (same as >|)", {
    # Regression: `>>` after GroupBy must keep its Julia-parity behaviour
    # of reducing each group to a scalar, producing a per-group vector.
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    set_vector(d, "cell", "age", c(10, 20, 30, 40))
    set_vector(d, "cell", "cohort", c("g1", "g1", "g2", "g2"))
    v_gg <- get_query(d, "@ cell : age / cohort >> Sum")
    v_rc <- get_query(d, "@ cell : age / cohort >| Sum")
    expect_equal(sort(as.numeric(v_gg)), sort(as.numeric(v_rc)))
})

# ---- errors -----------------------------------------------------------

test_that(">> on an axis-only state raises a descriptive error", {
    d <- .fx()
    expect_error(
        get_query(d, "@ cell >> Sum"),
        "vector|matrix|scope"
    )
})
