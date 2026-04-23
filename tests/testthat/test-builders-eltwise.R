# Element-wise query builders (Phase B): Abs, Round, Significant,
# Clamp, Convert, Fraction, Log. Four assertion types per builder
# (construct, pipe-compose, AST identity, error on bad input) plus a
# handful of cross-cutting round-trip and composition cases.

.prior_query <- function(src = "@ cell : age") {
    DafrQuery(ast = parse_query(src), canonical = src)
}

# ---- Abs ------------------------------------------------------------------

test_that("Abs() builds a nullary eltwise query", {
    q <- Abs()
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "% Abs")
    expect_identical(q@ast, parse_query("% Abs"))
})

test_that("Abs composes with a prior DafrQuery via |>", {
    q <- .prior_query() |> Abs()
    expect_identical(q@canonical, "@ cell : age % Abs")
    expect_identical(q@ast, parse_query("@ cell : age % Abs"))
})

test_that("Abs rejects non-query arguments", {
    expect_error(Abs(42), "expects zero arguments")
    expect_error(Abs("x"), "expects zero arguments")
})

# ---- Round ----------------------------------------------------------------

test_that("Round() builds a nullary eltwise query", {
    q <- Round()
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "% Round")
    expect_identical(q@ast, parse_query("% Round"))
})

test_that("Round composes with a prior DafrQuery via |>", {
    q <- .prior_query() |> Round()
    expect_identical(q@canonical, "@ cell : age % Round")
    expect_identical(q@ast, parse_query("@ cell : age % Round"))
})

test_that("Round rejects non-query arguments", {
    expect_error(Round(0), "expects zero arguments")
})

# ---- Significant ----------------------------------------------------------

test_that("Significant(high=3) builds a parameterised eltwise query", {
    q <- Significant(high = 3)
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "% Significant high: 3")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Significant(high, low) serialises both params in order", {
    q <- Significant(high = 3, low = 2)
    expect_identical(q@canonical, "% Significant high: 3 low: 2")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Significant composes after a prior DafrQuery", {
    q <- .prior_query() |> Significant(high = 3)
    expect_identical(q@canonical, "@ cell : age % Significant high: 3")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Significant rejects non-scalar type", {
    expect_error(Significant(type = c("a", "b")), "character scalar")
    expect_error(Significant(type = 42), "character scalar")
})

# ---- Clamp ----------------------------------------------------------------

test_that("Clamp(min, max) builds a parameterised eltwise query", {
    q <- Clamp(min = 0, max = 1)
    expect_identical(q@canonical, "% Clamp min: 0 max: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Clamp composes after a prior DafrQuery via |>", {
    q <- .prior_query() |> Clamp(min = 0, max = 1)
    expect_identical(q@canonical, "@ cell : age % Clamp min: 0 max: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Clamp rejects bad type arg", {
    expect_error(Clamp(type = 42, min = 0, max = 1), "character scalar")
})

# ---- Convert --------------------------------------------------------------

test_that("Convert(type) builds an eltwise query with type param", {
    q <- Convert(type = "Int64")
    expect_identical(q@canonical, "% Convert type: Int64")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Convert composes after a prior DafrQuery via |>", {
    q <- .prior_query() |> Convert(type = "Int64")
    expect_identical(q@canonical, "@ cell : age % Convert type: Int64")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Convert rejects non-character type", {
    expect_error(Convert(type = 64), "character scalar")
})

# ---- Fraction -------------------------------------------------------------

test_that("Fraction() builds a bare eltwise query", {
    q <- Fraction()
    expect_identical(q@canonical, "% Fraction")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Fraction composes after a prior DafrQuery via |>", {
    q <- .prior_query() |> Fraction()
    expect_identical(q@canonical, "@ cell : age % Fraction")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Fraction accepts optional type", {
    q <- Fraction(type = "Float64")
    expect_identical(q@canonical, "% Fraction type: Float64")
})

test_that("Fraction rejects non-scalar type", {
    expect_error(Fraction(type = 1L), "character scalar")
})

# ---- Log ------------------------------------------------------------------

test_that("Log() with no params builds a bare eltwise query", {
    q <- Log()
    expect_identical(q@canonical, "% Log")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Log(base, eps) serialises named params", {
    q <- Log(base = 2, eps = 1)
    expect_identical(q@canonical, "% Log base: 2 eps: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Log composes after a prior DafrQuery via |>", {
    q <- .prior_query() |> Log(base = 2, eps = 1)
    expect_identical(q@canonical, "@ cell : age % Log base: 2 eps: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Log rejects non-character type", {
    expect_error(Log(type = 42), "character scalar")
})

# ---- Cross-cutting -------------------------------------------------------

test_that("Chained element-wise ops preserve order", {
    q <- .prior_query() |> Abs() |> Log(base = 2)
    expect_identical(q@canonical, "@ cell : age % Abs % Log base: 2")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Element-wise builders round-trip canonical through parse_query", {
    builders <- list(
        Abs(), Round(), Fraction(), Log(), Log(base = 2, eps = 1),
        Clamp(min = 0, max = 1), Convert(type = "Int64"),
        Significant(high = 3), Significant(high = 3, low = 2)
    )
    for (q in builders) {
        expect_identical(q@ast, parse_query(q@canonical))
    }
})

test_that("Piped builders preserve the prior query AST prefix", {
    prior <- .prior_query()
    q <- prior |> Log(base = 2)
    expect_identical(q@ast[seq_len(length(prior@ast))], prior@ast)
})
