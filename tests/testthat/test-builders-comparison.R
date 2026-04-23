# Comparison query builders (Phase F): 8 value-op exports —
# IsEqual, IsNotEqual, IsGreater, IsGreaterEqual, IsLess, IsLessEqual,
# IsMatch, IsNotMatch.
#
# Test structure: 4 tests per operator.
#   1. Standalone build — DafrQuery membership + canonical string.
#   2. Compose via pipe after a vector lookup.
#   3. Numeric value (for </>/<=/>=/ =/!=) or regex pattern (for ~/!~) —
#      canonical-only round-trip (numeric AST values differ from parsed
#      character representation).
#   4. Errors when value/pattern is missing.

.prior_vector_query <- function(src = "@ cell : age") {
    DafrQuery(ast = parse_query(src), canonical = src)
}

# ---- IsEqual (= <v>) -------------------------------------------------------

test_that("IsEqual builds a = <value> query fragment", {
    q <- IsEqual("B")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "= B")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IsEqual composes after a vector lookup via pipe", {
    q <- .prior_vector_query() |> IsEqual("B")
    expect_identical(q@canonical, "@ cell : age = B")
    expect_identical(q@ast, parse_query("@ cell : age = B"))
})

test_that("IsEqual accepts numeric value (canonical-only round-trip)", {
    q <- IsEqual(42)
    expect_identical(q@canonical, "= 42")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IsEqual errors when value is missing", {
    expect_error(IsEqual(), "missing with no default")
})

# ---- IsNotEqual (!= <v>) ---------------------------------------------------

test_that("IsNotEqual builds a != <value> query fragment", {
    q <- IsNotEqual("B")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "!= B")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IsNotEqual composes after a vector lookup via pipe", {
    q <- .prior_vector_query() |> IsNotEqual("B")
    expect_identical(q@canonical, "@ cell : age != B")
    expect_identical(q@ast, parse_query("@ cell : age != B"))
})

test_that("IsNotEqual accepts numeric value (canonical-only round-trip)", {
    q <- IsNotEqual(0)
    expect_identical(q@canonical, "!= 0")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IsNotEqual errors when value is missing", {
    expect_error(IsNotEqual(), "missing with no default")
})

# ---- IsGreater (> <v>) -----------------------------------------------------

test_that("IsGreater builds a > <value> query fragment", {
    q <- IsGreater("10")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "> 10")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IsGreater composes after a vector lookup via pipe", {
    q <- .prior_vector_query() |> IsGreater("5")
    expect_identical(q@canonical, "@ cell : age > 5")
    expect_identical(q@ast, parse_query("@ cell : age > 5"))
})

test_that("IsGreater accepts numeric value (canonical-only round-trip)", {
    q <- IsGreater(2)
    expect_identical(q@canonical, "> 2")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IsGreater errors when value is missing", {
    expect_error(IsGreater(), "missing with no default")
})

# ---- IsGreaterEqual (>= <v>) -----------------------------------------------

test_that("IsGreaterEqual builds a >= <value> query fragment", {
    q <- IsGreaterEqual("10")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, ">= 10")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IsGreaterEqual composes after a vector lookup via pipe", {
    q <- .prior_vector_query() |> IsGreaterEqual("5")
    expect_identical(q@canonical, "@ cell : age >= 5")
    expect_identical(q@ast, parse_query("@ cell : age >= 5"))
})

test_that("IsGreaterEqual accepts numeric value (canonical-only round-trip)", {
    q <- IsGreaterEqual(18)
    expect_identical(q@canonical, ">= 18")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IsGreaterEqual errors when value is missing", {
    expect_error(IsGreaterEqual(), "missing with no default")
})

# ---- IsLess (< <v>) --------------------------------------------------------

test_that("IsLess builds a < <value> query fragment", {
    q <- IsLess("10")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "< 10")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IsLess composes after a vector lookup via pipe", {
    q <- .prior_vector_query() |> IsLess("5")
    expect_identical(q@canonical, "@ cell : age < 5")
    expect_identical(q@ast, parse_query("@ cell : age < 5"))
})

test_that("IsLess accepts numeric value (canonical-only round-trip)", {
    q <- IsLess(100)
    expect_identical(q@canonical, "< 100")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IsLess errors when value is missing", {
    expect_error(IsLess(), "missing with no default")
})

# ---- IsLessEqual (<= <v>) --------------------------------------------------

test_that("IsLessEqual builds a <= <value> query fragment", {
    q <- IsLessEqual("10")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "<= 10")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IsLessEqual composes after a vector lookup via pipe", {
    q <- .prior_vector_query() |> IsLessEqual("5")
    expect_identical(q@canonical, "@ cell : age <= 5")
    expect_identical(q@ast, parse_query("@ cell : age <= 5"))
})

test_that("IsLessEqual accepts numeric value (canonical-only round-trip)", {
    q <- IsLessEqual(65)
    expect_identical(q@canonical, "<= 65")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IsLessEqual errors when value is missing", {
    expect_error(IsLessEqual(), "missing with no default")
})

# ---- IsMatch (~ <pattern>) -------------------------------------------------

test_that("IsMatch builds a ~ <pattern> query fragment", {
    q <- IsMatch("celltype")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "~ celltype")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IsMatch composes after a vector lookup via pipe", {
    q <- .prior_vector_query() |> IsMatch("celltype")
    expect_identical(q@canonical, "@ cell : age ~ celltype")
    expect_identical(q@ast, parse_query("@ cell : age ~ celltype"))
})

test_that("IsMatch stores pattern as provided", {
    q <- IsMatch("^T")
    expect_identical(q@canonical, "~ ^T")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IsMatch errors when pattern is missing", {
    expect_error(IsMatch(), "missing with no default")
})

# ---- IsNotMatch (!~ <pattern>) ---------------------------------------------

test_that("IsNotMatch builds a !~ <pattern> query fragment", {
    q <- IsNotMatch("celltype")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "!~ celltype")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IsNotMatch composes after a vector lookup via pipe", {
    q <- .prior_vector_query() |> IsNotMatch("celltype")
    expect_identical(q@canonical, "@ cell : age !~ celltype")
    expect_identical(q@ast, parse_query("@ cell : age !~ celltype"))
})

test_that("IsNotMatch stores pattern as provided", {
    q <- IsNotMatch("^T")
    expect_identical(q@canonical, "!~ ^T")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IsNotMatch errors when pattern is missing", {
    expect_error(IsNotMatch(), "missing with no default")
})

# ---- Cross-cutting: full mask pipeline with comparison --------------------

test_that("comparison ops compose in a full mask pipeline", {
    q <- Axis("cell") |>
        BeginMask("age") |>
        IsGreater("18") |>
        EndMask() |>
        LookupVector("donor")
    expect_identical(q@canonical, "@ cell [ age > 18 ] : donor")
    expect_identical(q@ast, parse_query("@ cell [ age > 18 ] : donor"))
})

test_that("IsMatch composes inside a mask pipeline", {
    q <- Axis("cell") |>
        BeginMask("type") |>
        IsMatch("T") |>
        EndMask()
    expect_identical(q@canonical, "@ cell [ type ~ T ]")
    expect_identical(q@ast, parse_query("@ cell [ type ~ T ]"))
})
