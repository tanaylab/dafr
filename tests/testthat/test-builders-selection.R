# Selection/axis query builders (Phase D): 13 exports across four
# factories — string-op (Axis, BeginMask, BeginNegatedMask), nullary
# (EndMask, Names), value-op (IfMissing, SquareColumnIs, SquareRowIs),
# optional-string-op (AsAxis, IfNot, LookupScalar, LookupVector,
# LookupMatrix).

.prior_axis_query <- function(src = "@ cell") {
    DafrQuery(ast = parse_query(src), canonical = src)
}

.prior_vector_query <- function(src = "@ cell : age") {
    DafrQuery(ast = parse_query(src), canonical = src)
}

# ---- Axis (string-op) -----------------------------------------------------

test_that("Axis builds an @ <name> query fragment", {
    q <- Axis("cell")
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "@ cell")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Axis composes via pipe", {
    q <- .prior_axis_query() |> Axis("gene")
    expect_identical(q@canonical, "@ cell @ gene")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Axis with special chars round-trips through escape", {
    q <- Axis("name with spaces")
    expect_identical(q@canonical, "@ \"name with spaces\"")
    expect_identical(q@ast, parse_query("@ \"name with spaces\""))
})

test_that("Axis rejects non-character name", {
    expect_error(Axis(42), "character scalar")
})

# ---- BeginMask (string-op) ------------------------------------------------

test_that("BeginMask builds a [ <prop> query fragment", {
    q <- BeginMask("type")
    expect_identical(q@canonical, "[ type")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("BeginMask composes via pipe", {
    q <- .prior_axis_query() |> BeginMask("type")
    expect_identical(q@canonical, "@ cell [ type")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("BeginMask quotes property names with spaces", {
    q <- BeginMask("cell type")
    expect_identical(q@canonical, "[ \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("BeginMask rejects non-character property", {
    expect_error(BeginMask(42), "character scalar")
})

# ---- BeginNegatedMask (string-op) ----------------------------------------

test_that("BeginNegatedMask builds a [ ! <prop> query fragment", {
    q <- BeginNegatedMask("type")
    expect_identical(q@canonical, "[ ! type")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("BeginNegatedMask composes via pipe", {
    q <- .prior_axis_query() |> BeginNegatedMask("type")
    expect_identical(q@canonical, "@ cell [ ! type")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("BeginNegatedMask quotes property names with spaces", {
    q <- BeginNegatedMask("cell type")
    expect_identical(q@canonical, "[ ! \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("BeginNegatedMask rejects non-character property", {
    expect_error(BeginNegatedMask(TRUE), "character scalar")
})

# ---- EndMask (nullary) ----------------------------------------------------

test_that("EndMask builds a ] query fragment", {
    q <- EndMask()
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "]")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("EndMask composes via pipe", {
    q <- .prior_axis_query() |> BeginMask("type") |> EndMask()
    expect_identical(q@canonical, "@ cell [ type ]")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("EndMask rejects positional arguments", {
    expect_error(EndMask("foo"), "expects zero arguments")
})

# ---- Names (nullary) ------------------------------------------------------

test_that("Names builds a ? query fragment", {
    q <- Names()
    expect_identical(q@canonical, "?")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Names composes via pipe", {
    q <- .prior_axis_query() |> Names()
    expect_identical(q@canonical, "@ cell ?")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Names rejects positional arguments", {
    expect_error(Names("foo"), "expects zero arguments")
})

# ---- IfMissing (value-op) -------------------------------------------------

test_that("IfMissing builds a || <default> query fragment", {
    q <- IfMissing("N/A")
    expect_identical(q@canonical, "|| \"N/A\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IfMissing accepts numeric default (canonical-only round-trip)", {
    # Numeric defaults canonicalise via format() to bare tokens; the
    # parser reconstructs them as character. Canonical string matches;
    # AST differs only in the default field's storage type.
    q <- IfMissing(42)
    expect_identical(q@canonical, "|| 42")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IfMissing composes via pipe", {
    q <- .prior_vector_query() |> IfMissing(0)
    expect_identical(q@canonical, "@ cell : age || 0")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("IfMissing errors when default is missing", {
    expect_error(IfMissing(), "missing with no default")
})

# ---- SquareColumnIs (value-op) -------------------------------------------

test_that("SquareColumnIs builds a @| <value> query fragment", {
    q <- SquareColumnIs("M1")
    expect_identical(q@canonical, "@| M1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("SquareColumnIs composes via pipe", {
    q <- .prior_axis_query() |> SquareColumnIs("M1")
    expect_identical(q@canonical, "@ cell @| M1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("SquareColumnIs accepts numeric value (canonical-only round-trip)", {
    q <- SquareColumnIs(7)
    expect_identical(q@canonical, "@| 7")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("SquareColumnIs errors when value is missing", {
    expect_error(SquareColumnIs(), "missing with no default")
})

# ---- SquareRowIs (value-op) ----------------------------------------------

test_that("SquareRowIs builds a @- <value> query fragment", {
    q <- SquareRowIs("M1")
    expect_identical(q@canonical, "@- M1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("SquareRowIs composes via pipe", {
    q <- .prior_axis_query() |> SquareRowIs("M1")
    expect_identical(q@canonical, "@ cell @- M1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("SquareRowIs accepts numeric value (canonical-only round-trip)", {
    q <- SquareRowIs(7)
    expect_identical(q@canonical, "@- 7")
    expect_identical(canonical_query(q@canonical), q@canonical)
})

test_that("SquareRowIs errors when value is missing", {
    expect_error(SquareRowIs(), "missing with no default")
})

# ---- AsAxis (optional-string-op) -----------------------------------------

test_that("AsAxis() with no arg is nullary-like", {
    q <- AsAxis()
    expect_identical(q@canonical, "=@")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("AsAxis(name) becomes =@ <name>", {
    q <- AsAxis("cell")
    expect_identical(q@canonical, "=@ cell")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("AsAxis composes via pipe (with and without arg)", {
    q1 <- .prior_vector_query() |> AsAxis()
    expect_identical(q1@canonical, "@ cell : age =@")
    expect_identical(q1@ast, parse_query(q1@canonical))

    q2 <- .prior_vector_query() |> AsAxis("cell")
    expect_identical(q2@canonical, "@ cell : age =@ cell")
    expect_identical(q2@ast, parse_query(q2@canonical))
})

test_that("AsAxis rejects non-character axis_name", {
    expect_error(AsAxis(42), "character scalar or NULL")
})

# ---- IfNot (optional-string-op) ------------------------------------------

test_that("IfNot() with no arg is bare ??", {
    q <- IfNot()
    expect_identical(q@canonical, "??")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IfNot(value) becomes ?? <value>", {
    q <- IfNot("fallback")
    expect_identical(q@canonical, "?? fallback")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IfNot composes via pipe", {
    q <- .prior_vector_query() |> IfNot("fallback")
    expect_identical(q@canonical, "@ cell : age ?? fallback")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("IfNot rejects non-character value", {
    expect_error(IfNot(42), "character scalar or NULL")
})

# ---- LookupScalar (optional-string-op) -----------------------------------

test_that("LookupScalar() with no arg is bare .", {
    q <- LookupScalar()
    expect_identical(q@canonical, ".")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupScalar(name) becomes . <name>", {
    q <- LookupScalar("version")
    expect_identical(q@canonical, ". version")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupScalar composes via pipe", {
    q <- .prior_axis_query() |> LookupScalar("version")
    expect_identical(q@canonical, "@ cell . version")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupScalar rejects non-character name", {
    expect_error(LookupScalar(1L), "character scalar or NULL")
})

# ---- LookupVector (optional-string-op) -----------------------------------

test_that("LookupVector() with no arg is bare :", {
    q <- LookupVector()
    expect_identical(q@canonical, ":")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupVector(name) becomes : <name>", {
    q <- LookupVector("age")
    expect_identical(q@canonical, ": age")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupVector composes via pipe", {
    q <- .prior_axis_query() |> LookupVector("age")
    expect_identical(q@canonical, "@ cell : age")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupVector rejects non-character name", {
    expect_error(LookupVector(TRUE), "character scalar or NULL")
})

# ---- LookupMatrix (optional-string-op) -----------------------------------

test_that("LookupMatrix() with no arg is bare ::", {
    q <- LookupMatrix()
    expect_identical(q@canonical, "::")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupMatrix(name) becomes :: <name>", {
    q <- LookupMatrix("UMIs")
    expect_identical(q@canonical, ":: UMIs")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupMatrix composes via pipe", {
    q <- .prior_axis_query() |> Axis("gene") |> LookupMatrix("UMIs")
    expect_identical(q@canonical, "@ cell @ gene :: UMIs")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("LookupMatrix rejects non-character name", {
    expect_error(LookupMatrix(list()), "character scalar or NULL")
})

# ---- Cross-cutting --------------------------------------------------------

test_that("Selection builders round-trip canonical through parse_query", {
    # Builders whose AST stores character-typed scalars round-trip
    # through parse_query exactly. Numeric-valued builders (e.g.
    # IfMissing(42)) are covered separately via canonical-string equality.
    builders <- list(
        Axis("cell"), Axis("name with spaces"),
        BeginMask("type"), BeginNegatedMask("type"),
        EndMask(), Names(),
        IfMissing("N/A"),
        SquareColumnIs("M1"), SquareRowIs("M1"),
        AsAxis(), AsAxis("cell"),
        IfNot(), IfNot("fallback"),
        LookupScalar(), LookupScalar("v"),
        LookupVector(), LookupVector("age"),
        LookupMatrix(), LookupMatrix("UMIs")
    )
    for (q in builders) {
        expect_identical(q@ast, parse_query(q@canonical))
    }
})

test_that("Full selection pipeline round-trips end-to-end", {
    q <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("UMIs")
    expect_identical(q@canonical, "@ cell @ gene :: UMIs")
    expect_identical(q@ast, parse_query(q@canonical))

    q2 <- Axis("cell") |>
        BeginMask("type") |>
        EndMask() |>
        LookupVector("age")
    expect_identical(q2@canonical, "@ cell [ type ] : age")
    expect_identical(q2@ast, parse_query(q2@canonical))
})
