# Reduction query builders (Phase C): 19 exports across three
# categories — nullary (Max, Min), typed-reduction (Sum, Mean, Median,
# Count, Mode, GeoMean, Quantile, Std, StdN, Var, VarN),
# string-op (GroupBy, GroupRowsBy, GroupColumnsBy, CountBy), and the
# reduction-rewrapping builders (ReduceToColumn, ReduceToRow).

.prior_matrix_query <- function(src = "@ cell @ gene :: UMIs") {
    DafrQuery(ast = parse_query(src), canonical = src)
}

.prior_vector_query <- function(src = "@ cell : age") {
    DafrQuery(ast = parse_query(src), canonical = src)
}

# ---- Max (nullary) --------------------------------------------------------

test_that("Max() builds a nullary reduction query", {
    q <- Max()
    expect_true(S7::S7_inherits(q, DafrQuery))
    expect_identical(q@canonical, "% Max")
    expect_identical(q@ast, parse_query("% Max"))
})

test_that("Max composes via pipe", {
    q <- .prior_matrix_query() |> Max()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Max")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Max rejects non-query arguments", {
    expect_error(Max(42), "expects zero arguments")
})

# ---- Min (nullary) --------------------------------------------------------

test_that("Min() builds a nullary reduction query", {
    q <- Min()
    expect_identical(q@canonical, "% Min")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Min composes via pipe", {
    q <- .prior_matrix_query() |> Min()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Min")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Min rejects non-query arguments", {
    expect_error(Min("hi"), "expects zero arguments")
})

# ---- Mode (typed-reduction, numeric-only) --------------------------------

test_that("Mode() builds a bare eltwise query", {
    q <- Mode()
    expect_identical(q@canonical, "% Mode")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Mode composes via pipe", {
    q <- .prior_matrix_query() |> Mode()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Mode")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Mode rejects non-scalar type", {
    expect_error(Mode(type = 42), "character scalar")
})

# ---- Sum (typed-reduction) ------------------------------------------------

test_that("Sum() builds a bare reduction query", {
    q <- Sum()
    expect_identical(q@canonical, "% Sum")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Sum(type) serialises a type param", {
    q <- Sum(type = "Float64")
    expect_identical(q@canonical, "% Sum type: Float64")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Sum composes via pipe", {
    q <- .prior_matrix_query() |> Sum()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Sum")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Sum rejects non-scalar type", {
    expect_error(Sum(type = 42), "character scalar")
})

# ---- Mean (typed-reduction) -----------------------------------------------

test_that("Mean() builds a bare reduction query", {
    q <- Mean()
    expect_identical(q@canonical, "% Mean")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Mean(type) serialises a type param", {
    q <- Mean(type = "Float64")
    expect_identical(q@canonical, "% Mean type: Float64")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Mean composes via pipe", {
    q <- .prior_matrix_query() |> Mean()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Mean")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Mean rejects non-character type", {
    expect_error(Mean(type = 1L), "character scalar")
})

# ---- Median (typed-reduction) --------------------------------------------

test_that("Median() builds a bare reduction query", {
    q <- Median()
    expect_identical(q@canonical, "% Median")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Median(type) serialises a type param", {
    q <- Median(type = "Float64")
    expect_identical(q@canonical, "% Median type: Float64")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Median composes via pipe", {
    q <- .prior_matrix_query() |> Median()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Median")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Median rejects non-character type", {
    expect_error(Median(type = 0.5), "character scalar")
})

# ---- Count (typed-reduction) ---------------------------------------------

test_that("Count() builds a bare reduction query", {
    q <- Count()
    expect_identical(q@canonical, "% Count")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Count(type) serialises a type param", {
    q <- Count(type = "Int64")
    expect_identical(q@canonical, "% Count type: Int64")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Count composes via pipe", {
    q <- .prior_matrix_query() |> Count()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Count")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Count rejects non-character type", {
    expect_error(Count(type = 0L), "character scalar")
})

# ---- GeoMean (typed-reduction with eps) ----------------------------------

test_that("GeoMean() builds a bare reduction query", {
    q <- GeoMean()
    expect_identical(q@canonical, "% GeoMean")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GeoMean(eps) serialises eps param", {
    q <- GeoMean(eps = 1)
    expect_identical(q@canonical, "% GeoMean eps: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GeoMean composes via pipe with eps", {
    q <- .prior_matrix_query() |> GeoMean(eps = 1)
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % GeoMean eps: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GeoMean rejects non-scalar type", {
    expect_error(GeoMean(type = 42), "character scalar")
})

# ---- Quantile (typed-reduction with required p) ---------------------------

test_that("Quantile(p = 0.5) builds a reduction query with p param", {
    q <- Quantile(p = 0.5)
    # `0.5` contains a `.`, which is a query metacharacter — the
    # canonical escaper double-quotes the value. Round-trip still holds.
    expect_identical(q@canonical, "% Quantile p: \"0.5\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Quantile(p, type) serialises both params in order", {
    q <- Quantile(p = 0.9, type = "Float64")
    expect_identical(q@canonical, "% Quantile type: Float64 p: \"0.9\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Quantile composes via pipe", {
    q <- .prior_matrix_query() |> Quantile(p = 0.5)
    expect_identical(
        q@canonical,
        "@ cell @ gene :: UMIs % Quantile p: \"0.5\""
    )
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Quantile rejects non-scalar type", {
    expect_error(Quantile(type = 42, p = 0.5), "character scalar")
})

# ---- Std (typed-reduction) -----------------------------------------------

test_that("Std() builds a bare reduction query", {
    q <- Std()
    expect_identical(q@canonical, "% Std")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Std(type) serialises a type param", {
    q <- Std(type = "Float64")
    expect_identical(q@canonical, "% Std type: Float64")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Std composes via pipe", {
    q <- .prior_matrix_query() |> Std()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Std")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Std rejects non-character type", {
    expect_error(Std(type = 1L), "character scalar")
})

# ---- StdN (typed-reduction with eps) -------------------------------------

test_that("StdN() builds a bare reduction query", {
    q <- StdN()
    expect_identical(q@canonical, "% StdN")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("StdN(eps) serialises eps param", {
    q <- StdN(eps = 1)
    expect_identical(q@canonical, "% StdN eps: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("StdN composes via pipe with eps", {
    q <- .prior_matrix_query() |> StdN(eps = 1)
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % StdN eps: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("StdN rejects non-scalar type", {
    expect_error(StdN(type = 42), "character scalar")
})

# ---- Var (typed-reduction) -----------------------------------------------

test_that("Var() builds a bare reduction query", {
    q <- Var()
    expect_identical(q@canonical, "% Var")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Var(type) serialises a type param", {
    q <- Var(type = "Float64")
    expect_identical(q@canonical, "% Var type: Float64")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Var composes via pipe", {
    q <- .prior_matrix_query() |> Var()
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % Var")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("Var rejects non-character type", {
    expect_error(Var(type = 1L), "character scalar")
})

# ---- VarN (typed-reduction with eps) -------------------------------------

test_that("VarN() builds a bare reduction query", {
    q <- VarN()
    expect_identical(q@canonical, "% VarN")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("VarN(eps) serialises eps param", {
    q <- VarN(eps = 1)
    expect_identical(q@canonical, "% VarN eps: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("VarN composes via pipe with eps", {
    q <- .prior_matrix_query() |> VarN(eps = 1)
    expect_identical(q@canonical, "@ cell @ gene :: UMIs % VarN eps: 1")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("VarN rejects non-scalar type", {
    expect_error(VarN(type = 42), "character scalar")
})

# ---- GroupBy (string-op) --------------------------------------------------

test_that("GroupBy builds a group-by query fragment", {
    q <- GroupBy("donor")
    expect_identical(q@canonical, "/ donor")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GroupBy composes via pipe", {
    q <- .prior_vector_query() |> GroupBy("donor")
    expect_identical(q@canonical, "@ cell : age / donor")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GroupBy quotes property names with spaces", {
    q <- GroupBy("cell type")
    expect_identical(q@canonical, "/ \"cell type\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GroupBy rejects non-character property", {
    expect_error(GroupBy(42), "character scalar")
})

# ---- GroupRowsBy (string-op) ----------------------------------------------

test_that("GroupRowsBy builds a group-rows-by query fragment", {
    q <- GroupRowsBy("donor")
    expect_identical(q@canonical, "-/ donor")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GroupRowsBy composes via pipe", {
    q <- .prior_matrix_query() |> GroupRowsBy("type")
    expect_identical(q@canonical, "@ cell @ gene :: UMIs -/ type")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GroupRowsBy rejects non-character property", {
    expect_error(GroupRowsBy(list()), "character scalar")
})

# ---- GroupColumnsBy (string-op) -------------------------------------------

test_that("GroupColumnsBy builds a group-columns-by query fragment", {
    q <- GroupColumnsBy("type")
    expect_identical(q@canonical, "|/ type")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GroupColumnsBy composes via pipe", {
    q <- .prior_matrix_query() |> GroupColumnsBy("type")
    expect_identical(q@canonical, "@ cell @ gene :: UMIs |/ type")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("GroupColumnsBy rejects non-character property", {
    expect_error(GroupColumnsBy(0), "character scalar")
})

# ---- CountBy (string-op) --------------------------------------------------

test_that("CountBy builds a count-by query fragment", {
    q <- CountBy("donor")
    expect_identical(q@canonical, "* donor")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("CountBy composes via pipe", {
    q <- .prior_vector_query() |> CountBy("donor")
    expect_identical(q@canonical, "@ cell : age * donor")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("CountBy rejects non-character property", {
    expect_error(CountBy(TRUE), "character scalar")
})

# ---- ReduceToColumn (reduction rewrap) -----------------------------------

test_that("ReduceToColumn(Sum()) builds a >| Sum fragment", {
    q <- ReduceToColumn(Sum())
    expect_identical(q@canonical, ">| Sum")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("ReduceToColumn(Quantile(p)) carries the param across", {
    q <- ReduceToColumn(Quantile(p = 0.5))
    expect_identical(q@canonical, ">| Quantile p: \"0.5\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("ReduceToColumn composes via pipe with a reduction arg", {
    q <- .prior_matrix_query() |> GroupColumnsBy("type") |> ReduceToColumn(Mean())
    expect_identical(
        q@canonical,
        "@ cell @ gene :: UMIs |/ type >| Mean"
    )
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("ReduceToColumn errors when reduction is missing or wrong type", {
    expect_error(ReduceToColumn(), "missing with no default")
    expect_error(ReduceToColumn("Sum"), "DafrQuery")
})

# ---- ReduceToRow (reduction rewrap) --------------------------------------

test_that("ReduceToRow(Sum()) builds a >- Sum fragment", {
    q <- ReduceToRow(Sum())
    expect_identical(q@canonical, ">- Sum")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("ReduceToRow(Quantile(p)) carries the param across", {
    q <- ReduceToRow(Quantile(p = 0.9))
    expect_identical(q@canonical, ">- Quantile p: \"0.9\"")
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("ReduceToRow composes via pipe with a reduction arg", {
    q <- .prior_matrix_query() |> GroupRowsBy("type") |> ReduceToRow(Mean())
    expect_identical(
        q@canonical,
        "@ cell @ gene :: UMIs -/ type >- Mean"
    )
    expect_identical(q@ast, parse_query(q@canonical))
})

test_that("ReduceToRow errors when reduction is missing or wrong type", {
    expect_error(ReduceToRow(), "missing with no default")
    expect_error(ReduceToRow(42), "DafrQuery")
})

# ---- Cross-cutting --------------------------------------------------------

test_that("Reduction builders round-trip canonical through parse_query", {
    builders <- list(
        Max(), Min(), Mode(),
        Sum(), Sum(type = "Float64"),
        Mean(), Mean(type = "Float64"),
        Median(), Median(type = "Float64"),
        Count(), Count(type = "Int64"),
        GeoMean(), GeoMean(eps = 1),
        Quantile(p = 0.5), Quantile(p = 0.9, type = "Float64"),
        Std(), Std(type = "Float64"),
        StdN(), StdN(eps = 1),
        Var(), Var(type = "Float64"),
        VarN(), VarN(eps = 1),
        GroupBy("donor"), GroupRowsBy("donor"),
        GroupColumnsBy("type"), CountBy("donor"),
        ReduceToColumn(Sum()), ReduceToRow(Mean())
    )
    for (q in builders) {
        expect_identical(q@ast, parse_query(q@canonical))
    }
})

test_that("Full reduction pipeline round-trips end-to-end", {
    q <- .prior_matrix_query() |>
        GroupColumnsBy("type") |>
        ReduceToColumn(Mean())
    expect_identical(q@canonical, "@ cell @ gene :: UMIs |/ type >| Mean")
    expect_identical(q@ast, parse_query(q@canonical))
})
