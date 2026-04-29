# Parity tests against the canonical examples in DataAxesFormats.jl
# queries.jl. Each `test_that` runs a single jldoctest and asserts the
# shape/value reported in the Julia docstring. The line numbers in the
# Julia source are recorded so the doctest can be located.

.fx_cells <- function() example_cells_daf()
.fx_metacells <- function() example_metacells_daf()
.fx_chain <- function() example_chain_daf()

# queries.jl:92  --  cells[". ?"]
test_that("Q jl:92  scalar names", {
    cells <- .fx_cells()
    out <- cells[". ?"]
    expect_setequal(out, c("organism", "reference"))
})

# queries.jl:105  --  cells["@ ?"]
test_that("Q jl:105  axis names", {
    cells <- .fx_cells()
    out <- cells["@ ?"]
    expect_setequal(out, c("gene", "experiment", "donor", "cell"))
})

# queries.jl:120  --  cells["@ gene : ?"]
test_that("Q jl:120  vector names on gene axis", {
    cells <- .fx_cells()
    out <- cells["@ gene : ?"]
    expect_setequal(out, c("is_lateral"))
})

# queries.jl:132  --  cells["@ cell @ gene :: ?"]
test_that("Q jl:132  matrix names on cell x gene", {
    cells <- .fx_cells()
    out <- cells["@ cell @ gene :: ?"]
    expect_setequal(out, c("UMIs"))
})

# queries.jl:162  --  cells["@ gene : is_lateral >> Sum type Int64"]
test_that("Q jl:162  reduce vector to scalar (Sum)", {
    cells <- .fx_cells()
    out <- cells["@ gene : is_lateral >> Sum type Int64"]
    expect_equal(unname(out), 438L)
})

# queries.jl:174  --  cells["@ cell @ gene :: UMIs >> Sum type Int64"]
test_that("Q jl:174  reduce matrix to scalar (Sum)", {
    cells <- .fx_cells()
    out <- cells["@ cell @ gene :: UMIs >> Sum type Int64"]
    expect_equal(unname(out), 1171936L)
})

# queries.jl:195  --  cells["@ experiment"]
test_that("Q jl:195  axis vector for experiment", {
    cells <- .fx_cells()
    out <- cells["@ experiment"]
    expect_length(out, 23L)
})

# queries.jl:254  --  cells["@ gene [ ! is_lateral ]"]
test_that("Q jl:254  bracketed mask, negated", {
    cells <- .fx_cells()
    out <- cells["@ gene [ ! is_lateral ]"]
    expect_length(out, 245L)
})

# queries.jl:293  --  cells["@ donor [ age > 60 & sex = male ]"]
test_that("Q jl:293  combined mask with & and = literal", {
    cells <- .fx_cells()
    out <- cells["@ donor [ age > 60 & sex = male ]"]
    expect_length(out, 29L)
})

# queries.jl:332  --  metacells["@ metacell : type"]
test_that("Q jl:332  vector lookup of type", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell : type"]
    expect_length(out, 7L)
    expect_true("memory-B" %in% out)
})

# queries.jl:355  --  metacells["@ gene :: fraction @ metacell = M412.08"]
test_that("Q jl:355  matrix-by-axis column slice (the user's reported bug)", {
    metacells <- .fx_metacells()
    out <- metacells["@ gene :: fraction @ metacell = M412.08"]
    expect_length(out, 683L)
    expect_equal(unname(round(out["RPL22"], 6)), 0.003736)
})

# queries.jl:386  --  metacells["@ metacell :: edge_weight @| M412.08"]
test_that("Q jl:386  square matrix column slice (@| entry)", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell :: edge_weight @| M412.08"]
    expect_length(out, 7L)
    expect_equal(unname(round(out["M1440.15"], 1)), 0.5)
})

# queries.jl:408  --  metacells["@ metacell :: edge_weight @- M412.08"]
test_that("Q jl:408  square matrix row slice (@- entry)", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell :: edge_weight @- M412.08"]
    expect_length(out, 7L)
    expect_equal(unname(round(out["M756.63"], 1)), 0.9)
})

# queries.jl:448  --  metacells["@ metacell : type : color"]
test_that("Q jl:448  chained vector lookup `: type : color`", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell : type : color"]
    expect_length(out, 7L)
    expect_equal(unname(out["M412.08"]), "steelblue")
})

# queries.jl:468  --  cells["@ donor : age % Clamp min 40 max 60 type Int64"]
test_that("Q jl:468  eltwise Clamp on vector", {
    cells <- .fx_cells()
    out <- cells["@ donor : age % Clamp min 40 max 60 type Int64"]
    expect_length(out, 95L)
    expect_true(all(out >= 40L & out <= 60L))
})

# queries.jl:498  --  cells["@ donor : age > 60"]
test_that("Q jl:498  comparator on vector returns bool vector", {
    cells <- .fx_cells()
    out <- cells["@ donor : age > 60"]
    expect_length(out, 95L)
    expect_type(unname(out), "logical")
})

# queries.jl:548  --  metacells["@ metacell : type =@ : color"]
test_that("Q jl:548  explicit AsAxis chain (=@)", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell : type =@ : color"]
    expect_length(out, 7L)
    expect_equal(unname(out["M412.08"]), "steelblue")
})

# queries.jl:566  --  metacells["@ metacell : type =@ type : color"]
test_that("Q jl:566  AsAxis with explicit axis name", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell : type =@ type : color"]
    expect_length(out, 7L)
    expect_equal(unname(out["M412.08"]), "steelblue")
})

# queries.jl:598  --  chain["@ cell : donor : age / metacell ?? : type >> Mean"]
test_that("Q jl:598  group-by with IfNot chained-lookup mean", {
    chain <- .fx_chain()
    out <- chain["@ cell : donor : age / metacell ?? : type >> Mean"]
    expect_length(out, 4L)
    expect_setequal(names(out), c("MEBEMP-E", "MEBEMP-L", "MPP", "memory-B"))
})

# queries.jl:620  --  chain["@ cell [ metacell ?? : type != memory-B ] : donor : age / metacell : type =@ >> Mean || 0"]
test_that("Q jl:620  mask + chain + AsAxis + group-by + IfMissing", {
    chain <- .fx_chain()
    out <- chain["@ cell [ metacell ?? : type != memory-B ] : donor : age / metacell : type =@ >> Mean || 0"]
    expect_length(out, 4L)
    expect_equal(unname(out["memory-B"]), 0)
})

# queries.jl:647  --  metacells["@ metacell @ gene :: fraction >| Max"]
test_that("Q jl:647  reduce matrix to column (Max)", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell @ gene :: fraction >| Max"]
    expect_length(out, 7L)
})

# queries.jl:668  --  metacells["@ metacell @ gene :: fraction >- Max"]
test_that("Q jl:668  reduce matrix to row (Max)", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell @ gene :: fraction >- Max"]
    expect_length(out, 683L)
})

# queries.jl:709  --  cells["@ cell @ gene :: UMIs"]
test_that("Q jl:709  matrix lookup", {
    cells <- .fx_cells()
    out <- cells["@ cell @ gene :: UMIs"]
    expect_equal(dim(out), c(856L, 683L))
})

# queries.jl:754  --  cells["@ cell : experiment * donor : sex"]
test_that("Q jl:754  count-by vector x vector", {
    cells <- .fx_cells()
    out <- cells["@ cell : experiment * donor : sex"]
    expect_equal(dim(out), c(23L, 2L))
})

# queries.jl:786  --  cells["@ cell : experiment =@ * donor : sex"]
test_that("Q jl:786  count-by with explicit AsAxis", {
    cells <- .fx_cells()
    out <- cells["@ cell : experiment =@ * donor : sex"]
    expect_equal(dim(out), c(23L, 2L))
})

# queries.jl:830  --  metacells["@ metacell @ gene :: fraction % Log base 2 eps 1e-5"]
test_that("Q jl:830  matrix Log eltwise (relayout-required)", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell @ gene :: fraction % Log base 2 eps 1e-5"]
    expect_equal(dim(out), c(7L, 683L))
})

# queries.jl:866  --  metacells["@ metacell @ gene :: fraction -/ type >- Mean"]
test_that("Q jl:866  group rows by (relayout-required)", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell @ gene :: fraction -/ type >- Mean"]
    expect_equal(dim(out), c(4L, 683L))
})

# queries.jl:889  --  metacells["@ metacell @ gene :: fraction -/ type =@ >- Mean"]
test_that("Q jl:889  group rows by with AsAxis (relayout-required)", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell @ gene :: fraction -/ type =@ >- Mean"]
    expect_equal(dim(out), c(4L, 683L))
})

# --- Audit-driven additions (post-2026-04-29 sweep, see audit/queries) ----
# These exercise patterns that the doctests don't cover but that DAF.jl
# supports per its `PHRASES` table and `test/queries.jl`.

test_that("scalar entry-pick on a vector at top level", {
    cells <- .fx_cells()
    expect_equal(unname(cells[": age @ donor = N16"]), 61)
})

test_that("scalar entry-pick on a matrix at top level", {
    metacells <- .fx_metacells()
    expect_equal(
        unname(metacells[":: edge_weight @ metacell = M412.08 @ metacell = M756.63"]),
        0.9, tolerance = 0.0001
    )
})

test_that("compare_matrix returns a bool matrix", {
    cells <- .fx_cells()
    out <- cells["@ cell @ gene :: UMIs > 0"]
    expect_equal(dim(out), c(856L, 683L))
    expect_true(is.logical(out))
})

test_that("compare on @ axis returns a bool vector along the axis", {
    metacells <- .fx_metacells()
    out <- metacells["@ metacell != ''"]
    expect_length(out, 7L)
    expect_true(all(out))
})

test_that("matrix-derived mask: cells expressing a gene", {
    cells <- .fx_cells()
    expressing <- cells["@ cell [ UMIs @ gene = RPL22 > 0 ]"]
    not_expressing <- cells["@ cell [ ! UMIs @ gene = RPL22 > 0 ]"]
    expect_equal(length(expressing) + length(not_expressing), 856L)
})

test_that("square-matrix mask: outgoing edges from a metacell are non-zero", {
    metacells <- .fx_metacells()
    kept <- metacells["@ metacell [ edge_weight @| M412.08 > 0 ]"]
    # The doctest shows weights 0.5 and 0.1 are non-zero, the rest are 0.
    expect_equal(length(kept), 2L)
})

test_that("[ ! prop > val ] negates the comparator (regression)", {
    # Pre-fix this returned the same set as `[ prop > val ]` because the
    # leading `!` was applied to the truthy mask before the comparator
    # overwrote it.
    cells <- .fx_cells()
    young <- cells["@ donor [ ! age > 60 ]"]
    old <- cells["@ donor [ age > 60 ]"]
    total <- length(cells["@ donor"])
    expect_equal(length(young) + length(old), total)
})

test_that("chained ?? sentinels are not clobbered by later chains", {
    # Reproduces DataAxesFormats.jl test/queries.jl:610.
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "metacell", c("X", "Y"))
    add_axis(d, "type", c("U", "V"))
    set_vector(d, "cell", "metacell", c("X", "Y", ""))
    set_vector(d, "metacell", "type", c("U", ""))
    set_vector(d, "type", "color", c("red", "green"))
    out <- d["@ cell : metacell ?? blue : type ?? magenta : color"]
    expect_equal(unname(out), c("red", "magenta", "blue"))
})

test_that("3-deep matrix chain `:: type : color` (lookup_vector_by_matrix)", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    add_axis(d, "type", c("U", "V"))
    set_matrix(d, "cell", "gene", "type",
        matrix(c("U", "V", "U", "V", "U", "V"), nrow = 2, ncol = 3))
    set_vector(d, "type", "color", c("red", "green"))
    out <- d["@ cell @ gene :: type : color"]
    expect_equal(dim(out), c(2L, 3L))
    expect_equal(out["X", "B"], "red")
    expect_equal(out["Y", "C"], "green")
})

test_that("eltwise on a scalar applies the function to a single value", {
    d <- memory_daf(name = "t")
    set_scalar(d, "score", -3.5)
    expect_equal(d[". score % Abs"], 3.5)
})

test_that("IfMissing accepts both `|| v Type` and `|| v type Type`", {
    cells <- .fx_cells()
    # Bare type form (Julia parity).
    expect_equal(cells[". missing || 0 Float32"], 0)
    expect_equal(cells[". missing || 1 Int64"], 1L)
    # Legacy `type` keyword still works.
    expect_equal(cells[". missing || 0 type Float32"], 0)
})

test_that("tokenizer: `''` is the empty-value token; `#` starts a line comment", {
    metacells <- .fx_metacells()
    # `''` round-trips with Julia's escape_value("").
    out <- metacells["@ metacell : type != ''"]
    expect_length(out, 7L)
    # Hash comments are whitespace.
    out2 <- metacells["@ metacell # filter to all\n: type"]
    expect_length(out2, 7L)
})
