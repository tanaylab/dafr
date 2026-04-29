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
