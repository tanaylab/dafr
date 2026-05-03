# Parity with DataAxesFormats.jl: ordered/lexicographic comparators
# (`<`, `<=`, `>`, `>=`) in a `[ prop OP value ]` mask compare on the
# stored string value (Julia stores categoricals as plain Vector{String}
# at the storage boundary; queries.jl:2261-2400 uses native `<`).
#
# In R, a property may be stored as a factor — either set explicitly by
# the caller or loaded from h5ad categoricals via `.read_h5ad_categorical`
# (anndata_format.R:71). Default R semantics:
#
#   * `factor < "x"`            -> NA + warning (unordered)
#   * `ordered_factor < "x"`    -> compares via *level codes*, not labels
#
# Both diverge from Julia, which always compares on the label string.
# The mask helper has to coerce factor -> character before comparison.
#
# Equality already works on factors (R's Ops.factor delegates to a label
# comparison) and is exercised here only as a sanity check.

setup_factor_celltype <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    # Unordered factor: B < lexicographic threshold "M" -> {"B"}
    set_vector(d, "cell", "ct",
               factor(c("X", "B", "Z", "A", "M")))
    set_vector(d, "cell", "total_UMIs", c(10L, 20L, 30L, 40L, 50L))
    d
}

setup_ordered_factor_reversed_levels <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    # Levels reversed so factor codes disagree with lexical order.
    # Julia compares lexically; R ordered-factor `<` would compare by code.
    set_vector(d, "cell", "ct",
               factor(c("X", "B", "Z", "A", "M"),
                      levels = c("Z", "X", "M", "B", "A"),
                      ordered = TRUE))
    set_vector(d, "cell", "total_UMIs", c(10L, 20L, 30L, 40L, 50L))
    d
}

test_that("[ factor_prop = \"X\" ] picks the matching entries", {
    d <- setup_factor_celltype()
    out <- get_query(d, "@ cell [ ct = X ] : total_UMIs")
    expect_equal(length(out), 1L)
    expect_equal(sort(unname(out)), c(10L))
})

test_that("[ factor_prop < \"M\" ] compares lexically (unordered factor)", {
    # Lexical order: A, B < M; M, X, Z >= M. Survivors: B (20), A (40).
    d <- setup_factor_celltype()
    out <- get_query(d, "@ cell [ ct < M ] : total_UMIs")
    expect_equal(length(out), 2L)
    expect_equal(sort(unname(out)), c(20L, 40L))
})

test_that("[ factor_prop > \"M\" ] compares lexically (unordered factor)", {
    # Strictly greater than M lexically: X (10), Z (30).
    d <- setup_factor_celltype()
    out <- get_query(d, "@ cell [ ct > M ] : total_UMIs")
    expect_equal(length(out), 2L)
    expect_equal(sort(unname(out)), c(10L, 30L))
})

test_that("[ factor_prop <= \"M\" ] is inclusive lexical comparison", {
    # <= M: A, B, M -> 20, 40, 50.
    d <- setup_factor_celltype()
    out <- get_query(d, "@ cell [ ct <= M ] : total_UMIs")
    expect_equal(length(out), 3L)
    expect_equal(sort(unname(out)), c(20L, 40L, 50L))
})

test_that("[ ordered_factor < \"M\" ] uses lexical order, not level codes", {
    # Levels are c("Z","X","M","B","A") so internal codes give the
    # OPPOSITE order. Julia compares by string. Expected lexical
    # survivors of `< M`: B (20), A (40).
    d <- setup_ordered_factor_reversed_levels()
    out <- get_query(d, "@ cell [ ct < M ] : total_UMIs")
    expect_equal(length(out), 2L)
    expect_equal(sort(unname(out)), c(20L, 40L))
})

test_that("[ factor_prop ] (truthy mask) treats empty-string level as falsy", {
    # Sanity: .as_booleans already handles factor by going through
    # as.character(); empty string is falsy.
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "ct",
               factor(c("X", "", "Y"), levels = c("", "X", "Y")))
    set_vector(d, "cell", "total_UMIs", c(10L, 20L, 30L))
    out <- get_query(d, "@ cell [ ct ] : total_UMIs")
    expect_equal(length(out), 2L)
    expect_equal(sort(unname(out)), c(10L, 30L))
})
