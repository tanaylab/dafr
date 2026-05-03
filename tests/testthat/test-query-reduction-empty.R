# Parity port of DataAxesFormats.jl test/queries.jl >
# scalar > {vector,matrix} > reduction > {(), empty, !empty, !string}
# and vector > matrix > reduction > {column,row} > {empty, !empty, !string}.

# ---------------------------------------------------------------------------
# Vector reduction to scalar (>>): empty + default / no default / String
# ---------------------------------------------------------------------------

test_that(">> Sum on empty vector with || 0 returns 0", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "age", c(1, 2))
    expect_equal(get_query(d, "@ cell [ age < 0 ] : age >> Sum || 0"), 0)
})

test_that(">> Sum on empty vector with no default raises", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "age", c(1, 2))
    expect_error(
        get_query(d, "@ cell [ age < 0 ] : age >> Sum"),
        "IfMissing|empty"
    )
})

test_that(">> Sum on String vector raises (unsupported input type)", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "donor", c("A", "B"))
    expect_error(
        get_query(d, "@ cell : donor >> Sum"),
        "non-numeric|String|Sum|invalid|must be numeric"
    )
})

# ---------------------------------------------------------------------------
# Matrix reduction to scalar (>>): empty + default / no default / String
# ---------------------------------------------------------------------------

test_that(">> Sum on empty matrix with || 0 returns 0", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B"))
    set_vector(d, "cell", "age", c(1, 2))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 2, 1, 3), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B"))
    ))
    expect_equal(
        get_query(d, "@ cell [ age < 0 ] @ gene :: UMIs >> Sum || 0"), 0
    )
})

test_that(">> Sum on empty matrix with no default raises", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B"))
    set_vector(d, "cell", "age", c(1, 2))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 2, 1, 3), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B"))
    ))
    expect_error(
        get_query(d, "@ cell [ age < 0 ] @ gene :: UMIs >> Sum"),
        "IfMissing|empty"
    )
})

test_that(">> Sum on String matrix raises (unsupported input type)", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B"))
    set_matrix(d, "cell", "gene", "kind", matrix(
        c("A", "C", "B", "D"), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B"))
    ))
    expect_error(
        get_query(d, "@ cell @ gene :: kind >> Sum"),
        "non-numeric|String|Sum|invalid|must be numeric"
    )
})

# ---------------------------------------------------------------------------
# >| (per-row reduction): empty rows / empty cols / no default / String
# ---------------------------------------------------------------------------

test_that(">| Sum with empty rows yields empty result vector (no default needed)", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_vector(d, "cell", "age", c(1, 2))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    out <- get_query(d, "@ cell [ age < 0 ] @ gene :: UMIs >| Sum || 0")
    expect_length(out, 0L)
})

test_that(">| Sum with empty reduction-axis + default fills per-row defaults", {
    # NB: dafr currently does not parse a mask on the second axis of a
    # 2D query (`@ cell @ gene [ ... ] :: UMIs`). To exercise the
    # "reduce-axis empty" branch we put the mask on the first axis and
    # use >- (which reduces along the first axis), matching the Julia
    # `column > empty` semantics.
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_vector(d, "cell", "age", c(1, 2))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    out <- get_query(d, "@ cell [ age < 0 ] @ gene :: UMIs >- Sum || 0")
    expect_equal(out, c(A = 0, B = 0, C = 0))
})

test_that(">- Sum with empty reduction-axis + no default raises", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_vector(d, "cell", "age", c(1, 2))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    expect_error(
        get_query(d, "@ cell [ age < 0 ] @ gene :: UMIs >- Sum"),
        "IfMissing|empty"
    )
})

test_that(">| Sum on String matrix raises", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "kind", matrix(
        c("A", "B", "B", "A", "A", "B"), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    expect_error(
        get_query(d, "@ cell @ gene :: kind >| Sum"),
        "non-numeric|String|Sum|invalid|must be numeric"
    )
})

# ---------------------------------------------------------------------------
# >- (per-col reduction): empty rows + default / no default / String
# ---------------------------------------------------------------------------

test_that(">| Sum with empty output-axis yields empty result vector (no default needed)", {
    # >| reduces cols -> per-row vector. With nrow=0, result is empty.
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_vector(d, "cell", "age", c(1, 2))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    out <- get_query(d, "@ cell [ age < 0 ] @ gene :: UMIs >| Sum || 0")
    expect_length(out, 0L)
})

test_that(">- Sum on String matrix raises", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "kind", matrix(
        c("A", "B", "B", "A", "A", "B"), nrow = 2L,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    expect_error(
        get_query(d, "@ cell @ gene :: kind >- Sum"),
        "non-numeric|String|Sum|invalid|must be numeric"
    )
})
