# Parity tests against the canonical jldoctest examples in
# DataAxesFormats.jl writers.jl. Each test_that asserts the value sequence
# documented in the Julia docstring (captured into
# tests/testthat/fixtures/julia-readers-writers/ by
# tools/julia-fixtures/capture_readers_writers.jl).

.fx_dir <- function() "fixtures/julia-readers-writers"
.fx <- function(name) {
    jsonlite::fromJSON(file.path(.fx_dir(), paste0(name, ".json")),
                       simplifyVector = TRUE)
}

# writers.jl:63  -- set_scalar! with overwrite (1.0 -> 2.0)
test_that("W jl:63  set_scalar overwrite=TRUE", {
    fx <- .fx("writers_set_scalar_overwrite")
    d  <- example_cells_daf()
    set_scalar(d, "version", 1.0)
    expect_equal(get_scalar(d, "version"), fx$first)
    set_scalar(d, "version", 2.0, overwrite = TRUE)
    expect_equal(get_scalar(d, "version"), fx$second)
})

# writers.jl:109 -- delete_scalar!(cells, "organism")
test_that("W jl:109 delete_scalar(cells, organism)", {
    fx <- .fx("writers_delete_scalar_organism")
    d  <- example_cells_daf()
    expect_identical(has_scalar(d, "organism"), fx$before)
    delete_scalar(d, "organism")
    expect_identical(has_scalar(d, "organism"), fx$after)
})

# writers.jl:161 -- add_axis!(cells, "block", ["B1","B2"])
test_that("W jl:161 add_axis(cells, block)", {
    fx <- .fx("writers_add_axis_block")
    d  <- example_cells_daf()
    expect_identical(has_axis(d, "block"), fx$before)
    add_axis(d, "block", c("B1", "B2"))
    expect_identical(has_axis(d, "block"), fx$after)
})

# writers.jl:217 -- delete_axis!(metacells, "type")
test_that("W jl:217 delete_axis(metacells, type)", {
    fx <- .fx("writers_delete_axis_type")
    m  <- example_metacells_daf()
    expect_identical(has_axis(m, "type"), fx$before)
    delete_axis(m, "type")
    expect_identical(has_axis(m, "type"), fx$after)
})

# writers.jl:300 -- set_vector!(metacells, "type", "is_mebemp", ...)
test_that("W jl:300 set_vector is_mebemp + overwrite sequence", {
    fx <- .fx("writers_set_vector_is_mebemp")
    m  <- example_metacells_daf()
    s0 <- has_vector(m, "type", "is_mebemp")
    set_vector(m, "type", "is_mebemp", c(TRUE, TRUE, FALSE, FALSE))
    s1 <- has_vector(m, "type", "is_mebemp")
    set_vector(m, "type", "is_mebemp", c(TRUE, TRUE, TRUE, FALSE),
               overwrite = TRUE)
    s2    <- has_vector(m, "type", "is_mebemp")
    final <- unname(get_vector(m, "type", "is_mebemp"))
    expect_identical(c(s0, s1, s2),       fx$stages)
    expect_identical(as.logical(final),   fx$final)
})

# writers.jl:618 -- delete_vector!(metacells, "type", "color")
test_that("W jl:618 delete_vector(metacells, type, color)", {
    fx <- .fx("writers_delete_vector_color")
    m  <- example_metacells_daf()
    expect_identical(has_vector(m, "type", "color"), fx$before)
    delete_vector(m, "type", "color")
    expect_identical(has_vector(m, "type", "color"), fx$after)
})

# writers.jl:686 -- set_matrix! with relayout flag (3-stage sequence)
test_that("W jl:686 set_matrix relayout=FALSE then overwrite=TRUE relayout=TRUE", {
    fx <- .fx("writers_set_matrix_confidence")
    m  <- example_metacells_daf()
    snap <- function(d) c(
        has_matrix(d, "gene",     "metacell", "confidence"),                    # default relayout=TRUE
        has_matrix(d, "gene",     "metacell", "confidence", relayout = FALSE),
        has_matrix(d, "metacell", "gene",     "confidence", relayout = FALSE)
    )
    expect_identical(snap(m), fx$stage0)
    set_matrix(m, "metacell", "gene", "confidence",
               matrix(stats::runif(7L * 683L), 7L, 683L), relayout = FALSE)
    expect_identical(snap(m), fx$stage1)
    set_matrix(m, "metacell", "gene", "confidence",
               matrix(stats::runif(7L * 683L), 7L, 683L),
               overwrite = TRUE, relayout = TRUE)
    expect_identical(snap(m), fx$stage2)
})

# writers.jl:1147 -- delete_matrix! with relayout/must_exist (3-stage sequence)
test_that("W jl:1147 delete_matrix relayout=FALSE then must_exist=FALSE", {
    fx <- .fx("writers_delete_matrix_UMIs")
    d  <- example_cells_daf()
    snap <- function(daf) c(
        has_matrix(daf, "gene", "cell", "UMIs"),                                 # default relayout=TRUE
        has_matrix(daf, "gene", "cell", "UMIs", relayout = FALSE),
        has_matrix(daf, "cell", "gene", "UMIs", relayout = FALSE)
    )
    expect_identical(snap(d), fx$stage0)
    delete_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)
    expect_identical(snap(d), fx$stage1)
    delete_matrix(d, "gene", "cell", "UMIs", must_exist = FALSE)
    expect_identical(snap(d), fx$stage2)
})
