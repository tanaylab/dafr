# Parity tests against the canonical jldoctest examples in
# DataAxesFormats.jl readers.jl + example_data.jl. Each test_that asserts the
# value, shape, or names that the Julia docstring documents (captured into
# tests/testthat/fixtures/julia-readers-writers/<name>.json by
# tools/julia-fixtures/capture_readers_writers.jl).
#
# Where R semantics diverge from Julia (version-counter bump policy, no `relayout`
# flag on certain getters), the test pins R behaviour and the comment notes the
# divergence so future readers can find it.

.fx_dir <- function() "fixtures/julia-readers-writers"
.fx <- function(name) {
    jsonlite::fromJSON(file.path(.fx_dir(), paste0(name, ".json")),
                       simplifyVector = TRUE)
}

# example_data.jl:22  -- print(description(example_cells_daf()))
test_that("example_data jl:22  cells description structure", {
    fx <- .fx("example_data_cells_description")
    d  <- example_cells_daf()
    expect_equal(get_scalar(d, "organism"), fx$structure$scalars$organism)
    expect_equal(get_scalar(d, "reference"), fx$structure$scalars$reference)
    expect_equal(
        sort(axes_set(d)),
        sort(names(fx$structure$axes))
    )
    for (a in names(fx$structure$axes)) {
        expect_equal(axis_length(d, a), fx$structure$axes[[a]],
                     info = paste("axis", a))
    }
    for (a in names(fx$structure$vectors)) {
        expect_equal(sort(vectors_set(d, a)), sort(fx$structure$vectors[[a]]),
                     info = paste("vectors on axis", a))
    }
})

# example_data.jl:62  -- print(description(example_metacells_daf()))
test_that("example_data jl:62  metacells description structure", {
    fx <- .fx("example_data_metacells_description")
    d  <- example_metacells_daf()
    expect_equal(sort(axes_set(d)), sort(names(fx$structure$axes)))
    for (a in names(fx$structure$axes)) {
        expect_equal(axis_length(d, a), fx$structure$axes[[a]])
    }
    for (a in names(fx$structure$vectors)) {
        expect_equal(sort(vectors_set(d, a)), sort(fx$structure$vectors[[a]]))
    }
})

# example_data.jl:205 -- print(description(example_chain_daf()))
test_that("example_data jl:205  chain description structure", {
    fx <- .fx("example_data_chain_description")
    d  <- example_chain_daf()
    expect_equal(get_scalar(d, "organism"), fx$structure$scalars$organism)
    expect_equal(get_scalar(d, "reference"), fx$structure$scalars$reference)
    expect_equal(sort(axes_set(d)), sort(names(fx$structure$axes)))
})

# readers.jl:92  -- has_scalar(example_cells_daf(), "organism")
test_that("R jl:92  has_scalar(cells, organism)", {
    fx <- .fx("readers_has_scalar_cells_organism")
    expect_identical(has_scalar(example_cells_daf(), "organism"), fx$value)
})

# readers.jl:100 -- has_scalar(example_metacells_daf(), "organism")
test_that("R jl:100  has_scalar(metacells, organism)", {
    fx <- .fx("readers_has_scalar_metacells_organism")
    expect_identical(has_scalar(example_metacells_daf(), "organism"), fx$value)
})

# readers.jl:125 -- scalars_set(example_cells_daf())
test_that("R jl:125  scalars_set(cells)", {
    fx <- .fx("readers_scalars_set_cells")
    expect_setequal(scalars_set(example_cells_daf()), fx$values)
})

# readers.jl:157 -- get_scalar(example_cells_daf(), "organism")
test_that("R jl:157  get_scalar(cells, organism)", {
    fx <- .fx("readers_get_scalar_cells_organism")
    expect_equal(get_scalar(example_cells_daf(), "organism"), fx$value)
})

# readers.jl:165 -- get_scalar(metacells, "organism"; default = nothing)
test_that("R jl:165  get_scalar(metacells, organism, default=NULL) is NULL", {
    expect_null(get_scalar(example_metacells_daf(), "organism", default = NULL))
})

# readers.jl:210 -- has_axis(example_cells_daf(), "metacell")
test_that("R jl:210  has_axis(cells, metacell)", {
    fx <- .fx("readers_has_axis_cells_metacell")
    expect_identical(has_axis(example_cells_daf(), "metacell"), fx$value)
})

# readers.jl:218 -- has_axis(example_metacells_daf(), "metacell")
test_that("R jl:218  has_axis(metacells, metacell)", {
    fx <- .fx("readers_has_axis_metacells_metacell")
    expect_identical(has_axis(example_metacells_daf(), "metacell"), fx$value)
})

# readers.jl:246 -- axis_version_counter(metacells, "type") sequence
test_that("R jl:246  axis_version_counter delete+add (Julia parity: 0 -> 1)", {
    fx <- .fx("readers_axis_version_counter_type")
    m  <- example_metacells_daf()
    expect_identical(axis_version_counter(m, "type"), as.integer(fx$before))
    delete_axis(m, "type")
    add_axis(m, "type", c("Foo", "Bar", "Baz"))
    expect_identical(axis_version_counter(m, "type"), as.integer(fx$after))
})

# readers.jl:273 -- axes_set(example_cells_daf())
test_that("R jl:273  axes_set(cells)", {
    fx <- .fx("readers_axes_set_cells")
    expect_setequal(axes_set(example_cells_daf()), fx$values)
})

# readers.jl:308 -- axis_vector(example_metacells_daf(), "type")
test_that("R jl:308  axis_vector(metacells, type)", {
    fx <- .fx("readers_axis_vector_metacells_type")
    expect_equal(axis_vector(example_metacells_daf(), "type"), fx$values)
})

# readers.jl:353 -- axis_dict(example_metacells_daf(), "type")
test_that("R jl:353  axis_dict(metacells, type)", {
    fx <- .fx("readers_axis_dict_metacells_type")
    d <- axis_dict(example_metacells_daf(), "type")
    for (i in seq_along(fx$keys)) {
        expect_identical(d[[fx$keys[[i]]]], as.integer(fx$values[[i]]),
                         info = paste("key", fx$keys[[i]]))
    }
})

# readers.jl:389 -- axis_indices(metacells, "type", ["MPP",""]; allow_empty=true)
test_that("R jl:389  axis_indices allow_empty=TRUE", {
    fx <- .fx("readers_axis_indices_allow_empty")
    out <- axis_indices(example_metacells_daf(), "type",
                        c("MPP", ""), allow_empty = TRUE)
    expect_identical(as.integer(out), as.integer(fx$values))
})

# readers.jl:448 -- axis_entries(metacells, "type", [3,0]; allow_empty=true)
test_that("R jl:448  axis_entries allow_empty=TRUE", {
    fx <- .fx("readers_axis_entries_allow_empty")
    out <- axis_entries(example_metacells_daf(), "type",
                        indices = c(3L, 0L), allow_empty = TRUE)
    expect_equal(out, fx$values)
})

# readers.jl:487 -- axis_length(example_metacells_daf(), "type")
test_that("R jl:487  axis_length(metacells, type)", {
    fx <- .fx("readers_axis_length_metacells_type")
    expect_identical(axis_length(example_metacells_daf(), "type"),
                     as.integer(fx$value))
})

# readers.jl:525 -- has_vector(example_cells_daf(), "cell", "type")
test_that("R jl:525  has_vector(cells, cell, type)", {
    fx <- .fx("readers_has_vector_cells_cell_type")
    expect_identical(has_vector(example_cells_daf(), "cell", "type"), fx$value)
})

# readers.jl:533 -- has_vector(example_metacells_daf(), "metacell", "type")
test_that("R jl:533  has_vector(metacells, metacell, type)", {
    fx <- .fx("readers_has_vector_metacells_metacell_type")
    expect_identical(has_vector(example_metacells_daf(), "metacell", "type"),
                     fx$value)
})

# readers.jl:566 -- vector_version_counter sequence (1 -> 2 after overwrite)
test_that("R jl:566  vector_version_counter set with overwrite", {
    fx <- .fx("readers_vector_version_counter_color")
    m  <- example_metacells_daf()
    expect_identical(vector_version_counter(m, "type", "color"),
                     as.integer(fx$before))
    set_vector(m, "type", "color", as.character(1:4), overwrite = TRUE)
    expect_identical(vector_version_counter(m, "type", "color"),
                     as.integer(fx$after))
})

# readers.jl:595 -- vectors_set(example_cells_daf(), "cell")
test_that("R jl:595  vectors_set(cells, cell)", {
    fx <- .fx("readers_vectors_set_cells_cell")
    expect_setequal(vectors_set(example_cells_daf(), "cell"), fx$values)
})

# readers.jl:633 -- get_vector(example_metacells_daf(), "type", "color")
test_that("R jl:633  get_vector(metacells, type, color) names+values", {
    fx <- .fx("readers_get_vector_metacells_type_color")
    v  <- get_vector(example_metacells_daf(), "type", "color")
    expect_equal(names(v),  fx$names)
    expect_equal(unname(v), fx$values)
})

# readers.jl:748 -- has_matrix(cells, "gene", "cell", "UMIs")
test_that("R jl:748  has_matrix(cells, gene, cell, UMIs)", {
    fx <- .fx("readers_has_matrix_cells_gene_cell_UMIs")
    expect_identical(has_matrix(example_cells_daf(), "gene", "cell", "UMIs"),
                     fx$value)
})

# readers.jl:801 -- matrices_set(example_cells_daf(), "gene", "cell")
test_that("R jl:801  matrices_set(cells, gene, cell)", {
    fx <- .fx("readers_matrices_set_cells_gene_cell")
    expect_setequal(matrices_set(example_cells_daf(), "gene", "cell"),
                    fx$values)
})

# readers.jl:933 -- get_matrix(metacells, "gene", "metacell", "fraction")
test_that("R jl:933  get_matrix(metacells, gene, metacell, fraction)", {
    fx <- .fx("readers_get_matrix_metacells_gene_metacell_fraction")
    m  <- get_matrix(example_metacells_daf(), "gene", "metacell", "fraction")
    expect_equal(nrow(m), fx$rows)
    expect_equal(ncol(m), fx$cols)
    expect_equal(head(rownames(m), 8L), head(fx$row_names, 8L))
    expect_equal(colnames(m),         fx$col_names)
    head_want <- matrix(fx$head, nrow = 8L, ncol = 7L)
    head_got  <- as.matrix(m)[1:8, 1:7]
    expect_equal(head_got, head_want, ignore_attr = TRUE, tolerance = 1e-6)
    expect_equal(sum(as.matrix(m)), fx$checksum, tolerance = 1e-4)
})

# readers.jl:1088 -- matrix_version_counter sequence (1 -> 2 after overwrite)
test_that("R jl:1088 matrix_version_counter set with overwrite", {
    fx <- .fx("readers_matrix_version_counter_fraction")
    m  <- example_metacells_daf()
    expect_identical(matrix_version_counter(m, "gene", "metacell", "fraction"),
                     as.integer(fx$before))
    set_matrix(m, "gene", "metacell", "fraction",
               matrix(stats::runif(683L * 7L), 683L, 7L), overwrite = TRUE)
    expect_identical(matrix_version_counter(m, "gene", "metacell", "fraction"),
                     as.integer(fx$after))
})

# readers.jl:1170 -- description(example_chain_daf(); deep = true)
test_that("R jl:1170 description(chain) structural parity", {
    fx <- .fx("readers_description_chain_deep")
    d  <- example_chain_daf()
    expect_equal(sort(axes_set(d)), sort(names(fx$structure$axes)))
    for (a in names(fx$structure$axes)) {
        expect_equal(axis_length(d, a), fx$structure$axes[[a]])
    }
})
