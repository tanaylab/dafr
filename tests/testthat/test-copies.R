test_that("copy functions work correctly", {
    # Create source and destination Daf objects
    source <- memory_daf(name = "source!")
    destination <- memory_daf(name = "destination!")

    # Test copy_scalar
    set_scalar(source, "version", "1.0")
    copy_scalar(destination = destination, source = source, name = "version")
    expect_equal(get_scalar(destination, "version"), "1.0")

    # Test copy_axis
    add_axis(source, "cell", c("A", "B"))
    copy_axis(destination = destination, source = source, axis = "cell")
    expect_equal(as.character(axis_vector(destination, "cell")), c("A", "B"))

    add_axis(source, "gene", c("X", "Y", "Z"))
    copy_axis(destination = destination, source = source, axis = "gene")
    expect_equal(as.character(axis_vector(destination, "gene")), c("X", "Y", "Z"))

    # Test copy_vector
    set_vector(source, "cell", "age", c(0.0, 1.0))
    copy_vector(destination = destination, source = source, axis = "cell", name = "age")
    expect_equal(get_vector(destination, "cell", "age"), c(A = 0.0, B = 1.0))

    # Test copy_matrix
    umis <- matrix(c(0, 1, 2, 3, 4, 5), nrow = 2, ncol = 3, byrow = TRUE)
    rownames(umis) <- c("A", "B")
    colnames(umis) <- c("X", "Y", "Z")
    umis <- t(umis)
    set_matrix(source, "gene", "cell", "UMIs", umis)
    copy_matrix(
        destination = destination,
        source = source,
        rows_axis = "gene",
        columns_axis = "cell",
        name = "UMIs",
        relayout = FALSE
    )

    expect_equal(get_matrix(destination, "gene", "cell", "UMIs"), umis)

    # Test copy_tensor
    add_axis(destination, "batch", c("U", "V"))
    is_high <- matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), nrow = 2, ncol = 3, byrow = TRUE)
    rownames(is_high) <- c("A", "B")
    colnames(is_high) <- c("X", "Y", "Z")
    is_high <- t(is_high)

    set_matrix(source, "gene", "cell", "U_is_high", is_high)
    copy_tensor(
        destination = destination,
        source = source,
        main_axis = "batch",
        rows_axis = "gene",
        columns_axis = "cell",
        name = "is_high",
        empty = FALSE
    )

    expect_equal(get_matrix(destination, "gene", "cell", "U_is_high"), is_high)
    expect_equal(
        as.matrix(get_matrix(destination, "gene", "cell", "V_is_high")),
        matrix(FALSE, nrow = 3, ncol = 2),
        ignore_attr = TRUE
    )

    # Test copy_all
    new_destination <- memory_daf(name = "destination!")
    copy_all(destination = new_destination, source = source)
    expect_equal(get_scalar(new_destination, "version"), "1.0")
    expect_equal(as.character(axis_vector(new_destination, "cell")), c("A", "B"))
    expect_equal(as.character(axis_vector(new_destination, "gene")), c("X", "Y", "Z"))
    expect_equal(get_vector(new_destination, "cell", "age"), c(A = 0.0, B = 1.0))
    expect_equal(get_matrix(new_destination, "gene", "cell", "UMIs"), umis)
})

# test_that("copy_all with empty parameter works correctly", {
#     # Create source and target Daf objects with different axis entries
#     source <- memory_daf(name = "source!")
#     add_axis(source, "cell", c("A", "B"))
#     add_axis(source, "gene", c("X", "Y", "Z"))

#     # Set some vector and matrix data in source
#     set_vector(source, "cell", "score", c(10.5, 20.3))
#     umis <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
#     rownames(umis) <- c("X", "Y", "Z")
#     colnames(umis) <- c("A", "B")
#     set_matrix(source, "gene", "cell", "counts", umis)

#     # Create destination with extended axes
#     destination <- memory_daf(name = "destination!")
#     add_axis(destination, "cell", c("A", "B", "C", "D")) # Superset of source cells
#     add_axis(destination, "gene", c("W", "X", "Y", "Z")) # Different but overlapping genes

#     # Copy with empty values for missing data
#     empty_values <- list(
#         "cell,score" = -1.0, # Vector empty value
#         "gene,cell,counts" = 0 # Matrix empty value
#     )

#     copy_all(
#         destination = destination,
#         source = source,
#         empty = empty_values,
#         overwrite = FALSE
#     )

#     # Check vector data was copied with empty values for missing entries
#     cell_scores <- get_vector(destination, "cell", "score")
#     expect_equal(length(cell_scores), 4) # Should have values for all 4 cells
#     expect_equal(cell_scores["A"], 10.5) # Original values preserved
#     expect_equal(cell_scores["B"], 20.3)
#     expect_equal(cell_scores["C"], -1.0) # Empty value used for new cells
#     expect_equal(cell_scores["D"], -1.0)

#     # Check matrix data was copied with empty values for missing entries
#     counts_matrix <- get_matrix(destination, "gene", "cell", "counts")
#     expect_equal(dim(counts_matrix), c(4, 4)) # Should have all genes and cells

#     # Check original values are preserved
#     expect_equal(counts_matrix["X", "A"], 1)
#     expect_equal(counts_matrix["Y", "A"], 2)
#     expect_equal(counts_matrix["Z", "A"], 3)
#     expect_equal(counts_matrix["X", "B"], 4)
#     expect_equal(counts_matrix["Y", "B"], 5)
#     expect_equal(counts_matrix["Z", "B"], 6)

#     # Check empty values for new/missing combinations
#     expect_equal(counts_matrix["W", "A"], 0)
#     expect_equal(counts_matrix["W", "B"], 0)
#     expect_equal(counts_matrix["X", "C"], 0)
#     expect_equal(counts_matrix["Y", "D"], 0)
# })

# Tests for new copy parameters: type, eltype, bestify, insist

test_that("copy_scalar with default=NULL silently skips missing scalar in source", {
    source <- memory_daf(name = "source_insist!")
    destination <- memory_daf(name = "dest_insist!")

    # No scalar set in source; passing default=NULL maps to Julia nothing,
    # which means get_scalar returns nothing and copy is silently skipped
    expect_no_error(
        copy_scalar(
            destination = destination,
            source = source,
            name = "nonexistent",
            default = NULL
        )
    )

    # Verify the scalar was not created in the destination
    expect_false(has_scalar(destination, "nonexistent"))
})

test_that("copy_scalar errors on missing scalar when no default is provided", {
    source <- memory_daf(name = "source_insist2!")
    destination <- memory_daf(name = "dest_insist2!")

    # No scalar set in source; without a default, should error
    err <- tryCatch(
        {
            copy_scalar(
                destination = destination,
                source = source,
                name = "nonexistent"
            )
            NULL
        },
        error = function(e) e
    )
    expect_true(!is.null(err))
})

test_that("copy_scalar with insist=FALSE skips when destination already has scalar", {
    source <- memory_daf(name = "source_insist3!")
    destination <- memory_daf(name = "dest_insist3!")

    set_scalar(source, "version", "1.0")
    set_scalar(destination, "version", "2.0")

    # insist=FALSE + overwrite=FALSE: silently skip since destination already has it
    expect_no_error(
        copy_scalar(
            destination = destination,
            source = source,
            name = "version",
            insist = FALSE,
            overwrite = FALSE
        )
    )

    # Destination should still have original value
    expect_equal(get_scalar(destination, "version"), "2.0")
})

test_that("copy_scalar with type parameter converts types", {
    source <- memory_daf(name = "source_type!")
    destination <- memory_daf(name = "dest_type!")

    set_scalar(source, "count", as.integer(42))
    copy_scalar(
        destination = destination,
        source = source,
        name = "count",
        type = "Float64"
    )
    result <- get_scalar(destination, "count")
    expect_true(is.numeric(result))
    expect_equal(result, 42)
})

test_that("copy_vector with eltype parameter converts element types", {
    source <- memory_daf(name = "source_eltype!")
    destination <- memory_daf(name = "dest_eltype!")

    add_axis(source, "cell", c("A", "B", "C"))
    add_axis(destination, "cell", c("A", "B", "C"))

    # Set float vector in source with integer-representable values
    set_vector(source, "cell", "score", c(1.0, 2.0, 3.0))

    # Copy with eltype conversion to Int32
    copy_vector(
        destination = destination,
        source = source,
        axis = "cell",
        name = "score",
        eltype = "Int32"
    )

    result <- get_vector(destination, "cell", "score")
    expect_equal(result, c(A = 1, B = 2, C = 3))
})

test_that("copy_vector with default=NULL silently skips missing vector in source", {
    source <- memory_daf(name = "source_vec_insist!")
    destination <- memory_daf(name = "dest_vec_insist!")

    add_axis(source, "cell", c("A", "B"))
    add_axis(destination, "cell", c("A", "B"))

    # No vector set; default=NULL means missing source data is silently skipped
    expect_no_error(
        copy_vector(
            destination = destination,
            source = source,
            axis = "cell",
            name = "nonexistent",
            default = NULL
        )
    )

    expect_false(has_vector(destination, "cell", "nonexistent"))
})

test_that("copy_vector errors on missing vector when no default is provided", {
    source <- memory_daf(name = "source_vec_insist2!")
    destination <- memory_daf(name = "dest_vec_insist2!")

    add_axis(source, "cell", c("A", "B"))
    add_axis(destination, "cell", c("A", "B"))

    err <- tryCatch(
        {
            copy_vector(
                destination = destination,
                source = source,
                axis = "cell",
                name = "nonexistent"
            )
            NULL
        },
        error = function(e) e
    )
    expect_true(!is.null(err))
})

test_that("copy_vector with bestify parameter works", {
    source <- memory_daf(name = "source_bestify!")
    destination <- memory_daf(name = "dest_bestify!")

    add_axis(source, "cell", c("A", "B", "C"))
    add_axis(destination, "cell", c("A", "B", "C"))

    set_vector(source, "cell", "values", c(1.0, 2.0, 3.0))

    # Copy with bestify=TRUE
    expect_no_error(
        copy_vector(
            destination = destination,
            source = source,
            axis = "cell",
            name = "values",
            bestify = TRUE
        )
    )

    result <- get_vector(destination, "cell", "values")
    expect_equal(result, c(A = 1.0, B = 2.0, C = 3.0))
})

test_that("copy_matrix with eltype parameter converts element types", {
    source <- memory_daf(name = "source_mat_eltype!")
    destination <- memory_daf(name = "dest_mat_eltype!")

    add_axis(source, "cell", c("A", "B"))
    add_axis(source, "gene", c("X", "Y", "Z"))
    add_axis(destination, "cell", c("A", "B"))
    add_axis(destination, "gene", c("X", "Y", "Z"))

    # Use integer-representable Float64 values to avoid InexactError
    mat <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 2, ncol = 3)
    set_matrix(source, "cell", "gene", "expr", mat, relayout = FALSE)

    copy_matrix(
        destination = destination,
        source = source,
        rows_axis = "cell",
        columns_axis = "gene",
        name = "expr",
        eltype = "Int32",
        relayout = FALSE
    )

    result <- get_matrix(destination, "cell", "gene", "expr")
    expected <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
    expect_equal(as.matrix(result), expected, ignore_attr = TRUE)
})

test_that("copy_matrix with default=NULL silently skips missing matrix in source", {
    source <- memory_daf(name = "source_mat_insist!")
    destination <- memory_daf(name = "dest_mat_insist!")

    add_axis(source, "cell", c("A", "B"))
    add_axis(source, "gene", c("X", "Y"))
    add_axis(destination, "cell", c("A", "B"))
    add_axis(destination, "gene", c("X", "Y"))

    expect_no_error(
        copy_matrix(
            destination = destination,
            source = source,
            rows_axis = "cell",
            columns_axis = "gene",
            name = "nonexistent",
            relayout = FALSE,
            default = NULL
        )
    )

    expect_false(has_matrix(destination, "cell", "gene", "nonexistent"))
})

test_that("copy_matrix errors on missing matrix when no default is provided", {
    source <- memory_daf(name = "source_mat_insist2!")
    destination <- memory_daf(name = "dest_mat_insist2!")

    add_axis(source, "cell", c("A", "B"))
    add_axis(source, "gene", c("X", "Y"))
    add_axis(destination, "cell", c("A", "B"))
    add_axis(destination, "gene", c("X", "Y"))

    err <- tryCatch(
        {
            copy_matrix(
                destination = destination,
                source = source,
                rows_axis = "cell",
                columns_axis = "gene",
                name = "nonexistent",
                relayout = FALSE
            )
            NULL
        },
        error = function(e) e
    )
    expect_true(!is.null(err))
})

test_that("copy_matrix with bestify parameter works", {
    source <- memory_daf(name = "source_mat_bestify!")
    destination <- memory_daf(name = "dest_mat_bestify!")

    add_axis(source, "cell", c("A", "B"))
    add_axis(source, "gene", c("X", "Y", "Z"))
    add_axis(destination, "cell", c("A", "B"))
    add_axis(destination, "gene", c("X", "Y", "Z"))

    mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
    set_matrix(source, "cell", "gene", "expr", mat, relayout = FALSE)

    expect_no_error(
        copy_matrix(
            destination = destination,
            source = source,
            rows_axis = "cell",
            columns_axis = "gene",
            name = "expr",
            bestify = TRUE,
            relayout = FALSE
        )
    )

    result <- get_matrix(destination, "cell", "gene", "expr")
    expect_equal(as.matrix(result), mat, ignore_attr = TRUE)
})
