# Helper functions to create different DAF formats

make_memory_daf <- function() {
    daf <- memory_daf(name = "test!")
    return(list(daf = daf, extra = ""))
}

make_files_daf <- function() {
    tmpdir <- tempdir()
    daf <- files_daf(tmpdir, "w", name = "test!")
    extra <- paste0("path: ", normalizePath(tmpdir), "\nmode: w\n")
    return(list(daf = daf, extra = extra))
}

make_h5df <- function() {
    tmpdir <- tempdir()
    tmpdir <- gsub("//", "/", tempdir()) # A hack until the Julia version is updated
    h5_path <- file.path(tmpdir, "test.h5df")
    daf <- h5df(h5_path, "w", name = "test!")
    extra <- paste0("root: HDF5.File: (read-write) ", h5_path, "\nmode: w\n")
    return(list(daf = daf, extra = extra))
}

formats <- list(
    list(name = "MemoryDaf", create_fn = make_memory_daf),
    list(name = "FilesDaf", create_fn = make_files_daf),
    list(name = "H5df", create_fn = make_h5df)
)

# Test scalars for different formats

test_that("scalar operations work for different formats", {
    for (format_data in formats) {
        format_name <- format_data$name
        create_empty <- format_data$create_fn

        # Test with string scalar
        scalar_value <- "1.0.1"
        julia_type <- "String"

        result <- create_empty()
        daf <- result$daf
        extra <- result$extra

        expect_equal(name(daf), "test!")

        expect_equal(length(scalars_set(daf)), 0)
        expect_false(has_scalar(daf, "foo"))

        set_scalar(daf, "foo", scalar_value)
        expect_error(set_scalar(daf, "foo", NA))

        expect_true(has_scalar(daf, "foo"))
        expect_equal(get_scalar(daf, "foo"), scalar_value)
        expect_equal(scalars_set(daf), "foo")

        expect_error(get_scalar(daf, "savta"))
        expect_equal(get_scalar(daf, "savta", 17), 17)

        if (julia_type == "String") {
            formatted_value <- paste0('"', scalar_value, '"')
        } else {
            formatted_value <- scalar_value
        }

        expected_description <- paste0(
            "name: test!\n",
            "type: ", format_name, "\n",
            extra,
            "scalars:\n",
            "  foo: ", formatted_value, "\n"
        )

        expect_equal(description(daf), expected_description)

        delete_scalar(daf, "foo")
        expect_equal(length(scalars_set(daf)), 0)
        expect_false(has_scalar(daf, "foo"))

        # Test with numeric scalar
        scalar_value <- 0.5
        julia_type <- "Float64"

        set_scalar(daf, "foo", scalar_value)
        expect_true(has_scalar(daf, "foo"))
        expect_equal(get_scalar(daf, "foo"), scalar_value)

        delete_scalar(daf, "foo")
        expect_equal(length(scalars_set(daf)), 0)
        expect_false(has_scalar(daf, "foo"))

        # Test with integer scalar
        scalar_value <- 1L
        julia_type <- "Int64"

        set_scalar(daf, "foo", scalar_value)
        expect_true(has_scalar(daf, "foo"))
        expect_equal(get_scalar(daf, "foo"), scalar_value)

        delete_scalar(daf, "foo")
        expect_equal(length(scalars_set(daf)), 0)
        expect_false(has_scalar(daf, "foo"))
    }
})

test_that("validate_daf_object works", {
    daf <- memory_daf()
    expect_error(validate_daf_object(list()))
    expect_error(validate_daf_object(NULL))
    expect_error(validate_daf_object("not a daf"))
})

# Test axes for different formats
test_that("axes operations work for different formats", {
    for (format_data in formats) {
        format_name <- format_data$name
        create_empty <- format_data$create_fn

        # Test with character vector axis
        axis_name <- "cell"
        axis_entries <- c("A", "B")

        result <- create_empty()
        daf <- result$daf
        extra <- result$extra

        expect_equal(length(axes_set(daf)), 0)
        expect_false(has_axis(daf, axis_name))

        add_axis(daf, axis_name, axis_entries)

        expect_true(has_axis(daf, axis_name))
        expect_equal(axis_length(daf, axis_name), 2)
        expect_equal(axis_vector(daf, axis_name), axis_entries)

        expect_error(axis_vector(daf, "savta"))
        expect_null(axis_vector(daf, "savta", null_if_missing = TRUE))

        # Test axis_entries function
        expect_equal(axis_entries(daf, axis_name), axis_entries)
        expect_equal(axis_entries(daf, axis_name, 1), "A")
        expect_error(axis_entries(daf, axis_name, "A"))
        expect_error(axis_entries(daf, axis_name, 5))
        expect_error(axis_entries(daf, axis_name, -2))

        # Test with indices
        expect_equal(axis_indices(daf, axis_name, c("A", "B")), c(1, 2))
        expect_equal(axis_indices(daf, axis_name, "A"), 1)

        expect_error(axis_indices(daf, axis_name, c(1, 2)))

        # Test axis_dict
        dict <- axis_dict(daf, axis_name)
        expect_equal(dict[["A"]], 1)
        expect_equal(dict[["B"]], 2)

        expect_equal(axes_set(daf), axis_name)

        expected_description <- paste0(
            "name: test!\n",
            "type: ", format_name, "\n",
            extra,
            "axes:\n",
            "  cell: 2 entries\n"
        )

        expect_equal(description(daf), expected_description)

        delete_axis(daf, axis_name)

        expect_equal(length(axes_set(daf)), 0)
        expect_false(has_axis(daf, axis_name))
    }
})

# Test vectors for different formats
test_that("vector operations work for different formats", {
    for (format_data in formats) {
        format_name <- format_data$name
        create_empty <- format_data$create_fn

        result <- create_empty()
        daf <- result$daf
        extra <- result$extra

        # Add an axis
        add_axis(daf, "cell", c("A", "B"))

        # Test getting a missing vector
        expect_error(get_vector(daf, "cell", "foo"))

        expect_equal(get_vector(daf, "cell", "foo", default = NA), c(A = NA, B = NA))

        # Test with string default value
        expect_equal(get_vector(daf, "cell", "foo", default = "savta"), c(A = "savta", B = "savta"))

        # Test with default values
        expect_equal(get_vector(daf, "cell", "foo", default = 1), c(A = 1, B = 1))
        expect_equal(get_vector(daf, "cell", "foo", default = c(1, 2)), c(A = 1, B = 2))

        # Test with string vector
        vector_entries <- c(A = "X", B = "Y")
        julia_type <- "String"

        expect_equal(length(vectors_set(daf, "cell")), 0)
        expect_false(has_vector(daf, "cell", "foo"))

        set_vector(daf, "cell", "foo", vector_entries)

        expect_true(has_vector(daf, "cell", "foo"))
        expect_equal(vectors_set(daf, "cell"), "foo")

        stored_vector <- get_vector(daf, "cell", "foo")
        expect_equal(stored_vector, vector_entries)

        expected_description <- paste0(
            "name: test!\n",
            "type: ", format_name, "\n",
            extra,
            "axes:\n",
            "  cell: 2 entries\n",
            "vectors:\n",
            "  cell:\n",
            "    foo: 2 x Str (Dense)\n"
        )

        expect_equal(description(daf), expected_description)

        delete_vector(daf, "cell", "foo")

        expect_equal(length(vectors_set(daf, "cell")), 0)
        expect_false(has_vector(daf, "cell", "foo"))

        # Test with numeric vector
        vector_entries <- c(A = 1.0, B = 2.0)

        set_vector(daf, "cell", "foo", vector_entries)
        expect_equal(get_vector(daf, "cell", "foo"), vector_entries)

        delete_vector(daf, "cell", "foo")
    }
})

# Test matrices for different formats
test_that("matrix operations work for different formats", {
    for (format_data in formats) {
        format_name <- format_data$name
        create_empty <- format_data$create_fn

        result <- create_empty()
        daf <- result$daf
        extra <- result$extra

        # Add axes
        add_axis(daf, "cell", c("A", "B"))
        add_axis(daf, "gene", c("X", "Y", "Z"))

        # Test getting a missing matrix
        expect_error(get_matrix(daf, "cell", "gene", "UMIs"))
        expect_equal(get_matrix(daf, "cell", "gene", "UMIs", default = NA), matrix(NA, nrow = 2, ncol = 3, dimnames = list(c("A", "B"), c("X", "Y", "Z"))))

        # Test with default values
        expected_default <- matrix(1, nrow = 2, ncol = 3)
        result_default <- get_matrix(daf, "cell", "gene", "UMIs", default = 1)
        expect_equal(result_default, expected_default, ignore_attr = TRUE)

        # Create a custom default matrix
        custom_default <- matrix(c(1, 3, 5, 2, 4, 6), nrow = 2, ncol = 3)
        result_custom <- get_matrix(daf, "cell", "gene", "UMIs", default = custom_default)
        expect_equal(result_custom, custom_default, ignore_attr = TRUE)
        expect_equal(rownames(result_custom), axis_entries(daf, "cell"))
        expect_equal(colnames(result_custom), axis_entries(daf, "gene"))

        # Test setting and getting a matrix
        test_matrix <- matrix(c(0, 3.5, 0, 2.5, 0, 6.5), nrow = 2, ncol = 3)

        expect_equal(length(matrices_set(daf, "cell", "gene")), 0)
        expect_false(has_matrix(daf, "cell", "gene", "UMIs"))

        set_matrix(daf, "cell", "gene", "UMIs", test_matrix, relayout = FALSE)
        test_matrix1 <- test_matrix
        test_matrix1[1, 1] <- NA
        expect_error(set_matrix(daf, "cell", "gene", "UMIs", test_matrix1, relayout = FALSE))

        expect_true(has_matrix(daf, "cell", "gene", "UMIs", relayout = FALSE))
        expect_false(has_matrix(daf, "gene", "cell", "UMIs", relayout = FALSE))
        expect_equal(matrices_set(daf, "cell", "gene"), "UMIs")

        # Get and check the matrix
        stored_matrix <- get_matrix(daf, "cell", "gene", "UMIs", relayout = FALSE)
        expect_equal(stored_matrix, test_matrix, ignore_attr = TRUE)
        expect_equal(rownames(stored_matrix), axis_entries(daf, "cell"))
        expect_equal(colnames(stored_matrix), axis_entries(daf, "gene"))

        expected_description <- paste0(
            "name: test!\n",
            "type: ", format_name, "\n",
            extra,
            "axes:\n",
            "  cell: 2 entries\n",
            "  gene: 3 entries\n",
            "matrices:\n",
            "  cell,gene:\n",
            "    UMIs: 2 x 3 x Float64 in Columns (Dense)\n"
        )

        expect_equal(description(daf), expected_description)

        # Test relayout
        expect_false(has_matrix(daf, "gene", "cell", "UMIs", relayout = FALSE))
        relayout_matrix(daf, "cell", "gene", "UMIs")
        expect_true(has_matrix(daf, "gene", "cell", "UMIs", relayout = FALSE))

        # Get matrix with relayout
        relayout_stored <- get_matrix(daf, "gene", "cell", "UMIs", relayout = FALSE)
        expect_equal(t(relayout_stored), test_matrix, ignore_attr = TRUE)
        expect_equal(rownames(relayout_stored), axis_entries(daf, "gene"))
        expect_equal(colnames(relayout_stored), axis_entries(daf, "cell"))

        # Delete the matrix
        delete_matrix(daf, "cell", "gene", "UMIs")

        expect_equal(length(matrices_set(daf, "cell", "gene")), 0)
        expect_false(has_matrix(daf, "cell", "gene", "UMIs"))
    }
})

# Test sparse matrices
test_that("sparse matrix operations work", {
    for (format_data in formats) {
        result <- format_data$create_fn()
        format_name <- format_data$name
        extra <- result$extra
        daf <- result$daf

        # Add axes
        add_axis(daf, "cell", c("A", "B"))
        add_axis(daf, "gene", c("X", "Y", "Z"))

        # Create a sparse matrix
        sparse_matrix <- Matrix::sparseMatrix(
            i = c(1, 1, 1, 2, 2),
            j = c(1, 2, 3, 1, 2),
            x = c(0, 1, 2, 3, 4),
            dims = c(2, 3)
        )
        rownames(sparse_matrix) <- c("A", "B")
        colnames(sparse_matrix) <- c("X", "Y", "Z")

        # Set and get sparse matrix
        set_matrix(daf, "cell", "gene", "UMIs", sparse_matrix)
        m <- get_matrix(daf, "cell", "gene", "UMIs")
        expect_equal(m, sparse_matrix)

        # Memory format drops explicit zeros on relayout, others keep them
        if (format_name == "MemoryDaf") {
            relayout_sparse <- "Sparse 4 (67%) [Int64]"
        } else {
            relayout_sparse <- "Sparse 5 (83%) [Int64]"
        }

        expected_description <- paste0(
            "name: test!\n",
            "type: ", format_name, "\n",
            extra,
            "axes:\n",
            "  cell: 2 entries\n",
            "  gene: 3 entries\n",
            "matrices:\n",
            "  cell,gene:\n",
            "    UMIs: 2 x 3 x Float64 in Columns (Sparse 5 (83%) [Int64])\n",
            "  gene,cell:\n",
            "    UMIs: 3 x 2 x Float64 in Columns (", relayout_sparse, ")\n"
        )

        expect_equal(description(daf), expected_description)

        delete_matrix(daf, "cell", "gene", "UMIs")

        # Test with different matrix types
        row_major_umis <- as(sparse_matrix, "RsparseMatrix")
        set_matrix(daf, "cell", "gene", "UMIs", row_major_umis, relayout = FALSE)
        stored_matrix <- get_matrix(daf, "cell", "gene", "UMIs", relayout = FALSE)
        expect_equal(as.matrix(stored_matrix), as.matrix(row_major_umis))
        expect_equal(rownames(stored_matrix), axis_entries(daf, "cell"))
        expect_equal(colnames(stored_matrix), axis_entries(daf, "gene"))

        # Test matrix overwrite
        m <- matrix(c(5, 5, 5, 5, 5, 5), nrow = 2, ncol = 3)
        set_matrix(daf, "cell", "gene", "UMIs", m, overwrite = TRUE)
        stored_matrix <- get_matrix(daf, "cell", "gene", "UMIs")
        expect_equal(stored_matrix, m, ignore_attr = TRUE)
        expect_equal(rownames(stored_matrix), axis_entries(daf, "cell"))
        expect_equal(colnames(stored_matrix), axis_entries(daf, "gene"))

        # Test matrix deletion
        delete_matrix(daf, "cell", "gene", "UMIs")
        expect_false(has_matrix(daf, "cell", "gene", "UMIs"))
    }
})

# Test matrix caching
test_that("matrix caching works", {
    for (format_data in formats) {
        result <- format_data$create_fn()
        daf <- result$daf

        # Add axes
        add_axis(daf, "cell", c("A", "B"))
        add_axis(daf, "gene", c("X", "Y", "Z"))

        # Create test matrix
        test_matrix <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
        set_matrix(daf, "cell", "gene", "UMIs", test_matrix)

        # Get matrix to populate cache
        get_matrix(daf, "cell", "gene", "UMIs")

        # Clear cache
        empty_cache(daf)

        # Should still work after cache is cleared
        result_matrix <- get_matrix(daf, "cell", "gene", "UMIs")
        expect_equal(result_matrix, test_matrix, ignore_attr = TRUE)
        expect_equal(rownames(result_matrix), axis_entries(daf, "cell"))
        expect_equal(colnames(result_matrix), axis_entries(daf, "gene"))

        # Test selective cache clearing
        empty_cache(daf, clear = "MappedData")
        empty_cache(daf, keep = "MemoryData")

        # Should still work after cache is cleared
        result_matrix <- get_matrix(daf, "cell", "gene", "UMIs")
        expect_equal(result_matrix, test_matrix, ignore_attr = TRUE)
        expect_equal(rownames(result_matrix), axis_entries(daf, "cell"))
        expect_equal(colnames(result_matrix), axis_entries(daf, "gene"))
    }
})

# Test chains
test_that("chain operations work", {
    # Create first daf object
    first <- memory_daf(name = "first!")
    set_scalar(first, "version", 1.0)
    first_read <- read_only(first)
    expect_equal(name(first_read), "first!")
    expect_equal(name(read_only(first_read)), "first!")

    # Create a renamed read-only
    renamed <- read_only(first, name = "renamed!")
    expect_equal(name(renamed), "renamed!")

    # Create second daf object
    second <- memory_daf(name = "second!")
    set_scalar(second, "version", 2.0)

    # Test chain reader
    read_chain <- chain_reader(list(first_read, second), name = "chain!")
    expect_equal(name(read_chain), "chain!")
    expect_equal(get_scalar(read_chain, "version"), 2.0)

    # Test chain writer
    write_chain <- chain_writer(list(first_read, second), name = "chain!")
    set_scalar(write_chain, "version", 3.0, overwrite = TRUE)
    expect_equal(get_scalar(second, "version"), 3.0)
})

# Test get_dataframe function
test_that("get_dataframe works with different data types", {
    for (format_data in formats) {
        result <- format_data$create_fn()
        daf <- result$daf

        # Add axes
        add_axis(daf, "cell", c("A", "B"))
        add_axis(daf, "gene", c("X", "Y", "Z"))

        # Add vectors for the cell axis
        set_vector(daf, "cell", "type", c("T-cell", "B-cell"))
        set_vector(daf, "cell", "count", c(100, 200))

        # Add a matrix
        test_matrix <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
        set_matrix(daf, "cell", "gene", "UMIs", test_matrix)

        # Test get_dataframe with basic column selection
        df <- get_dataframe(daf, "cell", c("type", "count"))
        expect_equal(dim(df), c(2, 2))
        expect_equal(df$type, c("T-cell", "B-cell"))
        expect_equal(df$count, c(100, 200))

        # Test get_dataframe with named list for complex queries
        df2 <- get_dataframe(daf, "cell", list(
            "cell_type" = "type",
            "cell_count" = "count"
        ))
        expect_equal(dim(df2), c(2, 2))
        expect_equal(df2$cell_type, c("T-cell", "B-cell"))
        expect_equal(df2$cell_count, c(100, 200))

        # Test get_dataframe with caching
        df3 <- get_dataframe(daf, "cell", c("type", "count"), cache = TRUE)
        expect_equal(dim(df3), c(2, 2))
        expect_equal(df3$type, c("T-cell", "B-cell"))
        expect_equal(df3$count, c(100, 200))

        # Test get_dataframe with a single column
        df4 <- get_dataframe(daf, "cell", "type")
        expect_equal(dim(df4), c(2, 1))
        expect_equal(df4$type, c("T-cell", "B-cell"))
    }
})

# Test is_daf function
test_that("is_daf works", {
    daf <- memory_daf()
    expect_true(is_daf(daf))
    expect_false(is_daf(list()))
    expect_false(is_daf(NULL))
    expect_false(is_daf("not a daf"))
})

# Test print.Daf function
test_that("print.Daf works", {
    daf <- memory_daf(name = "test_print!")
    expect_output(print(daf), "name: test_print!")
    expect_output(print(daf), "type: MemoryDaf")
})
