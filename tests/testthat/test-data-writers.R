# Tests for new data writer functions: get_empty_dense_vector, get_empty_sparse_vector,
# get_empty_dense_matrix, get_empty_sparse_matrix, filled_empty_sparse_vector,
# filled_empty_sparse_matrix, and get_frame (now exported)

# --- get_empty_dense_vector ---

test_that("get_empty_dense_vector creates a vector for an axis", {
    daf <- memory_daf(name = "empty_dense_vec_test!")
    add_axis(daf, "cell", c("A", "B", "C"))

    # Get an empty dense vector of Float64
    vec <- get_empty_dense_vector(daf, "cell", "scores", "Float64")
    expect_true(inherits(vec, "JuliaObject"))

    # After getting the empty vector, it should exist in the daf
    expect_true(has_vector(daf, "cell", "scores"))
})

test_that("get_empty_dense_vector with Int32 type works", {
    daf <- memory_daf(name = "empty_dense_int_test!")
    add_axis(daf, "gene", c("X", "Y"))

    vec <- get_empty_dense_vector(daf, "gene", "counts", "Int32")
    expect_true(inherits(vec, "JuliaObject"))
    expect_true(has_vector(daf, "gene", "counts"))
})

test_that("get_empty_dense_vector with overwrite parameter works", {
    daf <- memory_daf(name = "empty_dense_overwrite_test!")
    add_axis(daf, "cell", c("A", "B"))

    # Create first vector
    vec1 <- get_empty_dense_vector(daf, "cell", "data", "Float64")
    expect_true(has_vector(daf, "cell", "data"))

    # Overwrite should succeed
    vec2 <- get_empty_dense_vector(daf, "cell", "data", "Float64", overwrite = TRUE)
    expect_true(inherits(vec2, "JuliaObject"))

    # Without overwrite should fail
    expect_error(
        get_empty_dense_vector(daf, "cell", "data", "Float64", overwrite = FALSE)
    )
})

# --- get_empty_sparse_vector ---

test_that("get_empty_sparse_vector creates a sparse vector", {
    daf <- memory_daf(name = "empty_sparse_vec_test!")
    add_axis(daf, "cell", c("A", "B", "C", "D", "E"))

    # Get an empty sparse vector with 2 non-zero elements
    vec <- get_empty_sparse_vector(daf, "cell", "markers", "Float64", nnz = 2)
    expect_true(inherits(vec, "JuliaObject"))
})

test_that("get_empty_sparse_vector with indtype parameter works", {
    daf <- memory_daf(name = "empty_sparse_indtype_test!")
    add_axis(daf, "gene", c("X", "Y", "Z"))

    vec <- get_empty_sparse_vector(daf, "gene", "flags", "Float64", nnz = 1, indtype = "Int32")
    expect_true(inherits(vec, "JuliaObject"))
})

test_that("get_empty_sparse_vector with overwrite parameter works", {
    daf <- memory_daf(name = "empty_sparse_overwrite_test!")
    add_axis(daf, "cell", c("A", "B", "C"))

    # Create and finalize a sparse vector first
    vec1 <- get_empty_sparse_vector(daf, "cell", "data", "Float64", nnz = 1)
    filled_empty_sparse_vector(daf, "cell", "data",
        nzind = c(1L, 0L),
        nzval = c(1.0, 0.0)
    )
    expect_true(has_vector(daf, "cell", "data"))

    # Overwrite should succeed
    vec2 <- get_empty_sparse_vector(daf, "cell", "data", "Float64", nnz = 2, overwrite = TRUE)
    expect_true(inherits(vec2, "JuliaObject"))

    # Without overwrite should fail since vector already exists (from the overwrite call)
    filled_empty_sparse_vector(daf, "cell", "data",
        nzind = c(1L, 2L),
        nzval = c(1.0, 2.0)
    )
    expect_error(
        get_empty_sparse_vector(daf, "cell", "data", "Float64", nnz = 1, overwrite = FALSE)
    )
})

# --- filled_empty_sparse_vector ---

test_that("filled_empty_sparse_vector stores data correctly", {
    daf <- memory_daf(name = "filled_sparse_vec_test!")
    add_axis(daf, "cell", c("A", "B", "C", "D", "E"))

    # Get an empty sparse vector with 2 non-zero elements
    vec <- get_empty_sparse_vector(daf, "cell", "scores", "Float64", nnz = 2)

    # Fill it with data: indices 2 and 4, values 10.0 and 20.0
    filled_empty_sparse_vector(daf, "cell", "scores",
        nzind = c(2L, 4L),
        nzval = c(10.0, 20.0)
    )

    # Retrieve and verify the vector
    result <- get_vector(daf, "cell", "scores")
    expect_equal(result[["A"]], 0)
    expect_equal(result[["B"]], 10.0)
    expect_equal(result[["C"]], 0)
    expect_equal(result[["D"]], 20.0)
    expect_equal(result[["E"]], 0)
})

test_that("filled_empty_sparse_vector with single non-zero element", {
    daf <- memory_daf(name = "filled_sparse_single_test!")
    add_axis(daf, "gene", c("X", "Y", "Z"))

    vec <- get_empty_sparse_vector(daf, "gene", "marker", "Float64", nnz = 1)
    filled_empty_sparse_vector(daf, "gene", "marker",
        nzind = c(2L),
        nzval = c(1.0)
    )

    result <- get_vector(daf, "gene", "marker")
    expect_equal(result[["X"]], 0)
    expect_equal(result[["Y"]], 1.0)
    expect_equal(result[["Z"]], 0)
})

# --- get_empty_dense_matrix ---

test_that("get_empty_dense_matrix creates a matrix for axes", {
    daf <- memory_daf(name = "empty_dense_mat_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    mat <- get_empty_dense_matrix(daf, "cell", "gene", "expression", "Float64")
    expect_true(inherits(mat, "JuliaObject"))
    expect_true(has_matrix(daf, "cell", "gene", "expression"))
})

test_that("get_empty_dense_matrix with Int32 type works", {
    daf <- memory_daf(name = "empty_dense_mat_int_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y"))

    mat <- get_empty_dense_matrix(daf, "cell", "gene", "counts", "Int32")
    expect_true(inherits(mat, "JuliaObject"))
    expect_true(has_matrix(daf, "cell", "gene", "counts"))
})

test_that("get_empty_dense_matrix with overwrite parameter works", {
    daf <- memory_daf(name = "empty_dense_mat_overwrite_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y"))

    mat1 <- get_empty_dense_matrix(daf, "cell", "gene", "data", "Float64")

    # Overwrite should succeed
    mat2 <- get_empty_dense_matrix(daf, "cell", "gene", "data", "Float64", overwrite = TRUE)
    expect_true(inherits(mat2, "JuliaObject"))

    # Without overwrite should fail
    expect_error(
        get_empty_dense_matrix(daf, "cell", "gene", "data", "Float64", overwrite = FALSE)
    )
})

# --- get_empty_sparse_matrix ---

test_that("get_empty_sparse_matrix creates a sparse matrix", {
    daf <- memory_daf(name = "empty_sparse_mat_test!")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y"))

    mat <- get_empty_sparse_matrix(daf, "cell", "gene", "umis", "Float64", nnz = 3)
    expect_true(inherits(mat, "JuliaObject"))
})

test_that("get_empty_sparse_matrix with indtype parameter works", {
    daf <- memory_daf(name = "empty_sparse_mat_indtype_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    mat <- get_empty_sparse_matrix(daf, "cell", "gene", "counts", "Float64", nnz = 2, indtype = "Int32")
    expect_true(inherits(mat, "JuliaObject"))
})

test_that("get_empty_sparse_matrix with overwrite parameter works", {
    daf <- memory_daf(name = "empty_sparse_mat_overwrite_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y"))

    # Create and finalize a sparse matrix first so it exists in the daf
    mat1 <- get_empty_sparse_matrix(daf, "cell", "gene", "data", "Float64", nnz = 2)
    filled_empty_sparse_matrix(daf, "cell", "gene", "data",
        colptr = c(1L, 2L, 3L),
        rowval = c(1L, 2L),
        nzval = c(1.0, 2.0)
    )
    expect_true(has_matrix(daf, "cell", "gene", "data"))

    # Overwrite should succeed
    mat2 <- get_empty_sparse_matrix(daf, "cell", "gene", "data", "Float64", nnz = 3, overwrite = TRUE)
    expect_true(inherits(mat2, "JuliaObject"))

    # Finalize the overwritten matrix
    filled_empty_sparse_matrix(daf, "cell", "gene", "data",
        colptr = c(1L, 2L, 4L),
        rowval = c(1L, 1L, 2L),
        nzval = c(1.0, 2.0, 3.0)
    )

    # Without overwrite should fail since matrix now exists
    expect_error(
        get_empty_sparse_matrix(daf, "cell", "gene", "data", "Float64", nnz = 1, overwrite = FALSE)
    )
})

# --- filled_empty_sparse_matrix ---

test_that("filled_empty_sparse_matrix stores data correctly", {
    daf <- memory_daf(name = "filled_sparse_mat_test!")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y"))

    # Get an empty sparse matrix with 3 non-zero elements
    # CSC format: 3 rows (cells) x 2 columns (genes)
    mat <- get_empty_sparse_matrix(daf, "cell", "gene", "umis", "Float64", nnz = 3)

    # Fill with CSC format data:
    # Column 1 (gene X): cell A=1.0, cell C=3.0
    # Column 2 (gene Y): cell B=5.0
    # colptr: [1, 3, 4] meaning col1 has entries at positions 1-2, col2 at position 3
    # rowval: [1, 3, 2] meaning row indices of non-zero entries
    # nzval: [1.0, 3.0, 5.0] the actual values
    filled_empty_sparse_matrix(daf, "cell", "gene", "umis",
        colptr = c(1L, 3L, 4L),
        rowval = c(1L, 3L, 2L),
        nzval = c(1.0, 3.0, 5.0)
    )

    # Retrieve and verify the matrix
    result <- get_matrix(daf, "cell", "gene", "umis")
    expect_equal(as.matrix(result), matrix(c(1, 0, 3, 0, 5, 0), nrow = 3, ncol = 2), ignore_attr = TRUE)
})

test_that("filled_empty_sparse_matrix with single non-zero element", {
    daf <- memory_daf(name = "filled_sparse_mat_single_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # 2 rows x 3 columns, 1 non-zero element
    mat <- get_empty_sparse_matrix(daf, "cell", "gene", "sparse_data", "Float64", nnz = 1)

    # Put a single value at cell A, gene Y (row 1, col 2)
    filled_empty_sparse_matrix(daf, "cell", "gene", "sparse_data",
        colptr = c(1L, 1L, 2L, 2L),
        rowval = c(1L),
        nzval = c(42.0)
    )

    result <- get_matrix(daf, "cell", "gene", "sparse_data")
    expected <- matrix(c(0, 0, 42, 0, 0, 0), nrow = 2, ncol = 3)
    expect_equal(as.matrix(result), expected, ignore_attr = TRUE)
})

# --- get_frame (now exported) ---

test_that("get_frame returns Julia-style result", {
    daf <- memory_daf(name = "frame_test!")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1.0, 2.0, 3.0))
    set_vector(daf, "cell", "type", c("T", "B", "NK"))

    # get_frame with NULL columns returns all columns including 'name'
    result <- get_frame(daf, "cell")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    # The result should have a 'name' column
    expect_true("name" %in% names(result))
    expect_true("age" %in% names(result))
    expect_true("type" %in% names(result))
})

test_that("get_frame with specific column names works", {
    daf <- memory_daf(name = "frame_col_test!")
    add_axis(daf, "cell", c("A", "B"))
    set_vector(daf, "cell", "age", c(10.0, 20.0))
    set_vector(daf, "cell", "type", c("T", "B"))

    # Get frame with single column
    result <- get_frame(daf, "cell", "age")
    expect_true(is.data.frame(result))
    expect_true("age" %in% names(result))
})

test_that("get_frame with named list columns works", {
    daf <- memory_daf(name = "frame_named_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "batch", c("B1", "B2"))
    set_vector(daf, "cell", "batch", c("B1", "B2"))
    set_vector(daf, "cell", "age", c(1.0, 2.0))
    set_vector(daf, "batch", "sex", c("Male", "Female"))

    # Get frame with named list (complex queries)
    result <- get_frame(daf, "cell", list(age = ": age", sex = ": batch => sex"))
    expect_true(is.data.frame(result))
    expect_true("age" %in% names(result))
    expect_true("sex" %in% names(result))
})

test_that("get_frame with cache parameter works", {
    daf <- memory_daf(name = "frame_cache_test!")
    add_axis(daf, "cell", c("A", "B"))
    set_vector(daf, "cell", "val", c(1.0, 2.0))

    # Test with cache=TRUE and cache=FALSE produce equivalent results
    result_cached <- get_frame(daf, "cell", "val", cache = TRUE)
    result_uncached <- get_frame(daf, "cell", "val", cache = FALSE)
    expect_equal(result_cached, result_uncached)
})

test_that("get_frame vs get_dataframe difference", {
    daf <- memory_daf(name = "frame_vs_df_test!")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "score", c(10, 20, 30))

    # get_frame returns raw result with 'name' column
    frame_result <- get_frame(daf, "cell")
    expect_true("name" %in% names(frame_result))

    # get_dataframe removes 'name' column and adds row names
    df_result <- get_dataframe(daf, "cell")
    expect_false("name" %in% names(df_result))
    expect_equal(rownames(df_result), c("A", "B", "C"))
})
