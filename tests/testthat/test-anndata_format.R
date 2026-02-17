test_that("h5ad conversion works", {
    # Create a memory daf
    origin <- memory_daf(name = "memory!")
    set_scalar(origin, "version", 1)
    add_axis(origin, "cell", c("A", "B"))
    add_axis(origin, "gene", c("X", "Y", "Z"))

    # Create a matrix - note R matrices are transposed compared to Julia/Python
    umis <- matrix(c(0, 1, 2, 3, 4, 5), nrow = 3, ncol = 2, byrow = TRUE)
    colnames(umis) <- c("A", "B")
    rownames(umis) <- c("X", "Y", "Z")

    set_matrix(origin, "gene", "cell", "UMIs", umis, relayout = FALSE)

    # Verify description contains expected content
    desc <- description(origin)
    expect_match(desc, "name: memory!")
    expect_match(desc, "scalars:\\s+version: 1")
    expect_match(desc, "axes:\\s+cell: 2 entries\\s+gene: 3 entries")
    expect_match(desc, "matrices:\\s+gene,cell:\\s+UMIs:")

    # Create temp directory for h5ad file
    h5ad_path <- tempfile(fileext = ".h5ad")

    # Convert to h5ad
    daf_as_h5ad(
        origin,
        obs_is = "cell",
        var_is = "gene",
        X_is = "UMIs",
        h5ad = h5ad_path
    )

    # Check that the file exists
    expect_true(file.exists(h5ad_path))

    # Convert back to daf
    back <- h5ad_as_daf(
        h5ad_path,
        obs_is = "cell",
        var_is = "gene",
        X_is = "UMIs",
        name = "anndata!"
    )

    # Verify the converted data
    expect_equal(name(back), "anndata!")
    expect_equal(get_scalar(back, "version"), 1)
    expect_equal(get_scalar(back, "obs_is"), "cell")
    expect_equal(get_scalar(back, "var_is"), "gene")
    expect_equal(get_scalar(back, "X_is"), "UMIs")

    # Check axes and entries
    expect_true(has_axis(back, "cell"))
    expect_true(has_axis(back, "gene"))
    expect_equal(sort(axis_vector(back, "cell")), c("A", "B"))
    expect_equal(sort(axis_vector(back, "gene")), c("X", "Y", "Z"))

    # Check that the matrix data is preserved
    original_matrix <- get_matrix(origin, "gene", "cell", "UMIs")
    converted_matrix <- get_matrix(back, "gene", "cell", "UMIs")
    expect_equal(original_matrix, converted_matrix)

    # Clean up
    unlink(h5ad_path)
})

test_that("h5ad_as_daf accepts different handler types", {
    # Create a simple h5ad file for testing
    origin <- memory_daf(name = "test_handlers!")
    add_axis(origin, "cell", c("A", "B"))
    add_axis(origin, "gene", c("X", "Y"))
    set_matrix(origin, "gene", "cell", "UMIs", matrix(1:4, nrow = 2, ncol = 2))

    h5ad_path <- tempfile(fileext = ".h5ad")
    daf_as_h5ad(
        origin,
        obs_is = "cell",
        var_is = "gene",
        X_is = "UMIs",
        h5ad = h5ad_path
    )

    # Test each handler type
    expect_no_error(
        h5ad_as_daf(h5ad_path, obs_is = "cell", var_is = "gene", X_is = "UMIs", unsupported_handler = IGNORE_HANDLER)
    )

    expect_no_error(
        h5ad_as_daf(h5ad_path, obs_is = "cell", var_is = "gene", X_is = "UMIs", unsupported_handler = WARN_HANDLER)
    )

    expect_no_error(
        h5ad_as_daf(h5ad_path, obs_is = "cell", var_is = "gene", X_is = "UMIs", unsupported_handler = ERROR_HANDLER)
    )

    # Test invalid handler
    expect_error(
        h5ad_as_daf(h5ad_path, obs_is = "cell", var_is = "gene", X_is = "UMIs", unsupported_handler = "InvalidHandler"),
        "Handler must be one of:"
    )

    # Clean up
    unlink(h5ad_path)
})

test_that("daf_as_h5ad with X_eltype parameter works", {
    # Create a memory daf with integer matrix
    origin <- memory_daf(name = "eltype_test!")
    add_axis(origin, "cell", c("A", "B"))
    add_axis(origin, "gene", c("X", "Y", "Z"))

    # Create integer matrix data
    umis <- matrix(c(0, 1, 2, 3, 4, 5), nrow = 3, ncol = 2, byrow = TRUE)
    colnames(umis) <- c("A", "B")
    rownames(umis) <- c("X", "Y", "Z")
    set_matrix(origin, "gene", "cell", "UMIs", umis, relayout = FALSE)

    h5ad_path <- tempfile(fileext = ".h5ad")

    # Convert to h5ad with X_eltype = "Float32"
    expect_no_error(
        daf_as_h5ad(
            origin,
            obs_is = "cell",
            var_is = "gene",
            X_is = "UMIs",
            h5ad = h5ad_path,
            X_eltype = "Float32"
        )
    )

    # Check that the file was created
    expect_true(file.exists(h5ad_path))

    # Convert back to daf and verify data is preserved
    back <- h5ad_as_daf(
        h5ad_path,
        obs_is = "cell",
        var_is = "gene",
        X_is = "UMIs",
        name = "anndata_eltype!"
    )

    expect_equal(name(back), "anndata_eltype!")
    expect_true(has_axis(back, "cell"))
    expect_true(has_axis(back, "gene"))

    # The matrix data should be preserved (possibly with float conversion)
    original_matrix <- get_matrix(origin, "gene", "cell", "UMIs")
    converted_matrix <- get_matrix(back, "gene", "cell", "UMIs")
    expect_equal(as.matrix(original_matrix), as.matrix(converted_matrix), tolerance = 1e-5, ignore_attr = TRUE)

    # Clean up
    unlink(h5ad_path)
})

test_that("daf_as_h5ad without X_eltype preserves original type", {
    # Create a memory daf
    origin <- memory_daf(name = "no_eltype_test!")
    add_axis(origin, "cell", c("A", "B"))
    add_axis(origin, "gene", c("X", "Y"))

    umis <- matrix(c(10, 20, 30, 40), nrow = 2, ncol = 2)
    colnames(umis) <- c("A", "B")
    rownames(umis) <- c("X", "Y")
    set_matrix(origin, "gene", "cell", "UMIs", umis, relayout = FALSE)

    h5ad_path <- tempfile(fileext = ".h5ad")

    # Convert without specifying X_eltype
    expect_no_error(
        daf_as_h5ad(
            origin,
            obs_is = "cell",
            var_is = "gene",
            X_is = "UMIs",
            h5ad = h5ad_path
        )
    )

    expect_true(file.exists(h5ad_path))

    # Convert back and verify
    back <- h5ad_as_daf(
        h5ad_path,
        obs_is = "cell",
        var_is = "gene",
        X_is = "UMIs",
        name = "back!"
    )

    original_matrix <- get_matrix(origin, "gene", "cell", "UMIs")
    converted_matrix <- get_matrix(back, "gene", "cell", "UMIs")
    expect_equal(as.matrix(original_matrix), as.matrix(converted_matrix), ignore_attr = TRUE)

    # Clean up
    unlink(h5ad_path)
})
