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
