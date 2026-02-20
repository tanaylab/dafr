test_that("open_daf correctly handles different file formats", {
    # Create temporary directories with unique names for testing
    temp_dir <- tempdir()
    random_suffix <- paste0(sample(letters, 8, replace = TRUE), collapse = "")
    files_dir <- file.path(temp_dir, paste0("files_daf_", random_suffix))
    h5df_path <- file.path(temp_dir, paste0("test_", random_suffix, ".h5df"))

    # Create test files/directories
    dir.create(files_dir, showWarnings = FALSE, recursive = TRUE)

    # Define cleanup with withr::defer
    withr::defer({
        # Force garbage collection to release file handles
        gc()

        # Clean up test files
        if (file.exists(h5df_path)) file.remove(h5df_path)
        if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)
    })

    # Create a files-based Daf
    files_daf <- files_daf(files_dir, "w", name = "files_test")
    set_scalar(files_daf, "source", "files")

    # Create an h5df file
    h5df_daf <- h5df(h5df_path, "w", name = "h5df_test")
    set_scalar(h5df_daf, "source", "h5df")

    # Force garbage collection to release file handles before opening again
    gc()

    # Test opening files-based Daf
    opened_files <- open_daf(files_dir, "r")
    expect_true(is_daf(opened_files))
    expect_equal(get_scalar(opened_files, "source"), "files")

    # Test opening h5df-based Daf
    opened_h5df <- open_daf(h5df_path, "r")
    expect_true(is_daf(opened_h5df))
    expect_equal(get_scalar(opened_h5df, "source"), "h5df")

    # Test error handling - use a non-existent path
    expect_error(open_daf(paste0(temp_dir, "/nonexistent_", random_suffix), "r"))
})

test_that("complete_daf correctly handles repository chains", {
    # Create temporary directories with unique names for testing
    temp_dir <- tempdir()
    random_suffix <- paste0(sample(letters, 8, replace = TRUE), collapse = "")
    chain_dir <- file.path(temp_dir, paste0("chain_test_", random_suffix))
    root_dir <- file.path(chain_dir, "root")
    base_dir <- file.path(root_dir, "base")
    leaf_dir <- file.path(base_dir, "leaf")


    # Create directory structure
    dir.create(leaf_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(root_dir, showWarnings = FALSE, recursive = TRUE)

    # Define cleanup with withr::defer
    withr::defer({
        # Clean up test directories
        if (dir.exists(chain_dir)) unlink(chain_dir, recursive = TRUE)
    })

    # Create root repository (no base)
    root_daf <- files_daf(root_dir, "w", name = "root_repo")
    set_scalar(root_daf, "level", "root")
    set_scalar(root_daf, "data", "root_data")

    # Create base repository with link to root
    base_daf <- files_daf(base_dir, "w", name = "base_repo")
    set_scalar(base_daf, "level", "base")
    set_scalar(base_daf, "data", "base_data")
    set_scalar(base_daf, "base_daf_repository", "../root")

    # Create leaf repository with link to base
    leaf_daf <- files_daf(leaf_dir, "w", name = "leaf_repo")
    set_scalar(leaf_daf, "level", "leaf")
    set_scalar(leaf_daf, "data", "leaf_data")
    set_scalar(leaf_daf, "base_daf_repository", "../base")

    # Test opening leaf only (no chain following)
    leaf_only <- files_daf(leaf_dir, "r")
    expect_equal(get_scalar(leaf_only, "level"), "leaf")
    expect_true(has_scalar(leaf_only, "base_daf_repository"))
    expect_false(has_scalar(leaf_only, "root_data"))
    # Test complete_daf in read-only mode
    complete <- complete_daf(leaf_dir, "r")
    expect_true(is_daf(complete))

    # Test that all repositories are accessible in the chain
    expect_equal(get_scalar(complete, "level"), "leaf") # Priority to later in chain
    expect_true(has_scalar(complete, "data"))
    expect_equal(get_scalar(complete, "data"), "leaf_data")

    # Test that we can't modify in read-only mode
    expect_error(set_scalar(complete, "test", "value"))

    # Test complete_daf in read-write mode
    complete_rw <- complete_daf(leaf_dir, "r+")

    # We should be able to write to the leaf repository
    set_scalar(complete_rw, "new_data", "written_through_chain")

    # Verify the data was written to leaf, not base or root
    leaf_reopened <- files_daf(leaf_dir, "r")
    base_reopened <- files_daf(base_dir, "r")
    expect_true(has_scalar(leaf_reopened, "new_data"))
    expect_false(has_scalar(base_reopened, "new_data"))

    # Test invalid mode error
    expect_error(complete_daf(leaf_dir, "invalid_mode"), "Mode must be one of")

    # Test with non-existent repository
    expect_error(complete_daf(file.path(chain_dir, "nonexistent"), "r"))
})
