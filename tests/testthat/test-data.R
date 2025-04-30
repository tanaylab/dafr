daf <- example_cells_daf()

test_that("get_vector returns named vector", {
    vec <- get_vector(daf, "cell", "donor")
    expect_true(!is.null(names(vec)))
    expect_equal(names(vec), axis_entries(daf, "cell"))

    # Test default NA vector
    vec_na <- get_vector(daf, "cell", "nonexistent_vector", default = NA)
    expect_true(!is.null(names(vec_na)))
    expect_equal(names(vec_na), axis_entries(daf, "cell"))
    expect_true(all(is.na(vec_na)))
})

test_that("get_matrix returns named matrix", {
    mat <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_true(!is.null(rownames(mat)))
    expect_true(!is.null(colnames(mat)))
    expect_equal(rownames(mat), axis_entries(daf, "cell"))
    expect_equal(colnames(mat), axis_entries(daf, "gene"))

    # Test default NA matrix
    mat_na <- get_matrix(daf, "cell", "gene", "nonexistent_matrix", default = NA)
    expect_true(!is.null(rownames(mat_na)))
    expect_true(!is.null(colnames(mat_na)))
    expect_equal(rownames(mat_na), axis_entries(daf, "cell"))
    expect_equal(colnames(mat_na), axis_entries(daf, "gene"))
    expect_true(all(is.na(mat_na)))
})

test_that("get_dataframe returns dataframe with row and col names", {
    df <- get_dataframe(daf, "cell", columns = c("donor", "experiment"))
    expect_true(!is.null(rownames(df)))
    expect_true(!is.null(colnames(df)))
    expect_equal(rownames(df), axis_entries(daf, "cell"))
    expect_equal(colnames(df), c("donor", "experiment"))

    # Test get_dataframe with NULL columns (should return all)
    df_all <- get_dataframe(daf, "cell")
    expect_true(!is.null(rownames(df_all)))
    expect_true(!is.null(colnames(df_all)))
    expect_true("donor" %in% colnames(df_all))
    expect_true("experiment" %in% colnames(df_all))
    expect_false("name" %in% colnames(df_all)) # 'name' column should be removed
    expect_equal(rownames(df_all), axis_entries(daf, "cell"))
})

test_that("get_tidy returns tidy tibble with correct columns", {
    tidy_df <- get_tidy(daf, "cell", columns = c("donor", "experiment"))
    expect_s3_class(tidy_df, "tbl_df")
    expect_equal(colnames(tidy_df), c("name", "key", "value"))
    expect_true(all(tidy_df$name %in% axis_entries(daf, "cell")))
    expect_true(all(tidy_df$key %in% c("donor", "experiment")))

    # Test get_tidy with NULL columns
    tidy_all <- get_tidy(daf, "cell")
    expect_s3_class(tidy_all, "tbl_df")
    expect_equal(colnames(tidy_all), c("name", "key", "value"))
    expect_true(all(tidy_all$name %in% axis_entries(daf, "cell")))
    expect_true(all(tidy_all$key %in% c("donor", "experiment")))
})
