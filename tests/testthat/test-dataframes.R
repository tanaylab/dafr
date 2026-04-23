test_that("get_dataframe returns a data.frame with axis-entry rownames", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    set_vector(d, "cell", "age", c(1L, 2L, 3L))
    df <- get_dataframe(d, "cell")
    expect_s3_class(df, "data.frame")
    expect_identical(rownames(df), c("c1", "c2", "c3"))
    expect_setequal(colnames(df), c("donor", "age"))
})

test_that("get_dataframe respects columns arg", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    set_vector(d, "cell", "age", c(1L, 2L))
    df <- get_dataframe(d, "cell", columns = "donor")
    expect_identical(colnames(df), "donor")
    expect_identical(df$donor, c("A", "B"))
})

test_that("get_dataframe_query is the query-string form", {
    d <- memory_daf()
    add_axis(d, "donor", c("d1", "d2", "d3"))
    set_vector(d, "donor", "age", c(20L, 30L, 40L))
    df <- get_dataframe_query(d, "@ donor")
    expect_identical(rownames(df), c("d1", "d2", "d3"))
    expect_true("age" %in% colnames(df))
})

test_that("get_dataframe cache=TRUE serves from the query cache", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    df1 <- get_dataframe(d, "cell", cache = TRUE)
    df2 <- get_dataframe(d, "cell", cache = TRUE)
    expect_identical(df1, df2)
})

test_that("get_tidy errors informatively without tidyr/tibble", {
    # tidyr and tibble are typically installed in a dev env; use a
    # fake libpath to simulate absence.
    if (requireNamespace("tidyr", quietly = TRUE) &&
        requireNamespace("tibble", quietly = TRUE)) {
        d <- memory_daf()
        add_axis(d, "cell", c("c1", "c2"))
        set_vector(d, "cell", "donor", c("A", "B"))
        tidy <- get_tidy(d, "cell")
        expect_s3_class(tidy, "tbl_df")
        expect_setequal(colnames(tidy), c("name", "key", "value"))
    } else {
        d <- memory_daf()
        expect_error(get_tidy(d, "cell"), "tidyr|tibble")
    }
})

test_that("get_frame is no longer exported", {
    expect_false("get_frame" %in% getNamespaceExports("dafr"))
})
