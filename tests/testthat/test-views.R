test_that("viewer creates correct views", {
    # Create a test daf object
    daf <- memory_daf(name = "test!")
    set_scalar(daf, "version", "1.0")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    set_vector(daf, "cell", "batch", c("U", "V"))
    set_vector(daf, "cell", "age", c(-1.0, 2.0))

    # Create the UMIs matrix as a 3x2 matrix (genes x cells)
    umis <- matrix(as.integer(c(1, 2, 3, 4, 5, 6)), nrow = 3, ncol = 2)
    set_matrix(daf, "gene", "cell", "UMIs", umis, relayout = FALSE)

    add_axis(daf, "batch", c("U", "V", "W"))
    set_vector(daf, "batch", "sex", c("Male", "Female", "Male"))

    # Verify the original daf structure
    desc <- description(daf)
    expect_true(grepl("name: test!", desc))
    expect_true(grepl("version: \"1.0\"", desc))
    expect_true(grepl("batch: 3 entries", desc))
    expect_true(grepl("cell: 2 entries", desc))
    expect_true(grepl("gene: 3 entries", desc))

    # Create a view
    data_list <- list()
    data_list[[ALL_SCALARS]] <- NULL
    data_list[[paste(ALL_VECTORS, collapse = ",")]] <- "="
    data_list[["obs,var,X"]] <- ": UMIs"

    view <- viewer(
        daf,
        name = "view!",
        axes = list("obs" = "/ cell", "var" = "/ gene"),
        data = data_list
    )

    # Verify the view structure
    expect_s3_class(view, "Daf")
    view_desc <- description(view)
    expect_true(grepl("name: view!", view_desc))
    expect_true(grepl("type: View", view_desc))
    expect_true(grepl("base: MemoryDaf test!", view_desc))
    expect_true(grepl("obs: 2 entries", view_desc))
    expect_true(grepl("var: 3 entries", view_desc))
    expect_true(grepl("age: 2 x Float64", view_desc))
    expect_true(grepl("batch: 2 x Str", view_desc))
    expect_true(grepl("X: 3 x 2 x Int64 in Columns", view_desc))
})
