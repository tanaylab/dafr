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
    expect_equal(get_vector(destination, "cell", "age"), c(0.0, 1.0))

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
    expect_equal(get_vector(new_destination, "cell", "age"), c(0.0, 1.0))
    expect_equal(get_matrix(new_destination, "gene", "cell", "UMIs"), umis)
})
