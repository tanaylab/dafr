test_that("concatenate function works correctly", {
    sources <- list(
        memory_daf(name = "source.1!"),
        memory_daf(name = "source.2!")
    )

    # Set up data in sources
    add_axis(sources[[1]], "cell", c("A", "B"))
    add_axis(sources[[2]], "cell", c("C", "D", "E"))
    set_scalar(sources[[1]], "version", as.integer(1))
    set_scalar(sources[[2]], "version", as.integer(2))

    # Test with CollectAxis merge strategy
    destination <- memory_daf(name = "destination!")
    concatenate(
        destination = destination,
        axis = "cell",
        sources = sources,
        merge = list(version = "CollectAxis")
    )

    # Verify description output
    expected_desc <- paste0(
        "name: destination!\n",
        "type: MemoryDaf\n",
        "axes:\n",
        "  cell: 5 entries\n",
        "  dataset: 2 entries\n",
        "vectors:\n",
        "  cell:\n",
        "    dataset: 5 x Str (Dense)\n",
        "  dataset:\n",
        "    version: 2 x Int64 (Dense)\n"
    )
    expect_equal(description(destination), expected_desc)

    # Verify structure
    expect_equal(axis_length(destination, "cell"), 5)
    expect_equal(axis_length(destination, "dataset"), 2)
    expect_true(has_vector(destination, "cell", "dataset"))
    expect_true(has_vector(destination, "dataset", "version"))
    expect_equal(get_vector(destination, "dataset", "version"), c(1, 2))

    # Test basic concatenation without merge
    destination2 <- memory_daf(name = "destination!")
    concatenate(
        destination = destination2,
        axis = "cell",
        sources = sources
    )

    # Verify description output
    expected_desc2 <- paste0(
        "name: destination!\n",
        "type: MemoryDaf\n",
        "axes:\n",
        "  cell: 5 entries\n",
        "  dataset: 2 entries\n",
        "vectors:\n",
        "  cell:\n",
        "    dataset: 5 x Str (Dense)\n"
    )
    expect_equal(description(destination2), expected_desc2)

    # Verify structure
    expect_equal(axis_length(destination2, "cell"), 5)
    expect_equal(axis_length(destination2, "dataset"), 2)
    expect_true(has_vector(destination2, "cell", "dataset"))
    expect_false(has_vector(destination2, "dataset", "version"))
})
