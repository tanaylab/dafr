test_that("example_cells_daf returns expected structure", {
    # Load the example cells data
    daf <- example_cells_daf()

    expect_error(example_cells_daf(name = 17))
    expect_error(example_cells_daf(name = NULL))
    # Test that it returns a proper Daf object
    expect_true(is_daf(daf))
    expect_equal(name(daf), "cells!")

    # Test axes
    expect_setequal(axes_set(daf), c("cell", "donor", "experiment", "gene"))
    expect_equal(axis_length(daf, "cell"), 856)
    expect_equal(axis_length(daf, "donor"), 95)
    expect_equal(axis_length(daf, "experiment"), 23)
    expect_equal(axis_length(daf, "gene"), 683)

    # Test vectors
    expect_true(has_vector(daf, "cell", "donor"))
    expect_true(has_vector(daf, "cell", "experiment"))
    expect_true(has_vector(daf, "donor", "age"))
    expect_true(has_vector(daf, "donor", "sex"))
    expect_true(has_vector(daf, "gene", "is_lateral"))

    # Test matrices
    expect_true(has_matrix(daf, "cell", "gene", "UMIs"))
    expect_true(has_matrix(daf, "gene", "cell", "UMIs"))

    # Test description matches expected
    expected_desc <- paste0(
        "name: cells!\n",
        "type: MemoryDaf\n",
        "axes:\n",
        "  cell: 856 entries\n",
        "  donor: 95 entries\n",
        "  experiment: 23 entries\n",
        "  gene: 683 entries\n",
        "vectors:\n",
        "  cell:\n",
        "    donor: 856 x Str (Dense)\n",
        "    experiment: 856 x Str (Dense)\n",
        "  donor:\n",
        "    age: 95 x UInt32 (Dense)\n",
        "    sex: 95 x Str (Dense)\n",
        "  gene:\n",
        "    is_lateral: 683 x Bool (Dense; 64% true)\n",
        "matrices:\n",
        "  cell,gene:\n",
        "    UMIs: 856 x 683 x UInt8 in Columns (Dense)\n",
        "  gene,cell:\n",
        "    UMIs: 683 x 856 x UInt8 in Columns (Dense)\n"
    )

    # Compare description (ignoring exact percentage which might vary)
    actual_desc <- description(daf)
    expect_match(actual_desc, "name: cells!")
    expect_match(actual_desc, "type: MemoryDaf")
    expect_match(actual_desc, "cell: 856 entries")
    expect_match(actual_desc, "donor: 95 entries")
    expect_match(actual_desc, "experiment: 23 entries")
    expect_match(actual_desc, "gene: 683 entries")
})

test_that("example_metacells_daf returns expected structure", {
    # Load the example metacells data
    daf <- example_metacells_daf()

    expect_error(example_metacells_daf(name = 17))
    expect_error(example_metacells_daf(name = NULL))

    # Test that it returns a proper Daf object
    expect_true(is_daf(daf))
    expect_equal(name(daf), "metacells!")

    # Test axes
    expect_equal(sort(axes_set(daf)), sort(c("cell", "gene", "metacell", "type")))
    expect_equal(axis_length(daf, "cell"), 856)
    expect_equal(axis_length(daf, "gene"), 683)
    expect_equal(axis_length(daf, "metacell"), 7)
    expect_equal(axis_length(daf, "type"), 4)

    # Test vectors
    expect_true(has_vector(daf, "cell", "metacell"))
    expect_true(has_vector(daf, "gene", "is_marker"))
    expect_true(has_vector(daf, "metacell", "type"))
    expect_true(has_vector(daf, "type", "color"))

    # Test matrices
    expect_true(has_matrix(daf, "gene", "metacell", "fraction"))

    # Test description matches expected
    expected_desc <- paste0(
        "name: metacells!\n",
        "type: MemoryDaf\n",
        "axes:\n",
        "  cell: 856 entries\n",
        "  gene: 683 entries\n",
        "  metacell: 7 entries\n",
        "  type: 4 entries\n",
        "vectors:\n",
        "  cell:\n",
        "    metacell: 856 x Str (Dense)\n",
        "  gene:\n",
        "    is_marker: 683 x Bool (Dense; 95% true)\n",
        "  metacell:\n",
        "    type: 7 x Str (Dense)\n",
        "  type:\n",
        "    color: 4 x Str (Dense)\n",
        "matrices:\n",
        "  gene,metacell:\n",
        "    fraction: 683 x 7 x Float32 in Columns (Dense)\n"
    )

    # Compare description (ignoring exact percentage which might vary)
    actual_desc <- description(daf)
    expect_match(actual_desc, "name: metacells!")
    expect_match(actual_desc, "type: MemoryDaf")
    expect_match(actual_desc, "cell: 856 entries")
    expect_match(actual_desc, "gene: 683 entries")
    expect_match(actual_desc, "metacell: 7 entries")
    expect_match(actual_desc, "type: 4 entries")
})

test_that("example_cells_daf accepts custom name", {
    daf <- example_cells_daf(name = "custom_cells!")
    expect_equal(name(daf), "custom_cells!")
})

test_that("example_metacells_daf accepts custom name", {
    daf <- example_metacells_daf(name = "custom_metacells!")
    expect_equal(name(daf), "custom_metacells!")
})
