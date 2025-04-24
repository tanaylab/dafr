test_that("reconstruct_axis works correctly", {
    # Create a memory daf object
    memory <- memory_daf(name = "memory!")

    # Add axis and vectors
    memory <- add_axis(memory, "cell", c("A", "B", "C", "D"))
    memory <- set_vector(memory, "cell", "age", as.integer(c(1, 1, 2, 3)))
    memory <- set_vector(memory, "cell", "score", c(0.0, 0.5, 1.0, 2.0))
    memory <- set_vector(memory, "cell", "batch", c("X", "X", "Y", ""))

    results <- reconstruct_axis(
        memory,
        existing_axis = "cell",
        implicit_axis = "batch"
    )


    # Check results
    expect_equal(names(results), "age")
    expect_equal(unlist(results), c(age = 3))

    # check that the axis is added
    expect_equal(axis_length(memory, "cell"), 4)
    expect_equal(axis_length(memory, "batch"), 2)

    # test that the axis has the correct values
    expect_equal(axis_vector(memory, "batch"), c("X", "Y"))

    # Check description
    expected_description <- "name: memory!
type: MemoryDaf
axes:
  batch: 2 entries
  cell: 4 entries
vectors:
  batch:
    age: 2 x Int64 (Dense)
  cell:
    batch: 4 x Str (Dense)
    score: 4 x Float64 (Dense)"

    # Normalize whitespace for comparison
    actual_description <- gsub("\\s+", " ", trimws(description(memory)))
    expected_description <- gsub("\\s+", " ", trimws(expected_description))

    expect_true(grepl(expected_description, actual_description, fixed = TRUE))
})
