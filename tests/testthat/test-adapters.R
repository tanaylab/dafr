test_that("adapter works correctly", {
    # Create a test daf object
    daf <- memory_daf(name = "test!")
    add_axis(daf, "cell", c("A", "B"))

    # Define a simple computation function
    computation <- function(adapted_daf) {
        # The adapter should expose "cell" as "obs"
        expect_true(has_axis(adapted_daf, "obs"))
        expect_false(has_axis(adapted_daf, "cell"))

        # Add a new vector using the renamed axis
        set_vector(adapted_daf, "obs", "score", c(10.0, 20.0))

        # Add a new scalar
        set_scalar(adapted_daf, "computed", TRUE)

        return("Computation completed")
    }

    # Run the adapter with minimal configuration
    output_data <- list()
    result <- adapter(
        daf,
        computation,
        input_axes = list("obs" = "/ cell")
    )

    # Check that computation results are returned
    expect_equal(result, "Computation completed")

    # Check that the vector was copied back
    expect_true(has_vector(daf, "obs", "score"))
    expect_equal(get_vector(daf, "obs", "score"), c(10.0, 20.0))

    # Check that the scalar was copied
    expect_true(has_scalar(daf, "computed"))
    expect_true(get_scalar(daf, "computed"))
})

test_that("adapter captures new axes", {
    # Create a test daf object
    daf <- memory_daf(name = "test_capture!")
    add_axis(daf, "cell", c("A", "B"))

    # Define a computation that creates a new axis
    computation <- function(adapted_daf) {
        # Add a new axis
        add_axis(adapted_daf, "sample", c("S1", "S2", "S3"))

        # Add a vector on the new axis
        set_vector(adapted_daf, "sample", "quality", c(0.9, 0.8, 0.7))

        return("New axis created")
    }

    # Run the adapter
    result <- adapter(
        daf,
        computation,
        input_axes = list("obs" = "/ cell")
    )

    # Check the computation result
    expect_equal(result, "New axis created")

    # Check that the new axis and vector were copied back
    expect_true(has_axis(daf, "sample"))
    expect_equal(as.character(axis_vector(daf, "sample")), c("S1", "S2", "S3"))
    expect_true(has_vector(daf, "sample", "quality"))
    expect_equal(get_vector(daf, "sample", "quality"), c(0.9, 0.8, 0.7))
})

test_that("adapter with scalar renaming works", {
    # Create a daf object
    daf <- memory_daf(name = "memory!")

    # Set an input scalar
    set_scalar(daf, "INPUT", 1)

    # Define a computation function that uses get_query
    computation <- function(adapted_daf) {
        # Get the input scalar using a query
        input_value <- get_query(adapted_daf, ": input")

        # Set the output scalar
        set_scalar(adapted_daf, "output", input_value)

        return(7)
    }

    # Run the adapter with scalar renaming
    input_data <- list("input" = ": INPUT")
    output_data <- list("OUTPUT" = ": output")

    result <- adapter(
        daf,
        computation,
        input_data = input_data,
        output_data = output_data
    )

    # Verify the computation's return value
    expect_equal(result, 7)

    # Verify the result
    expect_true(has_scalar(daf, "OUTPUT"))
    expect_equal(get_scalar(daf, "OUTPUT"), 1)
})
