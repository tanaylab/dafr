test_that("setup_logger works with default parameters", {
    # We can't directly test the side effect, but we can verify it doesn't error
    expect_no_error(setup_logger())
})

test_that("setup_logger accepts stdout as io parameter", {
    expect_no_error(setup_logger(io = stdout))
})

test_that("setup_logger accepts different log levels", {
    expect_no_error(setup_logger(level = "Debug"))
    expect_no_error(setup_logger(level = "Info"))
    expect_no_error(setup_logger(level = "Warn"))
    expect_no_error(setup_logger(level = "Error"))
    expect_no_error(setup_logger(level = 42)) # Numeric level
})

test_that("setup_logger rejects invalid log levels", {
    expect_error(
        setup_logger(level = "InvalidLevel"),
        "Invalid log level"
    )
    expect_error(
        setup_logger(level = NULL)
    )
})

test_that("setup_logger rejects invalid io parameters", {
    expect_error(
        setup_logger(io = "stdout"),
        "logging into anything other than stdout and stderr"
    )

    # Create a dummy connection that's neither stdout nor stderr
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    file_conn <- file(temp_file, "w")
    on.exit(close(file_conn), add = TRUE)

    expect_error(
        setup_logger(io = file_conn),
        "logging into anything other than stdout and stderr"
    )
})

test_that("setup_logger accepts boolean formatting options", {
    # Test all combinations of boolean parameters
    expect_no_error(setup_logger(show_time = TRUE, show_module = TRUE, show_location = TRUE))
    expect_no_error(setup_logger(show_time = TRUE, show_module = TRUE, show_location = FALSE))
    expect_no_error(setup_logger(show_time = TRUE, show_module = FALSE, show_location = TRUE))
    expect_no_error(setup_logger(show_time = TRUE, show_module = FALSE, show_location = FALSE))
    expect_no_error(setup_logger(show_time = FALSE, show_module = TRUE, show_location = TRUE))
    expect_no_error(setup_logger(show_time = FALSE, show_module = TRUE, show_location = FALSE))
    expect_no_error(setup_logger(show_time = FALSE, show_module = FALSE, show_location = TRUE))
    expect_no_error(setup_logger(show_time = FALSE, show_module = FALSE, show_location = FALSE))
})

test_that("setup_logger returns invisibly", {
    expect_invisible(setup_logger())
})
