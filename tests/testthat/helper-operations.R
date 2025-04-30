# Helper function to set up a test data frame
setup_test_data <- function() {
    daf <- memory_daf(name = "operations_test!")

    # Create axes
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # Add vectors
    set_vector(daf, "cell", "values", c(-1.5, 0, 2.5))
    set_vector(daf, "cell", "types", c("T", "B", "NK"))

    # Add a matrix - need to transpose since R matrices are column-major
    matrix_data <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
    set_matrix(daf, "cell", "gene", "data", matrix_data)

    return(daf)
}

var_uncorrected <- function(x) {
    mean((x - mean(x))^2)
}

sd_uncorrected <- function(x) {
    sqrt(mean((x - mean(x))^2))
}
