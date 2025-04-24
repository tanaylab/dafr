# Helper function to set up a basic test DAF
setup_test_daf <- function(name = "test_daf!") {
    daf <- memory_daf(name)

    # Common test data
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    set_vector(daf, "cell", "batch", c("U", "V"))
    set_vector(daf, "cell", "age", c(-1.0, 2.0))
    set_vector(daf, "cell", "type", c("T", "B"))
    set_matrix(daf, "gene", "cell", "UMIs", matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE))

    return(daf)
}

# Helper function to set up a more complex test DAF
setup_advanced_daf <- function(name = "test_daf_adv!") {
    daf <- memory_daf(name)

    # More complex test data
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    add_axis(daf, "batch", c("U", "V", "W"))
    set_scalar(daf, "version", "1.0")
    set_vector(daf, "cell", "batch", c("U", "V", "W"))
    set_vector(daf, "cell", "age", c(-1.0, 2.0, 3.5))
    set_vector(daf, "cell", "type", c("T", "B", "T"))
    set_vector(daf, "cell", "score", c(0.9, 0.7, 0.8))
    set_vector(daf, "batch", "sex", c("Male", "Female", "Male"))
    set_matrix(daf, "cell", "gene", "UMIs", matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE))

    return(daf)
}

# Helper function to implement set() functionality for comparing unordered sets
set <- function(x) {
    sort(unique(x))
}
