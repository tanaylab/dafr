test_that("chain_reader with one daf works", {
    # Create a daf
    source <- memory_daf(name = "source!")
    add_axis(source, "cell", c("A", "B"))
    set_scalar(source, "version", 1.0)
    set_vector(source, "cell", "type", c("T", "B"))

    # Create a chain reader with only one daf
    chain <- chain_reader(list(source), name = "chain!")

    # Test basic operations
    expect_equal(name(chain), "chain!")
    expect_equal(axes_set(chain), "cell")
    expect_equal(axis_length(chain, "cell"), 2)
    expect_equal(get_scalar(chain, "version"), 1.0)
    expect_equal(get_vector(chain, "cell", "type"), c(A = "T", B = "B"))

    # Test description
    desc <- description(chain)
    expect_match(desc, "name: chain!")
})

test_that("chain_reader with multiple dafs works", {
    # Create two dafs with overlapping and distinct data
    # Use the same set of entries for the overlapping axis to avoid errors
    source1 <- memory_daf(name = "source.1!")
    add_axis(source1, "cell", c("A", "B", "C", "D"))
    set_scalar(source1, "version", 1.0)
    set_vector(source1, "cell", "type", c("T", "B", "NK", "T"))

    source2 <- memory_daf(name = "source.2!")
    add_axis(source2, "cell", c("A", "B", "C", "D"))
    set_scalar(source2, "version", 2.0)
    set_vector(source2, "cell", "count", c(100, 200, 300, 400))

    # Create a chain reader with both dafs
    chain <- chain_reader(list(source1, source2))

    # Test data access - should prefer data from older sources
    expect_equal(get_scalar(chain, "version"), 2.0) # From source2

    # Test that axes have expected length
    expect_equal(axis_length(chain, "cell"), 4)
    expect_setequal(axis_vector(chain, "cell"), c("A", "B", "C", "D"))

    # Test vector access - should be able to access from both sources
    expect_equal(get_vector(chain, "cell", "type"), c(A = "T", B = "B", C = "NK", D = "T"))
    expect_equal(get_vector(chain, "cell", "count"), c(A = 100, B = 200, C = 300, D = 400))
})

test_that("chain_writer works correctly", {
    # Create a read-only daf and a writable daf
    # Use the same set of entries for the cell axis
    source1 <- memory_daf(name = "source.1!")
    add_axis(source1, "cell", c("A", "B", "C", "D"))
    set_scalar(source1, "version", 1.0)
    set_vector(source1, "cell", "type", c("T", "B", "NK", "T"))
    readonly_source <- read_only(source1)

    source2 <- memory_daf(name = "source.2!")
    add_axis(source2, "cell", c("A", "B", "C", "D"))

    # Create a chain writer
    chain <- chain_writer(list(readonly_source, source2), name = "chain!")

    # Test basic properties
    expect_equal(name(chain), "chain!")

    # Test reading data from the chain
    expect_equal(get_scalar(chain, "version"), 1.0) # From source1

    # Test writing data through the chain - should update the writable daf
    set_scalar(chain, "new_version", 3.0)
    expect_equal(get_scalar(source2, "new_version"), 3.0)
    expect_false(has_scalar(source1, "new_version"))

    # Test overwriting data
    expect_error(
        set_scalar(chain, "version", 3.0)
    )

    # Test overwriting with explicit flag
    set_scalar(chain, "version", 3.0, overwrite = TRUE)
    expect_equal(get_scalar(source2, "version"), 3.0)
    expect_equal(get_scalar(source1, "version"), 1.0) # Original unchanged

    # Test adding new data to an axis that exists in both sources
    # Should be added to the first writable source that has the axis
    set_vector(chain, "cell", "new_data", c("X", "Y", "Z", "W"))
    expect_equal(get_vector(source2, "cell", "new_data"), c(A = "X", B = "Y", C = "Z", D = "W"))
    expect_false(has_vector(source1, "cell", "new_data"))
})

test_that("chain_reader and chain_writer handle errors correctly", {
    # Create test dafs for error cases
    readonly1 <- read_only(memory_daf())
    readonly2 <- read_only(memory_daf())

    # Test with empty list - the Julia error message is "empty chain"
    expect_error(chain_reader(list()), "empty chain")
    expect_error(chain_writer(list()), "empty chain")

    # Test with non-daf objects - the specific error message depends on the Julia implementation
    expect_error(chain_reader(list("not a daf")))
    expect_error(chain_writer(list("not a daf")))

    # Test chain_writer with all read-only sources
    expect_error(
        chain_writer(list(readonly1, readonly2))
    )
})
