test_that("read_only creates a read-only wrapper", {
    # Create a writable daf
    source <- memory_daf(name = "source!")
    add_axis(source, "cell", c("A", "B"))
    set_scalar(source, "version", 1.0)
    set_vector(source, "cell", "type", c("T", "B"))

    # Create a read-only wrapper
    readonly <- read_only(source)

    # Test basic properties
    expect_equal(name(readonly), "source!")
    expect_equal(axes_set(readonly), "cell")
    expect_equal(axis_length(readonly, "cell"), 2)
    expect_equal(get_scalar(readonly, "version"), 1.0)
    expect_equal(get_vector(readonly, "cell", "type"), c(A = "T", B = "B"))

    # Test description
    desc <- description(readonly)
    expect_match(desc, "name: source!")
    expect_match(desc, "type: ReadOnly")

    # Test renaming the wrapper
    renamed <- read_only(source, name = "renamed!")
    expect_equal(name(renamed), "renamed!")

    # Test that changes to the source are visible through the wrapper
    set_scalar(source, "new_version", 2.0)
    expect_equal(get_scalar(readonly, "new_version"), 2.0)
})

test_that("read_only wrapper prevents modifications", {
    # Create a writable daf
    source <- memory_daf(name = "source!")
    add_axis(source, "cell", c("A", "B"))

    # Create a read-only wrapper
    readonly <- read_only(source)

    # Test that operations that modify data fail
    expect_error(set_scalar(readonly, "version", 1.0))
    expect_error(add_axis(readonly, "gene", c("X", "Y")))
    expect_error(set_vector(readonly, "cell", "type", c("T", "B")))
    expect_error(delete_scalar(readonly, "version"))
})

test_that("read_only on a read_only returns the same object", {
    # Create a writable daf
    source <- memory_daf(name = "source!")

    # Create a read-only wrapper
    readonly1 <- read_only(source)

    # Create a read-only wrapper of the read-only wrapper
    readonly2 <- read_only(readonly1)

    # Test that applying read_only twice doesn't wrap twice
    expect_equal(description(readonly1), description(readonly2))

    # Test with different name
    readonly_renamed <- read_only(readonly1, name = "renamed!")
    expect_equal(name(readonly_renamed), "renamed!")
})
