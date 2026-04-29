test_that("axis_version_counter starts at 0L and increments only on delete_axis (Julia parity)", {
    d <- memory_daf(name = "vc")
    expect_identical(axis_version_counter(d, "cell"), 0L)
    add_axis(d, "cell", c("c1", "c2"))
    # Julia parity: add_axis does NOT bump the counter.
    expect_identical(axis_version_counter(d, "cell"), 0L)
    delete_axis(d, "cell")
    expect_identical(axis_version_counter(d, "cell"), 1L)
    # Unrelated axis mutation does not bump "cell".
    add_axis(d, "gene", c("g1", "g2", "g3"))
    expect_identical(axis_version_counter(d, "cell"), 1L)
    expect_identical(axis_version_counter(d, "gene"), 0L)
    delete_axis(d, "gene")
    expect_identical(axis_version_counter(d, "cell"), 1L)
    expect_identical(axis_version_counter(d, "gene"), 1L)
})

test_that("vector_version_counter tracks per-vector mutation", {
    d <- memory_daf(name = "vc")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    expect_identical(vector_version_counter(d, "cell", "donor"), 0L)
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    expect_identical(vector_version_counter(d, "cell", "donor"), 1L)
    # Reads do not bump.
    get_vector(d, "cell", "donor")
    expect_identical(vector_version_counter(d, "cell", "donor"), 1L)
    # Overwrite bumps.
    set_vector(d, "cell", "donor", c("B", "A", "B"), overwrite = TRUE)
    expect_identical(vector_version_counter(d, "cell", "donor"), 2L)
    # Unrelated vector is 0.
    expect_identical(vector_version_counter(d, "cell", "age"), 0L)
})

test_that("matrix_version_counter tracks per-matrix mutation", {
    d <- memory_daf(name = "vc")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    expect_identical(matrix_version_counter(d, "cell", "gene", "UMIs"), 0L)
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(1:6, nrow = 2, ncol = 3))
    expect_identical(matrix_version_counter(d, "cell", "gene", "UMIs"), 1L)
})

test_that("version_counter return type is integer(1)", {
    d <- memory_daf(name = "vc")
    expect_type(axis_version_counter(d, "cell"), "integer")
    expect_length(axis_version_counter(d, "cell"), 1L)
    expect_type(vector_version_counter(d, "cell", "x"), "integer")
    expect_length(vector_version_counter(d, "cell", "x"), 1L)
    expect_type(matrix_version_counter(d, "cell", "gene", "x"), "integer")
    expect_length(matrix_version_counter(d, "cell", "gene", "x"), 1L)
})
