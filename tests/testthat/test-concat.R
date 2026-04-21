test_that("concatenate: single axis, two sources, creates dataset axis", {
    a <- memory_daf(name = "A")
    add_axis(a, "cell", c("a1", "a2"))
    set_vector(a, "cell", "age", c(10L, 20L))

    b <- memory_daf(name = "B")
    add_axis(b, "cell", c("b1", "b2", "b3"))
    set_vector(b, "cell", "age", c(1L, 2L, 3L))

    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b))

    expect_identical(axis_vector(dest, "cell"),
                     c("a1", "a2", "b1", "b2", "b3"))
    expect_identical(unname(get_vector(dest, "cell", "age")),
                     c(10L, 20L, 1L, 2L, 3L))
    expect_setequal(axes_set(dest), c("cell", "dataset"))
    expect_identical(axis_vector(dest, "dataset"), c("A", "B"))
    expect_identical(
        unname(get_vector(dest, "cell", "dataset")),
        c("A", "A", "B", "B", "B")
    )
})

test_that("concatenate: dataset_axis = NULL suppresses the dataset axis", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b), dataset_axis = NULL)
    expect_false(has_axis(dest, "dataset"))
    expect_identical(axis_vector(dest, "cell"), c("a1", "b1"))
})

test_that("concatenate: explicit names override source .name", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b), names = c("left", "right"))
    expect_identical(axis_vector(dest, "dataset"), c("left", "right"))
})
