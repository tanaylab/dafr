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

test_that("concatenate: missing property requires empty", {
    a <- memory_daf(name = "A")
    add_axis(a, "cell", c("a1"))
    set_vector(a, "cell", "age", c(10L))

    b <- memory_daf(name = "B")
    add_axis(b, "cell", c("b1"))

    dest <- memory_daf(name = "dest")
    expect_error(concatenate(dest, "cell", list(a, b)), "no empty value")

    dest2 <- memory_daf(name = "dest2")
    concatenate(dest2, "cell", list(a, b),
                empty = list("cell|age" = -1L))
    expect_identical(unname(get_vector(dest2, "cell", "age")),
                     c(10L, -1L))
})

test_that("concatenate: vector type unifies across sources", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    set_vector(a, "cell", "age", c(10L))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    set_vector(b, "cell", "age", c(2.5))
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b))
    expect_true(is.double(get_vector(dest, "cell", "age")))
})

test_that("concatenate: prefix=TRUE de-duplicates concat-axis entries", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("c1", "c2"))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("c1", "c3"))
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b), prefix = TRUE)
    expect_identical(axis_vector(dest, "cell"),
                     c("A.c1", "A.c2", "B.c1", "B.c3"))
})

test_that("concatenate: prefix heuristic prefixes properties named after the axis", {
    a <- memory_daf(name = "A")
    add_axis(a, "cell", c("c1"))
    add_axis(a, "cluster", c("cl1"))
    set_vector(a, "cell", "cluster", c("cl1"))
    b <- memory_daf(name = "B")
    add_axis(b, "cell", c("c1"))
    add_axis(b, "cluster", c("cl1"))
    set_vector(b, "cell", "cluster", c("cl1"))
    dest <- memory_daf(name = "dest")
    concatenate(dest, c("cell", "cluster"), list(a, b), prefix = TRUE)
    expect_identical(unname(get_vector(dest, "cell", "cluster")),
                     c("A.cl1", "B.cl1"))
})

test_that("concatenate: duplicate entries without prefix raise", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("c1"))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("c1"))
    dest <- memory_daf(name = "dest")
    expect_error(concatenate(dest, "cell", list(a, b)),
                 "duplicate entries")
})
