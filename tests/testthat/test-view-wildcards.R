test_that("VIEW_ALL_DATA exposes every scalar/vector/matrix in base", {
    d <- memory_daf(name = "base")
    set_scalar(d, "organism", "human")
    add_axis(d, "cell", "c1")
    set_vector(d, "cell", "age", 10)
    v <- viewer(d, data = list(VIEW_ALL_DATA))
    expect_setequal(scalars_set(v), "organism")
    expect_setequal(vectors_set(v, "cell"), "age")
})

test_that("ALL_AXES mapped to NULL hides all axes", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", "c1")
    v <- viewer(d, axes = list(list(ALL_AXES, NULL)))
    expect_length(axes_set(v), 0L)
})

test_that("wildcard + specific override: last wins", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE))
    v <- viewer(d, axes = list(
        list(ALL_AXES, "="),
        list("cell", "@ cell [ keep ]")
    ))
    expect_equal(axis_vector(v, "cell"), c("c1", "c3"))
})
