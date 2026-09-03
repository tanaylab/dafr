# Parity: since DataAxesFormats 0.3.0+, `reconstruct_axis!` no longer accepts an
# `empty_implicit` and no longer rewrites the implicit property. It requires a
# property of strings where "" means "no value", and says so when given anything
# else; saying which values mean nothing, and turning the rest into names, is
# what `unify_empty_vector_values()` is for.

test_that("reconstruct_axis rejects a non-string implicit property", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "batch", c(1L, 1L, 2L, 0L))
    expect_error(
        reconstruct_axis(d, existing_axis = "cell", implicit_axis = "batch"),
        "not a property of strings: batch"
    )
})

test_that("unify_empty_vector_values makes a numeric implicit usable", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "batch", c(1L, 1L, 2L, 0L))
    unify_empty_vector_values(d, axis = "cell", property = "batch",
                              empty_values = 0, dtype = "String")
    expect_identical(unname(get_vector(d, "cell", "batch")),
                     c("1", "1", "2", ""))
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "batch")
    expect_identical(axis_vector(d, "batch"), c("1", "2"))
})

test_that("reconstruct_axis leaves an already-string implicit alone", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("dA", "dB", "dA", "dB"))
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor")
    expect_identical(unname(get_vector(d, "cell", "donor")),
                     c("dA", "dB", "dA", "dB"))
})
