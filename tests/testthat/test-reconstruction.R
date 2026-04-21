test_that("reconstruct_axis creates new axis from unique implicit values", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("dA", "dB", "dA", "dB"))
    set_vector(d, "cell", "donor_age", c(30L, 40L, 30L, 40L))
    set_vector(d, "cell", "single_score", c(0.1, 0.2, 0.3, 0.4))

    empties <- reconstruct_axis(d,
        existing_axis = "cell", implicit_axis = "donor")

    expect_true(has_axis(d, "donor"))
    expect_identical(axis_vector(d, "donor"), c("dA", "dB"))
    expect_true(has_vector(d, "donor", "donor_age"))
    expect_identical(unname(get_vector(d, "donor", "donor_age")),
                     c(30L, 40L))
    expect_false(has_vector(d, "cell", "donor_age"))
    expect_true(has_vector(d, "cell", "single_score"))
    expect_identical(empties$donor_age, NULL)
})
