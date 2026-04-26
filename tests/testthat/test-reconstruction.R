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

test_that("reconstruct_axis: empty_implicit marks entries as no-value", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "type", c("T", "NA", "B", "NA"))
    set_vector(d, "cell", "color", c("red", "gray", "blue", "gray"))

    empties <- reconstruct_axis(d,
        existing_axis = "cell", implicit_axis = "type",
        empty_implicit = "NA")

    expect_identical(axis_vector(d, "type"), c("B", "T"))
    expect_identical(unname(get_vector(d, "type", "color")),
                     c("blue", "red"))
    expect_identical(empties$color, "gray")
})

test_that("reconstruct_axis: skipped_properties excludes from migration", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("dA", "dB"))
    set_vector(d, "cell", "age", c(30L, 40L))
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor",
                     skipped_properties = "age")
    expect_true(has_vector(d, "cell", "age"))
    expect_false(has_vector(d, "donor", "age"))
})

test_that("reconstruct_axis: implicit_properties enforces consistency", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("dA", "dA", "dB"))
    set_vector(d, "cell", "tag", c("x", "y", "z"))
    expect_error(
        reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor",
                         implicit_properties = "tag"),
        "inconsistent"
    )
})

test_that("reconstruct_axis: rename_axis uses a different target name", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("dA", "dB"))
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor",
                     rename_axis = "person")
    expect_true(has_axis(d, "person"))
    expect_false(has_axis(d, "donor"))
})

test_that("reconstruct_axis: errors when target axis pre-exists", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1"))
    add_axis(d, "donor", c("dA"))
    set_vector(d, "cell", "donor", c("dA"))
    expect_error(reconstruct_axis(d, existing_axis = "cell",
                                  implicit_axis = "donor"),
                 "already exists")
})
