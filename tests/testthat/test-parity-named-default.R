# Parity fix: a full-length NAMED `default` passed to get_vector must have its
# names match the axis entries (in order), matching Julia's require_axis_names.
# dafr previously discarded the default's own names and relabeled the positional
# values with the axis entries - silently mis-assigning values to the wrong
# entries. Audit probe P4-named-default-wrong-order.

test_that("get_vector named default with wrong name order errors (Julia parity)", {
    d <- memory_daf()
    add_axis(d, "type", c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
    # Names match the axis as a SET but in the wrong order -> Julia errors
    # (must not silently relabel memory-B with the value 1).
    expect_error(
        get_vector(d, "type", "no_such",
                   default = c(MPP = 1, `MEBEMP-L` = 2, `MEBEMP-E` = 3, `memory-B` = 4)),
        regexp = "entry names of the default mismatch"
    )
})

test_that("get_vector unnamed length-N default is used positionally", {
    d <- memory_daf()
    add_axis(d, "type", c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
    out <- get_vector(d, "type", "no_such2", default = c(10, 20, 30, 40))
    expect_identical(unname(out), c(10, 20, 30, 40))
    expect_identical(names(out), c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
})

test_that("get_vector correctly-ordered named default is accepted", {
    d <- memory_daf()
    add_axis(d, "type", c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
    out <- get_vector(d, "type", "no_such3",
                      default = c(`memory-B` = 4, `MEBEMP-E` = 3, `MEBEMP-L` = 2, MPP = 1))
    expect_identical(unname(out), c(4, 3, 2, 1))
})
