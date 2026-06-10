# Parity coverage for sparse VECTOR dtypes and copy-densification edge cases
# flagged by the DataAxesFormats.jl test audit. dafr already handles these
# correctly (these tests confirm it); they were just lacking explicit coverage
# (the existing sparse tests focus on matrices).

test_that("sparse Float/Bool/Int vectors round-trip through memory + files", {
    tmp <- tempfile(fileext = ".daf")
    on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
    d <- memory_daf(name = "m")
    add_axis(d, "cell", paste0("c", 1:6))
    set_vector(d, "cell", "sf", Matrix::sparseVector(c(1.5, 2.5), c(2L, 5L), 6L))
    set_vector(d, "cell", "sb", Matrix::sparseVector(c(TRUE, TRUE), c(1L, 4L), 6L))
    set_vector(d, "cell", "si", Matrix::sparseVector(c(7, 9), c(3L, 6L), 6L))

    # memory read-back
    expect_equal(as.numeric(get_vector(d, "cell", "sf")), c(0, 1.5, 0, 0, 2.5, 0))
    expect_equal(as.logical(get_vector(d, "cell", "sb")),
                 c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(as.numeric(get_vector(d, "cell", "si")), c(0, 0, 7, 0, 0, 9))

    # files round-trip (sparse on disk: .nzind/.nzval) preserves values + dtype
    fw <- files_daf(tmp, mode = "w"); copy_all(fw, d, relayout = FALSE); rm(fw); gc()
    fr <- files_daf(tmp, mode = "r")
    expect_equal(as.numeric(get_vector(fr, "cell", "sf")), c(0, 1.5, 0, 0, 2.5, 0))
    expect_equal(as.logical(get_vector(fr, "cell", "sb")),
                 c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(as.numeric(get_vector(fr, "cell", "si")), c(0, 0, 7, 0, 0, 9))
})

test_that("copy_vector to a superset fills only the new entries with `empty`", {
    d <- memory_daf(name = "src")
    add_axis(d, "cell", paste0("c", 1:6))
    set_vector(d, "cell", "sf", Matrix::sparseVector(c(1.5, 2.5), c(2L, 5L), 6L))
    dst <- memory_daf(name = "dst")
    add_axis(dst, "cell", paste0("c", 1:8))   # superset axis (c7, c8 are new)
    # `empty` fills only entries absent from the source; the source's own values
    # (including its structural zeros) are preserved verbatim. A non-zero empty
    # therefore yields a dense result with the new entries == empty.
    copy_vector(dst, d, "cell", "sf", empty = -1)
    expect_equal(unname(get_vector(dst, "cell", "sf")),
                 c(0, 1.5, 0, 0, 2.5, 0, -1, -1))
})

test_that("group_names is deterministic and member-order-invariant", {
    d <- memory_daf(name = "g")
    add_axis(d, "cell", paste0("c", 1:5))
    g1 <- group_names(d, "cell", list(c(2L, 4L), c(3L, 5L)), prefix = "G")
    g2 <- group_names(d, "cell", list(c(4L, 2L), c(5L, 3L)), prefix = "G")
    expect_length(g1, 2L)
    expect_identical(g1, g2)                    # order of members within a group
    expect_false(identical(g1[[1L]], g1[[2L]])) # different member sets differ
    expect_true(all(startsWith(g1, "G")))
})
