# Parity fix: after reconstruct_axis, the implicit property on the existing axis
# must be rewritten as a STRING foreign-key into the new axis (with "" for empty
# entries) - matching Julia's overwrite_implicit_values. dafr left the implicit
# property unchanged (e.g. numeric), so cell.batch stayed [1,1,2,0] instead of
# ["1","1","2",""]. Julia rewrites when the property is not already a string OR
# when a non-empty empty_implicit was given. Audit probe recon-int-rewrite.

test_that("reconstruct_axis rewrites a numeric implicit property as a string FK", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "batch", c(1L, 1L, 2L, 0L))
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "batch",
                     empty_implicit = 0)
    expect_identical(axis_vector(d, "batch"), c("1", "2"))
    # rewritten string FK into the batch axis, "" for the empty (0) entry
    expect_identical(unname(get_vector(d, "cell", "batch")), c("1", "1", "2", ""))
})

test_that("reconstruct_axis does NOT rewrite an already-string implicit with no empty_implicit", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("dA", "dB", "dA", "dB"))
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor")
    expect_identical(unname(get_vector(d, "cell", "donor")),
                     c("dA", "dB", "dA", "dB"))
})
