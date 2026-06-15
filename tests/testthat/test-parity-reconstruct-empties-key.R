# Parity fix: reconstruct_axis must keep an entry in the returned empties mapping
# for EVERY migrated property, even one with no empty-implicit entries (Julia sets
# value_of_empties_of_properties[property] = nothing unconditionally). dafr used
# `empty_values[[prop]] <- empty_v` which, when empty_v is NULL, DELETES the key.
# Audit probe recon-empties-keyset.

test_that("reconstruct_axis keeps the key for a migrated property with no empties", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("dA", "dB", "dA", "dB"))
    set_vector(d, "cell", "donor_age", c(30L, 40L, 30L, 40L))

    empties <- reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor")

    # Julia keeps donor_age in the mapping (value `nothing`) despite no empties.
    expect_true("donor_age" %in% names(empties))
    expect_null(empties$donor_age)
})
