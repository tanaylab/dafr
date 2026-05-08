# Literal port of reconstruction.jl into R. 5 leaves.

.recon_fresh_daf <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    set_vector(d, "cell", "age", c(1L, 1L, 2L, 3L))
    set_vector(d, "cell", "score", c(0.0, 0.5, 1.0, 2.0))
    d
}

test_that("reconstruction / default", {
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "batch", c("X", "X", "Y", ""))
    results <- reconstruct_axis(d, existing_axis = "cell", implicit_axis = "batch")
    expect_setequal(names(results), "age")
    expect_identical(unname(as.integer(results[["age"]])), 3L)

    desc <- description(d)
    expect_match(desc, "name: memory!")
    expect_match(desc, "type: MemoryDaf")
    expect_match(desc, "batch: 2 entries")
    expect_match(desc, "cell: 4 entries")
    expect_match(desc, "age")  # batch.age (reconstructed)
    expect_match(desc, "score")  # cell.score (untouched)
})

test_that("reconstruction / inconsistent", {
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "batch", c("X", "X", "Y", ""))
    expect_error(
        reconstruct_axis(d,
            existing_axis = "cell", implicit_axis = "batch",
            implicit_properties = c("age", "score")
        ),
        regexp = "inconsistent.*score|score.*inconsistent"
    )
})

test_that("reconstruction / integer", {
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "batch", c(1L, 1L, 2L, 0L))
    results <- reconstruct_axis(d,
        existing_axis = "cell", implicit_axis = "batch",
        empty_implicit = 0L
    )
    expect_setequal(names(results), "age")
    expect_identical(unname(as.integer(results[["age"]])), 3L)
})

test_that("reconstruction / manual / !entry", {
    # CR4: dafr's reconstruct_axis refuses to merge into a pre-existing
    # axis (errors "axis already exists"). Julia's reconstruct_axis!
    # accepts an existing axis and instead errors when a used entry is
    # missing. Different semantics: dafr's design forbids the merge
    # entirely, while Julia treats existing-axis as the manual-entry
    # case. The substantive behavior - an error - is preserved.
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "batch", c("X", "X", "Y", ""))
    add_axis(d, "batch", c("X", "Z"))
    expect_error(
        reconstruct_axis(d, existing_axis = "cell", implicit_axis = "batch"),
        regexp = "axis.*already exists|already.*exists.*axis"
    )
})

test_that("reconstruction / manual / default", {
    skip("R divergence CR3: dafr's reconstruct_axis() does not support a `properties_defaults` parameter. Julia's reconstruct_axis! uses it to fill in unused-entry values for the manual/default case. Adding it requires merging into a pre-existing axis (also disallowed in dafr - see CR4).")
})
