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
    unify_empty_vector_values(d,
        axis = "cell", property = "batch", empty_values = 0L, dtype = "String")
    results <- reconstruct_axis(d,
        existing_axis = "cell", implicit_axis = "batch"
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
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "batch", c("X", "X", "Y", ""))
    add_axis(d, "batch", c("X", "Y", "Z"))
    results <- reconstruct_axis(d,
        existing_axis = "cell", implicit_axis = "batch",
        properties_defaults = list(age = 4L)
    )
    expect_setequal(names(results), "age")
    expect_identical(unname(as.integer(results[["age"]])), 3L)
    expect_equal(unname(get_vector(d, "batch", "age")), c(1L, 2L, 4L))
    desc <- description(d)
    expect_match(desc, "batch: 3 entries")
    expect_match(desc, "cell: 4 entries")
})

# --- unify_empty_vector_values (port of reconstruction.jl "unify") ---

test_that("unify / strings", {
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "batch", c("X", "NA", "(Missing)", ""))
    unify_empty_vector_values(d, axis = "cell", property = "batch",
                              empty_values = c("NA", "(Missing)"))
    expect_identical(unname(get_vector(d, "cell", "batch")),
                     c("X", "", "", ""))
})

test_that("unify / floats", {
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "rank", c(1, 2, -2147483648, 3))
    unify_empty_vector_values(d, axis = "cell", property = "rank",
                              empty_values = -2147483648)
    values <- unname(get_vector(d, "cell", "rank"))
    expect_true(is.nan(values[[3L]]))
    expect_identical(values[c(1L, 2L, 4L)], c(1, 2, 3))
})

test_that("unify / !signed", {
    d <- .recon_fresh_daf()
    # A signed integer has no empty value: 0 and -1 are ordinary integers.
    expect_error(
        unify_empty_vector_values(d, axis = "cell", property = "age",
                                  empty_values = 1),
        "no empty value for the type"
    )
})

test_that("unify / signed", {
    d <- .recon_fresh_daf()
    unify_empty_vector_values(d, axis = "cell", property = "age",
                              empty_values = 1, empty_value = 0)
    expect_identical(unname(get_vector(d, "cell", "age")), c(0L, 0L, 2L, 3L))
})

test_that("unify / numbers as text", {
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "qc", c("23.5", "NA", "24.5", "NA"))
    unify_empty_vector_values(d, axis = "cell", property = "qc",
                              empty_values = "NA", dtype = "Float32")
    values <- unname(get_vector(d, "cell", "qc"))
    expect_identical(values[c(1L, 3L)], c(23.5, 24.5))
    expect_true(all(is.nan(values[c(2L, 4L)])))
})

test_that("unify / unsigned as text", {
    d <- .recon_fresh_daf()
    # An unsigned index is 1-based, so 0 is free to mean "none".
    set_vector(d, "cell", "plate_index", c("1", "", "32", ""))
    unify_empty_vector_values(d, axis = "cell", property = "plate_index",
                              empty_values = "", dtype = "UInt32")
    expect_identical(unname(get_vector(d, "cell", "plate_index")),
                     c(1L, 0L, 32L, 0L))
})

test_that("unify / !text", {
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "qc", c("23.5", "NA", "later", "NA"))
    expect_error(
        unify_empty_vector_values(d, axis = "cell", property = "qc",
                                  empty_values = "NA", dtype = "Float32"),
        "invalid value: later"
    )
})

test_that("unify / none", {
    d <- .recon_fresh_daf()
    # Which markers a property carries is a fact about the file, so matching
    # none of them is not an error.
    unify_empty_vector_values(d, axis = "cell", property = "age",
                              empty_values = 9)
    expect_identical(unname(get_vector(d, "cell", "age")), c(1L, 1L, 2L, 3L))
})

test_that("unify / as text", {
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "batch", c(1L, 1L, 2L, 0L))
    unify_empty_vector_values(d, axis = "cell", property = "batch",
                              empty_values = 0, dtype = "String")
    expect_identical(unname(get_vector(d, "cell", "batch")),
                     c("1", "1", "2", ""))
})

test_that("unify / !nothing", {
    d <- .recon_fresh_daf()
    expect_error(
        unify_empty_vector_values(d, axis = "cell", property = "age",
                                  empty_values = NULL),
        "no empty values and no type to convert to"
    )
})

# --- connect_axes (port of reconstruction.jl "connect") ---

.connect_fresh_daf <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "batch", c("B1", "B2", "B3", "B4"))
    add_axis(d, "plate", c("P1", "P2", "P3"))
    add_axis(d, "run", c("R1", "R2"))
    set_vector(d, "batch", "plate", c("P1", "P1", "P2", ""))
    set_vector(d, "batch", "run", c("R1", "R1", "R2", "R2"))
    d
}

test_that("connect / ()", {
    d <- .connect_fresh_daf()
    connect_axes(d, base_axis = "batch", from_axis = "plate", to_axis = "run")
    # P3 is named by no batch, so it is connected to nothing.
    expect_identical(unname(get_vector(d, "plate", "run")), c("R1", "R2", ""))
    # Nothing was moved, so the batch with no plate still has its run.
    expect_identical(unname(get_vector(d, "batch", "run")),
                     c("R1", "R1", "R2", "R2"))
})

test_that("connect / properties", {
    d <- .connect_fresh_daf()
    set_vector(d, "batch", "on_plate", c("P1", "P1", "P2", ""))
    set_vector(d, "batch", "sequenced_by", c("R1", "R1", "R2", "R2"))
    connect_axes(d, base_axis = "batch",
                 from_axis = "plate", from_property = "on_plate",
                 to_axis = "run", to_property = "sequenced_by",
                 connect_property = "sequenced_by")
    expect_identical(unname(get_vector(d, "plate", "sequenced_by")),
                     c("R1", "R2", ""))
    # The properties named after the axes are untouched, so the two coexist.
    expect_false(has_vector(d, "plate", "run"))
})

test_that("connect / !agree", {
    d <- .connect_fresh_daf()
    set_vector(d, "batch", "run", c("R1", "R2", "R2", "R2"), overwrite = TRUE)
    expect_error(
        connect_axes(d, base_axis = "batch", from_axis = "plate", to_axis = "run"),
        'conflicting entries: "R1" != "R2"'
    )
})

test_that("connect / !empty", {
    # An empty value is a value: batches of one plate disagreeing on whether
    # they have a run at all is as much a conflict as naming two different runs.
    d <- .connect_fresh_daf()
    set_vector(d, "batch", "run", c("R1", "", "R2", "R2"), overwrite = TRUE)
    expect_error(
        connect_axes(d, base_axis = "batch", from_axis = "plate", to_axis = "run"),
        'conflicting entries: "R1" != ""'
    )
})

test_that("connect / !to_entry", {
    d <- .connect_fresh_daf()
    set_vector(d, "batch", "run", c("R1", "R1", "R9", "R2"), overwrite = TRUE)
    expect_error(
        connect_axes(d, base_axis = "batch", from_axis = "plate", to_axis = "run"),
        "missing entry: R9"
    )
})

test_that("connect / !from_entry", {
    d <- .connect_fresh_daf()
    set_vector(d, "batch", "plate", c("P1", "P1", "P9", ""), overwrite = TRUE)
    expect_error(
        connect_axes(d, base_axis = "batch", from_axis = "plate", to_axis = "run"),
        "missing entry: P9"
    )
})

test_that("reconstruction / gaps", {
    # A missing number is NaN, so comparing values with != would read two absent
    # values as disagreeing and leave behind every property that has a gap.
    d <- .recon_fresh_daf()
    set_vector(d, "cell", "batch", c("X", "X", "Y", "Y"))
    set_vector(d, "cell", "score", c(NaN, NaN, 2.0, 2.0), overwrite = TRUE)
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "batch")
    expect_true("score" %in% vectors_set(d, "batch"))
    scores <- unname(get_vector(d, "batch", "score"))
    expect_true(is.nan(scores[[1L]]))
    expect_identical(scores[[2L]], 2.0)
})

test_that("unify / dtype names", {
    d <- .recon_fresh_daf()
    # `.UNIFY_STORAGE_MODE` is a named character vector, and `[[` on one throws
    # for an unknown name, so an unusable dtype has to be caught by name first.
    expect_error(
        unify_empty_vector_values(d, axis = "cell", property = "age",
                                  empty_values = 1, dtype = "Complex"),
        "unsupported dtype: Complex"
    )
    # The lowercase spellings Julia's DTYPE_BY_NAME accepts, plus "string".
    unify_empty_vector_values(d, axis = "cell", property = "age",
                              empty_values = 1, dtype = "string")
    expect_identical(unname(get_vector(d, "cell", "age")), c("", "", "2", "3"))
})
