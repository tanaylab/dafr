# Literal port of contracts.jl `vector` subgroup (lines 414-650) into R.
#
# Structure:
#   - vector / !axis     : 1 leaf, contractor() rejects vector-on-undeclared-axis
#   - vector / ~axis     : 1 leaf, contractor() rejects vector-on-incompatible-axis
#   - vector / ()        : 32 leaves (4 expectations x overwrite x direction x accessed)
#   - vector / missing   : 8 leaves (4 expectations x input/output)
#   - vector / !type     : 4 leaves (input{required, optional} + output{guaranteed, contingent})
# Total: 46 leaves.
#
# Expectation mapping (Julia -> R) - same as scalar/axis sub-slices:
#   required    -> RequiredInput
#   optional    -> OptionalInput
#   guaranteed  -> CreatedOutput   (R analogue of Julia GuaranteedOutput)
#   contingent  -> OptionalOutput  (Julia OptionalOutput)
#
# Divergence note: dev/notes/2026-05-08-contracts-vector-jl-parity-divergences.md.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.is_julia_output_expect_v <- function(expectation) {
    expectation %in% c(CreatedOutput, OptionalOutput)
}

.vector_existing_assert <- function(expectation, overwrite, direction, accessed) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        set_vector(d, "cell", "age", c(1L, 2L))
        contract <- Contract(
            axes = list(cell = list(RequiredInput, "description")),
            data = list(contract_vector("cell", "age", expectation,
                                        "integer", "description"))
        )
        cd <- contractor("computation", contract, d, overwrite = overwrite)
        stopifnot(axis_length(cd, "cell") == 2L)
        fn <- if (direction == "input") verify_input else verify_output
        if (accessed) {
            stopifnot(all(unname(get_vector(cd, "cell", "age")) == c(1L, 2L)))
            if (!overwrite && .is_julia_output_expect_v(expectation)) {
                expect_error(verify_input(cd),
                    regexp = "pre-existing.*vector.*age")
            } else {
                expect_silent(fn(cd))
            }
        } else {
            if (direction == "output" && expectation == RequiredInput) {
                expect_error(fn(cd),
                    regexp = "unused RequiredInput vector.*age")
            } else if (!overwrite && .is_julia_output_expect_v(expectation)) {
                expect_error(verify_input(cd),
                    regexp = "pre-existing.*vector.*age")
            } else {
                expect_silent(fn(cd))
            }
        }
    })
}

.vector_missing_assert <- function(expectation, axis_expect, direction) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        contract <- Contract(
            axes = list(cell = list(axis_expect, "description")),
            data = list(contract_vector("cell", "age", expectation,
                                        "integer", "description"))
        )
        cd <- contractor("computation", contract, d)
        # Mirror Julia: axis_length call marks the axis as accessed so the
        # unused-RequiredInput axis check in verify_output is satisfied even
        # when the vector itself is the test subject.
        stopifnot(axis_length(cd, "cell") == 2L)
        fn <- if (direction == "input") verify_input else verify_output
        if (direction == "input" && expectation == RequiredInput) {
            expect_error(fn(cd),
                regexp = "missing input vector.*age")
        } else if (direction == "output" && expectation == CreatedOutput) {
            expect_error(fn(cd),
                regexp = "missing output vector.*age")
        } else {
            expect_silent(fn(cd))
        }
    })
}

.vector_wrong_type_assert <- function(expectation, direction) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        set_vector(d, "cell", "age", c(1.0, 2.0))  # numeric not integer
        contract <- Contract(
            axes = list(cell = list(RequiredInput, "description")),
            data = list(contract_vector("cell", "age", expectation,
                                        "integer", "description"))
        )
        cd <- contractor("computation", contract, d)
        stopifnot(axis_length(cd, "cell") == 2L)
        fn <- if (direction == "input") verify_input else verify_output
        expect_error(fn(cd),
            regexp = sprintf("unexpected type.*numeric.*integer.*%s vector.*age",
                             direction))
    })
}

# ---------------------------------------------------------------------------
# vector / !axis - contractor rejects vector on undeclared axis
# ---------------------------------------------------------------------------

test_that("contracts / vector / !axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        contract <- Contract(
            data = list(contract_vector("cell", "age", RequiredInput,
                                        "integer", "description"))
        )
        expect_error(
            contractor("computation", contract, d),
            regexp = "non-contract axis.*cell.*RequiredInput vector.*age"
        )
    })
})

# ---------------------------------------------------------------------------
# vector / ~axis - axis OptionalInput is incompatible with RequiredInput vector
# ---------------------------------------------------------------------------

test_that("contracts / vector / ~axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        contract <- Contract(
            axes = list(cell = list(OptionalInput, "description")),
            data = list(contract_vector("cell", "age", RequiredInput,
                                        "integer", "description"))
        )
        expect_error(
            contractor("computation", contract, d),
            regexp = "incompatible OptionalInput axis.*cell.*RequiredInput vector.*age"
        )
    })
})

# ---------------------------------------------------------------------------
# vector / () / required (RequiredInput)
# ---------------------------------------------------------------------------

test_that("contracts / vector / () / required / overwrite / input / !accessed", {
    .vector_existing_assert(RequiredInput, overwrite = TRUE,
                            direction = "input", accessed = FALSE)
})
test_that("contracts / vector / () / required / overwrite / input / accessed", {
    .vector_existing_assert(RequiredInput, overwrite = TRUE,
                            direction = "input", accessed = TRUE)
})
test_that("contracts / vector / () / required / overwrite / output / !accessed", {
    .vector_existing_assert(RequiredInput, overwrite = TRUE,
                            direction = "output", accessed = FALSE)
})
test_that("contracts / vector / () / required / overwrite / output / accessed", {
    .vector_existing_assert(RequiredInput, overwrite = TRUE,
                            direction = "output", accessed = TRUE)
})
test_that("contracts / vector / () / required / !overwrite / input / !accessed", {
    .vector_existing_assert(RequiredInput, overwrite = FALSE,
                            direction = "input", accessed = FALSE)
})
test_that("contracts / vector / () / required / !overwrite / input / accessed", {
    .vector_existing_assert(RequiredInput, overwrite = FALSE,
                            direction = "input", accessed = TRUE)
})
test_that("contracts / vector / () / required / !overwrite / output / !accessed", {
    .vector_existing_assert(RequiredInput, overwrite = FALSE,
                            direction = "output", accessed = FALSE)
})
test_that("contracts / vector / () / required / !overwrite / output / accessed", {
    .vector_existing_assert(RequiredInput, overwrite = FALSE,
                            direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# vector / () / optional (OptionalInput)
# ---------------------------------------------------------------------------

test_that("contracts / vector / () / optional / overwrite / input / !accessed", {
    .vector_existing_assert(OptionalInput, overwrite = TRUE,
                            direction = "input", accessed = FALSE)
})
test_that("contracts / vector / () / optional / overwrite / input / accessed", {
    .vector_existing_assert(OptionalInput, overwrite = TRUE,
                            direction = "input", accessed = TRUE)
})
test_that("contracts / vector / () / optional / overwrite / output / !accessed", {
    .vector_existing_assert(OptionalInput, overwrite = TRUE,
                            direction = "output", accessed = FALSE)
})
test_that("contracts / vector / () / optional / overwrite / output / accessed", {
    .vector_existing_assert(OptionalInput, overwrite = TRUE,
                            direction = "output", accessed = TRUE)
})
test_that("contracts / vector / () / optional / !overwrite / input / !accessed", {
    .vector_existing_assert(OptionalInput, overwrite = FALSE,
                            direction = "input", accessed = FALSE)
})
test_that("contracts / vector / () / optional / !overwrite / input / accessed", {
    .vector_existing_assert(OptionalInput, overwrite = FALSE,
                            direction = "input", accessed = TRUE)
})
test_that("contracts / vector / () / optional / !overwrite / output / !accessed", {
    .vector_existing_assert(OptionalInput, overwrite = FALSE,
                            direction = "output", accessed = FALSE)
})
test_that("contracts / vector / () / optional / !overwrite / output / accessed", {
    .vector_existing_assert(OptionalInput, overwrite = FALSE,
                            direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# vector / () / guaranteed (Julia GuaranteedOutput -> R CreatedOutput)
# ---------------------------------------------------------------------------

test_that("contracts / vector / () / guaranteed / overwrite / input / !accessed", {
    .vector_existing_assert(CreatedOutput, overwrite = TRUE,
                            direction = "input", accessed = FALSE)
})
test_that("contracts / vector / () / guaranteed / overwrite / input / accessed", {
    .vector_existing_assert(CreatedOutput, overwrite = TRUE,
                            direction = "input", accessed = TRUE)
})
test_that("contracts / vector / () / guaranteed / overwrite / output / !accessed", {
    .vector_existing_assert(CreatedOutput, overwrite = TRUE,
                            direction = "output", accessed = FALSE)
})
test_that("contracts / vector / () / guaranteed / overwrite / output / accessed", {
    .vector_existing_assert(CreatedOutput, overwrite = TRUE,
                            direction = "output", accessed = TRUE)
})
test_that("contracts / vector / () / guaranteed / !overwrite / input / !accessed", {
    .vector_existing_assert(CreatedOutput, overwrite = FALSE,
                            direction = "input", accessed = FALSE)
})
test_that("contracts / vector / () / guaranteed / !overwrite / input / accessed", {
    .vector_existing_assert(CreatedOutput, overwrite = FALSE,
                            direction = "input", accessed = TRUE)
})
test_that("contracts / vector / () / guaranteed / !overwrite / output / !accessed", {
    .vector_existing_assert(CreatedOutput, overwrite = FALSE,
                            direction = "output", accessed = FALSE)
})
test_that("contracts / vector / () / guaranteed / !overwrite / output / accessed", {
    .vector_existing_assert(CreatedOutput, overwrite = FALSE,
                            direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# vector / () / contingent (Julia OptionalOutput -> R OptionalOutput)
# ---------------------------------------------------------------------------

test_that("contracts / vector / () / contingent / overwrite / input / !accessed", {
    .vector_existing_assert(OptionalOutput, overwrite = TRUE,
                            direction = "input", accessed = FALSE)
})
test_that("contracts / vector / () / contingent / overwrite / input / accessed", {
    .vector_existing_assert(OptionalOutput, overwrite = TRUE,
                            direction = "input", accessed = TRUE)
})
test_that("contracts / vector / () / contingent / overwrite / output / !accessed", {
    .vector_existing_assert(OptionalOutput, overwrite = TRUE,
                            direction = "output", accessed = FALSE)
})
test_that("contracts / vector / () / contingent / overwrite / output / accessed", {
    .vector_existing_assert(OptionalOutput, overwrite = TRUE,
                            direction = "output", accessed = TRUE)
})
test_that("contracts / vector / () / contingent / !overwrite / input / !accessed", {
    .vector_existing_assert(OptionalOutput, overwrite = FALSE,
                            direction = "input", accessed = FALSE)
})
test_that("contracts / vector / () / contingent / !overwrite / input / accessed", {
    .vector_existing_assert(OptionalOutput, overwrite = FALSE,
                            direction = "input", accessed = TRUE)
})
test_that("contracts / vector / () / contingent / !overwrite / output / !accessed", {
    .vector_existing_assert(OptionalOutput, overwrite = FALSE,
                            direction = "output", accessed = FALSE)
})
test_that("contracts / vector / () / contingent / !overwrite / output / accessed", {
    .vector_existing_assert(OptionalOutput, overwrite = FALSE,
                            direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# vector / missing - axis present, vector absent
# Note: Julia uses RequiredInput axis for required/guaranteed sub-leaves and
# OptionalInput axis for optional/contingent (since RequiredInput vector
# requires RequiredInput axis but OptionalInput vector is compatible with
# any axis).
# ---------------------------------------------------------------------------

test_that("contracts / vector / missing / required / input", {
    .vector_missing_assert(RequiredInput, axis_expect = RequiredInput, direction = "input")
})
test_that("contracts / vector / missing / required / output", {
    .vector_missing_assert(RequiredInput, axis_expect = RequiredInput, direction = "output")
})
test_that("contracts / vector / missing / optional / input", {
    .vector_missing_assert(OptionalInput, axis_expect = OptionalInput, direction = "input")
})
test_that("contracts / vector / missing / optional / output", {
    .vector_missing_assert(OptionalInput, axis_expect = OptionalInput, direction = "output")
})
test_that("contracts / vector / missing / guaranteed / input", {
    .vector_missing_assert(CreatedOutput, axis_expect = RequiredInput, direction = "input")
})
test_that("contracts / vector / missing / guaranteed / output", {
    .vector_missing_assert(CreatedOutput, axis_expect = RequiredInput, direction = "output")
})
test_that("contracts / vector / missing / contingent / input", {
    .vector_missing_assert(OptionalOutput, axis_expect = OptionalInput, direction = "input")
})
test_that("contracts / vector / missing / contingent / output", {
    .vector_missing_assert(OptionalOutput, axis_expect = OptionalInput, direction = "output")
})

# ---------------------------------------------------------------------------
# vector / !type - wrong-type vector present
# Julia structure: input arm tests required/optional; output arm tests
# guaranteed/contingent. Asymmetric vs scalar/!type.
# ---------------------------------------------------------------------------

test_that("contracts / vector / !type / input / required", {
    .vector_wrong_type_assert(RequiredInput, direction = "input")
})
test_that("contracts / vector / !type / input / optional", {
    .vector_wrong_type_assert(OptionalInput, direction = "input")
})
test_that("contracts / vector / !type / output / guaranteed", {
    .vector_wrong_type_assert(CreatedOutput, direction = "output")
})
test_that("contracts / vector / !type / output / contingent", {
    .vector_wrong_type_assert(OptionalOutput, direction = "output")
})
