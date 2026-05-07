# Literal port of contracts.jl `scalar` subgroup (lines 105-272) into R.
#
# Maps Julia's nested cross-product (4 expectations x overwrite/!overwrite
# x input/output x !accessed/accessed = 32 leaves for the `()` block,
# plus 8 for `missing` and 8 for `!type`) to one test_that per leaf.
#
# Expectation mapping (Julia -> R enum):
#   required    -> RequiredInput
#   optional    -> OptionalInput
#   guaranteed  -> CreatedOutput   (R's CreatedOutput is the only output
#                                   enum that triggers .is_mandatory on
#                                   output and .is_forbidden on input;
#                                   dafr's GuaranteedOutput exists as a
#                                   separate enum but is not enforced
#                                   in verify_*. CreatedOutput is the
#                                   semantic equivalent of Julia's
#                                   GuaranteedOutput.)
#   contingent  -> OptionalOutput  (Julia OptionalOutput. CS1 lifted
#                                   2026-05-08: .is_forbidden widened
#                                   to include OptionalOutput.)
#
# Divergence note (now empty / closed):
# dev/notes/2026-05-08-contracts-scalar-jl-parity-divergences.md.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.is_julia_output_expect <- function(expectation) {
    expectation %in% c(CreatedOutput, OptionalOutput)
}

.scalar_existing_assert <- function(expectation, overwrite, direction, accessed) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        set_scalar(d, "version", 1L)
        contract <- Contract(data = list(
            contract_scalar("version", expectation, "integer", "description")
        ))
        cd <- contractor("computation", contract, d, overwrite = overwrite)
        fn <- if (direction == "input") verify_input else verify_output
        if (accessed) {
            stopifnot(get_scalar(cd, "version") == 1L)
            if (!overwrite && .is_julia_output_expect(expectation)) {
                expect_error(verify_input(cd),
                    regexp = "pre-existing.*scalar.*version")
            } else {
                expect_silent(fn(cd))
            }
        } else {
            if (direction == "output" && expectation == RequiredInput) {
                expect_error(fn(cd),
                    regexp = "unused RequiredInput scalar.*version")
            } else if (!overwrite && .is_julia_output_expect(expectation)) {
                expect_error(verify_input(cd),
                    regexp = "pre-existing.*scalar.*version")
            } else {
                expect_silent(fn(cd))
            }
        }
    })
}

.scalar_missing_assert <- function(expectation, direction) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        contract <- Contract(data = list(
            contract_scalar("version", expectation, "integer", "description")
        ))
        cd <- contractor("computation", contract, d)
        fn <- if (direction == "input") verify_input else verify_output
        if (direction == "input" && expectation == RequiredInput) {
            expect_error(fn(cd),
                regexp = "missing input scalar.*version")
        } else if (direction == "output" && expectation == CreatedOutput) {
            expect_error(fn(cd),
                regexp = "missing output scalar.*version")
        } else {
            expect_silent(fn(cd))
        }
    })
}

.scalar_wrong_type_assert <- function(expectation, direction) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        set_scalar(d, "version", "1.0")  # character, not integer
        contract <- Contract(data = list(
            contract_scalar("version", expectation, "integer", "description")
        ))
        cd <- contractor("computation", contract, d, overwrite = TRUE)
        fn <- if (direction == "input") verify_input else verify_output
        expect_error(fn(cd),
            regexp = sprintf("unexpected type.*character.*integer.*%s scalar.*version",
                             direction))
    })
}

# ---------------------------------------------------------------------------
# scalar / () / required (RequiredInput)
# ---------------------------------------------------------------------------

test_that("contracts / scalar / () / required / overwrite / input / !accessed", {
    .scalar_existing_assert(RequiredInput, overwrite = TRUE,
                            direction = "input", accessed = FALSE)
})

test_that("contracts / scalar / () / required / overwrite / input / accessed", {
    .scalar_existing_assert(RequiredInput, overwrite = TRUE,
                            direction = "input", accessed = TRUE)
})

test_that("contracts / scalar / () / required / overwrite / output / !accessed", {
    .scalar_existing_assert(RequiredInput, overwrite = TRUE,
                            direction = "output", accessed = FALSE)
})

test_that("contracts / scalar / () / required / overwrite / output / accessed", {
    .scalar_existing_assert(RequiredInput, overwrite = TRUE,
                            direction = "output", accessed = TRUE)
})

test_that("contracts / scalar / () / required / !overwrite / input / !accessed", {
    .scalar_existing_assert(RequiredInput, overwrite = FALSE,
                            direction = "input", accessed = FALSE)
})

test_that("contracts / scalar / () / required / !overwrite / input / accessed", {
    .scalar_existing_assert(RequiredInput, overwrite = FALSE,
                            direction = "input", accessed = TRUE)
})

test_that("contracts / scalar / () / required / !overwrite / output / !accessed", {
    .scalar_existing_assert(RequiredInput, overwrite = FALSE,
                            direction = "output", accessed = FALSE)
})

test_that("contracts / scalar / () / required / !overwrite / output / accessed", {
    .scalar_existing_assert(RequiredInput, overwrite = FALSE,
                            direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# scalar / () / optional (OptionalInput)
# ---------------------------------------------------------------------------

test_that("contracts / scalar / () / optional / overwrite / input / !accessed", {
    .scalar_existing_assert(OptionalInput, overwrite = TRUE,
                            direction = "input", accessed = FALSE)
})

test_that("contracts / scalar / () / optional / overwrite / input / accessed", {
    .scalar_existing_assert(OptionalInput, overwrite = TRUE,
                            direction = "input", accessed = TRUE)
})

test_that("contracts / scalar / () / optional / overwrite / output / !accessed", {
    .scalar_existing_assert(OptionalInput, overwrite = TRUE,
                            direction = "output", accessed = FALSE)
})

test_that("contracts / scalar / () / optional / overwrite / output / accessed", {
    .scalar_existing_assert(OptionalInput, overwrite = TRUE,
                            direction = "output", accessed = TRUE)
})

test_that("contracts / scalar / () / optional / !overwrite / input / !accessed", {
    .scalar_existing_assert(OptionalInput, overwrite = FALSE,
                            direction = "input", accessed = FALSE)
})

test_that("contracts / scalar / () / optional / !overwrite / input / accessed", {
    .scalar_existing_assert(OptionalInput, overwrite = FALSE,
                            direction = "input", accessed = TRUE)
})

test_that("contracts / scalar / () / optional / !overwrite / output / !accessed", {
    .scalar_existing_assert(OptionalInput, overwrite = FALSE,
                            direction = "output", accessed = FALSE)
})

test_that("contracts / scalar / () / optional / !overwrite / output / accessed", {
    .scalar_existing_assert(OptionalInput, overwrite = FALSE,
                            direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# scalar / () / guaranteed (Julia GuaranteedOutput -> R CreatedOutput)
# ---------------------------------------------------------------------------

test_that("contracts / scalar / () / guaranteed / overwrite / input / !accessed", {
    .scalar_existing_assert(CreatedOutput, overwrite = TRUE,
                            direction = "input", accessed = FALSE)
})

test_that("contracts / scalar / () / guaranteed / overwrite / input / accessed", {
    .scalar_existing_assert(CreatedOutput, overwrite = TRUE,
                            direction = "input", accessed = TRUE)
})

test_that("contracts / scalar / () / guaranteed / overwrite / output / !accessed", {
    .scalar_existing_assert(CreatedOutput, overwrite = TRUE,
                            direction = "output", accessed = FALSE)
})

test_that("contracts / scalar / () / guaranteed / overwrite / output / accessed", {
    .scalar_existing_assert(CreatedOutput, overwrite = TRUE,
                            direction = "output", accessed = TRUE)
})

test_that("contracts / scalar / () / guaranteed / !overwrite / input / !accessed", {
    .scalar_existing_assert(CreatedOutput, overwrite = FALSE,
                            direction = "input", accessed = FALSE)
})

test_that("contracts / scalar / () / guaranteed / !overwrite / input / accessed", {
    .scalar_existing_assert(CreatedOutput, overwrite = FALSE,
                            direction = "input", accessed = TRUE)
})

test_that("contracts / scalar / () / guaranteed / !overwrite / output / !accessed", {
    .scalar_existing_assert(CreatedOutput, overwrite = FALSE,
                            direction = "output", accessed = FALSE)
})

test_that("contracts / scalar / () / guaranteed / !overwrite / output / accessed", {
    .scalar_existing_assert(CreatedOutput, overwrite = FALSE,
                            direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# scalar / () / contingent (Julia OptionalOutput -> R OptionalOutput)
# ---------------------------------------------------------------------------

test_that("contracts / scalar / () / contingent / overwrite / input / !accessed", {
    .scalar_existing_assert(OptionalOutput, overwrite = TRUE,
                            direction = "input", accessed = FALSE)
})

test_that("contracts / scalar / () / contingent / overwrite / input / accessed", {
    .scalar_existing_assert(OptionalOutput, overwrite = TRUE,
                            direction = "input", accessed = TRUE)
})

test_that("contracts / scalar / () / contingent / overwrite / output / !accessed", {
    .scalar_existing_assert(OptionalOutput, overwrite = TRUE,
                            direction = "output", accessed = FALSE)
})

test_that("contracts / scalar / () / contingent / overwrite / output / accessed", {
    .scalar_existing_assert(OptionalOutput, overwrite = TRUE,
                            direction = "output", accessed = TRUE)
})

test_that("contracts / scalar / () / contingent / !overwrite / input / !accessed", {
    .scalar_existing_assert(OptionalOutput, overwrite = FALSE,
                            direction = "input", accessed = FALSE)
})

test_that("contracts / scalar / () / contingent / !overwrite / input / accessed", {
    .scalar_existing_assert(OptionalOutput, overwrite = FALSE,
                            direction = "input", accessed = TRUE)
})

test_that("contracts / scalar / () / contingent / !overwrite / output / !accessed", {
    .scalar_existing_assert(OptionalOutput, overwrite = FALSE,
                            direction = "output", accessed = FALSE)
})

test_that("contracts / scalar / () / contingent / !overwrite / output / accessed", {
    .scalar_existing_assert(OptionalOutput, overwrite = FALSE,
                            direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# scalar / missing — scalar absent, all four expectations
# ---------------------------------------------------------------------------

test_that("contracts / scalar / missing / required / input", {
    .scalar_missing_assert(RequiredInput, direction = "input")
})

test_that("contracts / scalar / missing / required / output", {
    .scalar_missing_assert(RequiredInput, direction = "output")
})

test_that("contracts / scalar / missing / optional / input", {
    .scalar_missing_assert(OptionalInput, direction = "input")
})

test_that("contracts / scalar / missing / optional / output", {
    .scalar_missing_assert(OptionalInput, direction = "output")
})

test_that("contracts / scalar / missing / guaranteed / input", {
    .scalar_missing_assert(CreatedOutput, direction = "input")
})

test_that("contracts / scalar / missing / guaranteed / output", {
    .scalar_missing_assert(CreatedOutput, direction = "output")
})

test_that("contracts / scalar / missing / contingent / input", {
    .scalar_missing_assert(OptionalOutput, direction = "input")
})

test_that("contracts / scalar / missing / contingent / output", {
    .scalar_missing_assert(OptionalOutput, direction = "output")
})

# ---------------------------------------------------------------------------
# scalar / !type — wrong-type scalar present, all four expectations
# ---------------------------------------------------------------------------

test_that("contracts / scalar / !type / required / input", {
    .scalar_wrong_type_assert(RequiredInput, direction = "input")
})

test_that("contracts / scalar / !type / required / output", {
    .scalar_wrong_type_assert(RequiredInput, direction = "output")
})

test_that("contracts / scalar / !type / optional / input", {
    .scalar_wrong_type_assert(OptionalInput, direction = "input")
})

test_that("contracts / scalar / !type / optional / output", {
    .scalar_wrong_type_assert(OptionalInput, direction = "output")
})

test_that("contracts / scalar / !type / guaranteed / input", {
    .scalar_wrong_type_assert(CreatedOutput, direction = "input")
})

test_that("contracts / scalar / !type / guaranteed / output", {
    .scalar_wrong_type_assert(CreatedOutput, direction = "output")
})

test_that("contracts / scalar / !type / contingent / input", {
    .scalar_wrong_type_assert(OptionalOutput, direction = "input")
})

test_that("contracts / scalar / !type / contingent / output", {
    .scalar_wrong_type_assert(OptionalOutput, direction = "output")
})
