# Literal port of contracts.jl `axis` subgroup (lines 274-411) into R.
#
# Same shape as the scalar block: cross-product of 4 expectations x
# overwrite/!overwrite x input/output x !accessed/accessed = 32 leaves
# for `axis / ()`, plus 8 for `axis / missing`. No `!type` block (axes
# don't carry a type).
#
# Expectation mapping (Julia -> R), identical to the scalar slice:
#   required    -> RequiredInput
#   optional    -> OptionalInput
#   guaranteed  -> CreatedOutput   (R analogue of Julia GuaranteedOutput)
#   contingent  -> OptionalOutput  (Julia OptionalOutput. CA1 lifted
#                                   2026-05-08 alongside CS1.)
#
# Divergence note: dev/notes/2026-05-08-contracts-axis-jl-parity-divergences.md.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.is_julia_output_expect_axis <- function(expectation) {
    expectation %in% c(CreatedOutput, OptionalOutput)
}

.axis_existing_assert <- function(expectation, overwrite, direction, accessed) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        contract <- Contract(axes = list(
            cell = list(expectation, "description")
        ))
        cd <- contractor("computation", contract, d, overwrite = overwrite)
        fn <- if (direction == "input") verify_input else verify_output
        if (accessed) {
            stopifnot(axis_length(cd, "cell") == 2L)
            if (!overwrite && .is_julia_output_expect_axis(expectation)) {
                expect_error(verify_input(cd),
                    regexp = "pre-existing.*axis.*cell")
            } else {
                expect_silent(fn(cd))
            }
        } else {
            if (direction == "output" && expectation == RequiredInput) {
                expect_error(fn(cd),
                    regexp = "unused RequiredInput axis.*cell")
            } else if (!overwrite && .is_julia_output_expect_axis(expectation)) {
                expect_error(verify_input(cd),
                    regexp = "pre-existing.*axis.*cell")
            } else {
                expect_silent(fn(cd))
            }
        }
    })
}

.axis_missing_assert <- function(expectation, direction) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        contract <- Contract(axes = list(
            cell = list(expectation, "description")
        ))
        cd <- contractor("computation", contract, d)
        fn <- if (direction == "input") verify_input else verify_output
        if (direction == "input" && expectation == RequiredInput) {
            expect_error(fn(cd),
                regexp = "missing input axis.*cell")
        } else if (direction == "output" && expectation == CreatedOutput) {
            expect_error(fn(cd),
                regexp = "missing output axis.*cell")
        } else {
            expect_silent(fn(cd))
        }
    })
}

# ---------------------------------------------------------------------------
# axis / () / required (RequiredInput)
# ---------------------------------------------------------------------------

test_that("contracts / axis / () / required / overwrite / input / !accessed", {
    .axis_existing_assert(RequiredInput, overwrite = TRUE,
                          direction = "input", accessed = FALSE)
})

test_that("contracts / axis / () / required / overwrite / input / accessed", {
    .axis_existing_assert(RequiredInput, overwrite = TRUE,
                          direction = "input", accessed = TRUE)
})

test_that("contracts / axis / () / required / overwrite / output / !accessed", {
    .axis_existing_assert(RequiredInput, overwrite = TRUE,
                          direction = "output", accessed = FALSE)
})

test_that("contracts / axis / () / required / overwrite / output / accessed", {
    .axis_existing_assert(RequiredInput, overwrite = TRUE,
                          direction = "output", accessed = TRUE)
})

test_that("contracts / axis / () / required / !overwrite / input / !accessed", {
    .axis_existing_assert(RequiredInput, overwrite = FALSE,
                          direction = "input", accessed = FALSE)
})

test_that("contracts / axis / () / required / !overwrite / input / accessed", {
    .axis_existing_assert(RequiredInput, overwrite = FALSE,
                          direction = "input", accessed = TRUE)
})

test_that("contracts / axis / () / required / !overwrite / output / !accessed", {
    .axis_existing_assert(RequiredInput, overwrite = FALSE,
                          direction = "output", accessed = FALSE)
})

test_that("contracts / axis / () / required / !overwrite / output / accessed", {
    .axis_existing_assert(RequiredInput, overwrite = FALSE,
                          direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# axis / () / optional (OptionalInput)
# ---------------------------------------------------------------------------

test_that("contracts / axis / () / optional / overwrite / input / !accessed", {
    .axis_existing_assert(OptionalInput, overwrite = TRUE,
                          direction = "input", accessed = FALSE)
})

test_that("contracts / axis / () / optional / overwrite / input / accessed", {
    .axis_existing_assert(OptionalInput, overwrite = TRUE,
                          direction = "input", accessed = TRUE)
})

test_that("contracts / axis / () / optional / overwrite / output / !accessed", {
    .axis_existing_assert(OptionalInput, overwrite = TRUE,
                          direction = "output", accessed = FALSE)
})

test_that("contracts / axis / () / optional / overwrite / output / accessed", {
    .axis_existing_assert(OptionalInput, overwrite = TRUE,
                          direction = "output", accessed = TRUE)
})

test_that("contracts / axis / () / optional / !overwrite / input / !accessed", {
    .axis_existing_assert(OptionalInput, overwrite = FALSE,
                          direction = "input", accessed = FALSE)
})

test_that("contracts / axis / () / optional / !overwrite / input / accessed", {
    .axis_existing_assert(OptionalInput, overwrite = FALSE,
                          direction = "input", accessed = TRUE)
})

test_that("contracts / axis / () / optional / !overwrite / output / !accessed", {
    .axis_existing_assert(OptionalInput, overwrite = FALSE,
                          direction = "output", accessed = FALSE)
})

test_that("contracts / axis / () / optional / !overwrite / output / accessed", {
    .axis_existing_assert(OptionalInput, overwrite = FALSE,
                          direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# axis / () / guaranteed (Julia GuaranteedOutput -> R CreatedOutput)
# ---------------------------------------------------------------------------

test_that("contracts / axis / () / guaranteed / overwrite / input / !accessed", {
    .axis_existing_assert(CreatedOutput, overwrite = TRUE,
                          direction = "input", accessed = FALSE)
})

test_that("contracts / axis / () / guaranteed / overwrite / input / accessed", {
    .axis_existing_assert(CreatedOutput, overwrite = TRUE,
                          direction = "input", accessed = TRUE)
})

test_that("contracts / axis / () / guaranteed / overwrite / output / !accessed", {
    .axis_existing_assert(CreatedOutput, overwrite = TRUE,
                          direction = "output", accessed = FALSE)
})

test_that("contracts / axis / () / guaranteed / overwrite / output / accessed", {
    .axis_existing_assert(CreatedOutput, overwrite = TRUE,
                          direction = "output", accessed = TRUE)
})

test_that("contracts / axis / () / guaranteed / !overwrite / input / !accessed", {
    .axis_existing_assert(CreatedOutput, overwrite = FALSE,
                          direction = "input", accessed = FALSE)
})

test_that("contracts / axis / () / guaranteed / !overwrite / input / accessed", {
    .axis_existing_assert(CreatedOutput, overwrite = FALSE,
                          direction = "input", accessed = TRUE)
})

test_that("contracts / axis / () / guaranteed / !overwrite / output / !accessed", {
    .axis_existing_assert(CreatedOutput, overwrite = FALSE,
                          direction = "output", accessed = FALSE)
})

test_that("contracts / axis / () / guaranteed / !overwrite / output / accessed", {
    .axis_existing_assert(CreatedOutput, overwrite = FALSE,
                          direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# axis / () / contingent (Julia OptionalOutput -> R OptionalOutput)
# ---------------------------------------------------------------------------

test_that("contracts / axis / () / contingent / overwrite / input / !accessed", {
    .axis_existing_assert(OptionalOutput, overwrite = TRUE,
                          direction = "input", accessed = FALSE)
})

test_that("contracts / axis / () / contingent / overwrite / input / accessed", {
    .axis_existing_assert(OptionalOutput, overwrite = TRUE,
                          direction = "input", accessed = TRUE)
})

test_that("contracts / axis / () / contingent / overwrite / output / !accessed", {
    .axis_existing_assert(OptionalOutput, overwrite = TRUE,
                          direction = "output", accessed = FALSE)
})

test_that("contracts / axis / () / contingent / overwrite / output / accessed", {
    .axis_existing_assert(OptionalOutput, overwrite = TRUE,
                          direction = "output", accessed = TRUE)
})

test_that("contracts / axis / () / contingent / !overwrite / input / !accessed", {
    .axis_existing_assert(OptionalOutput, overwrite = FALSE,
                          direction = "input", accessed = FALSE)
})

test_that("contracts / axis / () / contingent / !overwrite / input / accessed", {
    .axis_existing_assert(OptionalOutput, overwrite = FALSE,
                          direction = "input", accessed = TRUE)
})

test_that("contracts / axis / () / contingent / !overwrite / output / !accessed", {
    .axis_existing_assert(OptionalOutput, overwrite = FALSE,
                          direction = "output", accessed = FALSE)
})

test_that("contracts / axis / () / contingent / !overwrite / output / accessed", {
    .axis_existing_assert(OptionalOutput, overwrite = FALSE,
                          direction = "output", accessed = TRUE)
})

# ---------------------------------------------------------------------------
# axis / missing — axis absent, all four expectations
# ---------------------------------------------------------------------------

test_that("contracts / axis / missing / required / input", {
    .axis_missing_assert(RequiredInput, direction = "input")
})

test_that("contracts / axis / missing / required / output", {
    .axis_missing_assert(RequiredInput, direction = "output")
})

test_that("contracts / axis / missing / optional / input", {
    .axis_missing_assert(OptionalInput, direction = "input")
})

test_that("contracts / axis / missing / optional / output", {
    .axis_missing_assert(OptionalInput, direction = "output")
})

test_that("contracts / axis / missing / guaranteed / input", {
    .axis_missing_assert(CreatedOutput, direction = "input")
})

test_that("contracts / axis / missing / guaranteed / output", {
    .axis_missing_assert(CreatedOutput, direction = "output")
})

test_that("contracts / axis / missing / contingent / input", {
    .axis_missing_assert(OptionalOutput, direction = "input")
})

test_that("contracts / axis / missing / contingent / output", {
    .axis_missing_assert(OptionalOutput, direction = "output")
})
