# Literal port of contracts.jl `matrix` subgroup (lines 652-934) into R.
#
# Structure:
#   - matrix / ()        : 32 leaves (4 expectations x overwrite x direction x accessed)
#   - matrix / missing   : 8 leaves (4 expectations x input/output)
#   - matrix / !axis     : 10 leaves (2 axes x { required/input,
#                          optional/{input,output}, contingent/{input,output}})
#   - matrix / !type     : 4 leaves (input{required,optional} +
#                          output{guaranteed,contingent})
# Total: 54 leaves.
#
# The Julia file has further sub-blocks under contracts/ (`access` /
# `tensor`) that are ported in their own slices. The `fill` block is
# part of `access` and exercises the `empty_*` builder API which dafr
# does not have (filed as C3); ported only to the extent that its
# leaves are testable.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.is_julia_output_expect_m <- function(expectation) {
    expectation %in% c(CreatedOutput, OptionalOutput)
}

.fresh_matrix_daf <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    d
}

.matrix_existing_assert <- function(expectation, overwrite, direction, accessed) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .fresh_matrix_daf()
        m <- matrix(c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
                    dimnames = list(c("A", "B"), c("X", "Y", "Z")))
        set_matrix(d, "cell", "gene", "UMIs", m)
        contract <- Contract(
            axes = list(
                cell = list(RequiredInput, "description"),
                gene = list(RequiredInput, "description")
            ),
            data = list(contract_matrix("cell", "gene", "UMIs",
                                        expectation, "integer", "description"))
        )
        cd <- contractor("computation", contract, d, overwrite = overwrite)
        stopifnot(axis_length(cd, "cell") == 2L,
                  axis_length(cd, "gene") == 3L)
        fn <- if (direction == "input") verify_input else verify_output
        if (accessed) {
            stopifnot(all(unname(get_matrix(cd, "cell", "gene", "UMIs")) ==
                          unname(m)))
            if (!overwrite && .is_julia_output_expect_m(expectation)) {
                expect_error(verify_input(cd),
                    regexp = "pre-existing.*matrix.*UMIs")
            } else {
                expect_silent(fn(cd))
            }
        } else {
            if (direction == "output" && expectation == RequiredInput) {
                expect_error(fn(cd),
                    regexp = "unused RequiredInput matrix.*UMIs")
            } else if (!overwrite && .is_julia_output_expect_m(expectation)) {
                expect_error(verify_input(cd),
                    regexp = "pre-existing.*matrix.*UMIs")
            } else {
                expect_silent(fn(cd))
            }
        }
    })
}

.matrix_missing_assert <- function(expectation, axis_expect, direction) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .fresh_matrix_daf()
        contract <- Contract(
            axes = list(
                cell = list(axis_expect, "description"),
                gene = list(axis_expect, "description")
            ),
            data = list(contract_matrix("cell", "gene", "UMIs",
                                        expectation, "integer", "description"))
        )
        cd <- contractor("computation", contract, d)
        # axis access pre-marks both axes as accessed for verify_output's
        # unused-RequiredInput axis check (matches Julia).
        stopifnot(axis_length(cd, "cell") == 2L,
                  axis_length(cd, "gene") == 3L)
        fn <- if (direction == "input") verify_input else verify_output
        if (direction == "input" && expectation == RequiredInput) {
            expect_error(fn(cd),
                regexp = "missing input matrix.*UMIs")
        } else if (direction == "output" && expectation == CreatedOutput) {
            expect_error(fn(cd),
                regexp = "missing output matrix.*UMIs")
        } else {
            expect_silent(fn(cd))
        }
    })
}

# Helper for !axis: deletes the named axis, builds a contract with the
# specified expectation; tests the requested direction. Julia's loop:
# - required: only input arm
# - optional: input AND output arms
# - contingent: input AND output arms
.matrix_no_axis_assert <- function(deleted_axis, expectation, axis_expect, direction) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .fresh_matrix_daf()
        delete_axis(d, deleted_axis)
        contract <- Contract(
            axes = list(
                cell = list(axis_expect, "description"),
                gene = list(axis_expect, "description")
            ),
            data = list(contract_matrix("cell", "gene", "UMIs",
                                        expectation, "integer", "description"))
        )
        cd <- contractor("computation", contract, d)
        fn <- if (direction == "input") verify_input else verify_output
        if (direction == "input" && expectation == RequiredInput) {
            expect_error(fn(cd),
                regexp = sprintf("missing input axis.*%s", deleted_axis))
        } else {
            expect_silent(fn(cd))
        }
    })
}

.matrix_wrong_type_assert <- function(expectation, direction) {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .fresh_matrix_daf()
        # Set as numeric (double), contract will declare integer
        m <- matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
                    dimnames = list(c("A", "B"), c("X", "Y", "Z")))
        set_matrix(d, "cell", "gene", "UMIs", m)
        contract <- Contract(
            axes = list(
                cell = list(RequiredInput, "description"),
                gene = list(RequiredInput, "description")
            ),
            data = list(contract_matrix("cell", "gene", "UMIs",
                                        expectation, "integer", "description"))
        )
        cd <- contractor("computation", contract, d)
        fn <- if (direction == "input") verify_input else verify_output
        expect_error(fn(cd),
            regexp = sprintf("unexpected type.*%s matrix.*UMIs", direction))
    })
}

# ---------------------------------------------------------------------------
# matrix / () cross-product (32 leaves)
# ---------------------------------------------------------------------------

# Macro-style test_that emitter via for-loops would lose readable names;
# unroll explicitly to mirror the chains/scalar/axis/vector pattern.

# required (RequiredInput)
test_that("contracts / matrix / () / required / overwrite / input / !accessed", {
    .matrix_existing_assert(RequiredInput, TRUE, "input", FALSE)
})
test_that("contracts / matrix / () / required / overwrite / input / accessed", {
    .matrix_existing_assert(RequiredInput, TRUE, "input", TRUE)
})
test_that("contracts / matrix / () / required / overwrite / output / !accessed", {
    .matrix_existing_assert(RequiredInput, TRUE, "output", FALSE)
})
test_that("contracts / matrix / () / required / overwrite / output / accessed", {
    .matrix_existing_assert(RequiredInput, TRUE, "output", TRUE)
})
test_that("contracts / matrix / () / required / !overwrite / input / !accessed", {
    .matrix_existing_assert(RequiredInput, FALSE, "input", FALSE)
})
test_that("contracts / matrix / () / required / !overwrite / input / accessed", {
    .matrix_existing_assert(RequiredInput, FALSE, "input", TRUE)
})
test_that("contracts / matrix / () / required / !overwrite / output / !accessed", {
    .matrix_existing_assert(RequiredInput, FALSE, "output", FALSE)
})
test_that("contracts / matrix / () / required / !overwrite / output / accessed", {
    .matrix_existing_assert(RequiredInput, FALSE, "output", TRUE)
})

# optional (OptionalInput)
test_that("contracts / matrix / () / optional / overwrite / input / !accessed", {
    .matrix_existing_assert(OptionalInput, TRUE, "input", FALSE)
})
test_that("contracts / matrix / () / optional / overwrite / input / accessed", {
    .matrix_existing_assert(OptionalInput, TRUE, "input", TRUE)
})
test_that("contracts / matrix / () / optional / overwrite / output / !accessed", {
    .matrix_existing_assert(OptionalInput, TRUE, "output", FALSE)
})
test_that("contracts / matrix / () / optional / overwrite / output / accessed", {
    .matrix_existing_assert(OptionalInput, TRUE, "output", TRUE)
})
test_that("contracts / matrix / () / optional / !overwrite / input / !accessed", {
    .matrix_existing_assert(OptionalInput, FALSE, "input", FALSE)
})
test_that("contracts / matrix / () / optional / !overwrite / input / accessed", {
    .matrix_existing_assert(OptionalInput, FALSE, "input", TRUE)
})
test_that("contracts / matrix / () / optional / !overwrite / output / !accessed", {
    .matrix_existing_assert(OptionalInput, FALSE, "output", FALSE)
})
test_that("contracts / matrix / () / optional / !overwrite / output / accessed", {
    .matrix_existing_assert(OptionalInput, FALSE, "output", TRUE)
})

# guaranteed (Julia GuaranteedOutput -> R CreatedOutput)
test_that("contracts / matrix / () / guaranteed / overwrite / input / !accessed", {
    .matrix_existing_assert(CreatedOutput, TRUE, "input", FALSE)
})
test_that("contracts / matrix / () / guaranteed / overwrite / input / accessed", {
    .matrix_existing_assert(CreatedOutput, TRUE, "input", TRUE)
})
test_that("contracts / matrix / () / guaranteed / overwrite / output / !accessed", {
    .matrix_existing_assert(CreatedOutput, TRUE, "output", FALSE)
})
test_that("contracts / matrix / () / guaranteed / overwrite / output / accessed", {
    .matrix_existing_assert(CreatedOutput, TRUE, "output", TRUE)
})
test_that("contracts / matrix / () / guaranteed / !overwrite / input / !accessed", {
    .matrix_existing_assert(CreatedOutput, FALSE, "input", FALSE)
})
test_that("contracts / matrix / () / guaranteed / !overwrite / input / accessed", {
    .matrix_existing_assert(CreatedOutput, FALSE, "input", TRUE)
})
test_that("contracts / matrix / () / guaranteed / !overwrite / output / !accessed", {
    .matrix_existing_assert(CreatedOutput, FALSE, "output", FALSE)
})
test_that("contracts / matrix / () / guaranteed / !overwrite / output / accessed", {
    .matrix_existing_assert(CreatedOutput, FALSE, "output", TRUE)
})

# contingent (Julia OptionalOutput -> R OptionalOutput)
test_that("contracts / matrix / () / contingent / overwrite / input / !accessed", {
    .matrix_existing_assert(OptionalOutput, TRUE, "input", FALSE)
})
test_that("contracts / matrix / () / contingent / overwrite / input / accessed", {
    .matrix_existing_assert(OptionalOutput, TRUE, "input", TRUE)
})
test_that("contracts / matrix / () / contingent / overwrite / output / !accessed", {
    .matrix_existing_assert(OptionalOutput, TRUE, "output", FALSE)
})
test_that("contracts / matrix / () / contingent / overwrite / output / accessed", {
    .matrix_existing_assert(OptionalOutput, TRUE, "output", TRUE)
})
test_that("contracts / matrix / () / contingent / !overwrite / input / !accessed", {
    .matrix_existing_assert(OptionalOutput, FALSE, "input", FALSE)
})
test_that("contracts / matrix / () / contingent / !overwrite / input / accessed", {
    .matrix_existing_assert(OptionalOutput, FALSE, "input", TRUE)
})
test_that("contracts / matrix / () / contingent / !overwrite / output / !accessed", {
    .matrix_existing_assert(OptionalOutput, FALSE, "output", FALSE)
})
test_that("contracts / matrix / () / contingent / !overwrite / output / accessed", {
    .matrix_existing_assert(OptionalOutput, FALSE, "output", TRUE)
})

# ---------------------------------------------------------------------------
# matrix / missing
# ---------------------------------------------------------------------------

test_that("contracts / matrix / missing / required / input", {
    .matrix_missing_assert(RequiredInput, RequiredInput, "input")
})
test_that("contracts / matrix / missing / required / output", {
    .matrix_missing_assert(RequiredInput, RequiredInput, "output")
})
test_that("contracts / matrix / missing / optional / input", {
    .matrix_missing_assert(OptionalInput, OptionalInput, "input")
})
test_that("contracts / matrix / missing / optional / output", {
    .matrix_missing_assert(OptionalInput, OptionalInput, "output")
})
test_that("contracts / matrix / missing / guaranteed / input", {
    .matrix_missing_assert(CreatedOutput, RequiredInput, "input")
})
test_that("contracts / matrix / missing / guaranteed / output", {
    .matrix_missing_assert(CreatedOutput, RequiredInput, "output")
})
test_that("contracts / matrix / missing / contingent / input", {
    .matrix_missing_assert(OptionalOutput, OptionalInput, "input")
})
test_that("contracts / matrix / missing / contingent / output", {
    .matrix_missing_assert(OptionalOutput, OptionalInput, "output")
})

# ---------------------------------------------------------------------------
# matrix / !axis - axis deleted before contract verification
# Julia loops over both axes (cell, gene). Per-axis sub-blocks:
# - required: only input
# - optional: input + output
# - contingent: input + output
# (no guaranteed sub-block per Julia)
# ---------------------------------------------------------------------------

for (.ax in c("cell", "gene")) local({
    ax <- .ax
    test_that(sprintf("contracts / matrix / !axis / %s / required / input", ax), {
        .matrix_no_axis_assert(ax, RequiredInput, RequiredInput, "input")
    })
    test_that(sprintf("contracts / matrix / !axis / %s / optional / input", ax), {
        .matrix_no_axis_assert(ax, OptionalInput, OptionalInput, "input")
    })
    test_that(sprintf("contracts / matrix / !axis / %s / optional / output", ax), {
        .matrix_no_axis_assert(ax, OptionalInput, OptionalInput, "output")
    })
    test_that(sprintf("contracts / matrix / !axis / %s / contingent / input", ax), {
        .matrix_no_axis_assert(ax, OptionalOutput, OptionalInput, "input")
    })
    test_that(sprintf("contracts / matrix / !axis / %s / contingent / output", ax), {
        .matrix_no_axis_assert(ax, OptionalOutput, OptionalInput, "output")
    })
})

# ---------------------------------------------------------------------------
# matrix / !type - type mismatch (set numeric, declare integer)
# ---------------------------------------------------------------------------

test_that("contracts / matrix / !type / input / required", {
    .matrix_wrong_type_assert(RequiredInput, "input")
})
test_that("contracts / matrix / !type / input / optional", {
    .matrix_wrong_type_assert(OptionalInput, "input")
})
test_that("contracts / matrix / !type / output / guaranteed", {
    .matrix_wrong_type_assert(CreatedOutput, "output")
})
test_that("contracts / matrix / !type / output / contingent", {
    .matrix_wrong_type_assert(OptionalOutput, "output")
})
