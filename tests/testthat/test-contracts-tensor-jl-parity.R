# Literal port of contracts.jl `tensor` subgroup (lines 1488-1635) into R.
#
# Tensor contract = per-entry matrices keyed by a main axis. For each
# entry e of the main axis, a matrix `<e>_<name>` must exist on
# (rows_axis, columns_axis).
#
# 8 leaves:
#   - tensor / input / ()           - both per-entry matrices present
#   - tensor / input / missing      - one per-entry matrix missing
#   - tensor / output / ()          - GuaranteedOutput, both set
#   - tensor / output / missing     - GuaranteedOutput, V_is_high missing
#   - tensor / !axis / guaranteed / ()         - missing batch axis
#   - tensor / !axis / guaranteed / add / ()   - axis added, no matrices
#   - tensor / !axis / guaranteed / add / create - both matrices created
#   - tensor / !axis / optional     - OptionalOutput tensor + missing axis

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.tensor_fresh_daf <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    add_axis(d, "batch", c("U", "V"))
    d
}

# Julia stores `gene x cell` matrices [3 x 2 with the test data]. dafr's
# parity uses (rows=gene, columns=cell) to match. Both U_is_high and
# V_is_high are 3x2 logical matrices.
.U_matrix <- function() {
    matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), nrow = 3L, ncol = 2L,
           dimnames = list(c("X", "Y", "Z"), c("A", "B")))
}
.V_matrix <- function() {
    matrix(c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE), nrow = 3L, ncol = 2L,
           dimnames = list(c("X", "Y", "Z"), c("A", "B")))
}

.tensor_input_contract <- function() {
    Contract(
        axes = list(
            gene  = list(RequiredInput, "description"),
            cell  = list(RequiredInput, "description"),
            batch = list(RequiredInput, "description")
        ),
        data = list(tensor_contract("batch", "cell", "gene",
                                     "is_high", RequiredInput,
                                     "logical", "description"))
    )
}

.tensor_output_contract <- function() {
    Contract(
        axes = list(
            gene  = list(RequiredInput, "description"),
            cell  = list(RequiredInput, "description"),
            batch = list(RequiredInput, "description")
        ),
        data = list(tensor_contract("batch", "cell", "gene",
                                     "is_high", CreatedOutput,
                                     "logical", "description"))
    )
}

# ---------------------------------------------------------------------------
# tensor / input / ()
# ---------------------------------------------------------------------------

test_that("contracts / tensor / input / ()", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .tensor_fresh_daf()
        set_matrix(d, "gene", "cell", "U_is_high", .U_matrix())
        set_matrix(d, "gene", "cell", "V_is_high", .V_matrix())
        cd <- contractor("computation", .tensor_input_contract(), d)
        # Read at least one entry to mark the tensor as accessed.
        invisible(get_matrix(cd, "gene", "cell", "U_is_high"))
        expect_silent(verify_input(cd))
        expect_silent(verify_output(cd))
    })
})

# ---------------------------------------------------------------------------
# tensor / input / missing
# ---------------------------------------------------------------------------

test_that("contracts / tensor / input / missing", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .tensor_fresh_daf()
        set_matrix(d, "gene", "cell", "U_is_high", .U_matrix())
        # V_is_high deliberately missing.
        cd <- contractor("computation", .tensor_input_contract(), d)
        invisible(get_matrix(cd, "gene", "cell", "U_is_high"))
        expect_error(verify_input(cd),
            regexp = "missing.*V_is_high|V_is_high.*missing")
    })
})

# ---------------------------------------------------------------------------
# tensor / output / ()
# ---------------------------------------------------------------------------

test_that("contracts / tensor / output / ()", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .tensor_fresh_daf()
        cd <- contractor("computation", .tensor_output_contract(), d)
        expect_silent(verify_input(cd))
        # set both per-entry matrices through the contracted daf
        set_matrix(cd, "gene", "cell", "U_is_high", .U_matrix())
        set_matrix(cd, "gene", "cell", "V_is_high", .V_matrix())
        expect_silent(verify_output(cd))
    })
})

# ---------------------------------------------------------------------------
# tensor / output / missing
# ---------------------------------------------------------------------------

test_that("contracts / tensor / output / missing", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .tensor_fresh_daf()
        cd <- contractor("computation", .tensor_output_contract(), d)
        expect_silent(verify_input(cd))
        set_matrix(cd, "gene", "cell", "U_is_high", .U_matrix())
        # V_is_high deliberately missing.
        expect_error(verify_output(cd),
            regexp = "missing.*V_is_high|V_is_high.*missing")
    })
})

# ---------------------------------------------------------------------------
# tensor / !axis / guaranteed / ()
# ---------------------------------------------------------------------------

test_that("contracts / tensor / !axis / guaranteed / ()", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .tensor_fresh_daf()
        delete_axis(d, "batch")
        contract <- Contract(
            axes = list(
                gene  = list(RequiredInput, "description"),
                cell  = list(RequiredInput, "description"),
                batch = list(CreatedOutput, "description")
            ),
            data = list(tensor_contract("batch", "cell", "gene",
                                         "is_high", CreatedOutput,
                                         "logical", "description"))
        )
        cd <- contractor("computation", contract, d)
        expect_silent(verify_input(cd))
        expect_error(verify_output(cd),
            regexp = "missing output axis.*batch")
    })
})

# ---------------------------------------------------------------------------
# tensor / !axis / guaranteed / add / ()
# ---------------------------------------------------------------------------

test_that("contracts / tensor / !axis / guaranteed / add / ()", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .tensor_fresh_daf()
        delete_axis(d, "batch")
        contract <- Contract(
            axes = list(
                gene  = list(RequiredInput, "description"),
                cell  = list(RequiredInput, "description"),
                batch = list(CreatedOutput, "description")
            ),
            data = list(tensor_contract("batch", "cell", "gene",
                                         "is_high", CreatedOutput,
                                         "logical", "description"))
        )
        cd <- contractor("computation", contract, d)
        expect_silent(verify_input(cd))
        # Add the batch axis but no matrices yet
        add_axis(cd, "batch", c("U", "V"))
        expect_error(verify_output(cd),
            regexp = "missing output.*U_is_high|U_is_high.*missing")
    })
})

# ---------------------------------------------------------------------------
# tensor / !axis / guaranteed / add / create
# ---------------------------------------------------------------------------

test_that("contracts / tensor / !axis / guaranteed / add / create", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .tensor_fresh_daf()
        delete_axis(d, "batch")
        contract <- Contract(
            axes = list(
                gene  = list(RequiredInput, "description"),
                cell  = list(RequiredInput, "description"),
                batch = list(CreatedOutput, "description")
            ),
            data = list(tensor_contract("batch", "cell", "gene",
                                         "is_high", CreatedOutput,
                                         "logical", "description"))
        )
        cd <- contractor("computation", contract, d)
        expect_silent(verify_input(cd))
        add_axis(cd, "batch", c("U", "V"))
        set_matrix(cd, "gene", "cell", "U_is_high", .U_matrix())
        set_matrix(cd, "gene", "cell", "V_is_high", .V_matrix())
        expect_silent(verify_output(cd))
    })
})

# ---------------------------------------------------------------------------
# tensor / !axis / optional
# ---------------------------------------------------------------------------

test_that("contracts / tensor / !axis / optional", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .tensor_fresh_daf()
        delete_axis(d, "batch")
        contract <- Contract(
            axes = list(
                gene  = list(OptionalInput, "description"),
                cell  = list(OptionalInput, "description"),
                batch = list(OptionalOutput, "description")
            ),
            data = list(tensor_contract("batch", "cell", "gene",
                                         "is_high", OptionalOutput,
                                         "logical", "description"))
        )
        cd <- contractor("computation", contract, d)
        expect_silent(verify_input(cd))
        expect_silent(verify_output(cd))
    })
})
