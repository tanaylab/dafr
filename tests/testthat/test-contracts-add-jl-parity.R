# Literal port of contracts.jl `add` subgroup (lines 8-114) into R.
#
# The `add` group tests Julia's `Contract |> Contract` composition
# operator (dafr equivalent: merge_contracts). Each leaf checks one
# (left, right) expectation pair and the resulting merged spec.
#
# The full contracts.jl file is 1639 lines / ~188 nested_tests. This
# slice covers only the add subgroup; scalar/axis/vector/matrix/tensor
# subgroups are deferred to subsequent slices (see kickoff doc's
# Slice C1/C2 split note for context).
#
# Documented divergences carry skip("R divergence: <id>: <reason>")
# from dev/notes/2026-05-07-contracts-add-jl-parity-divergences.md.

# ---------------------------------------------------------------------------
# contracts / add / axes
# ---------------------------------------------------------------------------

test_that("contracts / add / axes / left", {
    # Left has cell, right is empty -> result has cell.
    left <- Contract(axes = list(cell = list(RequiredInput, "description")))
    right <- Contract()
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], RequiredInput)
    expect_identical(merged@axes$cell[[2L]], "description")

    # Same with explicit empty axes list.
    right <- Contract(axes = list())
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], RequiredInput)
})

test_that("contracts / add / axes / right", {
    # Empty left, right has cell -> result has cell.
    left <- Contract()
    right <- Contract(axes = list(cell = list(RequiredInput, "description")))
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], RequiredInput)
    expect_identical(merged@axes$cell[[2L]], "description")

    left <- Contract(axes = list())
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], RequiredInput)
})

test_that("contracts / add / axes / required-required", {
    left <- Contract(axes = list(cell = list(RequiredInput, "description")))
    right <- Contract(axes = list(cell = list(RequiredInput, "description")))
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], RequiredInput)
})

test_that("contracts / add / axes / required-optional", {
    left <- Contract(axes = list(cell = list(RequiredInput, "description")))
    right <- Contract(axes = list(cell = list(OptionalInput, "description")))
    merged <- merge_contracts(left, right)
    # Required wins over Optional.
    expect_identical(merged@axes$cell[[1L]], RequiredInput)
})

test_that("contracts / add / axes / optional-required", {
    left <- Contract(axes = list(cell = list(OptionalInput, "description")))
    right <- Contract(axes = list(cell = list(RequiredInput, "description")))
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], RequiredInput)
})

test_that("contracts / add / axes / created-required", {
    left <- Contract(axes = list(cell = list(CreatedOutput, "description")))
    right <- Contract(axes = list(cell = list(RequiredInput, "description")))
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], CreatedOutput)
})

test_that("contracts / add / axes / created-optional", {
    left <- Contract(axes = list(cell = list(CreatedOutput, "description")))
    right <- Contract(axes = list(cell = list(OptionalInput, "description")))
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], CreatedOutput)
})

test_that("contracts / add / axes / guaranteed-required", {
    left <- Contract(axes = list(cell = list(GuaranteedOutput, "description")))
    right <- Contract(axes = list(cell = list(RequiredInput, "description")))
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], GuaranteedOutput)
})

test_that("contracts / add / axes / guaranteed-optional", {
    left <- Contract(axes = list(cell = list(GuaranteedOutput, "description")))
    right <- Contract(axes = list(cell = list(OptionalInput, "description")))
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], GuaranteedOutput)
})

test_that("contracts / add / axes / contingent-optional", {
    left <- Contract(axes = list(cell = list(OptionalOutput, "description")))
    right <- Contract(axes = list(cell = list(OptionalInput, "description")))
    merged <- merge_contracts(left, right)
    expect_identical(merged@axes$cell[[1L]], OptionalOutput)
})

test_that("contracts / add / axes / incompatible", {
    left <- Contract(axes = list(cell = list(OptionalOutput, "description")))
    right <- Contract(axes = list(cell = list(OptionalOutput, "description")))
    expect_error(
        merge_contracts(left, right),
        regexp = "incompatible expectation.*OptionalOutput|OptionalOutput.*incompatible"
    )
})

# ---------------------------------------------------------------------------
# contracts / add / data
# ---------------------------------------------------------------------------
#
# Julia tests use Int32 / Int64 / Integer to exercise type-narrowing
# semantics. R has only "integer" / "numeric" / "double" — no signed-
# width hierarchy. dafr's .merge_types uses
#   logical < integer < double < numeric < character
# (narrower wins; mismatch errors). The R analogue translation:
#   Julia Int32  -> R "integer"
#   Julia Integer (abstract) -> R "numeric" (the wider sibling)
#   Julia incompatible (Int64 vs Int32) -> R "integer" vs "character"
#     (no width-order relationship).

test_that("contracts / add / data / int32-int32", {
    # Both same narrower type -> result narrower.
    left  <- Contract(data = list(
        contract_vector("cell", "age", RequiredInput, "integer", "description")
    ))
    right <- Contract(data = list(
        contract_vector("cell", "age", RequiredInput, "integer", "description")
    ))
    merged <- merge_contracts(left, right)
    age <- merged@data[[1L]]
    expect_identical(age$type, "integer")
    expect_identical(age$expectation, RequiredInput)
})

test_that("contracts / add / data / int32-integer", {
    # Narrower vs wider -> narrower wins.
    left  <- Contract(data = list(
        contract_vector("cell", "age", RequiredInput, "integer", "description")
    ))
    right <- Contract(data = list(
        contract_vector("cell", "age", RequiredInput, "numeric", "description")
    ))
    merged <- merge_contracts(left, right)
    expect_identical(merged@data[[1L]]$type, "integer")
})

test_that("contracts / add / data / integer-int32", {
    # Wider vs narrower -> narrower wins (commutative).
    left  <- Contract(data = list(
        contract_vector("cell", "age", RequiredInput, "numeric", "description")
    ))
    right <- Contract(data = list(
        contract_vector("cell", "age", RequiredInput, "integer", "description")
    ))
    merged <- merge_contracts(left, right)
    expect_identical(merged@data[[1L]]$type, "integer")
})

test_that("contracts / add / data / incompatible", {
    # Julia rejects Int64-vs-Int32 because they're sibling types in
    # the type lattice. R's analogue: `character` is a sibling to the
    # numeric chain; merging `character` with `integer` errors with
    # the same `incompatible type:` message Julia uses.
    left <- Contract(
        axes = list(cell = list(RequiredInput, "axis")),
        data = list(contract_vector("cell", "x", RequiredInput,
                                    "integer", "x"))
    )
    right <- Contract(
        axes = list(cell = list(RequiredInput, "axis")),
        data = list(contract_vector("cell", "x", RequiredInput,
                                    "character", "x"))
    )
    expect_error(merge_contracts(left, right),
                 regexp = "incompatible type: integer.*type: character")
})
