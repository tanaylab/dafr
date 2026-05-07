# Audit: contracts.jl / matrix subgroup parity divergences

Date: 2026-05-08
Driver: literal port of contracts.jl's `matrix` subgroup (lines 652-934,
54 nested_test leaves) into
`tests/testthat/test-contracts-matrix-jl-parity.R`.

Fifth sub-slice of the contracts.jl port.

## Status

- **Fixed inline:** none.
- **Open / skipped:** none.

Skip count: 0. Result on this file: `FAIL 0 | SKIP 0 | PASS 54`.
Full-suite delta: `5158 -> 5212 PASS, 49 -> 49 SKIP, 0 FAIL`.

## Notes

Matrix verify behavior was already correct in dafr after CV0 / CS1
fixes from prior slices. No new inline fixes needed.

The `!axis` block (matrix / !axis / {cell,gene} / ...) verifies that
when an axis is deleted from the daf after the contract is built,
verify_input raises "missing input axis: ..." for RequiredInput
contracts but stays silent for OptionalInput / OptionalOutput. dafr
already handles this correctly via `.verify_axis_data`.

## Test catalog

`tests/testthat/test-contracts-matrix-jl-parity.R` - 54 `test_that`
blocks:
- 32 for `matrix / ()` (4 expectations x overwrite/!overwrite x
  input/output x !accessed/accessed)
- 8 for `matrix / missing` (4 expectations x input/output)
- 10 for `matrix / !axis` (2 axes x { required/input,
  optional/{input,output}, contingent/{input,output} })
- 4 for `matrix / !type`

## Remaining contracts.jl scope

- contracts/access (Julia lines 936-1486) - tests ContractDaf access
  semantics (relaxed, empty contract, fill-with-empty_*). Multi-block.
- contracts/tensor (Julia lines 1488-1638) - tensor verify cross-product

The kickoff doc grouped lines 664-1392 as "matrix" (~80 leaves);
that range includes the access subgroup which I've broken out for
clarity. Matrix proper is 54 leaves; access is its own slice.
