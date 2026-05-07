# Audit: contracts.jl / vector subgroup parity divergences

Date: 2026-05-08
Driver: literal port of contracts.jl's `vector` subgroup (lines 414-650,
46 nested_test leaves) into
`tests/testthat/test-contracts-vector-jl-parity.R`.

Fourth sub-slice of the contracts.jl port. Sister docs:
- `2026-05-07-contracts-add-jl-parity-divergences.md`
- `2026-05-08-contracts-scalar-jl-parity-divergences.md`
- `2026-05-08-contracts-axis-jl-parity-divergences.md`

## Status

- **Fixed inline:** **CV0** (`contractor()` was missing
  construction-time validation that every axis a vector/matrix/tensor
  references is declared in the contract, and that the axis's
  expectation is compatible with the data's expectation).
- **Open / skipped:** none.

Skip count: 0. Result on this file: `FAIL 0 | SKIP 0 | PASS 46`.
Full-suite delta: `5112 -> 5158 PASS, 49 -> 49 SKIP, 0 FAIL`.

## Inline fix

### CV0. `contractor()` did not enforce axis-prerequisite at construction time

- **Symptom (pre-fix).** A `Contract` declaring a vector `("cell",
  "age")` without a matching `axes = list(cell = ...)` entry was
  silently accepted by `contractor()`. The error surfaced only later
  when code tried to read the vector and `.access_axis` raised
  "accessing non-contract axis". Julia raises the equivalent error
  ("non-contract axis: cell\nfor the RequiredInput vector: age")
  during contract construction, before any access.
  Same for incompatible-expectation axes (e.g. `OptionalInput` axis
  with `RequiredInput` vector): Julia raises
  "incompatible OptionalInput axis: cell\nfor the RequiredInput
  vector: age" at construction. dafr accepted both silently.
- **Fix.** Added two helpers
  (`.is_compatible_axis_expectation`, `.ensure_contract_axis`) and a
  per-data-record validation switch in `contractor()`. Mirrors
  Julia's `ensure_axis` / `is_compatible_axis_expectation` (DAF.jl
  contracts.jl lines 478-525). Applies to vector, matrix, and tensor
  records; tensor records check all three of `main_axis`, `rows_axis`,
  `columns_axis`.
  Compatibility rules (Julia & dafr):
  - data `OptionalInput` / `OptionalOutput`: any axis expectation OK
  - data `RequiredInput`: axis must be `RequiredInput`
  - data `CreatedOutput` / `GuaranteedOutput`: axis must be
    `RequiredInput`, `CreatedOutput`, or `GuaranteedOutput`
- **Tests unblocked.** `vector / !axis` and `vector / ~axis` (2
  leaves). Without the fix, both would have failed because
  `contractor()` succeeded silently. Existing matrix and tensor
  parity (when ported) will exercise the same code path.

## Expectation mapping (Julia -> R)

Same as scalar/axis sub-slices. CS1/CA1 lift means the full 4x2x2x2
cross-product passes without skip.

## T-class (recurring): error wording

Same as prior slices: tests use `regexp` patterns that tolerate
expectation-token differences ("CreatedOutput" vs "GuaranteedOutput")
and Julia's chomp-formatted multi-line layout vs dafr's `\n`-joined
single string.

## Test catalog

`tests/testthat/test-contracts-vector-jl-parity.R` - 46 `test_that`
blocks:
- 1 for `vector / !axis`
- 1 for `vector / ~axis`
- 32 for `vector / ()` (4 expectations x overwrite/!overwrite x
  input/output x !accessed/accessed)
- 8 for `vector / missing` (4 expectations x input/output, with
  axis expectation chosen for compatibility per Julia)
- 4 for `vector / !type` (input/required, input/optional,
  output/guaranteed, output/contingent - asymmetric vs scalar/!type)

After CV0 fix and CS1/CA1 lift: 46 PASS, 0 SKIP.

The cross-product unrolling reuses the helper-driven pattern from
scalar/axis. Helpers:
`.vector_existing_assert(expectation, overwrite, direction, accessed)`,
`.vector_missing_assert(expectation, axis_expect, direction)`,
`.vector_wrong_type_assert(expectation, direction)`.

The Julia `missing` arm uses different axis expectations per leaf
(e.g. `optional` uses `OptionalInput` axis to satisfy CV0
compatibility); the R helper takes `axis_expect` as a parameter to
match.

## Remaining contracts.jl scope

- contracts/matrix (line 652-1392, ~80 leaves; recommend split when
  porting - includes a `fill` sub-block at line 1393+ that exercises
  the `empty_*` builder API which dafr lacks - filed forward as C3)
- contracts/tensor (line 1492-1638, ~10 leaves)
