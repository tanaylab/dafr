# Audit: contracts.jl / scalar subgroup parity divergences

Date: 2026-05-08
Driver: literal port of contracts.jl's `scalar` subgroup (lines 105-272,
48 nested_test leaves) into
`tests/testthat/test-contracts-scalar-jl-parity.R`.

This is the **second sub-slice** of the contracts.jl port. Sister doc:
`2026-05-07-contracts-add-jl-parity-divergences.md`. Per the kickoff
the full file splits into 6 sub-slices; remaining are axis, vector,
matrix (recommend further split), and tensor.

## Status

- **Fixed inline:** none.
- **Open / skipped:** **CS1** (R `OptionalOutput` enum is not enforced
  as forbidden-on-input). All four `contingent / !overwrite` cells of
  the `scalar / ()` cross-product hit it; 4 skips total.

Skip count: 4. Result on this file: `FAIL 0 | SKIP 4 | PASS 44`.
Full-suite delta: `5024 -> 5068 PASS, 49 -> 53 SKIP, 0 FAIL`.

## Expectation mapping (Julia -> R)

The Julia test loops over four expectations; the R port maps them like
this:

| Julia name   | Julia enum         | R enum used in port  | Notes |
|--------------|--------------------|----------------------|-------|
| required     | `RequiredInput`    | `RequiredInput`      | Direct parity. |
| optional     | `OptionalInput`    | `OptionalInput`      | Direct parity. |
| guaranteed   | `GuaranteedOutput` | **`CreatedOutput`**  | dafr's `CreatedOutput` is the only output enum that triggers `.is_mandatory(_, is_for_output=TRUE)` AND `.is_forbidden(_, is_for_output=FALSE, overwrite=FALSE)`. dafr also has a separate `GuaranteedOutput` enum, but verify_* does not enforce it; it merely participates in `.merge_expectations`. So `CreatedOutput` is the semantic equivalent of Julia's `GuaranteedOutput`. |
| contingent   | `OptionalOutput`   | `OptionalOutput`     | Same enum name on both sides, but enforcement diverges - see CS1. |

## Open divergences

### CS1. dafr `OptionalOutput` is not enforced as forbidden-on-input

- **Symptom.** Julia's `is_forbidden(expectation; is_output, overwrite)`
  returns `true` when `!is_output && expectation in
  (GuaranteedOutput, OptionalOutput) && !overwrite`. So if
  `OptionalOutput` is declared and the scalar already exists, calling
  `verify_input` errors with "pre-existing OptionalOutput scalar:
  ...". dafr's `.is_forbidden` only fires for `CreatedOutput`; both
  `GuaranteedOutput` and `OptionalOutput` are silently accepted on
  input regardless of overwrite or pre-existence.
- **Tests guarded.** Four cells of `scalar / ()`:
  - `contingent / !overwrite / input / !accessed`
  - `contingent / !overwrite / input / accessed`
  - `contingent / !overwrite / output / !accessed`
  - `contingent / !overwrite / output / accessed`
  All four are skipped with the same `CS1` message.
- **Likely scope.** Same gap will recur for vector / matrix / tensor
  in their own slices (each has a `contingent` arm hitting the
  `verify_input` pre-existing branch). Document a sister `CV1`/`CM1`/
  `CT1` for those rather than re-using `CS1`.
- **Fix path (deferred).** Extend `.is_forbidden` to include
  `OptionalOutput` (and likely `GuaranteedOutput`):
  ```r
  .is_forbidden <- function(expectation, is_for_output, overwrite) {
      !is_for_output &&
          expectation %in% c(CreatedOutput, GuaranteedOutput, OptionalOutput) &&
          !overwrite
  }
  ```
  This is a behavioral change with potential blast radius (any
  pipeline declaring `OptionalOutput` on a daf that already holds
  the property would start failing `verify_input`). Defer until the
  user requests the fix; the divergence is documented and the tests
  will start passing once `.is_forbidden` is widened.

### Recurring T-class: error wording

Julia's chomp-formatted multi-line errors include the expectation
token in caret-aligned form (e.g. "pre-existing GuaranteedOutput
scalar"). dafr emits "pre-existing CreatedOutput scalar" because
the R-side enum name differs. The parity tests use token-tolerant
regex (`pre-existing.*scalar.*version`) rather than the literal
expectation token. Same pattern as the chains / reorder ports.

## Test catalog

`tests/testthat/test-contracts-scalar-jl-parity.R` - 48 `test_that`
blocks: 32 for `scalar / ()` (4 expectations x overwrite/!overwrite x
input/output x !accessed/accessed), 8 for `scalar / missing` (4
expectations x input/output), 8 for `scalar / !type` (same shape).
44 PASS, 4 SKIP (all CS1).

The cross-product unrolling uses three helpers
(`.scalar_existing_assert`, `.scalar_missing_assert`,
`.scalar_wrong_type_assert`) parameterized by (expectation, overwrite,
direction, accessed). This matches the pattern used in the chains
parity port's `access` group. Each `test_that` is a one-line dispatch;
the helpers carry the conditional branches that Julia expresses inline
inside `nested_test`.

## Remaining contracts.jl scope

- contracts/axis (line 286-425, ~30 leaves) - same shape as scalar
- contracts/vector (line 426-663, ~50 leaves)
- contracts/matrix (line 664-1392, ~80 leaves; matrix has a sizable
  `fill` sub-block plus per-direction cross-products - recommend
  splitting into 2 sub-slices)
- contracts/tensor (line 1492-1638, ~10 leaves; tensor views are
  blocked by V1, so tensor verify may be partially blocked too)

Per kickoff, the recommendation is to bundle scalar + axis + vector
into one ship to main once all three green; matrix and tensor warrant
their own sessions.

## Out of scope

- Adding R-only test variants for `GuaranteedOutput` enum behavior
  (which differs from Julia's `GuaranteedOutput`). The contracts/add
  port did this for the merge tests; for verify tests it would mostly
  document "no enforcement" which is not informative. Defer.
- Lifting CS1 by widening `.is_forbidden`. See "Fix path" above.
