# Audit: contracts.jl / scalar subgroup parity divergences

Date: 2026-05-08
Driver: literal port of contracts.jl's `scalar` subgroup (lines 105-272,
48 nested_test leaves) into
`tests/testthat/test-contracts-scalar-jl-parity.R`.

This is the **second sub-slice** of the contracts.jl port. Sister doc:
`2026-05-07-contracts-add-jl-parity-divergences.md`.

## Status

- **Fixed inline:** **CS1** (lifted 2026-05-08 - widened
  `.is_forbidden` to include `OptionalOutput` and `GuaranteedOutput`).
- **Open / skipped:** none.

Skip count: 0. Result on this file: `FAIL 0 | SKIP 0 | PASS 48`.

## Closed divergences

### CS1. dafr `OptionalOutput` was not enforced as forbidden-on-input

- **Symptom (pre-fix).** Julia's `is_forbidden(expectation; is_output,
  overwrite)` returns `true` when `!is_output && expectation in
  (GuaranteedOutput, OptionalOutput) && !overwrite`. dafr's
  `.is_forbidden` only fired for `CreatedOutput`. Result: a contract
  declaring `OptionalOutput` for a property that already existed in
  the daf was silently accepted on `verify_input` even when overwrite
  was `FALSE`. Same gap for R's `GuaranteedOutput` enum.
- **Fix.** R/contracts.R - `.is_forbidden` widened to:
  ```r
  !is_for_output &&
      expectation %in% c(CreatedOutput, GuaranteedOutput, OptionalOutput) &&
      !overwrite
  ```
  And `.is_mandatory` widened to also include `GuaranteedOutput`
  alongside `CreatedOutput` so missing-on-output errors fire for
  `GuaranteedOutput` too. Net effect: R's `GuaranteedOutput` now
  behaves identically to R's `CreatedOutput`, both matching Julia's
  `GuaranteedOutput`. `OptionalOutput` matches Julia's `OptionalOutput`.
- **Tests unblocked.** All 4 `scalar / () / contingent / !overwrite`
  cells now pass without skip.

## Expectation mapping (Julia -> R)

The Julia test loops over four expectations; the R port maps them like
this:

| Julia name   | Julia enum         | R enum used in port  | Notes |
|--------------|--------------------|----------------------|-------|
| required     | `RequiredInput`    | `RequiredInput`      | Direct parity. |
| optional     | `OptionalInput`    | `OptionalInput`      | Direct parity. |
| guaranteed   | `GuaranteedOutput` | **`CreatedOutput`**  | After the CS1 fix, R's `CreatedOutput` and `GuaranteedOutput` enums are semantically identical for verify_*; the parity test uses `CreatedOutput` for traceability with the existing test-contracts-verify.R convention. |
| contingent   | `OptionalOutput`   | `OptionalOutput`     | Same enum on both sides. Post-fix: same enforcement on both sides. |

## Recurring T-class: error wording

Julia's chomp-formatted multi-line errors put the expectation token
in caret-aligned form (e.g. "pre-existing GuaranteedOutput
scalar"). dafr emits "pre-existing CreatedOutput scalar" because
the R-side enum used in the test is `CreatedOutput` rather than
`GuaranteedOutput`. The parity tests use token-tolerant regex
(`pre-existing.*scalar.*version`) rather than the literal
expectation token.

## Test catalog

`tests/testthat/test-contracts-scalar-jl-parity.R` - 48 `test_that`
blocks: 32 for `scalar / ()` (4 expectations x overwrite/!overwrite x
input/output x !accessed/accessed), 8 for `scalar / missing` (4
expectations x input/output), 8 for `scalar / !type` (same shape).
After CS1 lift: 48 PASS, 0 SKIP.

The cross-product unrolling uses three helpers parameterized by
(expectation, overwrite, direction, accessed). Same template as the
chains parity port's `access` group.
