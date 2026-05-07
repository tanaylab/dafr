# Audit: contracts.jl / axis subgroup parity divergences

Date: 2026-05-08
Driver: literal port of contracts.jl's `axis` subgroup (lines 274-411,
40 nested_test leaves) into
`tests/testthat/test-contracts-axis-jl-parity.R`.

Third sub-slice of the contracts.jl port. Sister docs:
- `2026-05-07-contracts-add-jl-parity-divergences.md`
- `2026-05-08-contracts-scalar-jl-parity-divergences.md`

## Status

- **Fixed inline:** **CA0** (format_axis_length on ContractDaf was not
  tagging the axis as accessed; closed by adding `.access_axis(...)`
  to `R/contracts.R:496-499`).
- **Open / skipped:** **CA1** - parallel to CS1; dafr's
  `OptionalOutput` enum is not enforced as forbidden-on-input. 4
  skips (the `axis / () / contingent / !overwrite` arm).

Skip count: 4. Result on this file: `FAIL 0 | SKIP 4 | PASS 36`.
Full-suite delta: `5068 -> 5104 PASS, 53 -> 57 SKIP, 0 FAIL`.

## Inline fix

### CA0. `format_axis_length` on ContractDaf did not tag the axis as accessed

- **Symptom.** `axis_length(contract_daf, "cell")` would return the
  length without setting `tracker$accessed = TRUE`. Consequence: a
  computation that read an axis solely via `axis_length` but no
  other read would later trip `verify_output`'s "unused
  RequiredInput axis" check, even though the axis WAS read. Julia
  has the parallel access hook on `Readers.axis_length` (DAF.jl,
  contracts.jl line 1055-1058), so this is a dafr-only gap.
- **Fix.** Added `.access_axis(daf, axis, is_for_modify = FALSE)` to
  the ContractDaf method for `format_axis_length` (R/contracts.R:496-501).
  This matches the pattern used in `format_axis_array` (line 489-494).
- **Tests unblocked.** The two `required / *overwrite / output / accessed`
  cells of `axis / ()` (lines 103, 123 of the parity file). Without
  the fix, those went through `expect_silent(verify_output(cd))` and
  caught the spurious unused-axis error.
- **Sibling check.** `format_axis_dict` on ContractDaf (R/contracts.R:501-504)
  has the same omission. Not exercised by this slice's tests but
  flagged here as a likely sibling bug for a follow-up - should be
  fixed by analogy when an axis-dict-read scenario surfaces in the
  vector / matrix sub-slice or under user code.

## Expectation mapping (Julia -> R)

Same as the scalar slice (see `2026-05-08-contracts-scalar-jl-parity-divergences.md`):

| Julia      | Julia enum         | R enum used      |
|------------|--------------------|------------------|
| required   | `RequiredInput`    | `RequiredInput`  |
| optional   | `OptionalInput`    | `OptionalInput`  |
| guaranteed | `GuaranteedOutput` | `CreatedOutput`  |
| contingent | `OptionalOutput`   | `OptionalOutput` |

## Open divergences

### CA1. dafr `OptionalOutput` is not enforced as forbidden-on-input

Same gap as **CS1** (the scalar-block divergence): `.is_forbidden`
fires only for `CreatedOutput`, never for `OptionalOutput` or
`GuaranteedOutput`. Julia rejects pre-existing `OptionalOutput` axes
in `verify_input` when `overwrite=FALSE` (token "pre-existing
OptionalOutput axis: ..."); dafr returns silently.

- **Tests guarded.** Four cells of `axis / ()`:
  - `contingent / !overwrite / input / !accessed`
  - `contingent / !overwrite / input / accessed`
  - `contingent / !overwrite / output / !accessed`
  - `contingent / !overwrite / output / accessed`
- **Same fix path as CS1.** Widening `.is_forbidden` to include
  `OptionalOutput` (and likely `GuaranteedOutput`) will close CA1
  and CS1 simultaneously, plus the CV1 / CM1 / CT1 occurrences that
  the remaining sub-slices will surface. Single-PR candidate when
  the user calls for it.

### Recurring T-class: error wording

Same as scalar slice: Julia's chomp-formatted multi-line errors put
the expectation token in caret-aligned form ("pre-existing
GuaranteedOutput axis"); dafr writes "pre-existing CreatedOutput
axis" because the R-side token differs. Tests use
token-tolerant regex.

## Test catalog

`tests/testthat/test-contracts-axis-jl-parity.R` - 40 `test_that`
blocks: 32 for `axis / ()`, 8 for `axis / missing`. 36 PASS, 4 SKIP
(all CA1).

Same helper-driven structure as the scalar parity file.
`.axis_existing_assert(expectation, overwrite, direction, accessed)`
encapsulates the `()` cross-product;
`.axis_missing_assert(expectation, direction)` covers the `missing`
arm. No `!type` block (axes don't carry a type).

The Julia `vector / !axis` and `vector / ~axis` leaves at lines
417-446 are NOT part of this slice - they belong to the upcoming
contracts/vector slice and exercise the cross-axis-prerequisite
handling.

## Remaining contracts.jl scope

- contracts/vector (line 426-663, ~50 leaves) - includes the cross-axis
  prerequisite leaves
- contracts/matrix (line 664-1392, ~80 leaves; recommend split)
- contracts/tensor (line 1492-1638, ~10 leaves)
