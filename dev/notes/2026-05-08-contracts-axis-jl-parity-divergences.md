# Audit: contracts.jl / axis subgroup parity divergences

Date: 2026-05-08
Driver: literal port of contracts.jl's `axis` subgroup (lines 274-411,
40 nested_test leaves) into
`tests/testthat/test-contracts-axis-jl-parity.R`.

Third sub-slice of the contracts.jl port. Sister docs:
- `2026-05-07-contracts-add-jl-parity-divergences.md`
- `2026-05-08-contracts-scalar-jl-parity-divergences.md`

## Status

- **Fixed inline:**
  - **CA0** (`format_axis_length` on ContractDaf was not tagging the
    axis as accessed; closed by adding `.access_axis(...)` to
    `R/contracts.R:496-501`).
  - **CA1** (lifted alongside CS1 - widened `.is_forbidden` to include
    `OptionalOutput` and `GuaranteedOutput`).
- **Open / skipped:** none.

Skip count: 0. Result on this file: `FAIL 0 | SKIP 0 | PASS 40`.

## Closed divergences

### CA0. `format_axis_length` on ContractDaf did not tag the axis as accessed

See pre-existing description in this file's earlier revision; closed
by adding `.access_axis(daf, axis, is_for_modify = FALSE)` to the
ContractDaf method for `format_axis_length` (R/contracts.R:496-501).
Sibling format_axis_dict has Julia's hook commented out (Julia DAF
test/contracts.jl line 1032 has `# access_axis(...)` deliberately
disabled), so dafr's no-tracking on format_axis_dict matches Julia
- not a bug; closed item.

### CA1. dafr `OptionalOutput` was not enforced as forbidden-on-input

Sibling of CS1; same fix at the same site
(`.is_forbidden` in R/contracts.R). Lifted simultaneously.

## Expectation mapping (Julia -> R)

Same as the scalar slice (see
`2026-05-08-contracts-scalar-jl-parity-divergences.md`):

| Julia      | Julia enum         | R enum used      |
|------------|--------------------|------------------|
| required   | `RequiredInput`    | `RequiredInput`  |
| optional   | `OptionalInput`    | `OptionalInput`  |
| guaranteed | `GuaranteedOutput` | `CreatedOutput`  |
| contingent | `OptionalOutput`   | `OptionalOutput` |

## Recurring T-class: error wording

Same as scalar slice: Julia's chomp-formatted multi-line errors put
the expectation token in caret-aligned form ("pre-existing
GuaranteedOutput axis"); dafr writes "pre-existing CreatedOutput
axis" because the R-side token differs. Tests use
token-tolerant regex.

## Test catalog

`tests/testthat/test-contracts-axis-jl-parity.R` - 40 `test_that`
blocks: 32 for `axis / ()`, 8 for `axis / missing`. 40 PASS, 0 SKIP
after CA0+CA1 fixes.

The Julia `vector / !axis` and `vector / ~axis` leaves at lines
417-446 are NOT part of this slice - they belong to the upcoming
contracts/vector slice and exercise the cross-axis-prerequisite
handling.
