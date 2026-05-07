# Slice exit: contracts.jl / axis subgroup parity port

Date: 2026-05-08
Branch: `dev`
Commit: <to be filled by ship step>
Predecessors:
- `2026-05-08-slice-contracts-scalar-jl-parity-exit.md`
- `2026-05-07-slice-contracts-add-jl-parity-exit.md`
- `2026-05-08-jl-parity-port-next-session-kickoff.md` (kickoff item 2)

## Scope

Literal port of contracts.jl's `axis` subgroup (Julia lines 274-411,
40 nested_test leaves) into
`tests/testthat/test-contracts-axis-jl-parity.R`.

## Result

- **Test counts.** New file: `FAIL 0 | SKIP 4 | PASS 36`. Full suite:
  `FAIL 0 | WARN 1 | SKIP 57 | PASS 5104`. Delta from scalar-slice
  exit baseline (`5068 PASS / 53 SKIP`): +36 PASS, +4 SKIP, no new
  FAIL.
- **R/ changes.** 1 inline fix (CA0 - `format_axis_length` on
  ContractDaf now tags accessed; was previously silent).
- **Open divergences.** 1 (CA1 - sibling of CS1).

## Inline fix (CA0)

`R/contracts.R:496-501`. Added `.access_axis(daf, axis, is_for_modify = FALSE)`
to `format_axis_length`'s ContractDaf method. Without this, calling
`axis_length(contract_daf, "cell")` did NOT mark the axis as
accessed, which caused spurious "unused RequiredInput axis" errors
in `verify_output` after the only access to the axis was a length
read. Closes 2 of the 32 `axis / ()` cells (the `required /
*overwrite / output / accessed` pair); without it those would have
been forced into a CA-N skip alongside CA1.

Sibling: `format_axis_dict` (line 501-504) has the same omission.
Not exercised by this slice's tests; flagged in the divergence note
for a follow-up fix.

## CA1 (open)

Sibling of CS1: dafr's `.is_forbidden` doesn't fire for
`OptionalOutput`, so the four `contingent / !overwrite` cells of
`axis / ()` skip with the CA1 message. CV1 / CM1 / CT1 expected to
recur in the remaining sub-slices.

## Cumulative state on dev

`dev` is now 4 commits ahead of `main`:
1. kickoff doc (fd57418)
2. contracts/add (25e14cd) - 14 tests
3. contracts/scalar (173bc3f) - 48 tests, 0 inline fixes
4. contracts/axis (this commit) - 40 tests, 1 inline fix (CA0)

Per the kickoff plan, ship to main happens after `vector` (slice 3)
is also green. No ship yet.

## Acceptance per kickoff

> `FAIL 0 | PASS >= <ported test count>`, every skip keyed to a
> divergence-note ID, no skip reads just `# TODO`.

Met: 0 FAIL, 36 PASS (>= 36 ported), 4 SKIP all keyed to CA1.

## Next sub-slice

Per the kickoff: **`contracts/vector`** (Julia lines 426-663, ~50
leaves). The vector block adds an axis-prerequisite layer:
- `vector / !axis` (line 417-425): `contractor()` errors if the
  contract declares a vector on an axis the contract does not also
  declare - "non-contract axis: cell ..."
- `vector / ~axis` (line 427+): tests that the prerequisite axis
  contract enforcement is consistent across expectations.

Then the same cross-product as scalar/axis but with a `vector`
existence/missing structure. Plus a separate `~vector` block testing
`overwrite + ~ scalar` cross-cases. ~50 leaves total.

CV1 will be the OptionalOutput-not-forbidden gap (analogous to
CS1/CA1). Likely 4 skips for the cross-product's `contingent /
!overwrite` arm.

## Out of scope (carried forward)

Same as kickoff: E11, C2, V3, R6, fixing format_axis_dict's missing
access tag, lifting CS1/CA1.
