# Audit: contracts.jl / add subgroup parity divergences

Date: 2026-05-07
Driver: literal port of contracts.jl's `add` subgroup (lines 8-114, 14
nested_test leaves) into
`tests/testthat/test-contracts-add-jl-parity.R`.

This is the **first sub-slice** of the contracts.jl port. The full
file is 1639 lines / ~188 nested_tests, naturally splits into 5+
sub-slices (add, scalar, axis, vector, matrix, tensor — and matrix
has a sizable `fill` sub-block plus per-direction cross-products).
Per the kickoff doc, splitting was anticipated.

## Status

- **Fixed inline:** none.
- **Open / skipped:** C1 (type lattice differences — R has no Int32 vs
  Int64 sibling-type concept).

Skip count: 1. Result: `FAIL 0 | SKIP 1 | PASS 19`.

---

## Open divergences

### C1. Type lattice differs: R has no sibling-type incompatibility

- **Symptom.** Julia's `Contract(...) |> Contract(...)` rejects merging
  two type specs that are siblings in the type lattice without a
  subtype relation, e.g. `Int64` vs `Int32` (both subtypes of `Signed`
  / `Integer` but not of each other). Error: "incompatible type:
  Int64 / and type: Int32 / for the contracts data: ('cell', 'age')".
  dafr's `.merge_types` uses a total width order
  `logical < integer < double < numeric < character` and always
  resolves to the narrower side. There is no R-side "incompatible
  siblings" case because R's atomic-type set is total.
- **Test guarded.** `contracts / add / data / incompatible` — 1 skip.
- **Conceptual note.** The Julia-Int32-vs-Int64 distinction maps
  loosely to R's "what bit width is used"? but R doesn't expose
  storage-width as a contract type; integers are 32-bit, doubles 64-bit
  by R's spec. Adding `int32` / `int64` typed columns would require
  pulling in `bit64::integer64` and treating it as a separate width.
  Out of scope for parity work.

### C-class T1. Error wording

Julia's chomp-formatted multi-line errors carry caret-aligned
position info; dafr's are single-line. The parity tests use
substring-tolerant regex.

---

## Test catalog

`tests/testthat/test-contracts-add-jl-parity.R` — 14 `test_that`
blocks (10 axes + 4 data); 13 substantive PASS, 1 documented SKIP.

The `data / int32-int32`, `int32-integer`, `integer-int32` tests are
ported with the R analogues `integer-integer`, `integer-numeric`,
`numeric-integer` since R has no Int32/Int64 distinction; the
narrower-wins semantic is preserved across the translation.

## Remaining contracts.jl scope

- contracts/scalar (line 116-285, ~30 leaves with cross-product loops)
- contracts/axis (line 286-425, ~30 leaves)
- contracts/vector (line 426-663, ~50 leaves)
- contracts/matrix (line 664-1392, ~80 leaves including the fill sub-block)
- contracts/tensor (line 1492-1638, ~10 leaves)

Each is its own future slice.
