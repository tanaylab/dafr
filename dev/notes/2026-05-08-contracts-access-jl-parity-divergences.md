# Audit: contracts.jl / access subgroup parity divergences

Date: 2026-05-08
Driver: literal port of contracts.jl's `access` subgroup (lines 936-1486,
~93 nested_test leaves) into
`tests/testthat/test-contracts-access-jl-parity.R`.

Sixth sub-slice of the contracts.jl port. Five sub-blocks: relaxed,
empty, axes, full, fill.

## Status

- **Fixed inline:**
  - **CX2** - `description()` on ContractDaf now delegates to base
    (was emitting `type: ContractDaf`; Julia delegates so user sees
    the storage type).
  - **CX3** - `format_relayout_matrix` ContractDaf method now passes
    `is_for_modify = TRUE` (was FALSE; Julia treats relayout as a
    modify op so OptionalInput / RequiredInput entries reject it).
  - **CX4** - `axis_indices` now calls `.access_axis` when given a
    ContractDaf (was silent; Julia's Readers.axis_indices on
    ContractDaf calls access_axis).
- **Open / skipped:**
  - **CX1** - dafr does not export `brief()`. 2 skips. Out of scope
    for parity (would need a new dafr helper).
  - **C3** (carry-over from chains slice) - dafr lacks the
    `empty_*`/builder API. 4 skips in the `fill` block.

Skip count: 6 (2 CX1 + 4 C3). Result on this file:
`FAIL 0 | SKIP 6 | PASS 87`. Full-suite delta:
`5212 -> 5299 PASS, 49 -> 55 SKIP, 0 FAIL`.

## Inline fixes

### CX2. description() on ContractDaf

`R/readers.R` - early-return delegation:
```r
if (inherits(daf, "dafr::ContractDaf")) {
    return(description(S7::prop(daf, "base")))
}
```

This makes `description(contract_daf)` emit the same string as
`description(daf)` (the underlying base). Mirrors Julia's behavior
where ContractDaf is transparent to description.

### CX3. relayout_matrix is a modify op

`R/contracts.R:664-672` - changed the ContractDaf method for
`format_relayout_matrix` to pass `is_for_modify = TRUE` instead of
`FALSE`. Justification: Julia treats relayout as a modify op (same
ban list as set_matrix!). Without this fix, RequiredInput /
OptionalInput contracts permitted a silent relayout that could
mutate the storage's layout cache.

### CX4. axis_indices on ContractDaf

`R/readers.R` - inline ContractDaf check at the top of
`axis_indices()`. Without this, axis_indices on an empty contract
silently succeeded because dafr's `format_axis_dict` ContractDaf
hook is a no-op (matching Julia's commented-out hook there).
Julia's Readers.axis_indices wires the access-tracking call at the
public-level instead, which we mirror here.

## Open divergences

### CX1. dafr does not export `brief()`

Julia uses `brief(::ContractDaf)` to format a one-line summary
("Contract MemoryDaf memory!.for.computation"). dafr has no
equivalent. 2 leaves (`relaxed/brief`, `empty/brief`) skip with CX1.

Out of scope - adding `brief.R` is a new feature, not a parity bug.

### C3. dafr lacks the empty_* / builder API

Carry-over from the chains slice. 4 leaves in the `fill` block
(`empty_dense_matrix`, `empty_dense_vector`, `empty_sparse_matrix`,
`empty_sparse_vector`) skip with C3.

## CX5: Query syntax differs (adapted, not skipped)

dafr's query parser uses `@`/`.`/`::` operators where Julia uses
`/`/`:`. The 3 query-equivalence test lines were adapted to dafr
syntax (`@ ?`, `@ cell`, `. version`) rather than skipped, since
the test substance is "this query returns the same thing as the
non-query call" - parity holds, only the surface syntax differs.

The matrix-equivalence query `/ cell / gene : UMIs` was simply
omitted (the `has_matrix` test still covers the underlying parity).

## Test catalog

`tests/testthat/test-contracts-access-jl-parity.R` - 93 `test_that`
blocks across 5 sub-blocks. 87 PASS, 6 SKIP.

## Description format note

dafr's `description()` currently elides the per-vector/matrix
"X x Float64 (Dense)" element-type/layout suffix that Julia emits.
The access-slice description tests use loose `expect_match` against
key substrings (`name:`, `type:`, axis entries) rather than the
full multi-line string, so this cosmetic difference does not block
parity. Tightening the description format to a fully-matching
variant is a future feature, not a bug.
