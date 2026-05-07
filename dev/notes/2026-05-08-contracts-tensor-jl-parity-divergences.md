# Audit: contracts.jl / tensor subgroup parity divergences

Date: 2026-05-08
Driver: literal port of contracts.jl's `tensor` subgroup (lines 1488-1635,
8 nested_test leaves) into
`tests/testthat/test-contracts-tensor-jl-parity.R`.

Seventh and final sub-slice of the contracts.jl port.

## Status

- **Fixed inline:**
  - **CT0** - `.verify_matrix_data` and `.verify_tensor_data` only
    looked up matrices in one orientation; now check both, mirroring
    `has_matrix()`/`get_matrix()`'s relayout-tolerant behavior.
  - **CT1** - tensor matrices did not propagate access to the main_axis
    tracker; now mirrors Julia's contracts.jl line 1445-1448 by adding
    a `main_axis` field to per-tensor trackers and propagating
    `.access_axis(cd, main_axis, FALSE)` whenever a per-entry matrix
    is accessed.
  - **CT2** - `format_add_axis` on ContractDaf now expands the
    tensor_index for any tensor record whose main_axis matches the
    axis just added, mirroring Julia's `expand_axis_tensors`. Without
    this, post-contractor `add_axis` left the tensor_index stale and
    follow-up `set_matrix` calls on per-entry matrices errored with
    "non-contract matrix".
- **Open / skipped:** none.

Skip count: 0. Result on this file: `FAIL 0 | SKIP 0 | PASS 15`
(8 leaves x 1-3 inner expects). Full-suite delta:
`5299 -> 5314 PASS, 55 -> 55 SKIP, 0 FAIL`.

## Inline fixes

### CT0. Verify matrix/tensor checks both orientations

`R/contracts.R:806-841` (`.verify_matrix_data`) and
`R/contracts.R:840-883` (`.verify_tensor_data`). The `format_has_matrix`
calls now try both `(rows_axis, columns_axis)` and the flipped
`(columns_axis, rows_axis)` keys before declaring the matrix missing.
The follow-up `format_get_matrix` for type-check picks the orientation
that exists. Mirrors public `has_matrix(relayout = TRUE)`.

### CT1. Tensor matrix access propagates to main_axis

`R/contracts.R` - added a `main_axis` field to `.new_tracker()`,
populated by `contractor()` for tensor records. `.access_matrix()`
now calls `.access_axis(cd, tracker$main_axis, FALSE)` when the
tracker has a non-NULL `main_axis`. Mirrors Julia's
`access_matrix` at `contracts.jl:1445-1448`.

### CT2. Lazy tensor_index expansion on post-contractor add_axis

`R/contracts.R` - added `.expand_tensor_index_for_axis()` helper,
called from the ContractDaf `format_add_axis` method. When an axis
is added that matches a tensor record's `main_axis`, the
tensor_index is populated with one entry per `<entry>_<tname>` for
the new axis entries. Mirrors Julia's `expand_axis_tensors`
(contracts.jl line 980-1010).

## Test catalog

`tests/testthat/test-contracts-tensor-jl-parity.R` - 8 `test_that`
blocks: input/(), input/missing, output/(), output/missing,
!axis/guaranteed/(), !axis/guaranteed/add/(), !axis/guaranteed/add/create,
!axis/optional. After CT0/CT1/CT2: 8 PASS, 0 SKIP.

## End of contracts.jl port

This commits the seventh and last sub-slice of the contracts.jl
port. Full file is 1639 lines / ~188 nested_tests; sub-slices
shipped: add (14), scalar (48), axis (40), vector (46),
matrix (54), access (87), tensor (8) = 297 PASS. Higher than the
"~188 nested_tests" estimate because helper-driven cross-products
expand into multiple `test_that` blocks plus inner expects.

C-class divergences captured along the way:
- C1 (contracts/add): R lacks Int32-vs-Int64 sibling type concept.
- CV0 (vector): contractor() axis-prerequisite validation - **fixed**.
- CS1, CA1 (scalar, axis): OptionalOutput not enforced - **fixed by widening .is_forbidden**.
- CA0 (axis): format_axis_length access tracking - **fixed**.
- CX1 (access): brief() unimplemented - skipped (out of scope).
- CX2 (access): description() type leak - **fixed**.
- CX3 (access): relayout_matrix is_for_modify - **fixed**.
- CX4 (access): axis_indices contract hook - **fixed**.
- CX5 (access): query syntax differences - adapted, not skipped.
- CT0 (tensor/matrix): verify both orientations - **fixed**.
- CT1 (tensor): main_axis propagation - **fixed**.
- CT2 (tensor): post-add_axis tensor_index expansion - **fixed**.
- C3 (chains, fill): empty_*/builder API unimplemented - skipped.

The contracts.jl port is now ready to ship to main. Bundle includes
all sub-slice commits and the inline fixes that closed CV0, CS1,
CA1, CA0, CX2/3/4, CT0/1/2.
