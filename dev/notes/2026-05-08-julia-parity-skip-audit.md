# dafr <-> Julia DAF main parity audit (2026-05-08)

## Context

User reported that R users were copying Julia DAF queries into R and
getting different results. We initially attempted a "modern Julia DSL"
migration over 12 commits; it turned out the alt-DSL was an unreleased
experiment in a stray `~/.julia/packages/DataAxesFormats/ob7pR` clone
(NOT what the user's `~/src/DataAxesFormats.jl` main branch actually
ships). dafr 0.2.x's existing surface DSL already matched
DataAxesFormats.jl `main` byte-for-byte at the operator level. The
migration was reset (`git reset --hard 7743d00`) and the discarded
work is preserved on tag `dev-migration-discarded`.

## Ground truth

`~/src/DataAxesFormats.jl` is on branch `main`, synced with origin
(`b40377f`). dafr's parity target. Use the `dafr-mcview` conda env
to run Julia (julia 1.12.5 with DAF.jl v0.2.0 dev'd at that path).

## Scope decided with user

In scope: **semantic divergences** between dafr and DataAxesFormats.jl
main. C-class / CO-class / CT-class behavioral fixes, named-result
audit, viewer wildcard validation.

Out of scope (deferred):
- C3: `empty_dense_*` / `empty_sparse_*` builder API
- C2: `description(deep = TRUE)` parameter
- CC3: multi-contract `computation()` framework
- R3: h5df backend
- CA9: file-based contract bridge
- M5: `Matrix::sparseVector` support
- CC4: macro internals (not applicable to R)

## Skip inventory (242 total)

Run `grep -h "^\s*skip(" tests/testthat/*.R | awk -F'"' '{print $2}'
| sort | uniq -c | sort -nr` for the live count.

### Operation divergences (in-scope)

Where dafr is too permissive vs Julia, or signature differs:

- **CO1 Fraction** (3 skips, test-operations-jl-parity.R):
  - Julia rejects `% Fraction type Int32` with
    `invalid value: "Int32"` ... `value must be: a float type`.
  - Julia errors on scalar input: `applying Fraction eltwise
    operation to a scalar`.
  - dafr errors on scalar with different wording; no `type` validation.
- **CO2 Significant** (7 skips):
  - Julia accepts low-only and uses high=low default. dafr requires
    both `high` and `low`.
  - Various param validation messages differ.
- **CO4 GeoMean** (4 skips): dafr signature/semantics differ.
- **CO6 Log** (~3 skips): dafr returns `NaN` for `log(negative)`;
  Julia errors `invalid value: ...`.
- **CO7 Abs / Clamp** (~6 skips): dafr silently coerces non-numeric
  type; Julia rejects.
- **CT1, CT3** (~10 skips): generic eltwise/reduction param validation.
- **T-class** (~2 skips): R kernels promote integer matrices to
  double during Sum reduction; can't detect Float64-default vs
  Int-result mismatch like Julia's InexactError.

For each operation: open the corresponding test in
`tests/testthat/test-operations-jl-parity.R` and the Julia equivalent
in `~/src/DataAxesFormats.jl/test/operations.jl` (search by op name).
Adjust dafr's R/operations.R to match Julia's validation + return
shape. Dispatch table for op->Julia-test-line:

| dafr op file lines | Julia test file | Julia file line |
|---|---|---|
| `R/operations.R::.op_fraction` | `test/operations.jl` | 158-194 |
| `R/operations.R::.op_significant` | `test/operations.jl` | 240-300 |
| `R/operations.R::.op_log` | `test/operations.jl` | 196-238 |
| `R/operations.R::.op_abs` | `test/operations.jl` | ~30 |
| `R/operations.R::.op_clamp` | `test/operations.jl` | ~80 |
| `R/operations.R::.op_geomean` | `test/operations.jl` | search |

### Viewer divergences (in-scope)

- **V2** (~6 skips, test-views-jl-parity.R): dafr's viewer is more
  permissive on wildcard queries. Julia validates that `*` keys must
  map to `=` or `NULL`. dafr currently silently accepts arbitrary RHS.
- **V7** (~1 skip): dafr's view-scalar layer doesn't validate that
  the resolving query produces a scalar shape; a vector-producing
  query silently returns the vector via `get_scalar`. Add validation
  to error or auto-reshape.

### Named-result audit (in-scope, user emphasis)

Spot check passed: `@ donor : age` in dafr produces identical names
as Julia's `NamedVector`. `@ cell @ gene :: UMIs` matrix has
correct dimnames. **TODO**: walk every Julia test_that that asserts
on names and verify dafr produces the same. Full pass is in:
- `~/src/DataAxesFormats.jl/test/queries.jl`
- `~/src/DataAxesFormats.jl/test/operations.jl`

If a name mismatch is found, find the dafr eval code path that
returns the value (R/query_eval.R) and align the names.

### Out-of-scope skips (leave as-is)

- `helper-*.R` skips (3-4 each in helper-mmap-zip, helper-http-server,
  helper-altrep, helper-julia, etc.) - infrastructure, not divergences.
- `test-http-live.R` (4 skips) - requires live HTTP server.
- `test-anndata-jl-parity.R` (11 skips) - h5ad / file format gaps.
- `test-mmap-zip-store-foreign.R` (2 skips) - cross-platform zip.
- `CC_VECTOR: covered by test-copies-vector.R` (6 skips) - benign
  de-dup (the parity port is covered by a separate test).

## Recommended attack order

1. **Named-result audit** (quick wins, user-emphasized).
   - Run a script to compare dafr vs Julia output names for every
     test_that in queries.jl + operations.jl.
   - Fix any mismatches in R/query_eval.R.

2. **CO1-CO7 op divergences** (~25 skips).
   - Tighten R/operations.R parameter validation.
   - Mirror Julia's error wording (the `invalid value: "..."` /
     `value must be: ...` / `for the parameter: ...` template).
   - Check dafr returns a typed result when `type` is provided.

3. **CT1, CT3** (~10 skips). Same pattern.

4. **V2, V7 viewer divergences** (~7 skips). Tighten R/view_daf.R
   validation.

5. **T-class** (~2 skips): minor; document or fix via type coercion.

6. Re-run full suite; expect 240 -> ~30 skips remaining (the
   out-of-scope set).

## Tooling

- `dafr-mcview` conda env runs Julia 1.12.5 with DAF.jl 0.2.0 dev'd.
  `conda run -n dafr-mcview julia -e '<code>'` for one-shot probes.
- For each fix: write a tiny test against the Julia expected
  output FIRST, then change R/operations.R to match. The tests are
  already in tests/testthat/test-operations-jl-parity.R - just
  delete the `skip()` line and let the existing assertions exercise
  the fix.

## Tag preserved

`dev-migration-discarded` retains the 12 migration commits in case
any test-only fixups (escape regex chars in IsMatch canonicals,
sprintf `%>` escape, etc.) turn out to be useful. Cherry-pick
selectively, do not merge wholesale.
