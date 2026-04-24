# Slice 6 exit

**Branch:** `slice-6-copies-concat-complete` (NOT yet merged into main;
controller will handle fast-forward merge). Final commit SHA: `c77f821cac154d9217e9acd4f8e4dd6cdb9239ab`. Tag `slice-6` pending.

## What landed

- Phase A: `copy_scalar` + `copy_axis` + `.verify_axis_relation`.
- Phase B: `copy_vector` (same/subset/superset + empty fill + type).
- Phase C: `copy_matrix` (same/subset/superset + sparse-preserving pad
  + relayout). **Slice-5 dense-coercion mine CLOSED.**
- Phase D: `copy_tensor` (per-main-axis-entry `copy_matrix` loop).
- Phase E: `copy_all` + `empty_data()`.
- Phase F: `adapter()` refactored to use `copy_all` internally.
  `.copy_view_to_daf` removed.
- Phase G: `concatenate()` — single + multi-axis + prefix heuristic +
  merge actions (Skip/LastValue/CollectAxis).
- Phase H: `complete_chain` + `complete_daf` + `open_daf` (FilesDaf).
- Phase I: `reconstruct_axis` (core behaviors).
- Phase J: Julia parity fixture for `copy_all` and `concatenate`.
- Phase Z: NEWS + NAMESPACE + Collate + `devtools::check` = 0/0/0.
  Two fixes applied during Z4: non-ASCII em-dash in `R/complete.R`
  error messages; bare `as()` in `.embed_matrix_in_pad` qualified as
  `methods::as()`.

## Test / check numbers

- `testthat::test_dir("tests/testthat")`: **1315 PASS / 0 FAIL / 0 SKIP /
  1 WARN** (pre-existing `scran::quickCluster` / `irlba::irlba` SVD
  notice, unchanged from Slice 0).
- `devtools::check(error_on = "note")`: **0 errors / 0 warnings / 0 notes**.

## Deviations from plan (audit-trail)

- **Phase B**: B1/B3/B4 (copy_vector impl + subset/superset tests +
  sparse passthrough test) bundled into one commit `a2518e2` titled
  "feat(copies): copy_vector same-axis dense path". The title
  understates scope; the commit body does not enumerate B3/B4 content.
  Functional: all 12 copy_vector tests green.
- **Phase G**: the plan's prefix heuristic was scoped to the current
  axis (`name == axis || startsWith(name, paste0(axis, "."))`). In G4
  test, the `cell.cluster` property refers to the `cluster` axis (which
  is another concat axis) -- under the plan's scoping, this property was
  NOT prefixed. Implementer correctly widened the heuristic to check
  the property name against ALL concat axes, matching Julia semantics.
  The fix is in commit `bf5f563`.
- **Phase H**: H1/H2/H3 bundled into one commit `990aca5` titled
  "feat(complete): open_daf FilesDaf dispatch (H5df deferred)".
  Title covers only 1 of 3 functions. The commit actually contains
  `open_daf`, `complete_chain`, `complete_daf`, and all 7 test blocks.
- **Phase Z**: Two additional fixes committed during check (Z4):
  `R/complete.R` em-dash (non-ASCII WARNING) and `R/copies.R` bare
  `as()` call (NOTE). Both were Slice-6 introductions.

## Still deferred from Slice 6

- **Slice 7 (already committed):** Ops expansion (C option) -- remaining
  ~20 Julia ops (`Clamp`, `Convert`, `Fraction`, `Significant`, `Type`,
  `GeoMean`, `Median`, `Quantile`, `Std`, `StdN`, `Var`, `VarN`, `All`,
  `Any`, etc.) routed through `register_eltwise` / reduction mechanisms.
- `bestify` heuristic for `copy_vector` / `copy_matrix` (sparse-vs-dense
  promote/demote by nnz). Not implemented this slice.
- `reconstruct_axis` with a pre-existing target axis
  (`properties_defaults` path). Not implemented.
- `complete_daf` + `base_daf_view`: JSON is stored/parsed but the view
  is not re-applied on reopen. Slice 7 if needed.
- H5df backend for `open_daf`. Originally Slice 8; still deferred.

## Mines laid in Slice 6 for Slice 7

- **`copy_all` does not infer tensor keys from matrix names.** Users
  must call `copy_tensor` explicitly for the tensor entries. Julia
  expands a `TensorKey` in `empty` / `types` automatically; R doesn't.
- **`copy_all` axis-collision detection is LAZY.** Unlike
  `.copy_view_to_daf` which caught disjoint axes eagerly ("already
  exists in destination"), `copy_all` raises only when a vector/matrix
  needs copying on the colliding axis (error wording: "disjoint
  entries"). This is arguably cleaner semantics (a destination with its
  own axis is left alone if no data to copy), but differs from Slice 5.
- **`.cast_matrix_type` with `type = "integer"` on a `dgCMatrix`**
  dense-coerces. Only triggered when the user requests integer type on
  a sparse source matrix; not exercised by any current test.
- **`concatenate` string-prefix logic only fires when the source vector
  is `character`.** An integer-keyed property (unusual but legal in
  Julia) would silently not-prefix. Flagged.
- **`reconstruct_axis`** constructs the new-axis property via
  `vapply(unique_vals, ..., FUN.VALUE = values[[1L]])`. If the first
  entry happens to be empty-implicit, the FUN.VALUE type hint may be
  wrong. Tests exercise only non-empty-first data.
- **`.concat_axis_matrix`** transposes via `Matrix::t()` which
  allocates. Fine at fixture scale; watch at metacell scale.
- **Phase B and Phase H commits have misleading titles** (see
  Deviations above). Git log will not tell the full story on its own;
  this note is the audit trail.
- **`.matrix_type_ok` missing `character` case** (pre-existing Slice-4
  mine; unchanged this slice).

## Julia DAF state at Slice 6 exit

- `~/src/DataAxesFormats.jl` at `49fbba140437387a378217c2fa658d4231d0c8c1`.
- `~/src/TanayLabUtilities.jl` unchanged.
- Fixture sets: `julia-queries/`, `julia-chains/`, `julia-adapter/`,
  **`julia-copies/` (NEW)**.

## L2 upstream PR

Declined across Slices 3-5. Re-ask at Slice 7 exit if still relevant.

## Push status

Local `main` will be 28 commits ahead of `origin/main` after the
fast-forward merge. Slice-5 and `slice-6` tags are both local only.
Push deferred to user discretion.
