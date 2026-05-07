# Audit: reorder.jl literal-parity divergences

Date: 2026-05-07
Driver: literal port of `~/src/DataAxesFormats.jl/test/reorder.jl` (501
lines, ~40 nested_test leaves) into
`tests/testthat/test-reorder-jl-parity.R`.

The port surfaced 2 real behavior gaps (fixed inline) and 5 divergences
worth documenting. dafr's existing R-side reorder coverage
(`test-reorder-{memory,files,crash,plan}.R`) was built before this
parity exercise and missed these gaps.

## Status

- **Fixed inline:** R0 (silent-skip on missing axis), R7
  (`reset_reorder_axes` returned daf, not Bool).
- **Open / skipped:** R2 (class-level is_leaf), R3 (no h5df backend),
  R4 (zarr_daf doesn't implement reorder), R5 (no multi-writer
  reorder_axes), R6 (memory_daf reorder is in-place not atomic).

Skip count in `test-reorder-jl-parity.R`: 7 across 5 unique IDs. Result:
`FAIL 0 | SKIP 7 | PASS 100`.

---

## FIXED in this slice (commits on `dev`)

### R0. `reorder_axes` silently skipped axes not on the daf

- **Symptom.** `reorder_axes(d, gene = c(1L, 2L))` on a daf without a
  `gene` axis returned the daf unchanged. Julia errors: `axis: gene
  does not exist in any of the writers`. The silent skip was at
  `R/reorder_plan.R:42` — an explicit `if (!format_has_axis(daf, axis))
  next` clause.
- **Fix.** `R/reorder_plan.R::.build_reorder_plan` — replace the silent
  `next` with `stop("axis: %s does not exist in the daf data: %s", ...)`.
- **Existing-test follow-up.** `test-reorder-plan.R` had a test pinning
  the silent-skip ("`.build_reorder_plan ignores axes that don't exist
  on the daf`"); rewrote it to assert the new error contract.
- **Julia ref.** `reorder.jl:175-182`.

### R7. `reset_reorder_axes` returned the daf, not a Bool

- **Symptom.** Julia's `reset_reorder_axes!` returns a Bool: TRUE if a
  pending reorder was rolled back, FALSE if no pending. dafr's wrapper
  always called the format method for its side effect and returned
  `invisible(daf)`, throwing away the format method's TRUE/FALSE.
- **Fix.** `R/reorder.R::reset_reorder_axes` — capture the
  `format_reset_reorder` return value and return `invisible(isTRUE(...))`.
  `R/memory_daf.R::format_reset_reorder` — return `invisible(FALSE)`
  (was `invisible()`, i.e. NULL). `R/files_daf_write.R`'s method already
  returned `invisible(TRUE)` on rollback / `invisible(FALSE)` on
  no-backup, so no change there.
- **No existing test breakage** — all callers in dafr's own test suite
  used `reset_reorder_axes` for side effect only (`expect_silent(...)`),
  so the contract change is invisible to them.
- **Julia ref.** `reorder.jl:73, 249, 306, 364, 453`.

---

## Open divergences

### R2. `is_leaf` class-level dispatch (Julia) vs. instance-only (dafr)

- **Symptom.** Julia's `is_leaf(MemoryDaf)` (passing the type) returns
  TRUE, allowing class-level checks before any instance is constructed.
  dafr's `is_leaf` is a `S7::new_generic("is_leaf", "daf")` keyed on a
  daf instance; you can't call it on a class. Conceptually replaceable
  with `inherits(., "MemoryDaf") || inherits(., "FilesDaf") || ...` at
  the call site, but the API surface is different.
- **Test guarded.** `reorder / is_leaf / types`.
- **Fix sketch.** Add a `is_leaf_class(cls)` helper that takes a class
  symbol/object. Low priority since dafr's instance-level check covers
  the actual use case (`reorder_axes` rejects non-leaf inputs at the
  instance check).

### R3. dafr does not have an h5df backend

- **Symptom.** Julia tests every backend including `H5df`. dafr has
  `memory_daf`, `files_daf`, `zarr_daf`, `anndata_format`, `http_format`
  — no h5df.
- **Tests guarded.** `reorder / is_leaf / h5df`,
  `reorder / reorder_axes! / h5df / *`. The `h5df / sparse_strings`
  test (which exercises sparse-string vector + matrix reorder) has no
  R counterpart at all.
- **Decision.** Out of scope for parity work; h5df is its own large
  backend port if ever wanted. Not on the roadmap.

### R4. dafr's `zarr_daf` does not implement reorder

- **Symptom.** No `format_replace_reorder` / `format_reset_reorder` /
  `format_cleanup_reorder` methods on `ZarrDaf`. Calling
  `reorder_axes(zarr_d, ...)` would fail with an S7 dispatch error.
- **Tests guarded.** `reorder / reorder_axes! / zarr / *`.
- **Fix sketch.** Implementable in principle (zarr stores are
  directory-shaped on the local FS path, similar to files_daf). Would
  need backup-via-rename + chunk-rewrite + reset path, ~150 lines.
  Not on the roadmap.

### R5. dafr's `reorder_axes` does not accept a list of writers

- **Symptom.** Julia's `reorder_axes!([daf1, daf2], dict)` reorders the
  shared axis across multiple writers atomically (with the per-axis
  validation that all writers' entries match). dafr's signature is
  single-daf (`reorder_axes(daf, ..., crash_counter = NULL)`).
- **Tests guarded.** `reorder / reorder_axes! / multiple_writers / *`
  (one collapsed skip-stub covering all 3 leaves).
- **Fix sketch.** Lift `reorder_axes` to accept either a single
  `DafWriter` or `list(DafWriter, ...)`; pre-validate axis-entry equality
  across writers (Julia's "axis: cell entries differ" error); apply the
  plan to each. ~50-line change. The Julia tests cover the
  cross-writer error path which dafr can't currently surface.

### R6. `memory_daf` reorder is in-place, not atomic

- **Symptom.** Julia's `MemoryDaf` reorder is atomic — a mid-reorder
  exception leaves the daf in its pre-reorder state. dafr's
  `memory_daf` reorder mutates entries / vectors / matrices in place.
  A simulated mid-reorder crash via `crash_counter` raises an R
  exception that exits the function call but leaves the daf
  partially-permuted. `reset_reorder_axes` for memory_daf is a no-op
  (returns FALSE). The dafr design comment at `R/memory_daf.R:427-428`
  explicitly justifies the non-atomicity ("if R crashes the in-memory
  store is gone anyway") — but that argument doesn't cover
  user-facing exceptions (unrelated to process crash) that should
  still leave the daf consistent.
- **Tests guarded.** `reorder / reorder_axes! / memory / crash_recovery
  / {after_1, after_4}`.
- **Fix sketch.** Snapshot the planned axes' state (entries dict +
  vectors envir + matrix buckets) at the start of
  `format_replace_reorder`, restore on error via `tryCatch(...,
  error = function(e) {restore; signalCondition(e)})`. Requires careful
  thought about the dict / env mutations (the entries dict is rebuilt;
  need to preserve the old one until success). ~30-50 lines.

---

## R-fundamental / non-portable

### T1. `mktempdir() do path ... end` (Julia) vs. `tempfile() + on.exit`

Pure idiom translation. R doesn't have a block-scoped tempdir helper;
the tests use `tempfile()` + `on.exit(unlink(tmp, recursive = TRUE))`.
No semantic divergence.

### T2. Error-text differences

Same shape as the concat slice's T2. Julia's chomp-formatted multi-line
errors (`non-leaf type: WriteChain\nfor the daf data: chain!\ngiven to
reorder_axes!`) translate to dafr's single-line `non-leaf type: %s for
the daf data: %s given to reorder_axes`. The parity test regex looks
for distinctive tokens (`non-leaf`, `chain`, axis name, etc.) rather
than exact wording.

---

## Test catalog

`tests/testthat/test-reorder-jl-parity.R` — 33 `test_that` blocks. The
h5df / zarr_reorder / multiple_writers groups are collapsed into one
skip-stub each (rather than per-leaf skips) since the divergence is
backend-wide rather than per-test. Helpers `.populate_reorder_test_data`,
`.assert_reorder_both_axes`, `.assert_reorder_single_axis`,
`.assert_original_data`, `.test_crash_recovery` mirror Julia's
top-of-file helper functions.

Counts:
- Behavior bugs fixed inline: 2 (R0, R7)
- Open divergences guarded by skip: 5 unique IDs across 7 skips (R2, R3
  ×2, R4, R5, R6 ×2)
- R-fundamental, no skip: T1 (tempdir idiom), T2 (error wording)
