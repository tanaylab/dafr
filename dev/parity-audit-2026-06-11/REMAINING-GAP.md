# Parity audit - remaining gap (resume here)

Status as of 2026-06-15. The parity-audit fixes shipped publicly:
**dafr 0.4.5** (the ~17 fixes below) and **0.4.6** (doc hotfix for the
`relayout=TRUE` default). Both on `tanaylab/dafr` `main`, CI all green
(R-CMD-check 5-OS, altrep-sanity, pkgdown). See `FIX-LOG.md` for the
fix-by-fix trail and `REPORT.md` / `verdicts.json` for the original audit.

This file is the **open backlog** - what a fresh session needs to close the
rest of the gap. Probe names map to rows in `verdicts.json` / `wf_all.json`;
reproduce a probe with the differential harness (run the same op in dafr and in
`~/src/DataAxesFormats.jl` @ `80aee1d` and diff). Test invocation: `cd tests &&
NOT_CRAN=true Rscript testthat.R` (or `pkgload::load_all(".")` +
`testthat::test_file(...)` against `/tmp/dafrlib044`). Always count BOTH
`df$failed` and `df$error` in sweeps.

## DONE (shipped in 0.4.5/0.4.6) - do not redo

groupby/countby bytewise order; concat missing-scalar error; named-default
order error; `set_matrix` relayout default TRUE (+ example_data/zarr_convert
fallout repair + 0.4.6 roxygen-example/codoc fix); copy_matrix flipped
transpose-read; reserved `name`/`index` reader vectors; reconstruct empties-key
+ implicit string-FK rewrite; zarr fill-chunk reconstruct (vector + matrix);
copy_all both-layouts dedup; copy_tensor missing-slice skip; adapter
insist-on-collision; computation() overwrite (COMP-01); **anndata dense `/X` +
`obsm`/`varm` canonical (n_obs,n_var)/(n_axis,k) orientation**.

## DONE (in working tree, not yet shipped) - do not redo

- **anndata >= 0.12 `nullable-string-array` read** (was OPEN item 1). Real scope
  was wider than the original note: the `{values, mask}` group encoding is used
  not only for categorical `categories` but also for the obs/var `_index`
  (so a 0.12 file failed at the very first `_index` read) and for plain pandas
  `string`-dtype columns (silently skipped). Fix: a shared
  `.read_h5ad_string_array()` helper (handles plain `string-array` dataset OR
  `nullable-string-array` group, mask -> NA) applied at all three sites, plus a
  `nullable-string-array` branch in the obs/var column loops. Real anndata 0.12.1
  fixture `tests/testthat/fixtures/anndata_nullable_strings.h5ad` (generator:
  `dev/fixtures/generate_anndata_nullable_strings.py`) + new
  `test-parity-anndata-nullable-strings.R`. Probe: nullable-string-array.
  NB anndata quirk: a `string` column that *contains* a missing value is written
  as `categorical` (codes with -1), not nullable-string-array.

- **anndata >= 0.12 `nullable-integer` / `nullable-boolean` column read**
  (was OPEN item 1). Generalised the above: an NA-bearing pandas `Int64`/
  `boolean` column writes as a `nullable-integer` / `nullable-boolean` `{values,
  mask}` group (NB the encoding strings are asymmetric - NO `-array` suffix,
  unlike `nullable-string-array`; my earlier note guessing `...-array` was
  wrong). Fix: a type-agnostic `.read_h5ad_nullable()` helper (reads `values`,
  `x[mask] <- NA` promotes to the right NA flavour) + a `.H5AD_NULLABLE_ENCODINGS`
  set covering all three; `.read_h5ad_string_array()` now delegates to it. Same
  fixture (extended with `n_umis`/`is_doublet`) + test file. Fully-populated
  numeric/bool columns still write as plain `array` datasets that already read.

## OPEN - genuinely fixable (priority order)

1. **`complete_daf` view scope / `r+` writability / cross-language view JSON.**
   Probes: `complete-view-scope`, `complete-rplus-view-readonly`,
   `complete-view-json-xlang`. File: `R/complete.R`. Multi-part; check each
   against Julia `complete.jl`. Was deferred as "complex multi-part" - needs a
   careful per-probe pass.

2. **sparse VECTOR densified on read.** Probe: `mem-sparse-vec-read`.
   File: `R/utils.R` `.attach_vector_axis_names` (and the read path that calls
   it). A vector stored sparse comes back dense instead of a `sparseVector`.
   Verify first whether this is truly fixable or an R-type constraint; UPDATE 9
   lumped it with inherent-type items but it is listed as "clean(ish)".

3. **reorder sparse index width.** Probe: `reorder-uint16-indtype`.
   File: `R/reorder.R`. Reorder widens the sparse index integer type. Partly
   inherent (R `dgCMatrix` uses `integer`/`double` indices, cannot hold
   `UInt16`), so the realistic outcome may be GUARD/document, not a full fix.
   Scope the non-inherent part.

## OPEN - needs a DESIGN decision first (not a straight code fix)

4. **http/files root `metadata.json` interop (cross-language serving).**
   Probes: `files-meta-json-missing`, `files-http-client-cross`.
   Files: `R/files_*.R`, `R/http_*.R`. dafr writes a `metadata.zip`; Julia
   writes a root `metadata.json`. Pick the canonical on-disk layout so a dafr
   store can be served to / read by Julia and vice-versa. Decide layout before
   coding.

## CLOSED BY DESIGN - do NOT "fix" (dafr is intentionally correct/safer)

Re-deciding these wastes a session. Document only; optionally raise upstream.

- **group_names** FNV-32 vs Julia simhash - deliberate (shape-parity test
  documents it).
- **regex `~`/`!~`** collects multi-token patterns to allow unescaped
  metacharacters - deliberate convenience (`query_parse.R`).
- **contracts `Optional`/`GuaranteedOutput` strictness** (CTR-P1..P4) - dafr
  rejects a pre-existing OptionalOutput / never-created GuaranteedOutput where
  DAF.jl 0.3.0 accepts; dafr's stricter behavior is safer (likely a DAF.jl
  quirk). DECIDED keep stricter, raise upstream.
- **chain `relayout_matrix` writer mutation** - dafr succeeds with both layouts;
  Julia errors. Matching Julia would degrade dafr to a quirk.

## INHERENT R-TYPE LIMITS (9) - GUARD/document, not a clean fix

Mapping DAF's full numeric tower onto R's types loses information:
`Float32` reduction/`Convert` precision (OPS-01); unsigned overflow -
`UInt32` >= 2^31 -> NA/negative, `UInt64` > 2^63 -> NA (OPS-03, P1/P4-uint32);
narrow/unsigned dtype width not preserved on write (TKR-04/05); int64/uint64
sparse `nzval` precision; `zarr-dtype-*`; `reorder-float32-widen`;
`files-int-sparse-matrix-eltype-loss`. Best move: a read/write-time overflow
GUARD that warns/errors loudly instead of corrupting silently.

## DEFERRED BACKENDS (feature work, user-gated) - not started

`h5df` backend; `ZipDaf` backend; **packed/sharded WRITE** (blosc/zstd encode -
read already works when a system c-blosc/libzstd is present via `configure`).
