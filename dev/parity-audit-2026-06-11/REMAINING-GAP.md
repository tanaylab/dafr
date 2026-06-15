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

## OPEN - genuinely fixable (priority order)

1. **anndata >= 0.12 `nullable-string-array` categorical read.**
   Probe: (discovered during anndata-X-orientation work, UPDATE 11).
   File: `R/anndata_format.R` `.read_h5ad_categorical`. Symptom: reading a real
   anndata>=0.12 `.h5ad` whose `obs`/`var` categorical `categories` are stored
   as a `nullable-string-array` group (not a plain string dataset) fails with
   "attempt to apply non-function". Add a reader branch for that encoding.
   Concrete + testable: write a fixture with Python `anndata` 0.12 (envs
   `daf_env`/`borzoi-finetune`/`crested` have it) that includes a categorical
   obs column, then read it. This is why `tests/testthat/fixtures/
   anndata_canonical.h5ad` was committed WITHOUT obs columns - it is the gap.

2. **`complete_daf` view scope / `r+` writability / cross-language view JSON.**
   Probes: `complete-view-scope`, `complete-rplus-view-readonly`,
   `complete-view-json-xlang`. File: `R/complete.R`. Multi-part; check each
   against Julia `complete.jl`. Was deferred as "complex multi-part" - needs a
   careful per-probe pass.

3. **sparse VECTOR densified on read.** Probe: `mem-sparse-vec-read`.
   File: `R/utils.R` `.attach_vector_axis_names` (and the read path that calls
   it). A vector stored sparse comes back dense instead of a `sparseVector`.
   Verify first whether this is truly fixable or an R-type constraint; UPDATE 9
   lumped it with inherent-type items but it is listed as "clean(ish)".

4. **reorder sparse index width.** Probe: `reorder-uint16-indtype`.
   File: `R/reorder.R`. Reorder widens the sparse index integer type. Partly
   inherent (R `dgCMatrix` uses `integer`/`double` indices, cannot hold
   `UInt16`), so the realistic outcome may be GUARD/document, not a full fix.
   Scope the non-inherent part.

## OPEN - needs a DESIGN decision first (not a straight code fix)

5. **http/files root `metadata.json` interop (cross-language serving).**
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
