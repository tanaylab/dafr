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

- **`complete_daf` view scope + `r+` writability** (probes `complete-view-scope`,
  `complete-rplus-view-readonly`; was OPEN item 1, first two parts). Root cause:
  on reopen `complete_daf` wrapped the WHOLE chain in `viewer(chain(base, leaf))`,
  so leaf-local data on the (renamed) view axis was reinterpreted through the
  view and vanished ("missing vector"), a leaf override returned the base value,
  and under `r+` the read-only viewer hid the writable leaf. Fix (`R/complete.R`):
  the view now wraps the BASE sub-chain only, with the leaf chained on top -
  `chain(list(viewer(base), leaf))`, matching the write side and Julia's
  `collect_dafs` (complete.jl:106-122). New tests in
  `test-complete-view-roundtrip.R`; two existing tests updated (reopened is now a
  chain, not a top-level ViewDaf). NB on `complete-rplus`: Julia itself CRASHES at
  reopen of a viewed r+ chain; dafr now returns a correct writable chain (leaf
  writable) - an intentional improvement over the Julia reference, not a gap.
  STILL OPEN: `complete-view-json-xlang` (see DESIGN section below).

- **sparse index width: narrow to UInt16 on write** (probe `reorder-uint16-indtype`;
  was the last OPEN genuinely-fixable item). The in-memory `dgCMatrix` uses R
  integer indices (the inherent part), but the ON-DISK colptr/rowval index dtype
  is a separate write-time choice. dafr's `.indtype_for_size` (R/files_io.R) only
  ever returned UInt32/UInt64; the canonical Julia
  `TanayLabUtilities.indtype_for_size` (dev path `~/src/TanayLabUtilities.jl`,
  pinned by DAF @ 80aee1d) floors at UInt16 (`size <= typemax(UInt16)`). Fix: add
  the UInt16 branch, keeping dafr's UInt32->UInt64 boundary at R's `integer.max`
  (2^31-1) rather than Julia's 2^32-1 (an index value >= 2^31 can't fit R's
  signed int - the genuinely-inherent residue). The IO layer already read/wrote
  UInt16. Blast radius (handled): test-files-matrices/-vectors had write-then-
  raw-read asserts hardcoding UInt32 (size=4L) + one sparsify-boundary case that
  flips to sparse under the cheaper UInt16 index (matches Julia's 0.75 formula).
  NB: there exist TWO TanayLabUtilities depot copies - an older `xbDaH` (UInt32-
  only) and newer `Puhfz`/dev (UInt16); the dev path is canonical.

## OPEN - genuinely fixable (priority order)

(none) - all genuinely-fixable, non-design-decision parity gaps are now closed.
What remains is the two cross-language DESIGN decisions below, the INHERENT
R-type limits, and the deferred backends.

## OPEN - needs a DESIGN decision first (not a straight code fix)

1. **http/files root `metadata.json` interop (cross-language serving).**
   Probes: `files-meta-json-missing`, `files-http-client-cross`.
   Files: `R/files_*.R`, `R/http_*.R`. dafr writes a `metadata.zip`; Julia
   writes a root `metadata.json`. Pick the canonical on-disk layout so a dafr
   store can be served to / read by Julia and vice-versa. Decide layout before
   coding.

2. **`complete_daf` `base_daf_view` JSON cross-language format.** Probe:
   `complete-view-json-xlang`. File: `R/complete.R` (`complete_chain` writes
   `jsonlite::toJSON(list(axes=, data=))` positional arrays;
   `complete_daf`/`.normalise_json_spec` read it back). Julia
   (`chains.jl:186-190`) serialises single-key objects with paren-tuple matrix
   keys, e.g. `{"axes":[{"cell":"="}], "data":[{"(cell,gene,umi)":"="}]}`. The
   two schemas are structurally incompatible, so a chain written by one language
   cannot be reopened by the other. Same flavour as the metadata.json item:
   pick a canonical view-spec JSON schema (adopt Julia's, most likely) before
   coding. The intra-dafr round-trip works today; this is purely cross-language.

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

## INHERENT R-TYPE LIMITS (10) - GUARD/document, not a clean fix

Mapping DAF's full numeric tower onto R's types loses information:
`Float32` reduction/`Convert` precision (OPS-01); unsigned overflow -
`UInt32` >= 2^31 -> NA/negative, `UInt64` > 2^63 -> NA (OPS-03, P1/P4-uint32);
narrow/unsigned dtype width not preserved on write (TKR-04/05); int64/uint64
sparse `nzval` precision; `zarr-dtype-*`; `reorder-float32-widen`;
`files-int-sparse-matrix-eltype-loss`. Best move: a read/write-time overflow
GUARD that warns/errors loudly instead of corrupting silently.

- **sparse VECTOR densified on read** (`mem-sparse-vec-read`). VERIFIED 2026-06-16:
  not cleanly fixable. `Matrix::sparseVector` is an S4 object with NO names slot -
  `names<-` errors ("invalid to use names()<- on an S4 object"). The format API
  contract requires named vectors ([[feedback_format_api_named]]), so
  `.attach_vector_axis_names` (R/utils.R:112) deliberately densifies a sparse
  vector to attach axis-entry names. R has no named-sparse-vector type equivalent
  to Julia's `NamedArray{SparseVector}`. Names win over sparsity; densify-and-name
  is the correct trade-off, already documented in the code. (Sparse MATRICES are
  fine - `dgCMatrix` carries `@Dimnames`; only the VECTOR case is constrained.)

## DEFERRED BACKENDS (feature work, user-gated) - not started

`h5df` backend; `ZipDaf` backend; **packed/sharded WRITE** (blosc/zstd encode -
read already works when a system c-blosc/libzstd is present via `configure`).
