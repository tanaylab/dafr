# Parity fix log (branch parity-fixes-2026-06-11)

Started from REPORT.md (55 confirmed divergences). Each fix is TDD: failing-first
test, then minimal fix, existing suite kept green. Run a single test against the
source with:
`R_LIBS=/tmp/dafrlib044 Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/<f>")'`

## DONE (committed)

| commit | fix | probes |
|---|---|---|
| cfa2a8f | GroupBy/CountBy labels ordered bytewise (`method="radix"`) not LC_COLLATE | qe-groupby-bytewise-order, qe-countby-bytewise-order |
| 1d26f10 | concat CollectAxis errors when a source lacks the scalar (was silent NA) | concat-scalar-collect-missing |
| b710c53 | get_vector errors on a named default whose names mismatch axis order | P4-named-default-wrong-order |

## RECLASSIFIED during fixing — these "bugs" are DELIBERATE design choices or
## entangled with dafr's type system. Do NOT blind-fix; need author decision.

- **group_names (FNV-32 vs Julia simhash)** — deliberate; `test-groups-jl-parity.R`
  documents it (shape-only assertion + rationale). DECISION: skip (user confirmed).
- **regex `~`/`!~` greedy multi-token** — deliberate; the code comment in
  `query_parse.R` (.parse_match_cmp) explains it collects multi-token patterns on
  purpose to allow UNescaped metachars. Julia takes one token + requires `\`-escape.
  Fixing reverses a deliberate convenience and would break unescaped-pattern users.
  RECOMMEND: flag, don't reverse (analogous to group_names).
- **UInt8/16/32 → R `integer` (and UInt64 → integer64)** — woven through the type
  system (`operations.R:349,552`). The UInt32≥2^31 → NA/negative and UInt64>2^63 → NA
  corruption (P1/P4-uint32, OPS-03) is an INHERENT consequence of mapping unsigned
  onto signed R integer. A real fix = map unsigned→double across the whole type
  system (large) OR a read-time overflow GUARD (warn/error). Not a one-liner.
- **set_matrix relayout default FALSE vs Julia TRUE** — documented WITH worked
  examples (`writers.R:151-160`). User approved flipping to TRUE, but it is
  high-blast-radius: changes stored state + storage size for every user and cascades
  through many tests (copies/backend/contracts). Do as a dedicated change + test sweep.
- **contracts Optional/GuaranteedOutput strictness (CTR-P1..P4)** — here R is
  STRICTER than Julia 0.3.0 (R rejects pre-existing OptionalOutput; Julia accepts).
  User approved relaxing to match Julia, but R's stricter behavior is arguably safer.
  Do deliberately; touches `contracts.R` verify_input/verify_output forbidden-set.

## REMAINING — genuinely clean(ish), resumable, priority order (silent-wrong first)

- copy_matrix on a flipped-only source: relayout-read instead of erroring (copies.R) — copy-flipped-matrix
- copy_all over a source storing both layouts: add Julia's `columns_axis >= rows_axis` guard — copy-all-both-layouts
- copy_tensor missing slice + empty=NULL: skip instead of erroring — copy-tensor-missing-slice-no-empty
- reconstruction: rewrite implicit property as String + keep empties key with nothing (reconstruction.R) — recon-int-rewrite, recon-empty-implicit-rewrite, recon-empties-keyset
- reorder: don't widen sparse index type unnecessarily (reorder.R) — reorder-uint16-indtype
- sparse VECTOR densified on read: preserve sparseVector (utils.R .attach_vector_axis_names) — mem-sparse-vec-read
- anndata dense /X orientation: write/read canonical (obs×var) transpose (anndata_format.R) — anndata-dense-X-orientation
- zarr all-zero dense chunk elision: reconstruct zeros from fill_value (zarr) — zarr-all-zero-dense-missing-chunk
- reserved `name`/`index` reader-API vectors (has_/get_vector) — P1-name-index-reserved
- complete_daf base_daf_view scope / r+ writability / view JSON x-lang (complete.R) — complete-view-scope, complete-rplus-view-readonly, complete-view-json-xlang
- http/files root metadata.json interop (cross-language serving) — files-meta-json-missing, http/files-http-client-cross (DESIGN: pick canonical layout)
- computation() overwrite plumbing (subtle: thread to contractor AND inner set) — COMP-01
- adapter copy-back insist on scalar collision — adapter-insist-collision-scalar
- chain relayout_matrix writer mutation — chain-relayout-writer-mutation

## INHERENT R LIMITS (9) — document + optional write-narrowing warning, not fixable
Float32 reduction/Convert precision; narrow/unsigned dtype width not preserved on
write; int64/uint64 sparse nzval precision; (OPS-01, OPS-03, TKR-04/05, zarr-dtype-*,
reorder-float32-widen, files-int-sparse-matrix-eltype-loss).

## DEFERRED FEATURES (user choice): h5df, ZipDaf, packed-write backends.

## UPDATE 2 — big ones

- **relayout default → TRUE: DONE** (commit c3ff8ac). Flipped set_matrix default;
  updated the 7 tests that implicitly assumed single-layout to pass relayout=FALSE.
  Full suite: 5923 pass / 0 fail after updates.
- **contracts Optional/GuaranteedOutput strictness: ANALYSED, NOT YET DONE.**
  The change is mechanically tiny — in R/contracts.R:
    - `.is_mandatory` (723): output set should be `CreatedOutput` only (drop GuaranteedOutput).
    - `.is_forbidden` (729): input set should be `CreatedOutput` only (drop GuaranteedOutput, OptionalOutput).
  Julia semantics (contracts.jl:71-78): GuaranteedOutput = "created UNLESS it already
  exists" (so pre-existing is fine, and CTR-P4 shows Julia does NOT enforce it on
  output); OptionalOutput pre-existing is also accepted.
  CAVEAT (why flagged, not auto-applied): this RELAXES dafr's stricter safety checks
  to match Julia 0.3.0 behavior that is arguably a quirk (a GuaranteedOutput that is
  never created passing verify_output is dubious). It also touches 9 test-*.R contract
  files that encode the strict behavior. Recommend confirming before loosening.
- **http/files metadata.json interop: DESIGN DECISION needed** (which on-disk layout
  is canonical — dafr metadata.zip vs Julia root metadata.json). Large; defer to a
  dedicated pass.

## UPDATE 3 — contracts decision
DECISION (user): KEEP dafr's stricter Optional/GuaranteedOutput enforcement. Do NOT
loosen to match Julia 0.3.0 (which lets a never-created GuaranteedOutput pass
verify_output — a likely DAF.jl bug). Document as "dafr intentionally stricter"
and raise upstream. CTR-P1..P4 are thus WONTFIX-by-design, not open bugs.

## UPDATE 4 — clean batch (committed)
- copy_matrix transpose-reads a flipped-only source (copy-flipped-matrix)
- reader API exposes reserved 'name'/'index' virtual vectors (P1-name-index-reserved)
- reconstruct_axis keeps the empties-mapping key for no-empties properties (recon-empties-keyset)
Total parity fixes committed: 7 (groupby/countby order, concat missing-scalar, named-default
order, relayout default, copy_matrix flipped, reserved name/index, reconstruct empties-key).

## UPDATE 5
- zarr reader reconstructs an elided all-fill chunk from fill_value (zarr-all-zero-dense-missing-chunk).
  NOTE: covers the dense-VECTOR path; the dense-matrix path likely wants the same guard (follow-up).
8 parity fixes committed. Remaining are the harder/riskier ones (recon-int-rewrite behavior
change, chain-relayout-writer-mutation, anndata X orientation round-trip, reorder index width
partly-inherent, adapter insist, complete_daf view scope) - each needs careful per-item handling.

## UPDATE 6 — fresh-attention fix
- reconstruct_axis rewrites the implicit property as a string FK into the new axis
  (recon-int-rewrite). Ported Julia's overwrite_implicit_values condition exactly
  (rewrite when not-already-string OR non-empty empty_implicit); guard case tested.
9 parity fixes committed total. Reconstruction subsystem now at full parity.

## UPDATE 7 — session 2026-06-14 (continuing)
- zarr dense MATRIX reader reconstructs an elided all-fill chunk (follow-up to vector fix) — zarr-all-zero-dense-missing-chunk (matrix)
- copy_all dedups a both-layouts matrix when relayout=TRUE (was 'existing matrix' collision) — copy-all-both-layouts
- copy_tensor with empty=NULL skips a missing slice (was 'missing matrix' error) — copy-tensor-missing-slice-no-empty
12 parity fixes committed.
RE-CLASSIFIED (flag, don't fix — like contracts, dafr's behavior is correct/safer):
- chain-relayout-writer-mutation: dafr's relayout_matrix on a write-chain succeeds with correct
  values (both layouts); Julia ERRORS. Matching Julia would degrade dafr to a quirk.

## UPDATE 8
- copy_tensor empty=NULL skips missing slice (copy-tensor-missing-slice-no-empty)
- adapter copy-back uses insist=TRUE: errors on collision instead of silent data loss (adapter-insist-collision-scalar)
13 parity fixes committed (copy_all-caller regression green: concat/backend/copies/adapter/complete/chain).

## UPDATE 9 — fresh-attention fix
- computation() threads overwrite into contractor: idempotent re-run with overwrite=TRUE
  now succeeds (COMP-01). The prior 'subtle' concern resolved: Julia keeps overwrite in the
  inner fn's kwargs (which dafr already forwards), so only the contractor call needed it.
14 parity fixes committed. Remaining: chain-relayout (FLAG, dafr correct), reorder/sparse-vec
(inherent R types), anndata-X-orientation (needs scanpy/h5ad verification - dedicated pass),
complete_daf view scope/rplus/json (complex multi-part).

## UPDATE 10 — IMPORTANT correction (relayout-default fallout)
The relayout-default->TRUE change (fix #4, c3ff8ac) had broader fallout than caught at
the time, because every regression sweep summed only `df$failed` and NOT `df$error` -
a thrown relayout_matrix("existing matrix") is an ERROR, not a failed assertion. A full
suite counting BOTH surfaced 62 errors (all "existing matrix"):
- ROOT CAUSE 1 (SOURCE BUG): R/example_data.R .load_matrix_file did set_matrix then
  relayout_matrix; with relayout=TRUE default the flip pre-exists -> collide. This broke
  example_cells_daf()/example_metacells_daf() FOR USERS (and ~53 tests via the fixture).
  Fix: set_matrix(..., relayout=FALSE).
- ROOT CAUSE 2 (SOURCE BUG): R/zarr_convert.R files<->zarr copy loop set_matrix-ed each
  stored orientation -> second collides. Broke files_to_zarr()/zarr_to_files() for any
  both-layout matrix. Fix: set_matrix(..., relayout=FALSE).
- 6 relayout/round-trip tests (zarr-format, files-matrices, files-julia-compat) updated to
  set_matrix(relayout=FALSE) to restore their single-layout premise.
FULL SUITE NOW: 6137 pass / 0 failed / 0 error (verified counting BOTH).
LESSON: always count df$error alongside df$failed.

## UPDATE 11 — anndata X orientation (session 2026-06-15)
Verified against REAL Python anndata (envs daf_env/borzoi-finetune/crested have it):
- dense /X + dense layers were written/read WITHOUT transpose -> on-disk (n_var, n_obs).
  A real scanpy/anndata file failed to load in dafr; dafr-written files were transposed.
- FIX (R/anndata_format.R): write t(X) so on-disk /X is canonical (n_obs, n_var); read via
  .read_h5ad_dense_matrix (robust reshape from known axis lengths - handles hdf5r dropping a
  singleton dim to a vector, which a blanket t() mis-shaped). Emit AnnData encoding attrs
  ('array' on /X+layers; 'dataframe' _index+column-order on obs/var so NAMES round-trip).
- Sparse /X already correct (explicit shape/indptr/indices) - unchanged.
- Regenerated inst/extdata/small_test.h5ad canonically; added committed Python-anndata fixture
  tests/testthat/fixtures/anndata_canonical.h5ad + test-parity-anndata-x-orientation.R.
- Verified: dafr reads canonical files correctly AND anndata reads dafr output correctly, for
  shapes (3,2),(2,1),(1,3). Full suite 6145 pass / 0 failed / 0 ERROR.
16 parity fixes committed.

NEW FOLLOW-UPS discovered (NOT fixed):
- obsm/varm dense embeddings have the SAME (n_obs,d)/(n_var,d) transpose bug as /X
  (read path lines ~356, write ~517 untouched). Same fix pattern; scoped out here.
- dafr cannot read anndata>=0.12 `nullable-string-array` categorical categories encoding
  ("attempt to apply non-function" in .read_h5ad_categorical). Separate read-compat gap.

METHODOLOGY FIX (this session): all regression sweeps now count df$error AND df$failed.
Earlier sweeps counted only df$failed and MISSED the relayout-default fallout (62 errors,
see UPDATE 10) and would have missed these. Always count both.

## UPDATE 12 - obsm/varm done + SHIPPED (session 2026-06-15)
- obsm/varm canonical orientation: DONE (commit 6831168). 17 parity fixes total.
- RELEASED publicly: **dafr 0.4.5** (the 17 fixes) then **0.4.6** (a doc hotfix).
  0.4.5's first CI went RED: the relayout=TRUE default broke the relayout_matrix
  roxygen *example* (set_matrix now writes both layouts -> relayout_matrix errored
  "existing matrix") and left set_matrix.Rd codoc-mismatched. The testthat suite
  does not run examples/codoc and ship.sh's gate is only devtools::test, so it slipped
  to main-branch CI. Fixed in 0.4.6 (example uses relayout=FALSE; devtools::document();
  verified locally with rcmdcheck --as-cran = 0 err/0 warn). Broken v0.4.5 tag+release
  deleted. LESSON: run `rcmdcheck --as-cran` before shipping, esp. after a signature/
  default change. (Also fixed the src/Makevars build-infra NOTE separately.)
- The remaining open gap is now tracked in **REMAINING-GAP.md** (this dir) - the
  resumable backlog for the next session. 0.4.5/0.4.6 closed everything in the
  "OPEN - genuinely fixable" set EXCEPT the 5 items listed there (anndata>=0.12
  nullable-string-array read; complete_daf view scope/rplus/json; sparse-vector
  densify-on-read; reorder index width; http/files metadata.json design).

## UPDATE 13 - anndata >= 0.12 nullable-string-array read (session 2026-06-16)
- Closed REMAINING-GAP OPEN item 1. TDD against a REAL anndata 0.12.1 fixture
  (generator `dev/fixtures/generate_anndata_nullable_strings.py`, committed
  `tests/testthat/fixtures/anndata_nullable_strings.h5ad`).
- The gap was WIDER than the original note (which named only categorical
  `categories`): in 0.12 the `nullable-string-array` `{values, mask}` group
  encoding is also used for the obs/var `_index` and for plain pandas
  `string`-dtype columns. So a real 0.12 file died at the FIRST `_index` read
  ("attempt to apply non-function"), never reaching the categorical; plain
  string columns were silently skipped as "nested column not supported".
- FIX (R/anndata_format.R): new `.read_h5ad_string_array(node)` - reads a plain
  `string-array` dataset OR a `nullable-string-array` group (mask TRUE -> NA).
  Applied at the obs/var `_index`, in `.read_h5ad_categorical` (`categories`),
  and as a new `nullable-string-array` branch in both obs/var column loops.
- Tests: `test-parity-anndata-nullable-strings.R` - real-file read (names,
  categorical, plain string column, float control) + a hand-crafted hdf5r test
  for the mask -> NA branch (anndata rewrites an NA-bearing string column as
  categorical, so the True-mask path needs the synthetic fixture).
- Verified green: all 6 anndata test files (format/facade/handlers/jl-parity/
  x-orientation/nullable-strings), 0 fail.
- NEW FOLLOW-UP (now REMAINING-GAP OPEN item 1): pandas `Int64`/`boolean`
  columns containing NA write as `nullable-integer-array`/`nullable-boolean-array`
  (same `{values, mask}` shape) and are still skipped. Generalise the helper +
  add a fixture. Scoped out here to keep the fix to the string item under test.
- Committed to dev: 02443f0. Not shipped (release held); no version bump.

## UPDATE 14 - nullable-integer / nullable-boolean columns (session 2026-06-16)
- Closed the UPDATE 13 follow-up. An NA-bearing pandas `Int64`/`boolean` column
  writes as a `nullable-integer` / `nullable-boolean` `{values, mask}` group.
  CORRECTION: the encoding strings have NO `-array` suffix (unlike
  `nullable-string-array`) - the UPDATE 13 follow-up note guessed wrong.
- FIX (R/anndata_format.R): type-agnostic `.read_h5ad_nullable(grp)` (reads
  `values`, `x[mask] <- NA` promotes to the right NA flavour - character /
  integer / logical); `.H5AD_NULLABLE_ENCODINGS` set; the obs/var column loops
  now read any of the three nullable encodings; `.read_h5ad_string_array()`
  refactored to delegate to `.read_h5ad_nullable()` (DRY, stayed green).
- Tests: extended the fixture generator with `n_umis` (nullable Int64, masked
  NA) + `is_doublet` (nullable boolean, masked NA); 2 new assertions
  (integer -> c(10L,NA,30L), logical -> c(TRUE,NA,FALSE)). All 6 anndata test
  files green (nullable-strings now 13 pass).
- NOT shipped/committed yet (working tree only); no version bump.
