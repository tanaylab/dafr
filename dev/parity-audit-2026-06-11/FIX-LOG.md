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
