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
