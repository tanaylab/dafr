# Design: Julia JSON-schema parity (metadata.json + base_daf_view)

Date: 2026-06-19
Status: approved
Scope: dafr-native, branch `julia-json-schema-parity` off `dev`
Reference: DataAxesFormats.jl `src/files_format.jl`, `src/http_format.jl`,
`src/chains.jl`, `src/complete.jl` (@ `80aee1d`); dafr `R/files_metadata_zip.R`,
`R/files_io.R`, `R/files_daf*.R`, `R/http_format.R`, `R/complete.R`.

## 1. Problem & goal

These are the last two **cross-language DESIGN-decision** parity gaps from
`dev/parity-audit-2026-06-11/REMAINING-GAP.md`. Both are cases where dafr and
DataAxesFormats.jl use structurally incompatible JSON, so a store/chain written
by one language cannot be read by the other. The maintainer has DECIDED: **adopt
Julia's schema in all cases.**

1. **`metadata.json` interop.** dafr writes a `metadata.zip` (a ZIP bundle of the
   individual per-property `.json` descriptors); Julia writes a single root
   `metadata.json` object mapping each relative property path to its descriptor.
   Julia's HttpDaf fetches `metadata.json` once to enumerate the store. dafr's
   HttpDaf fetches `metadata.zip`. Neither reads the other.
2. **`base_daf_view`.** `complete_daf` chains persist the base view spec as a
   scalar `base_daf_view`. dafr writes positional arrays; Julia writes objects
   with stringified-tuple data keys. Incompatible.

**Goal:** dafr writes and reads Julia's exact JSON for both, verified
bidirectionally against live DataAxesFormats.jl 0.3.0.

## 2. Decisions (resolved with maintainer)

| # | Decision | Choice |
|---|----------|--------|
| A | Schema | **Adopt Julia's** for both items |
| B | metadata.zip back-compat | **Clean break**: write only `metadata.json`; delete all metadata.zip read+write code; old stores re-packed via a migration helper |
| C | Migration helper | **Repurpose the existing public `pack_files_daf_metadata()`** to (re)build `metadata.json` |
| D | Order | **Track B (base_daf_view) first** (small/self-contained), then Track A (metadata.json) |

### Non-goals
- ZipDaf / h5df backends (separate deferred feature work).
- Changing the per-property on-disk `.json` descriptors or the data file layout -
  only the *consolidated index* (Track A) and the *view-spec scalar* (Track B).

## 3. Verified ground truth (live DataAxesFormats.jl 0.3.0, 2026-06-19)

### base_daf_view (Track B) - CONFIRMED by running Julia
`JSON.json(Dict(:axes => ViewAxes, :data => ViewData))` emits **single objects**,
not arrays. For `axes = ["cell"=>"=", "renamed_cell"=>"@ cell"]`,
`data = ["quality"=>"=", ("cell","age")=>"=", ("cell","gene","umi")=>"="]`:

```json
{"axes":{"cell":"=","renamed_cell":"@ cell"},
 "data":{"quality":"=","(\"cell\", \"age\")":"=","(\"cell\", \"gene\", \"umi\")":"="}}
```

- `axes`: object, axis-name -> query string.
- `data`: object, key -> query string. Key is: a plain string for a scalar; the
  **stringified Julia tuple** for vector/matrix - literally `("cell", "age")` and
  `("cell", "gene", "umi")` (parens, double-quoted elements, `, ` separators).
- Empty `axes`/`data` are filtered out (key absent), not emitted as `{}`/`[]`.
- Julia's reader (`complete.jl parse_view_parameters`) maps `(`->`[`, `)`->`]` and
  JSON-parses the bracketed key back to a tuple. So a dafr writer must emit that
  exact key string and a dafr reader must reverse it.
- dafr currently emits `{"axes":[["cell","="]],"data":[[["cell","gene","umi"],"="]]}`
  (positional arrays) - structurally different. **This IS real work** (an earlier
  automated read concluded "already compatible"; running Julia disproved it).

### metadata.json (Track A) - from source read; descriptor bytes to be PINNED
Root `<store>/metadata.json` is a single JSON object keyed by relative property
path (sorted), values are per-type descriptors (`files_format.jl`
`metadata_json_rebuild!` ~L1616-1673, `metadata_json_append!` ~L1592-1614,
`ensure_metadata_json!` ~L1710-1736):

```json
{"axes/cell":{"format":"axis","n_entries":1000},
 "scalars/version":{"type":"String","value":"1.0"},
 "vectors/cell/batch":{"format":"dense","eltype":"String"},
 "matrices/cell/gene/UMIs":{"format":"sparse","colptr":{...},"rowval":{...},"nzval":{...}}}
```

- axes -> `{"format":"axis","n_entries":N}`; scalars -> `{"type":,"value":}`;
  vectors/matrices -> the **same descriptor dafr already writes per-property**
  (dense/sparse, incl. the v0.4.8 packed fields).
- **[PIN]** the exact descriptor JSON for every type (esp. scalar `type`/`value`
  encoding and axis `n_entries`) against a freshly generated Julia FilesDaf
  `metadata.json` before trusting this - do not re-derive.
- Julia's HttpDaf fetches `metadata.json` once and serves all enumeration from the
  parsed dict; writable FilesDaf seeds it on open, appends on `set!`, rebuilds on
  `delete!`. Read-only / absent -> rebuild (silently swallowed on read-only FS).

### dafr current state (blast radius)
- `metadata.zip` written by `R/files_metadata_zip.R`
  (`.metadata_zip_rebuild`/`.metadata_zip_append`/`.ensure_metadata_zip`), called
  from `files_daf.R` (open) and ~6 `files_daf_write.R` sites (set/delete scalar,
  add/delete axis, set vector/matrix). POSIX-only (MmapZipStore).
- Local FilesDaf reads **walk the tree** - they do NOT need the consolidated
  index, so local reads are unaffected by the switch.
- HttpDaf (`R/http_format.R`) fetches `metadata.zip` once and reads members from
  it for all enumeration/descriptor queries.
- dafr does **not** read Julia's `metadata.json` today; Julia does not read dafr's
  `metadata.zip`.

## 4. Architecture

### Track B - base_daf_view (`R/complete.R` only)
- **Writer**: replace `jsonlite::toJSON(list(axes=axes, data=data))` with a helper
  `.view_spec_to_julia_json(axes, data)` that builds the Julia object form:
  - `axes` -> named list `{<axis>: <query>}`.
  - `data` -> named list keyed by `.view_data_key(k)`: scalar (length-1) ->
    `"name"`; vector (length-2) -> `paste0('("', axis, '", "', name, '")')`;
    matrix (length-3) -> `("rows", "cols", "name")`.
  - Drop empty axes/data (omit the key) to match Julia's `filter!`.
  - Emit with `jsonlite::toJSON(..., auto_unbox = TRUE)`; verify the object (not
    array) shape and exact key bytes against the §3 ground truth.
- **Reader**: `.view_spec_from_julia_json(spec)` (replacing the array-shaped
  `.normalise_json_spec`): for each `axes`/`data` object, turn name->query into
  dafr's internal `list(key, query)` pairs; decode a tuple key by mapping
  `(`->`[`, `)`->`]` and `jsonlite::fromJSON` -> character vector.
- The intra-dafr round-trip must still pass; existing
  `test-complete*.R`/`test-chains*.R` updated to the new on-disk bytes.

### Track A - metadata.json
- **Create `R/files_metadata_json.R`** (mirrors the deleted zip module):
  - `.metadata_json_descriptor(root, rel_path, kind)` - build the per-type
    descriptor (axis `n_entries`, scalar `type`/`value`, vector/matrix reuse the
    existing per-property descriptor builder). **[PIN]** to Julia.
  - `.metadata_json_rebuild(root)` - tree walk (sorted), assemble the object,
    write `<root>/metadata.json`.
  - `.metadata_json_append(root, rel_path, descriptor)` - insert
    `,"<rel_path>":<descriptor>` before the trailing `}` (byte-surgery; rebuild on
    collision / first-write), matching Julia's append.
  - `.metadata_json_ensure(root)` - rebuild if missing.
  - `.metadata_json_read(bytes|path)` + lookup helpers (for HttpDaf).
- **Swap call sites** in `files_daf.R` / `files_daf_write.R` from `_zip_` to
  `_json_` (same append-on-set / rebuild-on-delete structure). **Delete**
  `R/files_metadata_zip.R` and its tests; drop the MmapZipStore-for-metadata path.
- **HttpDaf** (`R/http_format.R`): fetch `<url>/metadata.json` once, parse to a
  dict, serve all enumeration/descriptor queries from it; remove the
  fetch-zip/unzip path. Missing metadata.json -> actionable error naming
  `pack_files_daf_metadata`.
- **Migration**: `pack_files_daf_metadata()` (public) now (re)writes
  `metadata.json` from the tree - the documented path to convert pre-existing
  stores (metadata.zip ignored; optionally removed).
- **Windows**: a plain file write, so the consolidated index now works on Windows
  (it did not under MmapZipStore) - a side benefit, not a goal.

## 5. Error handling
- HttpDaf open against a store with no `metadata.json` -> stop with an actionable
  message (re-pack with `pack_files_daf_metadata`).
- `metadata.json` parse failure on a writable local store -> rebuild from the tree
  (mirrors Julia); on read-only/HTTP -> error.
- base_daf_view with an unrecognized key shape -> stop with the offending key.

## 6. Testing (fixture- + interop-driven)
- **Track B**: unit test the writer emits the §3 ground-truth bytes for axis /
  rename / scalar / vector / matrix views; reader round-trips; intra-dafr
  complete_daf round-trip; **live interop** - Julia opens a dafr-written
  complete_daf chain and reads through the view, and dafr opens a Julia-written
  chain.
- **Track A**: pin each descriptor type against a fresh Julia FilesDaf
  `metadata.json` (byte-diff per entry); FilesDaf write -> read-back; **live
  interop** - Julia's FilesDaf/HttpDaf reads a dafr-written store's metadata.json,
  and dafr's FilesDaf/HttpDaf reads a Julia-written store; HttpDaf enumeration via
  metadata.json; the migration helper rebuilds a valid metadata.json.
- Full suite green (libs present) + CRAN no-lib + rcmdcheck ship gate (the
  `error_on=warning` + `NOT_CRAN=true`-on-CI lesson from 0.4.8: no non-portable or
  `skip_on_cran`-only-guarded assertions).

## 7. Phasing (-> writing-plans)
1. **Track B**: writer + reader + unit/round-trip tests + Julia interop.
2. **Track A**: `files_metadata_json.R` (descriptors pinned) -> swap call sites +
   delete zip module -> HttpDaf reader -> migration helper -> tests + interop.
3. Docs (NEWS, gap-doc update, remove metadata.zip docs), full verification, ship.

Each track is independently testable and shippable; Track B is the fast first win.

## 8. Risks
- **Descriptor byte-exactness** (Track A scalar/axis/vector/matrix) - mitigated by
  per-entry diff against a real Julia metadata.json fixture.
- **Tuple-key string exactness** (Track B) - mitigated by the §3 live capture +
  a round-trip-through-Julia interop test.
- **metadata.json append O(file) per set** - matches Julia; acceptable. Rebuild on
  delete/collision as Julia does.
- **HttpDaf clean break** - old dafr stores served over HTTP need a re-pack; the
  migration helper + actionable error cover it (accepted per Decision B).
