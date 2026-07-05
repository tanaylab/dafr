# ZipDaf backend for dafr - design

Date: 2026-07-05
Status: approved (design), pre-implementation

## Goal

Add a `ZipDaf` backend: a whole Daf store held in a single append-only
`.daf.zip` archive, read + write, byte-compatible with
`DataAxesFormats.jl` 0.3.0 (`src/zip_files.jl`). Closes one of the two
deliberately-deferred backends (the other, H5df, is a separate later slice).

Not a recent parity regression: the two newest Julia commits (`794b75b`
NamedArray key-type, `a8a245a`/`80aee1d` StringViews 2.0) are Julia
type-system / package-manager internals with no R analog, and the June
`nzval_is_present` v1.0-read fix is already implemented and tested in dafr.
ZipDaf and H5df are the only genuine gaps.

## Key insight: both hard halves already exist in dafr

The naive estimate (~1500 Julia LOC for `zip_files.jl` + ~1263 for
`mmap_zip_store.jl`) overstates the work, because dafr already built the
infrastructure for the `.daf.zarr.zip` (Zarr-in-zip) backend:

1. **The container** - `MmapZipStore` (C++, `src/mmap_zip_store.cpp`, opened
   via `dafr_mmap_zip_open`). A crash-safe, append-only, mmap-backed ZIP
   key-value store exposing `store_get_bytes / store_set_bytes / store_list
   (prefix) / store_exists / store_delete` (S7 methods in `R/zarr_store.R`).
   Already Julia-interop-verified for `.daf.zarr.zip`; extensive
   recovery/rollback tests (`test-mmap-zip-store-*.R`). Smoke-tested with
   arbitrary Daf keys (`daf.json`, `scalars/pi.json`, `vectors/cell/x.data`):
   set/get/exists/prefix-list and persistence across reopen all work.

2. **The serialization** - dafr's FilesDaf already writes the exact Julia
   on-disk layout: `<name>.json` descriptors + `.data` (dense numeric), `.txt`
   (dense string / axes), `.nzind`/`.nzval` (sparse vector),
   `.colptr`/`.rowval`/`.nzval` (sparse matrix), `.nztxt` (sparse string), and
   packed `.zip` shards. Byte-identical to Julia's FilesDaf/ZipDaf.

ZipDaf = the FilesDaf key layout stored **in the zip** instead of a directory.
The only new code is a thin format-API bridge.

## Layout

A `.daf.zip` archive holds the same relative keys as a FilesDaf directory:

```
daf.json                       # version marker {"version":[MAJOR,MINOR]}
scalars/<name>.json
axes/<axis>.txt                # newline-delimited entry names
vectors/<axis>/<name>.json     # + .data / .txt / .nzind / .nzval / .nztxt / .zip
matrices/<rows>/<cols>/<name>.json  # + .data / .colptr / .rowval / .nzval / ...
```

Differences from a FilesDaf directory (all match Julia, all intentional):
- **No `metadata.json`.** The ZIP central directory is the enumeration index.
  A writable open strips any stray `metadata.json` sidecar (guards against a
  `zip -r foo.daf` snapshot leaking in).
- **`daf.json` marker present** (FilesDaf directories don't need it; the ZIP
  needs a data-set marker key). Written on create, verified on open.
- **Append-only.** Overwriting or deleting a property raises a clean error -
  identical to Julia (`format_delete_*` -> `append_only_error`; the store's
  `store_set_bytes` on an existing key and `store_delete` both throw). No
  tombstone/shadow logic; ZipDaf is write-once-per-key.

## Components

### `R/zip_daf.R` (new)

- S7 classes `ZipDaf` (writer) and `ZipDafReadOnly` (reader), each holding a
  `MmapZipStore` plus name/mode/path, mirroring the FilesDaf class pair and its
  format-API method structure.
- The `format_api` generic methods (~25): `format_has_scalar/_get_scalar/
  _set_scalar/_scalars_set`; `format_has_axis/_add_axis/_axis_vector/
  _axis_length/_axes_set`; `format_has_vector/_get_vector/_set_vector/
  _vectors_set`; `format_has_matrix/_get_matrix/_set_matrix/_matrices_set/
  _relayout_matrix`; `format_description_header`; and the `format_delete_*`
  methods (raise append-only error). Each reads/writes bytes via the store on
  FilesDaf-layout keys and (de)serializes with the shared cores.
- Enumeration (`*_set`) via `store_list(prefix)` filtered to `.json` keys,
  mirroring Julia's `entries_in_directory`.

### `zip_daf(path, mode = "r", name = NULL)` (new, exported)

Parse the `.daf.zip` path, `new_mmap_zip_store(path, mode)`, then:
- create: write `daf.json`; open: verify `daf.json` marker exists.
- writable open: strip `metadata.json` sidecar if present.
- default `name` from the `scalars/name.json` scalar if present, else the path.
Returns a `ZipDaf` (writable modes) or `ZipDafReadOnly` (`r`).

### `open_daf` dispatch (edit `R/open_daf.R`)

Route a `.daf.zip` path to `zip_daf()`. (The `.dafs.zip#/group` grouped form is
out of scope - see Deferred.)

### Serialization seam (edit `R/files_io.R`, minimal)

FilesDaf's helpers read/write via file **paths**; ZipDaf has **bytes**. Extract
the pure bytes<->value cores so both backends share one source of truth:
- dense: `.encode_dense(value, dtype) -> raw` / `.decode_dense(raw, n, dtype)`
- scalar: `.encode_scalar_json(value) -> raw` / `.decode_scalar_json(raw)`
- descriptor: `.encode_descriptor_*(...) -> raw` / `.decode_descriptor(raw)`
- lines (axes / string vectors): `.encode_lines(x) -> raw` / `.decode_lines(raw)`

FilesDaf's existing path helpers become thin wrappers
(`writeBin(encode(...), path)` / `decode(readBin(path))`) - behavior unchanged,
tests stay green. Packed-shard decoders (`.files_packed_decode_vector/_matrix`)
already take raw bytes; reuse directly.

## Data flow

- **Read** `get_vector(d, axis, name)`: read `vectors/<axis>/<name>.json` bytes
  -> parse descriptor -> for each needed component `store_get_bytes` the
  `.data/.nzind/...` key -> decode via core -> attach axis names (existing
  FilesDaf name-attach path). Packed component: `store_get_bytes` the `.zip`
  shard bytes -> existing packed decoder.
- **Write** `set_vector`: serialize descriptor + components to bytes -> one
  `store_set_bytes` per key. Same for scalars/axes/matrices.
- **Cache group**: mirror FilesDaf's classifier (character/factor ->
  MEMORY_DATA, else MAPPED_DATA).

## Error handling

- Overwrite/delete of an existing property: clean append-only error (matches
  Julia), surfaced through the `format_delete_*` methods and store.
- Open non-existent archive without a create mode: "no such file" error.
- Missing/invalid `daf.json` marker on open: "not a daf data set" error.
- Wrong-mode HTTP/etc.: unchanged (`open_daf` already guards).

## Testing

- `test-zip-daf.R`: R-internal round-trips - scalars (float/int64/string),
  axes, dense/sparse numeric vectors, string vectors, Bool sparse (nzval
  absent), dense/sparse matrices; reopen persistence; append-only error on
  overwrite/delete; `open_daf(".daf.zip")` dispatch.
- `test-zip-daf-julia-compat.R`: bidirectional interop gated on the julia env
  (reuse `.have_julia_env()` / `run_julia()` from the FilesDaf compat test) -
  R-writes -> Julia-reads-and-asserts, Julia-writes -> R-reads-and-asserts.
- Packed round-trip: `zip_daf(..., packed = TRUE)` if the constructor exposes
  the packed default (reuse FilesDaf's `packed` option plumbing).

## Deferred (YAGNI)

- **`.dafs.zip#/group` grouped multi-daf archives** - the `#/group` addressing
  and `group_prefix` on every key. Adds real addressing complexity for a form
  no current dafr caller needs. Add when a concrete need appears.
- **H5df backend** - separate later slice.

## Risks / open items (resolve during implementation)

- Confirm the high-level `set_*` framework path routes an overwrite through
  `format_delete_*` so the append-only error is raised cleanly (not a crash).
- Confirm mmap/altrep zero-copy read classification (MemoryData vs MappedData)
  matches FilesDaf for the store's returned bytes.
- Confirm `daf.json` marker byte content matches Julia
  (`{"version":[MAJOR,MINOR]}\n`) for cross-language marker verification.
