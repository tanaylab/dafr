# Changelog

## dafr 0.2.0 (in development)

### queries.jl parity — Slice 3 (E6, E9, plus three T-class wins)

Slice 3 closes the last two semantic divergences plus three T-class
error-text items that turned out to be tractable.

- **E6 (vector-by-vector chain implicit AsAxis)** — fixed.
  `.apply_chained_lookup_vector` now falls back to the property name’s
  base (everything up to the last `.`) when the property name itself is
  not an axis. Mirrors Julia’s `ensure_vector_is_axis`.
  `@ cell : type.manual : color` now resolves through the `type` axis.
- **E6 (matrix-column slice auto-relayout)** — fixed.
  `.apply_matrix_column_by_axis` now auto-transposes when the matrix is
  stored on the swapped orientation (`(cols, rows)` instead of
  `(rows, cols)`). Mirrors `.apply_lookup_matrix`’s existing relayout.
- **E9 (auto-relayout)** — already worked; no skip remained for this
  class.
- **T-class: numeric-reduction-on-character-matrix** —
  `.apply_reduction` now type-checks before dispatching
  `Sum`/`Mean`/`Max`/etc., raising a `non-numeric input` error instead
  of letting base R’s `'x' must be numeric` leak through. Closes the
  `>| Sum` / `>- Sum` on string-matrix tests.
- **T-class: IfNot sentinel coercion error** —
  `.apply_chained_lookup_vector` now wraps the sentinel coercion in
  `withCallingHandlers` and converts an `NAs introduced by coercion`
  warning into a `cannot parse IfNot sentinel <s> as <type>` error.
  Closes the `?? foo : phase` test.

Suite: `FAIL 0 | WARN 1 | SKIP 6 | PASS 4619` (+9 over Slice 2). The
final 2 parity skips are both E11 (kernel-level type-strictness; out of
scope per Slice 2 exit).

### queries.jl parity — Slice 2 (E3, E7, E8 closed; E11 reclassified)

- **E3** (matrix-slice-as-mask, `[ UMIs @ gene = A > 0 ]`) — already
  worked in the evaluator; un-skipped.
- **E7** (group-by / count-by matrix-slice) — already worked across
  every passing matrix/group/\* test; no skip remained for this class.
- **E8** (cross-tabulate `: vec * other =@`) — already worked; the
  failing test asserted `sum=2` but the Julia reference (queries.jl
  line 1134) shows `sum=3`. Test expectation corrected.
- **E11** reclassified as **T-class** (error-text-only divergence). R’s
  kernel promotes integer matrices to double during `Sum` reduction,
  losing the type signal Julia uses to raise `InexactError` when the
  IfMissing default (`0.5`) is non-integer for an integer matrix. Real
  result is identical otherwise; only the error case diverges.

Suite: `FAIL 0 | WARN 1 | SKIP 12 | PASS 4610` (+7 over Slice 1b).

### queries.jl parity — Slice 1b (quick wins: E4, E5, E10, B7, B9, API1)

Six divergences from the Julia parity audit closed in one slice. 58
fewer parity-test skips, 113 more passes vs the post-Slice-1a baseline.

- **E4** (top-level comparator after `:` / `::`). Already worked in the
  evaluator; un-skipped 7 tests. `@ cell : score < 1.0` returns a named
  bool vector; `@ cell @ gene :: UMIs > 0` returns a named bool matrix.
- **E5** (`:` / `::` standalone with IfMissing fallback). Fixed: the
  `pending_if_missing` state-key shim in `R/query_eval.R` keeps the
  default alive across the lookahead reset that fires after every
  lookup. `: age || 1 @ cell = X` now returns `1` when `age` is missing
  instead of erroring. `:: UMIs || 0 @ cell = Y @ gene = B` likewise.
- **E10** (regex escape sequences in masks). Already worked; un-skipped.
- **B7** ([`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md) /
  [`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md) / etc.
  builders canonical form). Fixed: the reduction builders in
  `R/query_ast.R` now emit `ReduceToScalar` nodes (`>> Sum`) instead of
  broken `Eltwise` nodes (`% Sum`, which erred at runtime because the
  eltwise registry has no `Sum`). `ReduceToColumn` / `ReduceToRow`
  rewrap accepts both old `Eltwise` and new `ReduceToScalar` trailing
  nodes for back-compat.
- **B9** (`query_axis_name` introspection strictness). Fixed: the
  function now skips `Axis` nodes inside `[ ... ]` mask scopes when
  counting outer-axis references, so compound-mask and matrix-derived
  sub-mask queries get the same axis answer that `get_query` resolves.
- **API1** (`get_dataframe` named-list column-spec). Fixed: the
  bare-name shorthand form (`list("age", doublet = "is_doublet")`) now
  auto-prefixes to `@ axis : age`, mirroring Julia’s
  `["age", "doublet" => ":is_doublet"]`. Named-list
  (`list(out = ":query")`) and complex axis-traversal forms already
  worked.

Cumulative test-suite tally: `FAIL 0 | WARN 1 | SKIP 14 | PASS 4603`
(was `SKIP 72 | PASS 4489` before Slice 1a).

Remaining skips: - E3 (matrix-slice-as-mask) — Slice 2 - E6
(vector-by-vector / matrix-then-vector chains) — Slice 3 - E8
(cross-tabulate `* type =@`) — Slice 2 - E11 (as_axis group with `=@`
IfMissing-coverage) — Slice 2 - 3 T-class error-text-only divergences
(harmless; Julia and R reach the same failure mode through different
error wordings)

### queries.jl parity — Slice 1a (N1: named axis-listing)

`get_query` now returns a named character vector for axis-listing
queries, matching Julia’s NamedVector convention where names equal
entries.

- `get_query(d, "@ cell")` → `c(c1 = "c1", c2 = "c2", c3 = "c3")` (was
  unnamed `c("c1", "c2", "c3")`).
- `get_query(d, "@ donor [ age > 60 ]")` → `c(d3 = "d3", d4 = "d4")`
  (was unnamed `c("d3", "d4")`).
- Lookup vectors / matrices already carried names from S1; this closes
  N1 for the remaining axis-listing paths.

Changes: - `R/query_eval.R::.apply_axis` and `.apply_end_mask` set
`names(value) <- value` on the axis-entry character. - Test assertions
in `test-query-eval-lookups.R`, `test-query-eval-masks.R`,
`test-query-mask-variants.R` updated to the named contract. - New
regression tests in `test-query-result-names.R` pin the bare and masked
axis-listing paths.

### queries.jl parity port (P1-P3 + B-port) — caught up on main

Six queries-jl-parity tests previously skipped on main now pass:

- **Parser error format** (`R/query_parse.R`): error messages for
  unknown eltwise / reduction operations, unknown parameters, and
  repeated parameters no longer wrap names in curly quotes via `sQuote`;
  the wording moves to the Julia DAF literal form
  (`"parameter:" + colon`, no eltwise/reduction qualifier on
  `"for the operation:"`).
- **Empty-string round-trip** (`R/query_ast.R`): `escape_value("")`
  returns `''` (mirroring Julia’s `escape_value("") == "''"`);
  `unescape_value("''")` is the symmetric inverse and returns `""`.
- **Empty-matrix reduction semantics** (`R/query_eval.R`): both
  reduce-axis-empty and output-axis-empty matrix reductions now raise
  `"no IfMissing value specified for reducing an empty matrix"` when no
  `IfMissing` default is set — the previous output-axis-empty branch
  silently returned an empty vector.

### fix(readers): cache-layering defensive name re-apply

Restored the `if (is.null(names(out))) names(out) <- entries` defense in
`R/readers.R::get_vector` and an analogous dimnames-guard in
`get_matrix`, which the original S1 slice dropped on the assumption that
`format_get_*()` returns are always named. They are at the format layer,
but the format backend’s own `mapped` cache tier holds bare values for
canonical storage, and `get_vector`’s cache_lookup against the same tier
would hit the bare entry. The restored guard preserves the user-facing
named contract.

### S1 — Names everywhere on `format_get_*`

The format-API contract is now: every
`format_get_vector(daf, axis, name)` returns a
`.cache_group_value(named_vector, group)` whose `$value` is a named
atomic vector with `names = format_axis_array(daf, axis)$value`, and
every `format_get_matrix(daf, rows_axis, columns_axis, name)` returns a
`.cache_group_value(<matrix>, group)` whose `$value`’s dimnames are
`list(rows-axis entries, cols-axis entries)`.

- The contract is enforced for every backend: `MemoryDaf`, `FilesDaf` /
  `FilesDafReadOnly`, `ZarrDaf` / `ZarrDafReadOnly`, `HttpDaf`, and
  propagates automatically through wrapper layers (`ReadOnlyChainDaf` /
  `WriteChainDaf`, `ContractDaf`, `ViewDaf`).
- ALTREP-mmap vectors (`mmap_real` / `mmap_int` / `mmap_lgl`) preserve
  ALTREP status across `names<-`, via a new `Duplicate_method` on each
  ALTREP class. The mmap region is shared rather than copied when R
  duplicates the wrapper.
- Internal cleanup: `get_vector` / `get_matrix` no longer reattach names
  defensively; `query_eval.R::.apply_chained_lookup_vector` now asserts
  the named contract instead of working around it.
- Bug fix surfaced by the slice: `R/concat.R::.concat_axis_vector` now
  strips intermediate names (via
  [`unname()`](https://rdrr.io/r/base/unname.html)) before calling
  `format_set_vector` (whose `.validate_vector_value` correctly rejects
  names that don’t match the destination axis). Latent risk in
  `.concat_merge_vector` flagged for follow-up.
- Storage stays canonical: `format_set_*` continues to strip names so
  the on-disk / in-memory representation only carries axis entries on
  the axis itself, not redundantly on every value.
- Test suite ported from dev’s S1 slice:
  `tests/testthat/test-format-api-named-returns.R` (35 contract tests
  covering memory + files + chain + contract + view + round-trip
  - as_anndata) and `tests/testthat/test-queries-jl-parity.R` (134 PASS
    / 74 SKIP; 6 of the SKIPs are pre-existing parser/evaluator
    divergences on main awaiting a P1-P5 / B1-B3 port from dev).

### Carry-over from the previously-numbered v0.3.0: queries.jl literal-parity slice — B4-B6 + E1, E2

Closes the remaining behaviour and evaluator gaps surfaced by a literal
port of `~/src/DataAxesFormats.jl/test/queries.jl`. The related
parser-strictness (P1-P5) was already in place on `main` from earlier
slices; this bumps R-side parity to match DAF.jl on every test that does
not hit one of the still-deferred IDs (E3-E11, B7-B9, API1, N1)
catalogued during the port.

#### Evaluator behaviour

- **B4.** `% <Op>` element-wise on a numeric scalar applies the op (was:
  `'%' eltwise requires vector or matrix in scope`). Numeric R ops
  handle scalar natively; string scalars still error from base R.
- **B5.** Partial / unconsumed queries (e.g. `@ cell @ gene`) now error
  with `invalid query: <canonical>` (was: silent `NULL`).
- **B6.** A second `?` after a fully-resolved Names result errors
  `'?' is not valid after <kind>` (was: silently re-listed axes).

Plus:
[`canonical_query()`](https://tanaylab.github.io/dafr/reference/canonical_query.md)
now accepts a `DafrQuery` directly (uses the stored canonical string).

#### Evaluator additions

- **E1.** `[ filter ]` after `@ rows @ cols` is now valid; the mask
  filters the most-recently-entered axis (cols). The matrix lookup
  honours both `row_indices` and the new `col_indices`. Cols-mask
  reductions hit the existing empty-matrix IfMissing branch.
- **E2.** Virtual `name` property on every axis. `[ name = X ]`,
  logical-mask combinators on `name`, and `: name` lookups now return
  the axis-entry vector (`format_axis_array(daf, axis)`). The
  dataframe-side `name` column remains tracked under API1.

Plus: IfMissing defaults in vector and matrix lookups now route through
`.coerce_if_missing_default` so the fill type matches the type of a real
default (e.g. `: age || 1` returns an integer column, not a character
one).

## dafr 0.2.1

### R-only quirks vs Julia parity (audit pass)

Two correctness gaps surfaced by an audit of R-specific footguns that
the ported `queries.jl` test suite does not exercise:

- **Mask comparators on factor properties.** `[ prop < value ]`,
  `[ prop > value ]`, etc. on a property stored as a factor (e.g. an
  h5ad categorical loaded via the `categorical` encoding) previously
  returned `NA` (unordered factor) or compared level codes (ordered
  factor) — both diverging from `DataAxesFormats.jl`, which compares the
  stored string lexically (`queries.jl:2261-2400`). Mask helpers now
  coerce factor → character before stashing the comparator-target
  vector, mirroring the factor branch already in `.as_booleans` (the
  `ba9baa7` precedent for the truthy `[ prop ]` mask).
- **`>> Mode` on character / factor.** `Mode` previously rejected
  non-numeric/non-logical inputs even though the Julia operation is
  documented as supporting strings (`operations.jl:1058-1115`,
  `supports_strings(::Mode) = true`). `>> Mode` and `>| Mode` now accept
  character properties and factors (factor → character at the boundary,
  matching Julia’s `CategoricalVector` → `Vector{String}` normalization
  at `anndata_format.jl:403`). The grouped fast-path covers factor
  inputs via the existing `.grouped_mode_character` helper.

Build hygiene: `.Rbuildignore` excludes `AGENTS.md` and `CLAUDE.md`
(development-only files) so they no longer surface as a
`top-level files` NOTE under `R CMD check`.

## dafr 0.2.0

### Reader-API parity polish

- `description(daf)` now emits per-format header lines (`url:` for
  `HttpDaf`, `path:` + `mode:` for `FilesDaf` / `ZarrDaf`) after the
  `name:` / `type:` lines. New internal `format_description_header`
  generic mirrors upstream `Formats.format_description_header`; the
  default emits just `type: <ClassName>`, per-format methods extend it.
- New exported `is_leaf(daf)` predicate. Returns `TRUE` for storage
  formats that own their state directly (`MemoryDaf`, `FilesDaf` /
  `FilesDafReadOnly`, `ZarrDaf` / `ZarrDafReadOnly`, `HttpDaf`) and
  `FALSE` for wrappers (`ReadOnlyChainDaf`, `WriteChainDaf`,
  `ContractDaf`, `ViewDaf`). Mirrors upstream `Readers.is_leaf`.
- [`reorder_axes()`](https://tanaylab.github.io/dafr/reference/reorder_axes.md)
  now rejects non-leaf inputs up front with a clear
  `"non-leaf type: <Class> for the daf data: <name> given to reorder_axes"`
  error (previously surfaced as a cryptic missing-method dispatch).
- [`complete_path()`](https://tanaylab.github.io/dafr/reference/complete_path.md)
  now works for `ZarrDaf` (returns the directory path, `:memory:`, zip
  path, or HTTP URL — whichever store path the constructor recorded).
  Was previously `FilesDaf`-only.

### HttpDaf + HttpStore + metadata.zip parity

- New `HttpDaf` backend for read-only access to a `FilesDaf` directory
  served over HTTP(S). The client downloads `metadata.zip` once at open
  and serves all JSON metadata from it; non-JSON payloads (`.txt` /
  `.data` / `.nzind` / `.nzval` / `.colptr` / `.rowval` / `.nztxt`) are
  fetched lazily via one HTTP GET each.
- New `HttpStore` (`R/http_store.R`) implements the `R/zarr_store.R`
  store interface over HTTP. `zarr_daf("https://host/foo.daf.zarr/")`
  routes through it; reads `.zmetadata` once and serves
  `.zarray`/`.zattrs`/`.zgroup` from there.
- `open_daf("https://...")` dispatches to `http_daf` or `zarr_daf` based
  on the URL suffix. HTTP backends are read-only; writable modes
  hard-error. `*.daf.zarr.zip` URLs are explicitly out of scope and
  redirect users to opening the underlying `.daf.zarr` directory.
- New `pack_files_daf_metadata(path)` exported helper to bundle a
  `FilesDaf` tree’s JSON metadata into `metadata.zip` (for trees written
  by older dafr or modified outside dafr).
- **`FilesDaf` now maintains `metadata.zip` automatically** on every
  `set_*` / `delete_*` / `add_axis` / `delete_axis` / `reorder_axes`
  operation, plus a one-shot rebuild on writable open if the bundle is
  missing. Mirrors upstream `DataAxesFormats.jl::FilesFormat`. Pre-0.2.0
  stores are picked up automatically the first time they’re opened with
  mode `"r+"` or `"w+"`.
- `axes/metadata.json` sidecar now maintained by FilesDaf (sorted JSON
  array of axis names). Required by HTTP clients to enumerate axes
  without GET-ing every `axes/*.txt`.
- New `Imports`: `httr2` (and transitively `curl`).

### MmapZipStore + Zarr zip backend

- New `MmapZipStore` (C++ in `src/mmap_zip_store.cpp`) backs `ZarrDaf`
  with a single ZIP archive on the local filesystem.
  [`open_daf()`](https://tanaylab.github.io/dafr/reference/open_daf.md)
  and
  [`zarr_daf()`](https://tanaylab.github.io/dafr/reference/zarr_daf.md)
  now accept `.daf.zarr.zip` paths and return a working `ZarrDaf` /
  `ZarrDafReadOnly`.
- Reads use a shared mmap of the archive: stored (method-0) entries are
  returned as zero-copy `ALTREP RAW` views via a new `ZipRawAltrep`
  class. Deflate-compressed (method-8) entries are decompressed on
  demand via system zlib; deflate64 / other methods raise a clear error
  pointing to a stored / deflate re-save.
- Writes append entries via upstream’s two-step commit protocol (commit
  central directory + EOCD first, then write the local file header and
  data into the now-sparse hole). Crash-safe: a writable open’s recovery
  pass detects partial commits via tail validation (LFH signature + data
  CRC32) and rolls back the trailing run of invalid entries before
  returning. Internal tick-counter hooks at every commit-able decision
  point let recovery be tested deterministically (5 tick points; tests
  gated on `NOT_CRAN=true`).
- Always emits ZIP64 (per upstream `DataAxesFormats.jl`); every local
  file header is padded with a `0xDAF1` extra field so the data region
  starts at an 8-byte-aligned file offset (zero-copy unaligned-load
  safety on every host architecture).
- ALTREP safety net: when the store closes (or is GC’d), every
  outstanding ALTREP vector it produced is deactivated —
  [`length()`](https://rdrr.io/r/base/length.html) returns 0 and
  `Dataptr()` returns a stable inert byte. R callers who keep references
  past close get clean empty raws instead of segfaults.
- Internal-only `dafr:::dafr_mmap_zip_reserve()` /
  `dafr:::dafr_mmap_zip_patch_crc()` expose two-phase fill for large
  sparse arrays (writable in-place ALTREP view + post-fill CRC patch).
  Crash between reserve and patch rolls back via the same CRC-mismatch
  path as ordinary partial commits.
- `SystemRequirements`: zlib (linked via `-lz`).
- Cross-language smoke: dafr-written `.daf.zarr.zip` archives open
  cleanly in Python via `zipfile` and
  `zarr.open(zarr.storage.ZipStore(...))`. Foreign zips written by
  `python -m zipfile` (stored or deflate) open cleanly in dafr.
- Mirrors `DataAxesFormats.jl` `mmap_zip_store.jl` (~1070 LOC of Julia
  ported to ~1300 LOC of C++ + cpp11 + ALTREP).

### ZarrDaf backend

- New `zarr_daf(uri, mode, name)` backend reading and writing Zarr v2.
  Two store impls: `DirStore` (filesystem directory tree) and
  `DictStore` (in-memory). Zip-backed Zarr is also supported via the
  `MmapZipStore` backend (see above).
- New `files_to_zarr(src, dst)` and `zarr_to_files(src, dst)` conversion
  helpers (same-filesystem only; correctness-first implementation
  re-encodes through the public API; hard-link optimization deferred as
  a perf follow-up).
- `open_daf("foo.daf.zarr")` now returns a `ZarrDaf`.
- Compression policy: dafr writes Zarr chunks uncompressed; reads
  uncompressed and gzip; rejects blosc/zstd/lz4 with a clear error
  pointing to re-save with `compressor=None`.
- Sparse layouts mirror upstream `DataAxesFormats.jl`: 1-based on-disk
  indices for `nzind` / `colptr` / `rowval`; sparse-Bool all-`TRUE`
  skips `nzval` (storage compaction). Cross-language parity is verified
  via gated Python `zarr.open()` smoke tests.
- Mirrors `DataAxesFormats.jl` v0.2.0 commits `ea4b5f9` (Zarr v2
  directory tree), `8cc3ff6` (in-memory store), `47e7693` (CRC fix — N/A
  for our in-memory layer), `79034fd` (`.zmetadata` consolidation),
  `46d4ab2` (Files↔︎Zarr conversion).

### reorder_axes() + open_daf() factory

- New `reorder_axes(daf, axis = perm, ...)` permutes axis entries in
  place, rewriting every vector and matrix that depends on the axis. On
  `files_daf` the operation is crash-recoverable via a
  `.reorder.backup/` directory of hardlinks; on the next
  `files_daf(path, mode = "r+" | "w+")` open, any in-progress reorder is
  automatically rolled back to the pre-reorder state.
- New `reset_reorder_axes(daf)` to manually trigger recovery (mostly
  redundant given the auto-recovery on open).
- New `open_daf(uri, mode, name)` factory function — dispatches on path
  / URL pattern. `memory://` (or no path) → `memory_daf`, filesystem
  path → `files_daf`, `*.daf.zarr` / `*.daf.zarr.zip` → `zarr_daf`,
  `http(s)://` → `http_daf`. The factory replaces the previous
  filesystem-only `open_daf` from `R/complete.R`.
- Mirrors `DataAxesFormats.jl` v0.2.0 commits `90301ff`, `070bd34` (axis
  reordering) and `b40377f` (`open_daf` factory).

### Internal: per-item cache_group refactor

The internal format API now returns per-item cache classifications,
matching `DataAxesFormats.jl` v0.2.0 (upstream commit `49fbba1`). **No
user-visible behavior change.**

- Every backend `format_get_*` method (scalar/axis_array/vector/matrix)
  returns `list(value, cache_group)` instead of a bare value.
- Every backend `format_set_*` method returns the cache_group constant
  for the just-written value (or `NULL`) instead of
  [`invisible()`](https://rdrr.io/r/base/invisible.html).
- New exported character constants `MEMORY_DATA`, `MAPPED_DATA`,
  `QUERY_DATA` — accepted by `empty_cache(daf, clear = ...)` /
  `keep = ...` alongside the existing lowercase forms.
- The reader-level cache (`R/readers.R`) now consults the
  backend-returned cache_group when storing fresh reads, instead of
  hardcoding the `"memory"` tier. mmap-eligible reads on `files_daf` now
  correctly land in the `"mapped"` tier.
- Per-item classification: `files_daf` returns `MEMORY_DATA` for
  string/factor reads (R’s CHARSXP cache makes mmap moot for strings)
  and `MAPPED_DATA` for everything else. Matches upstream’s structural
  classification — no size thresholds.

This refactor is preparatory for the `ZarrDaf` and `HttpDaf` backends,
which require per-item classification to drive their internal caching.

## dafr 0.1.0 (development)

### Query DSL: Julia-parity parser additions

Three DataAxesFormats.jl query-DSL features that were previously missing
have been implemented, closing the last semantic gaps between `dafr` and
the upstream Julia package:

- `>> Reduction` reduces a vector or matrix to a scalar (e.g.
  `@ gene : is_lateral >> Sum type Int64`, or
  `@ cell @ gene :: UMIs >> Sum`). `>>` is no longer silently aliased to
  `>|`; on a grouped input (`... / g >> Sum`) it continues to produce a
  per-group vector, as before.
- `@ axis = entry` picks one entry from a vector
  (`@ cell : age @ cell = N89`) or one cell from a matrix
  (`@ cell @ gene :: UMIs @ cell = C @ gene = X`). Two successive picks
  collapse a matrix to a scalar.
- `|| value type T` attaches a Julia-style dtype (`Bool`,
  `Int8`..`Int64`, `UInt8`..`UInt64`, `Float32`, `Float64`, `String`) to
  a scalar-lookup default, matching the existing behaviour of the same
  suffix inside reductions and element-wise ops.
- [`IfMissing()`](https://tanaylab.github.io/dafr/reference/IfMissing.md)
  builder gains an optional `type` kwarg
  (`IfMissing(0, type = "Int64")`).

## dafr 0.1.0

First public release.

A native R + C++ implementation of the
[DataAxesFormats](https://github.com/tanaylab/DataAxesFormats.jl) (DAF)
data model for multi-dimensional data along arbitrary axes, ported from
the [Julia reference
implementation](https://github.com/tanaylab/DataAxesFormats.jl) with no
Julia dependency.

### Features

- **Core model.** Scalars, per-axis vectors, per-axis-pair matrices,
  axis entries, cache invalidation.
  [`memory_daf()`](https://tanaylab.github.io/dafr/reference/memory_daf.md)
  and
  [`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
  backends.
- **Query DSL.** String form (`daf["@ cell : donor"]`) and pipe-chain
  builders
  (`daf[Axis("cell") |> LookupVector("donor") |> IsGreater(2)]`).
- **Memory-mapped reads** for vectors and sparse matrices from a
  read-only
  [`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
  store (zero-copy).
- **OpenMP-parallel C++ kernels** for Sum / Mean / Var / Mode / Quantile
  / GeoMean, with a CRAN-compliant 2-core auto-cap via
  [`set_num_threads()`](https://tanaylab.github.io/dafr/reference/set_num_threads.md).
- **AnnData interop.** `DafAnnData` R6 facade,
  [`as_anndata()`](https://tanaylab.github.io/dafr/reference/as_anndata.md),
  [`h5ad_as_daf()`](https://tanaylab.github.io/dafr/reference/h5ad_as_daf.md),
  [`daf_as_h5ad()`](https://tanaylab.github.io/dafr/reference/daf_as_h5ad.md)
  — sparse `X`, categorical `obs`/`var`, nested `uns`, and `obsm`/`varm`
  all round-trip.
- **dplyr backend.** `tbl(daf, axis)` → lazy `daf_axis_tbl` with
  `filter`, `select`, `mutate`, `arrange`, `summarise`, `group_by`,
  `distinct`, `pull`, `collect`, `compute` (write-back).
- **Contracts.**
  [`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md),
  [`verify_contract()`](https://tanaylab.github.io/dafr/reference/verify_contract.md),
  [`contract_scalar()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  /
  [`contract_vector()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  /
  [`contract_matrix()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  /
  [`tensor_contract()`](https://tanaylab.github.io/dafr/reference/tensor_contract.md)
  /
  [`axis_contract()`](https://tanaylab.github.io/dafr/reference/axis_contract.md)
  for computation pre/post-condition validation.

See the [pkgdown site](https://tanaylab.github.io/dafr/) for full
documentation.
