# dafr 0.2.7

## Fix: row/col mask alignment under matrix GroupBy

`@ axis [ prop = X ] @ other :: M -/ prop >- Op` (and the
`GroupColumnsBy` mirror) returned correct values but assigned them
to the wrong group label. The mask filtered the matrix axis
correctly, but the subsequent `GroupRowsBy` / `GroupColumnsBy` then
fetched the group-property vector at the FULL axis length and
matched it against the masked matrix by position - so the group
labels and the matrix rows drifted out of alignment.

Concrete repro from the regression test:

```r
d <- memory_daf(name = "t")
add_axis(d, "metacell", c("M1","M2","M3","M4","M5","M6"))
add_axis(d, "gene", c("G1","G2"))
add_axis(d, "type", c("A","B","C"))
set_vector(d, "metacell", "type", c("A","A","B","B","C","C"))
set_matrix(d, "metacell", "gene", "UMIs", matrix(
    c(11,12, 21,22, 31,32, 41,42, 51,52, 61,62),
    nrow = 6L, byrow = TRUE,
    dimnames = list(c("M1","M2","M3","M4","M5","M6"), c("G1","G2"))))

# Pre-fix: returned A=72,74 (which is the M3+M4 sum, mis-labelled).
# Post-fix: B=72,74 only.
get_query(d, "@ metacell [ type = B ] @ gene :: UMIs -/ type >- Sum")
```

Julia parity: `MatrixState` in `DataAxesFormats.jl` keeps the
per-axis `VectorState` on the matrix, so masks and groupings are
always aligned by axis. The R port was passing matrix state through
a plain list that dropped the row/col indices on transition; now
the matrix-lookup carries `row_indices`/`col_indices` forward and
`apply_groupby_rows` / `apply_groupby_columns` subset the group
vector to match.

Two regression tests added under `test-query-eval-masks.R` (E3 row
+ cols variants).

# dafr 0.2.6

## CI: document `source =` param + refresh pkgdown index

R CMD check `--as-cran` on all platforms flagged WARNINGs after the
v0.2.5 ship; pkgdown also failed the sitrep check:

- `man/register_eltwise.Rd` / `man/register_reduction.Rd` had the
  new `source = NULL` parameter (introduced as the conflict-source
  capture hook in v0.2.5's CR1 fix) in `\usage{}` but no matching
  `\item{source}` arg block.
- `_pkgdown.yml` was missing the newly-exported
  `register_query_operation()` from the **Op registry** reference
  section.

Both regenerated and indexed. All man pages also re-emitted under
roxygen 8.0.0 (drops `\docType{data}` / `\format{}` / `\keyword{datasets}`
on constant exports; otherwise no behaviour change).

# dafr 0.2.5

## Query registry parity (CR1 closed)

- **`register_query_operation(kind, name, fn)`** is now exposed as a
  single user-facing entry point for adding custom eltwise /
  reduction ops to the dafr query DSL. Mirrors Julia's
  `register_query_operation()` from `DataAxesFormats.Registry`.
- **Collision errors** now match Julia's template:

  ```
  conflicting registrations for the eltwise operation: <name>
  first in: <file>:<line>
  second in: <file>:<line>
  ```

  Previously the error was `<name> already registered; use
  overwrite = TRUE`. The new wording includes both registration
  source locations (captured automatically from the caller's
  srcref, or supplied explicitly via the new `source =` parameter).

# dafr 0.2.4

## CI: regenerate stale man pages

R CMD check on Linux devel / oldrel, Windows, and macOS hit
`code/documentation mismatches` after the v0.2.2 / v0.2.3 ship. Two
man pages were out of date with their code:

- `man/is_leaf.Rd` still documented the S7-generic signature
  `function(daf, ...)`; the R2 refactor in v0.2.2 made `is_leaf()` a
  plain wrapper `function(daf)`.
- `man/reconstruct_axis.Rd` was missing the `properties_defaults`
  parameter added in v0.2.3.

Both regenerated. No behavioural changes.

# dafr 0.2.3

## Concat / contracts / reconstruction parity (M1 + M4 + C1 + CR3)

- **`concatenate(merge = list(ALL_SCALARS = ...))`** wildcards now
  expand against the source properties at concat time. Mirrors
  Julia's `[ALL_SCALARS => action]` / `[ALL_VECTORS => action]` /
  `[ALL_MATRICES => action]`. Explicit non-wildcard keys still
  override wildcard-expanded entries. The
  "can't collect axis for the scalar:" error wording now matches
  Julia byte-for-byte.
- **`concatenate(prefixed = list(axis = c(...)))`** is now an override
  that fires regardless of `prefix[axis]`. The list names cell-axis
  vectors whose values reference a prefixed axis and thus need the
  dataset name spliced in. Previously dafr ANDed the per-axis prefix
  flag with the list, so vectors listed in `prefixed[cell]` were not
  prefixed when `prefix[cell] == FALSE`.
- **`merge_contracts()` rejects `integer` vs `character`** (and other
  cross-lattice mixes) with Julia's `incompatible type:` error.
  Previously dafr's type lattice was a total order
  (`logical < integer < double < character`) so any pair quietly
  merged to the narrower type. Now `character` and `integer64` are
  siblings to the numeric chain - same-chain merges still pick the
  narrower type, cross-chain merges error.
- **`reconstruct_axis(..., properties_defaults = list(prop = val))`**
  merges into a pre-existing axis. Implicit-property values must be
  a subset of the axis entries; any extra entries get the
  per-property default value. Mirrors Julia's
  `reconstruct_axis!(..., properties_defaults = (; prop = val))`.

# dafr 0.2.2

## Reorder parity (R2 + R4 + R5 + R6 closed)

- **`is_leaf()` accepts S7 class objects** as well as instances,
  mirroring Julia's `is_leaf(::Type{<:DafReader})`. Class-level call
  returns `TRUE` for the concrete leaf classes (`MemoryDaf`,
  `FilesDaf{,ReadOnly}`, `ZarrDaf{,ReadOnly}`, `HttpDaf`) and
  `FALSE` for the abstract `DafReader` / `DafWriter` / `DafReadOnly`.
- **`zarr_daf` now supports `reorder_axes()`**. Best-effort in-place
  reorder via the existing zarr overwrite path. Crash recovery
  (backup-and-restore) is not yet implemented; a mid-reorder crash
  leaves the store in an undefined state.
- **`reorder_axes(list(d1, d2), axis = perm)` reorders multiple
  writers in one call** (Julia: `reorder_axes!([d1, d2], Dict(...))`).
  Each axis must agree on entry order across every writer that has
  it (`axis: <a> entries differ` error otherwise); writers missing
  the axis silently skip it.
- **`memory_daf` reorder is now atomic**. A pre-reorder snapshot is
  parked on the daf's internal state; a `SimulatedCrash`
  mid-reorder is rolled back by `reset_reorder_axes()`, which now
  returns `TRUE` if a pending reorder was rolled back, `FALSE`
  otherwise. Mirrors Julia's reset_reorder_axes! Bool contract.

# dafr 0.2.0

## Julia parity (DataAxesFormats.jl 0.2.0)

This release closes the bulk of the user-facing semantic divergences
between dafr and DataAxesFormats.jl `main`. The R query DSL now
mirrors Julia byte-for-byte at the operator level; `escape_value` /
`unescape_value` use Julia's `\<char>` backslash convention.

Operation parity (CO1-CO7 + CT1/CT3 closed):

- **`% Op type=<T>` now coerces the result.** Previously `type` was
  silently ignored on the numeric eltwise ops (Abs / Exp / Sqrt /
  Round / Log / Clamp). Honors `integer` / `numeric` / `double` /
  `Float32`/`Float64` / `Int8-64` / `UInt8-64` / `Bool` / `logical`.
- **`% Log` rejects non-positive `x + eps`** with Julia's
  `value must be: positive` template instead of returning `NaN`.
- **`% Fraction` rejects integer types** (Julia: `value must be: a
  float type`) and rejects scalar input with Julia's
  `applying Fraction eltwise operation to a scalar` wording.
- **`% Significant` accepts `low` only** (defaults `high = low`),
  matching Julia.
- **`% GeoMean` error wording** aligned with Julia's `value must be:
  not negative / for the parameter: eps / for the operation:
  GeoMean` template.
- **`% Abs` / `% Clamp` reject non-numeric `type=`** with Julia's
  `value must be: a number type` template.
- All eltwise / reduction ops use the same Julia error template:
  `invalid value: "<v>"` / `value must be: <constraint>` /
  `for the parameter: <name>` / `for the operation: <op>`.

Viewer parity (V2-V7 closed):

- **Wildcard `*` in view contracts validates** that values are `=`
  or `NULL` (Julia: `(*, *)`, `(*, prop)`, `(axis, *)` shapes).
- **Scalar-shape validation**: a vector-producing query in a scalar
  slot now errors instead of silently exposing the vector via
  `get_scalar`.
- **Strict-include semantics**: passing `data = list(...)` to
  `view_daf()` now exposes only the listed properties; `data =
  NULL` retains the original "expose all" behaviour.
- **`__axis__` placeholder substitution** in matrix slot queries.
- **Vector-slot rejects matrix-shape queries** with Julia's
  `matrix query: ... / for the vector: ...` wording.

Naming parity:

- Grouped reduction results (`-/`, `|/`) now use alphabetical group
  ordering to match Julia's `factor(..., levels = sort(unique))`,
  not first-appearance order.

Recovered fixes:

- **`complete_path()` returns `NULL`** for memory-backed dafs and
  non-FilesDaf chains instead of erroring; mirrors Julia's
  `complete_path` returning `nothing` for non-Files dafs.
  `complete_chain()` retains its explicit error since it requires
  a real on-disk path.
- **`memory_daf` accepts `Matrix::sparseVector`**. The atomic-only
  validator now detects `sparseVector` via S4 inheritance; readback
  densifies before name attachment. Storage roundtrip preserves
  values.

Test suite (5579 PASS / 0 FAIL / 142 SKIP, vs 5024 PASS / 0 FAIL /
49 SKIP at the start of the parity push):

- 23 `*-jl-parity.R` files mirror DataAxesFormats.jl's 22 main test
  files (contracts.jl is split across 7 sub-slices in R for tractable
  ports).
- The remaining 142 skips are out-of-scope feature gaps documented
  per-test (`R divergence` codes), not behavioral divergences:
  multi-contract `@computation`, `description(deep)`, `empty_*`
  builders, h5df backend, file-bridge, tensors-in-views, etc.

## Earlier in 0.2.0: Julia parity for chains, concat, reorder

A literal port of `DataAxesFormats.jl::concat.jl`, `reorder.jl`, and
`chains.jl` test suites surfaced and closed five behavior gaps:

- **`concatenate(..., merge = list("axis|name" = MERGE_COLLECT_AXIS))`
  now honors `empty=` for missing-source columns.** Previously, when a
  source lacked the vector being collect-axis-merged, that source's
  column in the destination matrix was filled with `NA` regardless of
  the user's `empty=` map. Now the empty fill is consulted; if neither
  the source nor `empty` provides a value, the same "no empty value"
  error as the per-axis vector path is raised.
- **`concatenate(..., merge = list("rows|cols|name" = MERGE_LAST_VALUE))`
  now actually fires for matrix properties** (rows / cols not in the
  concat set). Previously a silent no-op — the dispatch reached the
  3-part-key case but didn't write anything. The destination now holds
  the last source's matrix.
- **`reorder_axes()` now errors on missing axes** (was a silent skip).
  Aligned with Julia's `reorder_axes!` contract.
- **`reset_reorder_axes()` now returns `invisible(TRUE/FALSE)`**: TRUE
  if a pending reorder was rolled back, FALSE if no pending. Mirrors
  Julia's Bool return. Existing callers that ignored the return value
  (most of them) are unaffected.
- **`ReadOnlyChainDaf` / `WriteChainDaf` version counters now
  propagate from underlying sources.** Previously the chain wrappers
  had their own private `*_version_counter` env that never tracked
  source mutations — `vector_version_counter(chain, ...)` returned 0
  even after a source-side `set_vector` bumped the source's counter.
  More importantly, this broke cache invalidation on the chain: after
  `set_vector(chain, ..., overwrite = TRUE)` (which routes the write
  to the chain's writer), reads through the chain returned the
  cached pre-write value instead of the new value. The chain's
  stamp / counter functions now sum per-source counters.

## Windows support

`files_daf()` now works on Windows. Previously every write failed with
`MmapZipStore is not supported on Windows`. On Windows, dafr skips the
optional `metadata.zip` bundle (only used for serving a FilesDaf over
HTTP via `http_daf()`); local reads, writes, and round-trips are
unaffected. `pack_files_daf_metadata()` errors with a clear message,
and `zarr_daf()` rejects `.daf.zarr.zip` paths there — use the
unzipped `.daf.zarr` directory store instead, or run on Linux/macOS
for zip-backed storage.

## Named query results

`get_query()` and the format API now return named values matching the
Julia `NamedVector` / `NamedMatrix` convention:

- Lookup vectors / matrices carry axis-entry names / dimnames.
- Axis listings (`@ cell`, `@ donor [ age > 60 ]`) return character
  vectors with `names == values`.
- IfMissing-default vectors (`@ cell : missing || 0 Int64`) carry
  names too.

This is **a behavior change** — code that does
`expect_equal(get_query(...), unnamed_vec)` may need updating to
expect named results. ALTREP-mmap vectors (`mmap_real` / `mmap_int` /
`mmap_lgl`) preserve their ALTREP status across `names<-`, so the
mmap region stays shared rather than copied.

## Julia parity for `get_query`

Closes the remaining gaps from a literal port of the
`DataAxesFormats.jl::queries.jl` test suite. New query forms supported:

- **Top-level comparators** after `:` / `::` return a boolean vector /
  matrix: `@ cell : score < 1.0`, `@ cell @ gene :: UMIs > 0`.
- **Standalone `:` / `::` with `IfMissing`.**
  `: age || 1 @ cell = X` returns `1` when `age` is missing; same for
  the matrix form `:: UMIs || 0 @ cell = X @ gene = A`.
- **Implicit AsAxis fallback.** `@ cell : type.manual : color`
  resolves through the `type` axis when `type.manual` is not itself
  an axis.
- **Matrix-column slice auto-relayout.** `@ cell :: UMIs @ gene = A`
  works regardless of `UMIs` storage orientation.
- **Cols-axis mask after a second axis.** `@ rows @ cols [ filter ] :: M`
  filters the cols axis; the matrix lookup honours both row and column
  filters.
- **Virtual `name` property** on every axis. `[ name = X ]` and
  `: name` return the axis-entry vector.
- **Eltwise on scalar.** `. score % Abs` applies element-wise on
  numeric scalars (was: `'%' eltwise requires vector or matrix in
  scope`).
- **Regex escapes** in masks (`[ type ~ \^\[A-U\] ]`).
- **Empty-string round-trip.** `escape_value("")` is `''`;
  `unescape_value("''")` is `""`.

Stricter error reporting:

- Partial queries (`@ cell @ gene` with no lookup) error with
  `invalid query: <canonical>` instead of silently returning `NULL`.
  A second `?` after a `Names` result also errors.
- Empty-matrix reductions without `IfMissing` always error (was: the
  output-axis-empty branch silently returned an empty vector).
- Numeric reductions on a character matrix error with
  `non-numeric input` instead of leaking base R's
  `'x' must be numeric`.
- `?? sentinel : prop` raises a clear parse error when the sentinel
  can't be coerced to the lookup vector's type (was: silent `NA` via
  R's `as.integer` warning).
- Parser errors for unknown operations / parameters and repeated
  parameters now match the Julia DAF wording.
- `query_axis_name()` agrees with `get_query()` on compound-mask
  queries (`@ cell [ is_low & UMIs @ gene = B ]`).

## Reduction builders

`Sum()`, `Mean()`, `Median()`, `Min()`, `Max()`, `Mode()`, `Count()`,
`GeoMean()`, `Quantile()`, `Std()`, `StdN()`, `Var()`, `VarN()` now
produce the canonical reduction form (`>> Sum`) instead of `% Sum`.
The previous emission was an element-wise op that erred at runtime
when piped after a matrix or vector. **Behavior change**: stored
canonical strings from these builders change from `% Sum` to `>> Sum`.
`ReduceToColumn()` / `ReduceToRow()` accept both shapes for
back-compat. `canonical_query()` also accepts a `DafrQuery` directly.

`>> Mode` / `>| Mode` now accept character and factor inputs, matching
the Julia operation's documented support for strings.

IfMissing defaults in vector and matrix lookups thread through the
default coercion so `: age || 1` returns an integer column (not a
character one).

## `get_dataframe()` column-spec

`get_dataframe()` and `get_dataframe_query()` accept a list mixing
positional bare names with `name = ":query"` pairs:

```r
get_dataframe(d, "cell", columns = list("age", doublet = ":is_doublet"))
```

Mirrors Julia's `["age", "doublet" => ":is_doublet"]`.

## Mask comparators on factor properties

`[ prop < value ]`, `[ prop > value ]`, etc. on a property stored as a
factor (e.g. an h5ad categorical loaded via `categorical` encoding)
now compare the stored strings lexically, matching Julia. Previously
returned `NA` (unordered factor) or compared level codes (ordered
factor).

## Build hygiene

`.Rbuildignore` excludes `AGENTS.md` and `CLAUDE.md` so they no longer
surface as a `top-level files` NOTE under `R CMD check`.

# dafr 0.2.0

## Reader-API parity polish

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
- `reorder_axes()` now rejects non-leaf inputs up front with a clear
  `"non-leaf type: <Class> for the daf data: <name> given to reorder_axes"`
  error (previously surfaced as a cryptic missing-method dispatch).
- `complete_path()` now works for `ZarrDaf` (returns the directory
  path, `:memory:`, zip path, or HTTP URL — whichever store path the
  constructor recorded). Was previously `FilesDaf`-only.

## HttpDaf + HttpStore + metadata.zip parity

- New `HttpDaf` backend for read-only access to a `FilesDaf` directory
  served over HTTP(S). The client downloads `metadata.zip` once at
  open and serves all JSON metadata from it; non-JSON payloads
  (`.txt` / `.data` / `.nzind` / `.nzval` / `.colptr` / `.rowval` /
  `.nztxt`) are fetched lazily via one HTTP GET each.
- New `HttpStore` (`R/http_store.R`) implements the `R/zarr_store.R`
  store interface over HTTP. `zarr_daf("https://host/foo.daf.zarr/")`
  routes through it; reads `.zmetadata` once and serves
  `.zarray`/`.zattrs`/`.zgroup` from there.
- `open_daf("https://...")` dispatches to `http_daf` or `zarr_daf`
  based on the URL suffix. HTTP backends are read-only; writable modes
  hard-error. `*.daf.zarr.zip` URLs are explicitly out of scope and
  redirect users to opening the underlying `.daf.zarr` directory.
- New `pack_files_daf_metadata(path)` exported helper to bundle a
  `FilesDaf` tree's JSON metadata into `metadata.zip` (for trees
  written by older dafr or modified outside dafr).
- **`FilesDaf` now maintains `metadata.zip` automatically** on every
  `set_*` / `delete_*` / `add_axis` / `delete_axis` / `reorder_axes`
  operation, plus a one-shot rebuild on writable open if the bundle is
  missing. Mirrors upstream `DataAxesFormats.jl::FilesFormat`. Pre-0.2.0
  stores are picked up automatically the first time they're opened
  with mode `"r+"` or `"w+"`.
- `axes/metadata.json` sidecar now maintained by FilesDaf (sorted JSON
  array of axis names). Required by HTTP clients to enumerate axes
  without GET-ing every `axes/*.txt`.
- New `Imports`: `httr2` (and transitively `curl`).

## MmapZipStore + Zarr zip backend

- New `MmapZipStore` (C++ in `src/mmap_zip_store.cpp`) backs `ZarrDaf`
  with a single ZIP archive on the local filesystem. `open_daf()` and
  `zarr_daf()` now accept `.daf.zarr.zip` paths and return a working
  `ZarrDaf` / `ZarrDafReadOnly`.
- Reads use a shared mmap of the archive: stored (method-0) entries
  are returned as zero-copy `ALTREP RAW` views via a new
  `ZipRawAltrep` class. Deflate-compressed (method-8) entries are
  decompressed on demand via system zlib; deflate64 / other methods
  raise a clear error pointing to a stored / deflate re-save.
- Writes append entries via upstream's two-step commit protocol
  (commit central directory + EOCD first, then write the local file
  header and data into the now-sparse hole). Crash-safe: a writable
  open's recovery pass detects partial commits via tail validation
  (LFH signature + data CRC32) and rolls back the trailing run of
  invalid entries before returning. Internal tick-counter hooks at
  every commit-able decision point let recovery be tested
  deterministically (5 tick points; tests gated on `NOT_CRAN=true`).
- Always emits ZIP64 (per upstream `DataAxesFormats.jl`); every local
  file header is padded with a `0xDAF1` extra field so the data
  region starts at an 8-byte-aligned file offset (zero-copy
  unaligned-load safety on every host architecture).
- ALTREP safety net: when the store closes (or is GC'd), every
  outstanding ALTREP vector it produced is deactivated — `length()`
  returns 0 and `Dataptr()` returns a stable inert byte. R callers
  who keep references past close get clean empty raws instead of
  segfaults.
- Internal-only `dafr:::dafr_mmap_zip_reserve()` /
  `dafr:::dafr_mmap_zip_patch_crc()` expose two-phase fill for large
  sparse arrays (writable in-place ALTREP view + post-fill CRC
  patch). Crash between reserve and patch rolls back via the same
  CRC-mismatch path as ordinary partial commits.
- `SystemRequirements`: zlib (linked via `-lz`).
- Cross-language smoke: dafr-written `.daf.zarr.zip` archives open
  cleanly in Python via `zipfile` and `zarr.open(zarr.storage.ZipStore(...))`.
  Foreign zips written by `python -m zipfile` (stored or deflate)
  open cleanly in dafr.
- Mirrors `DataAxesFormats.jl` `mmap_zip_store.jl` (~1070 LOC of
  Julia ported to ~1300 LOC of C++ + cpp11 + ALTREP).

## ZarrDaf backend

- New `zarr_daf(uri, mode, name)` backend reading and writing Zarr v2.
  Two store impls: `DirStore` (filesystem directory tree) and
  `DictStore` (in-memory). Zip-backed Zarr is also supported via the
  `MmapZipStore` backend (see above).
- New `files_to_zarr(src, dst)` and `zarr_to_files(src, dst)`
  conversion helpers (same-filesystem only; correctness-first
  implementation re-encodes through the public API; hard-link
  optimization deferred as a perf follow-up).
- `open_daf("foo.daf.zarr")` now returns a `ZarrDaf`.
- Compression policy: dafr writes Zarr chunks uncompressed; reads
  uncompressed and gzip; rejects blosc/zstd/lz4 with a clear error
  pointing to re-save with `compressor=None`.
- Sparse layouts mirror upstream `DataAxesFormats.jl`: 1-based
  on-disk indices for `nzind` / `colptr` / `rowval`; sparse-Bool
  all-`TRUE` skips `nzval` (storage compaction). Cross-language
  parity is verified via gated Python `zarr.open()` smoke tests.
- Mirrors `DataAxesFormats.jl` v0.2.0 commits `ea4b5f9` (Zarr v2
  directory tree), `8cc3ff6` (in-memory store), `47e7693` (CRC
  fix — N/A for our in-memory layer), `79034fd` (`.zmetadata`
  consolidation), `46d4ab2` (Files↔Zarr conversion).

## reorder_axes() + open_daf() factory

- New `reorder_axes(daf, axis = perm, ...)` permutes axis entries
  in place, rewriting every vector and matrix that depends on the
  axis. On `files_daf` the operation is crash-recoverable via a
  `.reorder.backup/` directory of hardlinks; on the next
  `files_daf(path, mode = "r+" | "w+")` open, any in-progress reorder
  is automatically rolled back to the pre-reorder state.
- New `reset_reorder_axes(daf)` to manually trigger recovery (mostly
  redundant given the auto-recovery on open).
- New `open_daf(uri, mode, name)` factory function — dispatches on
  path / URL pattern. `memory://` (or no path) → `memory_daf`,
  filesystem path → `files_daf`, `*.daf.zarr` / `*.daf.zarr.zip`
  → `zarr_daf`, `http(s)://` → `http_daf`. The factory replaces the
  previous filesystem-only `open_daf` from `R/complete.R`.
- Mirrors `DataAxesFormats.jl` v0.2.0 commits `90301ff`, `070bd34`
  (axis reordering) and `b40377f` (`open_daf` factory).

## Internal: per-item cache_group refactor

The internal format API now returns per-item cache classifications,
matching `DataAxesFormats.jl` v0.2.0 (upstream commit `49fbba1`).
**No user-visible behavior change.**

- Every backend `format_get_*` method (scalar/axis_array/vector/matrix)
  returns `list(value, cache_group)` instead of a bare value.
- Every backend `format_set_*` method returns the cache_group constant
  for the just-written value (or `NULL`) instead of `invisible()`.
- New exported character constants `MEMORY_DATA`, `MAPPED_DATA`,
  `QUERY_DATA` — accepted by `empty_cache(daf, clear = ...)` /
  `keep = ...` alongside the existing lowercase forms.
- The reader-level cache (`R/readers.R`) now consults the
  backend-returned cache_group when storing fresh reads, instead of
  hardcoding the `"memory"` tier. mmap-eligible reads on
  `files_daf` now correctly land in the `"mapped"` tier.
- Per-item classification: `files_daf` returns `MEMORY_DATA` for
  string/factor reads (R's CHARSXP cache makes mmap moot for strings)
  and `MAPPED_DATA` for everything else. Matches upstream's
  structural classification — no size thresholds.

This refactor is preparatory for the `ZarrDaf` and `HttpDaf`
backends, which require per-item classification to drive their
internal caching.

# dafr 0.1.0 (development)

## Query DSL: Julia-parity parser additions

Three DataAxesFormats.jl query-DSL features that were previously
missing have been implemented, closing the last semantic gaps between
`dafr` and the upstream Julia package:

- `>> Reduction` reduces a vector or matrix to a scalar (e.g.
  `@ gene : is_lateral >> Sum type Int64`, or
  `@ cell @ gene :: UMIs >> Sum`). `>>` is no longer silently aliased
  to `>|`; on a grouped input (`... / g >> Sum`) it continues to
  produce a per-group vector, as before.
- `@ axis = entry` picks one entry from a vector
  (`@ cell : age @ cell = N89`) or one cell from a matrix
  (`@ cell @ gene :: UMIs @ cell = C @ gene = X`). Two successive
  picks collapse a matrix to a scalar.
- `|| value type T` attaches a Julia-style dtype (`Bool`,
  `Int8`..`Int64`, `UInt8`..`UInt64`, `Float32`, `Float64`, `String`)
  to a scalar-lookup default, matching the existing behaviour of the
  same suffix inside reductions and element-wise ops.
- `IfMissing()` builder gains an optional `type` kwarg
  (`IfMissing(0, type = "Int64")`).

# dafr 0.1.0

First public release.

A native R + C++ implementation of the [DataAxesFormats][daf] (DAF)
data model for multi-dimensional data along arbitrary axes, ported
from the [Julia reference implementation][daf] with no Julia
dependency.

[daf]: https://github.com/tanaylab/DataAxesFormats.jl

## Features

- **Core model.** Scalars, per-axis vectors, per-axis-pair matrices,
  axis entries, cache invalidation. `memory_daf()` and `files_daf()`
  backends.
- **Query DSL.** String form (`daf["@ cell : donor"]`) and pipe-chain
  builders (`daf[Axis("cell") |> LookupVector("donor") |> IsGreater(2)]`).
- **Memory-mapped reads** for vectors and sparse matrices from a
  read-only `files_daf()` store (zero-copy).
- **OpenMP-parallel C++ kernels** for Sum / Mean / Var / Mode /
  Quantile / GeoMean, with a CRAN-compliant 2-core auto-cap via
  `set_num_threads()`.
- **AnnData interop.** `DafAnnData` R6 facade, `as_anndata()`,
  `h5ad_as_daf()`, `daf_as_h5ad()` — sparse `X`, categorical
  `obs`/`var`, nested `uns`, and `obsm`/`varm` all round-trip.
- **dplyr backend.** `tbl(daf, axis)` → lazy `daf_axis_tbl` with
  `filter`, `select`, `mutate`, `arrange`, `summarise`, `group_by`,
  `distinct`, `pull`, `collect`, `compute` (write-back).
- **Contracts.** `create_contract()`, `verify_contract()`,
  `contract_scalar()` / `contract_vector()` / `contract_matrix()` /
  `tensor_contract()` / `axis_contract()` for computation
  pre/post-condition validation.

See the [pkgdown site](https://tanaylab.github.io/dafr/) for full
documentation.
