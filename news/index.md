# Changelog

## dafr 0.4.1

### Packaging

- Shorten the committed packed-fixture directory names
  (`tests/testthat/fixtures/{zpk,fpk}/...`) so every path in the source
  tarball stays under the 100-byte portable limit. R’s internal `tar`
  (used by `R CMD build` on Windows) emits a “storing paths of more than
  100 bytes is not portable” warning above it, which the CI’s
  `error-on: warning` turned into a failure. No functional change.

## dafr 0.4.0

### FilesFormat: writes v1.1 (DataAxesFormats.jl 0.3.0 default)

- `FilesDaf` now writes the **v1.1** on-disk format
  (`{"version":[1,1]}`), matching DataAxesFormats.jl 0.3.0. The only
  change from v1.0 is the sparse property JSON descriptor: it now
  carries a per-component object (`nzind` / `nzval` for vectors,
  `colptr` / `rowval` / `nzval` for matrices), each shaped like a
  stand-alone dense descriptor with its `eltype` and `n_elements`. The
  binary payload files are byte-identical to v1.0, and the reader
  accepts both shapes, so existing v1.0 repos still read. Verified
  round-trip in both directions against DAF 0.3.0 (dafr reads Julia’s
  v1.1; Julia reads dafr’s).

### ZarrDaf: faster bulk writes

- Writing many properties to a directory/in-memory ZarrDaf is now
  near-linear instead of O(N^2). `set_*` updates the root consolidated
  metadata incrementally (editing an in-memory index of node `zarr.json`
  strings and re-assembling the root by string concatenation) rather
  than re-scanning and re-parsing the whole store on every mutation.
  Writing 400 vectors dropped from ~45 s to ~1.7 s. The store stays
  consistent on disk after every write (no flush/close step), and the
  output is unchanged.

### ZarrDaf: Zarr v3 (DataAxesFormats.jl 0.3.0 interop)

- ZarrDaf now reads and writes the **Zarr v3** on-disk format used by
  DataAxesFormats.jl 0.3.0 (a single `zarr.json` per node, `c/`-prefixed
  chunk keys, the `daf` version marker as a root-group attribute, and
  inline consolidated metadata). Flat (uncompressed) read and write are
  supported.

- **Breaking:** the legacy Zarr v2 reader/writer is removed. Opening a
  Zarr v2 `.daf.zarr` now errors with a conversion hint
  (`python -m zarr v2_to_v3`), matching DataAxesFormats.jl 0.3.0’s own
  behaviour. \## ZarrDaf: packed/sharded v3 read

- Reading packed/sharded (`packed=true`) Zarr v3 `.daf.zarr` stores is
  now supported (read-only; dafr still writes flat). Each packed array’s
  start-located shard index (ZEP-0002, crc32c-checked) is parsed in R,
  and its inner chunks decode via `gzip` (base R, always available) or -
  when the optional system library is present - `c-blosc` (the default
  `blosc_zstd_bitshuffle` / `blosc_lz4_bitshuffle` codecs) and `libzstd`
  (plain `zstd`). Dense and sparse matrices/vectors, including
  `vlen-utf8` strings, are covered; flat sub-threshold components in a
  packed store read as before.

- **CRAN-safe:** `configure` probes for `c-blosc`/`libzstd` (honouring
  `BLOSC_HOME` / `ZSTD_HOME` / `CONDA_PREFIX`). With neither present the
  flat path is unchanged and a blosc/zstd-packed read raises an
  actionable “install c-blosc/libzstd” error. `crc32c` is always
  compiled (no dependency).

### FilesFormat: packed/sharded read (FilesDaf, HttpDaf)

- Reading packed (chunked + compressed) `FilesDaf` / `HttpDaf`
  properties is now supported (read-only; dafr still writes flat). A
  packed property is a dual-format shard (`<name>.zip`, or
  `<name>.<component>.zip` for an independently-packed sparse component)
  that carries the same start-located Zarr v3 shard index as a packed
  `ZarrDaf` array. dafr reads it through that index, reusing the
  crc32c + `gzip` / `c-blosc` / `libzstd` decode backend (so the same
  optional-library rules and CRAN-safety apply). Dense and sparse
  matrices/vectors and `vlen-utf8` strings are covered; flat
  sub-threshold components (small `colptr`, scalars, short vectors) in
  the same store read as before. A foreign `"zipped"`-only shard (a ZIP
  archive with no leading Zarr index) is rejected with an actionable
  message.

### ZarrDaf over HTTP: Zarr v3 read

- **Reading a Zarr v3 `.daf.zarr` over HTTP now works.**
  `zarr_daf("http://...")` parses the v3 inline consolidated metadata
  from the root `zarr.json` (v3 does not write the v2 `.zmetadata` file)
  as its node index, serves node metadata from that index, and fetches
  chunks lazily over HTTP. Scalars, axes, dense and sparse vectors,
  strings, bools, and dense and sparse matrices all round-trip. A legacy
  Zarr v2 store served over HTTP is rejected with the same
  `python -m zarr v2_to_v3` conversion hint as the local path.
- `HttpDaf` / `FilesDaf` over HTTP (the FilesFormat path,
  [`http_daf()`](https://tanaylab.github.io/dafr/reference/HttpDaf.md))
  is unaffected; that path does not use Zarr at all.

#### Known limitations

- dafr reads packed/sharded v3 stores but only ever **writes flat** (the
  common default). Reading blosc/zstd-packed stores needs the optional
  `c-blosc` / `libzstd` system libraries (see above); `gzip`-packed and
  all flat stores read with no extra dependency. Local **directory**
  (`DirStore`), **zip** (`MmapZipStore`, `.daf.zarr.zip`), and **HTTP**
  v3 stores are all supported.

## dafr 0.3.1

### Fix: read DataAxesFormats.jl 0.3.0 FilesFormat v1.1 directories

`DataAxesFormats.jl` 0.3.0 bumped the FilesDaf on-disk format from 1.0
to 1.1. The binary blobs are byte-identical, but a **sparse** property’s
JSON sidecar moved from top-level `eltype`/`indtype` keys to
per-component descriptors (`nzind`/`nzval` for vectors;
`colptr`/`rowval`/`nzval` for matrices). dafr was a 1.0-only reader and
rejected 1.1 directories outright (`incompatible format version: 1.1`).
dafr now:

- accepts FilesFormat minor version 1 (it still *writes* 1.0, and reads
  both 1.0 and 1.1);
- parses both the legacy top-level and the v1.1 per-component sparse
  descriptors - deriving the element type from the `nzval` component (or
  `Bool` when it is absent) and the index type from the index
  component - mirroring `DataAxesFormats.jl`’s
  `parse_sparse_descriptor`;
- raises a clear error on 0.3.0 “packed” (`.zip`, chunked + compressed)
  sparse components, which are not yet supported (re-save with flat
  components).

`HttpDaf` (a `FilesDaf` served over HTTP) gets the same treatment: it
accepts v1.1, parses per-component sparse descriptors, and rejects
packed components.

This covers reading flat FilesFormat 1.1 repos (directory and over HTTP)
only, not the rest of 0.3.0 (the zarr/zip and “packed view of a
directory as Zarr” machinery).

### Fix: ZarrDaf on-disk format now interoperates with DataAxesFormats.jl

`.daf.zarr` stores written by dafr and by `DataAxesFormats.jl` were
mutually unreadable. Opening a Julia-written store in R failed with
`missing daf.json`; opening an R-written store in Julia failed with
`not a daf data set`. Three divergences from upstream
(`DataAxesFormats.jl` v0.2.0, `src/zarr_format.jl`) caused this, all now
fixed:

- **`daf` marker array.** Upstream marks a store with a Zarr *array*
  named `daf` holding two `UInt8` bytes `[MAJOR, MINOR]` = `[1, 0]` and
  validates via `haskey(root.arrays, "daf")`. dafr wrote a plain
  `daf.json` file instead. dafr now writes (and validates) the `daf`
  marker array and no longer writes `daf.json` for Zarr stores. This is
  a **breaking change** to the dafr Zarr on-disk format: `.daf.zarr`
  stores written by earlier dafr versions (which carry `daf.json`, not
  the `daf` array) are not readable by this version. The FilesDaf
  `daf.json` marker is unchanged.
- **Intermediate `.zgroup` markers.** Upstream writes a real `.zgroup`
  for every group (the four `scalars`/`axes`/`vectors`/`matrices`
  containers, eagerly, plus every sub-group). dafr only wrote the root
  `.zgroup` and synthesised the rest inside the consolidated
  `.zmetadata` - enough for zarr-python’s consolidated reader, but
  Julia’s directory-store open navigates real `.zgroup` files and so
  raised `KeyError: key "axes" not found`. dafr now writes a `.zgroup`
  for every group.
- **Read-side dtype coverage.** The chunk reader only understood
  `<f8/<i4/<i8/|b1/|O`. It now also reads `|u1`, `<u1`, `<i1`, `<u2`,
  `<i2`, `<u4`, `<u8`, and `<f4` - the unsigned, narrow, and Float32
  dtypes upstream legitimately emits (Float32 expression matrices,
  unsigned index arrays, and the `|u1` marker itself).
- **Dense-matrix chunk separator.** dafr wrote multi-dimensional chunk
  keys with the `/` dimension separator (chunk file `0/0`); upstream and
  the Zarr v2 default use `.` (chunk file `0.0`). A Julia reader looked
  for `0.0`, did not find it, and failed with
  `missing chunks and no fill_value`. (Sparse matrices were unaffected -
  their components are 1-D, whose single chunk is `0` either way.) dafr
  now uses the `.` separator for all arrays, matching upstream.

Round-trip interop in both directions is covered by
`tests/testthat/test-zarr-julia-interop.R` (gated on the `dafr-mcview`
Julia env), and zarr-python interop remains green.

### Documentation pass

- Rewrote
  [`vignette("queries")`](https://tanaylab.github.io/dafr/articles/queries.md)
  to cover element-wise transforms, compound masks (string + builder),
  reductions to row / column / scalar, `GroupBy` on vectors and
  matrices, and `IfMissing` fallbacks. Previously the vignette only
  demonstrated trivial lookups.
- Expanded
  [`vignette("dafr")`](https://tanaylab.github.io/dafr/articles/dafr.md)
  (Getting Started) with
  [`description()`](https://tanaylab.github.io/dafr/reference/description.md),
  a `dplyr` backend demo, a working `files_daf` round-trip, and explicit
  vignette pointers for chains / views / contracts / computations.
- Fixed stale “Limitations (0.1.0)” block in
  [`vignette("anndata")`](https://tanaylab.github.io/dafr/articles/anndata.md).
  Sparse CSR / CSC, categorical columns, dense layers, and `obsm` /
  `varm` matrices have all been supported since 0.2.x; the vignette now
  lists the actual current gaps (`obsp`, `varp`, `raw`, sparse layer /
  obsm / varm entries).
- `README`: dropped the “First public release: 0.1.0” status line, added
  Zarr and HTTP backends to Key Features, expanded the dplyr verb list,
  and pointed the DSL link at our own `vignette("query-dsl-reference")`
  instead of the Julia upstream page.

### Fixed (docs only)

- [`vignette("dafr")`](https://tanaylab.github.io/dafr/articles/dafr.md)
  persistence example called `copy_all(d, fd)` with destination and
  source swapped. The example was gated under `eval = FALSE` so the bug
  was invisible until the chunk was set to evaluate. Package code was
  correct; only the vignette was affected.

## dafr 0.2.8.1

### CI: fix stale `Round` test expectation (`test-operations-registry.R:117`)

`Round(c(1.44, 1.55), digits = 1)` returns `c(1.4, 1.6)` as `Float64`,
which the default `Int64` cast (Julia parity, Round 5/6) then rejects
with `InexactError`. The test was written before that parity fix and
still expected the float result. Updated to
`expect_error("InexactError")` plus a `type = "Float64"` assertion that
keeps the fractional-result coverage. Resolves the CI break introduced
by v0.2.8.

### Fix: `set_matrix` rejects matrices with mismatched dimnames (Round-7 G7)

`set_matrix(d, "cell", "gene", "UMIs", m)` with a `dimnames`-bearing
matrix `m` previously discarded `rownames(m)` / `colnames(m)` silently
and overwrote them with the axis entries on readback. A caller passing
typo’d dimnames (e.g. `c("X","Y","Z")` against axis `c("A","B","C")`)
saw no error.

Now `set_matrix` validates dimnames against axis entries and raises when
they mismatch:

- `row names of the: matrix mismatch the entry names of the axis: <a>`
- `column names of the: matrix mismatch the entry names of the axis: <a>`

Mirrors Julia `data.jl > set_matrix > named > !rows|!columns > name`.

### Tests: port `copies.jl > matrix > sparse > {superset, disjoint}` grid (Round-7 G8)

12 new regression tests covering the sparse-matrix copy edge cases Julia
exercises:

- superset rows / cols with `()` (raises), `empty = NULL` (raises),
  `empty = -1` (fills with -1), `empty = 0` (fills with zero).
- disjoint rows / cols with `()`, `empty = NULL`, `empty = -1` (all
  raise `disjoint entries...`).

dafr’s `copy_matrix` already matched Julia semantically; these guards
lock the behaviour in. See `tests/testthat/test-copies-sparse-grid.R`.

## dafr 0.2.8

### Query DSL: Julia parity sweep (Round 5 + Round 6)

200 adversarial probes (Round 5) and a 1000-query grammar fuzzer (Round
6) surfaced a long tail of silent wrong-answer cases, type contracts
that diverged from `DataAxesFormats.jl`, and error messages that had
drifted out of alignment. Major user-visible changes:

- **Centralised op-invocation validator.** The per-op `.reject_*`
  dispatches scattered across five eltwise / reduction /
  grouped-reduction handlers are replaced by `.OP_META` +
  `.validate_op_invocation` in `R/op_dispatch.R`. The same type-tag
  rejection now fires at parse time before any axis / property lookup,
  mirroring Julia.
- **`integer64` is demoted in eltwise and reduction dispatch** so
  Float64-only kernels see the expected type instead of bit-aliased
  doubles.
- **`Median` preserves `NaN`** like Julia rather than promoting to `NA`.
- **Mask comparators on strings are bytewise** so `"é" < "f"` agrees
  with Julia’s lexicographic order.
- **`Significant high/low`, `Round digits`, `Convert` to `Bool` on
  non-{0,1}, `Float32` sum to `Int32`, …** now raise `InexactError`
  instead of returning silently-wrong values. `Convert` and `Round`
  accept the full set of dtype aliases (`Int8/16/32/64`,
  `UInt8/16/32/64`, `Float32/64`, `Bool` plus the lowercase R-style
  aliases).
- **`IfMissing` default is validated at parse time.** Typed defaults
  must be in-range for the declared eltype; hex / binary literals and
  the named constants `pi` / `e` are accepted.
- **Latin-1 / Unicode value tokens** raise the same
  `unexpected character` error as Julia instead of parsing through with
  a corrupted token.
- **Bare `??` without a lookup chain** raises `invalid operation` rather
  than returning a tautology.
- **GroupBy on `Bool` keys** uses lowercase `"true"` / `"false"` for
  bucket labels.
- **`NaN` group key with `IfMissing`** fills the NaN bucket with the
  user default instead of dropping it.
- **`BeginMask`** eager-rejects properties that are not matrix names so
  the error surfaces before the (more expensive) lookup path.
- **Matrix-reduction fast paths** honour the `type =` parameter via
  `.cast_to_type`; previously the type cast was silently dropped on the
  fast path.
- **`% Clamp` low/high** is rejected at parse time. (Julia exposes this
  via `min` / `max`; `Clamp` is not a DAF operation.)
- **Error-message alignment with Julia.** Collapses roughly 300 cosmetic
  divergence buckets in the parity fuzzer; common cases:
  - `the parameter: X does not exist for the operation: Y`
  - `missing required parameter: X` (Significant / Convert / Quantile)
  - `expected: value` (was `expected value after comparator at ...`)
  - Comparator-on-non-string drops the `for the comparison operation: X`
    suffix.

The Round-5 / Round-6 adversarial harness lives in
`dev/adversarial-parity/` (R + Julia runners, Python diff tool,
1100-query corpus, `FINDINGS.md` with per-bug provenance). About 55
regression tests added across `tests/testthat/test-query-*` and
`tests/testthat/test-operations-*`.

### Fix: cross-backend write/read parity (Round 7)

Six bugs surfaced by the new `dev/backend-parity/` audit harness, which
round-trips an 82-item fixture through Memory/Files/Zarr write -\>
reopen -\> read and every (src, dst) `copy_all` pair. Before fixes:
11/246 (single-backend) and 35/567 (cross-backend) diverged. After:
0/246 and 0/567.

- **NaN scalars are now accepted.** `set_scalar(d, "x", NaN)` previously
  raised `value may not be NA` because `.assert_scalar_value` used
  `is.na(value)`, which returns `TRUE` for `NaN`. `NaN` is a valid
  `Float64` per Julia DAF; only true `NA` is rejected now.
- **Float64 scalars round-trip at full precision on FilesDaf.**
  `set_scalar(d, "pi_val", pi)` used to read back as `3.1416` because
  [`jsonlite::toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  defaulted to `digits = 4`. The scalar writer now passes `digits = 17`.
- **`Int64` / `UInt64` dense vectors round-trip across the full 64-bit
  range on FilesDaf.** The reader’s
  `readBin(what = "integer", size = 8L)` silently truncated each value
  to its low 32 bits (base R has no 8-byte integer type), so values
  whose low 32 bits were zero (`2^32`, `2^62`, `-2^62`, …) all came back
  as `0`. The reader now reads 8-byte doubles and bit-aliases them into
  `integer64`.
- **All-NaN Float64 vectors preserve NaN on FilesDaf.** The
  auto-sparsifier counted NaN as zero (`sum(vec != 0, na.rm = TRUE)`
  drops NaN), so an all-NaN vector was written as an empty sparse vector
  and read back as all-zero. NaN is now counted as nonzero on the
  sparsify decision and kept in the sparse representation.
- **ZarrDaf reorders named-subset vectors to axis order.**
  `set_vector(d, "cell", "x", c(C = 3, A = 1, B = 2))` against ZarrDaf
  previously stored values in input order; against Memory and Files it
  stored in axis order. `.validate_vector_value` (which performs the
  reorder) is now called in the user-facing `set_vector` dispatcher so
  every backend - current and future - receives an axis-ordered,
  un-named vec.
- **FilesDaf scalar strings declare `Encoding() == "UTF-8"`.** The regex
  fast-path in `.read_scalar_json` returned bytes-only strings tagged
  `"unknown"`. The byte content was always correct
  ([`identical()`](https://rdrr.io/r/base/identical.html) returned
  `TRUE`), but
  [`serialize()`](https://rdrr.io/r/base/serialize.html)-based
  comparisons distinguished the tag.

Regression tests live in `tests/testthat/test-backend-parity-r7.R` (one
focused case per bug class). The audit harness, fixture, findings doc,
and diff tool live in `dev/backend-parity/`.

## dafr 0.2.7

### Fix: row/col mask alignment under matrix GroupBy

`@ axis [ prop = X ] @ other :: M -/ prop >- Op` (and the
`GroupColumnsBy` mirror) returned correct values but assigned them to
the wrong group label. The mask filtered the matrix axis correctly, but
the subsequent `GroupRowsBy` / `GroupColumnsBy` then fetched the
group-property vector at the FULL axis length and matched it against the
masked matrix by position - so the group labels and the matrix rows
drifted out of alignment.

Concrete repro from the regression test:

``` r

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

Julia parity: `MatrixState` in `DataAxesFormats.jl` keeps the per-axis
`VectorState` on the matrix, so masks and groupings are always aligned
by axis. The R port was passing matrix state through a plain list that
dropped the row/col indices on transition; now the matrix-lookup carries
`row_indices`/`col_indices` forward and `apply_groupby_rows` /
`apply_groupby_columns` subset the group vector to match.

Two regression tests added under `test-query-eval-masks.R` (E3 row +
cols variants).

## dafr 0.2.6

### CI: document `source =` param + refresh pkgdown index

R CMD check `--as-cran` on all platforms flagged WARNINGs after the
v0.2.5 ship; pkgdown also failed the sitrep check:

- `man/register_eltwise.Rd` / `man/register_reduction.Rd` had the new
  `source = NULL` parameter (introduced as the conflict-source capture
  hook in v0.2.5’s CR1 fix) in `\usage{}` but no matching
  `\item{source}` arg block.
- `_pkgdown.yml` was missing the newly-exported
  [`register_query_operation()`](https://tanaylab.github.io/dafr/reference/register_query_operation.md)
  from the **Op registry** reference section.

Both regenerated and indexed. All man pages also re-emitted under
roxygen 8.0.0 (drops `\docType{data}` / `\format{}` /
`\keyword{datasets}` on constant exports; otherwise no behaviour
change).

## dafr 0.2.5

### Query registry parity (CR1 closed)

- **`register_query_operation(kind, name, fn)`** is now exposed as a
  single user-facing entry point for adding custom eltwise / reduction
  ops to the dafr query DSL. Mirrors Julia’s
  [`register_query_operation()`](https://tanaylab.github.io/dafr/reference/register_query_operation.md)
  from `DataAxesFormats.Registry`.

- **Collision errors** now match Julia’s template:

      conflicting registrations for the eltwise operation: <name>
      first in: <file>:<line>
      second in: <file>:<line>

  Previously the error was
  `<name> already registered; use overwrite = TRUE`. The new wording
  includes both registration source locations (captured automatically
  from the caller’s srcref, or supplied explicitly via the new
  `source =` parameter).

## dafr 0.2.4

### CI: regenerate stale man pages

R CMD check on Linux devel / oldrel, Windows, and macOS hit
`code/documentation mismatches` after the v0.2.2 / v0.2.3 ship. Two man
pages were out of date with their code:

- `man/is_leaf.Rd` still documented the S7-generic signature
  `function(daf, ...)`; the R2 refactor in v0.2.2 made
  [`is_leaf()`](https://tanaylab.github.io/dafr/reference/is_leaf.md) a
  plain wrapper `function(daf)`.
- `man/reconstruct_axis.Rd` was missing the `properties_defaults`
  parameter added in v0.2.3.

Both regenerated. No behavioural changes.

## dafr 0.2.3

### Concat / contracts / reconstruction parity (M1 + M4 + C1 + CR3)

- **`concatenate(merge = list(ALL_SCALARS = ...))`** wildcards now
  expand against the source properties at concat time. Mirrors Julia’s
  `[ALL_SCALARS => action]` / `[ALL_VECTORS => action]` /
  `[ALL_MATRICES => action]`. Explicit non-wildcard keys still override
  wildcard-expanded entries. The “can’t collect axis for the scalar:”
  error wording now matches Julia byte-for-byte.
- **`concatenate(prefixed = list(axis = c(...)))`** is now an override
  that fires regardless of `prefix[axis]`. The list names cell-axis
  vectors whose values reference a prefixed axis and thus need the
  dataset name spliced in. Previously dafr ANDed the per-axis prefix
  flag with the list, so vectors listed in `prefixed[cell]` were not
  prefixed when `prefix[cell] == FALSE`.
- **[`merge_contracts()`](https://tanaylab.github.io/dafr/reference/merge_contracts.md)
  rejects `integer` vs `character`** (and other cross-lattice mixes)
  with Julia’s `incompatible type:` error. Previously dafr’s type
  lattice was a total order (`logical < integer < double < character`)
  so any pair quietly merged to the narrower type. Now `character` and
  `integer64` are siblings to the numeric chain - same-chain merges
  still pick the narrower type, cross-chain merges error.
- **`reconstruct_axis(..., properties_defaults = list(prop = val))`**
  merges into a pre-existing axis. Implicit-property values must be a
  subset of the axis entries; any extra entries get the per-property
  default value. Mirrors Julia’s
  `reconstruct_axis!(..., properties_defaults = (; prop = val))`.

## dafr 0.2.2

### Reorder parity (R2 + R4 + R5 + R6 closed)

- **[`is_leaf()`](https://tanaylab.github.io/dafr/reference/is_leaf.md)
  accepts S7 class objects** as well as instances, mirroring Julia’s
  `is_leaf(::Type{<:DafReader})`. Class-level call returns `TRUE` for
  the concrete leaf classes (`MemoryDaf`, `FilesDaf{,ReadOnly}`,
  `ZarrDaf{,ReadOnly}`, `HttpDaf`) and `FALSE` for the abstract
  `DafReader` / `DafWriter` / `DafReadOnly`.
- **`zarr_daf` now supports
  [`reorder_axes()`](https://tanaylab.github.io/dafr/reference/reorder_axes.md)**.
  Best-effort in-place reorder via the existing zarr overwrite path.
  Crash recovery (backup-and-restore) is not yet implemented; a
  mid-reorder crash leaves the store in an undefined state.
- **`reorder_axes(list(d1, d2), axis = perm)` reorders multiple writers
  in one call** (Julia: `reorder_axes!([d1, d2], Dict(...))`). Each axis
  must agree on entry order across every writer that has it
  (`axis: <a> entries differ` error otherwise); writers missing the axis
  silently skip it.
- **`memory_daf` reorder is now atomic**. A pre-reorder snapshot is
  parked on the daf’s internal state; a `SimulatedCrash` mid-reorder is
  rolled back by
  [`reset_reorder_axes()`](https://tanaylab.github.io/dafr/reference/reset_reorder_axes.md),
  which now returns `TRUE` if a pending reorder was rolled back, `FALSE`
  otherwise. Mirrors Julia’s reset_reorder_axes! Bool contract.

## dafr 0.2.0

### Julia parity (DataAxesFormats.jl 0.2.0)

This release closes the bulk of the user-facing semantic divergences
between dafr and DataAxesFormats.jl `main`. The R query DSL now mirrors
Julia byte-for-byte at the operator level; `escape_value` /
`unescape_value` use Julia’s `\<char>` backslash convention.

Operation parity (CO1-CO7 + CT1/CT3 closed):

- **`% Op type=<T>` now coerces the result.** Previously `type` was
  silently ignored on the numeric eltwise ops (Abs / Exp / Sqrt / Round
  / Log / Clamp). Honors `integer` / `numeric` / `double` /
  `Float32`/`Float64` / `Int8-64` / `UInt8-64` / `Bool` / `logical`.
- **`% Log` rejects non-positive `x + eps`** with Julia’s
  `value must be: positive` template instead of returning `NaN`.
- **`% Fraction` rejects integer types** (Julia:
  `value must be: a float type`) and rejects scalar input with Julia’s
  `applying Fraction eltwise operation to a scalar` wording.
- **`% Significant` accepts `low` only** (defaults `high = low`),
  matching Julia.
- **`% GeoMean` error wording** aligned with Julia’s
  `value must be: not negative / for the parameter: eps / for the operation: GeoMean`
  template.
- **`% Abs` / `% Clamp` reject non-numeric `type=`** with Julia’s
  `value must be: a number type` template.
- All eltwise / reduction ops use the same Julia error template:
  `invalid value: "<v>"` / `value must be: <constraint>` /
  `for the parameter: <name>` / `for the operation: <op>`.

Viewer parity (V2-V7 closed):

- **Wildcard `*` in view contracts validates** that values are `=` or
  `NULL` (Julia: `(*, *)`, `(*, prop)`, `(axis, *)` shapes).
- **Scalar-shape validation**: a vector-producing query in a scalar slot
  now errors instead of silently exposing the vector via `get_scalar`.
- **Strict-include semantics**: passing `data = list(...)` to
  `view_daf()` now exposes only the listed properties; `data = NULL`
  retains the original “expose all” behaviour.
- **`__axis__` placeholder substitution** in matrix slot queries.
- **Vector-slot rejects matrix-shape queries** with Julia’s
  `matrix query: ... / for the vector: ...` wording.

Naming parity:

- Grouped reduction results (`-/`, `|/`) now use alphabetical group
  ordering to match Julia’s `factor(..., levels = sort(unique))`, not
  first-appearance order.

Recovered fixes:

- **[`complete_path()`](https://tanaylab.github.io/dafr/reference/complete_path.md)
  returns `NULL`** for memory-backed dafs and non-FilesDaf chains
  instead of erroring; mirrors Julia’s `complete_path` returning
  `nothing` for non-Files dafs.
  [`complete_chain()`](https://tanaylab.github.io/dafr/reference/complete_chain.md)
  retains its explicit error since it requires a real on-disk path.
- **`memory_daf` accepts
  [`Matrix::sparseVector`](https://rdrr.io/pkg/Matrix/man/sparseVector.html)**.
  The atomic-only validator now detects `sparseVector` via S4
  inheritance; readback densifies before name attachment. Storage
  roundtrip preserves values.

Test suite (5579 PASS / 0 FAIL / 142 SKIP, vs 5024 PASS / 0 FAIL / 49
SKIP at the start of the parity push):

- 23 `*-jl-parity.R` files mirror DataAxesFormats.jl’s 22 main test
  files (contracts.jl is split across 7 sub-slices in R for tractable
  ports).
- The remaining 142 skips are out-of-scope feature gaps documented
  per-test (`R divergence` codes), not behavioral divergences:
  multi-contract `@computation`, `description(deep)`, `empty_*`
  builders, h5df backend, file-bridge, tensors-in-views, etc.

### Earlier in 0.2.0: Julia parity for chains, concat, reorder

A literal port of `DataAxesFormats.jl::concat.jl`, `reorder.jl`, and
`chains.jl` test suites surfaced and closed five behavior gaps:

- **`concatenate(..., merge = list("axis|name" = MERGE_COLLECT_AXIS))`
  now honors `empty=` for missing-source columns.** Previously, when a
  source lacked the vector being collect-axis-merged, that source’s
  column in the destination matrix was filled with `NA` regardless of
  the user’s `empty=` map. Now the empty fill is consulted; if neither
  the source nor `empty` provides a value, the same “no empty value”
  error as the per-axis vector path is raised.
- **`concatenate(..., merge = list("rows|cols|name" = MERGE_LAST_VALUE))`
  now actually fires for matrix properties** (rows / cols not in the
  concat set). Previously a silent no-op — the dispatch reached the
  3-part-key case but didn’t write anything. The destination now holds
  the last source’s matrix.
- **[`reorder_axes()`](https://tanaylab.github.io/dafr/reference/reorder_axes.md)
  now errors on missing axes** (was a silent skip). Aligned with Julia’s
  `reorder_axes!` contract.
- **[`reset_reorder_axes()`](https://tanaylab.github.io/dafr/reference/reset_reorder_axes.md)
  now returns `invisible(TRUE/FALSE)`**: TRUE if a pending reorder was
  rolled back, FALSE if no pending. Mirrors Julia’s Bool return.
  Existing callers that ignored the return value (most of them) are
  unaffected.
- **`ReadOnlyChainDaf` / `WriteChainDaf` version counters now propagate
  from underlying sources.** Previously the chain wrappers had their own
  private `*_version_counter` env that never tracked source mutations —
  `vector_version_counter(chain, ...)` returned 0 even after a
  source-side `set_vector` bumped the source’s counter. More
  importantly, this broke cache invalidation on the chain: after
  `set_vector(chain, ..., overwrite = TRUE)` (which routes the write to
  the chain’s writer), reads through the chain returned the cached
  pre-write value instead of the new value. The chain’s stamp / counter
  functions now sum per-source counters.

### Windows support

[`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
now works on Windows. Previously every write failed with
`MmapZipStore is not supported on Windows`. On Windows, dafr skips the
optional `metadata.zip` bundle (only used for serving a FilesDaf over
HTTP via
[`http_daf()`](https://tanaylab.github.io/dafr/reference/HttpDaf.md));
local reads, writes, and round-trips are unaffected.
[`pack_files_daf_metadata()`](https://tanaylab.github.io/dafr/reference/pack_files_daf_metadata.md)
errors with a clear message, and
[`zarr_daf()`](https://tanaylab.github.io/dafr/reference/zarr_daf.md)
rejects `.daf.zarr.zip` paths there — use the unzipped `.daf.zarr`
directory store instead, or run on Linux/macOS for zip-backed storage.

### Named query results

[`get_query()`](https://tanaylab.github.io/dafr/reference/get_query.md)
and the format API now return named values matching the Julia
`NamedVector` / `NamedMatrix` convention:

- Lookup vectors / matrices carry axis-entry names / dimnames.
- Axis listings (`@ cell`, `@ donor [ age > 60 ]`) return character
  vectors with `names == values`.
- IfMissing-default vectors (`@ cell : missing || 0 Int64`) carry names
  too.

This is **a behavior change** — code that does
`expect_equal(get_query(...), unnamed_vec)` may need updating to expect
named results. ALTREP-mmap vectors (`mmap_real` / `mmap_int` /
`mmap_lgl`) preserve their ALTREP status across `names<-`, so the mmap
region stays shared rather than copied.

### Julia parity for `get_query`

Closes the remaining gaps from a literal port of the
`DataAxesFormats.jl::queries.jl` test suite. New query forms supported:

- **Top-level comparators** after `:` / `::` return a boolean vector /
  matrix: `@ cell : score < 1.0`, `@ cell @ gene :: UMIs > 0`.
- **Standalone `:` / `::` with `IfMissing`.** `: age || 1 @ cell = X`
  returns `1` when `age` is missing; same for the matrix form
  `:: UMIs || 0 @ cell = X @ gene = A`.
- **Implicit AsAxis fallback.** `@ cell : type.manual : color` resolves
  through the `type` axis when `type.manual` is not itself an axis.
- **Matrix-column slice auto-relayout.** `@ cell :: UMIs @ gene = A`
  works regardless of `UMIs` storage orientation.
- **Cols-axis mask after a second axis.**
  `@ rows @ cols [ filter ] :: M` filters the cols axis; the matrix
  lookup honours both row and column filters.
- **Virtual `name` property** on every axis. `[ name = X ]` and `: name`
  return the axis-entry vector.
- **Eltwise on scalar.** `. score % Abs` applies element-wise on numeric
  scalars (was: `'%' eltwise requires vector or matrix in scope`).
- **Regex escapes** in masks (`[ type ~ \^\[A-U\] ]`).
- **Empty-string round-trip.** `escape_value("")` is `''`;
  `unescape_value("''")` is `""`.

Stricter error reporting:

- Partial queries (`@ cell @ gene` with no lookup) error with
  `invalid query: <canonical>` instead of silently returning `NULL`. A
  second `?` after a `Names` result also errors.
- Empty-matrix reductions without `IfMissing` always error (was: the
  output-axis-empty branch silently returned an empty vector).
- Numeric reductions on a character matrix error with
  `non-numeric input` instead of leaking base R’s `'x' must be numeric`.
- `?? sentinel : prop` raises a clear parse error when the sentinel
  can’t be coerced to the lookup vector’s type (was: silent `NA` via R’s
  `as.integer` warning).
- Parser errors for unknown operations / parameters and repeated
  parameters now match the Julia DAF wording.
- [`query_axis_name()`](https://tanaylab.github.io/dafr/reference/query_axis_name.md)
  agrees with
  [`get_query()`](https://tanaylab.github.io/dafr/reference/get_query.md)
  on compound-mask queries (`@ cell [ is_low & UMIs @ gene = B ]`).

### Reduction builders

[`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md),
[`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md),
[`Median()`](https://tanaylab.github.io/dafr/reference/Median.md),
[`Min()`](https://tanaylab.github.io/dafr/reference/Min.md),
[`Max()`](https://tanaylab.github.io/dafr/reference/Max.md),
[`Mode()`](https://tanaylab.github.io/dafr/reference/Mode.md),
[`Count()`](https://tanaylab.github.io/dafr/reference/Count.md),
[`GeoMean()`](https://tanaylab.github.io/dafr/reference/GeoMean.md),
[`Quantile()`](https://tanaylab.github.io/dafr/reference/Quantile.md),
[`Std()`](https://tanaylab.github.io/dafr/reference/Std.md),
[`StdN()`](https://tanaylab.github.io/dafr/reference/StdN.md),
[`Var()`](https://tanaylab.github.io/dafr/reference/Var.md),
[`VarN()`](https://tanaylab.github.io/dafr/reference/VarN.md) now
produce the canonical reduction form (`>> Sum`) instead of `% Sum`. The
previous emission was an element-wise op that erred at runtime when
piped after a matrix or vector. **Behavior change**: stored canonical
strings from these builders change from `% Sum` to `>> Sum`.
[`ReduceToColumn()`](https://tanaylab.github.io/dafr/reference/ReduceToColumn.md)
/
[`ReduceToRow()`](https://tanaylab.github.io/dafr/reference/ReduceToRow.md)
accept both shapes for back-compat.
[`canonical_query()`](https://tanaylab.github.io/dafr/reference/canonical_query.md)
also accepts a `DafrQuery` directly.

`>> Mode` / `>| Mode` now accept character and factor inputs, matching
the Julia operation’s documented support for strings.

IfMissing defaults in vector and matrix lookups thread through the
default coercion so `: age || 1` returns an integer column (not a
character one).

### `get_dataframe()` column-spec

[`get_dataframe()`](https://tanaylab.github.io/dafr/reference/get_dataframe.md)
and
[`get_dataframe_query()`](https://tanaylab.github.io/dafr/reference/get_dataframe_query.md)
accept a list mixing positional bare names with `name = ":query"` pairs:

``` r

get_dataframe(d, "cell", columns = list("age", doublet = ":is_doublet"))
```

Mirrors Julia’s `["age", "doublet" => ":is_doublet"]`.

### Mask comparators on factor properties

`[ prop < value ]`, `[ prop > value ]`, etc. on a property stored as a
factor (e.g. an h5ad categorical loaded via `categorical` encoding) now
compare the stored strings lexically, matching Julia. Previously
returned `NA` (unordered factor) or compared level codes (ordered
factor).

### Build hygiene

`.Rbuildignore` excludes `AGENTS.md` and `CLAUDE.md` so they no longer
surface as a `top-level files` NOTE under `R CMD check`.

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
