# H5df backend - design (2026-07-06)

A whole Daf store persisted in a single `.h5df` HDF5 file, interoperable with
`DataAxesFormats.jl`'s `H5df`. The last of the two deferred backends (ZipDaf
shipped in 0.6.0; H5df is next).

## Goal & success criterion

**Bidirectional semantic interop** with Julia `H5df`:

- Julia reads an R-written `.h5df` correctly.
- R reads a Julia-written `.h5df` correctly.
- values / dtypes / entry-names / sparsity are preserved across the round trip.

Unlike ZipDaf (a ZIP of raw byte buffers, verifiable with `cmp`), HDF5 files are
**not byte-identical across writers** - HDF5 owns internal B-trees, allocation
order, and free-space tracking. So the acceptance gate is a **semantic round
trip** through `conda run -n dafr-mcview julia`, not a byte comparison.

## Reference

- Julia: `~/src/DataAxesFormats.jl/src/h5df_format.jl` (module `H5dfFormat`,
  ~2290 LOC). Format version `MAJOR=1`, `MINOR=0`.
- dafr template: `R/zip_daf.R` (the other single-file backend), `R/files_io.R`
  (dtype helpers), `R/anndata_format.R` (existing `hdf5r` usage + the matrix
  orientation precedent).

## On-disk layout

Marker and version:

- A `daf` **dataset** (not a group) at the store root holds `UInt8[1, 0]`
  (`[major, minor]`). Its presence marks the file as an H5df store. **No HDF5
  attributes** are used anywhere for marking or versioning.
- Version check mirrors Julia `verify_daf`: require `major == 1` and
  `minor <= 0`, else error "incompatible format version".

Groups and keys (exact literal names):

- `scalars/` group; scalar -> dataset `scalars/<name>`.
- `axes/` group; axis -> 1-D variable-length UTF-8 string dataset `axes/<axis>`.
- `vectors/` group; vector -> `vectors/<axis>/<name>`: a **dataset** when
  dense/string-dense, a **group** when sparse.
- `matrices/` group; matrix -> `matrices/<rows_axis>/<cols_axis>/<name>` as
  **two nested groups** (rows group, then cols group), **not** a single
  comma-joined key: a **dataset** when dense, a **group** when sparse.
- The store's own name is read from the `scalars/name` dataset if present.

Eager group creation on `add_axis` (mirror Julia so Julia can open R files
without surprises): creating axis `A` also creates the empty groups
`vectors/A` and, for every existing axis `B` (including `A` itself),
`matrices/A/B` and `matrices/B/A`.

## Component encodings

### Scalars

Dataset `scalars/<name>`. Numeric -> matching HDF5 atomic type; string ->
vlen UTF-8. Read back with a plain read.

### Axes

1-D vlen UTF-8 string dataset of the entry names. Length is the dataset length.
Empty axis = a length-0 dataset (valid).

### Dense vectors and matrices

Contiguous, uncompressed ("flat") datasets. dtype follows the R/Julia eltype
via the existing `.dtype_canonical` / `.dtype_for_r_vector` table plus a small
Julia-eltype-string -> `hdf5r` H5T map. `Bool` uses hdf5r's boolean type.

**Matrix orientation (the #1 correctness risk).** Julia writes the HDF5
dataspace with dims *reversed* vs the daf `(rows, cols)` shape, keeping the raw
bytes column-major. `hdf5r` has its own dim/byte-order convention; the existing
AnnData writer compensates with a `t()` ("hdf5r writes an R matrix so h5py sees
its transpose"). This design does **not** derive the transpose on paper: the
plan pins it empirically with an assert-backed Julia round-trip test as the
first matrix step, then implements whatever that test demands (a `t()`, a
reversed-dims `create_dataset`, or an explicit dataspace).

### Sparse matrices

A **group** `matrices/<rows>/<cols>/<name>` with 1-D datasets:

- `colptr` - column pointers, length `ncols + 1`, **1-based** (Julia CSC).
- `rowval` - row indices of non-zeros, **1-based**.
- `nzval`  - non-zero values.

Maps to R `Matrix::dgCMatrix`: `colptr = @p + 1`, `rowval = @i + 1`,
`nzval = @x` (inverse on read: subtract 1). Sparsity is detected purely by
**group vs dataset**; there is no `nnz`/`sparse`/shape attribute. Shape is
recovered from the axis lengths.

**Index type**: Julia's `indtype_for_size(n)` picks `UInt16` (n <= 65535),
`UInt32` (n <= 4294967295), else `UInt64`. A small helper replicates this on
write (`colptr`/`rowval`/`nzind` datasets get the matching unsigned HDF5 type).
Reads accept whatever integer type is present.

### Sparse vectors

A **group** `vectors/<axis>/<name>` with `nzind` (1-based indices) and `nzval`.

**Boolean-all-true optimization** (replicated for read and write): when the
element type is `Bool` and every stored value is `TRUE`, the `nzval` dataset is
**omitted**; on read, its absence means "all `TRUE`".

### String components

Julia has a sparse-string form using `nztxt` (not `nzval`) to distinguish it
from numeric-sparse: sparse string vector = group `nzind` + `nztxt`; sparse
string matrix = group `colptr` + `rowval` + `nztxt`.

- **Read**: replicate both forms (dense vlen-string dataset, and the `nztxt`
  sparse form) so any Julia-written string component reads back.
- **Write** (ponytail cut): always write string vectors and matrices **dense**.
  Julia reads dense strings natively; the `nztxt` sparse form is purely a size
  optimization. **Ceiling**: R never emits the sparse-string form; add it if a
  store's string density makes the dense form wasteful.

## Backend class & wiring (mirrors ZipDaf)

- New file `R/h5df.R`.
- Class pair: `H5df` (writer, parent `DafWriter`) and `H5dfReadOnly`
  (parent `DafReadOnly`).
- `internal` env holds the `hdf5r::H5File` handle, `path`, `mode`; a finalizer
  closes the handle.
- Public constructor
  `h5df(path, mode = c("r", "r+", "w", "w+"), name = NULL)`. Name defaults to
  the `scalars/name` dataset if present, else `basename(path)` - same rule as
  `zip_daf()`. `"w"` on an existing H5df store errors (use `"w+"`); `"r"`/`"r+"`
  on a non-store errors.
- `open_daf()` dispatch: replace `stop("H5df backend not supported yet")` with
  `h5df(uri, mode = mode, name = name)` for `*.h5df`; keep `*.h5dfs#` (grouped)
  **rejected** with an actionable error.
- Implement every `format_*` generic: `has`/`get`/`set`/`delete` for scalars,
  axes, vectors, matrices; `format_*_set` listers; `format_relayout_matrix`;
  `format_replace_reorder` (simplified, see cuts); `.is_leaf_dispatch -> TRUE`;
  `format_description_header`.
- **Not append-only** (unlike ZipDaf): delete, overwrite, and reorder are
  supported. Overwrite is driven by the higher Writers layer (delete-for-set
  then set); delete uses HDF5 `link_delete`.

## Reuse

- `.dtype_canonical`, `.dtype_for_r_vector` (`R/files_io.R`) for eltype names.
- `Matrix` (already an Import) for `dgCMatrix` sparse round-tripping.
- `hdf5r` (already a Suggests, used by the AnnData reader) for all HDF5 I/O -
  **no new dependency**.
- The `ZipDaf` constructor / mode-guard / version-check shape as the template.

## Scope cuts (all Phase-2, YAGNI, each with a named ceiling)

1. **Packed/compressed writing** - write flat/uncompressed only. Julia's default
   read path handles flat natively, and this avoids the blosc/zstd HDF5-filter-
   plugin problem that bit ZipDaf on CI. No `packed` parameter in v1. *Ceiling*:
   add a `packed` mode wiring HDF5 chunk+filter when a caller needs compressed
   H5df.
2. **Blosc/zstd-packed read** - flat and gzip-packed (HDF5 built-in deflate)
   read for free; blosc/zstd-packed datasets need the external filter plugin and
   error clearly if it is absent (mirrors ZipDaf). *Ceiling*: document the plugin
   path; wire it if needed.
3. **Grouped `.h5dfs#/group` stores** - rejected in `open_daf()` / `h5df()`.
   *Ceiling*: add `#/group` addressing + a root-group parameter when a real
   caller needs multi-daf files.
4. **mmap zero-copy reads** - eager `hdf5r` reads (correct, copies). *Ceiling*:
   wire `hdf5r` mmap of contiguous datasets, as FilesDaf does, after measuring.
5. **Reorder crash-safety** - a simple delete-and-rewrite `format_replace_reorder`
   (read the axis's vectors/matrices, permute, delete, rewrite). Correct, but
   not crash-safe and does not preserve mmap. *Ceiling*: port Julia's
   lock/backup/in-place-overwrite machinery if crash-safety matters.
6. **Alignment tuning** (`alignment=(1,8)`) - skipped; only affects mmap
   efficiency, and Julia only warns (never errors) when it is absent.

## Platform / CI

- HDF5 works on Windows (unlike `MmapZipStore`), so H5df need **not** skip on
  Windows.
- Tests gate on `skip_if_not_installed("hdf5r")` (same as the AnnData tests;
  r-lib CI installs Suggests deps).
- Interop tests gate on the julia helper (`helper-julia.R`), like the other
  `test-*-julia-compat.R` suites.

## Testing

- `test-h5df.R` - R round trip: scalars, axes (incl. empty), dense + sparse +
  bool-all-true + string vectors, dense + sparse matrices, delete, overwrite,
  reorder, mode guards.
- `test-h5df-adversarial.R` - malformed/absent files, wrong version, mode-guard
  errors, grouped-path rejection, missing-hdf5r behavior.
- `test-h5df-julia-compat.R` - bidirectional interop: R-writes -> Julia-reads
  and Julia-writes -> R-reads, covering dense, sparse, bool, string, empty, and
  the matrix-orientation pin.

## Non-goals

Compressed/packed writing, grouped multi-daf files, mmap zero-copy reads,
crash-safe reorder, and the sparse-string write form are explicitly out of scope
for this phase (see cuts). They are additive and can land later without
reworking the v1 layout.
