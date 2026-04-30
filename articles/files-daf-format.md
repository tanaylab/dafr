# FilesDaf on-disk format

This article specifies the on-disk format of a `FilesDaf` store in
enough detail that a third-party implementation can read and write
stores produced by any conforming implementation (currently `dafr` and
[`DataAxesFormats.jl`](https://github.com/tanaylab/DataAxesFormats.jl)).
It is a reference, not a tutorial — see [Getting
started](https://tanaylab.github.io/dafr/dafr.md) for introductory
material.

Line citations of the form `files_format.jl:LN` refer to the reference
Julia implementation (`DataAxesFormats.jl/src/files_format.jl`).

## 1. Overview

A FilesDaf is a directory-based storage format for the `Daf` data model.
The directory tree holds scalars, axes, vectors, and matrices as
individual files, each in a self-describing, trivially machine-readable
form. Because every property lives in its own file, standard OS tools
(`ls`, `wc`, `od`, `make`) can inspect and manipulate the data without
any special library. The trade-off is that the store is a directory
rather than a single archive; shipping it requires `zip`, `tar`, or
similar.

All binary data is stored as a contiguous sequence of fixed-size
elements in **little-endian byte order** with no headers or alignment
padding (`files_format.jl:111–113`). String data is stored one entry per
line, terminated with `\n` regardless of the host OS
(`files_format.jl:115–117`). Dense matrices are stored in
**column-major** (Fortran / Julia-native) layout
(`files_format.jl:57–58`, `files_format.jl:111–113`).

## 2. Directory layout

A valid FilesDaf root directory contains exactly the following structure
(`files_format.jl:18–19`, `files_format.jl:222–226`):

    <root>/
    ├── daf.json                    # required — format sentinel and version
    ├── scalars/                    # required — scalar properties
    ├── axes/                       # required — axis entry lists
    ├── vectors/                    # required — per-axis vector properties
    └── matrices/                   # required — per-axis-pair matrix properties

All four subdirectories are created unconditionally when the store is
initialised (`files_format.jl:222–226`). A directory that lacks
`daf.json` is rejected as “not a daf directory”
(`files_format.jl:230–231`).

**Unknown files or subdirectories** in the root or any subdirectory are
silently ignored by the reader; discovery is by suffix (`.json` for
scalars and property descriptors, `.txt` for axes), so unrecognised
files cause no error (`files_format.jl:959–972`).

## 3. `daf.json` schema

`daf.json` is a UTF-8 JSON object with a single required field:

| Field | Type | Required | Description |
|----|----|----|----|
| `version` | `[integer, integer]` | Yes | `[major, minor]` format version numbers |

The only defined version is `[1, 0]` (`files_format.jl:25`,
`MAJOR_VERSION = 1`, `MINOR_VERSION = 0`, `files_format.jl:155–165`).

**Compatibility rule** (`files_format.jl:239–245`): a reader must reject
a store whose major version differs from the one it knows (`1`), and
must reject a store whose minor version is greater than the maximum it
supports (`0`). A reader may open stores with a lower minor version than
its maximum.

**Example:**

``` json
{"version":[1,0]}
```

(Written with a trailing `\n` by the Julia implementation —
`files_format.jl:220` — but the newline is not semantically significant
for a JSON parser.)

No other top-level keys are defined in version `[1,0]`. Unknown keys
should be tolerated by readers (standard JSON forward-compatibility
practice), but this is not explicitly enforced by the reference
implementation.

## 4. Scalar encoding

Each scalar property is stored as a single JSON file:

    scalars/<name>.json

The JSON object has two required fields (`files_format.jl:28–29`,
`files_format.jl:272–288`, `files_format.jl:303–319`):

| Field   | Type   | Description                                        |
|---------|--------|----------------------------------------------------|
| `type`  | string | Canonical type name of the value (see table below) |
| `value` | varies | The scalar value, encoded as a JSON scalar         |

**Canonical type strings** — the writer-canonical names, as produced by
Julia’s `"$(type)"`:

| Julia / canonical type | Written                |
|------------------------|------------------------|
| `Bool`                 | `"Bool"`               |
| `Int8` – `Int64`       | `"Int8"` … `"Int64"`   |
| `UInt8` – `UInt64`     | `"UInt8"` … `"UInt64"` |
| `Float32`              | `"Float32"`            |
| `Float64`              | `"Float64"`            |
| `String`               | `"String"`             |

**Reading is case-insensitive for known aliases.** The reader at
`files_format.jl:309` accepts both `"String"` and `"string"`, and the
`DTYPE_BY_NAME` dispatch table (`operations.jl:216–242`) maps both
capitalised (`"Float32"`, `"Int32"`, …) and lowercase (`"float32"`,
`"int32"`, …) variants to the same type. Conforming readers must accept
both casings.

**Numeric values** are stored as JSON numbers. `Bool` is stored as
`0`/`1` (JSON integers). `Float32` / `Float64` are stored as JSON
floating-point numbers. Integers are stored as JSON integers.

**Float32 precision and JSON.** A writer SHOULD emit `"Float64"` as the
canonical type for IEEE 754 binary64 scalars. `"Float32"` scalars
written through `JSON.Writer.print` will be formatted with up to 7
significant decimal digits and may round-trip lossily depending on the
JSON library; this imprecision affects only scalars stored as
`"Float32"`. Binary payloads (vector `.data`, matrix `.data`, sparse
`.nzval`) store the exact IEEE representation and are unaffected.
Readers MAY coerce `"Float32"` to the native wider float type of the
host language (e.g., `double` in R / C / Python) since IEEE widening is
exact.

**String scalars** have their value stored directly as a JSON string.

**Examples:**

``` json
{"type":"Float32","value":3.14}
```

``` json
{"type":"String","value":"batch_A"}
```

## 5. Axis encoding

Each axis is a single plain-text file:

    axes/<axis>.txt

Format (`files_format.jl:337–346`, `files_format.jl:390–393`,
`files_format.jl:974–986`):

- **Encoding:** UTF-8.
- **One entry per line.** Each line is terminated by `\n` (written via
  Julia’s `println`, which appends `\n` regardless of OS).
- **Trailing newline:** yes — every entry including the last one is
  followed by `\n`. The reader splits on `\n` and pops (and asserts
  empty) the final element, so the file must end with `\n`
  (`files_format.jl:981–983`).
- **Forbidden characters:** `\n` is explicitly forbidden in axis entry
  names (`files_format.jl:342`). No other characters are forbidden by
  the format code, though the module docstring warns that `/` is
  problematic on POSIX filesystems when property names are used as
  filenames (`files_format.jl:75–79`). Tabs are not forbidden.
- **Length:** the axis length is the number of lines. It is never stored
  separately; it is computed by counting lines.

**Example** for `axes/gene.txt` with three entries:

    BRCA1
    TP53
    MYC

(Each line followed by `\n`; the file ends with `\n` after `MYC`.)

## 6. Dense vector binary format

Dense non-string vectors are stored as two files:

    vectors/<axis>/<name>.json    # metadata descriptor
    vectors/<axis>/<name>.data    # raw binary payload

### 6.1 Metadata JSON

Written by `write_array_json` (`files_format.jl:1041–1060`):

``` json
{"format":"dense","eltype":"<T>"}
```

`<T>` is a canonical type name from Section 4.

### 6.2 Binary payload (`.data`)

- **Layout:** contiguous sequence of elements, one per axis entry, in
  axis order (`files_format.jl:447–449`).
- **Byte order:** little-endian (`files_format.jl:111`).
- **No header, no padding.**
- **Length:** `filesize(path) / sizeof(T)`. The axis length from
  `axes/<axis>.txt` provides the expected element count; the two must
  agree (`files_format.jl:586–591`).
- **Supported element types:** all `StorageReal` types — `Bool`, `Int8`,
  `Int16`, `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`, `UInt64`,
  `Float32`, `Float64` (`storage_types.jl:53–81`).
- `Bool` is stored as one byte per element (`storage_types.jl:31–33`:
  Julia uses `Vector{Bool}`, not `BitVector`).

### 6.3 Dense string vectors (`.txt`)

When `eltype` is `String` and format is `dense`, the payload file uses
the `.txt` extension instead of `.data` (`files_format.jl:425–426`,
`files_format.jl:583–585`):

    vectors/<axis>/<name>.txt

Format matches an axis file: one value per line, UTF-8, `\n`-separated,
trailing `\n`, `\n` forbidden inside values. Length must equal the axis
length.

## 7. Dense matrix binary format

Dense non-string matrices are stored as:

    matrices/<rows_axis>/<columns_axis>/<name>.json
    matrices/<rows_axis>/<columns_axis>/<name>.data

### 7.1 Metadata JSON

``` json
{"format":"dense","eltype":"<T>"}
```

Same structure as for vectors (`files_format.jl:655`,
`files_format.jl:683`, `files_format.jl:1050`).

### 7.2 Binary payload

- **Layout:** column-major (Fortran order) — elements vary fastest along
  the rows axis, then along the columns axis. Julia’s native `Matrix{T}`
  layout is column-major; the data is mmap’d directly as `Matrix{T}`
  with shape `(nrows, ncols)` (`files_format.jl:904–909`,
  `files_format.jl:57–58`, `files_format.jl:111–113`).
- **Shape:** determined from axis lengths, not stored in the file.
  `nrows` comes from `axes/<rows_axis>.txt` and `ncols` from
  `axes/<columns_axis>.txt`. Total expected bytes =
  `nrows * ncols * sizeof(T)`.
- **Byte order:** little-endian.
- **No header, no padding.**
- **Supported element types:** same as dense vectors.

### 7.3 Dense string matrices (`.txt`)

When `eltype` is `String` and format is `dense`, payload uses `.txt`
(`files_format.jl:659`, `files_format.jl:751–759`):

    matrices/<rows_axis>/<columns_axis>/<name>.txt

Values are written in **column-major** order (outer loop over columns,
inner loop over rows, `files_format.jl:752–758`), one value per line,
UTF-8, `\n`-terminated, trailing `\n`, `\n` forbidden inside values.
Total line count must equal `nrows * ncols`.

## 8. Sparse vector format

Sparse real-valued vectors are stored as two or three files:

    vectors/<axis>/<name>.json     # descriptor
    vectors/<axis>/<name>.nzind    # indices of non-zero entries (binary)
    vectors/<axis>/<name>.nzval    # values of non-zero entries (binary, may be absent for Bool)

### 8.1 Descriptor JSON

``` json
{"format":"sparse","eltype":"<T>","indtype":"<I>"}
```

Written by `write_array_json` (`files_format.jl:1052–1054`). `<T>` is
the element type; `<I>` is the index type.

### 8.2 `.nzind` — non-zero indices

- **Element type:** `<I>`, either `UInt32` or `UInt64`.
- **Index type selection**
  (`TanayLabUtilities/matrix_formats.jl:1214–1220`):
  - `UInt32` when the axis length is ≤ `typemax(UInt32)` (~4.3 billion
    entries).
  - `UInt64` otherwise.
- **Index base: 1-based** (Julia-native). Values stored are 1-based
  positions into the axis entry vector. For numeric sparse vectors, data
  comes directly from Julia’s `SparseVector.nzind`, which is also
  1-based.
- **Byte order:** little-endian, contiguous, no header.
- **`nnz` determination:** `div(filesize(nzind_path), sizeof(I))` —
  derived from file size, not stored separately (`files_format.jl:601`).

### 8.3 `.nzval` — non-zero values

- **Element type:** `<T>` (from the JSON descriptor).
- **Element count:** same as `.nzind` (`nnz`).
- **May be absent** when `eltype` is `Bool` and all stored non-zero
  values are `true`. The reader synthesises `fill(true, nnz)`
  (`files_format.jl:615–619`). Writers omit the file when
  `eltype == Bool` and `all(nzval(vector))` (`files_format.jl:437–439`).
- **Byte order:** little-endian, contiguous, no header.

### 8.4 Sparse string vectors (`.nztxt`)

When `eltype` is `String` and the sparse threshold is met, `.nzval` is
replaced by a `.nztxt` text file (`files_format.jl:480–496`):

    vectors/<axis>/<name>.nztxt

Contains only the **non-empty** string values, one per line,
`\n`-terminated, in the same order as the corresponding entries in
`.nzind`. The `.nzind` file is still present and holds the 1-based
positions of those non-empty values.

**Sparse threshold for string vectors** (`files_format.jl:476–479`):
sparse format is chosen over dense only when
`sparse_size ≤ dense_size * 0.75`, where
`sparse_size = nonempty_bytes + n_nonempty * (1 + sizeof(I))` and
`dense_size = nonempty_bytes + total_entries`.

## 9. Sparse (CSC) matrix format

Sparse real-valued matrices use Julia’s `SparseMatrixCSC` layout stored
across three files:

    matrices/<rows_axis>/<columns_axis>/<name>.json    # descriptor
    matrices/<rows_axis>/<columns_axis>/<name>.colptr  # column pointers (binary)
    matrices/<rows_axis>/<columns_axis>/<name>.rowval  # row indices (binary)
    matrices/<rows_axis>/<columns_axis>/<name>.nzval   # values (binary, may be absent for Bool)

### 9.1 Descriptor JSON

``` json
{"format":"sparse","eltype":"<T>","indtype":"<I>"}
```

(`files_format.jl:664–669`, `files_format.jl:1052–1054`)

### 9.2 `.colptr` — column pointers

- **Element count:** `ncols + 1`.
- **Element type:** `<I>` (`UInt32` or `UInt64`).
- **Semantics:** standard CSC column pointer array. `colptr[j]` is the
  1-based index of the first non-zero entry in column `j` within
  `rowval` / `nzval`; `colptr[ncols+1] = nnz + 1`. All values are
  **1-based** (`files_format.jl:730`, `files_format.jl:742`,
  `files_format.jl:920`).
- **`nnz` determination:** computed from `rowval` file size:
  `div(filesize(rowval_path), sizeof(I))` (`files_format.jl:923`).
- **Byte order:** little-endian, contiguous, no header.

### 9.3 `.rowval` — row indices

- **Element type:** `<I>`.
- **Element count:** `nnz`.
- **Semantics:** 1-based row index of each non-zero entry, grouped by
  column as dictated by `colptr` (`files_format.jl:924`,
  `files_format.jl:731–738`).
- **Byte order:** little-endian, contiguous, no header.

### 9.4 `.nzval` — values

- **Element type:** `<T>`.
- **Element count:** `nnz`.
- **May be absent** for `Bool` element type when all values are `true`.
  Writer omits the file when
  `eltype(matrix) == Bool && all(nzval(matrix))`
  (`files_format.jl:673–675`); reader synthesises `fill(true, nnz)`
  (`files_format.jl:946–950`).
- **Byte order:** little-endian, contiguous, no header.

### 9.5 Index type selection

Same rule as for sparse vectors: `UInt32` when the matrix “size”
satisfies `max(nrows, ncols, nnz) ≤ typemax(UInt32)`, `UInt64` otherwise
(`writers.jl:879`). The descriptor’s `indtype` field is the
authoritative source; readers must use it regardless of current axis
sizes.

### 9.6 Sparse string matrices

When `eltype` is `String` and the sparse threshold is met, `.nzval` is
replaced by `.nztxt` (`files_format.jl:721–748`):

    matrices/<rows_axis>/<columns_axis>/<name>.nztxt

Contains non-empty values in column-major order (outer loop columns,
inner loop rows), one per line. The `.colptr` and `.rowval` files are
present and 1-based as usual.

**Sparse threshold for string matrices** (`files_format.jl:718–720`):
`sparse_size = nonempty_bytes + n_nonempty + (ncols + 1 + n_nonempty) * sizeof(I)`
vs `dense_size = nonempty_bytes + nrows * ncols`; sparse chosen when the
ratio is ≤ 0.75.

## 10. Version counter files

There are **no version counter files on disk** in the FilesDaf format.
The version counters (`axis_version_counter`, `vector_version_counter`,
`matrix_version_counter`) are **in-memory only**, stored in
`format.internal.version_counters` (a `Dict`) within the process
(`formats.jl:1158–1170`). They are not persisted across process restarts
and are not part of the on-disk specification. A reader opening a
FilesDaf store starts all version counters at 0.

The only version-related on-disk artifact is the `daf.json` format
version array, which encodes the format spec version, not a mutation
counter.

## 11. Atomicity model

The FilesDaf format provides **no atomic-write or crash-recovery
guarantees** at the filesystem level. Writes are performed by:

1.  Opening the target file with `open(path, "w")` and writing content
    (`files_format.jl:280`, `files_format.jl:339`,
    `files_format.jl:447–449`).
2.  Or writing entire files at once via `write(path, data)` for binary
    arrays (`files_format.jl:436–439`, `files_format.jl:671–675`).

There is no use of `.tmp` sibling files and atomic rename (`mv`). There
is no explicit `fsync` call. A crash mid-write may leave a
partially-written file.

Concurrent-writer safety is handled entirely in-process via Julia read /
write locks (`Formats.has_data_write_lock`), not by filesystem-level
mechanisms. Multi-process concurrent writes to the same FilesDaf store
are **not** safe.

**Single-writer contract.** The FilesDaf format provides no
filesystem-level atomicity. A writer MUST complete every file write
associated with a property (descriptor + payload) before any reader
opens the store, and only one writer may touch a given store at a time.
Multi-process or multi-threaded concurrent writers require external
coordination (e.g., a filesystem lock on the root directory). Readers
MAY observe a partially-written store after a crash mid-write; recovery
is by restoration from backup, not by any in-format rollback. External
writers (outside the reference implementations) MUST follow the same
rule: a property is considered “written” only after ALL of its files —
descriptor JSON plus binary payload(s) — are present on disk.

## Appendix: supported type strings

Both capitalised (writer-canonical) and lowercase (reader-accepted
alias) spellings are listed. The `DTYPE_BY_NAME` table
(`operations.jl:216–242`) maps both to the same type.

| Canonical (written) | Alias (also accepted on read) | Bytes | Notes             |
|---------------------|-------------------------------|-------|-------------------|
| `Bool`              | `bool`                        | 1     | stored as `UInt8` |
| `Int8`              | `int8`                        | 1     |                   |
| `Int16`             | `int16`                       | 2     |                   |
| `Int32`             | `int32`                       | 4     |                   |
| `Int64`             | `int64`                       | 8     |                   |
| `UInt8`             | `uint8`                       | 1     |                   |
| `UInt16`            | `uint16`                      | 2     |                   |
| `UInt32`            | `uint32`                      | 4     |                   |
| `UInt64`            | `uint64`                      | 8     |                   |
| `Float32`           | `float32`                     | 4     | IEEE 754 single   |
| `Float64`           | `float64`                     | 8     | IEEE 754 double   |
| `String`            | `string`                      | n/a   | text, not binary  |

**`Int` / `int` alias.** Writers SHOULD NOT emit `"Int"` or `"int"`:
both Julia (where `Int == Int64` on 64-bit systems) and `dafr` produce
explicit-width names (`Int32`, `Int64`). Readers encountering `"Int"` /
`"int"` MUST deserialise as 64-bit signed integers for compatibility
with hand-written stores or legacy pre-spec writers.
