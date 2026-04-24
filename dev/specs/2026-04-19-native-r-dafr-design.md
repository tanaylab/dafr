# Native-R `dafr` — Design Doc

**Date:** 2026-04-19
**Status:** Draft — under user review
**Working directory:** `~/src/dafr-native/`
**Target package name (in DESCRIPTION):** `dafr`
**Relationship to existing package:** The current `dafr` (Julia facade) is to be renamed `DafJuliaWrapper` and kept as a separate package for labs that still depend on H5df / AnnData interop until this native package covers those.

---

## 1. Goals and scope

Reimplement `DataAxesFormats.jl` natively in R + C++, with Julia-level efficiency and first-class interoperability with Julia-produced on-disk data. Drop the JuliaCall bridge entirely.

**In scope:**
- Storage: `MemoryDaf`, `FilesDaf`, future `ZarrDaf` (placeholder generic surface).
- All non-storage subsystems: readers, writers, read-only wrappers, views, chains, queries, operations, contracts, adapters, computations, complete, reconstruction, copies, concat, groups, example data, handler/logging.

**Deferred:**
- `AnnDataFormat` import/export (trivial to wrap later via the R `anndata` package — kept out of v1 to keep surface small).
- `H5df` backend (explicit drop — `FilesDaf` + future `ZarrDaf` cover the durable-storage story).
- `Zarr` backend (implement when DataAxesFormats.jl ships its own, so we can target a stable spec).

**Non-goals:**
- Full cross-thread concurrency at the R level. R's parallelism ecosystem is process-based; shared-memory locking buys nothing here. OpenMP inside C++ kernels only.
- CRAN-style "everything works on all platforms from day one." CRAN-compliant by convention; don't fight unusual configurations up front.

## 2. Decisions summary

| # | Question | Decision |
|---|---|---|
| 1 | Scope | Full DataAxesFormats.jl feature set, including adapters and contracts. |
| 2 | Relationship to current `dafr` | New package, also named `dafr`; old one renamed `DafJuliaWrapper`. |
| 3 | AnnData interop in v1 | Deferred. |
| 4 | Storage backends | MemoryDaf, FilesDaf, future ZarrDaf. Drop H5df. |
| 5 | FilesDaf on-disk compat with Julia | Full bidirectional; upstream a written spec to DataAxesFormats.jl before first write. |
| 6 | Query DSL strategy | Port Julia `tokens.jl` + `queries.jl` literally. |
| 7 | R class system | S7. |
| 8 | Correctness verification | Golden-data regression + property-based tests; no live Julia in CI. |
| 9 | CRAN eligibility | CRAN-compliant pragmatically from v1; goldens/heavy examples skipped on CRAN; compile-time no longer a sharp constraint. |
| 10 | Memory-mapping for FilesDaf | ALTREP + `cpp11`; eager-read fallback via an option. |
| 11 | Sparse matrix representation | `dgCMatrix` facade over ALTREP-mmap slots; raw CSC pointers used in internal C++ kernels. |
| 12 | Concurrency | Single-threaded R API, no locks; OpenMP *inside* C++ kernels. |
| 13 | C++ stack | `cpp11` + raw + BLAS/LAPACK; `RcppArmadillo` as an optional dep for canned algorithms; gated bake-off benchmark against `RcppEigen` in Slice 0. |
| 14 | Caching | Three-tier (MappedData / MemoryData / QueryData), with version-counter invalidation, user-tunable heap cap via `options()`. |
| 15 | Handler/logging surface | R-idiomatic `options()` + `register_dafr_handler()` escape hatch. No port of TanayLabUtilities' enum framework. |
| Decomp | How to stage the port | Thin vertical slices (Slice 0..8), each shippable. |

## 3. High-level architecture

Hierarchy mirrors Julia DAF:

```
DafReader  (S7 virtual)
  ├─ DafReadOnly    (S7 virtual)
  │    ├─ DafReadOnlyWrapper
  │    ├─ DafView
  │    └─ ReadOnlyChain
  └─ DafWriter  (S7 virtual)
       ├─ WriteChain
       ├─ MemoryDaf
       └─ FilesDaf
              (Zarr later: + ZarrDaf)
```

Two parallel API surfaces:

1. **Format layer (internal, unexported)** — S7 generics `format_get_vector()`, `format_set_vector()`, `format_has_matrix()`, etc.; ~40 hooks. A new backend implements only these.
2. **User layer (public)** — `get_vector()`, `set_vector()`, `get_matrix()`, `add_axis()`, etc. Implemented once on top of the format layer. Handles caching, version counters, layout tracking, error messages.

**Where R and C++ meet:**
- R owns: S7 class hierarchy, Daf metadata (axes, names, version counters, cache), query parser + AST, orchestration.
- C++ owns: ALTREP mmap-backed vectors and `dgCMatrix` slot providers; hot-path kernels (eltwise, reductions, CSC↔CSR transpose, sparse slice); query tokenizer (hand-ported from Julia); `FilesDaf` binary I/O + `daf.json` manifest handling.
- Query parser lives in R (AST iteration and debugging is easier there); query executor dispatches to C++ kernels per node.

**Deliberate departures from Julia:**
- No thread-safety locks at the R level (Q12).
- R has no native unsigned integers; DAF `UInt32` axis indices round-trip as R `integer` with overflow checked at the boundary.
- Handler framework via `options()` + a single registration function (Q15), not Julia's enum-based machinery.

## 4. S7 class skeleton and format hooks

### Daf object shape

```r
DafReader <- new_class(
  "DafReader",
  abstract = TRUE,
  properties = list(
    name                     = class_character,
    internal                 = class_environment,  # caches, locks-if-any
    cache                    = class_environment,  # LRU tiers (Mapped / Memory / QueryData)
    axis_version_counter     = class_environment,
    vector_version_counter   = class_environment,
    matrix_version_counter   = class_environment
  )
)
```

All mutable state lives in `class_environment` slots — S7 properties are set-on-the-outside; holding mutable state in environments avoids Daf copies on cache updates.

### Low-level hooks (unexported S7 generics in `R/format_api.R`)

Mirror Julia's `FormatReader`/`FormatWriter` contract 1:1. Naming convention: `format_*`, verb-first.

```
# Scalars
format_has_scalar(daf, name) -> logical
format_get_scalar(daf, name) -> scalar
format_set_scalar(daf, name, value, overwrite) -> invisible
format_delete_scalar(daf, name, must_exist) -> invisible
format_scalars_set(daf) -> character

# Axes
format_has_axis(daf, axis) -> logical
format_add_axis(daf, axis, entries) -> invisible
format_delete_axis(daf, axis, must_exist) -> invisible
format_axes_set(daf) -> character
format_axis_array(daf, axis) -> character
format_axis_length(daf, axis) -> integer
format_axis_dict(daf, axis) -> environment  # name -> 1-based index

# Vectors (per-axis namespace)
format_has_vector(daf, axis, name) -> logical
format_get_vector(daf, axis, name) -> vector
format_set_vector(daf, axis, name, vec, overwrite) -> invisible
format_delete_vector(daf, axis, name, must_exist) -> invisible
format_vectors_set(daf, axis) -> character

# Matrices ((rows_axis, columns_axis) namespace, CSC canonical)
format_has_matrix(daf, rows_axis, columns_axis, name) -> logical
format_get_matrix(daf, rows_axis, columns_axis, name) -> matrix | dgCMatrix
format_set_matrix(daf, rows_axis, columns_axis, name, mat, overwrite) -> invisible
format_delete_matrix(daf, rows_axis, columns_axis, name, must_exist) -> invisible
format_matrices_set(daf, rows_axis, columns_axis) -> character
format_relayout_matrix(daf, rows_axis, columns_axis, name) -> invisible
```

~40 hooks total. User-facing `get_vector()` etc. in `R/readers.R` / `R/writers.R` call through and layer on caching, version bumps, layout tracking, error messaging.

### Matrix layout tracking

A matrix is stored at most twice — once with rows-axis-first and once with columns-axis-first. `get_matrix(rows, cols, name)` looks up `(rows, cols)`; if absent but `(cols, rows)` exists, either transpose-on-the-fly (sparse) or return a view with flipped access. `relayout_matrix()` physically writes the other layout so subsequent calls skip the transpose.

### Caveats

- `format_axis_dict()` returns an `environment` (hash table), not Julia's `OrderedDict`. Same O(1) lookup, no iteration-order guarantees. Works because entry-name → index maps never need ordered iteration; if something does, use a named integer vector instead.
- S7 generics don't support keyword args elegantly; use positional args.

## 5. Storage backends

### MemoryDaf

```
MemoryDaf properties:
  scalars  : env( name -> value )
  axes     : env( axis_name -> list(entries = character, dict = env) )
  vectors  : env( axis_name -> env( vector_name -> vector ) )
  matrices : env( rows_axis -> env( columns_axis -> env( name -> matrix | dgCMatrix ) ) )
```

R environments are hash tables with O(1) lookup; mutable by reference. No mmap. `get_vector()` returns the stored SEXP (unless layout tracking interposes).

### FilesDaf on-disk spec

Match Julia byte-for-byte:

```
<daf_root>/
  daf.json                         # version, axes list, content manifest
  scalars/
    <name>.json                    # {"type": "Float64", "value": 3.14}
    <name>.txt                     # for string scalars
  axes/
    <axis_name>.txt                # one entry per line, UTF-8
    <axis_name>_version.txt        # integer
  vectors/
    <axis_name>/
      <vector_name>.<T>.bin        # raw binary, little-endian, N * sizeof(T)
      <vector_name>.json           # {"type": "Float32", "length": N, "nbytes": ...}
      <vector_name>_version.txt
  matrices/
    <rows_axis>/
      <columns_axis>/
        <name>_dense.<T>.bin       # row_count * col_count * sizeof(T), column-major
        <name>_sparse_nzval.<T>.bin
        <name>_sparse_rowval.Int32.bin
        <name>_sparse_colptr.Int32.bin
        <name>.json                # {"type": "Float64", "storage": "sparse"|"dense", ...}
        <name>_version.txt
```

The exact filenames/extensions and byte placement get verified against `files_format.jl` during Slice 0 and upstreamed as a written spec to DataAxesFormats.jl before any write code lands in `dafr`. This turns an implicit spec into an explicit one that both packages agree on.

**Read path:** `format_get_vector(daf, axis, name)` → parse `<name>.json` → `mmap(2)` the `.bin` → return an ALTREP vector whose data pointer is the mmap region. For sparse matrix: mmap 3 files, construct `dgCMatrix` with ALTREP slots.

**Write path:** `open_temp_writer()` writes into a sibling `.tmp/` dir; on `close()`, `fsync` each file and atomic `rename()`. Bumps the in-memory version counter and rewrites `_version.txt`.

### Mmap via ALTREP (per Q10)

ALTREP classes, one per primitive R type:

- `MmapRealAltrep` → `numeric` (double)
- `MmapIntAltrep` → `integer` (int32)
- `MmapLglAltrep` → `logical` (int32 R storage)

Each holds `std::shared_ptr<MmapRegion>` (owns `void*`, length, mode, fd). Methods: `Dataptr`, `Length`, `Get_region`, `Elt`, `Is_sorted`, `No_NA`, `Serialized_state` / `Unserialized_state` (for `saveRDS` — serializes as an ordinary vector since a receiver won't have the mmap).

**`dgCMatrix` slots.** `x`, `i`, `p` are normal R slots; we install ALTREP vectors via `R_do_slot_assign()`. `Matrix`'s code reads slots through `x@i` etc. — hits ALTREP `Dataptr` zero-copy.

Slice 0 ships a smoke test running `Matrix::colSums`, `Matrix::rowSums`, `Matrix::t()`, `Seurat::CreateSeuratObject`, `scran::quickCluster` against a mmapped `dgCMatrix` to catch any `DATAPTR` assumptions in downstream code.

**Writes to mmapped slots.** Mmap is read-only by default. `x@x[1] <- 5` hits `Dataptr(writeable=TRUE)`; we refuse and trigger materialization — ALTREP duplicates into a regular vector, slot gets replaced. Standard ALTREP copy-on-write pattern.

### Fallback path (Q10 option B)

If mmap fails (platform, permissions, `options(dafr.mmap = FALSE)`): `readBin()` the whole `.bin` into a regular vector. Same `get_vector()` signature. Slower first access, identical thereafter.

### Zarr hook (deferred)

Generic format layer absorbs a `ZarrDaf` class when Julia DAF ships Zarr. R-side library: `Rarr` (CRAN) for v2; defer v3 until the R ecosystem stabilizes.

## 6. C++ stack, kernels, benchmarks

### Stack (Q13)

- `cpp11` for R↔C++ glue (faster compile, cleaner headers).
- BLAS/LAPACK via `R_ext/BLAS.h` + `R_ext/Lapack.h` for dense linalg.
- `RcppArmadillo` as *optional* dep for canned algorithms only (sparse LU, dense QR, stable summation). Configure-gated.
- No `RcppEigen` in v1 pending the bake-off.
- OpenMP via `SHLIB_OPENMP_CFLAGS` in `src/Makevars.in`; serial fallback if unavailable.

### Kernel inventory

```
src/
  altrep_mmap.cpp
  mmap_region.cpp
  files_io.cpp
  sparse_csc.cpp
  kernels_eltwise.cpp
  kernels_reduce.cpp
  kernels_transpose.cpp
  kernels_slice.cpp
  kernels_matvec.cpp
  query_tokens.cpp
  openmp_shim.hpp
```

Each kernel takes raw pointers and returns either via output pointers or a fresh `SEXP`; no Armadillo/Eigen expression templates in the hot path.

### OpenMP pattern

```cpp
#pragma omp parallel for if(n >= kParallelThreshold) schedule(static)
for (int32_t j = 0; j < ncols; ++j) {
    double s = 0.0;
    for (int32_t k = p[j]; k < p[j+1]; ++k) s += x[k];
    out[j] = s;
}
```

`kParallelThreshold` prevents OMP overhead from dominating small inputs. User-tunable via `options(dafr.omp_threshold = N)`.

### Benchmark harness (blocks Slice 3)

`inst/benchmarks/bench.R`, driven by `bench::mark()`, compares:

- Current Julia `dafr` facade (for reference).
- Native `dafr` under mmap (path A).
- Native `dafr` under eager-read (path B).

Workloads target 30K cells × 30K genes, ~10% sparse density:

1. `open_daf()` cold — target < 50 ms regardless of size.
2. `get_vector("cell", "n_counts")` cold — target < 5 ms.
3. `get_matrix("cell", "gene", "UMIs")` → `dgCMatrix` — cold < 10 ms, warm < 1 ms.
4. `colSums(UMIs)` — within 1.5× of Julia DAF sum reduction.
5. Query `/ cell @ metacell / gene : UMIs %> Sum` — within 2×.
6. Sparse slice by 20% cell mask — within 2×.
7. `FilesDaf` write of 30K×30K sparse — within 2×, fsync-durable.

### C++ stack bake-off (gate for Slice 3)

Three kernels under both stacks, same input:

| Kernel | Input | D (cpp11 + raw + BLAS) | B (RcppEigen) | Gate |
|---|---|---|---|---|
| `log(x) + y` eltwise | 30K × 30K dense | TBD in Slice 0 | TBD in Slice 0 | D wins → keep D |
| CSC column-sum | 100K × 1M, 1% sparse | TBD in Slice 0 | TBD in Slice 0 | D wins → keep D |
| CSC → CSR transpose | 100K × 100K, 5% sparse | TBD in Slice 0 | TBD in Slice 0 | D wins → keep D |

Eigen wins by >20% on any → reopen the decision and mix Eigen in for that kernel family.

### CRAN compile-time budget

Not a sharp constraint anymore; we treat it as a soft budget. Separate `.cpp` files per kernel still worth it for debug ergonomics.

## 7. Caching model (Q14)

### Three tiers (matches Julia)

| Tier | What | Backed by | Eviction |
|---|---|---|---|
| `MappedData` | ALTREP mmap views over `FilesDaf` bins | OS page cache | never (OS decides) |
| `MemoryData` | R vectors materialized from disk, or user-supplied | R heap | LRU under heap cap |
| `QueryData` | results of query execution, views, reductions | R heap | LRU under heap cap |

One shared `cache` environment per Daf:

```
cache
  ├─ mapped    : env( cache_key -> SEXP )
  ├─ memory    : env( cache_key -> SEXP )
  ├─ query     : env( cache_key -> list(value, deps, stamp) )
  └─ lru_list  : doubly-linked list (C++) for O(1) touch/evict
```

Cache keys:
- Scalars: `"scalar:<name>"`
- Axes: `"axis:<axis>"`
- Vectors: `"vector:<axis>:<name>"`
- Matrices: `"matrix:<rows_axis>:<cols_axis>:<name>"`
- Queries: `"query:<canonicalized_query_str>"`

### Version counters

Per-axis, per-vector-space, per-matrix-space. On mutation (`set_vector`, `delete_axis`, …) counters bump. Query cache entries record the version stamps of their dependencies; before returning a cached query result we check stamps — if any diverged, invalidate.

### User-tunable options

```
options(
  dafr.cache.memory_mb = 1024L,
  dafr.cache.disable   = FALSE,
  dafr.cache.stats     = FALSE,
  dafr.mmap            = TRUE,
  dafr.omp_threshold   = 10000L
)
```

`empty_cache()` / `empty_cache(group = c("memory", "query"))` mirrors Julia's `empty_cache!`.

### Interaction notes

- **Mapped data outside the heap cap** — it's OS-pageable, not R heap. Opening a 500 GB FilesDaf uses a few MB of process RSS until data is touched.
- **Cached `dgCMatrix` with ALTREP slots** — slot contents are pointers to the mmap region; cache entries are tiny. Evicting frees the shell; mmap stays alive until no ALTREP refs it (`std::shared_ptr`).
- **Copy-on-write materializations** — user mutations to mmapped slot contents produce a materialized copy in the user's hands; the cache still holds the original shell. Next `get_matrix()` returns the mapped original, which is correct: reads come from the backend, not from user-side mutations.

## 8. Query DSL port (Q6)

### Pipeline

```
query string  -->  tokenize  -->  parse  -->  AST  -->  plan  -->  execute  -->  named vector / matrix / scalar
                    (C++)        (R)                    (R)         (R + C++ kernels)
```

### Tokenizer (C++ port of `tokens.jl`)

`tokens.jl` (~290 lines) ports to `src/query_tokens.cpp`. Output: `vector<Token>` with `{kind, text, start, end}`. Kinds mirror Julia's `TokenKind` exactly. In C++ because (a) string-heavy loops are slow in R, (b) canonicalize-and-hash for query caching benefits from speed, (c) no R-side dependencies.

Golden tokens extracted from Julia test suite at port time, pinned in the R package.

### Parser (R port of `queries.jl`)

Pure R recursive-descent. AST node classes defined as S7:

```
QueryOp (abstract)
  ├─ Lookup          ("/axis")
  ├─ Fetch           ("/axis : property")
  ├─ Filter          ("&> name = value", "&> property < 0.5")
  ├─ Eltwise         ("%> Abs", "%> Log")
  ├─ Reduction       ("%> Sum")
  ├─ Slice           ("@group", "/axis = entry")
  ├─ MaskOp          ("&& other_query")
  └─ ...  (match queries.jl 1:1)
```

Field layout matches Julia; execute methods port mechanically.

### Executor

S7 generic `execute(op, daf, context)`, one method per node kind. `QueryContext` threaded through recursion (current axis, mask, intermediates). Calls C++ kernels for heavy lifting.

### Query result caching

Canonicalize → lookup in `cache$query` → stamp check → return / recompute. Same policy as Julia `QueryData` tier.

### Named-array semantics

- Vectors: `setNames()` — native R.
- Matrices: `dimnames(m)` — native R.
- Scalars: bare.

No custom `NamedArray` class — R has what the Julia package had to build a library for.

### Deliberate omissions in v1

- `@query_str` macro — R has no string macros; users pass ordinary strings.
- `QueryOperation` extension protocol — YAGNI for v1.

### Testing

Golden regressions: hundreds of query strings with their Julia-executed results. Property tests: idempotent `Abs` on non-negative data, commutativity of `%> Sum %> Sum`, etc.

## 9. Views, Chains, ReadOnly

### `DafReadOnlyWrapper`

Wraps a writer; rejects writes; passes reads through. Most mutating generics are one-line `stop("Daf is read-only")`; reads delegate to `daf@inner`.

### `DafView`

Defined by a set of query expressions. Internally:

- `inner` — backing reader
- `axes_map` — env( new_axis_name -> (old_axis_name, mask_or_query) )
- `vectors_map` — env( axis_name -> env( vector_name -> query_op ) )
- `matrices_map` — env( rows_axis -> env( cols_axis -> env( name -> query_op ) ) )

**Axis aliasing is kept:** same axis can be exposed under two different names inside a view (e.g., `gene` → `g`) — matches Julia and pays off when adapters need to rename.

Zero-copy identity policy: when a view exposes an axis unmodified and a vector without transformation, `format_get_vector(view, axis, name)` returns the same SEXP `inner` returned. No copy; query cache handles reuse.

Port target: `views.jl` (~1100 lines) → ~1400 R lines (S7 adds boilerplate).

### `ReadOnlyChain` / `WriteChain`

Ordered list of readers. `format_has_*` walks front-to-back; `format_get_*` returns first hit; `format_*_set` unions across layers. Writes route to the designated `write_target` (one writer per chain, same as Julia).

**Axis agreement:** fail fast at construction if two chained Dafs declare the same axis with different entry lists; show the diff.

### ReadOnly views of writers

`read_only(writer)` wraps in `DafReadOnlyWrapper`; shares cache and version counters so invalidation works when the underlying writer is mutated elsewhere.

### Port size estimates

- `views.jl` (1100) → ~1400 R
- `chains.jl` (830) → ~1000 R
- `read_only.jl` (220) → ~280 R

All land in Slice 5.

## 10. Contracts + Adapters + Computations

### Contracts (`contracts.jl` → ~1600 lines)

`ContractExpectation` enum: `RequiredInput`, `OptionalInput`, `CreatedOutput`, `GuaranteedOutput`, `OptionalOutput` — exact same set as current `dafr`'s `R/contracts.R` (preserve user muscle memory).

```r
Contract <- new_class("Contract", properties = list(
  axes       = class_list,
  scalars    = class_list,
  vectors    = class_list,   # keyed "axis:name"
  matrices   = class_list,   # keyed "rows,cols:name"
  is_relaxed = class_logical
))
```

Validation runs at two points:
1. Pre-call: every `RequiredInput` exists and is type-compatible.
2. Post-call: every `GuaranteedOutput`/`CreatedOutput` has been written.

Failures throw with a formatted report.

### Computations (`computations.jl` → ~360 lines)

Higher-order function `computation(contract, fn)` that returns a wrapped function; introspectable via `contract_of(compute_metacells)`.

```r
compute_metacells <- computation(
  contract(
    axes    = list(cell = required_input(), gene = required_input()),
    matrices = list("cell,gene:UMIs" = required_input(type = "Int32")),
    vectors  = list("cell:metacell"  = created_output(type = "String"))
  ),
  function(daf, ...) { ... }
)
```

### Adapters (`adapters.jl` → ~120 lines)

Builds a renaming `DafView` on the fly, runs `fn` against it, projects outputs back under the original names.

```r
with_adapter(
  daf,
  axes    = c("obs" = "cell", "var" = "gene"),
  vectors = c("obs:total" = "cell:n_counts"),
  fn      = compute_metacells
)
```

### Typed scalars and vectors

Type names map: `Int32 -> integer`, `Float64 -> double`, `String -> character`, `Bool -> logical`, `Int64 -> bit64::integer64`, `UInt*` with range checks.

### Forward-compat note

Because contracts use S7 generics, the same machinery can dispatch on types beyond `Daf` in future (e.g., `SingleCellExperiment`). Not in v1; design doesn't preclude it.

## 11. Operations and tail subsystems

### Operations (`operations.jl` → ~1860 lines)

**Eltwise:** `Abs`, `Round`, `Clamp`, `Convert`, `Fraction`, `Log`, `Significant`, `Sqrt`, `Type`. Dispatch on input type (dense/sparse, numeric/int). Pure C++ kernels + OpenMP on large inputs.

**Reductions:** `Count`, `CountBy`, `GeoMean`, `Max`, `Mean`, `Median`, `Min`, `Quantile`, `Std`, `StdN`, `Sum`, `Var`, `VarN`, `All`, `Any`. Dispatch on (dense/sparse) × (row/column). Output is a vector indexed by the non-reduced axis.

Power-user surface:

```r
op_sum(mat, axis = "columns")
op_mean(vec)
```

S7 generic `execute_operation(op, input, ctx)` — one method per op × input-type. ~30 ops × ~50 R LoC + ~80 C++ LoC each = ~1500 R + 2400 C++. Biggest chunk after queries.

### Groups / Complete / Reconstruction / Copies / Concat / ExampleData

- **Groups** (~100 lines): grouping lookup tables for `@group` query reductions.
- **Complete** (~160 lines): fills default vectors/scalars so queries don't fail on missing optional data.
- **Reconstruction** (~230 lines): recovers axes from implicit cross-products.
- **Copies** (~1040 lines): cross-Daf copy with renaming / retyping; uses contracts + adapters; lands after Slice 6. Zero-copy mmap→file path in C++ to avoid heap materialization.
- **Concat** (~1400 lines): merges Dafs along shared axes; custom CSC block-assembly in C++; streams through one block at a time to keep memory bounded.
- **ExampleData** (~260 lines): synthetic datasets; same entry names/values as Julia's to compare side-by-side.

## 12. Package scaffold

### File layout

```
~/src/dafr-native/                    # Package: dafr
├── DESCRIPTION
├── NAMESPACE                         # auto-generated
├── configure.ac + configure.win      # OpenMP + mmap availability
├── src/
│   ├── Makevars.in
│   ├── altrep_mmap.cpp
│   ├── mmap_region.cpp
│   ├── files_io.cpp
│   ├── sparse_csc.cpp
│   ├── kernels_eltwise.cpp
│   ├── kernels_reduce.cpp
│   ├── kernels_transpose.cpp
│   ├── kernels_slice.cpp
│   ├── kernels_matvec.cpp
│   ├── query_tokens.cpp
│   └── openmp_shim.hpp
├── R/
│   ├── classes.R
│   ├── format_api.R         # S7 generics
│   ├── memory_daf.R
│   ├── files_daf.R
│   ├── readers.R
│   ├── writers.R
│   ├── cache.R
│   ├── op_<name>.R          # one per operation
│   ├── query_parse.R
│   ├── query_ast.R
│   ├── query_exec.R
│   ├── views.R
│   ├── chains.R
│   ├── read_only.R
│   ├── contracts.R
│   ├── adapters.R
│   ├── computations.R
│   ├── concat.R
│   ├── copies.R
│   ├── complete.R
│   ├── reconstruction.R
│   ├── groups.R
│   ├── example_data.R
│   ├── handlers.R
│   ├── options.R
│   └── utils.R
├── inst/
│   ├── extdata/             # example FilesDaf directories
│   ├── benchmarks/          # harness
│   ├── goldens/             # NOT installed for CRAN
│   └── specs/               # upstreamed FilesDaf-on-disk spec
├── tests/testthat/
│   ├── test-memory-daf.R
│   ├── test-files-daf.R
│   ├── test-mmap.R
│   ├── test-cache.R
│   ├── test-queries-parser.R
│   ├── test-queries-exec.R
│   ├── test-goldens.R       # skip_on_cran()
│   ├── test-property.R
│   └── test-contracts.R
├── vignettes/
│   ├── getting-started.Rmd
│   ├── file-format.Rmd
│   ├── query-language.Rmd
│   └── contracts.Rmd
└── docs/superpowers/specs/
    └── 2026-04-19-native-r-dafr-design.md   # this document
```

### Dependencies

**Hard:** `S7`, `cpp11` (LinkingTo), `Matrix`, `cli`, `bit64`.
**Suggested:** `testthat (>= 3.0.0)`, `hedgehog`, `bench`, `knitr`, `rmarkdown`, `withr`, `DafJuliaWrapper` (for regenerating goldens only).
**Optional:** `RcppArmadillo` (configure-gated), `Rarr` (Zarr backend plumbing, Slice 8).

### R floor

R >= 4.4 (same as current `dafr`; matches S7 version requirements).

## 13. Dev workflow

In-place development — no install step during normal work:

- Compile C++ after changes: `pkgbuild::clean_dll(); pkgbuild::compile_dll(debug = FALSE)`.
- Load the package: `devtools::load_all()`.
- Style + roxygen: `alutil::sad()`.
- Run tests: `alutil::tst(parallel = TRUE)`.

This sidesteps the "two packages named `dafr` can't coexist" install problem during dev — `load_all()` pulls the package in without touching the library.

### Migration path (post-development)

1. Throughout development the current `dafr` (Julia facade) stays installed; `load_all()` shadows it per-session.
2. When Slices 0–4 are stable, rename the facade's GitHub repo: `dafr` → `DafJuliaWrapper`; update its `DESCRIPTION`; cut a new CRAN submission under the new name.
3. Cut native `dafr` v1.0 over the old one on CRAN. `install.packages("dafr")` now gets the native version.
4. `DafJuliaWrapper` stays available for labs needing H5df / AnnData interop until Slice 8 lands.

## 14. Testing infrastructure

- **Unit** — per-module under `tests/testthat/`.
- **Goldens** — `test-goldens.R` loads `inst/goldens/<case>.json` (expected outputs) + `inst/goldens/<case>/` (input FilesDaf); asserts equality. `skip_on_cran()`; runs in GitHub Actions with `DAFR_RUN_GOLDENS=1`. Regeneration driven by `inst/goldens/regenerate.jl` (uses `DafJuliaWrapper`).
- **Property** — `test-property.R` with `hedgehog` generators. Invariants: round-trip identity, query-cache hit equivalence, view zero-copy on identity fetches.
- **Cross-check** — separate workflow that runs the same inputs through both packages and diffs (opt-in, not in main test suite).

### CI

- `R CMD check` on linux-x86_64, macOS, Windows per PR.
- **ALTREP sanity** job: load a 10 GB FilesDaf, assert `get_matrix()` < 100 ms, peak RSS < 500 MB.
- **Golden diff** job: runs `test-goldens.R`.
- **Nightly bench** job on a dedicated runner; not a PR gate (perf noise).

## 15. Decomposition — thin vertical slices

| Slice | Ships |
|---|---|
| 0 | Scaffold, benchmark harness, ALTREP POC + `Matrix`/`Seurat`/`scran` smoke test, C++ stack bake-off (D vs B), upstreamed FilesDaf spec doc |
| 1 | `MemoryDaf` + scalar/vector/matrix get/set + axes + cache infrastructure (MemoryData tier active; MappedData tier waits for Slice 2, QueryData tier for Slice 3) + handler framework |
| 2 | `FilesDaf` + mmap + bidirectional Julia compat; `readBin` fallback path |
| 3 | Eltwise ops + reductions + tokenizer + simple queries (axis-fetch, masks, single reduction — nothing requiring `@group`, cross-axis joins, or `&&` composition) |
| 4 | Full query DSL (groups, joins, `&&`) + QueryData cache |
| 5 | Views + Chains + ReadOnly + axis aliasing in views |
| 6 | Contracts + Computations + Adapters |
| 7 | Concat + Reconstruction + Copies + Complete + Groups + ExampleData |
| 8 (deferred) | AnnData interop, Zarr backend |

Each slice is independently releasable. Benchmarks run starting from Slice 1. Goldens accumulate with each slice.

## 16. Risks

1. **ALTREP/`Matrix` compatibility.** Some `Matrix` internals may bypass ALTREP and assume `DATAPTR(s)` is a real heap pointer. Slice 0 smoke test against `Matrix::colSums`, `Matrix::rowSums`, `Matrix::t`, `Seurat::CreateSeuratObject`, `scran::quickCluster`. Fallback if that fails: eager-read for sparse matrices, mmap only for dense vectors.
2. **Sparse matvec performance vs Julia.** Hand-rolled C++ may lose to Julia's `SparseMatrixCSC` kernels. Slice 0 benchmark; Armadillo fallback if hand-rolled loses by >20%.
3. **Query DSL spec drift.** `queries.jl` is under active development. Pin to a specific DAF commit at port start; record commit in DESCRIPTION; resync deliberately per release.
4. **FilesDaf on-disk spec is implicit in Julia source.** Slice 0 deliverable: extract the spec, upstream as a design doc to DataAxesFormats.jl — both packages agree on a written contract.
5. **R 32-bit `integer` vs DAF `Int64`.** Use `bit64::integer64`; acknowledge `integer64` arithmetic is slower than native `integer`.
6. **Effort scale.** Honest estimate: ~6–9 months for one experienced dev through Slice 7. Slice 8 is another 1–2 months.

## 17. Open secondary decisions (to resolve inside Slice 0)

- R version floor (proposing R >= 4.4).
- `integer64` everywhere `Int64` appears vs refusing at the R boundary (proposing `integer64`).
- FilesDaf `daf.json` versioning: match Julia's existing `"format_version"` field byte-for-byte (proposing yes).
- Sparse index files with `UInt32`/`Int64` indices: R's `Matrix` uses `integer`. Proposal: convert on read if overflow-safe, else error; document `Int32`-indexed path as the fast path.
- `dgRMatrix` for row-major sparse matrices vs always transposing to `dgCMatrix` on return: proposing `dgCMatrix`-only at the user surface because downstream R sparse code mostly doesn't handle `dgRMatrix` well.
- Whether to port Julia's `TanayLabUtilities.Logger` or use `cli::cli_*` / `rlang::inform`: proposing the latter (R-idiomatic).

## 18. References

- `DataAxesFormats.jl` — upstream Julia implementation.
- Current `dafr` (to be renamed `DafJuliaWrapper`) — facade of record at `~/src/dafr/`.
- Brainstorming transcript (2026-04-19, this session) — reasoning behind each of the 15 decisions.
