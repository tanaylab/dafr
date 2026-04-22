# dafr (development version)

## Slice 9d-M — G3 grouped-CSC memory fix (2026-04-22)

### Performance

* **Row-partition rewrite of the axis = 3 branch of three grouped CSC
  kernels** (`kernel_grouped_reduce_csc`, `kernel_grouped_mode_csc`,
  `kernel_grouped_quantile_csc`). The prior implementation allocated an
  `O(nthreads × nrow × ngroups)` vector of thread-local accumulator
  buckets and merged them serially after the parallel scan. On a stress
  fixture of 10⁴ × 10⁴ CSC with 100 groups, that pattern grew to 7.34 GB
  at 128 threads and the serial merge made the 128-thread dispatch 10-
  30× *slower* than single-threaded. The rewrite gives each thread a
  disjoint row range `[r0, r1)` and lets it write directly into a single
  shared output-shaped accumulator; no thread buckets, no merge. Peak RSS
  at 128 threads drops to 340 MB (-7.0 GB), and wall-time at 128 threads
  is now 29–103× faster than the pre-fix value across the measured
  kernel variants. Single-threaded behaviour is bit-identical to the
  prior path (no bake-off regression); the fix is memory-and-perf at
  scale, invisible at OMP\_NUM\_THREADS = 1.

## Slice 9c — Dense perf closure (2026-04-22)

### Performance

* **Dense Int-aware Quantile, Mode, and grouped Min/Max kernels.** Three
  new cpp11 kernels — `kernel_quantile_dense_cpp`, `kernel_mode_dense_cpp`,
  `kernel_grouped_minmax_dense_cpp` — replace the prior
  `matrixStats::colQuantiles` / `apply(.op_mode)` /
  `matrixStats::rowMaxs`-in-loop paths for dense-layout queries on Int32
  matrices. All three accept INTSXP or REALSXP directly, avoiding the
  `storage.mode(m) <- "double"` copy that dominated light-tier query
  time on the 856 × 683 UMIs mmap matrix. Closes the 4 remaining
  bake-off breaches against DAF.jl (`julia_queries_026` Quantile,
  `_028` Mode, `_043` G2 Max, `_047` G3 Max); the remaining 4 accepted
  breaches are the mmap-query S7-ctor floor (deferred).

## Slice 9b — Perf parity with DAF.jl (2026-04-22)

### Performance

* **Dense grouped matrix reductions are now single-pass and int-aware.**
  The G2/G3 `rowsum`-based fast paths added in this slice now route
  through a new cpp11 kernel (`kernel_grouped_rowsum_dense_cpp`) that
  promotes `Int32` input to `double` during accumulation instead of
  materializing a full `double` copy first (`storage.mode(m) <- "double"`
  was costing ~1.5–2.5 ms per call on the 856 × 683 Int32 UMIs mmap
  matrix). For Var/Std/VarN/StdN, the same pass also accumulates sum-
  of-squares, eliminating the intermediate `m * m` allocation. 9 of the
  original 17 baseline breaches closed from this change alone; several
  queries now *beat* DAF.jl at single-thread.
* **Dense column Var/Std use `matrixStats::colVars` / `rowVars`.** The
  R-side two-pass formula `colMeans(m * m) - colMeans(m)^2` allocated a
  full copy of `m * m` before reducing; the one-pass two-moment
  algorithm in `matrixStats` is ~2.5× faster on the UMIs fixture and is
  now the default for the dense path of `Var`, `Std`, `VarN`, `StdN`.
* **G3 Sum-family uses a BLAS indicator matrix.** `t(rowsum(t(m), g))`
  has been replaced with `m %*% indicator` for G3 Sum/Mean/Var when the
  input is dense double; this hands the reduction to the underlying
  BLAS and cuts G3 kernel time by ~4.7× on the UMIs corpus.
* **FilesDaf descriptors and scalar-JSON files parsed by regex.** The
  three fixed-schema JSON files dafr emits (`daf.json`, scalar-value
  files, matrix/vector descriptors) are now parsed with `readChar` +
  regex, falling back to `jsonlite::fromJSON` on any non-matching
  content. Closed the `mmap_open_read_scalar` breach and shaved 125–160
  µs off every FilesDaf cold reopen.
* **GeoMean accumulator no longer wastes `std::log` calls.**
  `Acc::push` in `src/kernel_grouped_acc.h` previously computed
  `std::log(v + eps)` on every element for every op; now gated behind a
  `bool need_log` flag set to true only when the op is GeoMean. Pure
  waste removal — the existing Sum/Mean/Var/etc. paths never read
  `log_sum`.

### Tooling

* **Bake-off harness** at `benchmarks/` (package repo): reproducible R↔
  Julia performance comparison. Hand-authored 79-query set + 5 FilesDaf
  fixtures, SHA256-verified so the comparison can refuse to join runs
  against divergent corpora. R-side runner (`bench::mark`), Julia-side
  runner (`BenchmarkTools.jl`), both emit matching-schema CSVs consumed
  by `benchmarks/compare.R`, which produces a markdown breach report.
  See `benchmarks/README.md` to reproduce.

### Deferred

* `Quantile` and `Mode` dense-matrix reductions on `cells_daf`-scale
  inputs (julia-queries 26 and 28) still breach at ~2.3–3.0× and need
  new cpp11 dense-kernel analogs of the existing CSC quantile and mode
  kernels. Tracked for a follow-up slice.
* Five remaining baseline breaches (Max grouped on UMIs, and the mmap-
  reopen family on `vector`/`matrix`/`axis`) sit at the R per-call
  dispatch / S7-constructor floor. Further closure requires
  architectural work (query-parse caching or rewriting `files_daf` in
  C++) out of slice scope.
* `kernel_grouped_reduce_csc_cpp` axis=3 memory behaviour at 128 threads
  × 10 k nrow × 100 groups is unchanged; single-thread baseline did not
  exercise it. Profile-driven fix tracked for follow-up.

## CI stabilisation (post-Slice-9a)

### Bug fixes

* **`assert_no_densify_during` test helper**: replaced
  `assignInNamespace("as.matrix.Matrix", ...)` with direct
  namespace-binding + `.__S3MethodsTable__.` manipulation. The R 4.5
  `utils::assignInNamespace` S3-remap path touches
  `methods::slot(genfun, "default")@methods$ANY`, which fails with
  `"no slot of name 'methods' for this object of class
  'derivedDefaultMethod'"` against Matrix 1.7-4+. Tests passed locally
  against older Matrix but failed on all CI platforms (ubuntu, macos,
  windows).
* **`complete_daf` relative-path detection is Windows-safe**: the old
  test `startsWith(base, "/")` misclassified Windows absolute paths
  (`C:/...`, `C:\...`, UNC) as relative, producing nonsense
  `dirname(path) / abs_base` concatenations. Extracted to
  `.is_absolute_path()` handling Unix, drive-letter, and UNC forms.

## Slice 9a — Julia-parity correctness (2026-04-22)

### Breaking changes

* **Grouped-matrix operator semantics inverted to match DAF.jl.** The
  pairing between `GroupRowsBy` / `GroupColumnsBy` and `ReduceToRow` /
  `ReduceToColumn` has been swapped.

  | Pattern | Before | After |
  |---------|--------|-------|
  | G2 (matrix out, ngroups × ncol) | `-/ g >|` | `-/ g >-` |
  | G3 (matrix out, nrow × ngroups) | `\|/ g >-` | `\|/ g >\|` |
  | G4a (vector out, length ngroups) | `-/ g >-` | `-/ g >\|` |
  | G4b (vector out, length ngroups) | `\|/ g >\|` | `\|/ g >-` |

  G1 vector reduction (`/ g >|`) is unchanged.

### New features

* Julia G1 reduction syntax `>>` accepted as alias for `>|`.
* Convert op now accepts Julia type names: `Float32` / `Float64` →
  `double`; `Int32` → `integer`; `Int64` → `bit64::integer64`
  (dim/dimnames preserved on matrix input); `Bool` → `logical`.
  R-native names (`double`, `integer`, `logical`, `character`) continue
  to work.
* Julia-queries fixture extended with 23 new records covering G1 (`>>`),
  G2, G3, Convert-to-{Int32, Int64} (matrix + vector), and
  Mode-on-character. Byte-parity with DataAxesFormats.jl verified.
* `complete_daf()` correctly round-trips views with renamed axes
  (regression test added; was already supported, now guarded).

### Documented divergence

* `Convert type Bool` on a matrix behaves differently in DAF.jl (strict
  `InexactError` on values > 1) versus `dafr` (permissive; non-zero →
  TRUE via `as.logical`). This is an intrinsic language-level difference,
  not a bug. The fixture does not include a matrix Bool record.

### Internal

* Matrix comparison path in `tests/testthat/test-query-julia-compat.R`
  now handles `bit64::integer64` correctly. Previously `as.vector()` on
  an integer64 matrix stripped the S3 class and produced IEEE-754
  reinterpretation garbage; the fix casts both sides explicitly before
  comparison.

## Slice 8 — Matrix-kernel fast paths + complete_daf view re-apply

### Performance

* Sparse `dgCMatrix` reductions (Var, Std, VarN, StdN, Median, Quantile,
  GeoMean, Mode) now use custom CSC C++ kernels and no longer densify
  via `as.matrix()`. Min/Max on sparse input no longer densifies either.
* Grouped reductions (patterns G1–G4) use new CSC + dense C++ kernels
  instead of `split()` + `apply()`. Benchmark gates show 10–80× speedups
  at representative small-to-medium scale. Large-scale grouped
  reductions on high-thread-count machines may benefit from tuning
  `dafr.kernel_threshold` — see `?dafr-options`.

### New features

* `.op_convert` preserves sparsity for `dgCMatrix → integer` and
  `dgCMatrix → logical` conversions. Sparse → character still
  densifies (R has no character-valued sparse class).
* `Mode` reduction now accepts character input in grouped-vector
  reductions. Underlying grouped-reduction dispatch no longer hard-codes
  `vapply(..., numeric(1))` — output storage is inferred from the
  reducer's return value (supports integer, logical, character).
* `complete_daf()` re-applies `base_daf_view` JSON on reopen; previously
  the stored view spec was parsed but ignored. Supports identity views;
  axis-rename round-trip depends on `viewer()` rename syntax support.

### Fixes

* `.matrix_type_ok` now recognises `character` matrices and
  integer-/logical-valued `dgCMatrix` inputs for contract checking.
* Sparse Min/Max no longer silently densifies via
  `matrixStats::rowMaxs(as.matrix(m))` (pre-existing Slice-3 mine).
* `lgCMatrix` inputs route correctly to the slow path for ops whose
  kernels expect `dgCMatrix` (avoids `cpp11::doubles` type-mismatch
  crash).

### Known limitations

* **Grouped matrix reductions**: the R-side operator-to-reduce-kind
  mapping differs from Julia DAF's convention for grouped-matrix
  patterns. Byte-parity with DataAxesFormats.jl is not achieved for
  grouped queries. R-side semantics are internally self-consistent
  and tested.
* **Grouped G3 kernel at high thread counts**: thread-bucket layout is
  O(nthreads × nrow × ngroups). On many-core machines (>16 threads) at
  large scale (nrow > 10k with ngroups > 50), the fast path may be
  slower than the pure-R fallback due to bucket allocation cost.
  Set `options(dafr.kernel_threshold = Inf)` to force sequential
  execution if you observe this.

# dafr 0.6.0 (in development)

## New features

- **Query op surface expansion** — 12 new default ops registered at package
  load: eltwise `Clamp`, `Convert`, `Fraction`, `Significant`; reductions
  `Var`, `Std`, `VarN`, `StdN`, `Median`, `Quantile`, `GeoMean`, `Mode`.
  All available from query strings (e.g. `% Clamp min: 0 max: 10`,
  `>| Quantile p: 0.9`). Reductions use uncorrected (n-denom) variance to
  match DAF.jl. `Mode` is numeric-only this slice; string-axis grouping
  deferred. No new exports.
- **Copies surface** (`copy_scalar()` / `copy_axis()` / `copy_vector()` /
  `copy_matrix()` / `copy_tensor()` / `copy_all()`): port of Julia
  `Copies.jl`. Supports `rename` / `reaxis`, type coercion, `empty` fill
  for source-is-subset axes, `overwrite` and `insist` semantics matching
  Julia. Sparse matrix pad-mode preserves sparsity via
  `Matrix::sparseMatrix` embedding — fixes the Slice-5 dense-coercion
  mine. (#slice-6)
- `empty_data()` helper builds flat-keyed empty / types specs from a
  typed-list builder API. (#slice-6)
- `concatenate()`: stitches N dafs along one or more axes, creates a
  per-source `dataset` axis, supports prefix de-duplication with the
  "property-matches-any-concat-axis" heuristic (widened from the plan's
  original draft for Julia parity), fills missing per-source properties
  from an `empty` map, and applies per-property `merge` actions
  (`SkipProperty`, `LastValue`, `CollectAxis`). (#slice-6)
- `complete_chain()` / `complete_daf()` / `open_daf()`: persistent chain
  metadata. `complete_chain` stores a `base_daf_repository` pointer
  scalar; `complete_daf` walks the pointer chain back and returns a
  `chain_reader` or `chain_writer`. `open_daf` dispatches FilesDaf
  (directory) paths; H5df is deferred. (#slice-6)
- `reconstruct_axis()`: promotes an implicit property to an explicit
  axis, migrating consistently-mapped properties. Returns a per-property
  dict of values associated with empty-implicit entries. Core behaviors
  only; pre-existing target axis merge is Slice 7. (#slice-6)

## Refactor

- `adapter()` internal `.copy_view_to_daf()` removed; adapter now calls
  `copy_all()` with `insist = FALSE`. Same user-facing surface; sparse
  pad-mode is now sparse-preserving. (#slice-6)

# dafr 0.5.0 (in development)

## New features

- `computation(name, contract, fn)`: higher-order function that wraps
  `fn` with a contract. On each call, `contractor()` wraps the first
  argument, `verify_input()` runs before the body, `verify_output()`
  runs after, and the result is returned. Contract enforcement gates
  on `DAF_ENFORCE_CONTRACTS` / `options(dafr.enforce_contracts)` as in
  Slice 4. `function_contract(fn)` retrieves the bound contract;
  `contract_description(contract)` renders a contract as multi-line
  text for roxygen docstrings. Single-contract only this slice; dual-
  and triple-contract forms (Julia-UNTESTED upstream) are deferred.
  (#slice-5)
- `adapter(daf, fn, input_axes, input_data, output_axes, output_data,
  capture, empty, relayout, overwrite, name)`: apply a computation to
  a renaming view of `daf`, capturing the outputs in a fresh writable,
  and project them back under the caller's names. Mirrors Julia
  `adapter()`. The internal `.copy_view_to_daf()` supports pad-mode
  for subset-axis views via `empty` (`"axis|vector"` /
  `"rows|cols|matrix"` flat keys). (#slice-5)
- `example_cells_daf()` / `example_metacells_daf()` /
  `example_chain_daf()`: byte-parity versions of the Julia example
  datasets (856 cells × 683 genes; 7 metacells × 4 types; chained
  writer over both). Raw data files ship under
  `inst/extdata/example_data/`; loaders replicate the
  Bool → Int → Double → Character and UInt8 → UInt16 → Float32
  promotion lattices. (#slice-5)
- Multi-hop chained lookup: `@ cell : donor =@ : lab =@ : country`
  now resolves through an arbitrary number of hops. The evaluator
  stamps `$property` on each hop's return so the next `=@` can infer
  its target axis, and uses the carried `names(pivot_values)` so
  prior `??` row-drops survive subsequent hops. (#slice-5)

## Documentation

- `@examples` blocks added to all 60 exported functions (was: 1).
  Query DSL, view, chain, and contract examples use
  `example_cells_daf()` for realistic data; CRUD ops use compact
  inline memory_daf construction. `mmap_*` examples stay in
  `\donttest{}`. (#slice-5)

## Bug fixes

- `ViewDaf`'s `format_vectors_set` / `format_matrices_set` no longer
  crash with `startsWith(NULL, prefix)` when the view carries no
  vectors / matrices (latent Slice-3 bug, unearthed by Phase B's
  minimal test fixtures). (#slice-5)

# dafr 0.4.0 (in development)

## New features

- `chain_reader()` / `chain_writer()`: federate an ordered list of
  `DafReader`s into a single read-only (`ReadOnlyChainDaf`) or
  read-write (`WriteChainDaf`) view. Later entries override earlier
  entries on read; writes go to the final writer; deletes only succeed
  when the entry exists solely in the top writer. Axis consistency
  across overlapping axes is validated at construction. (#slice-4)
- `Contract()`, `contractor()`, `verify_input()`, `verify_output()`:
  typed pre/post-condition enforcement for computations consuming Daf
  data. Guards required / optional / created / guaranteed / optional
  outputs, tracks access to required inputs, and validates element
  types. Enforcement is off by default; enable via
  `DAF_ENFORCE_CONTRACTS=1` (env) or
  `options(dafr.enforce_contracts = TRUE)`. (#slice-4)
- `merge_contracts()`: combine two contracts (Julia's `|>` semantics),
  resolving expectations and element types. (#slice-4)
- `IfNot` / `AsAxis` evaluator semantics: single-hop chained lookup
  `@ A : v ?? X =@ : w` is now evaluated end-to-end. `??` bare drops
  empty-value entries; `?? X` substitutes `X`. (#slice-4)
- `ViewDaf`: axis rename (`viewer(d, axes = list(list("obs", "@ cell")))`)
  and axis filter (`viewer(d, axes = list(list("cell", "@ cell [ keep ]")))`)
  now propagate to `get_vector()` / `get_matrix()` reads. (#slice-4)

## Performance

- `% Log eps: 1` on a `dgCMatrix` now preserves sparsity (in-place
  `log1p` on the `@x` slot). Eliminates the multi-GB dense
  intermediate the previous path produced for typical UMI matrices.
  (#slice-4-P2)
- Bare default reductions (`>| Sum`, `>- Mean`, `>| Max`, etc., with
  no parameters) now route to `rowSums` / `Matrix::rowSums` /
  `matrixStats::rowMaxs` instead of `apply()`. (#slice-4-P3)
- `% Log eps: ε >| Sum` and the `Mean` / `>-` variants now dispatch
  to a fused C++ kernel (`kernel_log_reduce_{dense,csc}_cpp`); the
  CSC variant is single-pass over `nnz` with no dense intermediate.
  (#slice-4-P4)
- `dafr.omp_threshold` (declared in Slice 0 but orphaned) is now
  threaded through `kernel_log_add_cpp` and `kernel_csc_colsums_cpp`,
  and through the new fused log-reduce kernel. Defaults to 10000.
  (#slice-4-P1)

## Bug fixes

- `@ A [ v cmp X ]`: NA values in the masked property no longer leak
  into the result; they are dropped silently, matching Julia's boolean
  indexing semantics. (#slice-4)

## Internal changes

- `ViewDaf` now reuses the base daf's cache environment (the
  previously allocated but unused per-view `query` cache bucket was
  removed). (#slice-4)
- New Imports: `matrixStats` (used by Phase P3 fast path for
  `rowMaxs` / `colMins` etc.).

# dafr 0.3.0 (Slice 3)

## New features

- **Query DSL** (`parse_query()`, `get_query()`): text-based query
  language over any `DafReader`. Supports axis lookups, vector/matrix
  lookups, bracketed masks with comparators (`< <= = != > >= ~ !~`),
  logical combinators (`& | ^` with negation), square slicing (`@- @|`),
  `GroupBy` / `CountBy`, reductions (`>- >|`), and eltwise operations
  (`%`). See `?parse_query`.
- **Frames** (`get_frame()`): extract a `data.frame` of vectors along an
  axis query.
- **Views** (`ViewDaf`, `viewer()`): lazy read-only wrapper that exposes
  a renamed / filtered view of a base daf via query rewrites. No copies.
  Supports `ALL_AXES` / `ALL_SCALARS` / `ALL_VECTORS` / `ALL_MATRICES`
  wildcards with last-wins override semantics.
- **Operations registry** (`register_reduction()`, `register_eltwise()`):
  pluggable op table. Defaults shipped: `Sum`, `Mean`, `Max`, `Min`,
  `Count`, `Log`, `Abs`, `Exp`, `Sqrt`, `Round`.
- **Query cache tier** now populated (previously reserved). Entries keyed
  by canonical query string via `cache_lookup` / `cache_store`; invalidated
  on version-counter bumps.

## Compatibility

- Query strings parse and evaluate against Julia-produced FilesDaf stores;
  round-trip tested via a Julia-generated fixture (`example_cells_daf()`).
- 17/17 fixture query strings parse and evaluate to matching numeric/character
  values (values compared with tolerance; `>|` / `>-` reduction axis semantics
  corrected to match Julia in this release).

## Known limitations (deferred to Slice 4)

- **ViewDaf axis rename** does not propagate to vector / matrix reads.
  `viewer(d, axes = list(list("obs", "@ cell")))` renames the axis
  but `get_vector(v, "obs", ...)` does not resolve. Workaround: keep the
  original axis name in the view.
- **ViewDaf axis filter** does not propagate to vector / matrix reads.
  `viewer(d, axes = list(list("cell", "@ cell [ keep ]")))` exposes the
  filtered entries via `axis_vector()`, but `get_vector(v, "cell", ...)`
  returns the full base vector. Workaround: filter vectors explicitly via
  a query override in `data`.
- **`IfNot` and `AsAxis`** query modifiers parse successfully but are
  evaluated as no-ops in Slice 3 (Slice 4 lands the real semantics).
- **Performance**: reductions and eltwise ops use `apply()`; for large
  matrices (millions of elements) this is several times slower than a
  vectorized `colSums` / `rowMeans` path. A vectorized default-op path
  is planned.
- **Chains and Contracts** modules are not yet ported (scheduled for
  Slice 4).
