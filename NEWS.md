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
