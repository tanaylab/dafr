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
