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
- 15/17 fixture query strings pass byte-for-byte parity; 2 queries involving
  `>|` / `>-` on matrices where the stored orientation differs from the
  declared view axes are under investigation (known divergence).
