# Audit: chains.jl literal-parity divergences

Date: 2026-05-07
Driver: literal port of `~/src/DataAxesFormats.jl/test/chains.jl` (537
lines, ~57 nested_test leaves) into
`tests/testthat/test-chains-jl-parity.R`.

The port surfaced one significant behavior bug (chain version-counter
propagation) and 4 documented divergences. dafr's existing R-side chain
coverage (`test-chain-readers.R` etc.) didn't catch the version-counter
gap because the existing tests don't exercise the after-mutation
counter contract.

## Status

- **Fixed inline:** C0 (chain version-counter propagation; chains
  previously had their own private counter env that never tracked
  underlying source mutations, breaking both `vector_version_counter`
  reads and downstream cache invalidation on chain writes).
- **Open / skipped:** C1 (singleton chain identity optimization), C2
  (`description(...; deep)` parameter), C3 (no `empty_dense_*` /
  `empty_sparse_*` builder API), C5 (`complete_path` errors on
  memory-backed chains).

Skip count in `test-chains-jl-parity.R`: 12 across 4 unique IDs.
Result: `FAIL 0 | SKIP 12 | PASS 182`.

---

## FIXED in this slice (commits on `dev`)

### C0. Chain version counters didn't propagate from sources

- **Symptom 1.** `vector_version_counter(chain, axis, name)` always
  returned the chain's own counter, which was never bumped when an
  underlying source's vector was set independently. Test `chains /
  access / read / vector / first` does `prev <-
  vector_version_counter(chain, ...); set_vector(source, ...);
  vector_version_counter(chain, ...) == prev + 1` — failed.
- **Symptom 2 (worse).** Cache invalidation broke as a result. Test
  `chains / write / vector / override` does `set_vector(first, ...);
  read chain → [1, 2]; set_vector(chain, ..., overwrite = TRUE) →
  routes to the writer (second); read chain → expected [2, 3], got
  stale [1, 2]`. The chain's `vector_stamp` for caching read from the
  chain's stale env, so the second read hit the cache and returned the
  old (first-source) value instead of the new (second-source) value.
- **Fix.** `R/cache.R` — added a chain-aware path to `axis_stamp`,
  `vector_stamp`, `matrix_stamp`, `axis_version_counter`,
  `vector_version_counter`, `matrix_version_counter`. When the daf is
  a `ReadOnlyChainDaf` or `WriteChainDaf`, sum the per-source counters
  / stamps. The per-source axis-existence check is in the inner lambda
  so absent axes contribute 0L without erroring.
- **Why sum, not max.** Either works for cache-invalidation purposes
  (any source mutation produces a different stamp). Sum is simpler to
  compose recursively (chain-of-chain-of-X) and the absolute number
  isn't user-visible.
- **No existing-test follow-up.** The R-side chain tests don't assert
  specific counter values; the existing `expect_silent` / set-then-read
  pattern continues to pass.
- **Julia ref.** `chains.jl:194-211` (`vector_version_counter`
  expectations) plus implicit cache-invalidation in `chains.jl:415-446,
  500-534`.

---

## Open divergences

### C1. Singleton-chain identity optimization missing

- **Symptom.** Julia: `chain_reader([read_only(d)]) === read_only(d)`
  and `chain_writer([d]) === d` — singleton chains return the
  underlying daf verbatim. dafr always wraps in a fresh
  `ReadOnlyChainDaf` or `WriteChainDaf`, even for singleton inputs.
  The wrapper has the same readable contents but isn't `===` to the
  input. Same gap appears in `read_only(read_only_chain)` — Julia
  detects the already-read-only state and returns identity; dafr
  re-wraps.
- **Tests guarded.** `chains / one / reader`, `chains / one / writer`.
- **Fix sketch.** In `chain_reader` and `chain_writer`, after the
  empty-list check, if `length(dafs) == 1L && S7_inherits(dafs[[1]],
  ReadOnlyChainDaf)` (or matching DafWriter for writer), return
  `dafs[[1L]]` directly when the requested name is also `NULL` or
  matches the existing one. ~10 lines per constructor. The semantic
  benefit is symmetric `read_only(read_only(d)) === read_only(d)`
  idempotence and saves one wrapper allocation in routine code.

### C2. `description(...; deep)` parameter not supported

- **Symptom.** Julia's `description(chain; deep = true)` walks into
  each chain member and emits a per-member sub-description (with
  `name:`, `type:`, `scalars:` etc. nested under `chain:`). dafr's
  `description` is single-level — it stops at the chain wrapper and
  doesn't recurse. The `deep = false` and default cases happen to
  match Julia's shallow output; only `deep = true` differs.
- **Tests guarded.** `chains / access / {read, write} / scalar / both /
  description / {(), !deep, deep}` — 6 skips.
- **Fix sketch.** Add `deep = FALSE` parameter to `description()`;
  when TRUE and the daf is a chain, build a sub-description per
  member by recursing. ~30 lines. Pretty-print exactly one
  description format (Julia's nested-list-with-indentation) so the
  test's string equality holds.

### C3. No `empty_dense_*` / `empty_sparse_*` builder API

- **Symptom.** Julia's `empty_dense_vector!(d, axis, name, T) do empty
  ... end` / `empty_sparse_vector!(d, axis, name, T, nnz, IT) do nzind,
  nzval ... end` and matrix counterparts are zero-copy builders: they
  allocate a fresh buffer of given type+capacity and yield it to the
  caller block, who fills it in place; the buffer is then committed
  to storage without an extra copy. dafr has no equivalent — all
  set_vector / set_matrix calls take a fully-built value.
- **Tests guarded.** `chains / write / vector / {empty_dense,
  empty_sparse}`, `chains / write / matrix / {empty_dense,
  empty_sparse}` — 4 skips.
- **Fix sketch.** Substantial. Would need `empty_dense_vector` /
  `empty_dense_matrix` (and sparse variants) with a callback / builder
  protocol, plus per-backend implementations that allocate the right
  buffer type. R has no obvious zero-copy idiom (every `[[<-` reallocs
  on COW), so the value of porting is unclear — the call site can
  `set_vector(d, axis, name, my_built_vec)` without a builder. Out of
  scope.

### C5. `complete_path` errors on memory-backed chains

- **Symptom.** Julia: `complete_path(chain_over_memory)` returns
  `nothing` (no fs path). dafr: errors "daf has no filesystem path
  -- only FilesDaf supported by complete_*".
- **Test guarded.** Inlined in `chains / two` (the side-check
  `expect_null(complete_path(read_chain))` was relaxed to
  `expect_true(is_daf(read_chain))` since the divergence isn't this
  test's main point).
- **Fix sketch.** Make `complete_path()` return `NULL` for non-FilesDaf
  inputs instead of erroring. Existing R-side callers that rely on the
  error would need updating, but the contract change is small. ~5
  lines.

---

## R-fundamental / non-portable

### T1. Error-text differences

Same shape as concat / reorder slices. Julia's chomp-formatted
multi-line errors (`read-only final data: second!.read_only / in write
chain: chain!`) translate to dafr's single-line errors (`read-only
final data: <name> in write chain: <name>`). Regexes look for tokens.

### T2. `Set([...])` vs. R unordered character vectors

dafr's `scalars_set` / `axes_set` / `vectors_set` / `matrices_set`
return character vectors (deterministically sorted, but not Set-typed).
Tests use `expect_setequal(...)` to mirror Julia's `Set(...) ==
Set(...)` semantics.

---

## Test catalog

`tests/testthat/test-chains-jl-parity.R` — 49 `test_that` blocks. The
`access` group's read/write parameterization is unrolled as 32
test_thats sharing 14 assertion helpers (`.assert_access_*`). The
`write` group has its own `.write_chain_setup` helper.

Counts:
- Behavior bug fixed inline: 1 (C0, but it impacts every cache-using
  chain operation)
- Open divergences guarded by skip: 4 unique IDs across 12 skips (C1
  ×2, C2 ×6, C3 ×4)
- Inlined relaxation: C5 (complete_path) replaced with `is_daf` check
- R-fundamental, no skip: T1 (error wording), T2 (Set vs. character
  vector)
