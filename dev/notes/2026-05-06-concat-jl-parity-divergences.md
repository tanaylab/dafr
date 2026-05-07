# Audit: concat.jl literal-parity divergences

Date: 2026-05-06
Driver: literal port of `~/src/DataAxesFormats.jl/test/concat.jl` (474 lines,
~42 nested_test leaves) into `tests/testthat/test-concat-jl-parity.R`.

The port surfaced two real behavior bugs (fixed inline this slice) and five
divergences that warrant either documentation, follow-up, or a future
feature lift. This document is the punch list.

## Status

- **Fixed inline:** B1, B2 (concat behavior bugs).
- **Open:** M1 (wildcard merge keys), M2 (sparse preservation in vector
  collect-axis), M4 (prefixed-as-override), M5 (memory_daf
  sparseVector rejection).
- **R-fundamental:** T1 (no UInt8/UInt16 type assertions in R).

Skip count in `test-concat-jl-parity.R`: 6, all attributed to M1/M2/M4/M5.
Result line: `FAIL 0 | SKIP 6 | PASS 64`.

---

## FIXED in this slice (commits on `dev`)

### B1. `MERGE_COLLECT_AXIS` for a vector ignored `empty` and leaked NA columns

- **Symptom.** `concatenate(dest, "cell", sources, merge =
  list("gene|weight" = MERGE_COLLECT_AXIS), empty = list("gene|weight" =
  0))` returned a (gene × dataset) matrix where the column for any source
  missing the vector was filled with NA, regardless of `empty`. Julia's
  literal test asserts the column is filled with the empty value.
- **Fix.** `R/concat.R` — plumb `empty` from `concatenate()` through
  `.concat_merge()` into `.concat_merge_vector()`. The COLLECT_AXIS branch
  now consults `empty[[axis|name]]`: present source → real values; missing
  source with empty fill → fill column; missing source without fill →
  raise the same "no empty value" error as the per-axis vector path.
  Prototype value also bootstrapped from the first present source's
  `v[NA_integer_]` so column types cohere.
- **Julia ref.** `concat.jl:344-381`.

### B2. `MERGE_LAST_VALUE` for a 3-part matrix key was a silent no-op

- **Symptom.** `concatenate(dest, "cell", sources, merge =
  list("gene|gene|outgoing_edges" = MERGE_LAST_VALUE))` did nothing —
  the matrix was neither copied from the last source nor stamped on the
  destination. Julia's test asserts the destination holds the last
  source's matrix verbatim.
- **Fix.** `R/concat.R` — added `.concat_merge_matrix()` and routed the
  3-part-key + LASTVALUE case to it. Iterates sources in reverse, finds
  the last one holding the matrix, calls `format_set_matrix` on the
  destination. The COLLECT_AXIS branch (the only previously-handled
  3-part case, which errors) is unchanged.
- **Julia ref.** `concat.jl:415-431, 445-460`.

---

## Open divergences

### M1. `concatenate(merge = ...)` does not honor `ALL_SCALARS / ALL_VECTORS / ALL_MATRICES` wildcards

- **Symptom.** Julia's tests use `merge = [ALL_VECTORS => CollectAxis]`
  to mean "for every vector property not on a concat axis, do
  CollectAxis". dafr's `.concat_merge()` iterates `names(merge)` and
  treats the entries as literal `"axis|name"` keys. The constants
  `ALL_SCALARS = "*"`, `ALL_VECTORS = c("*","*")`, `ALL_MATRICES =
  c("*","*","*")` exist for `viewer()` but are not expanded by
  `concatenate()`.
- **Tests guarded.** `concat / merge / scalar / collect`,
  `concat / merge / scalar / !collect`,
  `concat / merge / vector / collect / sparse`. The other wildcard
  cases were translated to explicit `axis|name` keys (which exercise
  the same dispatch path) and pass.
- **Fix sketch.** `.concat_merge()` could expand wildcard keys: when the
  key parses to `c("*")` / `c("*","*")` / `c("*","*","*")`, enumerate the
  union of scalars / (axis × vector) pairs / (axis × axis × matrix)
  triples across all sources and dispatch each. Skip keys whose first
  axis is a concat axis (already filtered for the literal case at
  `concat.R:309`). Roughly a 30-line addition.
- **Julia ref.** `concat.jl:292-306, 332, 346-392, 420, 434, 453, 469`.

### M2. Vector COLLECT_AXIS path always allocates a dense matrix

- **Symptom.** Julia's `concat / merge / vector / collect / sparse`
  asserts that two sparse-input vectors collected along the dataset axis
  produce a `SparseMatrixCSC` destination. dafr's `.concat_merge_vector`
  always calls `matrix(proto, nrow, ncol, ...)` (now via the B1 fix —
  before the fix it was `matrix(NA, ...)`) which is dense regardless of
  source sparsity.
- **Test guarded.** `concat / merge / vector / collect / sparse`.
- **Fix sketch.** Detect when all present sources have sparse-typed
  vectors (`Matrix::sparseVector` for files_daf; memory_daf doesn't accept
  sparseVector at all — see M5), allocate a `dgCMatrix` instead, fill via
  column assignment, and skip the dense `out[, i] <- ...` path. Probably
  pairs naturally with M5 because the same question (does the backend
  accept sparseVector?) drives both.

### M4. `prefixed=` is gated by per-axis `prefix` flag, Julia treats it as override

- **Symptom.** Julia: `concatenate!(d, ["cell","metacell"], sources;
  prefix = [false, true], prefixed = [Set(["metacell","!metacell"]),
  Set{String}()])` prefixes both `cell|metacell` and `cell|!metacell`
  with the source-dataset name even though `prefix[cell] == false`,
  because `prefixed[1]` lists them. dafr's `.concat_axis_vector` gates
  the explicit-prefixed branch on `isTRUE(do_prefix) && name %in% vec`,
  so when the cell axis itself isn't prefixed nothing on the cell axis
  gets prefixed regardless of `prefixed[[cell]]`.
- **Test guarded.** `concat / prefix / prefixes`.
- **Behavioral question.** dafr's docstring says `prefixed` provides
  "additional property names to prefix, beyond the heuristic" — which
  reads as "augments the heuristic when it would already fire", i.e.
  the current behavior. Julia's interpretation is "explicit override
  list, fires regardless". The two readings are both internally
  consistent; the divergence is over which one is the contract. Pin
  the user-facing semantics in a follow-up before changing the gate.

### M5. `memory_daf` rejects `Matrix::sparseVector` (atomic-only)

- **Symptom.** `set_vector(memory_daf(...), axis, name, sv)` where
  `sv <- Matrix::sparseVector(...)` errors `vector 'name' on axis 'axis'
  must be atomic`. The same call on `files_daf(...)` works (and the
  files backend writes sparse format unconditionally — see
  `tests/testthat/test-files-vectors.R:333`).
- **Test guarded.** `concat / sparse / vector / dense`,
  `concat / sparse / vector / sparse`.
- **Site.** `R/utils.R:55-60` (`.validate_vector_value`) and the
  `format_set_vector` method on `MemoryDaf` (`R/memory_daf.R`) which uses
  it.
- **Fix sketch.** Either (a) coerce `Matrix::sparseVector` to the canonical
  in-memory representation `MemoryDaf` uses for sparse storage (which is
  currently nothing — memory_daf stores everything dense), or (b) lift
  `.validate_vector_value` to accept sparseVector and densify on the
  spot. Pairs with M2 if we want sparse-preserving collect-axis to work
  on memory_daf inputs.

---

## R-fundamental divergences (NOT bugs; documented for completeness)

### T1. R has no UInt8 / UInt16 / Int32 distinction

- Julia tests assert `eltype(get_matrix(...)) == UInt16` after merging
  UInt8-on-source.1 + UInt16-on-source.2 vectors. R has only `integer`
  (32-bit signed) and `double` (Float64). The literal port elides the
  `eltype` assertion and asserts only the values, since R tests can't
  distinguish UInt8 from UInt16. Same for the Int32 vs Int8/16 cases.
- **Tests affected.** `concat / merge / vector / collect / dense / *` —
  all three pass on values, neither port checks element type.

### T2. Error-text differences

R's `concatenate()` raises single-line errors with R-flavored wording
("`'<arg>' must be ...`", "`duplicate entries on axis ...`"); Julia's
errors are multi-line and chomp-formatted ("can't concatenate the
matrix: outgoing_edges / for the concatenated rows axis: cell / ..."
etc.). Where the parity test asserts an error, the regex looks for
distinctive identifying tokens (vector name, axis name, "no empty
value", "duplicate entries", "CollectAxis", etc.) rather than exact
wording. Not enumerated here per-test — the regexes inline in
`test-concat-jl-parity.R` document each one.

---

## Test catalog

`tests/testthat/test-concat-jl-parity.R` — 33 `test_that` blocks; one
per `nested_test` leaf in `concat.jl`. Helpers `.fresh()`,
`.prefix_setup()`, `.sparse_matrix_setup()`, `.merge_scalar_setup()`,
`.merge_vector_setup()`, `.merge_matrix_square_setup()`,
`.merge_matrix_rect_setup()` mirror Julia's nested_test parent setups
(each leaf rebuilds state from scratch).

Counts:
- Behavior bugs fixed inline: 2 (B1, B2)
- Open divergences guarded by skip: 4 unique IDs across 6 skips (M1×2,
  M2, M4, M5×2)
- R-fundamental, no skip: T1 (UInt8/UInt16), T2 (error wording)
