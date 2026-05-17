# Next audit: cross-backend write+read parity

Kickoff for the audit after Round 6. The query-DSL sweep is now
saturated (~1.1% divergence rate, central validator caught the
systematic gaps). The biggest remaining unswept surface is the
data-storage layer: writes, format backends, and round-trips.

## Why this and not "Round 7 of query fuzzing"

Round 6 plateaued at 1-3% real divergences. The remaining
divergences are case-by-case semantic edges (Mode tie-break, mask
precedence, Float32 type preservation in result types), not
systematic gaps. Pattern-based refactoring won't close them; each
needs a one-off decision.

Outside the query DSL, **no adversarial sweep has ever run**. The
formats `MemoryDaf` / `FilesDaf` / `ZarrDaf` / `H5adAsDaf` / `HttpDaf`
each implement the same `format_*` generics, but no test in the
suite confirms they agree byte-for-byte on:

- Writing a Float32 vector → reading it back → comparing storage
  dtype, value, names
- Writing a sparse matrix → reading it back through a different
  backend → comparing CSC structure
- Round-trip via `copy_all`, `concat`, `chain_writer`, `view_daf`
- Setting + immediately querying (cache invalidation correctness)

This audit will find Float32-preservation bugs, integer-width
promotion bugs, dimname-handling regressions, and AltRep boundary
issues that the query-DSL fuzzer cannot reach because it only reads.

## Scope

**In scope:**
- All `format_set_*` / `format_get_*` generics on each backend
- Round-trip: set → get on the same daf, compared against the
  original input
- Cross-backend round-trip: write to backend A, copy to backend B,
  read from B, diff against original
- Dtype preservation: Float32 / Float64 / Int8 / Int16 / Int32 /
  Int64 / UInt8 / UInt16 / UInt32 / UInt64 / Bool / String
- Sparse matrix preservation: dgCMatrix → reload → dgCMatrix
- Empty axes / empty vectors / empty matrices
- Names attribute preservation
- Edge values: NaN, Inf, -Inf, very large / very small floats,
  integer-edge values (Int8 min/max, UInt32 max, etc.)
- `copy_all`, `concat_axis`, `view_daf` round-trips
- Re-opening a daf after closing it (FilesDaf / ZarrDaf)
- `set_*` followed immediately by `get_*` (cache correctness)

**Out of scope (for this audit):**
- Query DSL parity (already covered by Rounds 1-6)
- Performance / kernel benchmarks
- Threading / concurrency
- HTTP backend on a real remote server (`HttpDaf` against `nginx`)
  - use the local mock instead

## Methodology

Mirror the Round 6 structure:

1. **Fixture generator** (`dev/backend-parity/build_fixture_R.R`):
   create a reference daf in `MemoryDaf` with every dtype, every
   value-edge, every shape (empty axis, 1-elem axis, 1-elem vector,
   sparse-only matrix, dense-only matrix, square cell×cell, mixed-
   sparsity). Reference dtype map: keep an R-side manifest with
   `(axis, prop, expected_dtype, expected_shape)`.
2. **Backend round-trip runner** (`dev/backend-parity/round_trip.R`):
   for each backend in `{memory, files, zarr, h5ad, http_mock}`:
   a. Write the fixture
   b. Re-open
   c. Read every scalar / vector / matrix
   d. Emit a JSONL line per item: `{backend, axis, prop, kind,
      dtype, shape, names_hash, value_hash, error}`
3. **Cross-format runner** (`dev/backend-parity/cross_format.R`):
   for each `(src_backend, dst_backend)` pair, write to src,
   `copy_all` to dst, read from dst, diff against the original
   value/dtype.
4. **Diff tool** (`dev/backend-parity/diff.py`): take two backends
   from a single JSONL file (the same fixture across formats),
   bucket divergences by `(kind, dtype, axis-presence,
   sparse-vs-dense)`. Aligns with Round 6's triage idiom.

## Concrete first week

Day 1-2 — fixture:

- Pick every cell of the dtype × shape grid that has a non-trivial
  representation choice. ~50 vectors and ~20 matrices is enough.
- Include `NaN`, `Inf`, `-Inf`, integer-type extremes, empty axis,
  empty vector, all-zero sparse, alternating-pattern sparse,
  cell-cell square, named matrices with non-ASCII names.
- Build via `MemoryDaf` (no on-disk format yet); R-side source of
  truth.

Day 3 — single-backend round-trip:

- Loop over backends in `{memory, files, zarr}` (skip h5ad / http
  for now — they have more setup).
- For each: write fixture → close → reopen → read back → diff
  against original.
- Expected output: a `single_backend.jsonl` with `match` /
  `dtype_drift` / `shape_drift` / `names_drift` / `value_drift`.

Day 4 — cross-backend:

- For each `(A, B)` pair from above, write to A, copy_all to B,
  read from B, diff against original.
- Expected output: a `cross_backend.jsonl`.

Day 5 — triage and fix the systematic ones:

- Look at the bucket-by-class summary. Anything that appears 5+
  times is systematic and worth fixing.
- One-offs (1-2 occurrences) go into a "punt list" - probably
  edge-case dtype mismatches that are intentional.

## Success criteria

- **Day-5 stopping condition:** every (vector, matrix) round-trips
  on memory / files / zarr with matching dtype + value + names.
- **Stretch:** every cross-backend pair round-trips, with a
  documented list of known dtype downgrades (e.g. h5ad has no
  Int8 → falls back to Int16).
- **Out-of-scope deferred:** AltRep + mmap interactions, HTTP
  round-trips with caching, anndata X-vs-layers asymmetries.

## Known traps from Round 6

These caught me by surprise; flagging so the next pass doesn't
re-discover them:

1. **R has no Float32 storage.** Every Float32 round-trip in R is
   actually Float64 with extra precision noise. The harness must
   compare with a tolerance, OR round-trip through Float32 bytes
   (writeBin/readBin size=4) before comparing. See R/operations.R
   `.check_inexact_int` for the pattern.
2. **bit64::integer64 silently coerces** when compared to a non-int
   double. The fixture must distinguish `as.integer64(5)` from
   `5.0` for any equality check.
3. **dgCMatrix sparse → reload → dgCMatrix** can lose the exact
   zero pattern (drop0 vs not). Compare by (i, p, x) triples, not
   by `==` on the materialised matrix.
4. **factor vs character.** Some backends preserve factor levels,
   others coerce to character. Decide policy and document.
5. **jsonlite auto_unbox=TRUE** turns a length-1 vector into a
   scalar in JSON, which the diff tool then mis-categorises. Use
   `auto_unbox = FALSE` for harness output, or normalise in the
   diff tool (we did the latter in `fuzz/triage.py`).
6. **Julia 1.12 vs 1.11.** The DataAxesFormats.jl manifest is
   pinned to 1.12. `juliaup` default is 1.11. Use
   `/home/aviezerl/tools/.julia/juliaup/julia-1.12.5+0.x64.linux.gnu/bin/julia`
   explicitly until reconfigured.

## Reuse from Round 6

- `fuzz/triage.py` bucketing pattern translates directly - take the
  per-record class key from `(kind, dtype, shape-class)` and group.
- `.canonicalize_julia_type` / `.OP_META` style of central
  validation is the right shape for any cross-cutting check.
- `FINDINGS.md` template (round header → bug-class → fix-location)
  is the same; add Round-7 section to the same doc.
- `R/op_dispatch.R` shows the "one front-door, per-feature meta
  table" pattern. The format layer should grow an equivalent.

## What this audit will NOT close

- AltRep boundary correctness under garbage collection
- Real HTTP server behavior (mock-only here)
- Multi-process / concurrent write-while-read scenarios
- Reorder under partial-write crash (filed as R4-recovery)
- The standing 1 pre-existing test fail in
  `test-operations-registry.R:117` (Round digits=1 default-Int64)

## Estimate

~1 person-week to get through Day-1-5 above with the systematic
bugs fixed. Diminishing returns kick in after Day-5; further
investment pays off only if a specific bug class shows up
repeatedly in real-world dafr use.
