# Slice 9d-N — Kickoff breadcrumb

**Date:** 2026-04-22
**Scope candidate:** extend the row-partition technique from 9d-M to
six other CSC kernels that still carry `O(nthreads × nrow)` thread-
bucket memory patterns or that are currently serial because of write-
race concerns the row-partition transform eliminates.
**Predecessor:** Slice 9d-M (tag `slice-9d-m` on `main` at merge
commit `676386e`), exit at `dev/notes/slice-9d-m-exit.md`.

## What changed between Slice 9d-M exit and now

Nothing in the package repo — 9d-N starts from a clean `main` at
`676386e`. Slice 9d-M shipped row-partition for the G3 (axis = 3)
branch of three grouped CSC kernels and demonstrated the technique:
peak RSS at 128 threads dropped 7 GB on the stress fixture, and wall-
time improved 29–103× because the serial merge phase went away.

## Motivation

The same anti-pattern the 9d-M fix targets — thread-local buckets
sized `O(nthreads × something)` that must be merged serially — exists
in several other kernels. At the metacell workload scale
(`nrow ≈ 10⁶`, 128 threads) these buckets carry ~1.5–2 GB each. Left
unchanged, they stack to ~6 GB of parallel-dispatch memory overhead
across the non-grouped CSC reduction family, plus wall-time they
shouldn't cost.

A second group of kernels is currently **single-threaded only** with
a comment explicitly acknowledging that parallelism was forgone
because of write-race concerns on `rows[pi[k]]` — which is exactly
the pattern row-partition eliminates. Fixing these turns "serial" into
"parallel" with no memory penalty and no merge phase.

## Candidate kernels

### A. Thread-bucket pattern (`nthreads × nrow` allocations, ready for row-partition)

| Source file | Line range | Bucket payload | Memory at 128 threads × 1 M nrow |
|---|---|---|---:|
| `src/kernel_var_csc.cpp` | `59-61` | `tsx` + `tsxx` (2 × double) | ~2.0 GB |
| `src/kernel_minmax_csc.cpp` | `57-61` | `tbuf` (double) + `tnnz` (int) | ~1.5 GB |
| `src/kernel_log_reduce.cpp` | `90-95` | `tsum` (double) + `tnnz` (int) | ~1.5 GB |
| `src/kernel_geomean_csc.cpp` | `80-86` | `tsum` (double) + `tnnz` (int) | ~1.5 GB |

All four currently parallelize over the column axis, push via
thread-local buckets keyed by `pi[k]`, then serially merge into a
single `nrow`-sized vector before reducing. The row-partition
transform is directly applicable: each thread owns `[r0, r1)`, scans
all columns, filters `if (pi[k] < r0 || pi[k] >= r1) continue`, and
writes directly into a single shared `nrow`-sized accumulator. No
thread buckets, no merge.

**Expected wins (at metacell scale, 128 threads):**
- Combined peak-RSS reduction: ~6 GB.
- Wall-time improvement: analogous to 9d-M — the serial merge phase
  is likely the dominant cost at high thread counts for these kernels
  too (ratio will vary since they don't have the `× ngroups` factor
  9d-M had, but the qualitative shape is the same).

### B. Currently-serial kernels (row-partition enables new parallelism)

| Source file | Line range | Why currently serial |
|---|---|---|
| `src/kernel_mode_csc.cpp` | `118-123` (axis = 0 fill) | Comment-free but writes to `rows[pi[k]]` would race across threads if the outer column loop were parallelised |
| `src/kernel_quantile_csc.cpp` | `118-125` (axis = 0 fill) | Explicit comment: *"No parallelism here; writes to `rows[pi[k]]` would race across threads."* |

Both kernels currently do a serial fill pass, then a parallel
post-process. Row-partition turns the fill pass parallel too — each
thread owns `[r0, r1)`, writes only to its rows. The post-process is
already parallel.

**Expected wins:**
- Pure wall-time win: the serial fill pass becomes parallel. On CSC
  reductions with `nnz ≈ 10⁷` the fill pass can be measurable
  (~20–50 ms serial, a fraction of that parallel).
- No memory delta — `rows` was already a single `nrow`-sized vector,
  not a thread bucket.

### C. Other `std::vector<std::vector<…>>` references worth a look

Grep turned up three other matches that are **already fine** (listed so
9d-N doesn't accidentally touch them):

- `kernel_grouped_mode_csc.cpp:128` — G2 `rows_in_group[g]`,
  precompute, serial, read-only in parallel region. Fine.
- `kernel_grouped_mode_csc.cpp:161` + `171` — row-partition accs
  already installed in 9d-M.
- `kernel_grouped_quantile_csc.cpp:74`, `118`, `137` — G2 per-column
  local vectors are not thread buckets; G3 is already row-partitioned.

## Prerequisite — dispatch audit

Before touching the non-grouped CSC kernels (category A), confirm
they are actually on the hot path for real queries. 9b shipped
several dense/BLAS fast paths (`matrixStats::colVars`,
`m %*% indicator`, single-pass `kernel_grouped_rowsum_dense_cpp`)
that may have pushed the CSC variants off the common dispatch. A
quick audit of `R/query_eval.R` for:

```r
grep -nE "kernel_(var|minmax|geomean|log_reduce)_csc_cpp" R/query_eval.R
```

Will tell us which queries still route through these kernels and at
what input layout. If some of them are only reached for dgCMatrix
inputs that never appear in the bake-off set, the memory fix is still
correct but the urgency drops from "ship next slice" to
"opportunistic cleanup".

The category-B kernels (`kernel_mode_csc`, `kernel_quantile_csc`
non-grouped axis = 0) are already confirmed hot on the `cells_daf`
UMIs workload — they are the dispatch target for julia_queries_026
and _028 on sparse layouts. The parallelism-enabling fix has clear
user-facing value independent of audit.

## Decision points to lock at 9d-N kickoff

1. **Scope — full six or subset?** Options:
   - **6A**: All six kernels, bundled. Clean "row-partition sweep"
     narrative, largest memory win.
   - **6B**: Only the two category-B kernels (mode + quantile serial
     fills). Smaller scope, clear parallelism-enabling win, no
     dispatch-audit dependency.
   - **6C**: Category-A kernels (4) that the audit confirms are on
     hot paths + both category-B kernels. Depends on audit outcome.
2. **Stress-test shape.** Same idea as 9d-M: pick one shared fixture
   that exercises all included kernels at a size large enough to
   show pathology but small enough to run on CI. Proposed:
   `nrow = 10000`, `ncol = 10000`, density 0.02. The group axis is
   N/A for the non-grouped variants.
3. **Profile-before vs. profile-after?** 9d-M did both (pre-fix
   baseline + post-fix comparison) and it paid off — the numbers
   made the exit note compelling. Recommend the same here.
4. **Unified helper or per-kernel edits?** Row-partition is now a
   well-understood pattern; we could factor a shared
   `row_partition_scan()` template, but 9d-M kept it inline in each
   kernel and the readability argument may favour the same here.
   Lock the call.
5. **Schedule clause.** 9d-M used `DAFR_OMP_PARALLEL_IF` (static
   chunking inherited). If post-9d-M profiling on real skewed
   metacell data shows load imbalance, 9d-N is a natural slice to
   add `DAFR_PARALLEL_FOR_DYNAMIC` with a tuned chunk size.
   Otherwise defer.

## Carry-over items NOT in 9d-N scope

- **mmap S7-ctor floor** — still 4 accept-class breaches from 9c/9d-M
  exits. Architectural, separate slice.
- **Two-pass flat-storage optimisation for mode/quantile per-cell
  overhead** — the `vector<Entry>` header cost that 9d-M explicitly
  deferred in its §2. Independent of category A or B.
- **Acc-struct slimming** — orthogonal constant-factor work.
- **`copy_all` double-write bug** — small focused fix.
- **9d-M code-review minor items** — deferred follow-up:
  - `src/kernel_grouped_reduce_csc.cpp:34` — unused
    `using dafr_grouped::acc_merge;`
  - `src/kernel_grouped_mode_csc.cpp:203-204` — redundant
    `std::sort` (entries arrive pre-sorted in the row-partition
    version). Small perf tweak; fold into 9d-N or a housekeeping
    slice.

## Known mines (for the 9d-N agent)

- **Formula authority is still `R/operations.R` `.op_*`.** Any
  rewrite must produce bit-identical output on the 1914-test
  regression net.
- **All kernels in scope should be fixed in a single slice** once
  the scope (6A/6B/6C) is locked. Leaving one unfixed means that
  one caps the memory ceiling for the parallel dispatch.
- **`OMP_NUM_THREADS=1` bake-off is unchanged.** Stress tests in
  `tests/testthat/`, not `benchmarks/`.
- **Bake-off `empty_cache` per iteration is load-bearing.**
- **`R CMD INSTALL . --preclean` before any bake-off run.**
- **cpp11 (NOT Rcpp)**, **`.h` headers (NOT `.hpp`)**, **OpenMP via
  `openmp_shim.h` helpers** (`DAFR_PARALLEL_FOR`,
  `DAFR_OMP_PARALLEL_IF`, `dafr_omp_get_*`), never raw pragmas at
  kernel call sites.
- **Follow the 9d-M row-partition template literally** — the three
  kernels already on `main` are the reference implementation. The
  pattern: `DAFR_OMP_PARALLEL_IF(nrow >= threshold)` + thread-local
  `[r0, r1)` + filter `if (pi[k] < r0 || pi[k] >= r1) continue` +
  race-free write into shared `nrow`-sized accs.
- **libgomp caches `OMP_NUM_THREADS` at DSO load.** Stress tests'
  `Sys.setenv` is no-op after load; budget-based RSS assertions
  must be calibrated to whatever thread count libgomp picked up.
- **`dafr.kernel_threshold`** must not be set to `Inf` anywhere.

## Repo conventions (carried forward from 9d-M)

- 4-space R indent, no tabs.
- S7 multi-dispatch always uses `list(ClassA, ...)` signatures.
- `#' @include` directives are load-bearing for S7 method registration.
- `sort(..., method = "radix")` for all listing returns.
- `.assert_name(x, "x")` / `.assert_flag(x, "x")` at public boundary.
- `.DAFR_UNDEF` sentinel + `.is_undef` for optional-default args.
- `sQuote()` around names in error messages.
- No emojis. Never `--no-verify` / `--amend` / force-push. Always
  NEW commits.
- Kernel naming: `kernel_<op>_<layout>.cpp` with `[[cpp11::register]]`
  entry points suffixed `_cpp`.
- Formula authority: `R/operations.R` `.op_*` is source of truth.

## Auto-memory carry-over

- **Native port motivation + perf parity goal** — with 9d-M closed,
  parallel-scale safety is in place for the grouped G3 family. 9d-N
  extends the same safety to the non-grouped family.
- **Opus for design-heavy work / final whole-branch reviews** — the
  9d-M Opus review caught two real minor issues and gave a rigorous
  bit-identity argument. Repeat for 9d-N.
- **Model selection — use Opus freely.**
- **Bake-off must invalidate cache per iteration** — unchanged.
- **L2 upstream PR declined** — do NOT re-raise at slice exits.

## Julia DAF state at handoff

`~/src/DataAxesFormats.jl` at `49fbba140437387a378217c2fa658d4231d0c8c1`
(unchanged since Slice 3 — ten slices of stability).
`~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
Before regenerating any fixture, verify DAF.jl has not moved.

## Ready-to-paste prompt for Slice 9d-N

> Start implementing Slice 9d-N of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native` on branch `main`, tag
>   `slice-9d-m` marks the Slice 9d-M merge commit (`676386e`).
> - Dev repo: `~/src/dafr-native/dev` — nested repo, remote
>   `aviezerl/dafr-native-notes`.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-9d-n-kickoff.md`
>   (this document).
> - Slice 9d-M exit: `~/src/dafr-native/dev/notes/slice-9d-m-exit.md`.
>
> **Scope candidate:** extend row-partition to up to 6 CSC kernels
> — four with `O(nthreads × nrow)` bucket patterns
> (`kernel_var_csc`, `kernel_minmax_csc`, `kernel_log_reduce`,
> `kernel_geomean_csc`) and two currently-serial kernels that could
> gain parallelism (`kernel_mode_csc`, `kernel_quantile_csc`
> non-grouped axis = 0). Lock final scope (6A / 6B / 6C) at design.
>
> **Dispatch audit prerequisite:** confirm which category-A kernels
> are still on the hot path of the bake-off query set before
> bundling; if some are dead dispatch paths, narrow scope to 6B +
> hot category-A kernels.
>
> Start with `superpowers:brainstorming` to lock scope, then
> `superpowers:writing-plans`, then
> `superpowers:subagent-driven-development`.
>
> **Model selection:** Opus for design and final whole-branch review,
> Sonnet for mechanical per-kernel edits (the 9d-M pattern is now
> established and each kernel edit follows the literal template).
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at
> `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice
> 3 — ten slices). `~/src/TanayLabUtilities.jl` at `48a4a57`.
