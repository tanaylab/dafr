# dafr vs DataAxesFormats.jl efficiency gaps - baseline (2026-06-09)

Baseline measurements taken after the ZarrDaf interop fix and **before** the
Phase-2 efficiency work (zero-copy zarr reads, parallel dense kernels). These
quantify the gaps surfaced by the 2026-06 parity review so the Phase-2 fixes
can be measured against them.

Reproduce:

```
rm -rf /tmp/daf_bench
Rscript benchmarks/efficiency-gaps.R                                  # builds the fixture, R side
conda run -n dafr-mcview julia -t 16 benchmarks/efficiency-gaps.jl    # Julia side
```

## Setup

- Fixture: a `.daf.zarr` with one dense `Float64` `cell x gene` matrix,
  4000 x 2500 = 10M elements (~80 MB), uncompressed single chunk.
- 7 reps, median reported. Page cache warm for both (this isolates
  decode/allocation cost from raw disk I/O).
- Threads: R `set_num_threads(16)`, Julia `-t 16`. R's dense reduction and
  element-wise paths are single-threaded regardless (no OpenMP kernel yet);
  Julia parallelises them.
- Result checksums match across both implementations (correctness control).

## Results (median ms)

| operation                              | dafr (R) | DAF.jl | R / Julia |
|----------------------------------------|---------:|-------:|----------:|
| ZarrDaf read dense matrix (`get_matrix`) |    511   |   8.1  |   **63×** |
| dense reduction `>\| Sum`                |    282   |  121   |    2.3×   |
| dense element-wise `% Abs`              |    291   |   78   |    3.7×   |
| dense element-wise `% Log base 2 eps 1` |    395   |   61   |    6.4×   |

## Reading the numbers

- **ZarrDaf read (63×) is the dominant gap.** Julia memory-maps the stored,
  uncompressed, single-chunk array and reads it zero-copy; `get_matrix`
  returns almost instantly and `sum` faults pages straight from the page
  cache. dafr decodes the whole chunk through `readBin` into a fresh R
  matrix (`R/zarr_v2.R::zarr_v2_decode_chunk`) on every read. The FilesDaf
  backend already has a typed-mmap fast path that was never wired into the
  zarr reader - that is the Phase-2 target with the largest payoff for
  read-heavy / random-access workloads over `.daf.zarr`.
- **Dense reduction / element-wise (2-6×)** is the single-threaded-R vs
  parallel-Julia gap (`R/query_eval.R::.apply_reduction_fast`,
  `R/operations.R`). Sparse and grouped reductions already have OpenMP
  kernels; the dense path does not.
- **`% Log` shows 6.4× despite R having an OpenMP `kernel_log_dense_mat_cpp`.**
  That is larger than expected and suggests the dense matrix-query path may
  not be dispatching to the parallel log kernel (or pays heavy per-call query
  / result-allocation overhead). Phase 2 should confirm the kernel is actually
  engaged for matrix `% Log` before adding new kernels.

These are baselines; Phase 2 will re-run this harness and report the closed
gaps.

## Phase 2 - zarr zero-copy reads (2026-06-09)

Wired the FilesDaf typed-mmap fast path into the ZarrDaf directory-store
reader (`R/zarr_format.R::.zarr_try_mmap_dense`): a stored, uncompressed,
single-chunk `<f8`/`<i4` array is now returned as an ALTREP mmap view
(`mmap_real`/`mmap_int`) instead of decoded through `readBin`. The mmap reads
are tagged `MappedData` so the capped in-memory cache tier doesn't copy them.

Effect on the dense-matrix read (4000x2500 Float64, mmap on vs off):

| stage                        | decode (mmap off) | mmap on |
|------------------------------|------------------:|--------:|
| `.zarr_get_dense_matrix`     |            322 ms |    1 ms |
| `get_matrix` (cold, full)    |            ~390 ms |   85 ms |

The data decode+copy is eliminated (322 ms -> ~1 ms, lazy). The ~85 ms
residual on a *cold* `get_matrix` is **not** the data: it is the one-time
axis-name (vlen-utf8) decode in `.attach_matrix_axis_dimnames` (~70 ms for
4000+2500 entries), which is cached after the first touch in real use. That
axis-string decode is a separate follow-up (it does not materialise the
matrix - ALTREP is preserved). DataAxesFormats.jl reads the same matrix in
~8 ms; the remaining gap is now that axis decode, not the chunk read.

Still single-threaded (separate follow-up): dense matrix reductions and
`% Abs`/`% Round`/`% Clamp`/`% Convert` element-wise ops.

## Phase 3 - `% Log` matrix per-call overhead (2026-06-10)

The `% Log` 6.4x gap was the per-call overhead the baseline suspected, not the
kernel. `.apply_eltwise` (`R/query_eval.R`) ran its negative-input domain check
as `any(as.numeric(value) + eps < 0)`: on a dense matrix that copied the whole
matrix to a vector (`as.numeric`) and allocated a second full temporary
(`+ eps`) - two ~80 MB allocations plus a single-threaded scan - *before* the
parallel `kernel_log_dense_mat_cpp` ran. Replaced with an allocation-free
`min(value) + eps < 0` reduction (equivalent, since `+ eps` is monotonic;
preserves the exact DomainError + reported value).

| stage (4000x2500 Float64, warm)        | before | after |
|----------------------------------------|-------:|------:|
| negative pre-check                     |  92 ms |  7 ms |
| full `% Log base 2 eps 1` matrix query | ~166 ms | 81 ms |

`% Log` is now ~81 ms vs DataAxesFormats.jl's ~61 ms (1.3x) - effectively at
parity. The dense `>| Sum` (2.3x) and `% Abs` (3.7x) gaps were *assumed* to be
the genuine single-threaded-R vs parallel-Julia kernel gaps. Phase 4 profiled
them and found that assumption was wrong - see below.

## Phase 4 - the `>| Sum` gap was the axis-name decode, not the kernel (2026-06-10)

Profiling the `>| Sum` query (not just the raw reduction) overturned the
Phase-1 attribution. On a 4000x2500 fixture the raw `rowSums` is ~24 ms, but
the full `@ cell @ gene :: expr >| Sum` query is ~170 ms. An `Rprof` of the
query loop showed `zarr_v3_decode_strings` + `readBin` (axis-name vlen-utf8
decode) at **~45%** of the time and `rowSums` at ~34%. The axis names were
re-decoded from the store on *every distinct query*: `.zarr_axis_entries`
(`R/zarr_format.R`) always called `zarr_v3_decode_strings`, and the
`.cache_group_value` wrapper only tags the tier - it does not memoize. Only an
exact-repeat query hit the `QueryData` result cache. (The Phase-2 note's "cached
after first touch" was inaccurate across distinct queries.)

**Fix:** memoize the decode in `.zarr_axis_entries` at the `"memory"` tier keyed
by `cache_key_axis` + `axis_stamp` - the exact invalidation contract the
vector/matrix caches already use (`axis_stamp` bumps on `delete_axis`, so a
deleted+recreated axis invalidates). Chains/views over a ZarrDaf delegate
`format_axis_array` down to here, so they inherit the cache. Guards in
`tests/testthat/test-zarr-axis-cache.R`.

Reliable comparison is **within one process** (this box is a shared 128-core
NUMA host; cross-process medians swing 2-5x from page-cache/core placement -
the same R `abs()` path measured 33 ms and 168 ms in two runs). Within one
process, toggling the axis cache by clearing the `memory` tier vs the `query`
tier:

| `>| Sum` query (4000x2500, warm matrix) | axis decode re-run | axis cached |
|-----------------------------------------|-------------------:|------------:|
| median                                  |             107 ms |       67 ms |

~40 ms saved per distinct query (1.6x). In MCView, where many distinct queries
run over the same axes, every query pays that decode without the cache. `% Log`
benefits identically (its matrix-load path decodes the same axis names).

### Dense parallel compute kernels - measured and REJECTED

With the decode removed, the residual compute (`rowSums` ~24 ms, `abs` ~28 ms
on 10M elements) is single-threaded. A dense parallel **`% Abs`** kernel
(`kernel_abs_dense_*`, OpenMP, mirroring `kernel_log_dense.cpp`) was built and
benchmarked. In **isolation** it is 5-6x faster than R's `abs` (5 ms vs 28 ms).
But wired into the query it **regressed** `% Abs` from 33 ms to 82 ms
(within-process, reproducible across rounds, single- *and* multi-threaded). The
cpp11 `doubles_matrix<>` conversion of the query's in-scope ALTREP matrix copies
in that context, and the cost is not recovered by parallelising a
memory-bandwidth-bound op. **Reverted.**

A dense reduction kernel was not pursued: `rowSums` is ~24 ms of the now-67 ms
`>| Sum` query, the headline gap is already closed by the decode cache, and the
`% Abs` result shows in-query kernel dispatch can cost more than it saves for
cheap per-element ops. The `% Log` kernel stays (its per-element cost is high
enough that parallelism wins despite the conversion: 90 ms vs 216 ms with the
fast path off).

**Takeaway:** the Phase-1 baseline (282/291 ms) over-attributed the `>| Sum` /
`% Abs` gaps to single-threaded compute. The real lever for reduction/scalar
queries was the per-query axis-name decode; the parallel kernels the kickoff
called for are net-neutral-to-negative for these cheap ops on this stack.
