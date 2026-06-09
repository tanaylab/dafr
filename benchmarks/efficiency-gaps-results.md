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
