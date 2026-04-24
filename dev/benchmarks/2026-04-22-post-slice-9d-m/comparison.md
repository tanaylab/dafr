# Slice 9d-M — pre-fix vs. post-fix profile comparison

Fixture: 10000 × 10000 CSC, 100 groups, density 0.01 (nnz ≈ 1M).
Machine: 128 threads, 1 TB RAM.
`threshold = 1L` forces parallel dispatch regardless of matrix size.

## Peak RSS (whole Rscript, /usr/bin/time -v `Maximum resident set size`)

| Threads | Pre-fix | Post-fix | Reduction |
|---:|---:|---:|---:|
| 1 | 397 MB | 342 MB | -55 MB |
| 8 | 779 MB | 341 MB | -438 MB |
| 32 | 2.09 GB | 340 MB | -1.75 GB |
| 128 | **7.34 GB** | **340 MB** | **-7.0 GB** |

Post-fix peak RSS is flat at ~340 MB across all thread counts — the
`nthreads` multiplier is gone, as designed. Row-partition footprint is
bounded by the output shape (`nrow × ngroups × sizeof(Acc) + per-cell
vector overhead + R/Matrix process cost`), independent of thread count.

## Wall-time per kernel call (G3, axis = 3)

| Kernel | Threads | Pre-fix | Post-fix | Speed-up |
|---|---:|---:|---:|---:|
| `reduce_csc` Sum  | 1   | 0.106 s | 0.053 s | 2.0× |
| `reduce_csc` Sum  | 8   | 0.273 s | 0.037 s | 7.4× |
| `reduce_csc` Sum  | 32  | 0.948 s | 0.036 s | 26× |
| `reduce_csc` Sum  | 128 | 3.177 s | 0.036 s | **88×** |
| `reduce_csc` Var  | 1   | 0.105 s | 0.053 s | 2.0× |
| `reduce_csc` Var  | 128 | 3.519 s | 0.034 s | **103×** |
| `mode_csc`        | 1   | 0.375 s | 0.312 s | 1.2× |
| `mode_csc`        | 128 | 3.615 s | 0.063 s | **57×** |
| `quantile_csc` p50| 1   | 0.163 s | 0.123 s | 1.3× |
| `quantile_csc` p50| 128 | 1.690 s | 0.059 s | **29×** |

Pre-fix wall-time at 128 threads was *slower* than at 1 thread
because the serial merge of `nthreads × nrow × ngroups` Accs
dominated. Row-partition removes the merge entirely — post-fix
wall-time decreases monotonically with thread count up to ~8–32
threads, then plateaus as the work per thread becomes too small to
amortise overhead.

## Verdict — acceptance criteria

- [x] Peak RSS at 128 threads is within ±30% of 1-thread baseline.
  Post-fix 128-thread RSS = 340 MB vs. 1-thread 342 MB — 0.6%
  delta. Criterion met with room to spare.
- [x] Wall-time at 128 threads is ≤ 1-thread wall-time for all
  four kernels.
  * reduce Sum:      0.036 s (128t) < 0.053 s (1t) ✓
  * reduce Var:      0.034 s (128t) < 0.053 s (1t) ✓
  * mode:            0.063 s (128t) < 0.312 s (1t) ✓
  * quantile p50:    0.059 s (128t) < 0.123 s (1t) ✓
- [x] No correctness regressions in the test suite: `[ FAIL 0 |
  WARN 1 | SKIP 1 | PASS 1914 ]` (1907 baseline + 7 new 9d-M
  assertions with `NOT_CRAN=true`).

All three acceptance criteria met. The row-partition rewrite achieves
the memory-fix goal and delivers a substantial performance win as a
side effect.
