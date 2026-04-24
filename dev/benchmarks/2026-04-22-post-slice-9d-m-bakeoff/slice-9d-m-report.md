# Bake-off: R vs Julia (2026-04-22)

Ratio = dafr_median / julia_median. Higher = dafr slower. A breach is
`ratio > threshold` for the query's tier.

## Headers

```
# runner: R
# dafr_commit: d6d9a14c681d559437926ee75a374ed71a8d7d98
# R_version: 4.4.1
# platform: x86_64-pc-linux-gnu
# OMP_NUM_THREADS: 1
# BLAS: /net/mraid20/ifs/wisdom/tanay_lab/data/users/eladch/tools/CO8/mkl/2024.1/lib/libmkl_gf_lp64.so.2
# fixtures: big_sparse=112972aa279bf184; cells_daf=892948c6ec288d6c; chain_triple=9f75f5a89bf4ba5f; mmap_reopen=892948c6ec288d6c
```

```
# runner: Julia
# dafr_commit: f7a373245ea16da6df7746f2526c346f2353752b
# julia_version: 1.12.5
# platform: x86_64-conda-linux-gnu
# JULIA_NUM_THREADS: default
# BLAS: LBTConfig([ILP64] libopenblas64_.so)
# fixtures: big_sparse=112972aa279bf184; cells_daf=892948c6ec288d6c; chain_triple=9f75f5a89bf4ba5f; mmap_reopen=892948c6ec288d6c
```

## BREACHED (4)

| query_id | category | fixture | R (median) | J (median) | ratio | threshold |
|---|---|---|---|---|---|---|
| mmap_open_read_axis | mmap | mmap_reopen |   2.24 ms | 793.71 µs | 2.82× | 1.50× |
| mmap_open_read_matrix | mmap | mmap_reopen |   4.35 ms |   1.66 ms | 2.62× | 1.50× |
| mmap_open_read_vector | mmap | mmap_reopen |   3.10 ms |   1.54 ms | 2.02× | 1.50× |
| mmap_open_read_scalar | mmap | mmap_reopen |   1.04 ms | 642.04 µs | 1.62× | 1.50× |

## Within threshold (75)

| query_id | category | fixture | R (median) | J (median) | ratio | threshold |
|---|---|---|---|---|---|---|
| julia_queries_024 | light | cells_daf |   4.80 ms |   2.78 ms | 1.73× | 2.00× |
| chain_reduce | chain | chain_triple |   2.78 ms |   1.67 ms | 1.67× | 2.00× |
| chain_read_scalar | chain | chain_triple | 659.54 µs | 398.17 µs | 1.66× | 2.00× |
| julia_queries_023 | light | cells_daf |   4.77 ms |   2.91 ms | 1.64× | 2.00× |
| julia_queries_015 | light | cells_daf |   3.23 ms |   2.08 ms | 1.55× | 2.00× |
| julia_queries_007 | light | cells_daf | 562.19 µs | 371.48 µs | 1.51× | 2.00× |
| julia_queries_016 | light | cells_daf |   2.62 ms |   1.76 ms | 1.49× | 2.00× |
| julia_queries_027 | light | cells_daf |  10.14 ms |   7.45 ms | 1.36× | 2.00× |
| julia_queries_026 | light | cells_daf |   6.39 ms |   5.04 ms | 1.27× | 2.00× |
| julia_queries_022 | light | cells_daf |   2.94 ms |   2.40 ms | 1.23× | 2.00× |
| julia_queries_021 | light | cells_daf |   2.94 ms |   2.40 ms | 1.22× | 2.00× |
| julia_queries_034 | light | cells_daf |   2.14 ms |   1.81 ms | 1.19× | 2.00× |
| julia_queries_028 | light | cells_daf |  10.39 ms |   8.81 ms | 1.18× | 2.00× |
| julia_queries_039 | light | cells_daf |   2.06 ms |   1.78 ms | 1.15× | 2.00× |
| julia_queries_037 | light | cells_daf |   2.05 ms |   1.80 ms | 1.14× | 2.00× |
| julia_queries_038 | light | cells_daf |   2.02 ms |   1.79 ms | 1.13× | 2.00× |
| julia_queries_002 | light | cells_daf | 530.28 µs | 476.60 µs | 1.11× | 2.00× |
| julia_queries_003 | light | cells_daf | 532.35 µs | 478.89 µs | 1.11× | 2.00× |
| julia_queries_020 | light | cells_daf |   1.57 ms |   1.42 ms | 1.11× | 2.00× |
| julia_queries_029 | light | cells_daf |   2.00 ms |   1.81 ms | 1.10× | 2.00× |
| julia_queries_030 | light | cells_daf |   1.98 ms |   1.82 ms | 1.09× | 2.00× |
| julia_queries_018 | light | cells_daf |   1.39 ms |   1.28 ms | 1.08× | 2.00× |
| julia_queries_017 | light | cells_daf |   1.39 ms |   1.30 ms | 1.07× | 2.00× |
| julia_queries_036 | light | cells_daf |   1.91 ms |   1.79 ms | 1.07× | 2.00× |
| julia_queries_031 | light | cells_daf |   1.90 ms |   1.78 ms | 1.07× | 2.00× |
| julia_queries_035 | light | cells_daf |   1.90 ms |   1.78 ms | 1.07× | 2.00× |
| julia_queries_033 | light | cells_daf |   1.89 ms |   1.79 ms | 1.06× | 2.00× |
| julia_queries_001 | light | cells_daf | 391.11 µs | 375.30 µs | 1.04× | 2.00× |
| julia_queries_044 | light | cells_daf |   5.31 ms |   5.12 ms | 1.04× | 2.00× |
| julia_queries_049 | light | cells_daf |   1.23 ms |   1.22 ms | 1.01× | 2.00× |
| julia_queries_032 | light | cells_daf |   1.81 ms |   1.79 ms | 1.01× | 2.00× |
| julia_queries_004 | light | cells_daf | 355.79 µs | 363.14 µs | 0.98× | 2.00× |
| julia_queries_009 | light | cells_daf |   1.09 ms |   1.12 ms | 0.97× | 2.00× |
| julia_queries_040 | light | cells_daf |   2.86 ms |   2.95 ms | 0.97× | 2.00× |
| julia_queries_045 | light | cells_daf |   4.13 ms |   4.32 ms | 0.96× | 2.00× |
| julia_queries_046 | light | cells_daf |   4.26 ms |   4.46 ms | 0.96× | 2.00× |
| julia_queries_008 | light | cells_daf | 929.96 µs | 995.37 µs | 0.93× | 2.00× |
| julia_queries_019 | light | cells_daf |   1.13 ms |   1.22 ms | 0.92× | 2.00× |
| julia_queries_047 | light | cells_daf |   4.00 ms |   4.41 ms | 0.91× | 2.00× |
| julia_queries_050 | light | cells_daf |   1.29 ms |   1.43 ms | 0.90× | 2.00× |
| julia_queries_043 | light | cells_daf |   4.33 ms |   4.94 ms | 0.88× | 2.00× |
| julia_queries_025 | light | cells_daf |   5.27 ms |   6.27 ms | 0.84× | 2.00× |
| chain_read_matrix | chain | chain_triple | 915.53 µs |   1.09 ms | 0.84× | 2.00× |
| julia_queries_010 | light | cells_daf | 994.72 µs |   1.22 ms | 0.81× | 2.00× |
| julia_queries_042 | light | cells_daf |   4.28 ms |   5.29 ms | 0.81× | 2.00× |
| julia_queries_011 | light | cells_daf |   1.11 ms |   1.39 ms | 0.80× | 2.00× |
| julia_queries_014 | light | cells_daf |   1.15 ms |   1.44 ms | 0.79× | 2.00× |
| julia_queries_048 | light | cells_daf |   1.41 ms |   1.80 ms | 0.79× | 2.00× |
| julia_queries_041 | light | cells_daf |   4.24 ms |   5.48 ms | 0.77× | 2.00× |
| julia_queries_013 | light | cells_daf |   1.28 ms |   1.66 ms | 0.77× | 2.00× |
| julia_queries_051 | light | cells_daf |   2.08 ms |   2.85 ms | 0.73× | 2.00× |
| julia_queries_012 | light | cells_daf |   1.12 ms |   1.57 ms | 0.71× | 2.00× |
| chain_read_vector | chain | chain_triple | 681.40 µs |   1.15 ms | 0.59× | 2.00× |
| julia_queries_006 | light | cells_daf | 233.88 µs | 558.54 µs | 0.42× | 2.00× |
| julia_queries_005 | light | cells_daf | 243.09 µs | 611.04 µs | 0.40× | 2.00× |
| grouped_g3_mean_1000 | grouped | big_sparse | 634.73 ms |   2.04 s | 0.31× | 1.20× |
| kernel_mean_col | kernel | big_sparse | 184.99 ms | 643.35 ms | 0.29× | 1.20× |
| kernel_sum_col | kernel | big_sparse | 182.21 ms | 712.71 ms | 0.26× | 1.20× |
| kernel_max_col | kernel | big_sparse | 172.74 ms | 683.00 ms | 0.25× | 1.20× |
| kernel_mode_row | kernel | big_sparse | 876.53 ms |   6.31 s | 0.14× | 1.20× |
| grouped_g2_sum_100 | grouped | big_sparse | 233.93 ms |   2.88 s | 0.08× | 1.20× |
| grouped_g2_mean_1000 | grouped | big_sparse | 313.78 ms |   3.87 s | 0.08× | 1.20× |
| grouped_g2_mean_100 | grouped | big_sparse | 235.98 ms |   3.16 s | 0.07× | 1.20× |
| kernel_geomean_row | kernel | big_sparse | 244.85 ms |   3.45 s | 0.07× | 1.20× |
| grouped_g3_max_100 | grouped | big_sparse | 145.94 ms |   2.07 s | 0.07× | 1.20× |
| kernel_quantile_row | kernel | big_sparse | 211.03 ms |   3.16 s | 0.07× | 1.20× |
| grouped_g3_sum_100 | grouped | big_sparse | 131.39 ms |   1.99 s | 0.07× | 1.20× |
| kernel_sum_row | kernel | big_sparse | 175.92 ms |   2.73 s | 0.06× | 1.20× |
| kernel_mean_row | kernel | big_sparse | 175.82 ms |   2.83 s | 0.06× | 1.20× |
| kernel_max_row | kernel | big_sparse | 178.79 ms |   2.90 s | 0.06× | 1.20× |
| grouped_g3_mean_100 | grouped | big_sparse | 127.84 ms |   2.11 s | 0.06× | 1.20× |
| kernel_median_row | kernel | big_sparse | 210.78 ms |   3.50 s | 0.06× | 1.20× |
| grouped_g2_max_100 | grouped | big_sparse | 114.07 ms |   2.94 s | 0.04× | 1.20× |
| kernel_std_row | kernel | big_sparse | 178.32 ms |   5.85 s | 0.03× | 1.20× |
| kernel_var_row | kernel | big_sparse | 177.83 ms |   5.86 s | 0.03× | 1.20× |

## Julia N/A (R-only or DAF.jl gap)

_(none)_

