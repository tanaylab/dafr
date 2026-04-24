# Bake-off: R vs Julia (2026-04-22)

Ratio = dafr_median / julia_median. Higher = dafr slower. A breach is
`ratio > threshold` for the query's tier.

## Headers

```
# runner: R
# dafr_commit: f7a373245ea16da6df7746f2526c346f2353752b
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
| mmap_open_read_axis | mmap | mmap_reopen |   2.11 ms | 793.71 µs | 2.66× | 1.50× |
| mmap_open_read_matrix | mmap | mmap_reopen |   4.09 ms |   1.66 ms | 2.46× | 1.50× |
| mmap_open_read_vector | mmap | mmap_reopen |   2.96 ms |   1.54 ms | 1.93× | 1.50× |
| mmap_open_read_scalar | mmap | mmap_reopen |   1.00 ms | 642.04 µs | 1.56× | 1.50× |

## Within threshold (75)

| query_id | category | fixture | R (median) | J (median) | ratio | threshold |
|---|---|---|---|---|---|---|
| julia_queries_024 | light | cells_daf |   4.79 ms |   2.78 ms | 1.72× | 2.00× |
| chain_reduce | chain | chain_triple |   2.80 ms |   1.67 ms | 1.68× | 2.00× |
| chain_read_scalar | chain | chain_triple | 655.78 µs | 398.17 µs | 1.65× | 2.00× |
| julia_queries_023 | light | cells_daf |   4.73 ms |   2.91 ms | 1.62× | 2.00× |
| julia_queries_015 | light | cells_daf |   3.26 ms |   2.08 ms | 1.57× | 2.00× |
| julia_queries_007 | light | cells_daf | 546.38 µs | 371.48 µs | 1.47× | 2.00× |
| julia_queries_016 | light | cells_daf |   2.58 ms |   1.76 ms | 1.46× | 2.00× |
| julia_queries_027 | light | cells_daf |  10.27 ms |   7.45 ms | 1.38× | 2.00× |
| julia_queries_026 | light | cells_daf |   6.23 ms |   5.04 ms | 1.24× | 2.00× |
| julia_queries_022 | light | cells_daf |   2.94 ms |   2.40 ms | 1.22× | 2.00× |
| julia_queries_021 | light | cells_daf |   2.92 ms |   2.40 ms | 1.22× | 2.00× |
| julia_queries_028 | light | cells_daf |  10.44 ms |   8.81 ms | 1.18× | 2.00× |
| julia_queries_002 | light | cells_daf | 533.21 µs | 476.60 µs | 1.12× | 2.00× |
| julia_queries_003 | light | cells_daf | 528.54 µs | 478.89 µs | 1.10× | 2.00× |
| julia_queries_034 | light | cells_daf |   1.96 ms |   1.81 ms | 1.08× | 2.00× |
| julia_queries_020 | light | cells_daf |   1.50 ms |   1.42 ms | 1.06× | 2.00× |
| julia_queries_038 | light | cells_daf |   1.87 ms |   1.79 ms | 1.05× | 2.00× |
| julia_queries_037 | light | cells_daf |   1.86 ms |   1.80 ms | 1.03× | 2.00× |
| julia_queries_033 | light | cells_daf |   1.81 ms |   1.79 ms | 1.01× | 2.00× |
| julia_queries_044 | light | cells_daf |   5.15 ms |   5.12 ms | 1.00× | 2.00× |
| julia_queries_018 | light | cells_daf |   1.27 ms |   1.28 ms | 0.99× | 2.00× |
| julia_queries_001 | light | cells_daf | 370.98 µs | 375.30 µs | 0.99× | 2.00× |
| julia_queries_049 | light | cells_daf |   1.20 ms |   1.22 ms | 0.99× | 2.00× |
| julia_queries_039 | light | cells_daf |   1.75 ms |   1.78 ms | 0.98× | 2.00× |
| julia_queries_031 | light | cells_daf |   1.73 ms |   1.78 ms | 0.98× | 2.00× |
| julia_queries_017 | light | cells_daf |   1.26 ms |   1.30 ms | 0.97× | 2.00× |
| julia_queries_035 | light | cells_daf |   1.73 ms |   1.78 ms | 0.97× | 2.00× |
| julia_queries_032 | light | cells_daf |   1.73 ms |   1.79 ms | 0.97× | 2.00× |
| julia_queries_036 | light | cells_daf |   1.73 ms |   1.79 ms | 0.97× | 2.00× |
| julia_queries_029 | light | cells_daf |   1.73 ms |   1.81 ms | 0.95× | 2.00× |
| julia_queries_004 | light | cells_daf | 344.89 µs | 363.14 µs | 0.95× | 2.00× |
| julia_queries_030 | light | cells_daf |   1.69 ms |   1.82 ms | 0.93× | 2.00× |
| julia_queries_045 | light | cells_daf |   3.98 ms |   4.32 ms | 0.92× | 2.00× |
| julia_queries_040 | light | cells_daf |   2.70 ms |   2.95 ms | 0.91× | 2.00× |
| julia_queries_046 | light | cells_daf |   4.05 ms |   4.46 ms | 0.91× | 2.00× |
| julia_queries_008 | light | cells_daf | 901.99 µs | 995.37 µs | 0.91× | 2.00× |
| julia_queries_047 | light | cells_daf |   3.99 ms |   4.41 ms | 0.90× | 2.00× |
| julia_queries_009 | light | cells_daf | 982.51 µs |   1.12 ms | 0.88× | 2.00× |
| julia_queries_050 | light | cells_daf |   1.24 ms |   1.43 ms | 0.87× | 2.00× |
| chain_read_matrix | chain | chain_triple | 930.57 µs |   1.09 ms | 0.85× | 2.00× |
| julia_queries_043 | light | cells_daf |   4.19 ms |   4.94 ms | 0.85× | 2.00× |
| julia_queries_025 | light | cells_daf |   5.27 ms |   6.27 ms | 0.84× | 2.00× |
| julia_queries_019 | light | cells_daf |   1.02 ms |   1.22 ms | 0.83× | 2.00× |
| julia_queries_051 | light | cells_daf |   2.24 ms |   2.85 ms | 0.78× | 2.00× |
| julia_queries_042 | light | cells_daf |   4.14 ms |   5.29 ms | 0.78× | 2.00× |
| julia_queries_048 | light | cells_daf |   1.39 ms |   1.80 ms | 0.77× | 2.00× |
| julia_queries_010 | light | cells_daf | 922.60 µs |   1.22 ms | 0.75× | 2.00× |
| julia_queries_041 | light | cells_daf |   4.09 ms |   5.48 ms | 0.75× | 2.00× |
| julia_queries_011 | light | cells_daf |   1.03 ms |   1.39 ms | 0.74× | 2.00× |
| julia_queries_014 | light | cells_daf |   1.05 ms |   1.44 ms | 0.73× | 2.00× |
| julia_queries_013 | light | cells_daf |   1.14 ms |   1.66 ms | 0.68× | 2.00× |
| julia_queries_012 | light | cells_daf |   1.07 ms |   1.57 ms | 0.68× | 2.00× |
| chain_read_vector | chain | chain_triple | 694.60 µs |   1.15 ms | 0.60× | 2.00× |
| grouped_g3_mean_1000 | grouped | big_sparse |   1.07 s |   2.04 s | 0.52× | 1.20× |
| julia_queries_006 | light | cells_daf | 236.25 µs | 558.54 µs | 0.42× | 2.00× |
| julia_queries_005 | light | cells_daf | 247.21 µs | 611.04 µs | 0.40× | 2.00× |
| kernel_mean_col | kernel | big_sparse | 161.37 ms | 643.35 ms | 0.25× | 1.20× |
| kernel_sum_col | kernel | big_sparse | 162.21 ms | 712.71 ms | 0.23× | 1.20× |
| kernel_max_col | kernel | big_sparse | 153.85 ms | 683.00 ms | 0.23× | 1.20× |
| kernel_mode_row | kernel | big_sparse | 872.30 ms |   6.31 s | 0.14× | 1.20× |
| grouped_g3_max_100 | grouped | big_sparse | 191.79 ms |   2.07 s | 0.09× | 1.20× |
| grouped_g3_sum_100 | grouped | big_sparse | 171.80 ms |   1.99 s | 0.09× | 1.20× |
| grouped_g3_mean_100 | grouped | big_sparse | 169.16 ms |   2.11 s | 0.08× | 1.20× |
| grouped_g2_sum_100 | grouped | big_sparse | 214.66 ms |   2.88 s | 0.07× | 1.20× |
| grouped_g2_mean_1000 | grouped | big_sparse | 286.85 ms |   3.87 s | 0.07× | 1.20× |
| grouped_g2_mean_100 | grouped | big_sparse | 214.56 ms |   3.16 s | 0.07× | 1.20× |
| kernel_sum_row | kernel | big_sparse | 184.45 ms |   2.73 s | 0.07× | 1.20× |
| kernel_geomean_row | kernel | big_sparse | 225.57 ms |   3.45 s | 0.07× | 1.20× |
| kernel_quantile_row | kernel | big_sparse | 194.26 ms |   3.16 s | 0.06× | 1.20× |
| kernel_max_row | kernel | big_sparse | 158.22 ms |   2.90 s | 0.05× | 1.20× |
| kernel_median_row | kernel | big_sparse | 190.01 ms |   3.50 s | 0.05× | 1.20× |
| kernel_mean_row | kernel | big_sparse | 153.28 ms |   2.83 s | 0.05× | 1.20× |
| grouped_g2_max_100 | grouped | big_sparse | 112.58 ms |   2.94 s | 0.04× | 1.20× |
| kernel_std_row | kernel | big_sparse | 158.57 ms |   5.85 s | 0.03× | 1.20× |
| kernel_var_row | kernel | big_sparse | 155.32 ms |   5.86 s | 0.03× | 1.20× |

## Julia N/A (R-only or DAF.jl gap)

_(none)_

