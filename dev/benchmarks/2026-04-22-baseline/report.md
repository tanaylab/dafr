# Bake-off: R vs Julia (2026-04-22)

Ratio = dafr_median / julia_median. Higher = dafr slower. A breach is
`ratio > threshold` for the query's tier.

## Headers

```
# runner: R
# dafr_commit: 2cc134867dcc16e3607e516e37789b809b02072c
# R_version: 4.4.1
# platform: x86_64-pc-linux-gnu
# OMP_NUM_THREADS: 1
# BLAS: /net/mraid20/ifs/wisdom/tanay_lab/data/users/eladch/tools/CO8/mkl/2024.1/lib/libmkl_gf_lp64.so.2
# fixtures: big_sparse=112972aa279bf184; cells_daf=892948c6ec288d6c; chain_triple=9f75f5a89bf4ba5f; mmap_reopen=892948c6ec288d6c
```

```
# runner: Julia
# dafr_commit: 2cc134867dcc16e3607e516e37789b809b02072c
# julia_version: 1.12.5
# platform: x86_64-conda-linux-gnu
# JULIA_NUM_THREADS: default
# BLAS: LBTConfig([ILP64] libopenblas64_.so)
# fixtures: big_sparse=112972aa279bf184; cells_daf=892948c6ec288d6c; chain_triple=9f75f5a89bf4ba5f; mmap_reopen=892948c6ec288d6c
```

## BREACHED (17)

| query_id | category | fixture | R (median) | J (median) | ratio | threshold |
|---|---|---|---|---|---|---|
| julia_queries_047 | light | cells_daf |  20.89 ms |   4.80 ms | 4.35× | 2.00× |
| julia_queries_042 | light | cells_daf |  20.66 ms |   4.88 ms | 4.23× | 2.00× |
| julia_queries_043 | light | cells_daf |  20.67 ms |   4.89 ms | 4.23× | 2.00× |
| julia_queries_045 | light | cells_daf |  20.89 ms |   5.27 ms | 3.96× | 2.00× |
| julia_queries_046 | light | cells_daf |  20.89 ms |   5.62 ms | 3.72× | 2.00× |
| julia_queries_044 | light | cells_daf |  20.54 ms |   5.56 ms | 3.69× | 2.00× |
| julia_queries_041 | light | cells_daf |  20.70 ms |   5.62 ms | 3.69× | 2.00× |
| julia_queries_026 | light | cells_daf |  18.28 ms |   6.01 ms | 3.04× | 2.00× |
| julia_queries_023 | light | cells_daf |   7.84 ms |   2.80 ms | 2.79× | 2.00× |
| mmap_open_read_axis | mmap | mmap_reopen |   2.14 ms | 810.76 µs | 2.64× | 1.50× |
| mmap_open_read_matrix | mmap | mmap_reopen |   4.20 ms |   1.68 ms | 2.50× | 1.50× |
| julia_queries_021 | light | cells_daf |   6.20 ms |   2.48 ms | 2.50× | 2.00× |
| julia_queries_022 | light | cells_daf |   6.07 ms |   2.45 ms | 2.48× | 2.00× |
| julia_queries_028 | light | cells_daf |  20.44 ms |   8.76 ms | 2.33× | 2.00× |
| julia_queries_024 | light | cells_daf |   6.29 ms |   2.80 ms | 2.25× | 2.00× |
| mmap_open_read_vector | mmap | mmap_reopen |   3.11 ms |   1.56 ms | 1.99× | 1.50× |
| mmap_open_read_scalar | mmap | mmap_reopen |   1.11 ms | 642.50 µs | 1.73× | 1.50× |

## Within threshold (62)

| query_id | category | fixture | R (median) | J (median) | ratio | threshold |
|---|---|---|---|---|---|---|
| chain_read_scalar | chain | chain_triple | 698.08 µs | 398.14 µs | 1.75× | 2.00× |
| chain_reduce | chain | chain_triple |   2.77 ms |   1.66 ms | 1.67× | 2.00× |
| julia_queries_016 | light | cells_daf |   2.69 ms |   1.69 ms | 1.60× | 2.00× |
| julia_queries_015 | light | cells_daf |   3.32 ms |   2.08 ms | 1.59× | 2.00× |
| julia_queries_007 | light | cells_daf | 565.33 µs | 393.23 µs | 1.44× | 2.00× |
| julia_queries_027 | light | cells_daf |  10.52 ms |   7.46 ms | 1.41× | 2.00× |
| julia_queries_034 | light | cells_daf |   2.21 ms |   1.81 ms | 1.22× | 2.00× |
| julia_queries_003 | light | cells_daf | 581.58 µs | 491.82 µs | 1.18× | 2.00× |
| julia_queries_002 | light | cells_daf | 589.31 µs | 504.28 µs | 1.17× | 2.00× |
| julia_queries_051 | light | cells_daf |   2.17 ms |   1.88 ms | 1.16× | 2.00× |
| julia_queries_037 | light | cells_daf |   2.11 ms |   1.83 ms | 1.15× | 2.00× |
| julia_queries_033 | light | cells_daf |   2.08 ms |   1.83 ms | 1.14× | 2.00× |
| julia_queries_018 | light | cells_daf |   1.40 ms |   1.24 ms | 1.13× | 2.00× |
| julia_queries_029 | light | cells_daf |   2.03 ms |   1.79 ms | 1.13× | 2.00× |
| julia_queries_036 | light | cells_daf |   2.04 ms |   1.84 ms | 1.11× | 2.00× |
| julia_queries_017 | light | cells_daf |   1.36 ms |   1.23 ms | 1.11× | 2.00× |
| julia_queries_038 | light | cells_daf |   2.07 ms |   1.88 ms | 1.10× | 2.00× |
| julia_queries_039 | light | cells_daf |   1.99 ms |   1.82 ms | 1.09× | 2.00× |
| julia_queries_035 | light | cells_daf |   2.01 ms |   1.85 ms | 1.09× | 2.00× |
| julia_queries_020 | light | cells_daf |   1.59 ms |   1.46 ms | 1.09× | 2.00× |
| julia_queries_031 | light | cells_daf |   1.97 ms |   1.83 ms | 1.08× | 2.00× |
| julia_queries_001 | light | cells_daf | 403.70 µs | 379.45 µs | 1.06× | 2.00× |
| julia_queries_049 | light | cells_daf |   1.31 ms |   1.24 ms | 1.06× | 2.00× |
| julia_queries_030 | light | cells_daf |   1.95 ms |   1.86 ms | 1.05× | 2.00× |
| julia_queries_032 | light | cells_daf |   1.91 ms |   1.86 ms | 1.03× | 2.00× |
| julia_queries_008 | light | cells_daf | 998.58 µs |   1.00 ms | 0.99× | 2.00× |
| julia_queries_009 | light | cells_daf |   1.10 ms |   1.14 ms | 0.97× | 2.00× |
| julia_queries_040 | light | cells_daf |   2.88 ms |   3.04 ms | 0.95× | 2.00× |
| julia_queries_019 | light | cells_daf |   1.14 ms |   1.21 ms | 0.95× | 2.00× |
| julia_queries_004 | light | cells_daf | 358.66 µs | 380.83 µs | 0.94× | 2.00× |
| julia_queries_050 | light | cells_daf |   1.32 ms |   1.47 ms | 0.90× | 2.00× |
| julia_queries_011 | light | cells_daf |   1.19 ms |   1.38 ms | 0.86× | 2.00× |
| julia_queries_010 | light | cells_daf |   1.06 ms |   1.25 ms | 0.85× | 2.00× |
| chain_read_matrix | chain | chain_triple | 908.08 µs |   1.09 ms | 0.84× | 2.00× |
| julia_queries_013 | light | cells_daf |   1.33 ms |   1.64 ms | 0.81× | 2.00× |
| julia_queries_025 | light | cells_daf |   5.24 ms |   6.51 ms | 0.80× | 2.00× |
| julia_queries_014 | light | cells_daf |   1.19 ms |   1.51 ms | 0.79× | 2.00× |
| julia_queries_012 | light | cells_daf |   1.19 ms |   1.56 ms | 0.76× | 2.00× |
| julia_queries_048 | light | cells_daf |   1.52 ms |   2.40 ms | 0.64× | 2.00× |
| chain_read_vector | chain | chain_triple | 675.92 µs |   1.14 ms | 0.59× | 2.00× |
| grouped_g3_mean_1000 | grouped | big_sparse |   1.12 s |   2.07 s | 0.54× | 1.20× |
| julia_queries_006 | light | cells_daf | 235.04 µs | 557.82 µs | 0.42× | 2.00× |
| julia_queries_005 | light | cells_daf | 243.14 µs | 622.79 µs | 0.39× | 2.00× |
| kernel_sum_col | kernel | big_sparse | 181.31 ms | 629.92 ms | 0.29× | 1.20× |
| kernel_mean_col | kernel | big_sparse | 179.88 ms | 651.26 ms | 0.28× | 1.20× |
| kernel_max_col | kernel | big_sparse | 171.69 ms | 684.94 ms | 0.25× | 1.20× |
| kernel_mode_row | kernel | big_sparse | 882.40 ms |   6.29 s | 0.14× | 1.20× |
| grouped_g3_max_100 | grouped | big_sparse | 258.93 ms |   2.09 s | 0.12× | 1.20× |
| grouped_g3_sum_100 | grouped | big_sparse | 245.46 ms |   2.12 s | 0.12× | 1.20× |
| grouped_g3_mean_100 | grouped | big_sparse | 229.18 ms |   2.15 s | 0.11× | 1.20× |
| grouped_g2_mean_100 | grouped | big_sparse | 291.93 ms |   2.93 s | 0.10× | 1.20× |
| grouped_g2_sum_100 | grouped | big_sparse | 292.12 ms |   2.93 s | 0.10× | 1.20× |
| grouped_g2_mean_1000 | grouped | big_sparse | 368.18 ms |   3.90 s | 0.09× | 1.20× |
| kernel_geomean_row | kernel | big_sparse | 246.08 ms |   3.45 s | 0.07× | 1.20× |
| kernel_quantile_row | kernel | big_sparse | 209.17 ms |   3.23 s | 0.06× | 1.20× |
| kernel_mean_row | kernel | big_sparse | 172.33 ms |   2.74 s | 0.06× | 1.20× |
| kernel_sum_row | kernel | big_sparse | 171.69 ms |   2.76 s | 0.06× | 1.20× |
| kernel_median_row | kernel | big_sparse | 210.96 ms |   3.53 s | 0.06× | 1.20× |
| kernel_max_row | kernel | big_sparse | 174.35 ms |   2.94 s | 0.06× | 1.20× |
| grouped_g2_max_100 | grouped | big_sparse | 170.00 ms |   3.23 s | 0.05× | 1.20× |
| kernel_std_row | kernel | big_sparse | 176.63 ms |   5.81 s | 0.03× | 1.20× |
| kernel_var_row | kernel | big_sparse | 174.54 ms |   5.79 s | 0.03× | 1.20× |

## Julia N/A (R-only or DAF.jl gap)

_(none)_

