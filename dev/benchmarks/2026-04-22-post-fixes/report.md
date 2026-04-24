# Bake-off: R vs Julia (2026-04-22)

Ratio = dafr_median / julia_median. Higher = dafr slower. A breach is
`ratio > threshold` for the query's tier.

## Headers

```
# runner: R
# dafr_commit: 365c1a08117af55d010b4e81509b33f0af75f355
# R_version: 4.4.1
# platform: x86_64-pc-linux-gnu
# OMP_NUM_THREADS: 1
# BLAS: /net/mraid20/ifs/wisdom/tanay_lab/data/users/eladch/tools/CO8/mkl/2024.1/lib/libmkl_gf_lp64.so.2
# fixtures: big_sparse=112972aa279bf184; cells_daf=892948c6ec288d6c; chain_triple=9f75f5a89bf4ba5f; mmap_reopen=892948c6ec288d6c
```

```
# runner: Julia
# dafr_commit: 365c1a08117af55d010b4e81509b33f0af75f355
# julia_version: 1.12.5
# platform: x86_64-conda-linux-gnu
# JULIA_NUM_THREADS: default
# BLAS: LBTConfig([ILP64] libopenblas64_.so)
# fixtures: big_sparse=112972aa279bf184; cells_daf=892948c6ec288d6c; chain_triple=9f75f5a89bf4ba5f; mmap_reopen=892948c6ec288d6c
```

## BREACHED (7)

| query_id | category | fixture | R (median) | J (median) | ratio | threshold |
|---|---|---|---|---|---|---|
| julia_queries_026 | light | cells_daf |  18.28 ms |   6.10 ms | 3.00× | 2.00× |
| mmap_open_read_axis | mmap | mmap_reopen |   2.11 ms | 832.69 µs | 2.53× | 1.50× |
| mmap_open_read_matrix | mmap | mmap_reopen |   4.02 ms |   1.68 ms | 2.40× | 1.50× |
| julia_queries_028 | light | cells_daf |  19.93 ms |   8.76 ms | 2.27× | 2.00× |
| julia_queries_043 | light | cells_daf |  10.30 ms |   4.76 ms | 2.16× | 2.00× |
| julia_queries_047 | light | cells_daf |  10.78 ms |   5.03 ms | 2.14× | 2.00× |
| mmap_open_read_vector | mmap | mmap_reopen |   2.89 ms |   1.55 ms | 1.86× | 1.50× |

## Within threshold (72)

| query_id | category | fixture | R (median) | J (median) | ratio | threshold |
|---|---|---|---|---|---|---|
| julia_queries_023 | light | cells_daf |   4.81 ms |   2.84 ms | 1.69× | 2.00× |
| julia_queries_024 | light | cells_daf |   4.74 ms |   2.83 ms | 1.68× | 2.00× |
| chain_reduce | chain | chain_triple |   2.79 ms |   1.70 ms | 1.64× | 2.00× |
| chain_read_scalar | chain | chain_triple | 642.38 µs | 399.88 µs | 1.61× | 2.00× |
| julia_queries_015 | light | cells_daf |   3.22 ms |   2.10 ms | 1.54× | 2.00× |
| julia_queries_016 | light | cells_daf |   2.61 ms |   1.71 ms | 1.53× | 2.00× |
| julia_queries_007 | light | cells_daf | 598.52 µs | 412.31 µs | 1.45× | 2.00× |
| mmap_open_read_scalar | mmap | mmap_reopen | 983.96 µs | 685.03 µs | 1.44× | 1.50× |
| julia_queries_027 | light | cells_daf |  10.50 ms |   7.47 ms | 1.40× | 2.00× |
| julia_queries_022 | light | cells_daf |   2.89 ms |   2.48 ms | 1.17× | 2.00× |
| julia_queries_021 | light | cells_daf |   2.92 ms |   2.50 ms | 1.17× | 2.00× |
| julia_queries_018 | light | cells_daf |   1.38 ms |   1.25 ms | 1.11× | 2.00× |
| julia_queries_003 | light | cells_daf | 547.21 µs | 495.53 µs | 1.10× | 2.00× |
| julia_queries_001 | light | cells_daf | 409.37 µs | 374.01 µs | 1.09× | 2.00× |
| julia_queries_051 | light | cells_daf |   2.08 ms |   1.91 ms | 1.09× | 2.00× |
| julia_queries_034 | light | cells_daf |   2.01 ms |   1.87 ms | 1.07× | 2.00× |
| julia_queries_017 | light | cells_daf |   1.31 ms |   1.24 ms | 1.06× | 2.00× |
| julia_queries_020 | light | cells_daf |   1.52 ms |   1.46 ms | 1.04× | 2.00× |
| julia_queries_038 | light | cells_daf |   1.92 ms |   1.86 ms | 1.03× | 2.00× |
| julia_queries_037 | light | cells_daf |   1.87 ms |   1.85 ms | 1.01× | 2.00× |
| julia_queries_033 | light | cells_daf |   1.86 ms |   1.85 ms | 1.00× | 2.00× |
| julia_queries_002 | light | cells_daf | 526.02 µs | 528.04 µs | 1.00× | 2.00× |
| julia_queries_004 | light | cells_daf | 360.87 µs | 367.00 µs | 0.98× | 2.00× |
| julia_queries_029 | light | cells_daf |   1.80 ms |   1.85 ms | 0.97× | 2.00× |
| julia_queries_036 | light | cells_daf |   1.83 ms |   1.89 ms | 0.97× | 2.00× |
| julia_queries_035 | light | cells_daf |   1.81 ms |   1.87 ms | 0.97× | 2.00× |
| julia_queries_008 | light | cells_daf | 970.31 µs |   1.01 ms | 0.96× | 2.00× |
| julia_queries_049 | light | cells_daf |   1.22 ms |   1.27 ms | 0.96× | 2.00× |
| julia_queries_040 | light | cells_daf |   2.90 ms |   3.01 ms | 0.96× | 2.00× |
| julia_queries_039 | light | cells_daf |   1.78 ms |   1.86 ms | 0.96× | 2.00× |
| julia_queries_030 | light | cells_daf |   1.80 ms |   1.90 ms | 0.95× | 2.00× |
| julia_queries_009 | light | cells_daf |   1.10 ms |   1.16 ms | 0.95× | 2.00× |
| julia_queries_044 | light | cells_daf |   5.18 ms |   5.48 ms | 0.95× | 2.00× |
| julia_queries_031 | light | cells_daf |   1.75 ms |   1.88 ms | 0.93× | 2.00× |
| julia_queries_032 | light | cells_daf |   1.78 ms |   1.92 ms | 0.92× | 2.00× |
| julia_queries_050 | light | cells_daf |   1.24 ms |   1.48 ms | 0.84× | 2.00× |
| julia_queries_011 | light | cells_daf |   1.17 ms |   1.40 ms | 0.84× | 2.00× |
| julia_queries_019 | light | cells_daf |   1.04 ms |   1.25 ms | 0.84× | 2.00× |
| chain_read_matrix | chain | chain_triple | 927.65 µs |   1.14 ms | 0.81× | 2.00× |
| julia_queries_042 | light | cells_daf |   4.25 ms |   5.25 ms | 0.81× | 2.00× |
| julia_queries_010 | light | cells_daf |   1.03 ms |   1.30 ms | 0.79× | 2.00× |
| julia_queries_046 | light | cells_daf |   4.14 ms |   5.27 ms | 0.79× | 2.00× |
| julia_queries_025 | light | cells_daf |   5.12 ms |   6.54 ms | 0.78× | 2.00× |
| julia_queries_045 | light | cells_daf |   4.04 ms |   5.19 ms | 0.78× | 2.00× |
| julia_queries_014 | light | cells_daf |   1.16 ms |   1.54 ms | 0.76× | 2.00× |
| julia_queries_041 | light | cells_daf |   4.20 ms |   5.67 ms | 0.74× | 2.00× |
| julia_queries_013 | light | cells_daf |   1.23 ms |   1.67 ms | 0.74× | 2.00× |
| julia_queries_012 | light | cells_daf |   1.14 ms |   1.55 ms | 0.73× | 2.00× |
| chain_read_vector | chain | chain_triple | 691.06 µs |   1.17 ms | 0.59× | 2.00× |
| julia_queries_048 | light | cells_daf |   1.45 ms |   2.55 ms | 0.57× | 2.00× |
| grouped_g3_mean_1000 | grouped | big_sparse |   1.06 s |   2.06 s | 0.52× | 1.20× |
| julia_queries_006 | light | cells_daf | 232.55 µs | 558.16 µs | 0.42× | 2.00× |
| julia_queries_005 | light | cells_daf | 244.12 µs | 664.82 µs | 0.37× | 2.00× |
| kernel_sum_col | kernel | big_sparse | 176.44 ms | 640.47 ms | 0.28× | 1.20× |
| kernel_mean_col | kernel | big_sparse | 173.38 ms | 662.36 ms | 0.26× | 1.20× |
| kernel_max_col | kernel | big_sparse | 161.39 ms | 697.48 ms | 0.23× | 1.20× |
| kernel_mode_row | kernel | big_sparse | 870.37 ms |   6.40 s | 0.14× | 1.20× |
| grouped_g3_max_100 | grouped | big_sparse | 182.26 ms |   2.15 s | 0.08× | 1.20× |
| grouped_g3_sum_100 | grouped | big_sparse | 165.31 ms |   2.04 s | 0.08× | 1.20× |
| grouped_g3_mean_100 | grouped | big_sparse | 166.04 ms |   2.13 s | 0.08× | 1.20× |
| grouped_g2_sum_100 | grouped | big_sparse | 224.80 ms |   3.00 s | 0.07× | 1.20× |
| grouped_g2_mean_100 | grouped | big_sparse | 223.66 ms |   3.04 s | 0.07× | 1.20× |
| grouped_g2_mean_1000 | grouped | big_sparse | 290.63 ms |   3.99 s | 0.07× | 1.20× |
| kernel_geomean_row | kernel | big_sparse | 236.49 ms |   3.48 s | 0.07× | 1.20× |
| kernel_quantile_row | kernel | big_sparse | 199.09 ms |   3.16 s | 0.06× | 1.20× |
| kernel_sum_row | kernel | big_sparse | 164.72 ms |   2.76 s | 0.06× | 1.20× |
| kernel_max_row | kernel | big_sparse | 167.87 ms |   2.82 s | 0.06× | 1.20× |
| kernel_mean_row | kernel | big_sparse | 164.07 ms |   2.81 s | 0.06× | 1.20× |
| kernel_median_row | kernel | big_sparse | 199.47 ms |   3.48 s | 0.06× | 1.20× |
| grouped_g2_max_100 | grouped | big_sparse | 102.43 ms |   3.03 s | 0.03× | 1.20× |
| kernel_var_row | kernel | big_sparse | 168.00 ms |   5.85 s | 0.03× | 1.20× |
| kernel_std_row | kernel | big_sparse | 166.48 ms |   5.87 s | 0.03× | 1.20× |

## Julia N/A (R-only or DAF.jl gap)

_(none)_

