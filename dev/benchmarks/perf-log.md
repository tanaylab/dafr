# Slice 9b perf log

Append a row per gap-close. Newest at top.

| date | commit | breach_id | before_ratio | after_ratio | notes |
|---|---|---|---|---|---|
| 2026-04-22 | `f7a3732` | julia_queries_{026,028,043,047} | 3.00×, 2.27×, 2.16×, 2.14× | **1.24×, 1.18×, 0.85×, 0.90×** | **CLOSED (4/4).** Dense Int-aware C++ kernels for Quantile, Mode, and grouped Min/Max; `R/query_eval.R` dispatch rewires non-grouped Quantile/Mode and G2/G3 Max/Min to new kernels. Eliminates the `storage.mode(m) <- "double"` materialization + matrixStats R-wrapper dispatch; Q043/Q047 now beat Julia outright. Remaining 4 breaches are all mmap S7-ctor floor (`mmap_open_read_{scalar,vector,matrix,axis}`) — accept-class, require FilesDaf-ctor architectural work. |
| 2026-04-22 | `365c1a0` | julia_queries_041-047 | 2.04-2.90× | Q41 0.90×, Q42 0.88×, Q43 2.00×, Q44 0.89×, Q45 0.81×, Q46 0.83×, Q47 2.01× | MOSTLY CLOSED (5/7 within threshold). Int-aware dense grouped-rowsum C++ kernel folds int->double promotion + sum-of-squares into a single accumulation pass; eliminates storage.mode materialization and m*m intermediate. Q43/Q47 (Var/Std) still at ~2.0× — residual is formula overhead (pmax/sqrt in R after kernel). |
| 2026-04-22 | `df2f57f` | mmap_open_read_scalar | 1.86× | 1.54× | PARTIAL (not fully closed; threshold 1.50×). Fast-path JSON parse for daf.json + scalar/descriptor files; jsonlite fallback. Projected 1.41× but actual ~1.53× — measurement noise ±5% on 1ms timings. |
| 2026-04-22 | `df2f57f` | mmap_open_read_{vector,matrix,axis} | 2.02-2.73× | 1.84-2.51× | PARTIAL. Same fix; 5-10% shave. Remaining gap is S7 ctor + normalizePath overhead — next fix (T11.4 Int32 rowsum kernel) will partially offset the Group A impact too. |
| 2026-04-22 | `7464df8` | julia_queries_045 | 4.0× | 2.00× | CLOSED (exactly at threshold). G3 Sum: BLAS indicator-matrix `m %*% ind` replaces `t(rowsum(t(m),gi))`. |
| 2026-04-22 | `7464df8` | julia_queries_021-024 | 2.6-4.0× | 1.18-1.79× | CLOSED. Dense Var/Std/VarN/StdN: matrixStats::colVars/rowVars * (n-1)/n replaces double-pass colMeans(m*m)-colMeans(m)^2. |
| 2026-04-22 | `d8090a2` | julia_queries_041-047 | ~4× | 2.04-2.90× | PARTIAL. Dense grouped G2/G3 rowsum fast path; Q41-47 still breached at 2.04-2.90× due to mmap ALTREP cold-open overhead (~5ms per empty_cache iteration). Warm-cache R is 0.7ms vs Julia 5ms (R faster). Root cause: `empty_cache` forces file re-open; Julia ALTREP has lower cold-open latency. |
| 2026-04-22 | `fc8e03b` | julia_queries_021-024 | 2.6-4.0× | ~1.3-1.8× | intermediate — matrixStats fix; see 7464df8 for final. |
| 2026-04-22 | `33702d0` | (perf) | — | — | Fix 3: gate log() in Acc::push to GeoMean-only. Pure waste-removal; no breach impact. |
| 2026-04-22 | `2cc1348` | (baseline) | — | — | Full 79-query baseline captured; see `dev/benchmarks/2026-04-22-baseline/report.md`. 17 breaches: 13 light (julia_queries grouped reductions + Var/Quantile/Mode reductions on sparse UMIs), 4 mmap (open+read). All big_sparse kernel, grouped G2/G3 on big_sparse, and chain queries are within tier threshold. |
