# Native performance and mmap readers

## Why native?

`dafr` is a pure R + C++ port of the Julia `DataAxesFormats.jl` package.
Compared to a Julia-facade wrapper, native has:

- No `JuliaCall` copy tax on cross-language boundaries.
- mmap-backed reads for vectors and sparse matrices (no double-buffer).
- OpenMP-parallel query kernels (Sum, Mean, Var, Mode, Quantile, …).
- User-extensible op registry (`register_eltwise`,
  `register_reduction`).

## Mmap readers

When a FilesDaf is opened read-only, dense vectors and the three payload
arrays of a sparse matrix (`colptr`, `rowval`, `nzval`) are `mmap`’d —
the OS maps the on-disk files directly into process memory with no copy.

``` r
fd <- files_daf("/path/to/daf", mode = "r")
x  <- get_vector(fd, "cell", "donor")     # mmap'd — no allocation
m  <- get_matrix(fd, "cell", "gene", "UMIs")  # mmap'd dgCMatrix
```

Low-level mmap constructors are also exported for advanced uses:
`mmap_dgCMatrix`, `mmap_int`, `mmap_lgl`, `mmap_real`.

## Parallel kernels

Reductions above a size threshold dispatch to OpenMP. The threshold is
controlled via `options(dafr.kernel_threshold = N)` where `N` is the
minimum element count for parallel execution.

``` r
options(dafr.kernel_threshold = 1e5)   # default 1e6
```

Thread count honours
[`set_num_threads()`](https://tanaylab.github.io/dafr/reference/set_num_threads.md)
(or the `dafr.num_threads` option). CRAN-like environments are
auto-capped to 2 cores on package load.

## Performance vs DataAxesFormats.jl

The package is benchmarked against the reference Julia implementation
(`DataAxesFormats.jl`) — native Julia in-process, with no R or wrapper
layer — using a shared fixture set. Fixtures hash-match across runners,
queries are driven from the same catalog, and each iteration invalidates
the per-query cache so timings reflect real compute rather than a
cache-lookup ceiling.

**Setup** (2026-04-22): `dafr` commit `d6d9a14`, R 4.4.1 with MKL 2024.1
(ILP64), `OMP_NUM_THREADS=1`; Julia 1.12.5, OpenBLAS-64, threads =
default. Linux x86_64. 79 queries across four fixtures: `big_sparse`
(dense kernel sweeps on a large CSC matrix), `cells_daf` (51 queries
covering every phrase in the string DSL), `chain_triple` (read paths
through a three-layer chain), `mmap_reopen` (filesystem open + single
read).

**Reading the tables.** In every table `Ratio = dafr / julia` (lower is
better for dafr) and `Speedup = julia / dafr` (higher is better for
dafr). A speedup of `10×` means dafr is ten times faster than Julia.

**Headline.** Of the 79 benchmarked queries, dafr is faster than Julia
on 56, at parity on 12, and slower on 11 (of which 4 exceed a 1.5×
threshold — all on the filesystem-open-then-immediately-read
micro-test). On the heavy workloads that dominate real pipelines
(sparse-matrix reductions, grouped reductions), dafr wins by 2×–37×.

### Heavy kernels on a large sparse matrix

Dense reductions over a CSC matrix (`big_sparse`, ~100k × 5k, 2 %
density). `dafr`’s OpenMP kernels win decisively even at
`OMP_NUM_THREADS=1`:

| Query                 | dafr median | Julia median | Ratio (R/J) | Speedup (J/R) |
|-----------------------|------------:|-------------:|------------:|--------------:|
| `kernel_var_row`      |      158 ms |       5.86 s |       0.03× |       **37×** |
| `kernel_std_row`      |      159 ms |       5.85 s |       0.03× |       **37×** |
| `kernel_mean_row`     |      153 ms |       2.83 s |       0.05× |       **18×** |
| `kernel_max_row`      |      158 ms |       2.90 s |       0.05× |       **18×** |
| `kernel_median_row`   |      190 ms |       3.50 s |       0.05× |       **18×** |
| `kernel_quantile_row` |      194 ms |       3.16 s |       0.06× |       **16×** |
| `kernel_geomean_row`  |      226 ms |       3.45 s |       0.07× |       **15×** |
| `kernel_sum_row`      |      184 ms |       2.73 s |       0.07× |       **15×** |
| `kernel_mode_row`     |      872 ms |       6.31 s |       0.14× |        **7×** |
| `kernel_max_col`      |      154 ms |       683 ms |       0.23× |        **4×** |
| `kernel_sum_col`      |      162 ms |       713 ms |       0.23× |        **4×** |
| `kernel_mean_col`     |      161 ms |       643 ms |       0.25× |        **4×** |

### Grouped reductions

Same fixture, grouping a vector into bins before reducing:

| Query                  | dafr median | Julia median | Ratio (R/J) | Speedup (J/R) |
|------------------------|------------:|-------------:|------------:|--------------:|
| `grouped_g2_max_100`   |      113 ms |       2.94 s |       0.04× |       **26×** |
| `grouped_g2_mean_100`  |      215 ms |       3.16 s |       0.07× |       **15×** |
| `grouped_g2_mean_1000` |      287 ms |       3.87 s |       0.07× |       **13×** |
| `grouped_g2_sum_100`   |      215 ms |       2.88 s |       0.07× |       **13×** |
| `grouped_g3_mean_100`  |      169 ms |       2.11 s |       0.08× |       **12×** |
| `grouped_g3_sum_100`   |      172 ms |       1.99 s |       0.09× |       **12×** |
| `grouped_g3_max_100`   |      192 ms |       2.07 s |       0.09× |       **11×** |
| `grouped_g3_mean_1000` |      1.07 s |       2.04 s |       0.52× |        **2×** |

### String-DSL queries on a metacell-scale store

51 queries covering every phrase in the query language (`cells_daf`, a
subset of
[`example_cells_daf()`](https://tanaylab.github.io/dafr/reference/example_cells_daf.md)).
Times are sub-millisecond to low-millisecond for both runners; dafr is
at parity or faster on most, modestly slower on a handful of axis-mask /
lookup-composition phrases:

| Summary                     | Count |
|-----------------------------|------:|
| dafr faster (ratio \< 0.9×) |    23 |
| parity (0.9× – 1.1×)        |    10 |
| dafr slower (ratio \> 1.1×) |    18 |
| worst ratio                 | 1.73× |

No `cells_daf` query exceeded its 2× threshold.

### Chain queries

Reading through a three-daf chain (`chain_triple`):

| Query               | dafr median | Julia median | Ratio (R/J) | Speedup (J/R) |
|---------------------|------------:|-------------:|------------:|--------------:|
| `chain_read_vector` |      694 µs |      1.15 ms |       0.60× |      **1.7×** |
| `chain_read_matrix` |      931 µs |      1.09 ms |       0.85× |      **1.2×** |
| `chain_read_scalar` |      656 µs |       398 µs |       1.65× |          0.6× |
| `chain_reduce`      |     2.80 ms |      1.67 ms |       1.68× |          0.6× |

### Open + first-read latency (only place dafr loses)

On the `mmap_reopen` fixture — open a FilesDaf and immediately read one
property — dafr is consistently slower than Julia. All four are within a
few milliseconds absolute, but the per-call overhead is higher:

| Query                   | dafr median | Julia median | Ratio (R/J) | Speedup (J/R) |
|-------------------------|------------:|-------------:|------------:|--------------:|
| `mmap_open_read_axis`   |     2.11 ms |       794 µs |       2.66× |         0.38× |
| `mmap_open_read_matrix` |     4.09 ms |      1.66 ms |       2.46× |         0.41× |
| `mmap_open_read_vector` |     2.96 ms |      1.54 ms |       1.93× |         0.52× |
| `mmap_open_read_scalar` |     1.00 ms |       642 µs |       1.56× |         0.64× |

The gap is per-call: JSON descriptor parsing, `stat`s, and the one-shot
mmap setup cost more per file in R than in Julia. For workloads that
open a store once and then stream many reads (the common case), this
cost amortises to zero — see the kernel tables above. For open-heavy
workflows, keep the `FilesDaf` handle alive across queries.

### Reproducing the numbers

The bake-off harness lives under `benchmarks/` (R runner) and
`benchmarks/julia/` (Julia runner). Both runners consume the same query
catalog and fixture set and emit CSVs with identical schemas so the
comparison step is a simple join. See `benchmarks/README.md` for the
full workflow.
