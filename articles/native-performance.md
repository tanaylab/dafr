# Native performance and mmap readers

## Why native?

dafr is a pure R + C++ port of the Julia `DataAxesFormats.jl` package.
Compared to the Julia-facade wrapper, native has:

- No JuliaCall copy tax on cross-language boundaries.
- mmap-backed reads for vectors and sparse matrices (no double-buffer).
- OpenMP-parallel query kernels (Sum, Mean, Var, Mode, Quantile, …).
- User-extensible op registry (`register_eltwise`,
  `register_reduction`).

## Mmap readers

When a FilesDaf is opened read-only, vectors and sparse matrices are
mmap’d — the OS maps the on-disk file into process memory without
copying.

``` r
fd <- files_daf("/path/to/daf", mode = "r")
x <- get_vector(fd, "cell", "donor")   # mmap'd — no allocation
```

Low-level mmap constructors are also exported for advanced uses:
`mmap_dgCMatrix`, `mmap_int`, `mmap_lgl`, `mmap_real`.

## Parallel kernels

Reductions above a size threshold dispatch to OpenMP. The threshold is
controlled via `options(dafr.kernel_threshold = N)` where N is the
minimum element count for parallel execution.

``` r
options(dafr.kernel_threshold = 1e5)   # default 1e6
```

## Bake-off headline (2026-04-22 post-9c)

On a metacell-scale fixture (100k x 5k x 0.02 density):

| Op       |   Wall |
|----------|-------:|
| Sum      |  28 ms |
| Var      |  26 ms |
| Mode     | 108 ms |
| Quantile |  44 ms |

See `dev/benchmarks/2026-04-22-post-slice-9c/` for full methodology.
