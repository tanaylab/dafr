# Parallel `log(x + eps) / log(base)` on dense numeric input.

Mirrors the fast-path that `% Log` takes inside the query evaluator, but
callable directly on an in-memory numeric matrix or vector without
wrapping the input in a
[`memory_daf()`](https://tanaylab.github.io/dafr/reference/memory_daf.md).
Intended for downstream packages whose hot paths already have the matrix
materialised in R (e.g. a session-level cache) and want to avoid the
single-threaded `log(x + eps, base)` the query fallback would otherwise
incur.

## Usage

``` r
fast_log(
  x,
  eps = 0,
  base = exp(1),
  threshold = dafr_opt("dafr.kernel_threshold")
)
```

## Arguments

- x:

  Dense numeric matrix or numeric vector. Integers are silently promoted
  to double.

- eps:

  Non-negative scalar added inside the log. Default 0.

- base:

  Positive scalar. Default `exp(1)`.

- threshold:

  Parallel-for threshold: enable OpenMP only when
  `length(x) >= threshold`. Defaults to
  `dafr_opt("dafr.kernel_threshold")`.

## Value

A numeric matrix or vector matching the shape of `x`. Names and dimnames
are preserved.

## Details

`base` values of 2, 10, and `exp(1)` dispatch to `std::log2`,
`std::log10`, and `std::log` respectively — matching R's preference for
the more accurate form over `log(x) / log(base)`. Result is equal to
`log(x + eps, base)` to machine precision (verified by the
query-julia-compat parity suite).

## See also

[`top_k_per_col()`](https://tanaylab.github.io/dafr/reference/top_k_per_col.md)
for a similarly scoped OpenMP primitive.

## Examples

``` r
v <- c(1, 10, 100, 1000)
fast_log(v, base = 10)
#> [1] 0 1 2 3

m <- matrix(runif(12, 0.1, 5), nrow = 3, ncol = 4)
fast_log(m, eps = 1e-5, base = 2)
#>           [,1]       [,2]      [,3]        [,4]
#> [1,] -1.012503 -0.2003633 1.3443281  1.95805029
#> [2,]  2.066345 -2.8754891 0.6039475  2.13275885
#> [3,]  1.605844  1.2541938 1.8840632 -0.06307895
```
