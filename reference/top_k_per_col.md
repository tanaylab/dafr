# Per-column bounded top-K with OpenMP.

Returns the `k` largest (or largest-by-magnitude if `use_abs = TRUE`)
values of each column together with their row indices. Input may be a
dense numeric matrix or a `Matrix::dgCMatrix`; the sparse path scans
only the nonzero entries per column so it scales with `nnz`, not
`nrow * ncol`.

## Usage

``` r
top_k_per_col(
  x,
  k,
  use_abs = FALSE,
  threshold = dafr_opt("dafr.kernel_threshold")
)
```

## Arguments

- x:

  Numeric dense matrix, or `Matrix::dgCMatrix`.

- k:

  Positive integer - number of entries to return per column.

- use_abs:

  Rank by `abs(value)` when `TRUE`, else by raw value. Values returned
  are always signed.

- threshold:

  Parallel-for threshold: enable OpenMP across columns only when
  `ncol(x) >= threshold`. Defaults to
  `dafr_opt("dafr.kernel_threshold")`.

## Value

A list with two `k`-by-`ncol` matrices:

- `indices`:

  Integer matrix of row indices (1-based). `NA_integer_` in padding
  slots.

- `values`:

  Double matrix of signed values. `NA_real_` in padding slots.

Column names of `x` are preserved on the output columns; row-name lookup
is the caller's responsibility (use `rownames(x)[indices]`).

## Details

Rows missing a valid comparison score (`NA` / `NaN` in dense input;
absent from the column's nonzero entries in the CSC input) are excluded
from consideration. When fewer than `k` entries qualify, the tail of
each output column is NA-padded.

## Examples

``` r
m <- matrix(c(1, -5, 3, 2, 4, -2, 7, -1, 6, 5, 8, -3), nrow = 4, ncol = 3)
top_k_per_col(m, k = 2)
#> $indices
#>      [,1] [,2] [,3]
#> [1,]    3    3    3
#> [2,]    4    1    1
#> 
#> $values
#>      [,1] [,2] [,3]
#> [1,]    3    7    8
#> [2,]    2    4    6
#> 
top_k_per_col(m, k = 2, use_abs = TRUE)
#> $indices
#>      [,1] [,2] [,3]
#> [1,]    2    3    3
#> [2,]    3    1    1
#> 
#> $values
#>      [,1] [,2] [,3]
#> [1,]   -5    7    8
#> [2,]    3    4    6
#> 

sm <- Matrix::sparseMatrix(
    i = c(1, 3, 2, 4, 1, 3),
    j = c(1, 1, 2, 2, 3, 3),
    x = c(5, 2, 7, 1, 4, 8),
    dims = c(4, 3)
)
top_k_per_col(sm, k = 2)
#> $indices
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    3    4    1
#> 
#> $values
#>      [,1] [,2] [,3]
#> [1,]    5    7    8
#> [2,]    2    1    4
#> 
```
