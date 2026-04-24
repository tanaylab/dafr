# Control OpenMP thread count for dafr kernels.

`set_num_threads(n)` caps the number of OpenMP threads used by every
subsequent kernel dispatch. `get_num_threads()` reports the current cap.

## Usage

``` r
set_num_threads(n)

get_num_threads()
```

## Arguments

- n:

  Positive integer; the maximum number of OpenMP threads to use. Values
  \< 1 are clamped to 1.

## Value

`set_num_threads` invisibly returns `n`. `get_num_threads` returns the
current OpenMP max-threads value.

## Details

dafr's kernels default to the system's OpenMP maximum (typically
`OMP_NUM_THREADS` or the machine's CPU count). CRAN policy allows at
most two cores for package checks, and `.onLoad()` detects CRAN-like
environments (via `_R_CHECK_LIMIT_CORES_` or an `OMP_THREAD_LIMIT` ≤ 2)
and caps to 2 automatically. Interactive users can raise or lower the
cap at any time.

## Examples

``` r
old <- get_num_threads()
set_num_threads(1L)   # single-thread mode
get_num_threads()     # 1
#> [1] 1
set_num_threads(old)  # restore
```
