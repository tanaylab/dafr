# Per-axis version counter.

Returns the monotonic counter for `axis` on `daf`. Incremented on
`add_axis` / `delete_axis`. Returns `0L` if the axis has never been
mutated (including non-existent axes, to match wrapper semantics).

## Usage

``` r
axis_version_counter(daf, axis)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- axis:

  Axis name (character scalar).

## Value

`integer(1)`.

## See also

[`vector_version_counter()`](https://tanaylab.github.io/dafr/reference/vector_version_counter.md),
[`matrix_version_counter()`](https://tanaylab.github.io/dafr/reference/matrix_version_counter.md)

## Examples

``` r
d <- memory_daf()
axis_version_counter(d, "cell") # 0L
#> [1] 0
add_axis(d, "cell", c("c1", "c2"))
axis_version_counter(d, "cell") # 1L
#> [1] 1
```
