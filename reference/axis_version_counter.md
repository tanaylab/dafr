# Per-axis version counter.

Returns the monotonic counter for `axis` on `daf`. Mirrors Julia DAF:
incremented every time `delete_axis` is called (NOT on `add_axis`).
Returns `0L` if `axis` has never been deleted (including non-existent
axes, to match wrapper semantics).

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
# Mirrors readers.jl jldoctest at line 246.
m <- example_metacells_daf()
axis_version_counter(m, "type")           # 0L
#> [1] 0
delete_axis(m, "type")
add_axis(m, "type", c("Foo", "Bar", "Baz"))
axis_version_counter(m, "type")           # 1L
#> [1] 1
```
