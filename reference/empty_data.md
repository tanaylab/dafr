# Build a flat-keyed `empty` (or `types`) list for `copy_all()`.

Users can pass a plain named list in the flat-key form directly. This
helper assembles one from a more typed builder API. Use `value = ...`
for `empty` specs, or `type = ...` for `types` specs.

## Usage

``` r
empty_data(
  vectors = list(),
  matrices = list(),
  tensors = list(),
  scalars = list()
)
```

## Arguments

- vectors:

  List of `list(axis, name, value/type)` records.

- matrices:

  List of `list(rows_axis, columns_axis, name, value/type)`.

- tensors:

  List of `list(main_axis, rows_axis, columns_axis, name, value/type)`.

- scalars:

  List of `list(name, value/type)` (typically used with `types`; scalars
  have no notion of `empty`).

## Value

A named list with flat string keys.

## Examples

``` r
empty_data(
    vectors  = list(list(axis = "cell", name = "age", value = 0L)),
    matrices = list(list(rows_axis = "cell", columns_axis = "gene",
                         name = "UMIs", value = 0))
)
#> $`cell|age`
#> [1] 0
#> 
#> $`cell|gene|UMIs`
#> [1] 0
#> 
```
