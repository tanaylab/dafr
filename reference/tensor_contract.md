# Tensor-contract record.

Builds a tensor specification record for use in
[`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md)'s
`tensors` argument. A tensor is a 3-D structure stored as
per-main-axis-entry matrices named `<entry>_<name>` on
`(rows_axis, columns_axis)`.

## Usage

``` r
tensor_contract(
  main_axis,
  rows_axis,
  columns_axis,
  name,
  expectation,
  type,
  description
)
```

## Arguments

- main_axis:

  Axis whose entries index the tensor slices.

- rows_axis, columns_axis:

  Axis names for each per-entry matrix.

- name:

  Tensor name; individual matrices will be `<main_axis_entry>_<name>`.

- expectation:

  One of the
  [expectation-constants](https://tanaylab.github.io/dafr/reference/expectation-constants.md).

- type:

  R class name of the matrix values (e.g. `"integer"`, `"numeric"`).

- description:

  Free-text description.

## Value

A list record with `$kind = "tensor"`.

## See also

[`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md),
[`axis_contract()`](https://tanaylab.github.io/dafr/reference/axis_contract.md)

## Examples

``` r
tensor_contract("batch", "cell", "gene", "UMIs",
    RequiredInput, "integer", "per-batch UMI matrices")
#> $kind
#> [1] "tensor"
#> 
#> $main_axis
#> [1] "batch"
#> 
#> $rows_axis
#> [1] "cell"
#> 
#> $columns_axis
#> [1] "gene"
#> 
#> $name
#> [1] "UMIs"
#> 
#> $expectation
#> [1] "RequiredInput"
#> 
#> $type
#> [1] "integer"
#> 
#> $description
#> [1] "per-batch UMI matrices"
#> 
```
