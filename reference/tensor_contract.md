# Create a tensor contract specification

A tensor is a 3D data structure stored as per-entry matrices along a
main axis. For example, if the main axis is "batch", then for each batch
entry there would be a matrix named "batchN_name" for some name.

## Usage

``` r
tensor_contract(
  main_axis,
  rows_axis,
  cols_axis,
  name,
  expectation,
  dtype,
  description
)
```

## Arguments

- main_axis:

  Main axis name (the axis along which matrices are stored)

- rows_axis:

  Rows axis name for each matrix

- cols_axis:

  Columns axis name for each matrix

- name:

  Tensor name (individual matrices will be named "entry_name")

- expectation:

  One of RequiredInput, OptionalInput, GuaranteedOutput, OptionalOutput

- dtype:

  Data type (e.g., "numeric", "integer")

- description:

  Human-readable description

## Value

Tensor specification list
