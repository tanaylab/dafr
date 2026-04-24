# Axis-contract record.

Builds an axis specification record for use in
[`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md)'s
`axes` argument.

## Usage

``` r
axis_contract(name, expectation, description)
```

## Arguments

- name:

  Axis name.

- expectation:

  One of the
  [expectation-constants](https://tanaylab.github.io/dafr/reference/expectation-constants.md)
  (e.g.
  [RequiredInput](https://tanaylab.github.io/dafr/reference/expectation-constants.md),
  [CreatedOutput](https://tanaylab.github.io/dafr/reference/expectation-constants.md)).

- description:

  Free-text description (character scalar).

## Value

A list record with class `"dafr_axis_contract"`.

## See also

[`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md),
[`tensor_contract()`](https://tanaylab.github.io/dafr/reference/tensor_contract.md),
[expectation-constants](https://tanaylab.github.io/dafr/reference/expectation-constants.md)

## Examples

``` r
axis_contract("cell", RequiredInput, "per-cell axis")
#> $kind
#> [1] "axis"
#> 
#> $name
#> [1] "cell"
#> 
#> $expectation
#> [1] "RequiredInput"
#> 
#> $description
#> [1] "per-cell axis"
#> 
#> attr(,"class")
#> [1] "dafr_axis_contract"
```
