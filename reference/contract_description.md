# Render a contract as a human-readable multi-line string.

Intended for splicing into roxygen docstrings of functions created by
[`computation()`](https://tanaylab.github.io/dafr/reference/computation.md).
Sections rendered: axes (with expectation + description), scalars,
vectors (per axis), matrices (per axis pair).

## Usage

``` r
contract_description(contract)
```

## Arguments

- contract:

  A `Contract`.

## Value

A character scalar.

## See also

[`computation()`](https://tanaylab.github.io/dafr/reference/computation.md),
[`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md).

## Examples

``` r
c <- Contract(
    axes = list(cell = list(RequiredInput, "per-cell axis")),
    data = list(contract_vector("cell", "donor",
        RequiredInput, "character", "donor id"))
)
cat(contract_description(c))
#> Axes:
#>   cell (RequiredInput): per-cell axis
#> Vectors:
#>   cell / donor (RequiredInput, character): donor id
```
