# Contract entry constructors

Build records describing a scalar/vector/matrix that a computation
consumes or produces. Records are stored in the `data` slot of a
[`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md)
and consumed by
[`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md).

## Usage

``` r
contract_scalar(name, expectation, type, description)

contract_vector(axis, name, expectation, type, description)

contract_matrix(rows_axis, columns_axis, name, expectation, type, description)
```

## Arguments

- name:

  Property name (character scalar).

- expectation:

  One of the
  [expectation-constants](https://tanaylab.github.io/dafr/reference/expectation-constants.md)
  (e.g. `RequiredInput`, `CreatedOutput`).

- type:

  R class name (e.g. `"integer"`, `"numeric"`, `"character"`).

- description:

  Free-text description (character scalar).

- axis:

  Axis name for a vector property.

- rows_axis, columns_axis:

  Axis names for a matrix property.

## Value

A list record with `$kind`, `$name`, `$expectation`, `$type`,
`$description`, and kind-specific axis fields.

## Examples

``` r
s <- contract_scalar("organism", RequiredInput, "character", "species name")
v <- contract_vector("cell", "donor", RequiredInput, "character", "donor id")
m <- contract_matrix("cell", "gene", "UMIs", RequiredInput, "integer", "UMI counts")
# These records go into Contract(data = ...):
c <- Contract(
    axes = list(cell = list(RequiredInput, "per-cell axis"),
                gene = list(RequiredInput, "per-gene axis")),
    data = list(s, v, m)
)
length(c@data)
#> [1] 3
```
