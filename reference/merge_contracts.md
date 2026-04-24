# Merge two contracts.

Mirrors Julia's `Contract |> Contract`. `left` is treated as the earlier
stage (upstream) and `right` as the later stage (downstream);
expectations and element types are resolved accordingly and descriptions
must match for overlapping axes / properties.

## Usage

``` r
merge_contracts(left, right)
```

## Arguments

- left, right:

  Two
  [`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md)
  objects to merge.

## Value

A new
[`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md)
combining `left` and `right`.

## Examples

``` r
upstream <- Contract(
    axes = list(cell = list(RequiredInput, "per-cell axis")),
    data = list(contract_vector("cell", "donor",
        RequiredInput, "character", "donor id"))
)
downstream <- Contract(
    axes = list(cell = list(RequiredInput, "per-cell axis")),
    data = list(contract_vector("cell", "score",
        CreatedOutput, "numeric", "computed score"))
)
merged <- merge_contracts(upstream, downstream)
length(merged@data)
#> [1] 2
```
