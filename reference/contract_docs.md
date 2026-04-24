# Render a [`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md) as human-readable documentation.

Returns a single character string describing the axes and data entries.
`format = "markdown"` renders as pipe-delimited tables;
`format = "text"` uses indented lines.

## Usage

``` r
contract_docs(contract, format = c("markdown", "text"))
```

## Arguments

- contract:

  A
  [`Contract()`](https://tanaylab.github.io/dafr/reference/Contract.md).

- format:

  One of `"markdown"` or `"text"`.

## Value

Character scalar.

## See also

[`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md),
[`verify_contract()`](https://tanaylab.github.io/dafr/reference/verify_contract.md)

## Examples

``` r
c <- create_contract(
    axes = list(axis_contract("cell", RequiredInput, "per-cell axis")),
    vectors = list(contract_vector("cell", "donor", RequiredInput,
        "character", "donor id"))
)
cat(contract_docs(c), "\n")
#> ## Axes
#> 
#> name | expectation | description
#> ---- | ----------- | -----------
#> cell | RequiredInput | per-cell axis
#> 
#> ## Data
#> 
#> key | kind | expectation | description
#> --- | ---- | ----------- | -----------
#> cell / donor | vector | RequiredInput | donor id 
```
