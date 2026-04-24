# Test whether an object is a `DafReader`.

Non-throwing predicate for any of the S7 class descendants (`MemoryDaf`,
`FilesDaf`, `ReadOnlyChainDaf`, `WriteChainDaf`, `ViewDaf`,
`ContractDaf`, ...).

## Usage

``` r
is_daf(x)
```

## Arguments

- x:

  Any R object.

## Value

`TRUE` if `x` inherits from
[DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md),
else `FALSE`.

## Examples

``` r
is_daf(memory_daf())
#> [1] TRUE
is_daf(NULL)
#> [1] FALSE
```
