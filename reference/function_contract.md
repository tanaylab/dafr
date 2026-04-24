# Retrieve the contract bound to a wrapped computation.

Mirror of Julia `function_contract(fn)`. Errors if `fn` was not wrapped
by
[`computation()`](https://tanaylab.github.io/dafr/reference/computation.md)
in this session.

## Usage

``` r
function_contract(fn)
```

## Arguments

- fn:

  A function returned by
  [`computation()`](https://tanaylab.github.io/dafr/reference/computation.md).

## Value

A `Contract`.

## See also

[`computation()`](https://tanaylab.github.io/dafr/reference/computation.md).

## Examples

``` r
c <- Contract()
w <- computation("demo", c, function(daf) daf)
identical(function_contract(w), c)
#> [1] TRUE
```
