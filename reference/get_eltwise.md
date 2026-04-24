# Retrieve a registered eltwise operation by name.

Looks up an eltwise operation previously stored via
[`register_eltwise()`](https://tanaylab.github.io/dafr/reference/register_eltwise.md).
Raises if the name is not registered.

## Usage

``` r
get_eltwise(name)
```

## Arguments

- name:

  Op name (character scalar).

## Value

The stored function.

## Examples

``` r
registered_eltwise()      # "Abs" is among the built-ins
#> [1] "Abs"         "Clamp"       "Convert"     "Exp"         "Fraction"   
#> [6] "Log"         "Round"       "Significant" "Sqrt"       
fn <- get_eltwise("Abs")
fn(c(-1, 2, -3))
#> [1] 1 2 3
```
