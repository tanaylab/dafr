# Register an eltwise operation.

Stores a user-defined element-wise function under the given name so that
it can be retrieved by
[`get_eltwise()`](https://tanaylab.github.io/dafr/reference/get_eltwise.md)
and invoked during query evaluation.

## Usage

``` r
register_eltwise(name, fn, overwrite = FALSE)
```

## Arguments

- name:

  Op name (character scalar, matches token in query strings).

- fn:

  Function `function(x, ...)` where `x` is a numeric vector or matrix
  (eltwise ops preserve shape) and `...` collects named parameters.

- overwrite:

  Logical scalar; set to `TRUE` to replace an already- registered
  operation.

## Value

Invisibly `NULL`.

## Examples

``` r
register_eltwise("Clamp01", function(x, ...) pmin(pmax(x, 0), 1),
                 overwrite = TRUE)
registered_eltwise()
#>  [1] "Abs"         "Clamp"       "Clamp01"     "Convert"     "Exp"        
#>  [6] "Fraction"    "Log"         "Round"       "Significant" "Sqrt"       
```
