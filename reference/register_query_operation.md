# Register a query operation (eltwise or reduction).

Single entry point for adding custom eltwise / reduction operations to
the dafr query DSL at runtime. Mirrors Julia's
`register_query_operation()` from `DataAxesFormats.Registry`.

## Usage

``` r
register_query_operation(kind, name, fn, overwrite = FALSE, source = NULL)
```

## Arguments

- kind:

  Either `"eltwise"` or `"reduction"`.

- name:

  Op name; must match `[A-Z][A-Za-z0-9_]*` to be a valid query token.

- fn:

  Function `function(x, ...)` where `x` is a numeric value (scalar /
  vector / matrix) and `...` collects named parameters.

- overwrite:

  Logical; replace an already-registered op of the same kind and name.

- source:

  Optional `"file:line"` string identifying where the registration
  happened. Used in the conflict error message; auto-captured from the
  caller's srcref if omitted.

## Value

Invisibly `NULL`.

## Examples

``` r
register_query_operation("eltwise", "Clamp01",
  function(x, ...) pmin(pmax(x, 0), 1),
  overwrite = TRUE)
```
