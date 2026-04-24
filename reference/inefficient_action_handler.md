# Register a handler for the `"inefficient"` action category.

Thin wrapper around `register_dafr_handler("inefficient", handler)`.
Exists to match the Julia-facade wrapper's API.

## Usage

``` r
inefficient_action_handler(handler)
```

## Arguments

- handler:

  One of
  [`ERROR_HANDLER`](https://tanaylab.github.io/dafr/reference/handler-constants.md),
  [`WARN_HANDLER`](https://tanaylab.github.io/dafr/reference/handler-constants.md),
  [`IGNORE_HANDLER`](https://tanaylab.github.io/dafr/reference/handler-constants.md),
  or a function `function(message, ...)`.

## Value

Invisibly `NULL`.

## Examples

``` r
inefficient_action_handler(IGNORE_HANDLER)
inefficient_action_handler(function(msg) message("inefficient: ", msg))
```
