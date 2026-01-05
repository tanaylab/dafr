# Set the handler for inefficient matrix access

Specify the handler to use when accessing a matrix in an inefficient way
("against the grain"). Returns the previous handler as a string.

## Usage

``` r
inefficient_action_handler(handler)
```

## Arguments

- handler:

  The handler to use, one of "IgnoreHandler", "WarnHandler", or
  "ErrorHandler"

## Value

The previous handler name
