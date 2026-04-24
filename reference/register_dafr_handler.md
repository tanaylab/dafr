# Register a handler for an action category.

Built-in categories: `"inefficient"`. Action is one of `"ignore"`,
`"warn"`, `"error"`, or a function `function(message, ...)` invoked by
`emit_action()`.

## Usage

``` r
register_dafr_handler(category, action)
```

## Arguments

- category:

  Character scalar: the action category.

- action:

  Either a string (`"ignore"`, `"warn"`, `"error"`) or a function taking
  the message as its first argument.

## Value

Invisibly `NULL`.

## Examples

``` r
register_dafr_handler("inefficient", "ignore")
register_dafr_handler("inefficient", "warn")
```
