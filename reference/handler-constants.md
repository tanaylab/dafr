# Inefficient-action handler constants.

String constants matching the lowercase tokens accepted by
[`register_dafr_handler()`](https://tanaylab.github.io/dafr/reference/register_dafr_handler.md).
Pass these to
[`inefficient_action_handler()`](https://tanaylab.github.io/dafr/reference/inefficient_action_handler.md)
or any registry entry that takes an action token.

## Usage

``` r
ERROR_HANDLER

WARN_HANDLER

IGNORE_HANDLER
```

## Format

An object of class `character` of length 1.

An object of class `character` of length 1.

An object of class `character` of length 1.

## Value

Character scalar (one of `"error"`, `"warn"`, `"ignore"`).

## Examples

``` r
inefficient_action_handler(WARN_HANDLER)
```
