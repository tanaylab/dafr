# Return the name of a `DafReader`.

Asserts that `x` is a
[DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md) and
returns its `name` property (the string passed to the constructor).

## Usage

``` r
daf_name(x)
```

## Arguments

- x:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

## Value

Character scalar.

## See also

[`is_daf()`](https://tanaylab.github.io/dafr/reference/is_daf.md),
[`read_only()`](https://tanaylab.github.io/dafr/reference/read_only.md)

## Examples

``` r
daf_name(memory_daf(name = "hello"))
#> [1] "hello"
```
