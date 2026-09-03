# One base repository of a [`complete_chain()`](https://tanaylab.github.io/dafr/reference/complete_chain.md).

Pairs a `DafReader` with the
[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md)
parameters to apply to it, which restrict it to a subset of its data
and/or rename that data. Pass a plain `DafReader` to
[`complete_chain()`](https://tanaylab.github.io/dafr/reference/complete_chain.md)
instead wherever the whole of it is used, which is the common case.

## Usage

``` r
base_daf(daf, axes = NULL, data = NULL)
```

## Arguments

- daf:

  A `DafReader` on disk.

- axes, data:

  Optional
  [`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) axes
  / data spec.

## Value

A `dafr_base_daf` spec.

## Examples

``` r
tmp <- tempfile()
dir.create(tmp)
d <- files_daf(tmp, name = "d", mode = "w+")
spec <- base_daf(d, axes = list(list("cell", "=")))
class(spec)
#> [1] "dafr_base_daf"
```
