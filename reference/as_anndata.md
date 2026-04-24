# One-shot factory for a [DafAnnData](https://tanaylab.github.io/dafr/reference/DafAnnData.md) facade.

One-shot factory for a
[DafAnnData](https://tanaylab.github.io/dafr/reference/DafAnnData.md)
facade.

## Usage

``` r
as_anndata(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs")
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- obs_axis:

  Axis name for observations. Defaults auto-detect: `"cell"` then
  `"metacell"`.

- var_axis:

  Axis name for variables. Defaults to `"gene"`.

- x_name:

  Matrix name for `X`. Default `"UMIs"`.

## Value

A [DafAnnData](https://tanaylab.github.io/dafr/reference/DafAnnData.md)
instance.

## See also

[DafAnnData](https://tanaylab.github.io/dafr/reference/DafAnnData.md)

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
add_axis(d, "gene", c("g1", "g2"))
set_matrix(d, "cell", "gene", "UMIs", matrix(1:4, 2, 2))
ann <- as_anndata(d)
```
