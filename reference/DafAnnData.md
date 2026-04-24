# Read-only AnnData-shaped facade over a Daf.

Read-only AnnData-shaped facade over a Daf.

Read-only AnnData-shaped facade over a Daf.

## Details

Exposes a `DafReader` through the property names familiar to `anndata` /
`scanpy` / `Seurat` users: `X`, `obs`, `var`, `layers`, `uns`,
`obs_names`, `var_names`, `n_obs`, `n_vars`, `shape`. All bindings are
read-only; modifying data requires writing to the underlying `Daf`
directly.

## See also

[`as_anndata()`](https://tanaylab.github.io/dafr/reference/as_anndata.md)

## Public fields

- `daf`:

  The underlying Daf object.

- `obs_axis`:

  Name of the observations axis.

- `var_axis`:

  Name of the variables axis.

- `x_name`:

  Matrix name for `X`.

## Active bindings

- `X`:

  The primary matrix (`obs_axis` x `var_axis`).

- `obs`:

  Data frame of observation vectors.

- `var`:

  Data frame of variable vectors.

- `obs_names`:

  Character vector of observation names.

- `var_names`:

  Character vector of variable names.

- `n_obs`:

  Number of observations.

- `n_vars`:

  Number of variables.

- `shape`:

  Dimensions `c(n_obs, n_vars)`.

- `layers`:

  Named list of additional matrices (excluding `X`).

- `uns`:

  Named list of scalars.

## Methods

### Public methods

- [`DafAnnData$new()`](#method-DafAnnData-new)

- [`DafAnnData$clone()`](#method-DafAnnData-clone)

------------------------------------------------------------------------

### Method `new()`

Create a `DafAnnData` facade.

#### Usage

    DafAnnData$new(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs")

#### Arguments

- `daf`:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- `obs_axis`:

  Axis name for observations. Defaults auto-detect: `"cell"` then
  `"metacell"`.

- `var_axis`:

  Axis name for variables. Defaults to `"gene"`.

- `x_name`:

  Matrix name for `X`. Default `"UMIs"`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DafAnnData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
add_axis(d, "gene", c("g1", "g2", "g3"))
set_matrix(d, "cell", "gene", "UMIs", matrix(1:6, 2, 3))
ann <- as_anndata(d)
ann$X
#>    g1 g2 g3
#> c1  1  3  5
#> c2  2  4  6
ann$obs_names
#> [1] "c1" "c2"
```
