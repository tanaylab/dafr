# Convert h5ad file to a Daf object

View AnnData as a Daf data set, specifically using a MemoryDaf

## Usage

``` r
h5ad_as_daf(
  h5ad,
  name = NULL,
  obs_is = NULL,
  var_is = NULL,
  X_is = NULL,
  unsupported_handler = WARN_HANDLER
)
```

## Arguments

- h5ad:

  Path to the h5ad file

- name:

  Optional name for the Daf object

- obs_is:

  Optional name for the observation axis

- var_is:

  Optional name for the variable axis

- X_is:

  Optional name for the main matrix

- unsupported_handler:

  How to handle unsupported features (one of IGNORE_HANDLER,
  WARN_HANDLER, or ERROR_HANDLER)

## Value

A Daf object

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/anndata_format.html#anndata_as_daf)
for details. Note that this function works with h5ad file paths, as the
Julia AnnData object comes from the Muon.jl package and is not
compatible with R/python anndata implementations.
