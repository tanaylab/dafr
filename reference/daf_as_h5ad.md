# Convert Daf object to h5ad file

View the Daf data set as AnnData and save it to an h5ad file

## Usage

``` r
daf_as_h5ad(daf, h5ad, obs_is = NULL, var_is = NULL, X_is = NULL)
```

## Arguments

- daf:

  A Daf object

- h5ad:

  Path where the h5ad file will be written

- obs_is:

  Optional name for the observation axis

- var_is:

  Optional name for the variable axis

- X_is:

  Optional name for the main matrix

## Value

Invisibly returns the input Daf object

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/anndata_format.html#DataAxesFormats.AnnDataFormat.daf_as_anndata)
for details. Note this just creates the h5ad file. The Julia (Muon.jl)
AnnData object is not returned or exposed to R.
