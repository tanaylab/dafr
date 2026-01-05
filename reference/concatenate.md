# Concatenate multiple Daf data sets along some axis

Concatenate multiple Daf data sets along some axis

## Usage

``` r
concatenate(
  destination,
  axis,
  sources,
  names = NULL,
  dataset_axis = "dataset",
  dataset_property = TRUE,
  prefix = FALSE,
  prefixed = NULL,
  empty = NULL,
  sparse_if_saves_storage_fraction = 0.1,
  merge = NULL,
  overwrite = FALSE
)
```

## Arguments

- destination:

  A Daf object to write the concatenated result to

- axis:

  Name of the axis or axes to concatenate along

- sources:

  A list of Daf objects to concatenate

- names:

  Optional vector of names for the sources

- dataset_axis:

  Optional name for the dataset axis

- dataset_property:

  Whether to add a dataset property

- prefix:

  Whether to prefix the names of properties in the source data sets

- prefixed:

  Optional set of names that are already prefixed and should be left
  alone

- empty:

  Value to use for filling in missing data

- sparse_if_saves_storage_fraction:

  Fraction of storage savings required to use sparse matrices

- merge:

  Optional named list mapping property keys to merge actions
  ("SkipProperty", "LastValue", or "CollectAxis")

- overwrite:

  Whether to overwrite existing data

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/concat.html)
for details.
