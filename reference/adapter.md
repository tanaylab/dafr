# Invoke a computation on a view of some data set and return the result

This function creates an adapter pattern that allows running a
computation on a view of the data and copying the results back to the
original dataset.

## Usage

``` r
adapter(
  daf,
  computation,
  input_axes = NULL,
  input_data = NULL,
  capture = memory_daf,
  output_axes = NULL,
  output_data = NULL,
  empty = NULL,
  relayout = TRUE,
  overwrite = FALSE
)
```

## Arguments

- daf:

  A Daf object to adapt

- computation:

  A function that takes a DafWriter and performs computations on it

- input_axes:

  Optional named list specifying axes to expose as input

- input_data:

  Optional named list specifying data to expose as input

- capture:

  Function to create a capture Daf (default: memory_daf)

- output_axes:

  Optional named list specifying axes to expose as output

- output_data:

  Optional named list specifying data to expose as output

- empty:

  Optional named list mapping data keys to values for filling missing
  data

- relayout:

  Whether to allow relayout when copying results (default: TRUE)

- overwrite:

  Whether to overwrite existing data when copying results (default:
  FALSE)

## Value

The result of the computation function

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/adapters.html#DataAxesFormats.Adapters.adapter)
for details.
