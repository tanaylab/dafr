# Copy all content from source to destination

Copy all content from source to destination

## Usage

``` r
copy_all(destination, source, empty = NULL, overwrite = FALSE, relayout = TRUE)
```

## Arguments

- destination:

  A Daf object to copy to

- source:

  A Daf object to copy from

- empty:

  A named list mapping data keys to values for filling missing data

- overwrite:

  Whether to overwrite if data already exists

- relayout:

  Whether to allow relayout

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_all!)
for details.
