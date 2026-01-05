# Create a Daf object with file-based storage

This function creates a Daf object that stores data in disk files. See
the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/files_format.html)
for details.

## Usage

``` r
files_daf(path, mode = "r", name = NULL)
```

## Arguments

- path:

  Path to the files storage location

- mode:

  Mode to open the storage ("r" for read-only, "r+" for read-write)

- name:

  Optional name for the Daf object

## Value

A Daf object with file-based storage
