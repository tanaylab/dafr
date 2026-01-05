# Create a Daf object with HDF5-based storage

This function creates a Daf object that stores data in an HDF5 disk
file. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/h5df_format.html)
for details.

## Usage

``` r
h5df(root, mode = "r", name = NULL)
```

## Arguments

- root:

  Path to the HDF5 file, or a Julia HDF5 File or Group object

- mode:

  Mode to open the storage ("r" for read-only, "r+" for read-write)

- name:

  Optional name for the Daf object

## Value

A Daf object with HDF5-based storage
