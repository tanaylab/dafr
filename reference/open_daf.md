# Open a Daf repository based on path

This function determines whether to open a files-based Daf or an
HDF5-based Daf based on the file path.

## Usage

``` r
open_daf(path, mode = "r", name = NULL)
```

## Arguments

- path:

  Path to the Daf repository

- mode:

  Mode to open the storage ("r" for read-only, "r+" for read-write)

- name:

  Optional name for the Daf object

## Value

A Daf object (either files_daf or h5df)

## Details

If the path ends with `.h5df` or contains `.h5dfs#` (followed by a group
path), then it opens an HDF5 file (or a group in one). Otherwise, it
opens a files-based Daf.

As a shorthand, you can specify a path to a group within an HDF5 file by
using a path with a `.h5dfs` suffix, followed by `#` and the path of the
group in the file.

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/complete.html)
for details.
