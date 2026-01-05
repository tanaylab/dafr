# Complete chain of Daf repositories

Open a complete chain of Daf repositories by tracing back through the
`base_daf_repository` property. Each repository in a chain contains a
scalar property called `base_daf_repository` which identifies its parent
repository (if any).

## Usage

``` r
complete_daf(leaf, mode = "r", name = NULL)
```

## Arguments

- leaf:

  Path to the leaf repository, which will be traced back through its
  ancestors

- mode:

  Mode to open the repositories ("r" for read-only, "r+" for read-write)

- name:

  Optional name for the complete Daf object

## Value

A Daf object combining the leaf repository with all its ancestors

## Details

If mode is "r+", only the first (leaf) repository is opened in write
mode. The `base_daf_repository` path is relative to the directory
containing the child repository.

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/complete.html)
for details.
