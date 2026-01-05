# Create a writable chain wrapper of DafReader objects

This function creates a chain wrapper for a chain of DafReader data,
presenting them as a single DafWriter. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/chains.html#DataAxesFormats.Chains.chain_writer)
for details.

## Usage

``` r
chain_writer(dsets, name = NULL)
```

## Arguments

- dsets:

  List of Daf objects to chain

- name:

  Optional name for the chained Daf object

## Value

A writable Daf object chaining the input Daf objects
