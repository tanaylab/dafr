# Create a read-only chain wrapper of DafReader objects

This function creates a read-only chain wrapper of DafReader objects,
presenting them as a single DafReader. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/chains.html#DataAxesFormats.Chains.chain_reader)
for details.

## Usage

``` r
chain_reader(dsets, name = NULL)
```

## Arguments

- dsets:

  List of Daf objects to chain

- name:

  Optional name for the chained Daf object

## Value

A read-only Daf object chaining the input Daf objects
