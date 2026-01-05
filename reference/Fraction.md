# Fraction query operation

Element-wise operation that converts every element to its fraction out
of the total. This operation preserves the shape of the data (scalar,
vector, or matrix) but changes each value to be the fraction of the
total sum of all values in the data. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Fraction)
for details.

## Usage

``` r
Fraction(...)
```

## Arguments

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
