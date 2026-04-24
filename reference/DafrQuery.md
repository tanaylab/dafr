# Pipe-composable query object.

Produced by the query builders
([`Axis()`](https://tanaylab.github.io/dafr/reference/Axis.md),
[`LookupVector()`](https://tanaylab.github.io/dafr/reference/LookupVector.md),
...) and their composition via `|>`. Carries both the parsed AST (a list
in the shape
[`parse_query()`](https://tanaylab.github.io/dafr/reference/parse_query.md)
returns) and its canonical string form.

## Usage

``` r
DafrQuery(ast = list(), canonical = character(0))
```

## Arguments

- ast:

  List of AST nodes.

- canonical:

  Character scalar; the canonical query string for `ast`.

## Value

A `DafrQuery` instance.

## See also

[`get_query()`](https://tanaylab.github.io/dafr/reference/get_query.md)

## Examples

``` r
DafrQuery(
    ast = list(list(op = "Axis", axis_name = "cell")),
    canonical = "@ cell"
)
#> <DafrQuery> @ cell 
```
