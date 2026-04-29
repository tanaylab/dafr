# Per-vector version counter.

Returns the monotonic counter for the `name` vector on `axis`. Mirrors
Julia DAF: incremented every time `set_vector` is called (NOT on
`delete_vector`). Returns `0L` if the vector has never been set.

## Usage

``` r
vector_version_counter(daf, axis, name)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- axis:

  Axis name (character scalar).

- name:

  Vector name (character scalar).

## Value

`integer(1)`.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 566.
m <- example_metacells_daf()
vector_version_counter(m, "type", "color")                       # 1L
#> [1] 1
set_vector(m, "type", "color", as.character(1:4), overwrite = TRUE)
vector_version_counter(m, "type", "color")                       # 2L
#> [1] 2
```
