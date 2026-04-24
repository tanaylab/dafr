# Per-vector version counter.

Returns the monotonic counter for the `name` vector on `axis`.
Incremented on `set_vector` / `delete_vector`. Returns `0L` if the
vector has never been mutated.

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
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
vector_version_counter(d, "cell", "donor") # 0L
#> [1] 0
set_vector(d, "cell", "donor", c("A", "B"))
vector_version_counter(d, "cell", "donor") # 1L
#> [1] 1
```
