# Spell "there is no value here" one way in a vector property.

Replace every one of the `empty_values` of a `property` of an `axis`
with a single `empty_value`, converting the property to a `dtype` on the
way if one is given.

## Usage

``` r
unify_empty_vector_values(
  daf,
  axis,
  property,
  empty_values,
  dtype = NULL,
  empty_value = NULL
)
```

## Arguments

- daf:

  A `DafWriter`.

- axis:

  Axis holding the property.

- property:

  Name of the vector property to rewrite.

- empty_values:

  Value, or vector of values, meaning "there is no value here". Pass
  `NULL` (or an empty vector) when only converting.

- dtype:

  Optional Julia type name (`"String"`, `"Float32"`, `"UInt32"`, ...) to
  convert the non-empty values to.

- empty_value:

  Optional value to store for the empty entries, overriding the per-type
  default.

## Value

`NULL`, invisibly.

## Details

Data arrives spelling absence several ways, often several ways in the
same property: an empty string in some entries and `NA` in others,
`(Missing)` elsewhere, and for numbers a sentinel such as the smallest
integer, which is a number rather than a visible absence.

This matters before
[`reconstruct_axis()`](https://tanaylab.github.io/dafr/reference/reconstruct_axis.md)
and
[`connect_axes()`](https://tanaylab.github.io/dafr/reference/connect_axes.md),
which decide what to do with an entry by asking whether its value is
empty.

Numbers often arrive as text for exactly this reason - a column of
measurements is a column of strings because a few of its entries say
`NA`. Giving a `dtype` converts the values that are not empty, which is
an error unless all of them are values of that type; the ones that are
empty become the `empty_value`.

By default the `empty_value` is the empty string for strings, `NaN` for
floats, and `0` for unsigned integers (Daf indices are 1-based, so `0`
is free to mean "none"). A signed integer or a Boolean has no such
value, so one must be given, or a `dtype` that has one.

A property none of whose values is empty is left as it is. What *is* an
error is asking for nothing at all - no `empty_values` and no `dtype` -
since that cannot do anything whatever the data says.

Unlike Julia's `unify_empty_vector_values!`, the result is not
`bestify`d; a mostly-empty result is stored as given.

## Examples

``` r
d <- memory_daf(name = "d")
add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
set_vector(d, "cell", "batch", c("X", "NA", "(Missing)", ""))
unify_empty_vector_values(d,
    axis = "cell", property = "batch",
    empty_values = c("NA", "(Missing)")
)
get_vector(d, "cell", "batch")
#>  c1  c2  c3  c4 
#> "X"  ""  ""  "" 
```
