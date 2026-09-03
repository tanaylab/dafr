# Record which entry of one axis each entry of another belongs to.

Given a `base_axis` with two vector properties, one holding a reference
to `from_axis` and one to `to_axis`, create a property of `from_axis`
that references `to_axis`. This is only possible if every entry of
`from_axis` is always associated with a single entry of `to_axis`.

## Usage

``` r
connect_axes(
  daf,
  base_axis,
  from_axis,
  to_axis,
  from_property = NULL,
  to_property = NULL,
  connect_property = NULL,
  overwrite = FALSE
)
```

## Arguments

- daf:

  A `DafWriter`.

- base_axis:

  Axis holding both reference properties.

- from_axis:

  Axis to create the new property on.

- to_axis:

  Axis the new property references.

- from_property:

  Property of `base_axis` referencing `from_axis` (default:
  `from_axis`).

- to_property:

  Property of `base_axis` referencing `to_axis` (default: `to_axis`).

- connect_property:

  Name of the created property of `from_axis` (default: `to_axis`).

- overwrite:

  Whether to overwrite an existing `connect_property`.

## Value

`NULL`, invisibly.

## Details

This can happen when one axis (say, "batch") references two other axes
(say, "plate" and "run"). If every batch had a plate and every plate a
run,
[`reconstruct_axis()`](https://tanaylab.github.io/dafr/reference/reconstruct_axis.md)
would be enough and batch simply wouldn't have a "run" property. If,
however, some batches have a run reference but no plate reference, we
still want to record that "each plate is in a run" while not giving up
on "each batch is in a run", so the data is duplicated: unlike
[`reconstruct_axis()`](https://tanaylab.github.io/dafr/reference/reconstruct_axis.md),
which *moves* data, this *copies* it and leaves the original in place.

By default the properties of `base_axis` holding the references are
named after the axes they refer to, and the created `connect_property`
of `from_axis` is named after `to_axis`. Specify `from_property`,
`to_property` and `connect_property` when they are not; a base axis may
refer to the same axis twice (a "sorted_by" and a "sequenced_by" run,
say), in which case the property name is the only thing telling them
apart.

An entry of `base_axis` with no `from_axis` reference is skipped, so its
`to_axis` reference is not examined at all. An entry of `from_axis` that
no entry of `base_axis` refers to is given an empty value.

## Examples

``` r
d <- memory_daf(name = "d")
add_axis(d, "batch", c("B1", "B2", "B3", "B4"))
add_axis(d, "plate", c("P1", "P2", "P3"))
add_axis(d, "run", c("R1", "R2"))
set_vector(d, "batch", "plate", c("P1", "P1", "P2", ""))
set_vector(d, "batch", "run", c("R1", "R1", "R2", "R2"))
connect_axes(d, base_axis = "batch", from_axis = "plate", to_axis = "run")
get_vector(d, "plate", "run") # "R1" "R2" "" -- P3 is named by no batch
#>   P1   P2   P3 
#> "R1" "R2"   "" 
```
