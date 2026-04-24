# Promote an implicit property to an explicit axis.

Given an `existing_axis` with a property `implicit_axis`, create a new
axis from the unique non-empty values of the property. Scan the other
vector properties on `existing_axis`; for each one whose value is
uniquely determined by the implicit value, migrate it to the new axis.

## Usage

``` r
reconstruct_axis(
  daf,
  existing_axis,
  implicit_axis,
  rename_axis = NULL,
  empty_implicit = NULL,
  implicit_properties = NULL,
  skipped_properties = NULL
)
```

## Arguments

- daf:

  A `DafWriter`.

- existing_axis:

  Axis that holds the implicit property.

- implicit_axis:

  Property name on `existing_axis`; becomes the new axis's name (unless
  `rename_axis`).

- rename_axis:

  Optional name for the new axis.

- empty_implicit:

  If non-NULL, values equal to this are treated as empty (equivalent to
  the empty string).

- implicit_properties:

  Optional character vector: only these properties are considered for
  migration.

- skipped_properties:

  Optional character vector: properties to exclude from migration (even
  if consistent).

## Value

Named list of "value for empty-implicit entries" per migrated property.

## Details

Returns a named list: for each migrated property, the (consistent) value
associated with `existing_axis` entries whose implicit value is empty —
or `NULL` if no such entries exist. These values can be used to
reconstruct the original property via the `?? X` query modifier.

This slice requires `rename_axis` (or the default, `implicit_axis` name)
to not already exist in `daf`. Pre-existing axis merge is a Slice 7
follow-up.

## Examples

``` r
d <- memory_daf(name = "d")
add_axis(d, "cell", c("c1", "c2", "c3"))
set_vector(d, "cell", "donor", c("dA", "dB", "dA"))
set_vector(d, "cell", "donor_age", c(30L, 40L, 30L))
reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor")
#> list()
get_vector(d, "donor", "donor_age")
#> dA dB 
#> 30 40 
```
