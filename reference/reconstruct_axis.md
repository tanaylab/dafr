# Reconstruct implicit axes

Given an existing axis with a property representing an implicit axis,
create a new axis. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/reconstruction.html)
for details.

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

  A Daf object

- existing_axis:

  Name of the existing axis containing implicit axis data

- implicit_axis:

  Name of the property in existing_axis that contains implicit axis
  values

- rename_axis:

  Optional new name for the implicit axis (default: NULL, uses
  implicit_axis name)

- empty_implicit:

  Value to consider as empty/NA in the implicit axis values (default:
  NULL)

- implicit_properties:

  Optional set of properties to copy (default: NULL for all properties)

- skipped_properties:

  Optional set of properties to skip (default: NULL for none)

## Value

A named list of properties and their associated values
