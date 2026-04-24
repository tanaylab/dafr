# Contract-enforcing daf wrapper

S7 class returned by
[`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md)
when enforcement is enabled. Intercepts `format_*` reads/writes on an
underlying
[DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md) to
track access and reject operations outside the contract.

## Usage

``` r
ContractDaf(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv()),
  computation = character(0),
  is_relaxed = logical(0),
  overwrite = logical(0),
  base = DafReader(),
  axes = new.env(parent = emptyenv()),
  data = new.env(parent = emptyenv()),
  tensor_index = new.env(parent = emptyenv())
)
```

## Arguments

- name:

  Human-readable identifier for the `Daf` store.

- internal:

  Internal per-store environment used by format backends to stash
  backend-specific state; reserved for package use.

- cache:

  Three-tier cache environment (mapped / memory / query). See
  `new_cache_env()`.

- axis_version_counter:

  Environment tracking per-axis mutation counters; invalidates cached
  reads when an axis is modified.

- vector_version_counter:

  Environment tracking per-vector mutation counters.

- matrix_version_counter:

  Environment tracking per-matrix mutation counters.

- computation:

  Name of the computation being guarded.

- is_relaxed:

  If `TRUE`, accesses to properties outside the contract are allowed
  (matching `Contract(is_relaxed = TRUE)`).

- overwrite:

  If `TRUE`, pre-existing `CreatedOutput` properties are allowed.

- base:

  Underlying `DafReader` / `DafWriter`.

- axes:

  Per-axis access-tracking environment (keyed by axis name).

- data:

  Per-property access-tracking environment (keyed by
  `<kind>:<axes>:<name>`).

- tensor_index:

  Environment mapping `<rows_axis>:<columns_axis>:<mat_name>` → tensor
  key in `data`, used to resolve per-entry matrix reads to their owning
  tensor tracker.

## Value

An S7 class object; users normally obtain instances via
[`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md)
rather than calling the constructor directly.

## See also

[`contractor()`](https://tanaylab.github.io/dafr/reference/contractor.md),
[`verify_input()`](https://tanaylab.github.io/dafr/reference/verify_input.md),
[`verify_output()`](https://tanaylab.github.io/dafr/reference/verify_input.md).

## Examples

``` r
withr::with_options(list(dafr.enforce_contracts = TRUE), {
    d <- memory_daf()
    set_scalar(d, "organism", "human")
    ct <- create_contract(scalars = list(
        contract_scalar("organism", RequiredInput, "character", "species")
    ))
    guarded <- contractor("demo_comp", ct, d)
    inherits(guarded, "dafr::ContractDaf")
})
#> [1] TRUE
```
