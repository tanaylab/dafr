# Load a Muon-style h5ad file into a `memory_daf`.

Loads dense or sparse `/X`, `/obs` and `/var` column groups (dense
scalar columns and categorical columns), flat + nested `/uns` scalar
entries (nested keys are flattened via `_`), dense `/layers`, and
`/obsm` / `/varm` dense matrices (each stored on a synthetic axis
`obsm_<name>_dim` / `varm_<name>_dim`). `obsp` / `varp` and `raw` are
still escalated through `unsupported_handler` and skipped.

## Usage

``` r
h5ad_as_daf(path, name = NULL, mode = "r", unsupported_handler = WARN_HANDLER)
```

## Arguments

- path:

  Path to the `.h5ad` file.

- name:

  Optional name for the returned Daf; defaults to the file basename
  without extension.

- mode:

  `hdf5r` open mode. Default `"r"` (read-only).

- unsupported_handler:

  Handler for h5ad features we don't translate (default
  [WARN_HANDLER](https://tanaylab.github.io/dafr/reference/handler-constants.md)).
  Accepts `ERROR_HANDLER`, `WARN_HANDLER`, `IGNORE_HANDLER`, or a
  `function(message)` dispatched via
  [`inefficient_action_handler()`](https://tanaylab.github.io/dafr/reference/inefficient_action_handler.md).

## Value

A
[`memory_daf()`](https://tanaylab.github.io/dafr/reference/memory_daf.md)
populated with the h5ad contents.

## Details

Requires the `hdf5r` and `Matrix` packages.

## See also

[`daf_as_h5ad()`](https://tanaylab.github.io/dafr/reference/daf_as_h5ad.md),
[DafAnnData](https://tanaylab.github.io/dafr/reference/DafAnnData.md),
[`as_anndata()`](https://tanaylab.github.io/dafr/reference/as_anndata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- system.file("extdata", "small_test.h5ad", package = "dafr")
d <- h5ad_as_daf(p)
is_daf(d)
} # }
```
