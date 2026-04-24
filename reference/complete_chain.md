# Create a persistent chain by linking `new_daf` to a `base_daf`.

Writes a `base_daf_repository` scalar on `new_daf` that points at
`base_daf`'s filesystem path. If `axes` and/or `data` are specified, the
chain reads through a
[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) of
`base_daf` and the spec is stored as JSON under `base_daf_view`. The
returned chain is `chain_writer(list( viewer_or_base, new_daf))`.

## Usage

``` r
complete_chain(
  base_daf,
  new_daf,
  name = NULL,
  axes = NULL,
  data = NULL,
  absolute = FALSE
)
```

## Arguments

- base_daf:

  A `DafReader` on disk (its path is stored).

- new_daf:

  A `DafWriter` on disk (receives the pointer scalar).

- name:

  Optional name for the returned chain.

- axes, data:

  Optional
  [`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) axes
  / data spec applied on top of `base_daf`.

- absolute:

  If `TRUE`, store the absolute base path (default is relative).

## Value

The write chain.

## Details

Call
[`complete_daf()`](https://tanaylab.github.io/dafr/reference/complete_daf.md)
later to reopen the chain from disk using the stored scalars.

## Examples

``` r
base_dir <- tempfile(); dir.create(base_dir)
new_dir <- tempfile(); dir.create(new_dir)
base <- files_daf(base_dir, name = "base", mode = "w+")
new <- files_daf(new_dir, name = "new", mode = "w+")
ch <- complete_chain(base_daf = base, new_daf = new, absolute = TRUE)
```
