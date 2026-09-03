# Create a persistent chain by linking `new_daf` to its base repositories.

Immediately after creating an empty disk-based `new_daf`, chain it on
top of one or more disk-based base repositories and return the new
chain. Each base is a `DafReader`, or a
[`base_daf()`](https://tanaylab.github.io/dafr/reference/base_daf.md)
spec when only a view of it is used. Give several when `new_daf` rests
on more than one repository - say, a repository of shared computed
results and one of the parameters this variant of the analysis uses,
both resting in turn on the same raw data. Later bases override earlier
ones, as in any chain, and a repository reached more than once is used
once, at its earliest position.

## Usage

``` r
complete_chain(base_daf, new_daf, name = NULL, absolute = FALSE)
```

## Arguments

- base_daf:

  A `DafReader` on disk, a
  [`base_daf()`](https://tanaylab.github.io/dafr/reference/base_daf.md)
  spec, or a list of either.

- new_daf:

  A `DafWriter` on disk (receives the pointer scalar).

- name:

  Optional name for the returned chain.

- absolute:

  If `TRUE`, store absolute base paths (default is relative).

## Value

The write chain.

## Details

This sets the `base_daf_repository` scalar of `new_daf` to describe the
bases, so the chain can be recreated later by
[`complete_daf()`](https://tanaylab.github.io/dafr/reference/complete_daf.md).
By default the stored paths are relative to `new_daf`, for the common
case where a group of repositories is stored under a common root; set
`absolute` to store absolute paths.

## Examples

``` r
base_dir <- tempfile(); dir.create(base_dir)
new_dir <- tempfile(); dir.create(new_dir)
base <- files_daf(base_dir, name = "base", mode = "w+")
new <- files_daf(new_dir, name = "new", mode = "w+")
ch <- complete_chain(base_daf = base, new_daf = new, absolute = TRUE)
```
