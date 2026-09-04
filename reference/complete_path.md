# Canonical disk path of a (possibly chained) daf.

Public alias of the internal `.complete_path`. For a `FilesDaf`, returns
the root directory on disk. For a chain, returns the path of its last
repository, but only when the chain holds exactly what reopening that
path with
[`complete_daf()`](https://tanaylab.github.io/dafr/reference/complete_daf.md)
would give; otherwise `NULL`. Returns `NULL` for a daf with no on-disk
location.

## Usage

``` r
complete_path(daf)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

## Value

Character scalar, or `NULL`.

## Examples

``` r
tmp <- tempfile("dafr-")
dir.create(tmp)
fd <- files_daf(tmp, mode = "w+", name = "fd")
complete_path(fd)
#> [1] "/tmp/Rtmpg41fgo/dafr-1f164d70e56c"
unlink(tmp, recursive = TRUE)
```
