# Canonical disk path of a (possibly chained) daf.

Public alias of the internal `.complete_path`. For a `FilesDaf`, returns
the root directory on disk. For a chain whose last writer is a
`FilesDaf`, returns that directory. Errors on dafs with no on-disk
location.

## Usage

``` r
complete_path(daf)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

## Value

Character scalar (absolute path).

## Examples

``` r
tmp <- tempfile("dafr-")
dir.create(tmp)
fd <- files_daf(tmp, mode = "w+", name = "fd")
complete_path(fd)
#> [1] "/tmp/Rtmp49UEvY/dafr-1b252d20320f"
unlink(tmp, recursive = TRUE)
```
