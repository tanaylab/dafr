# Reset any in-progress axis reorder.

Idempotent. On `files_daf`, completes the rollback of any pending
reorder by restoring the backup hardlinks and removing the
`.reorder.backup/` directory. On `memory_daf`, no-op.

## Usage

``` r
reset_reorder_axes(daf, crash_counter = NULL)
```

## Arguments

- daf:

  A [DafWriter](https://tanaylab.github.io/dafr/reference/DafWriter.md).

- crash_counter:

  Internal — for testing only.

## Value

Invisibly the input `daf`.

## Details

Note: `files_daf(path, mode = "r+")` and `mode = "w+"` automatically
invoke this on open, so manual calls are usually unnecessary.

## Examples

``` r
d <- memory_daf()
reset_reorder_axes(d)  # no-op for memory_daf
```
