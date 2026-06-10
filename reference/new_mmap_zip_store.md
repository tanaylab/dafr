# Create a zip-archive-backed Zarr store (mmap, append-only).

Opens (or creates) a single ZIP archive at `path` as a Zarr store. Reads
use a shared mmap of the archive (zero-copy for stored entries via
ALTREP RAW); writes append entries with a crash-safe two-step commit
protocol. Mirrors upstream `DataAxesFormats.MmapZipStores.MmapZipStore`.

## Usage

``` r
new_mmap_zip_store(path, mode = "r", max_file_size = 2^40)
```

## Arguments

- path:

  Filesystem path to a `.daf.zarr.zip` archive.

- mode:

  One of `"r"`, `"r+"`, `"w+"`, `"w"`.

- max_file_size:

  Cap on the writable virtual reservation, in bytes. Ignored for
  read-only opens. Defaults to 1 TiB.

## Value

A `MmapZipStore`.

## Details

Modes: `"r"` (read existing), `"r+"` (read/write existing), `"w+"`
(read/write, create if missing), `"w"` (truncate + create). On a
writable open, the store reserves `max_file_size` bytes of virtual
address space (no RAM cost) and grows the file via `ftruncate` as
entries append. Default `max_file_size` is 1 TiB.

## Examples

``` r
if (FALSE) { # \dontrun{
s <- new_mmap_zip_store("/path/to/foo.daf.zarr.zip", mode = "w")
} # }
```
