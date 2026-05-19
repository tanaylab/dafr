# Zarr and HTTP storage

In addition to the in-memory (`memory_daf`) and one-file-per-property
(`files_daf`) storage backends, dafr ships two Zarr-backed backends and
an HTTP reader. All three are bidirectionally compatible with
`DataAxesFormats.jl`’s `ZarrDaf` and `HttpDaf`, and the Zarr layout is
readable by `zarr-python` and any other Zarr v2 consumer.

## ZarrDaf — directory layout

`zarr_daf(path, mode)` reads and writes a Zarr v2 group tree on the
local filesystem. The path conventionally ends in `.daf.zarr`:

``` r

path <- tempfile(fileext = ".daf.zarr")
d <- zarr_daf(path, mode = "w")
add_axis(d, "cell", c("c1", "c2", "c3"))
set_scalar(d, "organism", "human")
set_vector(d, "cell", "score", c(1.5, 2.5, 3.5))
set_matrix(d, "cell", "cell", "kin",
           matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3))
rm(d); gc()
#>           used  (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 2512738 134.2    5048441 269.7  3274195 174.9
#> Vcells 4318104  33.0   10146329  77.5  7138392  54.5

# Reopen read-only and inspect.
d <- zarr_daf(path, mode = "r")
cat(description(d))
#> name: file27781cd41096.daf.zarr
#> type: ZarrDaf
#> path: /tmp/RtmpRyQKr3/file27781cd41096.daf.zarr
#> mode: r
#> scalars:
#>   organism: "human"
#> axes:
#>   cell: 3 entries
#> vectors:
#>   cell:
#>     score
#> matrices:
#>   cell,cell:
#>     kin
```

A directory ZarrDaf store is a normal Zarr group. Python consumers can
open it with `zarr.open()`:

``` python
# Python
import zarr
g = zarr.open("/path/to/foo.daf.zarr", mode="r")
g["axes/cell"][...]          # array(['c1', 'c2', 'c3'], ...)
g["scalars/organism"][...]   # array(['human'], dtype=object)
```

## ZarrDaf — single-file zip layout

For shipping a daf as a single artifact, `zarr_daf(path, mode)` accepts
a `.daf.zarr.zip` path. The store is backed by an mmap’d ZIP archive:
reads of stored (uncompressed) entries are zero-copy ALTREP views over
the mapped region, and writes append-commit through a crash-safe
two-phase protocol.

``` r

zip_path <- tempfile(fileext = ".daf.zarr.zip")
d <- zarr_daf(zip_path, mode = "w")
add_axis(d, "gene", c("g1", "g2"))
set_vector(d, "gene", "is_marker", c(TRUE, FALSE))
rm(d); gc()
#>           used  (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 2737372 146.2    5048441 269.7  3274195 174.9
#> Vcells 4692457  35.9   10146329  77.5  7138392  54.5

# Reopen.
d <- zarr_daf(zip_path, mode = "r")
get_vector(d, "gene", "is_marker")
#>    g1    g2 
#>  TRUE FALSE
```

> **Note:** the single-file `.daf.zarr.zip` backend depends on POSIX
> `mmap` and is not available on Windows in this build. Use the
> directory layout above on Windows.

Foreign consumers open the same archive via Python’s
`zarr.storage.ZipStore`:

``` python
# Python
import zarr
from zarr.storage import ZipStore
g = zarr.open(ZipStore("foo.daf.zarr.zip", mode="r"), mode="r")
```

## HttpDaf and HttpStore — reading over HTTP(S)

dafr provides two read-only HTTP backends:

- **`http_daf(url)`** — read a `files_daf` directory served over HTTP.
  The client downloads `metadata.zip` once at open time, parses it in
  memory, and serves all JSON metadata from there. Non-JSON payloads
  (axis `.txt` files, vector / matrix `.data` etc.) are fetched lazily
  on first access via one HTTP `GET` each, cached by the standard cache
  layer.

- **`zarr_daf("http(s)://...")`** — read a `.daf.zarr` directory served
  over HTTP. Routes through `HttpStore`, an HTTP-backed implementation
  of dafr’s internal `ZarrStore` interface. Fetches `.zmetadata` once to
  discover the tree, then pulls chunks on demand.

Both backends are read-only; writable modes hard-error. Server data is
assumed stable while a daf is open — reopen to pick up changes. Each
HTTP `GET` uses a 30-second timeout, overridable via
`options(dafr.http_timeout = N)` or env `DAFR_HTTP_TIMEOUT`. There is no
automatic retry; flaky-network handling is the caller’s responsibility.

``` r

# Open a remote files_daf:
d <- http_daf("https://example.com/path/to/foo.daf/")
get_scalar(d, "organism")

# Open a remote ZarrDaf:
d <- zarr_daf("https://example.com/path/to/foo.daf.zarr/", mode = "r")
get_vector(d, "cell", "score")

# open_daf() routes by URL pattern:
d <- open_daf("https://example.com/path/to/foo.daf.zarr/", mode = "r")
```

To publish a `files_daf` over HTTP, the directory needs a `metadata.zip`
bundle at its root. From dafr 0.2.0 onward this is maintained
automatically by every write; for pre-0.2.0 stores call
`pack_files_daf_metadata(path)` once before publishing.

``` r

# One-time: bundle a pre-0.2.0 store's metadata so HttpDaf clients can
# open it. Idempotent; no-op once metadata.zip exists.
pack_files_daf_metadata("/srv/www/data/foo.daf")
```

`zarr_daf("https://...zip")` is intentionally not supported — open the
zip locally instead, since byte-range reads against a remote zip would
need separate handling.

## Cross-language smoke

Both Zarr layouts and the HTTP backend are continuously cross-checked
against `zarr-python` in the package test suite. Round-trip parity is
verified for axes, dense and sparse vectors, dense and sparse matrices,
and scalars of every dafr type. See `tests/testthat/test-zarr-python.R`,
`tests/testthat/test-mmap-zip-store-foreign.R`, and
`tests/testthat/test-http-live.R` for the live test scripts.
