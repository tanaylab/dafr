# Zarr v2 Store abstract base class.

Uniform read/write API for byte content keyed by path strings. Concrete
implementations: DirStore, DictStore, MmapZipStore.

## Usage

``` r
ZarrStore()

DirStore(root = character(0))

DictStore(env = NULL)

MmapZipStore(path = character(0), mode = character(0), xptr = NULL)
```

## Arguments

- root:

  (`DirStore`) Filesystem root path; created if missing.

- env:

  (`DictStore`) Internal environment used as the key-value backing
  store; typically created by
  [`new_dict_store()`](https://tanaylab.github.io/dafr/reference/new_dict_store.md).

- path:

  (`MmapZipStore`) Filesystem path to a zip archive.

- mode:

  (`MmapZipStore`) One of `"r"`, `"r+"`, `"w+"`, `"w"`; set by
  [`new_mmap_zip_store()`](https://tanaylab.github.io/dafr/reference/new_mmap_zip_store.md).

- xptr:

  (`MmapZipStore`) Internal external pointer to the C++ store; set by
  [`new_mmap_zip_store()`](https://tanaylab.github.io/dafr/reference/new_mmap_zip_store.md)
  and not intended for direct use.
