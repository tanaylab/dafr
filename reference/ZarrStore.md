# Zarr Store abstract base class.

Uniform read/write API for byte content keyed by path strings. Concrete
implementations: DirStore, DictStore, MmapZipStore.

## Usage

``` r
ZarrStore()

DirStore(root = character(0), consolidate_cache = new.env(parent = emptyenv()))

DictStore(env = NULL, consolidate_cache = new.env(parent = emptyenv()))

MmapZipStore(path = character(0), mode = character(0), xptr = NULL)
```

## Arguments

- root:

  (`DirStore`) Filesystem root path; created if missing.

- consolidate_cache:

  (`DirStore`/`DictStore`) Internal environment holding the in-memory
  consolidated-metadata index, maintained incrementally across writes so
  `set_*` need not re-parse the whole store root on every mutation.
  Created automatically; not intended for direct use.

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
