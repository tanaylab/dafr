# Slice — Windows CI MmapZipStore POSIX-only fallback — Exit note

**Date:** 2026-05-05
**Branch:** `slice-windows-mmap` off `dev` post-parity-Slice-3 (`eb83bc0`).

## Why

`gh run list --workflow=R-CMD-check.yaml --limit 5` showed the last 5
runs failing on `windows-latest, release` with
`[ FAIL 201 | WARN 0 | SKIP 17 | PASS 4026 ]`. Two error patterns:

1. `MmapZipStore is not supported on Windows in this build of dafr`
   from `.metadata_zip_rebuild` → `new_mmap_zip_store(staging, "w+")`
   on every FilesDaf write (~190 tests).
2. Same error from `R/zarr_format.R:112` for `.daf.zarr.zip` opens
   (~10 tests in `test-zarr-daf-zip-roundtrip.R`).

Pre-existing since slice-17 (POSIX-only MmapZipStore) and slice-18
(FilesDaf metadata.zip bundling). The `src/mmap_zip_store_win_stubs.cpp`
TU lets the package compile on Windows but every API call hits a
runtime stop. There was no R-side fallback in `R/`.

## Scope delivered

### R-side (3 files)

- `R/files_metadata_zip.R`:
  - New `.is_windows()` helper, factored out so tests can mock.
  - `.metadata_zip_rebuild`: returns early after `.write_axes_metadata(path)`
    on Windows. Local reads still get `axes/metadata.json`; the
    `metadata.zip` bundle (only used by `http_daf` clients) is omitted.
  - `.metadata_zip_append`: bare no-op on Windows (caller has already
    written the on-disk JSON).
  - `pack_files_daf_metadata`: explicit `stop()` on Windows with a
    "POSIX-only" message (the function's whole job is to produce a
    bundle, so silent no-op would mislead).
- `R/zarr_format.R::zarr_daf`: Windows guard before
  `new_mmap_zip_store(zip_path, ...)` for `.daf.zarr.zip` paths,
  replacing the cryptic stub error with a clear API-surface message.

### Test-side (8 files)

- `tests/testthat/helper-mmap-zip.R`: new `skip_if_no_mmap_zip()`
  helper.
- 7 test files with direct MmapZipStore API exercise get
  `skip_if_no_mmap_zip()` injected as the first line of every
  test_that:
  - `test-mmap-zip-store-{altrep,basic,foreign,recovery,reserve}.R`
  - `test-zarr-daf-zip-roundtrip.R`
  - `test-files-metadata-zip.R`
- `tests/testthat/test-windows-mmap-fallback.R` (new): four tests
  pinning the fallback contracts via
  `testthat::local_mocked_bindings(.is_windows = function() TRUE)`.
  Runs on POSIX; verifies the no-op + error paths without needing a
  Windows host.

### Compile-time (already in place)

- `src/mmap_zip_store_win_stubs.cpp` — runtime stops if the API is
  reached. Now unreachable from the public API on Windows.

## Numbers

- **Linux baseline (before fix):** `FAIL 0 | WARN 1 | SKIP 6 | PASS 4619`.
- **Linux post-fix:** `FAIL 0 | WARN 1 | SKIP 6 | PASS 4626` (+7
  from the new fallback test file).
- **Windows:** verifiable only on CI. Expected outcome: the 200+
  FilesDaf-write failures clear; the ~10 zarr-zip test failures
  convert to skips; mmap-zip-store-direct tests skip cleanly.

## Files touched

- `R/files_metadata_zip.R`
- `R/zarr_format.R`
- `NEWS.md`
- `tests/testthat/helper-mmap-zip.R`
- `tests/testthat/test-windows-mmap-fallback.R` (new)
- 7 test files with `skip_if_no_mmap_zip()` injection
- `dev/notes/slice-windows-mmap-exit.md` (this note)

## Verification plan

1. Linux CI (already green locally) — push and confirm.
2. Windows CI — push and confirm `FAIL 0` with the right count of
   skips. If something else surfaces, re-investigate; do not stack
   fixes blindly.
