# Verifies Windows-guard code paths by mocking `.is_windows()` to TRUE on a
# POSIX host. The actual Windows CI run is the source of truth for live
# behaviour; these tests pin the contract so future edits don't accidentally
# remove the guard.
#
# Note: Tests for metadata.zip rebuild/append were removed when
# files_metadata_zip.R was deleted (Task 7). The metadata.json path is covered
# by test-files-metadata-json.R.

test_that("zarr_daf with .daf.zarr.zip on Windows errors at the API surface", {
    path <- withr::local_tempfile(fileext = ".daf.zarr.zip")

    testthat::local_mocked_bindings(.is_windows = function() TRUE,
                                    .package = "dafr")
    expect_error(
        zarr_daf(path, mode = "w+"),
        regexp = "not supported on Windows.*POSIX-only|MmapZipStore is POSIX-only"
    )
})
