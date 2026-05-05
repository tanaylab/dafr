# Verifies the slice-17 MmapZipStore Windows fallback paths by mocking
# `.is_windows()` to TRUE on a POSIX host. The actual Windows CI run is
# the source of truth for the live behaviour; these tests pin the
# fallback contract so future edits don't accidentally reintroduce the
# `MmapZipStore is not supported on Windows` error path.

test_that(".metadata_zip_rebuild on Windows writes axes/metadata.json but skips the zip", {
    path <- withr::local_tempdir("daf-win-rebuild-")
    # Build the FilesDaf skeleton on POSIX (no-op short-circuit not yet
    # active here because .is_windows() is unmocked during setup).
    d <- files_daf(path, "w+")
    add_axis(d, "cell", c("c1", "c2"))
    rm(d); gc()

    # A pre-existing metadata.zip from the POSIX setup would mask the
    # no-op behavior; remove it so the post-condition is unambiguous.
    unlink(file.path(path, "metadata.zip"), force = TRUE)

    testthat::local_mocked_bindings(.is_windows = function() TRUE)
    out <- dafr:::.metadata_zip_rebuild(path)

    expect_identical(out, file.path(path, "metadata.zip"))
    expect_false(file.exists(file.path(path, "metadata.zip")))
    expect_true(file.exists(file.path(path, "axes", "metadata.json")))
})

test_that(".metadata_zip_append on Windows is a bare no-op", {
    path <- withr::local_tempdir("daf-win-append-")
    files_daf(path, "w+")
    unlink(file.path(path, "metadata.zip"), force = TRUE)

    testthat::local_mocked_bindings(.is_windows = function() TRUE)
    expect_silent(dafr:::.metadata_zip_append(path, "scalars/whatever.json"))
    expect_false(file.exists(file.path(path, "metadata.zip")))
})

test_that("pack_files_daf_metadata on Windows errors with a clear message", {
    path <- withr::local_tempdir("daf-win-pack-")
    files_daf(path, "w+")

    testthat::local_mocked_bindings(.is_windows = function() TRUE)
    expect_error(
        pack_files_daf_metadata(path),
        regexp = "not supported on Windows.*POSIX-only"
    )
})

test_that("zarr_daf with .daf.zarr.zip on Windows errors at the API surface", {
    path <- withr::local_tempfile(fileext = ".daf.zarr.zip")

    testthat::local_mocked_bindings(.is_windows = function() TRUE,
                                    .package = "dafr")
    expect_error(
        zarr_daf(path, mode = "w+"),
        regexp = "not supported on Windows.*POSIX-only|MmapZipStore is POSIX-only"
    )
})
