# Parity test for description() header lines.
#
# Upstream DataAxesFormats.jl emits per-format header lines after the
# `name:` line via `format_description_header`. Mirror this in dafr so
# `description()` shows storage-specific identifiers (path/mode/url).

test_that("description(memory_daf) shows name and type", {
    d <- memory_daf("mem!")
    out <- description(d)
    expect_match(out, "name: mem!", fixed = TRUE)
    expect_match(out, "type: MemoryDaf", fixed = TRUE)
})

test_that("description(files_daf) shows path and mode (writer)", {
    p <- withr::local_tempdir("dafr-desc-fd-")
    d <- files_daf(p, "w", name = "fd!")
    out <- description(d)
    # files_daf normalises its path; on macOS that resolves
    # /var/folders/... -> /private/var/folders/... via the symlink, so
    # match against the normalised form rather than the raw tempdir.
    p_norm <- normalizePath(p, winslash = "/", mustWork = FALSE)
    expect_match(out, "name: fd!", fixed = TRUE)
    expect_match(out, "type: FilesDaf", fixed = TRUE)
    expect_match(out, sprintf("path: %s", p_norm), fixed = TRUE)
    expect_match(out, "mode: w", fixed = TRUE)
})

test_that("description(files_daf, mode=r) shows path and mode r", {
    p <- withr::local_tempdir("dafr-desc-fdro-")
    d <- files_daf(p, "w+", name = "fd!"); rm(d); gc()
    ro <- files_daf(p, "r", name = "fd!")
    out <- description(ro)
    p_norm <- normalizePath(p, winslash = "/", mustWork = FALSE)
    expect_match(out, "type: FilesDaf", fixed = TRUE)
    expect_match(out, sprintf("path: %s", p_norm), fixed = TRUE)
    expect_match(out, "mode: r", fixed = TRUE)
})

test_that("description(zarr_daf, dir) shows path and mode", {
    p <- tempfile("dafr-desc-zd-", fileext = ".daf.zarr")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    d <- zarr_daf(p, "w", name = "zd!")
    out <- description(d)
    expect_match(out, "type: ZarrDaf", fixed = TRUE)
    expect_match(out, sprintf("path: %s", p), fixed = TRUE)
    expect_match(out, "mode: w", fixed = TRUE)
})

test_that("description(zarr_daf, :memory:) shows :memory: path", {
    d <- zarr_daf(":memory:", "w", name = "zdm!")
    out <- description(d)
    expect_match(out, "type: ZarrDaf", fixed = TRUE)
    expect_match(out, "path: :memory:", fixed = TRUE)
    expect_match(out, "mode: w", fixed = TRUE)
})

test_that("description(http_daf) shows url", {
    skip_on_cran()
    skip_if_not_installed("processx")
    skip_if_not(nzchar(Sys.which("python")))
    skip_if_no_mmap_zip()
    root <- withr::local_tempdir("daf-http-desc-")
    files_path <- file.path(root, "served.daf")
    fd <- files_daf(files_path, "w+", name = "served!")
    set_scalar(fd, "name", "served!")
    rm(fd); gc()
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    daf <- http_daf(paste0(h$url, "/served.daf"))
    out <- description(daf)
    expect_match(out, "type: HttpDaf", fixed = TRUE)
    expect_match(out, sprintf("url: %s/served.daf", h$url), fixed = TRUE)
})

# format_description_header generic — direct API contract.
# Generic is internal (mirrors other format_* generics); reach via :::.
test_that("format_description_header default returns type only", {
    d <- memory_daf("x")
    lines <- dafr:::format_description_header(d)
    expect_identical(lines, "type: MemoryDaf")
})

test_that("format_description_header honours indent", {
    d <- memory_daf("x")
    lines <- dafr:::format_description_header(d, indent = "  ")
    expect_identical(lines, "  type: MemoryDaf")
})
