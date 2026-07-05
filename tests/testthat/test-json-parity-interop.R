# Live interop: DataAxesFormats.jl reads dafr-written stores/chains via the
# shared JSON schemas. Skips unless the Julia env is DAF >= 0.3.0.

test_that("Julia complete_daf reads a dafr-written chain view", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3(), "DAF >= 0.3.0 not available")
    root <- withr::local_tempdir()
    bdir <- file.path(root, "base"); ndir <- file.path(root, "new")
    base <- files_daf(bdir, name = "base", mode = "w+")
    add_axis(base, "cell", paste0("c", 1:4)); add_axis(base, "gene", paste0("g", 1:3))
    set_matrix(base, "cell", "gene", "expr", matrix(as.numeric(1:12), 4, 3))
    new <- files_daf(ndir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE,
                   axes = list(list("cell", "="), list("gene", "=")),
                   data = list(list(c("cell", "gene", "expr"), "=")))
    res <- run_julia(c(
        "using DataAxesFormats",
        sprintf('d = complete_daf(raw"%s", "r")', ndir),
        'm = get_matrix(d, "cell", "gene", "expr")',
        'println(size(m)==(4,3) && m[1,1]==1.0 && m[4,3]==12.0 ? "ALLOK" : "BAD $(m)")'))
    if (!any(grepl("ALLOK", res))) cat("JULIA OUTPUT:\n", paste(res, collapse = "\n"), "\n")
    expect_true(any(grepl("ALLOK", res)))
})

test_that("Julia FilesDaf reads a dafr-written store's metadata.json", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3(), "DAF >= 0.3.0 not available")
    root <- withr::local_tempdir(); path <- file.path(root, "s")
    d <- files_daf(path, mode = "w+", name = "s")
    add_axis(d, "cell", paste0("c", 1:5))
    set_vector(d, "cell", "v", as.numeric(1:5))
    set_scalar(d, "title", "hi")
    res <- run_julia(c("using DataAxesFormats",
        sprintf('d = FilesDaf(raw"%s", "r")', path),
        'ok = get_scalar(d, "title")=="hi" && get_vector(d,"cell","v")[5]==5.0',
        'println(ok ? "ALLOK" : "BAD")'))
    if (!any(grepl("ALLOK", res))) cat("JULIA OUTPUT:\n", paste(res, collapse = "\n"), "\n")
    expect_true(any(grepl("ALLOK", res)))
})

test_that("dafr FilesDaf reads a Julia-written store's metadata.json", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3(), "DAF >= 0.3.0 not available")
    root <- withr::local_tempdir(); path <- file.path(root, "j")
    res <- run_julia(c("using DataAxesFormats",
        sprintf('d = FilesDaf(raw"%s", "w"; name="j")', path),
        'add_axis!(d, "cell", ["c$(i)" for i in 1:5])',
        'set_vector!(d, "cell", "v", Float64.(1:5))',
        'set_scalar!(d, "title", "hi")', 'println("WROTE")'))
    skip_if_not(any(grepl("WROTE", res)), "julia write failed")
    dd <- files_daf(path, mode = "r")
    expect_equal(get_scalar(dd, "title"), "hi")
    expect_equal(as.numeric(get_vector(dd, "cell", "v")), as.numeric(1:5))
})
