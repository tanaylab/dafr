# Live interop: DataAxesFormats.jl reads dafr-written stores/chains via the
# shared JSON schemas. Skips unless the Julia env is DAF >= 0.3.0.

test_that("Julia complete_daf reads a dafr-written chain view", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3())
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
