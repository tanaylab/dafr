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
    complete_chain(
        base_daf = base_daf(base,
            axes = list(list("cell", "="), list("gene", "=")),
            data = list(list(c("cell", "gene", "expr"), "="))),
        new_daf = new, absolute = TRUE)
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

# The other direction: DataAxesFormats writes a DAG of repositories and dafr
# reopens it. Gated on the feature rather than on a version, since the DAG
# rework is unreleased at the time of writing (Project.toml still says 0.3.0).
.daf_jl_has_base_daf <- function() {
    if (!.have_julia_env()) return(FALSE)
    out <- run_julia(c(
        "using DataAxesFormats",
        'println(isdefined(DataAxesFormats, :BaseDaf) ? "HASBASEDAF" : "NOBASEDAF")'
    ))
    any(grepl("^HASBASEDAF$", out))
}

test_that("dafr complete_daf reads a Julia-written DAG of repositories", {
    skip_on_cran()
    skip_if_not(.daf_jl_has_base_daf(), "DataAxesFormats without BaseDaf")
    root <- withr::local_tempdir()
    res <- run_julia(c(
        "using DataAxesFormats",
        sprintf('root = raw"%s"', root),
        'cells = FilesDaf("$(root)/cells", "w"; name="cells!")',
        'add_axis!(cells, "cell", ["A", "B", "C"])',
        'add_axis!(cells, "gene", ["X", "Y"])',
        'set_vector!(cells, "cell", "age", [10, 20, 30])',
        'results = FilesDaf("$(root)/results", "w"; name="results!")',
        'rc = complete_chain!(; base_daf=cells, new_daf=results)',
        'set_vector!(rc, "cell", "score", [1.0, 2.0, 3.0])',
        'masks = FilesDaf("$(root)/masks", "w"; name="masks!")',
        'mc = complete_chain!(; base_daf=cells, new_daf=masks)',
        'set_vector!(mc, "gene", "is_marker", [true, false])',
        'leaf = FilesDaf("$(root)/leaf", "w"; name="leaf!")',
        'complete_chain!(; base_daf=[rc, mc], new_daf=leaf)',
        'viewed = FilesDaf("$(root)/viewed", "w"; name="viewed!")',
        'complete_chain!(; base_daf=BaseDaf(; daf=cells, axes=["cell" => "="]), new_daf=viewed)',
        'println("WROTE")'))
    if (!any(grepl("WROTE", res))) cat("JULIA OUTPUT:\n", paste(res, collapse = "\n"), "\n")
    skip_if_not(any(grepl("WROTE", res)), "Julia did not write the DAG")

    # The cells are reached through both arms and appear once, before both.
    # A reopened repository is named after its directory; Julia's `name=` is an
    # in-memory label and is not stored.
    chain <- complete_daf(file.path(root, "leaf"), name = "reopened!")
    expect_identical(
        vapply(dafr:::.chain_dafs(chain), function(d) S7::prop(d, "name"), character(1)),
        c("cells", "results", "masks", "leaf")
    )
    # Julia's Int64 reads back as bit64::integer64, as everywhere else in dafr.
    expect_equal(as.integer(unname(get_vector(chain, "cell", "age"))), c(10L, 20L, 30L))
    expect_identical(unname(get_vector(chain, "cell", "score")), c(1, 2, 3))
    expect_identical(unname(get_vector(chain, "gene", "is_marker")), c(TRUE, FALSE))

    # Julia writes a viewed base as an array of one object, keys in its own
    # order; dafr applies the view on reopen.
    viewed <- complete_daf(file.path(root, "viewed"), name = "viewed!")
    expect_setequal(axes_set(viewed), "cell")
    expect_false(has_axis(viewed, "gene"))
})
