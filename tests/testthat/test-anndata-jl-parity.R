# Literal port of anndata.jl into R. 16 leaves.
#
# Julia tests in-memory AnnData<->Daf conversion via daf_as_anndata /
# anndata_as_daf. dafr's anndata bridge is FILE-based:
# h5ad_as_daf(path) and daf_as_h5ad(daf, path). dafr does not have an
# in-memory AnnData type because R's anndata representation is via
# Python's anndata.AnnData (through reticulate) - not first-class
# in dafr. Skip in-memory leaves with CA9.
#
# The `file` leaf, which Julia tests via mktempdir + h5ad roundtrip, is
# covered by dafr's existing test-anndata-format.R; ported here for
# Julia-traceability with a roundtrip assertion.

# in-memory paths skip
test_that("anndata / daf_as_anndata", {
    skip("R divergence CA9: dafr's anndata bridge is file-based (h5ad_as_daf / daf_as_h5ad). No in-memory AnnData type.")
})

test_that("anndata / anndata_as_daf / name", {
    skip("R divergence CA9: file-based bridge only")
})
test_that("anndata / anndata_as_daf / !name", {
    skip("R divergence CA9: file-based bridge only")
})
test_that("anndata / anndata_as_daf / +name", {
    skip("R divergence CA9: file-based bridge only")
})
test_that("anndata / anndata_as_daf / unsupported / mapping / ignore", {
    skip("R divergence CA9: file-based bridge; unsupported_handler equivalent surfaces only at h5ad load")
})
test_that("anndata / anndata_as_daf / unsupported / mapping / warn", {
    skip("R divergence CA9")
})
test_that("anndata / anndata_as_daf / unsupported / mapping / error", {
    skip("R divergence CA9")
})
test_that("anndata / anndata_as_daf / unknown_axis / ignore", {
    skip("R divergence CA9")
})
test_that("anndata / anndata_as_daf / unknown_axis / warn", {
    skip("R divergence CA9")
})
test_that("anndata / anndata_as_daf / unknown_axis / error", {
    skip("R divergence CA9")
})

# file roundtrip
test_that("anndata / file", {
    if (!requireNamespace("hdf5r", quietly = TRUE)) {
        skip("hdf5r not installed - dafr's anndata bridge requires it")
    }

    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
        dimnames = list(c("A", "B"), c("X", "Y", "Z"))
    ))
    set_vector(d, "cell", "age", c(0L, 1L))

    tmpdir <- tempfile("anndata-parity-")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
    path <- file.path(tmpdir, "test.h5ad")

    daf_as_h5ad(d, path, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_true(file.exists(path))

    # h5ad_as_daf renames axes to "obs" / "var" (h5ad convention).
    # dafr's anndata bridge uses obs/var as the canonical axis names;
    # original axis names ("cell"/"gene") are stored as scalars on the
    # round-trip but not auto-renamed back.
    back <- h5ad_as_daf(path, name = "back!")
    expect_setequal(axes_set(back), c("obs", "var"))
    expect_identical(unname(axis_vector(back, "obs")), c("A", "B"))
    expect_identical(unname(axis_vector(back, "var")), c("X", "Y", "Z"))
    # The X matrix should round-trip (dafr stores it under x_name = "UMIs")
    expect_true(has_matrix(back, "obs", "var", "UMIs"))
    res <- get_matrix(back, "obs", "var", "UMIs")
    expect_identical(as.numeric(unname(as.matrix(res))),
                     as.numeric(c(0, 3, 1, 4, 2, 5)))
})
