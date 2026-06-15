# Parity fix: dense /X (and dense layers) must use the AnnData (n_obs, n_var)
# C-order convention, so dafr interoperates with the wider AnnData ecosystem
# (Python anndata / R anndata / scanpy). Previously dafr wrote/read the dense
# matrix with no transpose, producing a (n_var, n_obs) on-disk /X: a real
# scanpy/anndata file failed to load (dim mismatch) and dafr-written files were
# transposed. Audit probe anndata-dense-X-orientation.
#
# The fixture below was written by Python anndata (committed) so this verifies
# alignment against the REAL convention, not just dafr reading its own output.

test_that("h5ad_as_daf reads a real Python-anndata /X in the correct orientation", {
    skip_if_not_installed("hdf5r")
    p <- testthat::test_path("fixtures", "anndata_canonical.h5ad")
    skip_if(!file.exists(p), "canonical fixture missing")

    d <- h5ad_as_daf(p)
    expect_identical(axis_vector(d, "obs"), c("o1", "o2", "o3"))
    expect_identical(axis_vector(d, "var"), c("v1", "v2"))

    m <- as.matrix(get_matrix(d, "obs", "var", "UMIs"))
    expect_identical(dim(m), c(3L, 2L))
    # X[obs, var]: o2/v1 = 21, o1/v2 = 12 (would be swapped if transposed)
    expect_equal(unname(m), matrix(c(11, 21, 31, 12, 22, 32), 3L, 2L))
})

test_that("daf_as_h5ad round-trips dense /X in AnnData orientation", {
    skip_if_not_installed("hdf5r")
    d <- memory_daf()
    add_axis(d, "cell", c("o1", "o2", "o3"))
    add_axis(d, "gene", c("v1", "v2"))
    set_matrix(d, "cell", "gene", "UMIs",
               matrix(c(11, 21, 31, 12, 22, 32), 3L, 2L), relayout = FALSE)

    p <- tempfile(fileext = ".h5ad")
    daf_as_h5ad(d, p, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")

    # Read back through dafr: with read+write both canonical, values are recovered.
    d2 <- h5ad_as_daf(p)
    m2 <- as.matrix(get_matrix(d2, "obs", "var", "UMIs"))
    expect_equal(unname(m2), matrix(c(11, 21, 31, 12, 22, 32), 3L, 2L))
    expect_identical(axis_vector(d2, "obs"), c("o1", "o2", "o3"))
    expect_identical(axis_vector(d2, "var"), c("v1", "v2"))

    # And the on-disk /X is the canonical (n_obs, n_var): hdf5r presents it
    # reversed as (n_var, n_obs), so a (3,2) Daf matrix lands as 2x3 in hdf5r.
    h <- hdf5r::H5File$new(p, "r")
    on.exit(h$close_all(), add = TRUE)
    expect_identical(h[["X"]]$dims, c(2L, 3L))
})

test_that("obsm/varm round-trip in AnnData (n_axis, k) orientation", {
    skip_if_not_installed("hdf5r")
    d <- memory_daf()
    add_axis(d, "cell", c("o1", "o2", "o3"))
    add_axis(d, "gene", c("v1", "v2"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(seq_len(6), 3L, 2L),
               relayout = FALSE)
    add_axis(d, "obsm_emb_dim", c("1", "2"))
    set_matrix(d, "cell", "obsm_emb_dim", "emb",
               matrix(c(11, 21, 31, 12, 22, 32), 3L, 2L), relayout = FALSE)

    p <- tempfile(fileext = ".h5ad")
    daf_as_h5ad(d, p, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")

    d2 <- h5ad_as_daf(p)
    expect_equal(
        unname(as.matrix(get_matrix(d2, "obs", "obsm_emb_dim", "emb"))),
        matrix(c(11, 21, 31, 12, 22, 32), 3L, 2L))

    # on-disk /obsm/emb is canonical (n_obs=3, k=2); hdf5r reads it reversed 2x3
    h <- hdf5r::H5File$new(p, "r")
    on.exit(h$close_all(), add = TRUE)
    expect_identical(h[["obsm/emb"]]$dims, c(2L, 3L))
})
