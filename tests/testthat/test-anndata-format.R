.fixture_path <- function() {
    system.file("extdata", "small_test.h5ad", package = "dafr")
}

.make_rt_daf <- function() {
    d <- memory_daf(name = "rt")
    add_axis(d, "obs", c("o1", "o2", "o3"))
    add_axis(d, "var", c("v1", "v2"))
    set_matrix(d, "obs", "var", "UMIs", matrix(as.numeric(1:6), 3, 2))
    set_vector(d, "obs", "donor", c("A", "B", "A"))
    set_vector(d, "obs", "age", c(10L, 20L, 30L))
    set_vector(d, "var", "chrom", c("1", "X"))
    set_scalar(d, "organism", "human")
    d
}

# ---- Fixture load ----

test_that("h5ad_as_daf loads the small_test fixture", {
    skip_if_not_installed("hdf5r")
    p <- .fixture_path()
    skip_if(p == "", "fixture not available")

    d <- h5ad_as_daf(p, name = "loaded")
    expect_true(is_daf(d))
    expect_identical(daf_name(d), "loaded")

    expect_true(has_axis(d, "obs"))
    expect_true(has_axis(d, "var"))
    expect_identical(length(axis_entries(d, "obs")), 50L)
    expect_identical(length(axis_entries(d, "var")), 20L)

    expect_true(has_matrix(d, "obs", "var", "UMIs"))
    m <- get_matrix(d, "obs", "var", "UMIs")
    expect_identical(dim(m), c(50L, 20L))
    expect_type(m, "double")

    expect_true(all(c("donor", "age") %in% vectors_set(d, "obs")))
    expect_true("chrom" %in% vectors_set(d, "var"))
    expect_identical(get_scalar(d, "organism"), "human")
})

test_that("h5ad_as_daf default name derives from file basename", {
    skip_if_not_installed("hdf5r")
    p <- .fixture_path()
    skip_if(p == "", "fixture not available")
    d <- h5ad_as_daf(p)
    expect_identical(daf_name(d), "small_test")
})

test_that("h5ad_as_daf preserves obs column dtypes", {
    skip_if_not_installed("hdf5r")
    p <- .fixture_path()
    skip_if(p == "", "fixture not available")
    d <- h5ad_as_daf(p)
    # donor is character, age is integer
    donor <- get_vector(d, "obs", "donor")
    age <- get_vector(d, "obs", "age")
    expect_type(donor, "character")
    expect_type(age, "integer")
})

# ---- Round-trip write + read ----

test_that("daf_as_h5ad writes a Daf and round-trips axes/values", {
    skip_if_not_installed("hdf5r")
    d <- .make_rt_daf()
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)

    res <- daf_as_h5ad(d, p, obs_axis = "obs", var_axis = "var", x_name = "UMIs")
    expect_identical(res, p)
    expect_true(file.exists(p))

    d2 <- h5ad_as_daf(p, name = "back")
    expect_identical(axis_entries(d2, "obs"), c("o1", "o2", "o3"))
    expect_identical(axis_entries(d2, "var"), c("v1", "v2"))

    m <- get_matrix(d2, "obs", "var", "UMIs")
    expect_identical(dim(m), c(3L, 2L))
    expect_identical(as.numeric(m), as.numeric(1:6))

    expect_identical(unname(get_vector(d2, "obs", "donor")), c("A", "B", "A"))
    expect_identical(unname(get_vector(d2, "obs", "age")), c(10L, 20L, 30L))
    expect_identical(unname(get_vector(d2, "var", "chrom")), c("1", "X"))
    expect_identical(get_scalar(d2, "organism"), "human")
})

test_that("daf_as_h5ad writes extra matrices into /layers", {
    skip_if_not_installed("hdf5r")
    d <- .make_rt_daf()
    set_matrix(d, "obs", "var", "normalized",
        matrix(as.numeric(1:6) / 10, 3, 2))
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)
    daf_as_h5ad(d, p, obs_axis = "obs", var_axis = "var")

    d2 <- h5ad_as_daf(p)
    # UMIs is primary; normalized should be present as a layer (extra matrix)
    expect_true(has_matrix(d2, "obs", "var", "UMIs"))
    expect_true(has_matrix(d2, "obs", "var", "normalized"))
    m <- get_matrix(d2, "obs", "var", "normalized")
    expect_equal(as.numeric(m), as.numeric(1:6) / 10)
})

test_that("round-trip preserves dtypes (int vs double)", {
    skip_if_not_installed("hdf5r")
    d <- memory_daf(name = "dt")
    add_axis(d, "obs", c("o1", "o2"))
    add_axis(d, "var", c("v1"))
    # integer matrix
    set_matrix(d, "obs", "var", "UMIs", matrix(c(1L, 2L), 2, 1))
    # double vector + integer vector
    set_vector(d, "obs", "dbl", c(1.5, 2.5))
    set_vector(d, "obs", "int", c(7L, 8L))
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)
    daf_as_h5ad(d, p, obs_axis = "obs", var_axis = "var")

    d2 <- h5ad_as_daf(p)
    m <- get_matrix(d2, "obs", "var", "UMIs")
    expect_type(m, "integer")
    expect_type(get_vector(d2, "obs", "dbl"), "double")
    expect_type(get_vector(d2, "obs", "int"), "integer")
})

# ---- Overwrite guard ----

test_that("daf_as_h5ad errors on existing file without overwrite", {
    skip_if_not_installed("hdf5r")
    p <- tempfile(fileext = ".h5ad")
    file.create(p)
    on.exit(unlink(p), add = TRUE)
    d <- .make_rt_daf()
    expect_error(
        daf_as_h5ad(d, p, obs_axis = "obs", var_axis = "var", overwrite = FALSE),
        "file exists"
    )
})

test_that("daf_as_h5ad with overwrite = TRUE replaces an existing file", {
    skip_if_not_installed("hdf5r")
    p <- tempfile(fileext = ".h5ad")
    writeLines("garbage", p)
    on.exit(unlink(p), add = TRUE)
    d <- .make_rt_daf()
    expect_silent(
        daf_as_h5ad(d, p, obs_axis = "obs", var_axis = "var", overwrite = TRUE)
    )
    # Reading back should succeed — proving it was replaced with a valid h5ad.
    d2 <- h5ad_as_daf(p)
    expect_identical(length(axis_entries(d2, "obs")), 3L)
})

# ---- Input validation ----

test_that("h5ad_as_daf errors on missing file", {
    skip_if_not_installed("hdf5r")
    expect_error(
        h5ad_as_daf(tempfile(fileext = ".h5ad")),
        "file does not exist"
    )
})

test_that("daf_as_h5ad rejects non-daf input", {
    skip_if_not_installed("hdf5r")
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)
    expect_error(daf_as_h5ad(NULL, p), "DafReader")
})

# ---- hdf5r absence ----

test_that("h5ad_as_daf errors when hdf5r is not installed", {
    skip_if(requireNamespace("hdf5r", quietly = TRUE), "hdf5r installed")
    expect_error(h5ad_as_daf("/tmp/does-not-matter.h5ad"), "hdf5r")
})

test_that("daf_as_h5ad errors when hdf5r is not installed", {
    skip_if(requireNamespace("hdf5r", quietly = TRUE), "hdf5r installed")
    expect_error(daf_as_h5ad(memory_daf(), "/tmp/x.h5ad"), "hdf5r")
})

# ---- Slice 13: sparse X ----------------------------------------------------

.sparse_fixture_path <- function() {
    system.file("extdata", "sparse_test.h5ad", package = "dafr")
}

test_that("h5ad_as_daf reads CSR sparse X as a sparseMatrix", {
    skip_if_not_installed("hdf5r")
    skip_if_not_installed("Matrix")
    p <- .sparse_fixture_path()
    skip_if(p == "", "sparse fixture not available")

    d <- h5ad_as_daf(p)
    expect_true(is_daf(d))
    expect_identical(length(axis_entries(d, "obs")), 50L)
    expect_identical(length(axis_entries(d, "var")), 20L)

    X <- get_matrix(d, "obs", "var", "UMIs")
    expect_true(methods::is(X, "sparseMatrix"))
    expect_identical(dim(X), c(50L, 20L))
    # Fixture has nnz = 200 (density 0.20 on 50*20 = 1000 cells).
    expect_identical(length(X@x), 200L)
})

test_that("sparse X round-trips bit-identical via CSC write + CSR-compatible read", {
    skip_if_not_installed("hdf5r")
    skip_if_not_installed("Matrix")
    p <- .sparse_fixture_path()
    skip_if(p == "", "sparse fixture not available")

    d <- h5ad_as_daf(p)
    X <- get_matrix(d, "obs", "var", "UMIs")
    X_csc <- methods::as(X, "CsparseMatrix")

    tmp <- tempfile(fileext = ".h5ad")
    on.exit(unlink(tmp), add = TRUE)
    daf_as_h5ad(d, tmp, obs_axis = "obs", var_axis = "var")

    d2 <- h5ad_as_daf(tmp)
    X2 <- get_matrix(d2, "obs", "var", "UMIs")
    expect_true(methods::is(X2, "sparseMatrix"))
    expect_identical(dim(X2), dim(X_csc))
    expect_identical(as.numeric(X2@x), as.numeric(X_csc@x))
    expect_identical(as.integer(X2@i), as.integer(X_csc@i))
    expect_identical(as.integer(X2@p), as.integer(X_csc@p))
})

test_that("daf_as_h5ad writes sparse matrix without densifying", {
    skip_if_not_installed("hdf5r")
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "sp")
    add_axis(d, "obs", c("o1", "o2", "o3"))
    add_axis(d, "var", c("v1", "v2"))
    M <- Matrix::sparseMatrix(
        i = c(1L, 3L, 2L),
        j = c(1L, 1L, 2L),
        x = c(7, 9, 4),
        dims = c(3L, 2L),
        repr = "C"
    )
    set_matrix(d, "obs", "var", "UMIs", M)
    tmp <- tempfile(fileext = ".h5ad")
    on.exit(unlink(tmp), add = TRUE)
    daf_as_h5ad(d, tmp, obs_axis = "obs", var_axis = "var")

    # The on-disk /X should be a group (sparse) not a dataset.
    h5 <- hdf5r::H5File$new(tmp, mode = "r")
    x_obj <- h5[["X"]]
    expect_true(inherits(x_obj, "H5Group"))
    expect_identical(hdf5r::h5attr(x_obj, "encoding-type"), "csc_matrix")
    expect_identical(as.integer(hdf5r::h5attr(x_obj, "shape")), c(3L, 2L))
    h5$close_all()

    d2 <- h5ad_as_daf(tmp)
    M2 <- get_matrix(d2, "obs", "var", "UMIs")
    expect_true(methods::is(M2, "sparseMatrix"))
    expect_identical(as.numeric(M2@x), as.numeric(M@x))
    expect_identical(as.integer(M2@i), as.integer(M@i))
    expect_identical(as.integer(M2@p), as.integer(M@p))
})

test_that("sparse layers round-trip", {
    skip_if_not_installed("hdf5r")
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "spl")
    add_axis(d, "obs", c("o1", "o2"))
    add_axis(d, "var", c("v1", "v2"))
    set_matrix(d, "obs", "var", "UMIs", matrix(as.numeric(1:4), 2, 2))
    M <- Matrix::sparseMatrix(
        i = c(1L, 2L),
        j = c(1L, 2L),
        x = c(10, 20),
        dims = c(2L, 2L),
        repr = "C"
    )
    set_matrix(d, "obs", "var", "extra", M)
    tmp <- tempfile(fileext = ".h5ad")
    on.exit(unlink(tmp), add = TRUE)
    daf_as_h5ad(d, tmp, obs_axis = "obs", var_axis = "var")

    d2 <- h5ad_as_daf(tmp)
    expect_true(has_matrix(d2, "obs", "var", "extra"))
    M2 <- get_matrix(d2, "obs", "var", "extra")
    expect_true(methods::is(M2, "sparseMatrix"))
    expect_identical(as.numeric(M2@x), as.numeric(M@x))
})

# ---- Slice 13: categorical obs/var columns ---------------------------------

test_that("categorical obs column reads as a factor", {
    skip_if_not_installed("hdf5r")
    p <- .sparse_fixture_path()
    skip_if(p == "", "sparse fixture not available")
    d <- h5ad_as_daf(p)
    ct <- get_vector(d, "obs", "celltype")
    expect_true(is.factor(ct))
    expect_false(is.ordered(ct))
    expect_setequal(levels(ct), c("T", "B", "NK"))
    expect_identical(length(ct), 50L)
})

test_that("factor vector round-trips as a categorical group", {
    skip_if_not_installed("hdf5r")
    d <- memory_daf(name = "cat")
    add_axis(d, "obs", c("o1", "o2", "o3", "o4"))
    add_axis(d, "var", c("v1"))
    set_matrix(d, "obs", "var", "UMIs", matrix(as.numeric(1:4), 4, 1))
    set_vector(d, "obs", "ct",
        factor(c("A", "B", "A", "C"), levels = c("A", "B", "C")))
    tmp <- tempfile(fileext = ".h5ad")
    on.exit(unlink(tmp), add = TRUE)
    daf_as_h5ad(d, tmp, obs_axis = "obs", var_axis = "var")

    # On-disk check: /obs/ct is a group with encoding-type = categorical.
    h5 <- hdf5r::H5File$new(tmp, mode = "r")
    obs_grp <- h5[["obs"]]
    ct_grp <- obs_grp[["ct"]]
    expect_true(inherits(ct_grp, "H5Group"))
    expect_identical(hdf5r::h5attr(ct_grp, "encoding-type"), "categorical")
    expect_identical(ct_grp[["categories"]]$read(), c("A", "B", "C"))
    expect_identical(as.integer(ct_grp[["codes"]]$read()), c(0L, 1L, 0L, 2L))
    h5$close_all()

    d2 <- h5ad_as_daf(tmp)
    ct2 <- get_vector(d2, "obs", "ct")
    expect_true(is.factor(ct2))
    expect_identical(levels(ct2), c("A", "B", "C"))
    expect_identical(as.character(ct2), c("A", "B", "A", "C"))
})

test_that("ordered factor preserves ordering on round-trip", {
    skip_if_not_installed("hdf5r")
    d <- memory_daf(name = "ord")
    add_axis(d, "obs", c("o1", "o2", "o3"))
    add_axis(d, "var", c("v1"))
    set_matrix(d, "obs", "var", "UMIs", matrix(as.numeric(1:3), 3, 1))
    set_vector(d, "obs", "grade",
        ordered(c("low", "high", "mid"), levels = c("low", "mid", "high")))
    tmp <- tempfile(fileext = ".h5ad")
    on.exit(unlink(tmp), add = TRUE)
    daf_as_h5ad(d, tmp, obs_axis = "obs", var_axis = "var")

    d2 <- h5ad_as_daf(tmp)
    g <- get_vector(d2, "obs", "grade")
    expect_true(is.ordered(g))
    expect_identical(levels(g), c("low", "mid", "high"))
    expect_identical(as.character(g), c("low", "high", "mid"))
})

test_that("categorical NA codes (-1) become NA on read", {
    skip_if_not_installed("hdf5r")
    # Hand-craft an h5ad with an explicit -1 categorical code.
    tmp <- tempfile(fileext = ".h5ad")
    on.exit(unlink(tmp), add = TRUE)
    h5 <- hdf5r::H5File$new(tmp, mode = "w")
    h5$create_attr("encoding-type", robj = "anndata",
        space = hdf5r::H5S$new("scalar"))
    h5$create_attr("encoding-version", robj = "0.1.0",
        space = hdf5r::H5S$new("scalar"))
    h5$create_dataset("X", robj = matrix(as.numeric(1:3), 3, 1))
    obs <- h5$create_group("obs")
    obs$create_dataset("_index", robj = c("a", "b", "c"))
    ct <- obs$create_group("ct")
    ct$create_attr("encoding-type", robj = "categorical",
        space = hdf5r::H5S$new("scalar"))
    ct$create_attr("encoding-version", robj = "0.2.0",
        space = hdf5r::H5S$new("scalar"))
    ct$create_attr("ordered", robj = FALSE)
    ct$create_dataset("codes", robj = c(0L, -1L, 1L))
    ct$create_dataset("categories", robj = c("X", "Y"))
    var <- h5$create_group("var")
    var$create_dataset("_index", robj = "v1")
    h5$close_all()

    d <- h5ad_as_daf(tmp)
    v <- get_vector(d, "obs", "ct")
    expect_true(is.factor(v))
    expect_true(is.na(v[2L]))
    expect_identical(as.character(v[1L]), "X")
    expect_identical(as.character(v[3L]), "Y")
})

# ---- Slice 13: nested uns --------------------------------------------------

test_that("nested uns groups flatten via '_' separator", {
    skip_if_not_installed("hdf5r")
    p <- .sparse_fixture_path()
    skip_if(p == "", "sparse fixture not available")
    d <- h5ad_as_daf(p)
    scs <- scalars_set(d)
    expect_true("params_seed" %in% scs)
    expect_true("params_normalization" %in% scs)
    expect_true("organism" %in% scs)
    expect_identical(as.integer(get_scalar(d, "params_seed")), 42L)
    expect_identical(get_scalar(d, "params_normalization"), "log1p")
    expect_identical(get_scalar(d, "organism"), "human")
})

test_that("nested uns: multi-level nesting flattens with '_'", {
    skip_if_not_installed("hdf5r")
    tmp <- tempfile(fileext = ".h5ad")
    on.exit(unlink(tmp), add = TRUE)
    h5 <- hdf5r::H5File$new(tmp, mode = "w")
    h5$create_attr("encoding-type", robj = "anndata",
        space = hdf5r::H5S$new("scalar"))
    h5$create_attr("encoding-version", robj = "0.1.0",
        space = hdf5r::H5S$new("scalar"))
    h5$create_dataset("X", robj = matrix(as.numeric(1:2), 2, 1))
    obs <- h5$create_group("obs")
    obs$create_dataset("_index", robj = c("a", "b"))
    var <- h5$create_group("var")
    var$create_dataset("_index", robj = "v1")
    uns <- h5$create_group("uns")
    a <- uns$create_group("a")
    b <- a$create_group("b")
    b$create_dataset("c", robj = 99L)
    uns$create_dataset("top", robj = "t")
    h5$close_all()

    d <- h5ad_as_daf(tmp)
    expect_identical(as.integer(get_scalar(d, "a_b_c")), 99L)
    expect_identical(get_scalar(d, "top"), "t")
})

test_that("nested uns: non-scalar leaves are skipped", {
    skip_if_not_installed("hdf5r")
    tmp <- tempfile(fileext = ".h5ad")
    on.exit(unlink(tmp), add = TRUE)
    h5 <- hdf5r::H5File$new(tmp, mode = "w")
    h5$create_attr("encoding-type", robj = "anndata",
        space = hdf5r::H5S$new("scalar"))
    h5$create_attr("encoding-version", robj = "0.1.0",
        space = hdf5r::H5S$new("scalar"))
    h5$create_dataset("X", robj = matrix(as.numeric(1:2), 2, 1))
    obs <- h5$create_group("obs")
    obs$create_dataset("_index", robj = c("a", "b"))
    var <- h5$create_group("var")
    var$create_dataset("_index", robj = "v1")
    uns <- h5$create_group("uns")
    inner <- uns$create_group("inner")
    # Non-scalar entries under /uns/inner — should be silently dropped.
    inner$create_dataset("array", robj = c(1L, 2L, 3L))
    inner$create_dataset("scalar", robj = 7L)
    h5$close_all()

    d <- h5ad_as_daf(tmp)
    scs <- scalars_set(d)
    expect_true("inner_scalar" %in% scs)
    expect_false("inner_array" %in% scs)
    expect_identical(as.integer(get_scalar(d, "inner_scalar")), 7L)
})
