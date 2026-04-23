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
