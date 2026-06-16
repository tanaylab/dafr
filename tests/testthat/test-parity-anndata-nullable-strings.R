# Parity fix: anndata >= 0.12 stores strings as the `nullable-string-array`
# encoding - a {values, mask} GROUP rather than a plain `string-array` dataset.
# This affects the obs/var `_index`, categorical `categories`, and plain pandas
# `string`-dtype columns. The pre-0.4.7 reader called `$read()` on the group
# (failing with "attempt to apply non-function" for `_index`/categories) and
# silently skipped plain string columns. Audit probe nullable-string-array.
#
# The fixture is a REAL Python-anndata 0.12.1 file (see
# dev/fixtures/generate_anndata_nullable_strings.py), so this verifies alignment
# against the actual on-disk encoding, not just dafr reading its own output.

test_that("h5ad_as_daf reads a real anndata>=0.12 nullable-string-array file", {
    skip_if_not_installed("hdf5r")
    p <- testthat::test_path("fixtures", "anndata_nullable_strings.h5ad")
    skip_if(!file.exists(p), "nullable-strings fixture missing")

    d <- h5ad_as_daf(p)

    # obs/var `_index` are nullable-string-array groups, must still yield names.
    expect_identical(axis_vector(d, "obs"), c("o1", "o2", "o3"))
    expect_identical(axis_vector(d, "var"), c("v1", "v2"))

    # Categorical column: `categories` stored as a nullable-string-array group.
    ct <- get_vector(d, "obs", "cell_type")
    expect_true(is.factor(ct))
    expect_identical(levels(ct), c("B", "T"))
    expect_identical(as.character(ct), c("B", "T", "B"))

    # Plain pandas `string` column: a nullable-string-array group, read as a
    # character vector (previously skipped as an unsupported nested column).
    batch <- get_vector(d, "obs", "batch")
    expect_false(is.factor(batch))
    expect_identical(unname(batch), c("x", "y", "z"))

    # Plain float column is unaffected by the string-encoding change.
    expect_equal(unname(get_vector(d, "obs", "score")), c(1.5, 2.5, 3.5))
})

test_that("h5ad_as_daf reads nullable-integer / nullable-boolean columns (mask -> NA)", {
    skip_if_not_installed("hdf5r")
    p <- testthat::test_path("fixtures", "anndata_nullable_strings.h5ad")
    skip_if(!file.exists(p), "nullable-strings fixture missing")

    d <- h5ad_as_daf(p)

    # pandas Int64 column with a missing value -> `nullable-integer` group.
    n_umis <- get_vector(d, "obs", "n_umis")
    expect_type(n_umis, "integer")
    expect_identical(unname(n_umis), c(10L, NA, 30L))

    # pandas boolean column with a missing value -> `nullable-boolean` group.
    is_doublet <- get_vector(d, "obs", "is_doublet")
    expect_type(is_doublet, "logical")
    expect_identical(unname(is_doublet), c(TRUE, NA, FALSE))
})

test_that(".read_h5ad_string_array maps a masked nullable-string entry to NA", {
    skip_if_not_installed("hdf5r")
    # Hand-craft a nullable-string-array group with one masked (TRUE) entry.
    # anndata rewrites a string column that contains a missing value as
    # categorical, so the mask -> NA branch is exercised directly here.
    tmp <- tempfile(fileext = ".h5")
    on.exit(unlink(tmp), add = TRUE)
    h5 <- hdf5r::H5File$new(tmp, mode = "w")
    grp <- h5$create_group("col")
    grp$create_attr("encoding-type", robj = "nullable-string-array",
        space = hdf5r::H5S$new("scalar"))
    grp$create_dataset("values", robj = c("x", "REPLACED", "z"))
    grp$create_dataset("mask", robj = c(FALSE, TRUE, FALSE))

    got <- dafr:::.read_h5ad_string_array(h5[["col"]])
    h5$close_all()
    expect_identical(got, c("x", NA, "z"))
})
