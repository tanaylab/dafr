# Tests for unsupported_handler dispatch in h5ad_as_daf.
# We craft a tiny h5ad containing a group-valued obs column with an
# unrecognised encoding-type — an unsupported feature that triggers the
# inefficient-action handler per Slice-10b §3.15. (Nested uns groups
# were lifted out of the unsupported set in slice-13.)

.write_unsupported_h5ad <- function(path) {
    h5 <- hdf5r::H5File$new(path, mode = "w")
    on.exit(h5$close_all())
    # Axes
    obs <- h5$create_group("obs")
    obs$create_dataset("_index", robj = c("o1", "o2"))
    # Group-valued obs column with a non-categorical (hence unsupported)
    # encoding — triggers the handler.
    fancy <- obs$create_group("fancy")
    fancy$create_attr("encoding-type", robj = "mystery_encoding",
        space = hdf5r::H5S$new("scalar"))
    fancy$create_dataset("payload", robj = c(1L, 2L))
    var <- h5$create_group("var")
    var$create_dataset("_index", robj = c("v1", "v2"))
    # X
    h5$create_dataset("X", robj = matrix(1.0, 2, 2))
    invisible(path)
}

# ---- WARN_HANDLER default ----

test_that("WARN_HANDLER (default) warns on unsupported feature", {
    skip_if_not_installed("hdf5r")
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)
    .write_unsupported_h5ad(p)

    expect_warning(h5ad_as_daf(p), "nested obs column")
})

# ---- IGNORE_HANDLER ----

test_that("IGNORE_HANDLER silences unsupported-feature messages", {
    skip_if_not_installed("hdf5r")
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)
    .write_unsupported_h5ad(p)

    expect_silent(d <- h5ad_as_daf(p, unsupported_handler = IGNORE_HANDLER))
    # And the load still succeeded
    expect_true(is_daf(d))
})

# ---- ERROR_HANDLER ----

test_that("ERROR_HANDLER converts unsupported feature into an error", {
    skip_if_not_installed("hdf5r")
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)
    .write_unsupported_h5ad(p)

    expect_error(
        h5ad_as_daf(p, unsupported_handler = ERROR_HANDLER),
        "nested obs column"
    )
})

# ---- Handler restoration on exit ----

test_that("h5ad_as_daf restores prior inefficient handler on exit", {
    skip_if_not_installed("hdf5r")
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)
    .write_unsupported_h5ad(p)

    # Establish a known handler beforehand.
    inefficient_action_handler(IGNORE_HANDLER)
    # Call with a different handler; should restore IGNORE afterward.
    suppressWarnings(h5ad_as_daf(p, unsupported_handler = WARN_HANDLER))

    restored <- get("inefficient", envir = dafr:::.dafr_handlers,
                    inherits = FALSE)
    expect_identical(restored, IGNORE_HANDLER)
})

test_that("daf_as_h5ad restores prior inefficient handler on exit", {
    skip_if_not_installed("hdf5r")
    d <- memory_daf()
    add_axis(d, "obs", c("o1", "o2"))
    add_axis(d, "var", c("v1"))
    set_matrix(d, "obs", "var", "UMIs", matrix(1.0, 2, 1))
    p <- tempfile(fileext = ".h5ad")
    on.exit(unlink(p), add = TRUE)

    inefficient_action_handler(IGNORE_HANDLER)
    daf_as_h5ad(d, p, obs_axis = "obs", var_axis = "var",
                unsupported_handler = WARN_HANDLER)
    restored <- get("inefficient", envir = dafr:::.dafr_handlers,
                    inherits = FALSE)
    expect_identical(restored, IGNORE_HANDLER)
})
