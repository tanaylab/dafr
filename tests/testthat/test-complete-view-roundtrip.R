test_that("complete_daf re-applies base_daf_view on reopen", {
    skip_if_not_installed("jsonlite")

    base_dir <- withr::local_tempdir()
    new_dir <- withr::local_tempdir()

    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", paste0("c", 1:5))
    add_axis(base, "gene", paste0("g", 1:4))
    set_matrix(base, "cell", "gene", "umi", matrix(seq_len(20L), 5L, 4L))

    new <- files_daf(new_dir, name = "new", mode = "w+")

    # viewer-compatible axes/data format: list-of-lists
    axes_spec <- list(list("cell", "="), list("gene", "="))
    data_spec <- list(list(c("cell", "gene", "umi"), "="))

    complete_chain(base_daf = base, new_daf = new,
                   axes = axes_spec, data = data_spec, absolute = TRUE)

    reopened <- complete_daf(new_dir)

    expect_true(inherits(reopened, "dafr::ViewDaf"))
    expect_setequal(format_axes_set(reopened), c("cell", "gene"))

    v1 <- format_get_matrix(reopened, "cell", "gene", "umi")$value
    v2 <- format_get_matrix(base, "cell", "gene", "umi")$value
    expect_equal(unname(as.matrix(v1)), unname(as.matrix(v2)))
})

test_that("complete_daf round-trips a renamed-axis view", {
    skip_if_not_installed("jsonlite")

    base_dir <- withr::local_tempdir()
    new_dir <- withr::local_tempdir()

    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1", "c2", "c3"))
    set_vector(base, "cell", "umi_count", c(10L, 20L, 30L))

    new <- files_daf(new_dir, name = "new", mode = "w+")

    # Axis-rename arg shape: list(list(view_name, base_axis_query)).
    # The base axis name is extracted from the query by query_axis_name().
    axes_spec <- list(list("renamed_cell", "@ cell"))

    complete_chain(base_daf = base, new_daf = new,
                   axes = axes_spec, absolute = TRUE)

    reopened <- complete_daf(new_dir)

    expect_true(inherits(reopened, "dafr::ViewDaf"))
    expect_setequal(format_axes_set(reopened), "renamed_cell")
    expect_true(has_axis(reopened, "renamed_cell"))
    expect_false(has_axis(reopened, "cell"))
    expect_equal(axis_vector(reopened, "renamed_cell"), c("c1", "c2", "c3"))
    expect_equal(
        as.vector(get_vector(reopened, "renamed_cell", "umi_count")),
        c(10L, 20L, 30L)
    )
})

test_that("complete_daf without view returns plain chain", {
    base_dir <- withr::local_tempdir()
    new_dir <- withr::local_tempdir()

    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", paste0("c", 1:3))

    new <- files_daf(new_dir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    reopened <- complete_daf(new_dir)
    expect_false(inherits(reopened, "dafr::ViewDaf"))
})
