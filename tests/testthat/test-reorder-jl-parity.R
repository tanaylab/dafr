# Literal port of ~/src/DataAxesFormats.jl/test/reorder.jl into R.
#
# Each `nested_test` leaf in reorder.jl becomes one `test_that` here, named
# with the Julia path. Each leaf does its own setup, mirroring Julia's
# fresh-state semantics.
#
# Divergences carry skip("R divergence: <id>: <reason>") naming a gap from
#   dev/notes/2026-05-07-reorder-jl-parity-divergences.md.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Mirrors populate_reorder_test_data!. Three cells, four genes; dense
# vectors + sparse vector + dense + sparse matrices + dense + sparse text.
.populate_reorder_test_data <- function(d) {
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y", "Z", "W"))

    set_vector(d, "cell", "age", c(10L, 20L, 30L))
    # memory_daf rejects Matrix::sparseVector (M5 from concat slice); store
    # marker as dense integer here. The structural assertion (post-permute
    # values) is what matters; sparsity preservation is its own divergence.
    set_vector(d, "gene", "marker", c(1L, 0L, 1L, 0L))
    set_vector(d, "cell", "color", c("aa", "bb", "cc"))
    set_vector(d, "gene", "label", c("", "", "", "hi"))

    set_matrix(d, "cell", "gene", "UMIs",
               matrix(c(1L, 5L, 9L, 2L, 6L, 10L, 3L, 7L, 11L, 4L, 8L, 12L),
                      nrow = 3L, ncol = 4L,
                      dimnames = list(c("A", "B", "C"),
                                      c("X", "Y", "Z", "W"))))
    set_matrix(d, "cell", "gene", "sparse",
               methods::as(matrix(c(1, 0, 9, 0, 6, 0, 3, 0, 0, 0, 8, 12),
                                  nrow = 3L, ncol = 4L,
                                  dimnames = list(c("A", "B", "C"),
                                                  c("X", "Y", "Z", "W"))),
                           "dgCMatrix"))
    set_matrix(d, "cell", "gene", "annotation",
               matrix(c("a", "e", "i", "b", "f", "j", "c", "g", "k", "d", "h", "l"),
                      nrow = 3L, ncol = 4L,
                      dimnames = list(c("A", "B", "C"),
                                      c("X", "Y", "Z", "W"))))
    set_matrix(d, "cell", "gene", "sparse_text",
               matrix(c("", "", "", "", "", "", "", "", "", "", "", "ab"),
                      nrow = 3L, ncol = 4L,
                      dimnames = list(c("A", "B", "C"),
                                      c("X", "Y", "Z", "W"))))
    invisible(d)
}

# Asserts the post-permute state for "both axes" (cell = [3,1,2], gene =
# [4,3,2,1]).
.assert_reorder_both_axes <- function(d) {
    expect_equal(unname(axis_vector(d, "cell")), c("C", "A", "B"))
    expect_equal(unname(axis_vector(d, "gene")), c("W", "Z", "Y", "X"))
    expect_equal(unname(get_vector(d, "cell", "age")), c(30L, 10L, 20L))
    expect_equal(unname(get_vector(d, "gene", "marker")), c(0L, 1L, 0L, 1L))
    expect_equal(unname(get_vector(d, "cell", "color")), c("cc", "aa", "bb"))
    expect_equal(unname(get_vector(d, "gene", "label")),
                 c("hi", "", "", ""))
    expect_equal(unname(as.matrix(get_matrix(d, "cell", "gene", "UMIs"))),
                 matrix(c(12L, 4L, 8L, 11L, 3L, 7L, 10L, 2L, 6L, 9L, 1L, 5L),
                        nrow = 3L, ncol = 4L))
    expect_equal(unname(as.matrix(get_matrix(d, "cell", "gene", "sparse"))),
                 matrix(c(12, 0, 8, 0, 3, 0, 0, 0, 6, 9, 1, 0),
                        nrow = 3L, ncol = 4L))
    expect_equal(unname(as.matrix(get_matrix(d, "cell", "gene", "annotation"))),
                 matrix(c("l", "d", "h", "k", "c", "g", "j", "b", "f",
                          "i", "a", "e"),
                        nrow = 3L, ncol = 4L))
    expect_equal(
        unname(as.matrix(get_matrix(d, "cell", "gene", "sparse_text"))),
        matrix(c("ab", "", "", "", "", "", "", "", "", "", "", ""),
               nrow = 3L, ncol = 4L)
    )
}

.assert_reorder_single_axis <- function(d) {
    expect_equal(unname(axis_vector(d, "cell")), c("C", "A", "B"))
    expect_equal(unname(axis_vector(d, "gene")), c("X", "Y", "Z", "W"))
    expect_equal(unname(get_vector(d, "cell", "age")), c(30L, 10L, 20L))
    expect_equal(unname(get_vector(d, "gene", "marker")), c(1L, 0L, 1L, 0L))
    expect_equal(unname(get_vector(d, "cell", "color")), c("cc", "aa", "bb"))
    expect_equal(unname(get_vector(d, "gene", "label")),
                 c("", "", "", "hi"))
    expect_equal(unname(as.matrix(get_matrix(d, "cell", "gene", "UMIs"))),
                 matrix(c(9L, 1L, 5L, 10L, 2L, 6L, 11L, 3L, 7L, 12L, 4L, 8L),
                        nrow = 3L, ncol = 4L))
    expect_equal(unname(as.matrix(get_matrix(d, "cell", "gene", "sparse"))),
                 matrix(c(9, 1, 0, 0, 0, 6, 0, 3, 0, 12, 0, 8),
                        nrow = 3L, ncol = 4L))
    expect_equal(unname(as.matrix(get_matrix(d, "cell", "gene", "annotation"))),
                 matrix(c("i", "a", "e", "j", "b", "f", "k", "c", "g",
                          "l", "d", "h"),
                        nrow = 3L, ncol = 4L))
    expect_equal(
        unname(as.matrix(get_matrix(d, "cell", "gene", "sparse_text"))),
        matrix(c("", "", "", "", "", "", "", "", "", "ab", "", ""),
               nrow = 3L, ncol = 4L)
    )
}

.assert_original_data <- function(d) {
    expect_equal(unname(axis_vector(d, "cell")), c("A", "B", "C"))
    expect_equal(unname(axis_vector(d, "gene")), c("X", "Y", "Z", "W"))
    expect_equal(unname(get_vector(d, "cell", "age")), c(10L, 20L, 30L))
    expect_equal(unname(get_vector(d, "gene", "marker")), c(1L, 0L, 1L, 0L))
    expect_equal(unname(get_vector(d, "cell", "color")), c("aa", "bb", "cc"))
    expect_equal(unname(get_vector(d, "gene", "label")), c("", "", "", "hi"))
    expect_equal(unname(as.matrix(get_matrix(d, "cell", "gene", "UMIs"))),
                 matrix(c(1L, 5L, 9L, 2L, 6L, 10L, 3L, 7L, 11L, 4L, 8L, 12L),
                        nrow = 3L, ncol = 4L))
    expect_equal(
        unname(as.matrix(get_matrix(d, "cell", "gene", "sparse_text"))),
        matrix(c("", "", "", "", "", "", "", "", "", "", "", "ab"),
               nrow = 3L, ncol = 4L)
    )
}

# The R analog of test_crash_recovery!: drive reorder_axes with
# crash_counter that fires after `crash_after` ticks, then reset (must
# roll back, returning TRUE — memory and files backends both restore
# from their pre-reorder backup), then re-run reorder cleanly and
# assert.
.test_crash_recovery <- function(d, crash_after) {
    counter <- new.env(parent = emptyenv())
    counter$n <- crash_after
    expect_error(
        reorder_axes(d, cell = c(3L, 1L, 2L), gene = c(4L, 3L, 2L, 1L),
                     crash_counter = counter)
    )
    expect_true(reset_reorder_axes(d))
    .assert_original_data(d)
    reorder_axes(d, cell = c(3L, 1L, 2L), gene = c(4L, 3L, 2L, 1L))
    .assert_reorder_both_axes(d)
}

# ---------------------------------------------------------------------------
# reorder / is_leaf
# ---------------------------------------------------------------------------

test_that("reorder / is_leaf / types", {
    expect_true(is_leaf(MemoryDaf))
    expect_true(is_leaf(FilesDaf))
    expect_true(is_leaf(ZarrDaf))
    expect_false(is_leaf(DafReader))
    expect_false(is_leaf(DafWriter))
})

test_that("reorder / is_leaf / memory", {
    expect_true(is_leaf(memory_daf(name = "memory!")))
})

test_that("reorder / is_leaf / files", {
    tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    d <- files_daf(tmp, mode = "w", name = "files!")
    expect_true(is_leaf(d))
})

test_that("reorder / is_leaf / h5df", {
    skip("R divergence R3: dafr does not have an h5df backend")
})

test_that("reorder / is_leaf / zarr", {
    tmp <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    d <- zarr_daf(tmp, mode = "w", name = "zarr!")
    expect_true(is_leaf(d))
})

test_that("reorder / is_leaf / wrappers / read_only", {
    first <- memory_daf(name = "first!")
    add_axis(first, "cell", c("A", "B"))
    set_vector(first, "cell", "age", c(1L, 2L))
    expect_false(is_leaf(read_only(first)))
})

test_that("reorder / is_leaf / wrappers / view", {
    first <- memory_daf(name = "first!")
    add_axis(first, "cell", c("A", "B"))
    expect_false(is_leaf(viewer(first, name = "view!")))
})

test_that("reorder / is_leaf / wrappers / read_chain", {
    first <- memory_daf(name = "first!")
    second <- memory_daf(name = "second!")
    expect_false(is_leaf(chain_reader(list(first, second), name = "read_chain!")))
})

test_that("reorder / is_leaf / wrappers / write_chain", {
    first <- memory_daf(name = "first!")
    second <- memory_daf(name = "second!")
    expect_false(is_leaf(chain_writer(list(first, second), name = "write_chain!")))
})

test_that("reorder / is_leaf / wrappers / contract", {
    # Julia gates this on DAF_ENFORCE_CONTRACTS; dafr gates contractor's
    # wrapping on env DAF_ENFORCE_CONTRACTS=1 (otherwise it returns the
    # raw daf, which IS a leaf). Set the env var locally.
    withr::with_envvar(c(DAF_ENFORCE_CONTRACTS = "1"), {
        first <- memory_daf(name = "first!")
        add_axis(first, "cell", c("A", "B"))
        contract <- Contract(axes = list(cell = list(OptionalOutput, "cell")))
        expect_false(is_leaf(contractor("computation", contract, first,
                                        overwrite = TRUE)))
    })
})

# ---------------------------------------------------------------------------
# reorder / reorder_axes!
# ---------------------------------------------------------------------------

test_that("reorder / reorder_axes! / errors / non_leaf", {
    first <- memory_daf(name = "first!")
    second <- memory_daf(name = "second!")
    chain <- chain_writer(list(first, second), name = "chain!")
    add_axis(first, "cell", c("A"))
    expect_error(
        reorder_axes(chain, cell = 1L),
        regexp = "non-leaf|chain"
    )
})

test_that("reorder / reorder_axes! / errors / missing_axis", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    expect_error(
        reorder_axes(d, gene = c(1L, 2L)),
        regexp = "axis: gene|gene.*does not exist|gene.*not found"
    )
})

test_that("reorder / reorder_axes! / errors / bad_length", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    expect_error(
        reorder_axes(d, cell = c(1L, 2L, 3L)),
        regexp = "permutation length.*does not match|length.*does not match.*axis"
    )
})

test_that("reorder / reorder_axes! / errors / bad_permutation", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    expect_error(
        reorder_axes(d, cell = c(1L, 1L)),
        regexp = "permutation"
    )
})

test_that("reorder / reorder_axes! / empty", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    reorder_axes(d)
    expect_equal(unname(axis_vector(d, "cell")), c("A", "B"))
})

# ----- memory backend -----

test_that("reorder / reorder_axes! / memory / both_axes", {
    d <- memory_daf(name = "memory!")
    .populate_reorder_test_data(d)
    reorder_axes(d, cell = c(3L, 1L, 2L), gene = c(4L, 3L, 2L, 1L))
    .assert_reorder_both_axes(d)
})

test_that("reorder / reorder_axes! / memory / single_axis", {
    d <- memory_daf(name = "memory!")
    .populate_reorder_test_data(d)
    reorder_axes(d, cell = c(3L, 1L, 2L))
    .assert_reorder_single_axis(d)
})

test_that("reorder / reorder_axes! / memory / identity", {
    d <- memory_daf(name = "memory!")
    .populate_reorder_test_data(d)
    reorder_axes(d, cell = c(1L, 2L, 3L))
    expect_equal(unname(axis_vector(d, "cell")), c("A", "B", "C"))
    expect_equal(unname(get_vector(d, "cell", "age")), c(10L, 20L, 30L))
    expect_equal(unname(get_vector(d, "cell", "color")), c("aa", "bb", "cc"))
})

test_that("reorder / reorder_axes! / memory / crash_recovery / after_1", {
    d <- memory_daf(name = "memory!")
    .populate_reorder_test_data(d)
    .test_crash_recovery(d, crash_after = 1L)
})

test_that("reorder / reorder_axes! / memory / crash_recovery / after_4", {
    d <- memory_daf(name = "memory!")
    .populate_reorder_test_data(d)
    .test_crash_recovery(d, crash_after = 4L)
})

test_that("reorder / reorder_axes! / memory / crash_recovery / no_pending", {
    d <- memory_daf(name = "memory!")
    .populate_reorder_test_data(d)
    # Memory: reset_reorder_axes is a no-op — returns FALSE (no pending
    # reorder), Julia's reset_reorder_axes! is also FALSE on no-pending.
    expect_false(isTRUE(reset_reorder_axes(d)))
})

# ----- files backend -----

test_that("reorder / reorder_axes! / files / both_axes", {
    tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf()
    .populate_reorder_test_data(src)
    d <- files_daf(tmp, mode = "w", name = "files!")
    copy_all(d, src, relayout = FALSE)
    reorder_axes(d, cell = c(3L, 1L, 2L), gene = c(4L, 3L, 2L, 1L))
    .assert_reorder_both_axes(d)
})

test_that("reorder / reorder_axes! / files / single_axis", {
    tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf()
    .populate_reorder_test_data(src)
    d <- files_daf(tmp, mode = "w", name = "files!")
    copy_all(d, src, relayout = FALSE)
    reorder_axes(d, cell = c(3L, 1L, 2L))
    .assert_reorder_single_axis(d)
})

test_that("reorder / reorder_axes! / files / identity", {
    tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf()
    .populate_reorder_test_data(src)
    d <- files_daf(tmp, mode = "w", name = "files!")
    copy_all(d, src, relayout = FALSE)
    reorder_axes(d, cell = c(1L, 2L, 3L))
    expect_equal(unname(axis_vector(d, "cell")), c("A", "B", "C"))
    expect_equal(unname(get_vector(d, "cell", "age")), c(10L, 20L, 30L))
})

test_that("reorder / reorder_axes! / files / crash_recovery / after_1", {
    tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf()
    .populate_reorder_test_data(src)
    d <- files_daf(tmp, mode = "w", name = "files!")
    copy_all(d, src, relayout = FALSE)
    .test_crash_recovery(d, crash_after = 1L)
})

test_that("reorder / reorder_axes! / files / crash_recovery / after_4", {
    tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf()
    .populate_reorder_test_data(src)
    d <- files_daf(tmp, mode = "w", name = "files!")
    copy_all(d, src, relayout = FALSE)
    .test_crash_recovery(d, crash_after = 4L)
})

test_that("reorder / reorder_axes! / files / crash_recovery / no_pending", {
    tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf()
    .populate_reorder_test_data(src)
    d <- files_daf(tmp, mode = "w", name = "files!")
    copy_all(d, src, relayout = FALSE)
    expect_false(isTRUE(reset_reorder_axes(d)))
})

# ----- h5df backend -----

test_that("reorder / reorder_axes! / h5df / *", {
    skip("R divergence R3: dafr does not have an h5df backend")
})

# ----- zarr backend -----

test_that("reorder / reorder_axes! / zarr / both_axes", {
    tmp <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf(); .populate_reorder_test_data(src)
    d <- zarr_daf(tmp, mode = "w", name = "zarr!")
    copy_all(d, src, relayout = FALSE)
    reorder_axes(d, cell = c(3L, 1L, 2L), gene = c(4L, 3L, 2L, 1L))
    .assert_reorder_both_axes(d)
})

test_that("reorder / reorder_axes! / zarr / single_axis", {
    tmp <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf(); .populate_reorder_test_data(src)
    d <- zarr_daf(tmp, mode = "w", name = "zarr!")
    copy_all(d, src, relayout = FALSE)
    reorder_axes(d, cell = c(3L, 1L, 2L))
    .assert_reorder_single_axis(d)
})

test_that("reorder / reorder_axes! / zarr / identity", {
    tmp <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf(); .populate_reorder_test_data(src)
    d <- zarr_daf(tmp, mode = "w", name = "zarr!")
    copy_all(d, src, relayout = FALSE)
    reorder_axes(d, cell = c(1L, 2L, 3L))
    expect_equal(unname(axis_vector(d, "cell")), c("A", "B", "C"))
    expect_equal(unname(get_vector(d, "cell", "age")), c(10L, 20L, 30L))
})

test_that("reorder / reorder_axes! / zarr / crash_recovery / no_pending", {
    tmp <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    src <- memory_daf(); .populate_reorder_test_data(src)
    d <- zarr_daf(tmp, mode = "w", name = "zarr!")
    copy_all(d, src, relayout = FALSE)
    expect_false(isTRUE(reset_reorder_axes(d)))
})

test_that("reorder / reorder_axes! / zarr / crash_recovery / mid_reorder", {
    skip("R4-recovery: zarr_daf reorder is best-effort (no on-disk backup); a mid-reorder crash leaves an undefined state")
})

# ----- multiple writers -----

test_that("reorder / reorder_axes! / multiple_writers / memory_pair", {
    daf1 <- memory_daf(name = "first!"); .populate_reorder_test_data(daf1)
    daf2 <- memory_daf(name = "second!"); .populate_reorder_test_data(daf2)
    reorder_axes(list(daf1, daf2), cell = c(3L, 1L, 2L))
    expect_equal(unname(axis_vector(daf1, "cell")), c("C", "A", "B"))
    expect_equal(unname(axis_vector(daf2, "cell")), c("C", "A", "B"))
    expect_equal(unname(get_vector(daf1, "cell", "age")), c(30L, 10L, 20L))
    expect_equal(unname(get_vector(daf2, "cell", "age")), c(30L, 10L, 20L))
})

test_that("reorder / reorder_axes! / multiple_writers / mixed_axes", {
    daf1 <- memory_daf(name = "first!")
    daf2 <- memory_daf(name = "second!")
    add_axis(daf1, "cell", c("A", "B", "C"))
    set_vector(daf1, "cell", "age", c(10L, 20L, 30L))
    add_axis(daf2, "gene", c("X", "Y"))
    set_vector(daf2, "gene", "marker", c(1L, 0L))
    reorder_axes(list(daf1, daf2), cell = c(3L, 1L, 2L), gene = c(2L, 1L))
    expect_equal(unname(axis_vector(daf1, "cell")), c("C", "A", "B"))
    expect_equal(unname(get_vector(daf1, "cell", "age")), c(30L, 10L, 20L))
    expect_equal(unname(axis_vector(daf2, "gene")), c("Y", "X"))
    expect_equal(unname(get_vector(daf2, "gene", "marker")), c(0L, 1L))
})

test_that("reorder / reorder_axes! / multiple_writers / mismatched_entries", {
    daf1 <- memory_daf(name = "first!")
    daf2 <- memory_daf(name = "second!")
    add_axis(daf1, "cell", c("A", "B"))
    add_axis(daf2, "cell", c("X", "Y"))
    expect_error(
        reorder_axes(list(daf1, daf2), cell = c(2L, 1L)),
        regexp = "axis: cell entries differ"
    )
})
