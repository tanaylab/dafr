# Crash-counter tests for files_daf reorder. The protocol uses
# .reorder.backup/ hardlinks + in-place overwrite. Recovery on next
# open restores the original (pre-reorder) state.
#
# Tick-point reality: one tick per vector + one tick per matrix on a
# permuted axis. To exercise crashes at multiple tick positions, the
# fixture has 2 vectors + 2 matrices on the "cell" axis, giving 4
# total ticks.

# Helper: build a files_daf with the standard fixture for these tests.
.crash_test_fixture <- function(tmp) {
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C"))
    add_axis(d_mem, "gene", c("X", "Y"))
    set_vector(d_mem, "cell", "x", c(10, 20, 30))
    set_vector(d_mem, "cell", "y", c(100, 200, 300))
    set_matrix(d_mem, "cell", "gene", "M",
               matrix(c(1, 2, 3, 4, 5, 6), nrow = 3))
    set_matrix(d_mem, "cell", "gene", "N",
               matrix(c(7, 8, 9, 10, 11, 12), nrow = 3))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    fd_w
}

# Original entries / values for assertion convenience.
.ORIGINAL_ENTRIES <- c("A", "B", "C")
.ORIGINAL_X <- c(10, 20, 30)
.ORIGINAL_Y <- c(100, 200, 300)

test_that("crash at first tick → backup directory present, recovery restores original", {
    skip_on_cran()
    tmp <- tempfile()
    fd_w <- .crash_test_fixture(tmp)
    expect_error(
        reorder_axes(fd_w, cell = c(2L, 3L, 1L),
                     crash_counter = dafr:::new_crash_counter(1L)),
        class = "SimulatedCrash"
    )
    # Backup directory should exist after crash.
    expect_true(dir.exists(file.path(tmp, ".reorder.backup")))
    rm(fd_w)
    # Reopen with mode "r+" — recovery hook fires.
    fd <- files_daf(tmp, mode = "r+")
    expect_identical(axis_vector(fd, "cell"), .ORIGINAL_ENTRIES)
    expect_equal(unname(get_vector(fd, "cell", "x")), .ORIGINAL_X)
    expect_equal(unname(get_vector(fd, "cell", "y")), .ORIGINAL_Y)
    expect_false(dir.exists(file.path(tmp, ".reorder.backup")))
})

test_that("crash at second tick → recovery restores original", {
    skip_on_cran()
    tmp <- tempfile()
    fd_w <- .crash_test_fixture(tmp)
    expect_error(
        reorder_axes(fd_w, cell = c(2L, 3L, 1L),
                     crash_counter = dafr:::new_crash_counter(2L)),
        class = "SimulatedCrash"
    )
    rm(fd_w)
    fd <- files_daf(tmp, mode = "r+")
    expect_identical(axis_vector(fd, "cell"), .ORIGINAL_ENTRIES)
    expect_equal(unname(get_vector(fd, "cell", "x")), .ORIGINAL_X)
    expect_equal(unname(get_vector(fd, "cell", "y")), .ORIGINAL_Y)
    expect_false(dir.exists(file.path(tmp, ".reorder.backup")))
})

test_that("crash at third tick → recovery restores original", {
    skip_on_cran()
    tmp <- tempfile()
    fd_w <- .crash_test_fixture(tmp)
    expect_error(
        reorder_axes(fd_w, cell = c(2L, 3L, 1L),
                     crash_counter = dafr:::new_crash_counter(3L)),
        class = "SimulatedCrash"
    )
    rm(fd_w)
    fd <- files_daf(tmp, mode = "r+")
    expect_identical(axis_vector(fd, "cell"), .ORIGINAL_ENTRIES)
    m <- get_matrix(fd, "cell", "gene", "M")
    expect_equal(unname(m[, 1]), c(1, 2, 3))
})

test_that("crash at fourth tick → recovery restores original", {
    skip_on_cran()
    tmp <- tempfile()
    fd_w <- .crash_test_fixture(tmp)
    expect_error(
        reorder_axes(fd_w, cell = c(2L, 3L, 1L),
                     crash_counter = dafr:::new_crash_counter(4L)),
        class = "SimulatedCrash"
    )
    rm(fd_w)
    fd <- files_daf(tmp, mode = "r+")
    expect_identical(axis_vector(fd, "cell"), .ORIGINAL_ENTRIES)
    n <- get_matrix(fd, "cell", "gene", "N")
    expect_equal(unname(n[, 1]), c(7, 8, 9))
})

test_that("crash with counter > total tick count → reorder completes successfully", {
    skip_on_cran()
    # 4 ticks total in this fixture; counter = 100 means no crash fires.
    tmp <- tempfile()
    fd_w <- .crash_test_fixture(tmp)
    reorder_axes(fd_w, cell = c(2L, 3L, 1L),
                 crash_counter = dafr:::new_crash_counter(100L))
    expect_identical(axis_vector(fd_w, "cell"), c("B", "C", "A"))
    expect_false(dir.exists(file.path(tmp, ".reorder.backup")))
})

test_that("explicit reset_reorder_axes after crash also recovers", {
    skip_on_cran()
    tmp <- tempfile()
    fd_w <- .crash_test_fixture(tmp)
    expect_error(
        reorder_axes(fd_w, cell = c(2L, 3L, 1L),
                     crash_counter = dafr:::new_crash_counter(2L)),
        class = "SimulatedCrash"
    )
    expect_true(dir.exists(file.path(tmp, ".reorder.backup")))
    # Manually call reset (rather than relying on reopen-time recovery)
    reset_reorder_axes(fd_w)
    expect_false(dir.exists(file.path(tmp, ".reorder.backup")))
    expect_identical(axis_vector(fd_w, "cell"), .ORIGINAL_ENTRIES)
})

test_that("recovery hook fires automatically on r+ open", {
    skip_on_cran()
    tmp <- tempfile()
    fd_w <- .crash_test_fixture(tmp)
    expect_error(
        reorder_axes(fd_w, cell = c(2L, 3L, 1L),
                     crash_counter = dafr:::new_crash_counter(1L)),
        class = "SimulatedCrash"
    )
    expect_true(dir.exists(file.path(tmp, ".reorder.backup")))
    rm(fd_w)
    # Open r+; recovery should run.
    fd <- files_daf(tmp, mode = "r+")
    expect_false(dir.exists(file.path(tmp, ".reorder.backup")))
})

test_that("read-only open does NOT trigger recovery (leaves .reorder.backup/ untouched)", {
    skip_on_cran()
    tmp <- tempfile()
    fd_w <- .crash_test_fixture(tmp)
    expect_error(
        reorder_axes(fd_w, cell = c(2L, 3L, 1L),
                     crash_counter = dafr:::new_crash_counter(1L)),
        class = "SimulatedCrash"
    )
    rm(fd_w)
    # Open read-only; recovery should NOT run; backup dir should remain.
    fd_r <- files_daf(tmp, mode = "r")
    expect_true(dir.exists(file.path(tmp, ".reorder.backup")))
})
