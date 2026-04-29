test_that("files_daf format_replace_reorder rewrites vector + matrix payloads", {
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C"))
    add_axis(d_mem, "gene", c("X", "Y"))
    set_vector(d_mem, "cell", "x", c(10, 20, 30))
    set_matrix(d_mem, "cell", "gene", "M", matrix(1:6, nrow = 3))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    plan <- dafr:::.build_reorder_plan(fd_w, list(cell = c(2L, 3L, 1L)))
    dafr:::format_replace_reorder(fd_w, plan)
    dafr:::format_cleanup_reorder(fd_w, plan)
    rm(fd_w)
    fd_r <- files_daf(tmp, mode = "r")
    expect_identical(axis_vector(fd_r, "cell"), c("B", "C", "A"))
    expect_equal(unname(get_vector(fd_r, "cell", "x")), c(20, 30, 10))
    m <- get_matrix(fd_r, "cell", "gene", "M")
    expect_equal(unname(m[, 1]), c(2, 3, 1))
})

test_that("files_daf reorder leaves no .reorder.backup/ behind on success", {
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B"))
    set_vector(d_mem, "cell", "x", c(1, 2))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    plan <- dafr:::.build_reorder_plan(fd_w, list(cell = c(2L, 1L)))
    dafr:::format_replace_reorder(fd_w, plan)
    dafr:::format_cleanup_reorder(fd_w, plan)
    expect_false(dir.exists(file.path(tmp, ".reorder.backup")))
})

test_that("files_daf reorder bumps version counters on success", {
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B"))
    set_vector(d_mem, "cell", "x", c(1, 2))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    a0 <- axis_version_counter(fd_w, "cell")
    v0 <- vector_version_counter(fd_w, "cell", "x")
    plan <- dafr:::.build_reorder_plan(fd_w, list(cell = c(2L, 1L)))
    dafr:::format_replace_reorder(fd_w, plan)
    dafr:::format_cleanup_reorder(fd_w, plan)
    expect_gt(axis_version_counter(fd_w, "cell"), a0)
    expect_gt(vector_version_counter(fd_w, "cell", "x"), v0)
})

test_that("files_daf reorder preserves dimnames on read after reopen", {
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C"))
    add_axis(d_mem, "gene", c("X", "Y"))
    set_matrix(d_mem, "cell", "gene", "M", matrix(1:6, nrow = 3))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    plan <- dafr:::.build_reorder_plan(fd_w, list(cell = c(2L, 3L, 1L)))
    dafr:::format_replace_reorder(fd_w, plan)
    dafr:::format_cleanup_reorder(fd_w, plan)
    rm(fd_w)
    fd_r <- files_daf(tmp, mode = "r")
    m <- get_matrix(fd_r, "cell", "gene", "M")
    expect_identical(rownames(m), c("B", "C", "A"))
    expect_identical(colnames(m), c("X", "Y"))
})

test_that("files_daf reorder handles sparse matrix correctly", {
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C"))
    add_axis(d_mem, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(i = c(1L, 2L, 3L), j = c(1L, 2L, 1L),
                               x = c(1.0, 2.0, 3.0), dims = c(3L, 2L))
    set_matrix(d_mem, "cell", "gene", "S", sp)
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    plan <- dafr:::.build_reorder_plan(fd_w, list(cell = c(3L, 1L, 2L)))
    dafr:::format_replace_reorder(fd_w, plan)
    dafr:::format_cleanup_reorder(fd_w, plan)
    rm(fd_w)
    fd_r <- files_daf(tmp, mode = "r")
    m <- get_matrix(fd_r, "cell", "gene", "S")
    expect_equal(as.numeric(m[1, ]), c(3.0, 0.0))
    expect_equal(as.numeric(m[2, ]), c(1.0, 0.0))
    expect_equal(as.numeric(m[3, ]), c(0.0, 2.0))
})

test_that("files_daf format_reset_reorder is no-op when no .reorder.backup/ exists", {
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B"))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    expect_silent(dafr:::format_reset_reorder(fd_w))
})

# ---- Public API ------------------------------------------------------------

test_that("reorder_axes() works end-to-end on files_daf with reopen", {
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C"))
    add_axis(d_mem, "gene", c("X", "Y"))
    set_vector(d_mem, "cell", "x", c(10, 20, 30))
    set_matrix(d_mem, "cell", "gene", "M", matrix(1:6, nrow = 3))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    reorder_axes(fd_w, cell = c(2L, 3L, 1L))
    rm(fd_w)
    fd_r <- files_daf(tmp, mode = "r")
    expect_identical(axis_vector(fd_r, "cell"), c("B", "C", "A"))
    expect_equal(unname(get_vector(fd_r, "cell", "x")), c(20, 30, 10))
    m <- get_matrix(fd_r, "cell", "gene", "M")
    expect_equal(unname(m[, 1]), c(2, 3, 1))
})

test_that("reset_reorder_axes() on a clean files_daf is a no-op", {
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A"))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    expect_silent(reset_reorder_axes(fd_w))
})

# ---- Integration: files_daf sparse + chain --------------------------------

test_that("files_daf reorder of sparse matrix is byte-correct after reopen", {
    skip_on_cran()
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C", "D", "E"))
    add_axis(d_mem, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(
        i = c(1L, 3L, 5L, 2L), j = c(1L, 1L, 1L, 2L),
        x = c(10.0, 30.0, 50.0, 20.0),
        dims = c(5L, 2L)
    )
    set_matrix(d_mem, "cell", "gene", "S", sp)
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    reorder_axes(fd_w, cell = c(5L, 4L, 3L, 2L, 1L))  # reverse
    rm(fd_w)
    fd_r <- files_daf(tmp, mode = "r")
    m <- get_matrix(fd_r, "cell", "gene", "S")
    # Original (cell x gene):
    # A=1: 10 in col1
    # B=2: 20 in col2
    # C=3: 30 in col1
    # D=4: 0
    # E=5: 50 in col1
    # After reverse perm c(5,4,3,2,1):
    # cell[1] = E:  50 in col1
    # cell[2] = D:  0
    # cell[3] = C:  30 in col1
    # cell[4] = B:  20 in col2
    # cell[5] = A:  10 in col1
    expect_equal(as.numeric(m[1, ]), c(50.0, 0.0))
    expect_equal(as.numeric(m[2, ]), c(0.0, 0.0))
    expect_equal(as.numeric(m[3, ]), c(30.0, 0.0))
    expect_equal(as.numeric(m[4, ]), c(0.0, 20.0))
    expect_equal(as.numeric(m[5, ]), c(10.0, 0.0))
})

test_that("files_daf reorder bumps version counter such that cached reads invalidate", {
    skip_on_cran()
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B"))
    set_vector(d_mem, "cell", "x", c(1.0, 2.0))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    # Prime the cache
    v_before <- get_vector(fd_w, "cell", "x")
    reorder_axes(fd_w, cell = c(2L, 1L))
    # Subsequent get must see permuted values, not stale cache
    v_after <- get_vector(fd_w, "cell", "x")
    expect_equal(unname(v_after), c(2.0, 1.0))
    expect_false(identical(v_before, v_after))
})

test_that("files_daf reorder + read with names produces aligned output", {
    skip_on_cran()
    tmp <- tempfile()
    d_mem <- memory_daf()
    add_axis(d_mem, "cell", c("A", "B", "C"))
    set_vector(d_mem, "cell", "x", c(10.0, 20.0, 30.0))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d_mem, relayout = FALSE)
    reorder_axes(fd_w, cell = c(3L, 1L, 2L))
    rm(fd_w)
    fd_r <- files_daf(tmp, mode = "r")
    v <- get_vector(fd_r, "cell", "x")
    expect_identical(names(v), c("C", "A", "B"))
    expect_equal(unname(v), c(30.0, 10.0, 20.0))
})
