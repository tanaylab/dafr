skip_if_no_hdf5r <- function() testthat::skip_if_not_installed("hdf5r")

# HDF5 packs datasets smaller than its small-data-block size (2048 B) into an
# unaligned aggregation block; larger datasets get their own element-aligned
# allocation. mmap only fires on the aligned (large) ones, so the fixture uses
# axes long enough that its dense components clear that threshold.
NR <- 1000L
NC <- 20L

.mk_h5df <- function() {
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "row", sprintf("r%04d", seq_len(NR)))
    add_axis(d, "col", sprintf("c%02d", seq_len(NC)))
    m <- matrix(as.double(seq_len(NR * NC)), NR, NC)      # dense Float64, 160 KB
    set_matrix(d, "row", "col", "dense", m, relayout = FALSE)
    set_vector(d, "row", "fvec", as.double(seq_len(NR)))  # dense Float64, 8 KB
    set_vector(d, "row", "ivec", seq_len(NR))             # dense Int32, 4 KB
    sp <- Matrix::sparseMatrix(i = c(1, NR), j = c(1, NC), x = c(1, 2), dims = c(NR, NC))
    set_matrix(d, "row", "col", "sparse", sp, relayout = FALSE)
    rm(d); gc()
    p
}

test_that("dense Float64 matrix read is mmap-backed (ALTREP) and correct", {
    skip_if_no_hdf5r()
    d <- h5df(.mk_h5df(), mode = "r")
    m <- get_matrix(d, "row", "col", "dense")
    expect_true(dafr:::is_altrep_cpp(m))
    expect_equal(dim(m), c(NR, NC))
    expect_equal(as.vector(m), as.double(seq_len(NR * NC)))
    expect_equal(rownames(m), sprintf("r%04d", seq_len(NR)))
    expect_equal(colnames(m), sprintf("c%02d", seq_len(NC)))
    rm(d); gc()
})

test_that("dense Float64 vector read is mmap-backed and correct", {
    skip_if_no_hdf5r()
    d <- h5df(.mk_h5df(), mode = "r")
    v <- get_vector(d, "row", "fvec")
    expect_true(dafr:::is_altrep_cpp(v))
    expect_equal(unname(v), as.double(seq_len(NR)))
    expect_equal(names(v), sprintf("r%04d", seq_len(NR)))
    rm(d); gc()
})

test_that("dense Int32 vector read is mmap-backed and correct", {
    skip_if_no_hdf5r()
    d <- h5df(.mk_h5df(), mode = "r")
    v <- get_vector(d, "row", "ivec")
    expect_true(dafr:::is_altrep_cpp(v))
    expect_true(is.integer(v))
    expect_equal(unname(v), seq_len(NR))
    rm(d); gc()
})

test_that("dafr.mmap toggles the mmap fast path with identical values", {
    skip_if_no_hdf5r()
    # Test below dafr's content-addressed component cache (which would reuse an
    # mmap value across reads): .h5_get_matrix_impl reads straight from HDF5.
    d <- h5df(.mk_h5df(), mode = "r")
    on_read <- dafr:::.h5_get_matrix_impl(d, "row", "col", "dense")
    old <- options(dafr.mmap = FALSE); on.exit(options(old))
    off_read <- dafr:::.h5_get_matrix_impl(d, "row", "col", "dense")
    expect_true(dafr:::is_altrep_cpp(on_read))
    expect_false(dafr:::is_altrep_cpp(off_read))
    expect_identical(as.vector(on_read), as.vector(off_read))
    expect_equal(as.vector(off_read), as.double(seq_len(NR * NC)))
    rm(d); gc()
})

test_that("sparse matrix read is unaffected (not mmap-backed)", {
    skip_if_no_hdf5r()
    d <- h5df(.mk_h5df(), mode = "r")
    sp <- get_matrix(d, "row", "col", "sparse")
    expect_false(dafr:::is_altrep_cpp(sp))
    expect_true(methods::is(sp, "dgCMatrix"))
    expect_equal(sp[1, 1], 1)
    expect_equal(sp[NR, NC], 2)
    rm(d); gc()
})

test_that("a below-threshold dense component reads eagerly but correctly", {
    skip_if_no_hdf5r()
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "row", c("a", "b", "c"))
    set_vector(d, "row", "tiny", c(1.5, 2.5, 3.5))        # 24 B, packed unaligned
    rm(d); gc()
    d <- h5df(p, mode = "r")
    v <- get_vector(d, "row", "tiny")
    expect_false(dafr:::is_altrep_cpp(v))                  # eager fallback
    expect_equal(unname(v), c(1.5, 2.5, 3.5))
    rm(d); gc()
})
