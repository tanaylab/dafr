# ZipDaf mmap zero-copy reads. STORE'd (packed=FALSE) dense components have
# their data region 8-byte aligned inside the archive by design (the writer's
# compute_alignment_padding), so mmap fires for any dense Float64/Int32 read
# opened read-only. Mirrors test-h5df-mmap.R. Compressed (packed=TRUE) entries
# and writable-mode reads fall back to eager.

NR <- 1000L
NC <- 20L

.mk_zip <- function(packed = FALSE) {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w", packed = packed)
    add_axis(d, "row", sprintf("r%04d", seq_len(NR)))
    add_axis(d, "col", sprintf("c%02d", seq_len(NC)))
    m <- matrix(as.double(seq_len(NR * NC)), NR, NC)      # dense Float64
    set_matrix(d, "row", "col", "dense", m, relayout = FALSE)
    set_vector(d, "row", "fvec", as.double(seq_len(NR)))  # dense Float64
    set_vector(d, "row", "ivec", seq_len(NR))             # dense Int32
    set_vector(d, "row", "i64", bit64::as.integer64(seq_len(NR)))  # dense Int64
    sp <- Matrix::sparseMatrix(i = c(1, NR), j = c(1, NC), x = c(1, 2), dims = c(NR, NC))
    set_matrix(d, "row", "col", "sparse", sp, relayout = FALSE)
    rm(d); gc()
    p
}

test_that("dense Float64 matrix read is mmap-backed (ALTREP) and correct", {
    d <- zip_daf(.mk_zip(), mode = "r")
    m <- get_matrix(d, "row", "col", "dense")
    expect_true(dafr:::is_altrep_cpp(m))
    expect_equal(dim(m), c(NR, NC))
    expect_equal(as.vector(m), as.double(seq_len(NR * NC)))
    expect_equal(rownames(m), sprintf("r%04d", seq_len(NR)))
    expect_equal(colnames(m), sprintf("c%02d", seq_len(NC)))
})

test_that("dense Float64 vector read is mmap-backed and correct", {
    d <- zip_daf(.mk_zip(), mode = "r")
    v <- get_vector(d, "row", "fvec")
    expect_true(dafr:::is_altrep_cpp(v))
    expect_equal(unname(v), as.double(seq_len(NR)))
    expect_equal(names(v), sprintf("r%04d", seq_len(NR)))
})

test_that("dense Int32 vector read is mmap-backed and correct", {
    d <- zip_daf(.mk_zip(), mode = "r")
    v <- get_vector(d, "row", "ivec")
    expect_true(dafr:::is_altrep_cpp(v))
    expect_true(is.integer(v))
    expect_equal(unname(v), seq_len(NR))
})

test_that("dafr.mmap toggles the mmap fast path with identical values", {
    # Test below dafr's content-addressed component cache at the impl fn.
    d <- zip_daf(.mk_zip(), mode = "r")
    on_read <- dafr:::.zip_get_matrix_impl(d, "row", "col", "dense")
    old <- options(dafr.mmap = FALSE); on.exit(options(old))
    off_read <- dafr:::.zip_get_matrix_impl(d, "row", "col", "dense")
    expect_true(dafr:::is_altrep_cpp(on_read))
    expect_false(dafr:::is_altrep_cpp(off_read))
    expect_identical(as.vector(on_read), as.vector(off_read))
    expect_equal(as.vector(off_read), as.double(seq_len(NR * NC)))
})

test_that("sparse matrix read is unaffected (not mmap-backed)", {
    d <- zip_daf(.mk_zip(), mode = "r")
    sp <- get_matrix(d, "row", "col", "sparse")
    expect_false(dafr:::is_altrep_cpp(sp))
    expect_true(methods::is(sp, "dgCMatrix"))
    expect_equal(sp[1, 1], 1)
    expect_equal(sp[NR, NC], 2)
})

test_that("non-mmappable dtype (Int64) reads eagerly but correctly", {
    # Assert at the impl level: get_vector wraps integer64 in an ALTREP wrapper
    # regardless of mmap (a bit64 quirk), so is_altrep_cpp is only meaningful for
    # Int64 below that layer.
    d <- zip_daf(.mk_zip(), mode = "r")
    v <- dafr:::.zip_get_vector_impl(d, "row", "i64")
    expect_false(dafr:::is_altrep_cpp(v))                  # not Float64/Int32 -> eager
    expect_true(bit64::is.integer64(v))
    expect_equal(unname(v), bit64::as.integer64(seq_len(NR)))
})

test_that("writable-mode read falls back to eager (no overlay mmap)", {
    p <- .mk_zip()
    d <- zip_daf(p, mode = "r+")
    m <- dafr:::.zip_get_matrix_impl(d, "row", "col", "dense")
    expect_false(dafr:::is_altrep_cpp(m))                  # mode != "r" -> eager
    expect_equal(as.vector(m), as.double(seq_len(NR * NC)))
})
