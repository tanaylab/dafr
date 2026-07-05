# Adversarial / edge-case coverage for the ZipDaf backend: the numeric tower,
# NaN, all-zero, empty columns, sparse strings, Bool-with-explicit-false, packed
# shards, r+ append semantics, error-path distinctions, name derivation, and
# unicode. These try to break the thin ZipDaf orchestration, not the (shared,
# already-tested) serialization cores.

test_that("numeric tower round-trips (Float32, Int32, Int64, UInt16-indexed sparse)", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", sprintf("c%d", 1:6))
    set_vector(d, "cell", "f32", c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5)) # dense double
    set_vector(d, "cell", "i32", c(10L, 20L, 30L, 40L, 50L, 60L)) # dense int
    set_vector(d, "cell", "i64", bit64::as.integer64(c(1, 2, 3, 4, 5, 6) * 1e10))
    # a sparse int vector on a small axis -> UInt16 on-disk index
    set_vector(d, "cell", "sp", c(0L, 0L, 7L, 0L, 0L, 9L))
    rm(d)
    gc()
    d2 <- zip_daf(p, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "f32")), c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5))
    expect_equal(unname(get_vector(d2, "cell", "i32")), c(10L, 20L, 30L, 40L, 50L, 60L))
    expect_equal(unname(get_vector(d2, "cell", "i64")),
        bit64::as.integer64(c(1, 2, 3, 4, 5, 6) * 1e10))
    expect_equal(unname(get_vector(d2, "cell", "sp")), c(0L, 0L, 7L, 0L, 0L, 9L))
})

test_that("NaN is preserved through a sparsified float vector", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", sprintf("c%d", 1:5))
    v <- c(0, NaN, 0, 3.5, 0) # NaN counts as nonzero -> stays sparse-encoded
    set_vector(d, "cell", "v", v)
    rm(d)
    gc()
    got <- unname(get_vector(zip_daf(p, mode = "r"), "cell", "v"))
    expect_true(is.nan(got[2]))
    expect_equal(got[c(1, 3, 4, 5)], c(0, 0, 3.5, 0))
})

test_that("all-zero vector (nnz = 0) round-trips", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", sprintf("c%d", 1:4))
    set_vector(d, "cell", "z", c(0, 0, 0, 0))
    rm(d)
    gc()
    expect_equal(unname(get_vector(zip_daf(p, mode = "r"), "cell", "z")), c(0, 0, 0, 0))
})

test_that("sparse matrix with an empty column round-trips", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", sprintf("c%d", 1:3))
    add_axis(d, "gene", c("X", "Y", "Z"))
    sp <- Matrix::sparseMatrix(
        i = c(1, 3), j = c(1, 3), x = c(5, 7), dims = c(3, 3)
    ) # middle column empty
    set_matrix(d, "cell", "gene", "m", sp)
    rm(d)
    gc()
    m2 <- get_matrix(zip_daf(p, mode = "r"), "cell", "gene", "m")
    expect_s4_class(m2, "dgCMatrix")
    expect_equal(as.matrix(unname(m2)), as.matrix(sp))
})

test_that("sparse string vector (many empties) uses the .nztxt path", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", sprintf("c%d", 1:6))
    v <- c("", "hi", "", "", "there", "")
    set_vector(d, "cell", "s", v)
    rm(d)
    gc()
    expect_equal(unname(get_vector(zip_daf(p, mode = "r"), "cell", "s")), v)
})

test_that("Bool sparse matrix with an explicit stored FALSE writes+reads nzval", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", sprintf("c%d", 1:3))
    add_axis(d, "gene", c("X", "Y"))
    # lgCMatrix whose @x contains an explicit FALSE -> the all-true optimization
    # does NOT apply, so a .nzval component must be written and read back.
    mk <- methods::new("lgCMatrix",
        i = c(0L, 2L), p = c(0L, 2L, 2L),
        Dim = c(3L, 2L), Dimnames = list(NULL, NULL), x = c(TRUE, FALSE)
    )
    set_matrix(d, "cell", "gene", "mk", mk)
    rm(d)
    gc()
    mk2 <- get_matrix(zip_daf(p, mode = "r"), "cell", "gene", "mk")
    expect_s4_class(mk2, "lgCMatrix")
    expect_equal(as.matrix(unname(mk2)), as.matrix(mk))
})

test_that("packed = TRUE writes zip-shard components readable back", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w", packed = TRUE)
    add_axis(d, "cell", sprintf("c%d", 1:4000))
    add_axis(d, "gene", sprintf("g%d", 1:8))
    v <- as.double(seq_len(4000)) # > 8 KB -> packs to a .zip shard entry
    set_vector(d, "cell", "big", v)
    m <- matrix(as.double(seq_len(4000 * 8)), nrow = 4000)
    set_matrix(d, "cell", "gene", "dm", m)
    rm(d)
    gc()
    d2 <- zip_daf(p, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "big")), v)
    expect_equal(unname(get_matrix(d2, "cell", "gene", "dm")), m)
})

test_that("r+ append adds new keys but overwrite of existing raises", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    set_scalar(d, "a", 1L)
    rm(d)
    gc()
    d2 <- zip_daf(p, mode = "r+")
    set_scalar(d2, "b", 2L) # new key -> ok
    set_vector(d2, "cell", "v", c(1, 2)) # new key -> ok
    expect_error(set_scalar(d2, "a", 9L, overwrite = TRUE), "append-only")
    rm(d2)
    gc()
    d3 <- zip_daf(p, mode = "r")
    expect_equal(get_scalar(d3, "a"), 1L)
    expect_equal(get_scalar(d3, "b"), 2L)
    expect_equal(unname(get_vector(d3, "cell", "v")), c(1, 2))
})

test_that("default overwrite=FALSE gives an 'exists' error, distinct from append-only", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "v", c(1, 2))
    # overwrite defaults to FALSE -> the standard already-exists guard, NOT the
    # append-only message.
    err <- tryCatch(set_vector(d, "cell", "v", c(3, 4)),
        error = function(e) conditionMessage(e))
    expect_false(grepl("append-only", err))
})

test_that("missing scalar/vector/matrix and missing axis error cleanly", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", c("A", "B"))
    rm(d)
    gc()
    d2 <- zip_daf(p, mode = "r")
    expect_error(get_scalar(d2, "nope"))
    expect_error(get_vector(d2, "cell", "nope"))
    expect_error(get_vector(d2, "ghost", "v")) # missing axis
    expect_error(get_matrix(d2, "cell", "cell", "nope"))
})

test_that("description() reports type ZipDaf; name derives from the name scalar", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    set_scalar(d, "name", "my_zip_store")
    add_axis(d, "cell", c("A", "B"))
    rm(d)
    gc()
    d2 <- zip_daf(p, mode = "r")
    expect_identical(S7::prop(d2, "name"), "my_zip_store")
    expect_true(any(grepl("type: ZipDaf", description(d2))))
})

test_that("unicode axis entries and string values round-trip", {
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    cells <- c("α", "中", "café")
    add_axis(d, "cell", cells)
    vals <- c("ü", "ñ", "☃")
    set_vector(d, "cell", "lbl", vals)
    rm(d)
    gc()
    d2 <- zip_daf(p, mode = "r")
    expect_equal(axis_vector(d2, "cell"), cells)
    expect_equal(unname(get_vector(d2, "cell", "lbl")), vals)
})
