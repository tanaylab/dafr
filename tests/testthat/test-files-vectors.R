test_that("FilesDaf vectors_set lists descriptor-backed vectors", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"dense","eltype":"Float64"}',
        file.path(dir, "vectors", "cell", "donor.json")
    )
    writeBin(c(1.0, 2.0),
        file.path(dir, "vectors", "cell", "donor.data"),
        size = 8L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    expect_equal(vectors_set(d, "cell"), "donor")
    expect_true(has_vector(d, "cell", "donor"))
})

test_that("format_get_vector returns an ALTREP-backed vector for Float64 dense", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"dense","eltype":"Float64"}',
        file.path(dir, "vectors", "cell", "x.json")
    )
    writeBin(c(1.5, 2.5, -3.25),
        file.path(dir, "vectors", "cell", "x.data"),
        size = 8L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    v <- format_get_vector(d, "cell", "x")$value
    expect_equal(v, c(1.5, 2.5, -3.25))
    expect_true(is_altrep(v))
})

test_that("format_get_vector eager-reads Float64 when dafr.mmap = FALSE", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"dense","eltype":"Float64"}',
        file.path(dir, "vectors", "cell", "x.json")
    )
    writeBin(c(10.0, 20.0), file.path(dir, "vectors", "cell", "x.data"),
        size = 8L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    v <- withr::with_options(
        list(dafr.mmap = FALSE),
        format_get_vector(d, "cell", "x")
    )$value
    expect_equal(v, c(10.0, 20.0))
    expect_false(is_altrep(v))
})

test_that("format_get_vector densifies Int32 via mmap", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"dense","eltype":"Int32"}',
        file.path(dir, "vectors", "cell", "i.json")
    )
    writeBin(c(1L, -2L, 3L), file.path(dir, "vectors", "cell", "i.data"),
        size = 4L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    v <- format_get_vector(d, "cell", "i")$value
    expect_equal(v, c(1L, -2L, 3L))
    expect_true(is_altrep(v) || is.integer(v))
})

test_that("format_get_vector Bool dense (eager read path, not mmap)", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"dense","eltype":"Bool"}',
        file.path(dir, "vectors", "cell", "b.json")
    )
    writeBin(as.raw(c(1L, 0L, 1L)), file.path(dir, "vectors", "cell", "b.data"))
    d <- files_daf(dir, mode = "r")
    v <- format_get_vector(d, "cell", "b")$value
    expect_equal(v, c(TRUE, FALSE, TRUE))
})

test_that("format_get_vector String dense round-trip", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"dense","eltype":"String"}',
        file.path(dir, "vectors", "cell", "s.json")
    )
    writeLines(
        c("foo", "bar"),
        file.path(dir, "vectors", "cell", "s.txt")
    )
    d <- files_daf(dir, mode = "r")
    v <- format_get_vector(d, "cell", "s")$value
    expect_equal(v, c("foo", "bar"))
})

test_that("format_get_vector errors on missing descriptor", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines("A", file.path(dir, "axes", "cell.txt"))
    d <- files_daf(dir, mode = "r")
    expect_error(format_get_vector(d, "cell", "nope"), "missing vector:")
})

test_that("format_get_vector errors on truncated payload", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"dense","eltype":"Float64"}',
        file.path(dir, "vectors", "cell", "t.json")
    )
    # Write only 2 doubles instead of 3:
    writeBin(c(1.0, 2.0), file.path(dir, "vectors", "cell", "t.data"),
        size = 8L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    expect_error(format_get_vector(d, "cell", "t"), "truncated")
})

test_that("set_vector + get_vector dense Float64 round-trip", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.5, 2.5, -3.25))
    d2 <- files_daf(dir, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "x")), c(1.5, 2.5, -3.25))
    j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "x.json"))
    expect_equal(j$format, "dense")
    expect_equal(j$eltype, "Float64")
    expect_equal(file.size(file.path(dir, "vectors", "cell", "x.data")), 24L)
})

test_that("set_vector dense Int32 and Bool round-trip", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "i", c(7L, 42L))
    set_vector(d, "cell", "b", c(TRUE, FALSE))
    d2 <- files_daf(dir, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "i")), c(7L, 42L))
    expect_equal(unname(get_vector(d2, "cell", "b")), c(TRUE, FALSE))
})

test_that("set_vector dense String round-trip", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "s", c("foo", "bar"))
    txt <- readLines(file.path(dir, "vectors", "cell", "s.txt"))
    expect_equal(txt, c("foo", "bar"))
    j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "s.json"))
    expect_equal(j$format, "dense")
    expect_equal(j$eltype, "String")
})

test_that("set_vector rejects wrong length / requires overwrite", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    expect_error(set_vector(d, "cell", "x", c(1, 2, 3)), "length")
    set_vector(d, "cell", "x", c(1, 2))
    expect_error(set_vector(d, "cell", "x", c(3, 4)), "existing vector:")
    set_vector(d, "cell", "x", c(3, 4), overwrite = TRUE)
})

test_that("delete_vector removes payload + descriptor", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1, 2))
    delete_vector(d, "cell", "x")
    expect_false(file.exists(file.path(dir, "vectors", "cell", "x.json")))
    expect_false(file.exists(file.path(dir, "vectors", "cell", "x.data")))
})

test_that("delete_vector must_exist=FALSE is a no-op on missing", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", "A")
    expect_silent(delete_vector(d, "cell", "nope", must_exist = FALSE))
    expect_error(delete_vector(d, "cell", "nope"), "missing vector:")
})

test_that("format_get_vector densifies a sparse Float64 vector written Julia-style", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C", "D"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"sparse","eltype":"Float64","indtype":"UInt32"}',
        file.path(dir, "vectors", "cell", "sv.json")
    )
    writeBin(c(2L, 4L), file.path(dir, "vectors", "cell", "sv.nzind"),
        size = 4L, endian = "little"
    )
    writeBin(c(10.0, 30.0), file.path(dir, "vectors", "cell", "sv.nzval"),
        size = 8L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    v <- format_get_vector(d, "cell", "sv")$value
    expect_equal(v, c(0, 10, 0, 30))
})

test_that("format_get_vector sparse Bool without .nzval file synthesizes fill(TRUE, nnz)", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"sparse","eltype":"Bool","indtype":"UInt32"}',
        file.path(dir, "vectors", "cell", "sb.json")
    )
    writeBin(c(1L, 3L), file.path(dir, "vectors", "cell", "sb.nzind"),
        size = 4L, endian = "little"
    )
    d <- files_daf(dir, mode = "r")
    v <- format_get_vector(d, "cell", "sb")$value
    expect_equal(v, c(TRUE, FALSE, TRUE))
})

test_that("format_get_vector sparse String reads .nztxt", {
    dir <- new_tempdir()
    dir.create(file.path(dir, "axes"), recursive = TRUE)
    dir.create(file.path(dir, "vectors", "cell"), recursive = TRUE)
    writeLines('{"version":[1,0]}', file.path(dir, "daf.json"))
    writeLines(c("A", "B", "C", "D", "E"), file.path(dir, "axes", "cell.txt"))
    writeLines(
        '{"format":"sparse","eltype":"String","indtype":"UInt32"}',
        file.path(dir, "vectors", "cell", "ss.json")
    )
    writeBin(c(2L, 5L), file.path(dir, "vectors", "cell", "ss.nzind"),
        size = 4L, endian = "little"
    )
    writeLines(c("foo", "bar"), file.path(dir, "vectors", "cell", "ss.nztxt"))
    d <- files_daf(dir, mode = "r")
    v <- format_get_vector(d, "cell", "ss")$value
    expect_equal(v, c("", "foo", "", "", "bar"))
})

test_that("set_vector auto-sparsifies a vector dominated by zeros", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", sprintf("e%d", 1:100))
    v <- numeric(100)
    v[c(10, 50, 90)] <- c(1.5, 2.5, 3.5)
    set_vector(d, "cell", "x", v)
    j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "x.json"))
    expect_equal(j$format, "sparse")
    expect_equal(j$eltype, "Float64")
    expect_equal(j$indtype, "UInt32")
    idx <- readBin(file.path(dir, "vectors", "cell", "x.nzind"),
        what = "integer", n = 3L, size = 4L, endian = "little"
    )
    expect_equal(idx, c(10L, 50L, 90L))
    d2 <- files_daf(dir, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "x")), v)
})

test_that("set_vector keeps dense when threshold not met", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", sprintf("e%d", 1:10))
    v <- c(1, 2, 3, 4, 5, 6, 0, 0, 0, 0)
    set_vector(d, "cell", "x", v)
    j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "x.json"))
    expect_equal(j$format, "dense")
})

test_that("set_vector sparse Bool all-TRUE omits .nzval", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", sprintf("e%d", 1:100))
    v <- logical(100)
    v[c(5, 25, 55)] <- TRUE
    set_vector(d, "cell", "b", v)
    j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "b.json"))
    expect_equal(j$format, "sparse")
    expect_equal(j$eltype, "Bool")
    expect_true(file.exists(file.path(dir, "vectors", "cell", "b.nzind")))
    expect_false(file.exists(file.path(dir, "vectors", "cell", "b.nzval")))
    d2 <- files_daf(dir, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "b")), v)
})

test_that("set_vector sparse string writes .nztxt with only non-empty values", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", sprintf("e%d", 1:21))
    v <- rep("", 21)
    v[5] <- "hello"
    set_vector(d, "cell", "s", v)
    j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "s.json"))
    expect_equal(j$format, "sparse")
    expect_equal(j$eltype, "String")
    expect_equal(readLines(file.path(dir, "vectors", "cell", "s.nztxt")), "hello")
    d2 <- files_daf(dir, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "s")), v)
})

test_that("set_vector accepts Matrix::sparseVector and writes sparse unconditionally", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    sv <- Matrix::sparseVector(x = c(10.0, 30.0), i = c(2L, 4L), length = 4L)
    set_vector(d, "cell", "sv", sv)
    j <- jsonlite::fromJSON(file.path(dir, "vectors", "cell", "sv.json"))
    expect_equal(j$format, "sparse")
    d2 <- files_daf(dir, mode = "r")
    expect_equal(unname(get_vector(d2, "cell", "sv")), c(0, 10, 0, 30))
})

test_that("get_vector on files_daf preserves axis-entry names (slice-14 regression guard)", {
    tmp <- tempfile(fileext = ".daf")
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.0, 2.0, 3.0))
    fd_w <- files_daf(tmp, mode = "w")
    copy_all(fd_w, d, relayout = FALSE)
    fd <- files_daf(tmp, mode = "r")
    v <- get_vector(fd, "cell", "x")
    expect_equal(names(v), c("A", "B", "C"))
    expect_equal(unname(v), c(1.0, 2.0, 3.0))

    # Also after a second call (cache hit path) — names must persist:
    v2 <- get_vector(fd, "cell", "x")
    expect_equal(names(v2), c("A", "B", "C"))
})
