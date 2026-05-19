# Regression guards for the cross-backend round-trip audit (Round 7).
# Each test pins one bug class found by dev/backend-parity/round_trip.R
# and dev/backend-parity/cross_format.R and would fail without the fix
# in this same branch. They are post-hoc guards, not failing-first TDD
# tests - the audit harness already proves the bug exists.

suppressMessages(library(bit64))
suppressMessages(library(Matrix))

# ---- Bug A: NaN scalar must be accepted (Julia DAF parity) ----
test_that("set_scalar accepts NaN as a Float64 value (not NA)", {
    d <- memory_daf()
    expect_no_error(set_scalar(d, "x", NaN))
    expect_true(is.nan(get_scalar(d, "x")))
    # Plain NA is still rejected.
    expect_error(set_scalar(d, "y", NA_real_),
                 "value may not be NA")
})

# ---- Bug C: Float64 scalar round-trips at full Float64 precision ----
test_that("Float64 scalars round-trip without precision loss on FilesDaf", {
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE), add = TRUE)
    d <- files_daf(p, mode = "w")
    set_scalar(d, "pi_val", 3.141592653589793)
    rm(d)
    r <- files_daf(p, mode = "r")
    expect_equal(get_scalar(r, "pi_val"), 3.141592653589793,
                 tolerance = 0)
})

# ---- Bug D: Int64 vectors round-trip across all 64 bits ----
test_that("Int64 dense vector round-trips across 64-bit range on FilesDaf", {
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE), add = TRUE)
    d <- files_daf(p, mode = "w")
    add_axis(d, "k", sprintf("k%d", 1:5))
    v <- bit64::as.integer64(c(
        "-4611686018427387904", "0", "1",
        "4294967296", "4611686018427387904"
    ))
    set_vector(d, "k", "wide", v)
    rm(d)
    r <- files_daf(p, mode = "r")
    out <- unname(get_vector(r, "k", "wide"))
    expect_true(inherits(out, "integer64"))
    expect_equal(as.character(out), as.character(v))
})

# ---- Bug E: All-NaN dense Float64 vector preserves NaN (not all-zero) ----
test_that("All-NaN Float64 vector preserves NaN through FilesDaf sparsify", {
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE), add = TRUE)
    d <- files_daf(p, mode = "w")
    add_axis(d, "k", sprintf("k%d", 1:5))
    set_vector(d, "k", "all_nan", rep(NaN, 5))
    rm(d)
    r <- files_daf(p, mode = "r")
    out <- unname(get_vector(r, "k", "all_nan"))
    expect_true(all(is.nan(out)))
    expect_equal(length(out), 5L)
})

# ---- Bug F: ZarrDaf reorders named-subset vectors to axis order ----
test_that("set_vector with named subset is stored in axis order on ZarrDaf", {
    p <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(p, recursive = TRUE), add = TRUE)
    d <- zarr_daf(p, mode = "w")
    add_axis(d, "k", c("A", "B", "C", "D", "E"))
    set_vector(d, "k", "named",
               c(C = 2.5, A = 0.5, B = 1.5, E = 3.5, D = -1.0))
    rm(d)
    r <- zarr_daf(p, mode = "r")
    out <- get_vector(r, "k", "named")
    # Stored values must match axis order, not input order.
    expect_equal(unname(out), c(0.5, 1.5, 2.5, -1.0, 3.5))
    expect_equal(names(out), c("A", "B", "C", "D", "E"))
})

# ---- Concatenate: matrix wildcard skips concat-axis matrices ----
test_that("concatenate '*|*|*'=MERGE_LAST_VALUE does not clobber stitched matrices", {
    # Two sources with a cell-x-empty-axis matrix (5 cells split 2/3,
    # cols axis is empty). Wildcard merge previously expanded to this
    # key and then .concat_merge_matrix tried to MERGE_LAST_VALUE the
    # 3 x 0 from src_b into a destination that expects 5 x 0.
    a <- memory_daf("a")
    add_axis(a, "cell", c("c1", "c2"))
    add_axis(a, "gene", c("g1", "g2", "g3"))
    add_axis(a, "empty_axis", character(0))
    set_matrix(a, "cell", "empty_axis", "ec",
               matrix(integer(0), nrow = 2L, ncol = 0L))
    set_matrix(a, "cell", "gene", "u",
               matrix(seq_len(6L), nrow = 2L, ncol = 3L))

    b <- memory_daf("b")
    add_axis(b, "cell", c("c3", "c4", "c5"))
    add_axis(b, "gene", c("g1", "g2", "g3"))
    add_axis(b, "empty_axis", character(0))
    set_matrix(b, "cell", "empty_axis", "ec",
               matrix(integer(0), nrow = 3L, ncol = 0L))
    set_matrix(b, "cell", "gene", "u",
               matrix(7:15, nrow = 3L, ncol = 3L))

    dst <- memory_daf("dst")
    expect_no_error(concatenate(
        dst, "cell", list(a, b),
        dataset_axis = NULL, prefix = FALSE,
        merge = list("*|*|*" = MERGE_LAST_VALUE)
    ))
    # cell-x-empty-axis stitched to 5 x 0 (not picked from src_b 3 x 0).
    ec <- get_matrix(dst, "cell", "empty_axis", "ec")
    expect_equal(dim(ec), c(5L, 0L))
    # cell-x-gene stitched to 5 x 3.
    u <- get_matrix(dst, "cell", "gene", "u")
    expect_equal(dim(u), c(5L, 3L))
})

# ---- Sparse drop0 / explicit-zero preservation (Round-7 follow-up) ----
test_that("sparse matrices preserve (i, p, x) structure (no silent drop0)", {
    # Matrix with an explicit zero stored in @x.
    mat <- Matrix::sparseMatrix(
        i    = c(1L, 1L, 3L, 5L),
        j    = c(1L, 2L, 2L, 3L),
        x    = c(5.0, 0.0, 7.0, 9.0),
        dims = c(5L, 3L)
    )
    expect_equal(mat@x, c(5, 0, 7, 9))  # sanity: input keeps the zero

    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE), add = TRUE)
    d <- files_daf(p, mode = "w")
    add_axis(d, "cell", sprintf("c%d", 1:5))
    add_axis(d, "gene", sprintf("g%d", 1:3))
    set_matrix(d, "cell", "gene", "m", mat)
    rm(d)

    r <- files_daf(p, mode = "r")
    out <- as(get_matrix(r, "cell", "gene", "m"), "CsparseMatrix")
    expect_equal(out@i, mat@i)
    expect_equal(out@p, mat@p)
    expect_equal(out@x, mat@x)   # explicit zero must survive
})

test_that("copy_all does not silently drop0 sparse matrices", {
    src <- memory_daf("src")
    add_axis(src, "cell", sprintf("c%d", 1:5))
    add_axis(src, "gene", sprintf("g%d", 1:3))
    mat <- Matrix::sparseMatrix(
        i    = c(1L, 1L, 3L, 5L),
        j    = c(1L, 2L, 2L, 3L),
        x    = c(5.0, 0.0, 7.0, 9.0),
        dims = c(5L, 3L)
    )
    set_matrix(src, "cell", "gene", "m", mat)

    p <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(p, recursive = TRUE), add = TRUE)
    dst <- zarr_daf(p, mode = "w")
    copy_all(dst, src, relayout = FALSE)
    rm(dst)
    r <- zarr_daf(p, mode = "r")
    out <- as(get_matrix(r, "cell", "gene", "m"), "CsparseMatrix")
    expect_equal(out@x, mat@x)   # explicit zero survives copy_all
})

# ---- G7: set_matrix rejects mismatched dimnames (Julia parity) ----
# Julia data.jl `set_matrix > named > !rows|!columns > name`: a named
# matrix whose row/column names don't match the axis entries must
# raise rather than silently overwrite the names from the axis.
test_that("set_matrix raises on mismatched rownames", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("g1", "g2", "g3", "g4"))
    bad <- matrix(seq_len(12L), nrow = 3L, ncol = 4L,
                  dimnames = list(c("X", "Y", "Z"),
                                  c("g1", "g2", "g3", "g4")))
    expect_error(
        set_matrix(d, "cell", "gene", "UMIs", bad),
        "row names of the: matrix.*mismatch the entry names of the axis: cell"
    )
})

test_that("set_matrix raises on mismatched colnames", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("g1", "g2", "g3", "g4"))
    bad <- matrix(seq_len(12L), nrow = 3L, ncol = 4L,
                  dimnames = list(c("A", "B", "C"),
                                  c("X", "Y", "Z", "W")))
    expect_error(
        set_matrix(d, "cell", "gene", "UMIs", bad),
        "column names of the: matrix.*mismatch the entry names of the axis: gene"
    )
})

test_that("set_matrix accepts a matrix with correct dimnames", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("g1", "g2"))
    good <- matrix(1:6, nrow = 3L, ncol = 2L,
                   dimnames = list(c("A", "B", "C"), c("g1", "g2")))
    expect_no_error(set_matrix(d, "cell", "gene", "UMIs", good))
})

test_that("set_matrix accepts a matrix with no dimnames", {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("g1", "g2"))
    plain <- matrix(1:6, nrow = 3L, ncol = 2L)
    expect_no_error(set_matrix(d, "cell", "gene", "UMIs", plain))
})

# ---- Bug B/harness: FilesDaf scalar strings come back UTF-8 tagged ----
test_that("FilesDaf scalar strings preserve UTF-8 encoding tag", {
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE), add = TRUE)
    d <- files_daf(p, mode = "w")
    s <- "alpha-á-ß-☃"   # á ß ☃
    set_scalar(d, "u", s)
    rm(d)
    r <- files_daf(p, mode = "r")
    got <- get_scalar(r, "u")
    expect_equal(got, s)
    expect_equal(Encoding(got), "UTF-8")
})
