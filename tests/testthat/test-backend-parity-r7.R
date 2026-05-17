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
