# Parity port of DataAxesFormats.jl test/queries.jl > vector > {axis, mask}.
# Covers the mask-combinator leaves dafr did not previously exercise:
#   - matrix-derived masks (`[ UMIs @ gene = A > 0 ]`)
#   - square-matrix masks (`[ distance @| / @- entry ]`)
#   - boolean combinators with negated first operand
#   - mask combo with matrix/square-matrix sub-mask

# ---- axis identity --------------------------------------------------------

test_that("`@ axis` returns axis entries (identity name -> name)", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "gene", c("A", "B"))
    out <- get_query(d, "@ gene")
    expect_equal(out, c("A", "B"))
    # Names are the entries themselves (the vector is identity-named).
    expect_equal(names(setNames(out, out)), out)
})

# ---- matrix-derived mask --------------------------------------------------

test_that("[ UMIs @ gene = A > 0 ] selects cells with UMIs > 0 in gene A", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B"))
    set_matrix(d, "gene", "cell", "UMIs", matrix(
        c(1, 0, 0, 1), nrow = 2L,
        dimnames = list(c("A", "B"), c("X", "Y"))
    ))
    expect_equal(get_query(d, "@ cell [ UMIs @ gene = A > 0 ]"), "X")
    expect_equal(get_query(d, "@ cell [ ! UMIs @ gene = A > 0 ]"), "Y")
})

# ---- square-matrix masks --------------------------------------------------

test_that("[ distance @| entry ] uses a column of a square matrix as a mask", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_matrix(d, "cell", "cell", "distance", matrix(
        c(0, 1, 1, 0), nrow = 2L,
        dimnames = list(c("X", "Y"), c("X", "Y"))
    ))
    expect_equal(get_query(d, "@ cell [ distance @| X ]"), "Y")
    expect_equal(get_query(d, "@ cell [ ! distance @| X ]"), "X")
    expect_equal(get_query(d, "@ cell [ distance @| Y ]"), "X")
    expect_equal(get_query(d, "@ cell [ ! distance @| Y ]"), "Y")
})

test_that("[ distance @- entry ] uses a row of a square matrix as a mask", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    set_matrix(d, "cell", "cell", "distance", matrix(
        c(0, 1, 1, 0), nrow = 2L,
        dimnames = list(c("X", "Y"), c("X", "Y"))
    ))
    expect_equal(get_query(d, "@ cell [ distance @- X ]"), "Y")
    expect_equal(get_query(d, "@ cell [ ! distance @- X ]"), "X")
    expect_equal(get_query(d, "@ cell [ distance @- Y ]"), "X")
    expect_equal(get_query(d, "@ cell [ ! distance @- Y ]"), "Y")
})

# ---- boolean combinators with negated first operand -----------------------
# These verify the fix that `!` on the *first* property does not propagate
# to the whole bracket — `[ ! a & b ]` = `(!a) & b`, not `!(a & b)`.

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("LE", "LO", "HE", "HO"))
        set_vector(d, "cell", "is_low", c(TRUE, TRUE, FALSE, FALSE))
        set_vector(d, "cell", "is_even", c(TRUE, FALSE, TRUE, FALSE))
        d
    }

    test_that("AND with negated first operand: [ ! is_low & is_even ]", {
        d <- setup()
        expect_equal(get_query(d, "@ cell [ is_low & is_even ]"), "LE")
        expect_equal(get_query(d, "@ cell [ ! is_low & is_even ]"), "HE")
        expect_equal(get_query(d, "@ cell [ is_low & ! is_even ]"), "LO")
        expect_equal(get_query(d, "@ cell [ ! is_low & ! is_even ]"), "HO")
    })

    test_that("OR with negated first operand: [ ! is_low | is_even ]", {
        d <- setup()
        expect_setequal(
            get_query(d, "@ cell [ is_low | is_even ]"),
            c("LE", "LO", "HE")
        )
        expect_setequal(
            get_query(d, "@ cell [ ! is_low | is_even ]"),
            c("LE", "HE", "HO")
        )
        expect_setequal(
            get_query(d, "@ cell [ is_low | ! is_even ]"),
            c("LE", "LO", "HO")
        )
        expect_setequal(
            get_query(d, "@ cell [ ! is_low | ! is_even ]"),
            c("LO", "HE", "HO")
        )
    })

    test_that("XOR with negated first operand: [ ! is_low ^ is_even ]", {
        d <- setup()
        expect_setequal(
            get_query(d, "@ cell [ is_low ^ is_even ]"), c("LO", "HE")
        )
        expect_setequal(
            get_query(d, "@ cell [ ! is_low ^ is_even ]"), c("LE", "HO")
        )
        expect_setequal(
            get_query(d, "@ cell [ is_low ^ ! is_even ]"), c("LE", "HO")
        )
        expect_setequal(
            get_query(d, "@ cell [ ! is_low ^ ! is_even ]"), c("LO", "HE")
        )
    })
})

# ---- mask combo with matrix-derived sub-mask ------------------------------

test_that("[ is_low & UMIs @ gene = entry ] combines vector and matrix mask", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("LE", "LO", "HE", "HO"))
    add_axis(d, "gene", c("A", "B"))
    set_vector(d, "cell", "is_low", c(TRUE, TRUE, FALSE, FALSE))
    set_matrix(d, "gene", "cell", "UMIs", matrix(
        c(1, 1, 1, 0, 0, 1, 0, 0), nrow = 2L,
        dimnames = list(c("A", "B"), c("LE", "LO", "HE", "HO"))
    ))
    expect_equal(
        get_query(d, "@ cell [ is_low & UMIs @ gene = B ]"), "LE"
    )
})

# ---- mask combo with square-matrix sub-mask --------------------------------

test_that("[ is_low & distance @| entry ] combines vector mask with square-col", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("LE", "LO", "HE", "HO"))
    set_vector(d, "cell", "is_low", c(TRUE, TRUE, FALSE, FALSE))
    set_matrix(d, "cell", "cell", "distance", matrix(
        c(0, 0, 0, 0,
          1, 0, 0, 0,
          1, 1, 0, 0,
          1, 1, 1, 0),
        nrow = 4L,
        dimnames = list(c("LE", "LO", "HE", "HO"),
                        c("LE", "LO", "HE", "HO"))
    ))
    expect_equal(
        get_query(d, "@ cell [ is_low & distance @| LO ]"), "LE"
    )
})

test_that("[ ! is_low & distance @- entry ] combines negated vector with square-row", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("LE", "LO", "HE", "HO"))
    set_vector(d, "cell", "is_low", c(TRUE, TRUE, FALSE, FALSE))
    set_matrix(d, "cell", "cell", "distance", matrix(
        c(0, 0, 0, 0,
          1, 0, 0, 0,
          1, 1, 0, 0,
          1, 1, 1, 0),
        nrow = 4L,
        dimnames = list(c("LE", "LO", "HE", "HO"),
                        c("LE", "LO", "HE", "HO"))
    ))
    expect_setequal(
        get_query(d, "@ cell [ ! is_low & distance @- LO ]"),
        c("HE", "HO")
    )
})
