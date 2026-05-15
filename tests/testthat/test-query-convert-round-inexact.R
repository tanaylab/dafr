# Julia DAF.jl Convert / Round raise InexactError when the target integer
# type can't losslessly hold a value (non-integer fraction, out-of-range,
# or non-finite). dafr previously:
#   - rejected Julia's UInt8/Int8/etc aliases in Convert ('type must be
#     one of double, integer, logical, integer64')
#   - silently coerced .5 / negative / out-of-range values via storage.mode
# These checks now match Julia's parse_int_type_value / InexactError path.

.fx <- function() {
    d <- memory_daf(name = "ix")
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    set_vector(d, "cell", "score", c(0.5, 1.5, 2.5, -1.0, 3.5))
    set_vector(d, "cell", "age",   c(10, 20, 30, 40, 50))
    d
}

# --- Convert: aliases now accepted ---

test_that("Convert type UInt8 on in-range integer values succeeds", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Convert type UInt8")
    expect_equal(unname(as.integer(v)), c(10L, 20L, 30L, 40L, 50L))
})

test_that("Convert type Int16 / UInt16 on integer values succeeds", {
    d <- .fx()
    expect_equal(unname(as.integer(get_query(d, "@ cell : age % Convert type Int16"))),
        c(10L, 20L, 30L, 40L, 50L))
    expect_equal(unname(as.integer(get_query(d, "@ cell : age % Convert type UInt16"))),
        c(10L, 20L, 30L, 40L, 50L))
})

test_that("Convert type UInt8 on a fractional value raises InexactError", {
    d <- .fx()
    expect_error(get_query(d, "@ cell : score % Convert type UInt8"),
        "InexactError: UInt8\\(0\\.5\\)")
})

test_that("Convert type UInt8 on a negative value raises InexactError", {
    d <- memory_daf(name = "n")
    add_axis(d, "cell", c("A"))
    set_vector(d, "cell", "x", c(-1))
    expect_error(get_query(d, "@ cell : x % Convert type UInt8"),
        "InexactError: UInt8\\(-1\\)")
})

test_that("Convert type Int8 on a 200 raises InexactError (out of range)", {
    d <- memory_daf(name = "o")
    add_axis(d, "cell", c("A"))
    set_vector(d, "cell", "x", c(200))
    expect_error(get_query(d, "@ cell : x % Convert type Int8"),
        "InexactError: Int8\\(200\\)")
})

test_that("Convert type Bool on 0.5 raises InexactError (Julia parity)", {
    d <- .fx()
    expect_error(get_query(d, "@ cell : score % Convert type Bool"),
        "InexactError: Bool\\(0\\.5\\)")
})

test_that("Convert type Bool on a 0/1 vector succeeds", {
    d <- memory_daf(name = "b")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(0, 1, 1))
    v <- get_query(d, "@ cell : x % Convert type Bool")
    expect_equal(unname(v), c(FALSE, TRUE, TRUE))
})

# --- Round: existing aliases now range-checked ---

test_that("Round type UInt8 on -1.0 raises InexactError (was silently coerced)", {
    d <- .fx()
    expect_error(get_query(d, "@ cell : score % Round type UInt8"),
        "InexactError: UInt8\\(-1\\)")
})

test_that("Round type UInt32 on -1.0 raises InexactError", {
    d <- .fx()
    expect_error(get_query(d, "@ cell : score % Round type UInt32"),
        "InexactError: UInt32\\(-1\\)")
})

test_that("Round type Int8 on in-range integers succeeds", {
    d <- memory_daf(name = "rs")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(10, -20, 30))
    v <- get_query(d, "@ cell : x % Round type Int8")
    expect_equal(unname(as.integer(v)), c(10L, -20L, 30L))
})

test_that("Round type Int8 on 200 raises InexactError (out of Int8 range)", {
    d <- memory_daf(name = "rr")
    add_axis(d, "cell", c("A"))
    set_vector(d, "cell", "x", c(200))
    expect_error(get_query(d, "@ cell : x % Round type Int8"),
        "InexactError: Int8\\(200\\)")
})

# --- Abs / Convert chain: round behaves like Julia Round(x) then Convert ---

test_that("Abs type UInt8 on .5 values silently rounds nothing (just abs+cast)", {
    # This documents that % Abs type UInt8 still does Abs->Cast and the
    # cast on 0.5 should InexactError now.
    d <- .fx()
    expect_error(get_query(d, "@ cell : score % Abs type UInt8"),
        "InexactError: UInt8\\(0\\.5\\)")
})

test_that("% Round (no type) on NaN raises InexactError (Julia parity)", {
    # Julia % Round defaults to Int64 when no explicit type, and
    # InexactError fires on NaN since NaN -> Int is undefined.
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.0, NaN, 3.0))
    expect_error(get_query(d, "@ cell : x % Round"),
        "InexactError")
})
