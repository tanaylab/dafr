# integer64 inputs were byte-reinterpreted by .op_round / .op_convert
# (which call storage.mode(x) <- target on REALSXP bytes). Julia DAF.jl
# operates on the storage value. Cover both Round and Convert.

.fx <- function() {
    d <- memory_daf(name = "i64e")
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    set_vector(d, "cell", "age",
        bit64::as.integer64(c(10L, 20L, 30L, 40L, 50L)))
    d
}

test_that("% Round type Int8 on integer64 preserves values", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Round type Int8")
    expect_equal(as.integer(unname(v)), c(10L, 20L, 30L, 40L, 50L))
})

test_that("% Round (no target type) on integer64 stays as int values", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Round")
    expect_equal(as.double(unname(v)), c(10, 20, 30, 40, 50))
})

test_that("% Convert type Float32 on integer64 returns plain numeric values", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Convert type Float32")
    expect_false(inherits(v, "integer64"))
    expect_equal(unname(v), c(10, 20, 30, 40, 50))
})

test_that("% Convert type integer on integer64 returns integer values", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Convert type integer")
    expect_false(inherits(v, "integer64"))
    expect_equal(as.integer(unname(v)), c(10L, 20L, 30L, 40L, 50L))
})

test_that("% Abs on integer64 still works (sanity, was already OK)", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Abs")
    expect_equal(as.double(unname(v)), c(10, 20, 30, 40, 50))
})

test_that("% Log base 2 on integer64 returns floats", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Log base 2")
    expect_equal(unname(v), log2(c(10, 20, 30, 40, 50)), tolerance = 1e-12)
})
