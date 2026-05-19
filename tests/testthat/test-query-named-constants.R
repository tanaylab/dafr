# Julia DAF.jl recognises named constants (pi, e, true, false) in
# IfMissing defaults and in Log base/eps even when an explicit type is
# given. dafr's .coerce_if_missing_default only recognised them when the
# type was omitted, and .require_numeric_param rejected "e" / "pi"
# entirely for Log base.

.fx <- function() {
    d <- memory_daf(name = "k")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "age", c(10L, 20L, 30L))
    d
}

test_that("|| pi Float32 returns the numeric value of pi", {
    d <- .fx()
    expect_equal(get_query(d, ". missing_scalar || pi Float32"), pi)
})

test_that("|| pi Float64 returns the numeric value of pi", {
    d <- .fx()
    expect_equal(get_query(d, ". missing_scalar || pi Float64"), pi)
})

test_that("|| e Float64 returns exp(1)", {
    d <- .fx()
    expect_equal(get_query(d, ". missing_scalar || e Float64"), exp(1))
})

test_that("|| pi (no type) still works (regression)", {
    d <- .fx()
    expect_equal(get_query(d, ". missing_scalar || pi"), pi)
})

test_that("|| true Bool yields TRUE", {
    d <- .fx()
    expect_true(get_query(d, ". missing_scalar || true Bool"))
})

test_that("vector |Lookup || pi Float32 fills missing entries with pi", {
    d <- .fx()
    v <- get_query(d, "@ cell : missing_prop || pi Float32")
    expect_equal(unname(v), rep(pi, 3))
})

test_that("Log base e produces natural log", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Log base e")
    expect_equal(unname(v), log(c(10, 20, 30)), tolerance = 1e-12)
})

test_that("Log base pi produces log_pi(x)", {
    d <- .fx()
    v <- get_query(d, "@ cell : age % Log base pi")
    expect_equal(unname(v), log(c(10, 20, 30), base = pi), tolerance = 1e-12)
})
