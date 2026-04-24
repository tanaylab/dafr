# Tests for fast_log() — exported user-facing wrapper around the
# kernel_log_dense_{vec,mat}_cpp primitives.

test_that("fast_log on vectors matches log(x + eps, base)", {
    v <- c(1, 2, 5, 10, 100)
    expect_equal(fast_log(v), log(v))
    expect_equal(fast_log(v, base = 2), log2(v))
    expect_equal(fast_log(v, base = 10), log10(v))
    expect_equal(fast_log(v, eps = 1e-5, base = 2), log2(v + 1e-5))
})

test_that("fast_log on matrices matches log2(x + eps)", {
    set.seed(1L)
    m <- matrix(runif(60, 0.01, 5), nrow = 6, ncol = 10)
    expect_equal(fast_log(m, eps = 1e-5, base = 2), log2(m + 1e-5),
                 tolerance = 1e-12)
})

test_that("fast_log preserves names and dimnames", {
    v <- c(a = 1, b = 10, c = 100)
    out_v <- fast_log(v, base = 10)
    expect_equal(names(out_v), names(v))

    m <- matrix(1:12, 3, 4)
    rownames(m) <- paste0("r", 1:3)
    colnames(m) <- paste0("c", 1:4)
    out_m <- fast_log(m)
    expect_equal(rownames(out_m), rownames(m))
    expect_equal(colnames(out_m), colnames(m))
})

test_that("fast_log promotes integer input to double", {
    v <- 1:5
    expect_equal(fast_log(v), log(as.double(v)))
    m <- matrix(1:12, 3, 4)
    expect_equal(fast_log(m), log(matrix(as.double(1:12), 3, 4)))
})

test_that("fast_log rejects bad input", {
    expect_error(fast_log("abc"), "numeric")
    expect_error(fast_log(list(1, 2)), "numeric")
    expect_error(fast_log(1:5, eps = -1), "non-negative")
    expect_error(fast_log(1:5, base = 0), "positive")
    expect_error(fast_log(1:5, base = -2), "positive")
    expect_error(fast_log(1:5, eps = c(1, 2)), "single")
    expect_error(fast_log(1:5, base = c(2, 10)), "single")
})

test_that("fast_log result matches DSL fast path on memory_daf", {
    set.seed(2L)
    m <- matrix(runif(30, 0.1, 5), nrow = 5, ncol = 6)
    rownames(m) <- paste0("r", 1:5)
    colnames(m) <- paste0("c", 1:6)

    d <- memory_daf(name = "fast_log_dsl_parity")
    add_axis(d, "row", rownames(m))
    add_axis(d, "col", colnames(m))
    set_matrix(d, "row", "col", "x", m)

    via_dsl  <- d["@ row @ col :: x % Log eps: 1e-5 base: 2"]
    via_fast <- fast_log(m, eps = 1e-5, base = 2)
    expect_equal(as.vector(via_fast), as.vector(via_dsl), tolerance = 1e-12)
})
