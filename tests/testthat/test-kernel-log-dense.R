# Tests for the dense Log eltwise fast-path.
# U1: extends .apply_eltwise beyond the sparsity-preserving log1p case.

test_that("dense log fast-path matches log2(x + eps)", {
    set.seed(1L)
    x <- matrix(runif(600, 0.01, 5), nrow = 20, ncol = 30)
    got <- dafr:::kernel_log_dense_mat_cpp(x, eps = 1e-5, base = 2, threshold = 0L)
    expect_equal(got, log2(x + 1e-5), tolerance = 1e-12)
})

test_that("dense log fast-path matches log10 and log", {
    x <- matrix(c(1, 10, 100, 1000, 0.5, 2), nrow = 2, ncol = 3)
    expect_equal(
        dafr:::kernel_log_dense_mat_cpp(x, eps = 0, base = 10, threshold = 0L),
        log10(x),
        tolerance = 1e-12
    )
    expect_equal(
        dafr:::kernel_log_dense_mat_cpp(x, eps = 0, base = exp(1), threshold = 0L),
        log(x),
        tolerance = 1e-12
    )
})

test_that("vector log fast-path preserves names", {
    v <- c(a = 1, b = 10, c = 100)
    expect_equal(
        dafr:::kernel_log_dense_vec_cpp(v, eps = 0, base = 10, threshold = 0L),
        unname(log10(v)),
        tolerance = 1e-12
    )
})

test_that("eltwise Log on dense matrix goes through fast path", {
    set.seed(2L)
    d <- dafr::memory_daf(name = "log_dense_fast_test")
    dafr::add_axis(d, "row", paste0("r", 1:10))
    dafr::add_axis(d, "col", paste0("c", 1:5))
    m <- matrix(runif(50, 0.1, 5), nrow = 10, ncol = 5)
    rownames(m) <- paste0("r", 1:10)
    colnames(m) <- paste0("c", 1:5)
    dafr::set_matrix(d, "row", "col", "x", m)

    out <- d["@ row @ col :: x % Log eps: 1e-5 base: 2"]
    expect_equal(as.vector(out), as.vector(log2(m + 1e-5)), tolerance = 1e-12)
})

test_that("eltwise Log respects dafr.perf.fast_paths option", {
    d <- dafr::memory_daf(name = "log_dense_opt_test")
    dafr::add_axis(d, "row", paste0("r", 1:5))
    dafr::add_axis(d, "col", paste0("c", 1:3))
    m <- matrix(runif(15, 0.1, 5), nrow = 5, ncol = 3)
    rownames(m) <- paste0("r", 1:5)
    colnames(m) <- paste0("c", 1:3)
    dafr::set_matrix(d, "row", "col", "x", m)

    with_fast  <- d["@ row @ col :: x % Log eps: 1e-5 base: 2"]

    opt <- getOption("dafr.perf.fast_paths")
    options(dafr.perf.fast_paths = FALSE)
    on.exit(options(dafr.perf.fast_paths = opt), add = TRUE)
    without_fast <- d["@ row @ col :: x % Log eps: 1e-5 base: 2"]

    expect_equal(as.vector(with_fast), as.vector(without_fast), tolerance = 1e-12)
})
