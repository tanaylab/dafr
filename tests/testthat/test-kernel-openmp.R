# Coverage for the parallel branch of the two OpenMP-enabled kernels.
#
# The kernels guard the omp parallel-for with a size condition
# (`n >= 10000` for log_add, `ncol >= 1000` for colsums). We run the kernels
# at `max(dafr.omp_threshold, 20000)` so the parallel branch fires whenever
# the package was built with OpenMP. On serial builds these are still valid
# correctness tests at large size -- they just don't observe multi-threading.

test_that("kernel_log_add triggers the OMP branch (n >= threshold)", {
    threshold <- getOption("dafr.omp_threshold", 10000L)
    n <- as.integer(max(threshold, 20000L))
    set.seed(1L)
    x <- runif(n, min = 0.1, max = 10)
    y <- runif(n)
    out <- dafr:::kernel_log_add_cpp(x, y, threshold = dafr:::dafr_opt("dafr.omp_threshold"))
    expect_equal(out, log(x) + y, tolerance = 1e-12)
})

test_that("kernel_csc_colsums triggers the OMP branch (ncol >= threshold)", {
    threshold <- getOption("dafr.omp_threshold", 10000L)
    ncol <- as.integer(max(threshold, 20000L))
    nrow <- 50L
    set.seed(2L)
    m <- Matrix::rsparsematrix(nrow, ncol, density = 0.02)
    m <- as(m, "CsparseMatrix")
    out <- dafr:::kernel_csc_colsums_cpp(m@x, m@p, ncol, threshold = dafr:::dafr_opt("dafr.omp_threshold"))
    expect_equal(out, unname(Matrix::colSums(m)), tolerance = 1e-12)
})
