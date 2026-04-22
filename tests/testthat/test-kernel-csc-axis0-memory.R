# Slice 9d-N regression guards for the CSC axis=0 row-partition rewrite.
# Two test_that blocks:
#   1. Bit-identity: parallel dispatch (threshold = 1L) matches serial
#      dispatch (threshold = .Machine$integer.max) on a 2k x 2k fixture
#      for all six row-partitioned kernels.
#   2. Peak-RSS: at the larger 100k x 5k fixture, one representative
#      category-A kernel stays under a 100 MB bench_process_memory delta.
#      Pre-fix the thread-bucket pattern would allocate 128 * 100k * 16B
#      ~= 200 MB at 128 threads - reintroducing it will fail this test.

test_that("CSC axis=0 row-partition is bit-identical to serial dispatch", {
    skip_on_cran()
    set.seed(42L)
    nr <- 2000L
    nc <- 2000L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc),
        repr = "C"
    )

    # kernel_var_csc - all four variants share the axis=0 path.
    for (variant in c("Var", "Std", "VarN", "StdN")) {
        par <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                  axis = 0L, variant = variant, eps = 1e-6,
                                  threshold = 1L)
        ser <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                  axis = 0L, variant = variant, eps = 1e-6,
                                  threshold = .Machine$integer.max)
        expect_identical(par, ser)
    }

    # kernel_minmax_csc - Min and Max.
    for (variant in c("Min", "Max")) {
        par <- kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     axis = 0L, variant = variant,
                                     threshold = 1L)
        ser <- kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     axis = 0L, variant = variant,
                                     threshold = .Machine$integer.max)
        expect_identical(par, ser)
    }

    # kernel_log_reduce - Sum and Mean reducers.
    for (reducer in c("Sum", "Mean")) {
        par <- kernel_log_reduce_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            eps = 1e-5, base = 2,
            axis = 0L, reducer = reducer, threshold = 1L
        )
        ser <- kernel_log_reduce_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            eps = 1e-5, base = 2,
            axis = 0L, reducer = reducer,
            threshold = .Machine$integer.max
        )
        expect_identical(par, ser)
    }

    # kernel_geomean_csc - both eps > 0 and eps == 0 paths.
    for (eps in c(0, 1e-6)) {
        par <- kernel_geomean_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, eps = eps, threshold = 1L
        )
        ser <- kernel_geomean_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, eps = eps,
            threshold = .Machine$integer.max
        )
        expect_identical(par, ser)
    }

    # kernel_mode_csc - single axis=0 path.
    par_mode <- kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                    axis = 0L, threshold = 1L)
    ser_mode <- kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                    axis = 0L,
                                    threshold = .Machine$integer.max)
    expect_identical(par_mode, ser_mode)

    # kernel_quantile_csc - q = 0.5 (median); q-choice does not affect
    # the fill-pass parallelism under test.
    par_q <- kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     axis = 0L, q = 0.5, threshold = 1L)
    ser_q <- kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     axis = 0L, q = 0.5,
                                     threshold = .Machine$integer.max)
    expect_identical(par_q, ser_q)
})

test_that("kernel_var_csc axis=0 peak RSS stays under 100 MB on stress fixture", {
    skip_on_cran()
    skip_if_not_installed("bench")

    # NOTE: libgomp caches max_threads at DSO-load time, so a runtime
    # Sys.setenv(OMP_NUM_THREADS=...) does NOT change the thread count.
    # The test asserts peak RSS under whatever thread count libgomp
    # picked up - typically parallel::detectCores(). On a 128-thread
    # dev machine that means 128 threads; on an 8-thread CI box, 8.
    # Pre-fix the O(nthreads * nrow) bucket pattern allocated 16 B per
    # (thread, row); at 128 threads x 100k rows that is ~200 MB, well
    # over this 100 MB budget. Post-fix the footprint is bounded by the
    # output shape plus two nrow-sized double vectors (1.6 MB total).

    set.seed(42L)
    nr <- 100000L
    nc <- 5000L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc),
        repr = "C"
    )

    gc(full = TRUE)
    mem_before <- bench::bench_process_memory()
    out <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                              axis = 0L, variant = "Var", eps = 0,
                              threshold = 1L)
    mem_after <- bench::bench_process_memory()

    delta <- as.numeric(mem_after["max"]) - as.numeric(mem_before["max"])
    expect_lt(delta, 100 * 1024 * 1024)

    # Sanity: output shape and finite values.
    expect_equal(length(out), nr)
    expect_true(all(is.finite(out)))
})
