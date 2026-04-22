# Slice 9d-M regression guards for the G3 row-partition rewrite.
# These exercise the axis == 3 branches of the three grouped CSC kernels
# at a size that triggers the parallel-dispatch path (threshold = 1L),
# asserting (a) bit-identical output against the serial-dispatch path
# (threshold = .Machine$integer.max), and (b) peak RSS stays bounded —
# if someone reintroduces the O(nthreads × nrow × ngroups) bucket
# pattern, this test catches it even at modest thread counts.

test_that("G3 row-partition is bit-identical to serial dispatch", {
    skip_on_cran()
    set.seed(42L)
    nr <- 2000L
    nc <- 2000L
    ngroups <- 20L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc),
        repr = "C"
    )
    group <- rep_len(seq_len(ngroups), nc)
    n_in_group <- tabulate(group, nbins = ngroups)

    # kernel_grouped_reduce_csc — Sum
    par_sum <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0, threshold = 1L
    )
    ser_sum <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0, threshold = .Machine$integer.max
    )
    expect_identical(par_sum, ser_sum)

    # kernel_grouped_reduce_csc — Var (uses sum_x2 too)
    par_var <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Var", eps = 0, threshold = 1L
    )
    ser_var <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Var", eps = 0, threshold = .Machine$integer.max
    )
    expect_identical(par_var, ser_var)

    # kernel_grouped_mode_csc
    par_mode <- kernel_grouped_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, threshold = 1L
    )
    ser_mode <- kernel_grouped_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, threshold = .Machine$integer.max
    )
    expect_identical(par_mode, ser_mode)

    # kernel_grouped_quantile_csc — p50 (median)
    par_q <- kernel_grouped_quantile_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, q = 0.5, threshold = 1L
    )
    ser_q <- kernel_grouped_quantile_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, q = 0.5, threshold = .Machine$integer.max
    )
    expect_identical(par_q, ser_q)
})

test_that("G3 row-partition peak RSS stays under 50 MB on stress fixture", {
    skip_on_cran()
    skip_if_not_installed("bench")

    # NOTE: libgomp caches max_threads at DSO-load time, so a runtime
    # Sys.setenv(OMP_NUM_THREADS=...) does NOT change the thread count.
    # The test asserts peak RSS under whatever thread count libgomp
    # picked up — typically parallel::detectCores(). On a 128-thread
    # dev machine that means 128 threads; on an 8-thread CI box, 8.
    # Pre-fix the bucket pattern allocates ~48 MB per thread; row-
    # partition's footprint is bounded by the output shape regardless.

    set.seed(42L)
    nr <- 2000L
    nc <- 2000L
    ngroups <- 20L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc),
        repr = "C"
    )
    group <- rep_len(seq_len(ngroups), nc)
    n_in_group <- tabulate(group, nbins = ngroups)

    gc(full = TRUE)
    mem_before <- bench::bench_process_memory()
    out <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0, threshold = 1L
    )
    mem_after <- bench::bench_process_memory()

    # Budget: 50 MB delta. Row-partition accs is nr*ngroups*48 B = 1.9 MB;
    # output matrix is nr*ngroups*8 B = 312 KB. Loose bound tolerates
    # allocator slack and bench's own overhead. Pre-fix at even 2 threads
    # would push the delta over 50 MB due to thread buckets.
    delta <- as.numeric(mem_after["max"]) - as.numeric(mem_before["max"])
    expect_lt(delta, 50 * 1024 * 1024)

    # Sanity: output shape and finite values.
    expect_equal(dim(out), c(nr, ngroups))
    expect_true(all(is.finite(out)))
})
