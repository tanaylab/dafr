test_that("kernel_log_reduce_dense matches Log + Sum unfused", {
    skip_if_not(exists("kernel_log_reduce_dense_cpp",
        envir = asNamespace("dafr")
    ))
    m <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 2)
    eps <- 1.0
    base <- exp(1)
    expected <- rowSums(log(m + eps))
    actual <- dafr:::kernel_log_reduce_dense_cpp(
        m, eps, base,
        axis = 0L, reducer = "Sum",
        threshold = 1L
    )
    expect_equal(actual, expected)
})

test_that("kernel_log_reduce_csc matches Log + Sum unfused", {
    skip_if_not(exists("kernel_log_reduce_csc_cpp",
        envir = asNamespace("dafr")
    ))
    skip_if_not_installed("Matrix")
    m <- methods::as(matrix(c(1, 0, 3, 0, 5, 0), nrow = 2), "dgCMatrix")
    eps <- 1.0
    base <- exp(1)
    # log(0 + 1) = 0, so zero entries contribute 0 to the sum.
    expected <- as.numeric(Matrix::rowSums(log(m + eps)))
    actual <- dafr:::kernel_log_reduce_csc_cpp(
        m@x, m@i, m@p,
        nrow(m), ncol(m),
        eps, base,
        axis = 0L, reducer = "Sum",
        threshold = 1L
    )
    expect_equal(actual, expected)
})

test_that("get_query routes UMIs % Log eps:1 >| Sum through fused kernel", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- methods::as(
        matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE),
        "dgCMatrix"
    )
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 1 >| Sum")
    expected <- as.numeric(Matrix::rowSums(log1p(m)))
    expect_equal(unname(result), expected)
})

test_that("Mean variant matches", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 1 >| Mean")
    expected <- rowMeans(log(m + 1))
    expect_equal(unname(result), expected)
})

test_that("fused CSC path: eps:2 on dgCMatrix routes through fused kernel", {
    # eps != 1 means P2's sparsity-preserving Log fast path bails; the
    # eltwise falls through to densify-then-reduce in the unfused path,
    # OR the P4 peephole dispatches to kernel_log_reduce_csc_cpp. This
    # test asserts numeric equivalence against the unfused expected value.
    # Combined with the direct-kernel tests (1 and 2), coverage is real.
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- methods::as(matrix(c(1, 0, 3, 0, 5, 0), nrow = 2), "dgCMatrix")
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 2 >| Sum")
    expected <- as.numeric(Matrix::rowSums(log(m + 2)))
    expect_equal(unname(result), expected)
})

test_that("fused dense path: integer matrix UMIs coerced to double", {
    # Integer UMI matrices are the canonical real-world input. The fused
    # dense kernel takes cpp11::doubles_matrix<>; the peephole must coerce
    # integer to double before dispatching. Without the coercion, the
    # cpp11 glue throws "Invalid input type, expected 'double' actual
    # 'integer'".
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- matrix(c(1L, 2L, 3L, 4L, 5L, 6L), nrow = 2, byrow = TRUE)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 1 >| Sum")
    expected <- rowSums(log(m + 1))
    expect_equal(unname(result), expected)
})
