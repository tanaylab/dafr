test_that("ReduceToColumn Sum on dense matrix matches rowSums", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >| Sum")
    expect_identical(unname(result), as.numeric(rowSums(m)))
})

test_that("ReduceToColumn Sum on dgCMatrix uses Matrix::rowSums", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- methods::as(matrix(c(1, 0, 3, 0, 5, 0), nrow = 2), "dgCMatrix")
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >| Sum")
    expect_equal(unname(result), as.numeric(Matrix::rowSums(m)))
})

test_that("ReduceToColumn Mean matches rowMeans", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(2, 4, 6, 8), nrow = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >| Mean")
    expect_identical(unname(result), as.numeric(rowMeans(m)))
})

test_that("ReduceToRow Sum matches colSums", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4), nrow = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >- Sum")
    expect_identical(unname(result), as.numeric(colSums(m)))
})

test_that("Max via matrixStats::rowMaxs matches", {
    skip_if_not_installed("matrixStats")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 5, 9, 3), nrow = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >| Max")
    expect_identical(unname(result), as.numeric(matrixStats::rowMaxs(m)))
})

# Count is intentionally NOT in the fast path: the slow apply()-based path
# already evaluates to length(row), which is constant ncol(m); replicating
# that constant in a fast path would not save anything. Falls through to
# the slow path with the existing Slice-3 semantics.
