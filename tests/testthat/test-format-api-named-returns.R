# Format-API named-return contract: every backend's format_get_vector
# returns a named atomic vector (names = axis entries in axis order),
# and every format_get_matrix returns a matrix / dgCMatrix / lgCMatrix
# whose dimnames are list(rows-axis entries, cols-axis entries).

.fixture_named_memory_daf <- function() {
    d <- memory_daf(name = "names-fixture")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("gA", "gB"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
    set_matrix(d, "cell", "gene", "expr",
        matrix(c(1.0, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    )
    set_matrix(d, "cell", "gene", "expr_sparse",
        Matrix::sparseMatrix(
            i = c(1L, 3L), j = c(1L, 2L), x = c(7, 9),
            dims = c(3L, 2L), repr = "C"
        )
    )
    d
}

test_that("MemoryDaf format_get_vector returns named atomic", {
    d <- .fixture_named_memory_daf()
    v <- format_get_vector(d, "cell", "donor")
    expect_equal(names(v), c("c1", "c2", "c3"))
    expect_equal(unname(v), c("d1", "d2", "d1"))
})

test_that("MemoryDaf format_get_matrix returns dense with axis dimnames", {
    d <- .fixture_named_memory_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr")
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("MemoryDaf format_get_matrix returns sparse with @Dimnames", {
    d <- .fixture_named_memory_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")
    expect_s4_class(m, "dgCMatrix")
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})

.fixture_named_files_daf <- function(envir = parent.frame()) {
    src <- .fixture_named_memory_daf()
    root <- tempfile(pattern = "dafr-names-")
    dir.create(root)
    withr::defer(unlink(root, recursive = TRUE), envir = envir)
    dst <- files_daf(root, mode = "w+")
    copy_all(dst, src)
    dst
}

test_that("FilesDaf format_get_vector returns named atomic", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    v <- format_get_vector(d, "cell", "donor")
    expect_equal(names(v), c("c1", "c2", "c3"))
    expect_equal(unname(v), c("d1", "d2", "d1"))
})

test_that("FilesDaf format_get_matrix returns dense with axis dimnames", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr")
    expect_equal(rownames(m), c("c1", "c2", "c3"))
    expect_equal(colnames(m), c("gA", "gB"))
})

test_that("FilesDaf format_get_matrix returns sparse with @Dimnames", {
    skip_if_not_installed("withr")
    d <- .fixture_named_files_daf()
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")
    expect_s4_class(m, "dgCMatrix")
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})

test_that("FilesDafReadOnly inherits the named contract", {
    skip_if_not_installed("withr")
    d <- read_only(.fixture_named_files_daf())
    expect_equal(names(format_get_vector(d, "cell", "donor")),
                 c("c1", "c2", "c3"))
    m <- format_get_matrix(d, "cell", "gene", "expr_sparse")
    expect_equal(m@Dimnames, list(c("c1", "c2", "c3"), c("gA", "gB")))
})
