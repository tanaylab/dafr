# Cross-language interop for the H5df (.h5df) backend against
# DataAxesFormats.jl. Gated on the conda julia env (helper-julia.R).
skip_if_no_hdf5r <- function() testthat::skip_if_not_installed("hdf5r")

test_that("R-written .h5df is readable by Julia with identical values", {
    skip_if_no_hdf5r()
    skip_if_not(.have_julia_env())
    p <- tempfile(fileext = ".h5df")
    d <- h5df(p, mode = "w")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    set_scalar(d, "pi", 3.14)
    set_scalar(d, "note", "hello")
    set_vector(d, "cell", "donor", c(1L, 2L, 3L, 4L))
    set_vector(d, "cell", "score", c(1.5, 2.5, 3.5, 4.5))
    set_vector(d, "cell", "flag", c(TRUE, FALSE, TRUE, TRUE))
    set_matrix(d, "cell", "gene", "dm", matrix(as.double(1:8), nrow = 4))
    sp <- Matrix::sparseMatrix(i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30), dims = c(4, 2))
    set_matrix(d, "cell", "gene", "sm", sp)
    rm(d); gc()
    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = H5df(raw"%s", "r")', p),
        '@assert get_scalar(daf, "pi")   == 3.14',
        '@assert get_scalar(daf, "note") == "hello"',
        '@assert axis_vector(daf, "cell") == ["A","B","C","D"]',
        '@assert get_vector(daf, "cell", "donor") == Int32[1,2,3,4]',
        '@assert get_vector(daf, "cell", "score") == Float64[1.5,2.5,3.5,4.5]',
        '@assert get_vector(daf, "cell", "flag") == Bool[1,0,1,1]',
        '@assert get_matrix(daf, "cell", "gene", "dm") == Float64[1 5; 2 6; 3 7; 4 8]',
        'sm = get_matrix(daf, "cell", "gene", "sm")',
        '@assert size(sm) == (4,2)',
        '@assert Matrix(sm) == Float64[10 0; 0 30; 20 0; 0 0]',
        'println("JULIA_H5DF_OK")'
    )
    out <- run_julia(script)
    expect_true(any(grepl("JULIA_H5DF_OK", out)), info = paste(out, collapse = "\n"))
})

test_that("Julia-written .h5df is readable by R with identical values", {
    skip_if_no_hdf5r()
    skip_if_not(.have_julia_env())
    p <- tempfile(fileext = ".h5df")
    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = H5df(raw"%s", "w")', p),
        'add_axis!(daf, "cell", ["A","B","C","D"])',
        'add_axis!(daf, "gene", ["X","Y"])',
        'set_scalar!(daf, "pi", 3.14)',
        'set_vector!(daf, "cell", "donor", Int32[1,2,3,4])',
        'set_matrix!(daf, "cell", "gene", "dm", Float64[1 5; 2 6; 3 7; 4 8])',
        'set_matrix!(daf, "cell", "gene", "sm", sparse(Float64[10 0; 0 30; 20 0; 0 0]))',
        'println("JULIA_WROTE")'
    )
    out <- run_julia(script)
    skip_if_not(any(grepl("JULIA_WROTE", out)), paste(out, collapse = "\n"))
    d <- h5df(p, mode = "r")
    expect_equal(get_scalar(d, "pi"), 3.14)
    expect_equal(as.integer(get_vector(d, "cell", "donor")), 1:4)
    expect_equal(as.vector(get_matrix(d, "cell", "gene", "dm")), as.double(1:8), ignore_attr = TRUE)
    sm <- get_matrix(d, "cell", "gene", "sm")
    expect_equal(as.matrix(sm),
        matrix(c(10, 0, 20, 0, 0, 30, 0, 0), 4, 2), ignore_attr = TRUE)
    rm(d); gc()
})
