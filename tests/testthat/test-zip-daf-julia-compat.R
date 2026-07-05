# Cross-language interop for the ZipDaf (.daf.zip) backend against
# DataAxesFormats.jl 0.3.0. Gated on the conda julia env (helper-julia.R).

test_that("R-written .daf.zip is readable by Julia with identical values", {
    skip_if_not(.have_julia_env())
    p <- tempfile(fileext = ".daf.zip")
    d <- zip_daf(p, mode = "w")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("X", "Y"))
    set_scalar(d, "pi", 3.14)
    set_scalar(d, "cells", bit64::as.integer64(100))
    set_scalar(d, "note", "hello")
    set_vector(d, "cell", "donor", c(1L, 2L, 3L, 4L))
    set_vector(d, "cell", "sx", c(0, 10, 0, 30)) # sparsifies
    set_vector(d, "cell", "flags", c(TRUE, FALSE, TRUE, FALSE)) # Bool sparse
    set_matrix(d, "cell", "gene", "dm", matrix(1:8, nrow = 4))
    sp <- Matrix::sparseMatrix(
        i = c(1, 3, 2), j = c(1, 1, 2), x = c(10, 20, 30), dims = c(4, 2)
    )
    set_matrix(d, "cell", "gene", "sm", sp)
    rm(d)
    gc()

    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = ZipDaf(raw"%s", "r")', p),
        '@assert get_scalar(daf, "pi")    == 3.14',
        '@assert get_scalar(daf, "cells") == Int64(100)',
        '@assert get_scalar(daf, "note")  == "hello"',
        '@assert axis_vector(daf, "cell") == ["A","B","C","D"]',
        '@assert axis_vector(daf, "gene") == ["X","Y"]',
        '@assert get_vector(daf, "cell", "donor") == Int32[1,2,3,4]',
        '@assert get_vector(daf, "cell", "sx") == Float64[0,10,0,30]',
        '@assert get_vector(daf, "cell", "flags") == Bool[1,0,1,0]',
        '@assert get_matrix(daf, "cell", "gene", "dm") == Float64[1 5; 2 6; 3 7; 4 8]',
        'sm = get_matrix(daf, "cell", "gene", "sm")',
        "@assert size(sm) == (4,2)",
        "@assert nnz(sm) == 3",
        '@assert Matrix(sm) == Float64[10 0; 0 30; 20 0; 0 0]',
        'println("JULIA_OK")'
    )
    out <- run_julia(script)
    expect_true(any(grepl("JULIA_OK", out)), info = paste(out, collapse = "\n"))
})

test_that("Julia-written .daf.zip is readable by R with identical values", {
    skip_if_not(.have_julia_env())
    p <- tempfile(fileext = ".daf.zip")
    script <- c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = ZipDaf(raw"%s", "w")', p),
        'add_axis!(daf, "cell", ["A","B","C","D"])',
        'add_axis!(daf, "gene", ["X","Y"])',
        'set_scalar!(daf, "pi", 3.14)',
        'set_scalar!(daf, "note", "hello")',
        'set_vector!(daf, "cell", "donor", Int32[1,2,3,4])',
        'set_vector!(daf, "cell", "sx", Float64[0,10,0,30])',
        'set_matrix!(daf, "cell", "gene", "dm", Float64[1 5; 2 6; 3 7; 4 8])',
        'set_matrix!(daf, "cell", "gene", "sm", sparse(Float64[10 0; 0 30; 20 0; 0 0]))',
        'println("JULIA_WROTE")'
    )
    out <- run_julia(script)
    skip_if_not(any(grepl("JULIA_WROTE", out)), message = paste(out, collapse = "\n"))

    d <- zip_daf(p, mode = "r")
    expect_equal(axis_vector(d, "cell"), c("A", "B", "C", "D"))
    expect_equal(axis_vector(d, "gene"), c("X", "Y"))
    expect_equal(get_scalar(d, "pi"), 3.14)
    expect_equal(get_scalar(d, "note"), "hello")
    expect_equal(unname(get_vector(d, "cell", "donor")), c(1L, 2L, 3L, 4L))
    expect_equal(unname(get_vector(d, "cell", "sx")), c(0, 10, 0, 30))
    expect_equal(
        unname(get_matrix(d, "cell", "gene", "dm")),
        matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4)
    )
    sm <- get_matrix(d, "cell", "gene", "sm")
    expect_s4_class(sm, "dgCMatrix")
    expect_equal(
        as.matrix(unname(sm)),
        matrix(c(10, 0, 20, 0, 0, 30, 0, 0), nrow = 4)
    )
})
