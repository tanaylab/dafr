test_that(".example_data_dir resolves to installed extdata", {
    d <- dafr:::.example_data_dir()
    expect_true(dir.exists(d))
    expect_true(dir.exists(file.path(d, "axes")))
    expect_true(dir.exists(file.path(d, "vectors")))
    expect_true(dir.exists(file.path(d, "matrices")))
})

test_that(".cast_vector: true/false strings become logical", {
    v <- dafr:::.cast_vector(c("true", "false", "true"))
    expect_identical(v, c(TRUE, FALSE, TRUE))
})

test_that(".cast_vector: integer-valued numeric strings become integer", {
    v <- dafr:::.cast_vector(c("1.0", "2.0", "3.0"))
    expect_identical(v, c(1L, 2L, 3L))
})

test_that(".cast_vector: negative integer-valued strings become integer", {
    v <- dafr:::.cast_vector(c("-1.0", "-2.0"))
    expect_identical(v, c(-1L, -2L))
})

test_that(".cast_vector: mixed alpha strings stay character", {
    v <- dafr:::.cast_vector(c("male", "female", "male"))
    expect_identical(v, c("male", "female", "male"))
})

test_that(".cast_vector: non-integer numeric stays double", {
    v <- dafr:::.cast_vector(c("1.5", "2.5"))
    expect_identical(v, c(1.5, 2.5))
})

test_that(".cast_matrix promotes to integer when all values fit UInt8", {
    m <- matrix(c(0, 1, 2, 3, 4, 5), nrow = 2)
    out <- dafr:::.cast_matrix(m)
    expect_true(is.integer(out))
    expect_equal(dim(out), dim(m))
    expect_identical(out, matrix(c(0L, 1L, 2L, 3L, 4L, 5L), nrow = 2))
})

test_that(".cast_matrix: non-integer-valued matrix stays double", {
    m <- matrix(c(1.5, 2.5), nrow = 1L, ncol = 2L)
    result <- dafr:::.cast_matrix(m)
    expect_type(result, "double")
})

test_that(".cast_matrix: values > 65535 stay double", {
    m <- matrix(c(0, 65536), nrow = 1L, ncol = 2L)
    storage.mode(m) <- "double"
    result <- dafr:::.cast_matrix(m)
    expect_type(result, "double")
})

test_that(".load_axis_file respects the cells/metacells/shared kind filter", {
    cells_daf <- memory_daf(name = "cells-filter-test")
    meta_daf <- memory_daf(name = "meta-filter-test")
    dir <- dafr:::.example_data_dir()

    # mc.cell should load for both "c" and "m" (mc contains both)
    dafr:::.load_axis_file(cells_daf, "c", file.path(dir, "axes", "mc.cell.rds"))
    dafr:::.load_axis_file(meta_daf, "m", file.path(dir, "axes", "mc.cell.rds"))

    expect_equal(length(axis_vector(cells_daf, "cell")), 856L)
    expect_equal(length(axis_vector(meta_daf, "cell")), 856L)

    # c.donor should load for "c" only, not "m"
    dafr:::.load_axis_file(cells_daf, "c", file.path(dir, "axes", "c.donor.rds"))
    dafr:::.load_axis_file(meta_daf, "m", file.path(dir, "axes", "c.donor.rds"))

    expect_equal(length(axis_vector(cells_daf, "donor")), 95L)
    expect_null(axis_vector(meta_daf, "donor", null_if_missing = TRUE))
})

test_that("example_cells_daf returns a MemoryDaf with correct scalars and axes", {
    d <- example_cells_daf()
    expect_s3_class(d, "dafr::MemoryDaf")
    expect_identical(S7::prop(d, "name"), "cells!")
    expect_identical(get_scalar(d, "organism"), "human")
    expect_identical(get_scalar(d, "reference"), "test")

    axes <- axes_set(d)
    expect_true(setequal(axes, c("cell", "gene", "donor", "experiment")))
    expect_equal(length(axis_vector(d, "cell")), 856L)
    expect_equal(length(axis_vector(d, "gene")), 683L)
    expect_equal(length(axis_vector(d, "donor")), 95L)
    expect_equal(length(axis_vector(d, "experiment")), 23L)

    # donor age → integer (UInt32-equivalent)
    age <- get_vector(d, "donor", "age")
    expect_type(age, "integer")
    expect_true(is.integer(get_vector(d, "donor", "age")))
    expect_true(is.character(get_vector(d, "donor", "sex")))
    expect_true(is.character(get_vector(d, "cell", "donor")))
    expect_true(is.character(get_vector(d, "cell", "experiment")))

    # gene is_lateral → logical (Bool)
    is_lat <- get_vector(d, "gene", "is_lateral")
    expect_type(is_lat, "logical")

    # UMIs matrix → integer (UInt8-equivalent), shape (856 x 683)
    umis <- get_matrix(d, "cell", "gene", "UMIs")
    expect_type(umis, "integer")
    expect_equal(dim(umis), c(856L, 683L))
})

test_that("example_metacells_daf returns a MemoryDaf with correct axes", {
    d <- example_metacells_daf()
    expect_s3_class(d, "dafr::MemoryDaf")
    expect_identical(S7::prop(d, "name"), "metacells!")

    axes <- axes_set(d)
    expect_true(setequal(axes, c("cell", "gene", "metacell", "type")))
    expect_equal(length(axis_vector(d, "cell")), 856L)
    expect_equal(length(axis_vector(d, "gene")), 683L)
    expect_equal(length(axis_vector(d, "metacell")), 7L)
    expect_equal(length(axis_vector(d, "type")), 4L)

    # gene is_marker → logical
    is_mk <- get_vector(d, "gene", "is_marker")
    expect_type(is_mk, "logical")
    expect_true(is.logical(get_vector(d, "gene", "is_marker")))
    expect_true(is.character(get_vector(d, "cell", "metacell")))
    expect_true(is.character(get_vector(d, "metacell", "type")))
    expect_true(is.character(get_vector(d, "type", "color")))

    # fraction matrix → double (Float32, doesn't fit UInt16)
    frac <- get_matrix(d, "gene", "metacell", "fraction")
    expect_type(frac, "double")

    # edge_weight matrix → double (Float32)
    ew <- get_matrix(d, "metacell", "metacell", "edge_weight")
    expect_type(ew, "double")
    expect_equal(dim(ew), c(7L, 7L))
})

test_that("example_cells_daf matches Slice-3 FilesDaf dump entry-for-entry", {
    skip_if_not(
        dir.exists("fixtures/julia-queries/example-daf"),
        "Julia FilesDaf fixture absent"
    )
    fdd <- files_daf("fixtures/julia-queries/example-daf", mode = "r")
    mdd <- example_cells_daf()

    expect_identical(axes_set(mdd), axes_set(fdd))
    for (ax in axes_set(fdd)) {
        expect_identical(axis_vector(mdd, ax), axis_vector(fdd, ax),
            info = sprintf("axis %s", ax))
    }
    expect_identical(get_scalar(mdd, "organism"), get_scalar(fdd, "organism"))
    expect_identical(get_scalar(mdd, "reference"), get_scalar(fdd, "reference"))
    expect_identical(
        unname(get_vector(mdd, "donor", "age")),
        unname(get_vector(fdd, "donor", "age"))
    )
    expect_identical(
        unname(get_vector(mdd, "gene", "is_lateral")),
        unname(get_vector(fdd, "gene", "is_lateral"))
    )
    expect_equal(
        unname(get_matrix(mdd, "cell", "gene", "UMIs")),
        unname(get_matrix(fdd, "cell", "gene", "UMIs"))
    )
})

test_that("example_chain_daf returns a WriteChainDaf with unioned axes", {
    ch <- example_chain_daf()
    expect_s3_class(ch, "dafr::WriteChainDaf")
    expect_identical(S7::prop(ch, "name"), "chain!")

    expect_setequal(
        axes_set(ch),
        c("cell", "donor", "experiment", "gene", "metacell", "type")
    )
    expect_equal(length(axis_vector(ch, "metacell")), 7L)
    expect_identical(get_scalar(ch, "organism"), "human")
    expect_true(is.logical(get_vector(ch, "gene", "is_lateral")))
    expect_true(is.logical(get_vector(ch, "gene", "is_marker")))
})
