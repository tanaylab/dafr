# Parity with DataAxesFormats.jl: bare `[ prop ]` and bare `??` reduce
# values to booleans via Julia's `as_booleans` (queries.jl:5221-5230).
# For strings the rule is `vec != ""`. R's default `vec != 0` coerces 0
# to "0" for character vectors so empty strings would silently pass —
# which previously let outlier cells (`metacell == ""`) survive a
# `[ metacell ]` mask while being correctly dropped by a chained
# `: metacell ?? : type`.

setup_string_outliers <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    # Cell E is an outlier (empty metacell)
    set_vector(d, "cell", "metacell", c("X", "Y", "X", "Y", ""))
    set_vector(d, "cell", "total_UMIs", c(10L, 20L, 30L, 40L, 50L))
    add_axis(d, "metacell", c("X", "Y"))
    set_vector(d, "metacell", "type", c("U", "V"))
    d
}

test_that("[ string_prop ] drops entries whose value is the empty string", {
    d <- setup_string_outliers()
    out <- get_query(d, "@ cell [ metacell ] : total_UMIs")
    expect_equal(out, c(A = 10L, B = 20L, C = 30L, D = 40L))
})

test_that("[ ! string_prop ] keeps only entries whose value is the empty string", {
    d <- setup_string_outliers()
    out <- get_query(d, "@ cell [ ! metacell ] : total_UMIs")
    expect_equal(out, c(E = 50L))
})

test_that("[ string_prop ] and `: prop ?? : prop2` agree on the surviving entries", {
    d <- setup_string_outliers()
    umis    <- get_query(d, "@ cell [ metacell ] : total_UMIs")
    cl_type <- get_query(d, "@ cell : metacell ?? : type")
    expect_equal(setdiff(names(umis), names(cl_type)), character())
    expect_equal(setdiff(names(cl_type), names(umis)), character())
})

test_that("string-prop mask combines correctly with another bool mask", {
    d <- setup_string_outliers()
    set_vector(d, "cell", "is_low",
               c(TRUE, FALSE, TRUE, FALSE, TRUE))
    # `[ metacell & is_low ]` keeps cells with non-empty metacell AND is_low.
    # Survivors: A (X, TRUE), C (X, TRUE). E has is_low=TRUE but metacell="".
    out <- get_query(d, "@ cell [ metacell & is_low ] : total_UMIs")
    expect_equal(out, c(A = 10L, C = 30L))
})

test_that("numeric mask still drops zeros", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "n_umis", c(0L, 5L, 0L))
    set_vector(d, "cell", "total_UMIs", c(10L, 20L, 30L))
    out <- get_query(d, "@ cell [ n_umis ] : total_UMIs")
    expect_equal(out, c(B = 20L))
})

test_that("logical mask passes through unchanged", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "is_keep", c(TRUE, FALSE, TRUE))
    set_vector(d, "cell", "total_UMIs", c(10L, 20L, 30L))
    out <- get_query(d, "@ cell [ is_keep ] : total_UMIs")
    expect_equal(out, c(A = 10L, C = 30L))
})
