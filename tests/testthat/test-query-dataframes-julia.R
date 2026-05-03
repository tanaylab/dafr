# Parity port of DataAxesFormats.jl test/queries.jl > dataframes >
# {simple, complex} > axis / columns / mask leaves.

# ---- simple > axis -------------------------------------------------------

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        set_vector(d, "cell", "is_doublet", c(TRUE, FALSE))
        set_vector(d, "cell", "age", c(0L, 1L))
        d
    }

    test_that("get_dataframe(daf, axis_name) returns full axis frame", {
        d <- setup()
        df <- get_dataframe(d, "cell")
        expect_equal(rownames(df), c("A", "B"))
        expect_setequal(colnames(df), c("age", "is_doublet"))
        expect_equal(df["A", "age"], 0L)
        expect_equal(df["B", "is_doublet"], FALSE)
    })

    test_that("get_dataframe_query(daf, '@ cell') matches axis-name form", {
        d <- setup()
        df <- get_dataframe_query(d, "@ cell")
        expect_equal(rownames(df), c("A", "B"))
        expect_equal(df["A", "age"], 0L)
    })

    test_that("non-axis query is rejected by get_dataframe_query", {
        d <- setup()
        expect_error(get_dataframe_query(d, "@ cell : age"), "axis")
    })

    test_that("masked axis query yields filtered frame", {
        d <- setup()
        df <- get_dataframe_query(d, "@ cell [ is_doublet ]")
        expect_equal(rownames(df), "A")
        expect_equal(df["A", "age"], 0L)
    })
})

# ---- simple > columns ----------------------------------------------------

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        set_vector(d, "cell", "is_doublet", c(TRUE, FALSE))
        set_vector(d, "cell", "age", c(0L, 1L))
        d
    }

    test_that("explicit column names return only the named vectors in order", {
        d <- setup()
        df <- get_dataframe(d, "cell", c("age", "is_doublet"))
        expect_equal(colnames(df), c("age", "is_doublet"))
        expect_equal(df["A", "age"], 0L)
    })

    test_that("named-query columns evaluate query strings against the axis", {
        d <- setup()
        df <- get_dataframe(d, "cell",
            c(age = ": age", doublet = ": is_doublet"))
        expect_equal(colnames(df), c("age", "doublet"))
        expect_equal(df["A", "age"], 0L)
        expect_equal(df["B", "doublet"], FALSE)
    })

    test_that("reduction in a column query raises 'invalid column query'", {
        d <- setup()
        expect_error(
            get_dataframe(d, "cell",
                c(age = ": age >> Sum", doublet = ": is_doublet")),
            "invalid column query"
        )
    })
})

# ---- complex > axis / masked / !masked -----------------------------------

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B", "D", "E"))
        add_axis(d, "metacell", c("X", "Y"))
        set_vector(d, "cell", "metacell", c("X", "X", "Y", "Y"))
        set_vector(d, "metacell", "type", c("U", "V"))
        set_vector(d, "cell", "age", c(0L, 1L, 2L, 3L))
        d
    }

    test_that("complex axis query with reduction column", {
        d <- setup()
        df <- get_dataframe(d, "metacell",
            c(mean_age = "@ cell : age / metacell >> Mean"))
        expect_equal(df["X", "mean_age"], 0.5)
        expect_equal(df["Y", "mean_age"], 2.5)
    })

    test_that("complex masked frame with mask-respecting column", {
        d <- setup()
        df <- get_dataframe_query(d, "@ metacell [ type = U ]",
            c(mean_age = "@ cell [ metacell : type = U ] : age / metacell >> Mean"))
        expect_equal(rownames(df), "X")
        expect_equal(df["X", "mean_age"], 0.5)
    })

    test_that("complex column query that ignores the outer mask raises", {
        d <- setup()
        expect_error(
            get_dataframe_query(d, "@ metacell [ type = U ]",
                c(mean_age = "@ cell [ metacell ] : age / metacell >> Mean")),
            "invalid column query"
        )
    })
})
