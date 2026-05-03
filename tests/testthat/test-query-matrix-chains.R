# Parity port of DataAxesFormats.jl test/queries.jl > matrix > lookup
# leaves: vector / vector as_axis / vector if_not / matrix / matrix as_axis /
# square column / square row.

local({
    setup_base <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("X", "Y"))
        add_axis(d, "gene", c("A", "B", "C"))
        add_axis(d, "tag", c("U", "V", "W"))
        set_vector(d, "tag", "color", c("red", "green", "blue"))
        d
    }

    test_that(":: tag : color chains a matrix through a vector lookup", {
        d <- setup_base()
        set_matrix(d, "cell", "gene", "tag", matrix(
            c("U", "V", "W",
              "W", "V", "U"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C"))
        ))
        out <- get_query(d, "@ cell @ gene :: tag : color")
        expect_equal(out["X", ], c(A = "red",  B = "green", C = "blue"))
        expect_equal(out["Y", ], c(A = "blue", B = "green", C = "red"))
    })

    test_that(":: tig =@ tag : color renames the chain target axis", {
        d <- setup_base()
        set_matrix(d, "cell", "gene", "tig", matrix(
            c("U", "V", "W",
              "W", "V", "U"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C"))
        ))
        out <- get_query(d, "@ cell @ gene :: tig =@ tag : color")
        expect_equal(out["X", ], c(A = "red",  B = "green", C = "blue"))
        expect_equal(out["Y", ], c(A = "blue", B = "green", C = "red"))
    })

    test_that(":: tag ?? purple : color substitutes default for empty entries", {
        d <- setup_base()
        set_matrix(d, "cell", "gene", "tag", matrix(
            c("",  "V", "W",
              "",  "V", "U"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C"))
        ))
        out <- get_query(d, "@ cell @ gene :: tag ?? purple : color")
        expect_equal(out["X", ], c(A = "purple", B = "green", C = "blue"))
        expect_equal(out["Y", ], c(A = "purple", B = "green", C = "red"))
    })

    test_that(":: tag :: color @ kind = K chains matrix through matrix lookup", {
        d <- setup_base()
        add_axis(d, "kind", c("K", "L"))
        set_matrix(d, "kind", "tag", "color", matrix(
            c("red", "green", "blue",
              "cyan", "magenta", "yellow"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("K", "L"), c("U", "V", "W"))
        ))
        set_matrix(d, "cell", "gene", "tag", matrix(
            c("U", "V", "W",
              "W", "V", "U"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C"))
        ))
        out <- get_query(d, "@ cell @ gene :: tag :: color @ kind = K")
        expect_equal(out["X", ], c(A = "red",  B = "green", C = "blue"))
        expect_equal(out["Y", ], c(A = "blue", B = "green", C = "red"))
    })

    test_that(":: tig =@ tag :: color @ kind = K renames axis in matrix chain", {
        d <- setup_base()
        add_axis(d, "kind", c("K", "L"))
        set_matrix(d, "kind", "tag", "color", matrix(
            c("red", "green", "blue",
              "cyan", "magenta", "yellow"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("K", "L"), c("U", "V", "W"))
        ))
        set_matrix(d, "cell", "gene", "tig", matrix(
            c("U", "V", "W",
              "W", "V", "U"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C"))
        ))
        out <- get_query(d, "@ cell @ gene :: tig =@ tag :: color @ kind = K")
        expect_equal(out["X", ], c(A = "red",  B = "green", C = "blue"))
        expect_equal(out["Y", ], c(A = "blue", B = "green", C = "red"))
    })

    test_that(":: tag :: color @| U slices a square chain matrix by column", {
        d <- setup_base()
        # Square matrix on tag axis (3x3)
        set_matrix(d, "tag", "tag", "color", matrix(
            c("red",   "green",   "blue",
              "cyan",  "magenta", "yellow",
              "black", "white",   "gray"),
            nrow = 3L, byrow = TRUE,
            dimnames = list(c("U", "V", "W"), c("U", "V", "W"))
        ))
        set_matrix(d, "cell", "gene", "tag", matrix(
            c("U", "V", "W",
              "W", "V", "U"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C"))
        ))
        out <- get_query(d, "@ cell @ gene :: tag :: color @| U")
        expect_equal(out["X", ], c(A = "red",   B = "cyan", C = "black"))
        expect_equal(out["Y", ], c(A = "black", B = "cyan", C = "red"))
    })

    test_that(":: tag :: color @- U slices a square chain matrix by row", {
        d <- setup_base()
        set_matrix(d, "tag", "tag", "color", matrix(
            c("red",   "green",   "blue",
              "cyan",  "magenta", "yellow",
              "black", "white",   "gray"),
            nrow = 3L, byrow = TRUE,
            dimnames = list(c("U", "V", "W"), c("U", "V", "W"))
        ))
        set_matrix(d, "cell", "gene", "tag", matrix(
            c("U", "V", "W",
              "W", "V", "U"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C"))
        ))
        out <- get_query(d, "@ cell @ gene :: tag :: color @- U")
        expect_equal(out["X", ], c(A = "red",  B = "green", C = "blue"))
        expect_equal(out["Y", ], c(A = "blue", B = "green", C = "red"))
    })
})
