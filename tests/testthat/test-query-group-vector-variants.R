# Parity port of DataAxesFormats.jl test/queries.jl > vector > group > vector
# leaves: (), !string, matrix-key, square-key (column / row), as_axis, missing.

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B", "C", "D"))
        add_axis(d, "gene", c("X", "Y"))
        add_axis(d, "type", c("U", "V", "W"))
        set_vector(d, "cell", "type", c("U", "U", "V", "V"))
        set_vector(d, "cell", "score", c(0, 1, 2, 3))
        # Julia matrix literal `[a b; c d]` is row-major; R needs byrow = TRUE
        # to match.
        set_matrix(d, "gene", "cell", "level", matrix(
            c("low", "middle", "middle", "high",
              "middle", "high", "low", "high"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C", "D"))
        ))
        set_matrix(d, "cell", "cell", "distance", matrix(
            c(0, 1, 1, 1,
              0, 0, 1, 1,
              0, 0, 0, 1,
              0, 0, 0, 0),
            nrow = 4L, byrow = TRUE,
            dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D"))
        ))
        d
    }

    test_that("group-by Sum on String values raises (unsupported input type)", {
        d <- setup()
        expect_error(
            get_query(d, "@ cell : type / score >> Sum"),
            "non-numeric|String|Sum|invalid|character"
        )
    })

    test_that("group-by with matrix-slice key (`/ level @ gene = X`)", {
        d <- setup()
        out <- get_query(d, "@ cell : score / level @ gene = X >> Sum")
        # Julia: ["high" => 3.0, "low" => 0.0, "middle" => 3.0]
        expect_equal(out[["high"]], 3)
        expect_equal(out[["low"]], 0)
        expect_equal(out[["middle"]], 3)
    })

    test_that("group-by with square @| key (column-of-square)", {
        d <- setup()
        out <- get_query(d, "@ cell : score / distance @| C >> Sum")
        # Julia: ["0" => 5.0, "1" => 1.0]
        expect_equal(out[["0"]], 5)
        expect_equal(out[["1"]], 1)
    })

    test_that("group-by with square @- key (row-of-square)", {
        d <- setup()
        out <- get_query(d, "@ cell : score / distance @- A >> Sum")
        # Julia: ["0" => 0.0, "1" => 6.0]
        expect_equal(out[["0"]], 0)
        expect_equal(out[["1"]], 6)
    })

    test_that("group-by with =@ axis and || default fills unused entries", {
        d <- setup()
        out <- get_query(d, "@ cell : score / type =@ >> Sum || 0.0")
        # Julia: ["U" => 1.0, "V" => 5.0, "W" => 0.0]
        expect_equal(out[["U"]], 1)
        expect_equal(out[["V"]], 5)
        expect_equal(out[["W"]], 0)
    })

    test_that("group-by with =@ axis but no default raises on unused entries", {
        d <- setup()
        expect_error(
            get_query(d, "@ cell : score / type =@ >> Sum"),
            "unused entry|IfMissing|W"
        )
    })
})
