# Parity port of DataAxesFormats.jl test/queries.jl > matrix > count
# leaves: vector / column / square > {column, row}.

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("X", "Y"))
        add_axis(d, "gene", c("A", "B", "C"))
        set_vector(d, "gene", "width", c(1, 2, 1))
        d
    }

    test_that("count `* type =@` returns axis-by-axis matrix with zeros", {
        d <- setup()
        add_axis(d, "type", c("U", "V", "W"))
        set_vector(d, "gene", "type", c("U", "U", "V"))
        # Julia: ((:A,"type"), [("1","U")=>1,("1","V")=>1,("1","W")=>0,
        #                       ("2","U")=>1,("2","V")=>0,("2","W")=>0])
        out <- get_query(d, "@ gene : width * type =@")
        expect_equal(dim(out), c(2L, 3L))
        expect_equal(out["1.0", "U"], 1L)
        expect_equal(out["1.0", "V"], 1L)
        expect_equal(out["1.0", "W"], 0L)
        expect_equal(out["2.0", "U"], 1L)
        expect_equal(out["2.0", "V"], 0L)
        expect_equal(out["2.0", "W"], 0L)
    })

    test_that("count `* level @ cell = X` cross-tabs by matrix-slice key", {
        d <- setup()
        # Julia matrix is row-major; use byrow = TRUE.
        set_matrix(d, "cell", "gene", "level", matrix(
            c("low",  "middle", "high",
              "high", "middle", "low"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C"))
        ))
        # Julia: [("1","high")=>1,("1","low")=>1,("1","middle")=>0,
        #        ("2","high")=>0,("2","low")=>0,("2","middle")=>1]
        out <- get_query(d, "@ gene : width * level @ cell = X")
        expect_equal(out["1.0", "high"], 1L)
        expect_equal(out["1.0", "low"], 1L)
        expect_equal(out["1.0", "middle"], 0L)
        expect_equal(out["2.0", "high"], 0L)
        expect_equal(out["2.0", "low"], 0L)
        expect_equal(out["2.0", "middle"], 1L)
    })

    test_that("count `* distance @| C` cross-tabs by square-column slice", {
        d <- setup()
        set_matrix(d, "gene", "gene", "distance", matrix(
            c(0, 1, 1,
              1, 1, 0,
              1, 0, 1),
            nrow = 3L, byrow = TRUE,
            dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
        ))
        # Julia: [("1","0")=>0,("1","1")=>2,("2","0")=>1,("2","1")=>0]
        out <- get_query(d, "@ gene : width * distance @| C")
        expect_equal(out["1.0", "0.0"], 0L)
        expect_equal(out["1.0", "1.0"], 2L)
        expect_equal(out["2.0", "0.0"], 1L)
        expect_equal(out["2.0", "1.0"], 0L)
    })

    test_that("count `* distance @- A` cross-tabs by square-row slice", {
        d <- setup()
        set_matrix(d, "gene", "gene", "distance", matrix(
            c(0, 1, 1,
              1, 1, 0,
              1, 0, 1),
            nrow = 3L, byrow = TRUE,
            dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
        ))
        # Julia: [("1","0")=>1,("1","1")=>1,("2","0")=>0,("2","1")=>1]
        out <- get_query(d, "@ gene : width * distance @- A")
        expect_equal(out["1.0", "0.0"], 1L)
        expect_equal(out["1.0", "1.0"], 1L)
        expect_equal(out["2.0", "0.0"], 0L)
        expect_equal(out["2.0", "1.0"], 1L)
    })
})
