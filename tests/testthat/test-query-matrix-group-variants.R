# Parity port of DataAxesFormats.jl test/queries.jl > matrix > group
# leaves: column / row × {(), slice, square > {column, row}, !string,
# as_axis > {(), missing}, missing}.

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B", "C", "D"))
        add_axis(d, "gene", c("X", "Y"))
        add_axis(d, "type", c("U", "V", "W"))
        set_vector(d, "cell", "type", c("U", "U", "V", "V"))
        # Julia: 2 rows (X,Y) x 4 cols (A,B,C,D) with values
        # [0 1 2 3; 4 5 6 7]
        set_matrix(d, "gene", "cell", "UMIs", matrix(
            c(0, 1, 2, 3,
              4, 5, 6, 7),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C", "D"))
        ))
        # Julia: ["A" "B" "C" "A"; "C" "A" "B" "C"]
        set_matrix(d, "gene", "cell", "kind", matrix(
            c("A", "B", "C", "A",
              "C", "A", "B", "C"),
            nrow = 2L, byrow = TRUE,
            dimnames = list(c("X", "Y"), c("A", "B", "C", "D"))
        ))
        # Julia: [0 1 1 1; 0 0 1 1; 0 0 0 1; 0 0 0 0]
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

    # --- column (|/) ------------------------------------------------------

    test_that("|/ type >| Sum groups columns by type and reduces", {
        d <- setup()
        out <- get_query(d, "@ gene @ cell :: UMIs |/ type >| Sum")
        expect_equal(out["X", "U"], 1)
        expect_equal(out["X", "V"], 5)
        expect_equal(out["Y", "U"], 9)
        expect_equal(out["Y", "V"], 13)
    })

    test_that("|/ kind @ gene = X >| Sum groups by matrix-slice key", {
        d <- setup()
        out <- get_query(d, "@ gene @ cell :: UMIs |/ kind @ gene = X >| Sum")
        # Julia: X-row of kind = ["A","B","C","A"]; group cols A,B,C,D into A:{A,D}, B:{B}, C:{C}
        # UMIs row X = [0,1,2,3]; A:0+3=3, B:1, C:2 — that's row X
        # UMIs row Y = [4,5,6,7]; A:4+7=11, B:5, C:6 — that's row Y
        expect_equal(out["X", "A"], 3)
        expect_equal(out["X", "B"], 1)
        expect_equal(out["X", "C"], 2)
        expect_equal(out["Y", "A"], 11)
        expect_equal(out["Y", "B"], 5)
        expect_equal(out["Y", "C"], 6)
    })

    test_that("|/ distance @| B >| Sum groups by square-column key", {
        d <- setup()
        out <- get_query(d, "@ gene @ cell :: UMIs |/ distance @| B >| Sum")
        # distance col B = [1,0,0,0]; cells A:1, B:0, C:0, D:0
        # group cols A,B,C,D by [1,0,0,0]: 0:{B,C,D}, 1:{A}
        # UMIs row X: 0:{1+2+3=6}, 1:{0}; row Y: 0:{5+6+7=18}, 1:{4}
        expect_equal(out["X", "0.0"], 6)
        expect_equal(out["X", "1.0"], 0)
        expect_equal(out["Y", "0.0"], 18)
        expect_equal(out["Y", "1.0"], 4)
    })

    test_that("|/ distance @- B >| Sum groups by square-row key", {
        d <- setup()
        out <- get_query(d, "@ gene @ cell :: UMIs |/ distance @- B >| Sum")
        # distance row B = [0,0,1,1]; cells A:0,B:0,C:1,D:1
        # 0:{A,B}, 1:{C,D}
        # UMIs row X: 0:{0+1=1}, 1:{2+3=5}; row Y: 0:{4+5=9}, 1:{6+7=13}
        expect_equal(out["X", "0.0"], 1)
        expect_equal(out["X", "1.0"], 5)
        expect_equal(out["Y", "0.0"], 9)
        expect_equal(out["Y", "1.0"], 13)
    })

    test_that("|/ kind |/ type >| Sum on String values raises", {
        d <- setup()
        expect_error(
            get_query(d, "@ gene @ cell :: kind |/ type >| Sum"),
            "non-numeric|String|Sum|invalid|character"
        )
    })

    test_that("|/ type =@ >| Sum || 0 expands to full type axis with default", {
        d <- setup()
        out <- get_query(d, "@ gene @ cell :: UMIs |/ type =@ >| Sum || 0")
        expect_equal(out["X", "U"], 1)
        expect_equal(out["X", "V"], 5)
        expect_equal(out["X", "W"], 0)
        expect_equal(out["Y", "U"], 9)
        expect_equal(out["Y", "V"], 13)
        expect_equal(out["Y", "W"], 0)
    })

    test_that("|/ type =@ >| Sum (no default) raises on unused axis entry", {
        d <- setup()
        expect_error(
            get_query(d, "@ gene @ cell :: UMIs |/ type =@ >| Sum"),
            "unused entry|IfMissing|W"
        )
    })

    # --- row (-/) ---------------------------------------------------------

    test_that("-/ type >- Sum groups rows by type and reduces", {
        d <- setup()
        out <- get_query(d, "@ cell @ gene :: UMIs -/ type >- Sum")
        # type = [U,U,V,V] for cells A,B,C,D
        # UMIs col X (transposed: cell,gene → so A,B,C,D rows for gene X) = [0,4]?
        # Actually UMIs in cell @ gene order is transposed from gene @ cell.
        # For each type, sum across the type's rows for each gene:
        # U:{A,B}, V:{C,D}; UMIs gene X col = [0,1,2,3] (cells A,B,C,D), gene Y = [4,5,6,7]
        # Wait — the matrix was stored gene @ cell, so transposing gives cell @ gene.
        # UMIs[cell, gene]: row A = [0,4], B=[1,5], C=[2,6], D=[3,7]
        # Sum group U (A,B) for X: 0+1=1; for Y: 4+5=9
        # Sum group V (C,D) for X: 2+3=5; for Y: 6+7=13
        expect_equal(out["U", "X"], 1)
        expect_equal(out["U", "Y"], 9)
        expect_equal(out["V", "X"], 5)
        expect_equal(out["V", "Y"], 13)
    })

    test_that("-/ kind @ gene = X >- Sum groups rows by matrix-slice key", {
        d <- setup()
        out <- get_query(d, "@ cell @ gene :: UMIs -/ kind @ gene = X >- Sum")
        # kind X-row = ["A","B","C","A"] → cell A:A, B:B, C:C, D:A
        # group rows A:{A,D}, B:{B}, C:{C}
        # Per-gene reductions on UMIs (cell @ gene; row-major from setup):
        # gene X col = [0,1,2,3]; gene Y col = [4,5,6,7]
        # A:{A=0,D=3} → X=3, Y=11; B:{B=1} → X=1, Y=5; C:{C=2} → X=2, Y=6
        expect_equal(out["A", "X"], 3)
        expect_equal(out["A", "Y"], 11)
        expect_equal(out["B", "X"], 1)
        expect_equal(out["B", "Y"], 5)
        expect_equal(out["C", "X"], 2)
        expect_equal(out["C", "Y"], 6)
    })

    test_that("-/ distance @| B >- Sum groups rows by square-column key", {
        d <- setup()
        out <- get_query(d, "@ cell @ gene :: UMIs -/ distance @| B >- Sum")
        # distance col B = [1,0,0,0]; cells A:1,B:0,C:0,D:0
        # 0:{B,C,D}, 1:{A}
        # gene X: 0:{1+2+3=6}, 1:{0}; gene Y: 0:{5+6+7=18}, 1:{4}
        expect_equal(out["0.0", "X"], 6)
        expect_equal(out["0.0", "Y"], 18)
        expect_equal(out["1.0", "X"], 0)
        expect_equal(out["1.0", "Y"], 4)
    })

    test_that("-/ distance @- B >- Sum groups rows by square-row key", {
        d <- setup()
        out <- get_query(d, "@ cell @ gene :: UMIs -/ distance @- B >- Sum")
        # distance row B = [0,0,1,1]; cells A:0,B:0,C:1,D:1
        # 0:{A,B}, 1:{C,D}
        # gene X: 0:{0+1=1}, 1:{2+3=5}; gene Y: 0:{4+5=9}, 1:{6+7=13}
        expect_equal(out["0.0", "X"], 1)
        expect_equal(out["0.0", "Y"], 9)
        expect_equal(out["1.0", "X"], 5)
        expect_equal(out["1.0", "Y"], 13)
    })

    test_that("-/ type =@ >- Sum || 0 expands to full type axis with default", {
        d <- setup()
        out <- get_query(d, "@ cell @ gene :: UMIs -/ type =@ >- Sum || 0")
        expect_equal(out["U", "X"], 1)
        expect_equal(out["U", "Y"], 9)
        expect_equal(out["V", "X"], 5)
        expect_equal(out["V", "Y"], 13)
        expect_equal(out["W", "X"], 0)
        expect_equal(out["W", "Y"], 0)
    })

    test_that("-/ type =@ >- Sum (no default) raises on unused axis entry", {
        d <- setup()
        expect_error(
            get_query(d, "@ cell @ gene :: UMIs -/ type =@ >- Sum"),
            "unused entry|IfMissing|W"
        )
    })

    test_that("-/ kind -/ type >- Sum on String values raises", {
        d <- setup()
        expect_error(
            get_query(d, "@ cell @ gene :: kind -/ type >- Sum"),
            "non-numeric|String|Sum|invalid|character"
        )
    })
})
