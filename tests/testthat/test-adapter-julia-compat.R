test_that("R adapter()+computation() matches Julia fixture bit-identically", {
    skip_if_not(file.exists("fixtures/julia-adapter/fixture.json"),
                "Julia adapter fixture absent")
    fx <- jsonlite::fromJSON("fixtures/julia-adapter/fixture.json",
                             simplifyVector = TRUE)

    withr::local_options(list(dafr.enforce_contracts = TRUE))
    d <- example_cells_daf()

    c <- Contract(
        axes = list(
            obs = list(RequiredInput, "renamed cell axis"),
            var = list(RequiredInput, "renamed gene axis")
        ),
        data = list(
            contract_matrix("obs", "var", "UMIs",
                RequiredInput, "integer", "UMI counts matrix"),
            contract_vector("obs", "total_umis",
                CreatedOutput, "integer", "total UMIs per cell")
        )
    )
    inner <- function(adapted) {
        m <- get_matrix(adapted, "obs", "var", "UMIs")
        totals <- as.integer(rowSums(as.matrix(m)))
        set_vector(adapted, "obs", "total_umis", totals)
        "ok"
    }
    comp <- computation("sum_umis", c, inner)

    res <- adapter(d, comp,
        input_axes = list(
            list("obs", "@ cell"), list("var", "@ gene"),
            list("cell", NULL), list("gene", NULL)
        ),
        input_data = VIEW_ALL_DATA,
        output_axes = list(
            list("cell", "@ obs"), list("gene", "@ var"),
            list("obs", NULL), list("var", NULL)
        ),
        output_data = list(
            list(ALL_VECTORS, NULL),
            list(ALL_MATRICES, NULL),
            list(c("cell", "total_umis"), "=")
        )
    )
    expect_identical(res, as.character(fx$result_returned))
    got <- unname(get_vector(d, "cell", "total_umis"))
    expect_identical(length(got), as.integer(fx$total_umis_length))
    expect_identical(got, as.integer(fx$total_umis_values))
})
