test_that("expectation constants have string values", {
    expect_identical(RequiredInput, "RequiredInput")
    expect_identical(OptionalInput, "OptionalInput")
    expect_identical(CreatedOutput, "CreatedOutput")
    expect_identical(GuaranteedOutput, "GuaranteedOutput")
    expect_identical(OptionalOutput, "OptionalOutput")
})

test_that("Contract() builds an object with axes + data slots", {
    c1 <- Contract(
        axes = list(cell = list(RequiredInput, "cell axis")),
        data = list(
            contract_scalar("version", RequiredInput, "integer", "dataset version"),
            contract_vector("cell", "age", RequiredInput, "integer", "cell age")
        )
    )
    expect_s3_class(c1, "dafr::Contract")
    expect_named(S7::prop(c1, "axes"), "cell")
    expect_length(S7::prop(c1, "data"), 2L)
})

test_that("Contract() rejects unknown expectation", {
    expect_error(
        contract_scalar("v", "NotAnExpectation", "integer", "d"),
        "unknown expectation"
    )
})
