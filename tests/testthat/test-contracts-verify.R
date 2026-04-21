test_that("verify_input on plain DafReader is a no-op", {
    d <- memory_daf(name = "d")
    expect_null(verify_input(d))
    expect_null(verify_output(d))
})

test_that("verify_input on RequiredInput scalar fails when missing", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    expect_error(verify_input(cd), "missing input scalar: version")
})

test_that("verify_input on RequiredInput scalar of wrong type fails", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", "oops")
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    expect_error(verify_input(cd), "unexpected type: character")
})

test_that("verify_output on CreatedOutput fails when pre-existing + !overwrite", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "result", 1L)
    cn <- Contract(
        data = list(contract_scalar("result", CreatedOutput, "integer", "r"))
    )
    cd <- contractor("comp", cn, d)
    expect_error(verify_input(cd), "pre-existing CreatedOutput scalar: result")
})

test_that("verify_output on unused RequiredInput fails after computation", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    # Never access version.
    expect_error(verify_output(cd), "unused RequiredInput scalar: version")
})

test_that("verify_output pass after accessing RequiredInput", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    invisible(get_scalar(cd, "version"))
    expect_null(verify_output(cd))
})
